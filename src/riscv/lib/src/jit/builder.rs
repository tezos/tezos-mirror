// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Builder for turning [instructions] into functions.
//!
//! [instructions]: crate::machine_state::instruction::Instruction

pub(super) mod arithmetic;
pub(super) mod block_state;
pub(super) mod comparable;
pub(super) mod errno;

use cranelift::codegen::ir;
use cranelift::codegen::ir::InstBuilder;
use cranelift::codegen::ir::MemFlags;
use cranelift::codegen::ir::Type;
use cranelift::codegen::ir::Value;
use cranelift::codegen::ir::condcodes::IntCC;
use cranelift::codegen::ir::types::I32;
use cranelift::codegen::ir::types::I64;
use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::types::I128;
use errno::AtomicAccessGuard;

use self::block_state::DynamicValues;
use self::errno::Errno;
use super::state_access::JitStateAccess;
use super::state_access::JsaCalls;
use crate::instruction_context::ICB;
use crate::instruction_context::LoadStoreWidth;
use crate::instruction_context::MulHighType;
use crate::instruction_context::PhiValue;
use crate::instruction_context::Predicate;
use crate::instruction_context::arithmetic::Arithmetic;
use crate::instruction_context::comparable::Comparable;
use crate::jit::builder::block_state::PCUpdate;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::registers::NonZeroXRegister;
use crate::parser::instruction::InstrWidth;

/// A newtype for wrapping [`Value`], representing a 64-bit value in the JIT context.
#[derive(Copy, Clone, Debug)]
pub struct X64(pub Value);

/// A newtype for wrapping [`Value`], representing a 32-bit value in the JIT context.
#[derive(Copy, Clone, Debug)]
pub struct X32(pub Value);

/// Builder context used when lowering individual instructions within a block.
pub(crate) struct Builder<'a, MC: MemoryConfig, JSA: JitStateAccess> {
    /// Cranelift function builder
    builder: FunctionBuilder<'a>,

    /// Helpers for calling locally imported [JitStateAccess] methods.
    jsa_call: JsaCalls<'a, MC, JSA>,

    /// The IR-type of pointers on the current native platform
    ptr: Type,

    /// Value representing a pointer to [`MachineCoreState<MC, JSA>`]
    ///
    /// [`MachineCoreState<MC, JSA>`]: crate::machine_state::MachineCoreState
    core_ptr_val: Value,

    /// Value representing a pointer to `steps: usize`
    steps_ptr_val: Value,

    /// Values that are dynamically updated throughout lowering.
    dynamic: DynamicValues,

    /// The final Cranelift-IR block that is last executed on exit from a JIT-compiled
    /// block cache block.
    ///
    /// It is responsible for writing the final values of `steps` and the `instr_pc` back to the state.
    /// *N.B.* the end block can be jumped-to from multiple places, for example by every branching
    /// point, and also once all instructions have been executed--if no branching took place.
    end_block: Option<ir::Block>,

    /// Value representing a pointer to `result: Result<(), EnvironException>`
    result_ptr_val: Value,
}

impl<'a, MC: MemoryConfig, JSA: JitStateAccess> Builder<'a, MC, JSA> {
    /// Create a new block builder.
    ///
    /// The function constructed after compilation takes
    /// `core_ptr`, `instr_pc` & `steps_ptr` as arguments.
    pub(super) fn new(
        ptr: ir::Type,
        mut builder: FunctionBuilder<'a>,
        jsa_call: JsaCalls<'a, MC, JSA>,
    ) -> Self {
        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // first param ignored
        let core_ptr_val = builder.block_params(entry_block)[1];
        let pc_val = X64(builder.block_params(entry_block)[2]);
        let steps_ptr_val = builder.block_params(entry_block)[3];
        let result_ptr_val = builder.block_params(entry_block)[4];
        // last param ignored

        Self {
            ptr,
            builder,
            jsa_call,
            core_ptr_val,
            steps_ptr_val,
            result_ptr_val,
            dynamic: DynamicValues::new(pc_val),
            end_block: None,
        }
    }

    /// Construct the end block - which writes the updated `pc` and `steps`
    /// back to the state.
    ///
    /// Since the end block can be jumped to from multiple places, it takes
    /// `pc` and `steps` as dynamic-parameters. These are provided by the caller.
    fn finalise_end_block(&mut self, end_block: ir::Block) {
        if self.end_block.is_some() {
            return;
        }

        self.builder.switch_to_block(end_block);
        // We will pass the instr_pc & steps as parameters
        let pc_val = self.builder.append_block_param(end_block, I64);
        let steps_val = self.builder.append_block_param(end_block, I64);

        // write steps back to the `steps: &mut usize` reference.
        self.builder
            .ins()
            .store(MemFlags::trusted(), steps_val, self.steps_ptr_val, 0);

        // write the final pc to the state.
        self.jsa_call
            .pc_write(&mut self.builder, self.core_ptr_val, X64(pc_val));

        self.builder.ins().return_(&[]);

        self.end_block = Some(end_block);
    }

    /// Consume the builder, allowing for the function under construction to be [`finalised`].
    ///
    /// [`finalised`]: super::JIT::finalise
    pub(super) fn end(mut self) {
        self.jump_to_end();
        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    pub(super) fn end_unconditional_exception(mut self) {
        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    /// Clear the builder context on failure.
    pub(super) fn fail(mut self) {
        // On failure, the context must be cleared to ensure a clean context for the next block to
        // be compiled.

        // Before clearing the context, we need to ensure that
        // the block compiled so far matches the ABI of the function
        //
        // In this case, we must ensure that we explicitly declare
        // a lack of return values.
        self.builder.ins().return_(&[]);

        // Clearing the context is done via `finalize`, which internally clears the
        // buffers to allow re-use.
        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    /// Complete a step, updating the program counter in the process.
    ///
    /// Returns `false` if an unconditional exit from the block occurs, in which case compilation
    /// should be finalised without proceeding to the following instruction.
    pub(super) fn complete_step<U: Into<block_state::PCUpdate>>(&mut self, pc_update: U) -> bool {
        self.dynamic.complete_step(pc_update)
    }

    /// Jump from the current block to the end block, exiting the function.
    fn jump_to_end(&mut self) {
        // update steps taken so far
        let steps_val =
            self.builder
                .ins()
                .load(self.ptr, ir::MemFlags::trusted(), self.steps_ptr_val, 0);
        let steps_val = self
            .builder
            .ins()
            .iadd_imm(steps_val, self.dynamic.steps() as i64);

        // get the new value of the pc to write back to the state
        let pc_val = self.dynamic.read_pc(&mut self.builder);

        let end_block = self
            .end_block
            .unwrap_or_else(|| self.builder.create_block());

        self.builder.ins().jump(end_block, &[pc_val.0, steps_val]);
        self.finalise_end_block(end_block)
    }

    /// Handle an exception that has occurred.
    fn handle_exception(&mut self, exception_ptr: Value) {
        let current_pc = self.pc_read();

        let outcome = self.jsa_call.handle_exception(
            &mut self.builder,
            self.core_ptr_val,
            exception_ptr,
            self.result_ptr_val,
            current_pc,
        );

        // if handled -> jump to end with updated pc_ptr. Add steps += 1
        self.exit_on_branch(outcome.handled, |builder| {
            builder.complete_step(PCUpdate::Absolute(outcome.new_pc));
        });

        // if !handled -> exit directly; environ needs to be consulted.
        //                pc has been committed
        self.jump_to_end();
    }

    /// Branch and exit if the given condition holds.
    ///
    /// When needing to exit the 'block-cache' block - on either a `run_branch` or due to
    /// an exception occuring, we jump to the end of the compiled function.
    ///
    /// There is a little cleanup to do, before doing so, however: potentially needing to
    /// complete the current step.
    ///
    /// Semantically, this function returns the caller into the context of the 'fallthrough'
    /// block - ie, the where the branch condition fails.
    fn exit_on_branch(&mut self, cond: Value, on_branching: impl FnOnce(&mut Self)) {
        let on_branch = self.builder.create_block();
        let fallthrough = self.builder.create_block();

        self.builder
            .ins()
            .brif(cond, on_branch, &[], fallthrough, &[]);

        // both IR blocks need access to the dynamic values at this point in time.
        // These are modified by the jump to the end block below, and possibly by the
        // `on_branching` function.
        let snapshot = self.dynamic;

        self.builder.switch_to_block(on_branch);

        on_branching(self);
        self.jump_to_end();
        self.builder.seal_block(on_branch);

        // Restore the dynamic values to the branching point, for the fallthrough block.
        self.dynamic = snapshot;
        self.builder.switch_to_block(fallthrough);
    }
}

impl<MC: MemoryConfig, JSA: JitStateAccess> ICB for Builder<'_, MC, JSA> {
    type XValue = X64;
    type IResult<Value> = Option<Value>;

    /// An `I8` width value.
    type Bool = Value;

    fn bool_and(&mut self, lhs: Self::Bool, rhs: Self::Bool) -> Self::Bool {
        self.builder.ins().band(lhs, rhs)
    }

    type XValue32 = X32;

    fn narrow(&mut self, value: Self::XValue) -> Self::XValue32 {
        X32(self.builder.ins().ireduce(I32, value.0))
    }

    fn extend_signed(&mut self, value: Self::XValue32) -> Self::XValue {
        X64(self.builder.ins().sextend(I64, value.0))
    }

    fn extend_unsigned(&mut self, value: Self::XValue32) -> Self::XValue {
        X64(self.builder.ins().uextend(I64, value.0))
    }

    fn mul_high(
        &mut self,
        lhs: Self::XValue,
        rhs: Self::XValue,
        mul_high_type: MulHighType,
    ) -> Self::XValue {
        let (lhs, rhs) = match mul_high_type {
            MulHighType::Signed => (
                self.builder.ins().sextend(I128, lhs.0),
                self.builder.ins().sextend(I128, rhs.0),
            ),
            MulHighType::Unsigned => (
                self.builder.ins().uextend(I128, lhs.0),
                self.builder.ins().uextend(I128, rhs.0),
            ),
            MulHighType::SignedUnsigned => (
                self.builder.ins().sextend(I128, lhs.0),
                self.builder.ins().uextend(I128, rhs.0),
            ),
        };
        let result = self.builder.ins().imul(lhs, rhs);
        let (_low, high) = self.builder.ins().isplit(result);

        X64(high)
    }

    fn xregister_read_nz(&mut self, reg: NonZeroXRegister) -> Self::XValue {
        JSA::ir_xreg_read(
            &mut self.jsa_call,
            &mut self.builder,
            self.core_ptr_val,
            reg,
        )
    }

    fn xregister_write_nz(&mut self, reg: NonZeroXRegister, value: Self::XValue) {
        JSA::ir_xreg_write(
            &mut self.jsa_call,
            &mut self.builder,
            self.core_ptr_val,
            reg,
            value,
        )
    }

    fn xvalue_of_imm(&mut self, imm: i64) -> Self::XValue {
        X64(self.builder.ins().iconst(I64, imm))
    }

    fn xvalue_from_bool(&mut self, value: Self::Bool) -> Self::XValue {
        // unsigned extension works as boolean can never be negative (only 0 or 1)
        X64(self.builder.ins().uextend(I64, value))
    }

    /// Read the effective current program counter by adding `self.pc_offset` (due to instructions
    /// already lowered into this block) to `self.pc_val` (the initial value of the program counter
    /// for the block).
    fn pc_read(&mut self) -> Self::XValue {
        self.dynamic.read_pc(&mut self.builder)
    }

    fn branch(
        &mut self,
        condition: Self::Bool,
        offset: i64,
        instr_width: InstrWidth,
    ) -> ProgramCounterUpdate<Self::XValue> {
        self.exit_on_branch(condition, |builder| {
            builder.complete_step(block_state::PCUpdate::Offset(offset));
        });

        ProgramCounterUpdate::Next(instr_width)
    }

    fn branch_merge<Phi: PhiValue, OnTrue, OnFalse>(
        &mut self,
        cond: Self::Bool,
        true_branch: OnTrue,
        false_branch: OnFalse,
    ) -> Phi::IcbValue<Self>
    where
        OnTrue: FnOnce(&mut Self) -> Phi::IcbValue<Self>,
        OnFalse: FnOnce(&mut Self) -> Phi::IcbValue<Self>,
    {
        // Set up parallel blocks.
        let true_block = self.builder.create_block();
        let false_block = self.builder.create_block();

        // set up common post-block.
        let post_block = self.builder.create_block();

        // Add a parameter to the post-block for each parameter returned by the closures.
        Phi::IR_TYPES.iter().for_each(|v| {
            self.builder.append_block_param(post_block, *v);
        });

        self.builder
            .ins()
            .brif(cond, true_block, &[], false_block, &[]);

        // both IR blocks need access to the dynamic values at this point in time.
        // These can be modified by the `true_branch` and `false_branch` functions.
        let snapshot = self.dynamic;
        self.builder.switch_to_block(true_block);

        let res_val = Phi::to_ir_vals(true_branch(self));
        self.builder
            .ins()
            .jump(post_block, &res_val.into_iter().collect::<Vec<_>>());
        self.builder.seal_block(true_block);

        self.dynamic = snapshot;
        self.builder.switch_to_block(false_block);

        let res_val = Phi::to_ir_vals(false_branch(self));
        self.builder
            .ins()
            .jump(post_block, &res_val.into_iter().collect::<Vec<_>>());
        self.builder.seal_block(false_block);

        // The post-block is the common exit point for both branches.
        self.builder.switch_to_block(post_block);
        let params = self.builder.block_params(post_block);

        Phi::from_ir_vals(params)
    }

    fn atomic_access_fault_guard(
        &mut self,
        address: Self::XValue,
        width: LoadStoreWidth,
    ) -> Self::IResult<()> {
        let width = self.xvalue_of_imm(width as i64);
        let remainder = address.modulus(width, self);

        // The steps of taking the comparison are technically not needed, as cranelift will
        // treat any non-zero value as a take-branch (i.e. raise exception) value, so we could
        // pass the remainder directly. However for completeness and clarity, we are keeping the
        // comparison here.
        let zero = self.xvalue_of_imm(0);
        let not_aligned = remainder.compare(zero, Predicate::NotEqual, self);
        let errno = AtomicAccessGuard::new(not_aligned, address.0);
        errno.handle(self);

        Some(())
    }

    fn ok<Value>(&mut self, val: Value) -> Self::IResult<Value> {
        Some(val)
    }

    fn err_illegal_instruction<In>(&mut self) -> Self::IResult<In> {
        let exception_ptr = self
            .jsa_call
            .raise_illegal_instruction_exception(&mut self.builder);

        self.handle_exception(exception_ptr);

        None
    }

    fn map<Value, Next, F>(res: Self::IResult<Value>, f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Next,
    {
        res.map(f)
    }

    fn and_then<Value, Next, F>(res: Self::IResult<Value>, f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Self::IResult<Next>,
    {
        match res {
            Some(value) => f(value),
            None => None,
        }
    }

    fn ecall(&mut self) -> Self::IResult<ProgramCounterUpdate<Self::XValue>> {
        let exception_ptr = self.jsa_call.ecall(&mut self.builder, self.core_ptr_val);

        self.handle_exception(exception_ptr);

        None
    }

    fn main_memory_store(
        &mut self,
        phys_address: Self::XValue,
        value: Self::XValue,
        width: LoadStoreWidth,
    ) -> Self::IResult<()> {
        let errno = self.jsa_call.memory_store(
            &mut self.builder,
            self.core_ptr_val,
            phys_address,
            value,
            width,
        );

        errno.handle(self);

        Some(())
    }

    fn main_memory_load(
        &mut self,
        phys_address: Self::XValue,
        signed: bool,
        width: LoadStoreWidth,
    ) -> Self::IResult<Self::XValue> {
        let errno = self.jsa_call.memory_load(
            &mut self.builder,
            self.core_ptr_val,
            phys_address,
            signed,
            width,
        );

        let res = errno.handle(self);

        Some(res)
    }
}

impl From<Predicate> for IntCC {
    fn from(value: Predicate) -> Self {
        match value {
            Predicate::Equal => IntCC::Equal,
            Predicate::NotEqual => IntCC::NotEqual,
            Predicate::LessThanSigned => IntCC::SignedLessThan,
            Predicate::LessThanUnsigned => IntCC::UnsignedLessThan,
            Predicate::LessThanOrEqualSigned => IntCC::SignedLessThanOrEqual,
            Predicate::GreaterThanSigned => IntCC::SignedGreaterThan,
            Predicate::GreaterThanOrEqualSigned => IntCC::SignedGreaterThanOrEqual,
            Predicate::GreaterThanOrEqualUnsigned => IntCC::UnsignedGreaterThanOrEqual,
        }
    }
}
