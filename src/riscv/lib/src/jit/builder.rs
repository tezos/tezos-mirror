// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Builder for turning [instructions] into functions.
//!
//! [instructions]: crate::machine_state::instruction::Instruction

pub(super) mod arithmetic;
pub(super) mod block_state;

use cranelift::codegen::ir;
use cranelift::codegen::ir::InstBuilder;
use cranelift::codegen::ir::MemFlags;
use cranelift::codegen::ir::Type;
use cranelift::codegen::ir::Value;
use cranelift::codegen::ir::condcodes::IntCC;
use cranelift::codegen::ir::types::I64;
use cranelift::frontend::FunctionBuilder;

use self::block_state::DynamicValues;
use super::state_access::JitStateAccess;
use super::state_access::JsaCalls;
use crate::instruction_context::ICB;
use crate::instruction_context::Predicate;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::registers::NonZeroXRegister;
use crate::parser::instruction::InstrWidth;

#[derive(Copy, Clone, Debug)]
pub struct X64(pub Value);

/// Builder context used when lowering individual instructions within a block.
pub(super) struct Builder<'a, MC: MemoryConfig, JSA: JitStateAccess> {
    /// Cranelift function builder
    builder: FunctionBuilder<'a>,

    /// Helpers for calling locally imported [JitStateAccess] methods.
    jsa_call: JsaCalls<'a, MC, JSA>,

    /// The IR-type of pointers on the current native platform
    ptr: Type,

    /// Value representing a pointer to `MachineCoreState<MC, JSA>`
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

        let core_ptr_val = builder.block_params(entry_block)[0];
        let pc_val = X64(builder.block_params(entry_block)[1]);
        let steps_ptr_val = builder.block_params(entry_block)[2];

        Self {
            ptr,
            builder,
            jsa_call,
            core_ptr_val,
            steps_ptr_val,
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
}

impl<'a, MC: MemoryConfig, JSA: JitStateAccess> ICB for Builder<'a, MC, JSA> {
    type XValue = X64;
    type IResult<Value> = Value;

    /// An `I8` width value.
    type Bool = Value;

    fn xregister_read_nz(&mut self, reg: NonZeroXRegister) -> Self::XValue {
        self.jsa_call
            .xreg_read(&mut self.builder, self.core_ptr_val, reg)
    }

    fn xregister_write_nz(&mut self, reg: NonZeroXRegister, value: Self::XValue) {
        self.jsa_call
            .xreg_write(&mut self.builder, self.core_ptr_val, reg, value)
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

    fn xvalue_compare(
        &mut self,
        comparison: crate::instruction_context::Predicate,
        lhs: Self::XValue,
        rhs: Self::XValue,
    ) -> Self::Bool {
        // icmp returns 1 if the condition holds, 0 if it does not.
        //
        // This matches the required semantics of bool - namely that it coerces to XValue with
        // - true => 1
        // - false => 0
        //
        // See
        // <https://docs.rs/cranelift-codegen/0.117.2/cranelift_codegen/ir/trait.InstBuilder.html#method.icmp>
        self.builder.ins().icmp(comparison, lhs.0, rhs.0)
    }

    fn branch(
        &mut self,
        condition: Self::Bool,
        offset: i64,
        instr_width: InstrWidth,
    ) -> ProgramCounterUpdate<Self::XValue> {
        let branch_block = self.builder.create_block();
        let fallthrough_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(condition, branch_block, &[], fallthrough_block, &[]);

        let snapshot = self.dynamic;

        // Handle branching block
        self.builder.switch_to_block(branch_block);

        self.complete_step(block_state::PCUpdate::Offset(offset));
        self.jump_to_end();

        self.builder.seal_block(branch_block);

        // Continue on the fallthrough block
        self.dynamic = snapshot;
        self.builder.switch_to_block(fallthrough_block);

        ProgramCounterUpdate::Next(instr_width)
    }

    fn ok<Value>(&mut self, val: Value) -> Self::IResult<Value> {
        val
    }

    fn map<Value, Next, F>(_res: Self::IResult<Value>, _f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Next,
    {
        todo!("RV-415: support fallible pathways in JIT")
    }

    fn and_then<Value, Next, F>(_res: Self::IResult<Value>, _f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Self::IResult<Next>,
    {
        todo!("RV-415: support fallible pathways in JIT")
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
