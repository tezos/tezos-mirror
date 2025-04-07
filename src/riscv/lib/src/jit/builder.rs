// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Builder for turning [instructions] into functions.
//!
//! [instructions]: crate::machine_state::instruction::Instruction

use super::state_access::{JitStateAccess, JsaCalls};
use crate::{
    instruction_context::ICB,
    machine_state::{
        main_memory::{Address, MainMemoryLayout},
        registers::NonZeroXRegister as XRegister,
    },
};
use cranelift::{
    codegen::ir::{InstBuilder, MemFlags, Type, Value},
    frontend::FunctionBuilder,
};

/// Builder context used when lowering individual instructions within a block.
pub(super) struct Builder<'a, ML: MainMemoryLayout, JSA: JitStateAccess> {
    /// Cranelift function builder
    pub builder: FunctionBuilder<'a>,

    /// Helpers for calling locally imported [JitStateAccess] methods.
    pub jsa_call: JsaCalls<'a, ML, JSA>,

    /// The IR-type of pointers on the current native platform
    pub ptr: Type,

    /// Value representing a pointer to `MachineCoreState<ML, JSA>`
    pub core_ptr_val: Value,

    /// Value representing a pointer to `steps: usize`
    pub steps_ptr_val: Value,

    /// The number of steps taken within the function
    pub steps: usize,

    /// Value representing the initial value of `instr_pc`
    pub pc_val: Value,

    /// The static offset so far of the `instr_pc`
    pub pc_offset: Address,
}

impl<'a, ML: MainMemoryLayout, JSA: JitStateAccess> Builder<'a, ML, JSA> {
    /// Consume the builder, allowing for the function under construction to be [`finalised`].
    ///
    /// [`finalised`]: super::JIT::finalise
    pub(super) fn end(mut self) {
        // flush steps
        let steps = self
            .builder
            .ins()
            .load(self.ptr, MemFlags::trusted(), self.steps_ptr_val, 0);
        let steps = self.builder.ins().iadd_imm(steps, self.steps as i64);
        self.builder
            .ins()
            .store(MemFlags::trusted(), steps, self.steps_ptr_val, 0);

        // flush pc
        let pc_val = self
            .builder
            .ins()
            .iadd_imm(self.pc_val, self.pc_offset as i64);
        self.jsa_call
            .pc_write(&mut self.builder, self.core_ptr_val, pc_val);

        self.builder.ins().return_(&[]);
        self.builder.finalize();
    }

    /// Clear the builder context on failure.
    pub(super) fn fail(self) {
        // On failure, the context must be cleared to ensure a clean context for the next
        // block to be compiled. This is done via `finalize`, which internally clears the
        // buffers to allow re-use.
        self.builder.finalize();
    }
}

impl<'a, ML: MainMemoryLayout, JSA: JitStateAccess> ICB for Builder<'a, ML, JSA> {
    type XValue = Value;
    type IResult<Value> = Value;

    fn xregister_read(&mut self, reg: XRegister) -> Self::XValue {
        self.jsa_call
            .xreg_read(&mut self.builder, self.core_ptr_val, reg)
    }

    fn xregister_write(&mut self, reg: XRegister, value: Self::XValue) {
        self.jsa_call
            .xreg_write(&mut self.builder, self.core_ptr_val, reg, value)
    }

    fn xvalue_wrapping_add(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        // wrapping add; does not depend on whether operands are signed
        self.builder.ins().iadd(lhs, rhs)
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
