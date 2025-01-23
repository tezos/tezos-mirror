// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Builder for turning [instructions] into functions.
//!
//! [instructions]: crate::machine_state::instruction::Instruction

use super::state_access::{JitStateAccess, JsaCalls};
use crate::machine_state::main_memory::{Address, MainMemoryLayout};
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
    /// [`finalised`]: JIT::finalise
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
}
