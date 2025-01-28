// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Builder for turning [instructions] into functions.
//!
//! [instructions]: crate::machine_state::instruction::Instruction

use crate::machine_state::main_memory::Address;
use cranelift::{
    codegen::ir::{InstBuilder, MemFlags, Type, Value},
    frontend::FunctionBuilder,
};

/// Builder context used when lowering individual instructions within a block.
pub(super) struct Builder<'a> {
    /// Cranelift function builder
    pub(super) builder: FunctionBuilder<'a>,
    /// The IR-type of pointers on the current native platform
    pub(super) ptr: Type,
    /// Value representing a pointer to `steps: usize`
    pub(super) steps_ptr_val: Value,
    /// The number of steps taken within the function
    pub(super) steps: usize,
    /// Value representing the initial value of `instr_pc`
    pub(super) pc_val: Value,
    /// The static offset so far of the `instr_pc`
    pub(super) pc_offset: Address,
}

impl<'a> Builder<'a> {
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
        let pc = self
            .builder
            .ins()
            .iadd_imm(self.pc_val, self.pc_offset as i64);
        self.builder.ins().return_(&[pc]);

        self.builder.finalize();
    }
}
