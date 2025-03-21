// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! State that is kept per Cranelift-IR block.

use cranelift::codegen::ir::InstBuilder;
use cranelift::frontend::FunctionBuilder;

use super::X64;
use crate::machine_state::ProgramCounterUpdate;

/// Program Counter update, within the context of JIT-compilation.
///
/// This is distinct from [`ProgramCounterUpdate`] - in that the offset
/// is not restricted to merely instruction widths. Namely, branching
/// instructions also produce such a PC-relative offset when the condition
/// holds.
#[derive(Debug)]
pub enum PCUpdate {
    /// Relative offset to the current program counter.
    Offset(i64),
    /// The program counter is set to an absolute address, irrelevant to the
    /// current value of the program counter.
    Absolute(X64),
}

impl From<ProgramCounterUpdate<X64>> for PCUpdate {
    fn from(value: ProgramCounterUpdate<X64>) -> Self {
        match value {
            ProgramCounterUpdate::Next(width) => Self::Offset(width as i64),
            ProgramCounterUpdate::Set(address) => Self::Absolute(address),
        }
    }
}

/// Dynamic values that are updated during lowering of instructions.
///
/// This is especially relevant when dealing with code that may branch
/// (including for error handling) - due to switching between cranelift blocks.
///
/// Whilst in the context of one block (e.g. the branching block), these values may
/// be updated - but *must not* impact the values of the non-branching block while
/// doing so. Keeping all such state together, simplifies snapshotting & restoring
/// these values when switching between such blocks.
///
/// The non-branching block, can otherwise be considered as the 'fallthrough' block.
/// That is to say, the fallthrough block is the block taken when execution 'falls through'
/// a branching instruction--the branch is not taken.
#[derive(Debug, Clone, Copy)]
pub struct DynamicValues {
    /// The number of steps taken within the current compilation context.
    steps: usize,

    /// Value representing the last-updated value of `instr_pc`.
    ///
    /// Whenever the program counter is read, we implicitly update this value
    /// to take into account the `pc_offset` accrued (e.g. by having lowered a few instructions).
    ///
    /// Doing this offset-mechanism allows us to avoid updating the `pc` on every step, effectively
    /// updating it in one go, only when it matters.
    pc_val: X64,

    /// The current offset of the pc, as a result of steps taken
    /// so far.
    pc_offset: i64,
}

impl DynamicValues {
    /// Create a new set of values, given an initial program counter.
    pub fn new(pc_val: X64) -> Self {
        Self {
            pc_val,
            steps: 0,
            pc_offset: 0,
        }
    }

    /// The current value of the program counter may be offset from the original
    /// instruction counter given during construction.
    pub fn read_pc(&mut self, builder: &mut FunctionBuilder<'_>) -> X64 {
        if self.pc_offset == 0 {
            return self.pc_val;
        }

        let new_pc = builder.ins().iadd_imm(self.pc_val.0, self.pc_offset);

        self.pc_val = X64(new_pc);
        self.pc_offset = 0;

        X64(new_pc)
    }

    /// Complete a step, updating the program counter in the process.
    ///
    /// Returns `true` if compilation should continue with the next instructions.
    /// If `false` is returned, this indicates an unconditional exit from the instruction
    /// block and compilation can be finalised.
    pub fn complete_step<U: Into<PCUpdate>>(&mut self, pc_update: U) -> bool {
        self.steps += 1;

        match pc_update.into() {
            PCUpdate::Offset(offset) => {
                self.pc_offset = self.pc_offset.wrapping_add(offset);
                true
            }
            PCUpdate::Absolute(address) => {
                self.pc_offset = 0;
                self.pc_val = address;
                false
            }
        }
    }

    /// The number of steps that have been taken thus far in the compilation process,
    /// where 'step' maps to the lowering of an instruction.
    pub fn steps(&self) -> usize {
        self.steps
    }
}
