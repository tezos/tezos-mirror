// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of Zifencei extension for RISC-V

use crate::{
    machine_state::{
        bus::main_memory::MainMemoryLayout, instruction_cache::InstructionCacheLayout, AccessType,
        MachineState,
    },
    state_backend,
};

impl<ML, ICL, M> MachineState<ML, ICL, M>
where
    ML: MainMemoryLayout,
    ICL: InstructionCacheLayout,
    M: state_backend::ManagerReadWrite,
{
    /// Execute a `fence.i` instruction.
    #[inline(always)]
    pub fn run_fencei(&mut self) {
        self.translation_cache.invalidate([AccessType::Instruction]);
        self.instruction_cache.invalidate();
    }
}
