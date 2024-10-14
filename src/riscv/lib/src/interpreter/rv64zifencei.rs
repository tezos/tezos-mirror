// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of Zifencei extension for RISC-V

use crate::{
    machine_state::{bus::main_memory::MainMemoryLayout, AccessType, CacheLayouts, MachineState},
    state_backend,
};

impl<ML, CL, M> MachineState<ML, CL, M>
where
    ML: MainMemoryLayout,
    CL: CacheLayouts,
    M: state_backend::ManagerReadWrite,
{
    /// Execute a `fence.i` instruction.
    #[inline(always)]
    pub fn run_fencei(&mut self) {
        self.core
            .translation_cache
            .invalidate([AccessType::Instruction]);
        self.instruction_cache.invalidate();
        self.block_cache.invalidate();
    }
}
