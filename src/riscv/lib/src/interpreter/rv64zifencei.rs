// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of Zifencei extension for RISC-V

use crate::{
    machine_state::{
        AccessType, CacheLayouts, MachineState, block_cache::bcall::Block,
        main_memory::MainMemoryLayout,
    },
    state_backend,
};

impl<ML, CL, B, M> MachineState<ML, CL, B, M>
where
    ML: MainMemoryLayout,
    CL: CacheLayouts,
    B: Block<ML, M>,
    M: state_backend::ManagerReadWrite,
{
    /// Execute a `fence.i` instruction.
    #[inline(always)]
    pub fn run_fencei(&mut self) {
        self.core
            .translation_cache
            .invalidate([AccessType::Instruction]);
        self.block_cache.invalidate();
    }
}
