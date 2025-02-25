// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of Zifencei extension for RISC-V

use crate::{
    machine_state::{
        AccessType, CacheLayouts, MachineState, block_cache::bcall::Block, memory::MemoryConfig,
    },
    state_backend,
};

impl<MC, CL, B, M> MachineState<MC, CL, B, M>
where
    MC: MemoryConfig,
    CL: CacheLayouts,
    B: Block<MC, M>,
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
