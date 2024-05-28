// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of Zifencei extension for RISC-V

use crate::{
    machine_state::{bus::main_memory, MachineState},
    state_backend,
};

impl<ML, M> MachineState<ML, M>
where
    ML: main_memory::MainMemoryLayout,
    M: state_backend::Manager,
{
    /// Execute a `fence.i` instruction.
    #[inline(always)]
    pub fn run_fencei(&mut self) {
        // By default RISC-V does not guarantee that instructions written to
        // memory are seen by the Hart. The Hart will cache instruction
        // memory and therefore won't bother re-reading the cache from memory
        // until `fence.i` is called.
        // Because we have no instruction cache yet, this operation is a no-op.
    }
}
