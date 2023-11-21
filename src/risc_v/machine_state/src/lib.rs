// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![deny(rustdoc::broken_intra_doc_links)]

pub mod backend;
pub mod memory_backend;
pub mod registers;

/// RISC-V hart state
pub struct HartState<M: backend::Manager> {
    /// Integer registers
    pub xregisters: registers::XRegisters<M>,

    /// Floating-point number registers
    pub fregisters: registers::FRegisters<M>,
}

/// Layout of [HartState]
pub type HartStateLayout = (registers::XRegistersLayout, registers::FRegistersLayout);

impl<M: backend::Manager> HartState<M> {
    /// Bind the hart state to the given allocated space.
    pub fn new_in(space: backend::AllocatedOf<HartStateLayout, M>) -> Self {
        HartState {
            xregisters: registers::XRegisters::new_in(space.0),
            fregisters: registers::FRegisters::new_in(space.1),
        }
    }
}

#[cfg(test)]
mod tests {}
