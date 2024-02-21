// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::machine_state::bus::{self, main_memory::MainMemoryLayout, Address};
use std::{borrow::Cow, collections::BTreeMap};

/// RISC-V program
pub struct Program<'a> {
    /// Address of the program's entrypoint
    pub entrypoint: Address,

    /// Segments to be written to the main memory
    // Note: `[u8]` owned is `Vec<u8>`. The segment is either re-using an
    // existing slice (e.g. from an open ELF file), or a vector in case the
    // bytes were dynamically created (e.g. chunks for zero-ed memory).
    pub segments: BTreeMap<Address, Cow<'a, [u8]>>,
}

impl<'a> Program<'a> {
    /// Parse the given ELF executable and convert it into our program
    /// representation. The main memory layout `ML` is used to compute the
    /// correct addresses
    pub fn from_elf<ML: MainMemoryLayout>(_elf: &'a [u8]) -> Self {
        let _start_of_main_memory = bus::start_of_main_memory::<ML>();
        todo!("ELF loader not implemented yet")
    }
}
