// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

/// RISC-V parsed instructions. Along with legal instructions, potentially
/// illegal instructions are parsed as `Unknown` or `UnknownCompressed`.
/// These instructions are successfully parsed, but must not be interpreted.
#[derive(Debug, PartialEq)]
pub enum Instr {
    Unknown { instr: u32 },
    UnknownCompressed { instr: u16 },
}
