// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// Allow dead code because I don't want to delete or add these register shortcuts
// everytime I change code elsewhere.
#![allow(dead_code)]

// Named register mapping, named after RISC-V Spec ABI
// E.g. a7 => x17
pub const A0: u64 = 10;
pub const A1: u64 = 11;
pub const A2: u64 = 12;
pub const A3: u64 = 13;
pub const A4: u64 = 14;
pub const A5: u64 = 15;
pub const A6: u64 = 16;
pub const A7: u64 = 17;
