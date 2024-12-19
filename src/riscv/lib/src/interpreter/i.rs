// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of Risc-V's 32 & 64 bit I extensions for RISC-V
//!
//! Chapter 2,4 - Unprivileged spec

use crate::{instruction_context::ICB, machine_state::registers::XRegister};

/// `ADD` R-type instruction
///
/// Perform val(rs1) + val(rs2) and store the result in `rd`
pub fn run_add(icb: &mut impl ICB, rs1: XRegister, rs2: XRegister, rd: XRegister) {
    let lhs = icb.xregister_read(rs1);
    let rhs = icb.xregister_read(rs2);
    let result = icb.xvalue_wrapping_add(lhs, rhs);
    icb.xregister_write(rd, result)
}
