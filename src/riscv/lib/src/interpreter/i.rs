// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend_test, create_state,
        machine_state::{
            main_memory::tests::T1K,
            registers::{a0, t0},
            MachineCoreState, MachineCoreStateLayout,
        },
    };

    backend_test!(test_add, F, {
        let imm_rs1_res = [
            (0_i64, 0_u64, 0_u64),
            (0, 0xFFF0_0420, 0xFFF0_0420),
            (-1, 0, 0xFFFF_FFFF_FFFF_FFFF),
            (1_000_000, -123_000_987_i64 as u64, -122_000_987_i64 as u64),
            (1_000_000, 123_000_987, 124_000_987),
            (-1, -321_000_000_000_i64 as u64, -321_000_000_001_i64 as u64),
        ];

        for (imm, rs1, res) in imm_rs1_res {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);

            state.hart.xregisters.write(a0, rs1);
            state.hart.xregisters.write(t0, imm as u64);

            run_add(&mut state, a0, t0, a0);
            assert_eq!(state.hart.xregisters.read(a0), res);
        }
    });
}
