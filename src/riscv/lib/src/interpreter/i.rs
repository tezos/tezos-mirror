// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of Risc-V's 32 & 64 bit I extensions for RISC-V
//!
//! Chapter 5,1 - Unprivileged spec

use crate::{instruction_context::ICB, machine_state::registers::NonZeroXRegister};

/// Perform `val(rs1) + val(rs2)` and store the result in `rd`
///
/// Relevant RISC-V opcodes:
/// - ADD
/// - C.ADD
pub fn run_add(
    icb: &mut impl ICB,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read(rs1);
    let rhs = icb.xregister_read(rs2);
    // Wrapped addition in two's complement behaves the same for signed and unsigned
    let result = icb.xvalue_wrapping_add(lhs, rhs);
    icb.xregister_write(rd, result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::machine_state::MachineCoreState;
    use crate::{
        backend_test, create_state,
        machine_state::{MachineCoreStateLayout, main_memory::tests::T1K, registers::nz},
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

            state.hart.xregisters.write_nz(nz::a0, rs1);
            state.hart.xregisters.write_nz(nz::t0, imm as u64);

            run_add(&mut state, nz::a0, nz::t0, nz::a0);
            assert_eq!(state.hart.xregisters.read_nz(nz::a0), res);
        }
    });
}
