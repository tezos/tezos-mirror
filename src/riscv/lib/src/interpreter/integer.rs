// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of integer arithmetic operations for RISC-V over the ICB.
// TODO: RV 519: Update remaining 'neg' handler in the file to work over the ICB

use crate::instruction_context::ICB;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegisters;
use crate::state_backend as backend;

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
    /// Moves the two's complement representation of `rs1` into `rd`.
    ///
    /// Relevant RISC-V opcodes:
    /// - SUB
    pub fn run_neg(&mut self, rd_rs1: NonZeroXRegister, rs2: NonZeroXRegister) {
        let result = 0_u64.wrapping_sub(self.read_nz(rs2));
        self.write_nz(rd_rs1, result)
    }
}

/// Copies the value in register `rs2` into register `rd_rs1`.
///
/// Relevant RISC-V opcodes:
/// - C.MV
/// - ADD
/// - SUB
/// - OR
/// - XOR
/// - SLL
/// - SRL
/// - SRA
pub fn run_mv(icb: &mut impl ICB, rd_rs1: NonZeroXRegister, rs2: NonZeroXRegister) {
    let rs2_val = icb.xregister_read_nz(rs2);
    icb.xregister_write_nz(rd_rs1, rs2_val)
}

/// Does nothing.
///
/// Relevant RISC-V opcodes:
/// - C.NOP
/// - ADDI
/// - C.ADDI4SPN
/// - C.ANDI
/// - C.SRLI
/// - C.SRAI
/// - C.AND
/// - C.OR
/// - C.XOR
/// - BNE
/// - C.BNEZ
/// - C.SUB
pub fn run_nop(_icb: &mut impl ICB) {}

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
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);
    // Wrapped addition in two's complement behaves the same for signed and unsigned
    let result = icb.xvalue_wrapping_add(lhs, rhs);
    icb.xregister_write_nz(rd, result)
}

/// Saves in `rd` the bitwise AND between the value in `rs1` and `rs2`
///
/// Relevant RISC-V opcodes:
/// - `AND`
/// - `C.AND`
pub fn run_and(
    icb: &mut impl ICB,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);

    let res = icb.xvalue_bitwise_and(lhs, rhs);
    icb.xregister_write_nz(rd, res);
}

/// Saves in `rd` the bitwise OR between the value in `rs1` and `rs2`
///
/// Relevant RISC-V opcodes:
/// - `OR`
/// - `C.OR`
pub fn run_or(
    icb: &mut impl ICB,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);

    let res = icb.xvalue_bitwise_or(lhs, rhs);
    icb.xregister_write_nz(rd, res);
}

/// Add `imm` to val(rs1) and store the result in `rd`
///
/// Relevant RISC-V opcodes:
/// - `ADDI`
/// - `C.ADDI`
/// - `C.ADDI4SPN`
/// - `C.ADDI16SP`
pub fn run_addi(icb: &mut impl ICB, imm: i64, rs1: NonZeroXRegister, rd: NonZeroXRegister) {
    // Return the lower XLEN (64 bits in our case) bits of the addition
    // Irrespective of sign, the result is the same, casting to u64 for addition;
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(imm);
    let result = icb.xvalue_wrapping_add(lhs, rhs);
    icb.xregister_write_nz(rd, result)
}

/// Saves in `rd` the bitwise AND between the value in `rs1` and `imm`
///
/// Relevant RISC-V opcodes:
/// - `ANDI`
/// - `C.ANDI`
pub fn run_andi(icb: &mut impl ICB, imm: i64, rs1: NonZeroXRegister, rd: NonZeroXRegister) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(imm);
    let res = icb.xvalue_bitwise_and(lhs, rhs);
    icb.xregister_write_nz(rd, res);
}

#[cfg(test)]
mod tests {
    use crate::backend_test;
    use crate::create_state;
    use crate::interpreter::integer::run_add;
    use crate::interpreter::integer::run_mv;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::nz;
    use crate::machine_state::registers::nz::a0;

    backend_test!(test_negate, F, {
        let rs2val_rd_res = [
            (0_u64, nz::t3, 0_u64),
            (0xFFFF_FFFF_FFF0_0420, nz::t2, 0x0000_0000_000F_FBE0),
            (0xFFFF_FFFF_FFFF_FFFF, nz::t4, 0x0000_0000_0000_0001),
        ];

        for (rs2, rd, res) in rs2val_rd_res {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            state.hart.xregisters.write_nz(a0, rs2);
            state.hart.xregisters.run_neg(rd, nz::a0);
            assert_eq!(state.hart.xregisters.read_nz(rd), res);
        }
    });

    backend_test!(test_add_mv, F, {
        let imm_rs1_res = [
            (0_i64, 0_u64, 0_u64),
            (0, 0xFFF0_0420, 0xFFF0_0420),
            (-1, 0, 0xFFFF_FFFF_FFFF_FFFF),
            (1_000_000, -123_000_987_i64 as u64, -122_000_987_i64 as u64),
            (1_000_000, 123_000_987, 124_000_987),
            (-1, -321_000_000_000_i64 as u64, -321_000_000_001_i64 as u64),
        ];

        for (imm, rs1, res) in imm_rs1_res {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            state.hart.xregisters.write_nz(nz::a3, rs1);
            state.hart.xregisters.write_nz(nz::a4, imm as u64);

            run_add(&mut state, nz::a3, nz::a4, nz::a3);
            assert_eq!(state.hart.xregisters.read_nz(nz::a3), res);
            run_mv(&mut state, nz::a4, nz::a3);
            assert_eq!(state.hart.xregisters.read_nz(nz::a4), res);
        }
    });
}
