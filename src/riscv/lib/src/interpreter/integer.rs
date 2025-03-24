// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of integer arithmetic operations for RISC-V over the ICB.

use crate::instruction_context::ICB;
use crate::instruction_context::Predicate;
use crate::instruction_context::Shift;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegister;
use crate::parser::SHIFT_BITMASK;

/// Moves the two's complement representation of `rs1` into `rd`.
///
/// Relevant RISC-V opcodes:
/// - SUB
pub fn run_neg(icb: &mut impl ICB, rd_rs1: NonZeroXRegister, rs2: NonZeroXRegister) {
    let rs2_val = icb.xregister_read_nz(rs2);
    let result = icb.xvalue_negate(rs2_val);
    icb.xregister_write_nz(rd_rs1, result)
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

/// Perform [`val(rs1) - val(rs2)`] and store the result in `rd`
///
/// Relevant RISC-V opcodes:
/// - SUB
/// - C.SUB
pub fn run_sub(
    icb: &mut impl ICB,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);
    // Wrapped subtraction in two's complement behaves the same for signed and unsigned
    let result = icb.xvalue_wrapping_sub(lhs, rhs);
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

/// `SLTI` I-type instruction
///
/// Places the value 1 in `rd` if val(rs1) is less than the immediate
/// when treated as signed integers, 0 otherwise
///
/// Relevant RISC-V opcodes:
/// - SLTI
pub fn run_set_less_than_immediate_signed(
    icb: &mut impl ICB,
    imm: i64,
    rs1: XRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read(rs1);
    let rhs = icb.xvalue_of_imm(imm);

    let cmp = icb.xvalue_compare(Predicate::LessThanSigned, lhs, rhs);
    let res = icb.xvalue_from_bool(cmp);

    icb.xregister_write_nz(rd, res);
}

/// Places the value 1 in `rd` if val(rs1) is less than the immediate
/// when treated as unsigned integers, 0 otherwise
///
/// Relevant RISC-V opcodes:
/// - SLTIU
pub fn run_set_less_than_immediate_unsigned(
    icb: &mut impl ICB,
    imm: i64,
    rs1: XRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read(rs1);
    let rhs = icb.xvalue_of_imm(imm);

    let cmp = icb.xvalue_compare(Predicate::LessThanUnsigned, lhs, rhs);
    let res = icb.xvalue_from_bool(cmp);

    icb.xregister_write_nz(rd, res);
}

/// Places the value 1 in `rd` if val(rs1) < val(rs2)
/// when treated as signed integers, 0 otherwise
///
/// Relevant RISC-V opcodes:
/// - SLT
pub fn run_set_less_than_signed(
    icb: &mut impl ICB,
    rs1: XRegister,
    rs2: XRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read(rs1);
    let rhs = icb.xregister_read(rs2);

    let cmp = icb.xvalue_compare(Predicate::LessThanSigned, lhs, rhs);
    let res = icb.xvalue_from_bool(cmp);

    icb.xregister_write_nz(rd, res);
}

/// Places the value 1 in `rd` if val(rs1) < val(rs2)
/// when treated as unsigned integers, 0 otherwise
///
/// Relevant RISC-V opcodes:
/// - SLTU
pub fn run_set_less_than_unsigned(
    icb: &mut impl ICB,
    rs1: XRegister,
    rs2: XRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read(rs1);
    let rhs = icb.xregister_read(rs2);

    let cmp = icb.xvalue_compare(Predicate::LessThanUnsigned, lhs, rhs);
    let res = icb.xvalue_from_bool(cmp);

    icb.xregister_write_nz(rd, res);
}

/// Multiply val(rs1) with val(rs2) and store the lower 64 bits of the result
/// in register `rd`.
///
/// Relevant RISC-V opcodes:
/// - MUL
pub fn run_mul(
    icb: &mut impl ICB,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);
    let result = icb.xvalue_wrapping_mul(lhs, rhs);
    icb.xregister_write_nz(rd, result)
}

/// Shift bits in `rs1` by `shift_amount = val(rs2)\[5:0\]` in the method specified by `shift`
/// saving the result in `rd`.
///
/// Relevant opcodes:
/// - `SLL`
/// - `SRL`
/// - `SRA`
#[inline]
pub fn run_shift(
    icb: &mut impl ICB,
    shift: Shift,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let bitmask = icb.xvalue_of_imm(SHIFT_BITMASK);
    let rs2val = icb.xregister_read_nz(rs2);
    let shift_amount = icb.xvalue_bitwise_and(rs2val, bitmask);

    let rs1val = icb.xregister_read_nz(rs1);
    let result = icb.xvalue_shift(rs1val, shift_amount, shift);

    icb.xregister_write_nz(rd, result);
}

/// Shift bits in `rs1` by `shift_amount = imm` in the method specified by `shift`
/// saving the result in `rd`.
///
/// Relevant opcodes:
/// - `SLLI`
/// - `SRLI`
/// - `SRAI`
/// - `C.SLLI`
/// - `C.SRLI`
/// - `C.SRAI`
#[inline]
pub fn run_shift_immediate(
    icb: &mut impl ICB,
    shift: Shift,
    imm: i64,
    rs1: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let shift_amount = icb.xvalue_of_imm(imm);
    let rs1val = icb.xregister_read_nz(rs1);
    let result = icb.xvalue_shift(rs1val, shift_amount, shift);

    icb.xregister_write_nz(rd, result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend_test;
    use crate::create_state;
    use crate::instruction_context::Shift;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::a0;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::a2;
    use crate::machine_state::registers::nz;
    use crate::machine_state::registers::t0;
    use crate::machine_state::registers::t1;
    use crate::machine_state::registers::t3;

    backend_test!(test_negate, F, {
        let rs2val_rd_res = [
            (0_u64, nz::t3, 0_u64),
            (0xFFFF_FFFF_FFF0_0420, nz::t2, 0x0000_0000_000F_FBE0),
            (0xFFFF_FFFF_FFFF_FFFF, nz::t4, 0x0000_0000_0000_0001),
        ];

        for (rs2, rd, res) in rs2val_rd_res {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            state.hart.xregisters.write_nz(nz::a0, rs2);
            run_neg(&mut state, rd, nz::a0);
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

    backend_test!(test_add_sub, F, {
        let imm_rs1_rd_res = [
            (0_i64, 0_u64, nz::t3, 0_u64),
            (0, 0xFFF0_0420, nz::t2, 0xFFF0_0420),
            (-1, 0, nz::t4, 0xFFFF_FFFF_FFFF_FFFF),
            (
                1_000_000,
                -123_000_987_i64 as u64,
                nz::a2,
                -122_000_987_i64 as u64,
            ),
            (1_000_000, 123_000_987, nz::a2, 124_000_987),
            (
                -1,
                -321_000_000_000_i64 as u64,
                nz::a1,
                -321_000_000_001_i64 as u64,
            ),
        ];

        for (imm, rs1, rd, res) in imm_rs1_rd_res {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            state.hart.xregisters.write_nz(nz::a0, rs1);
            state.hart.xregisters.write_nz(nz::t0, imm as u64);
            run_addi(&mut state, imm, nz::a0, rd);
            assert_eq!(state.hart.xregisters.read_nz(rd), res);
            run_add(&mut state, nz::a0, nz::t0, nz::a0);
            assert_eq!(state.hart.xregisters.read_nz(nz::a0), res);
            // test sub with: res - imm = rs1 and res - rs1 = imm
            state.hart.xregisters.write_nz(nz::a0, res);
            state.hart.xregisters.write_nz(nz::t0, imm as u64);
            run_sub(&mut state, nz::a0, nz::t0, nz::a1);
            assert_eq!(state.hart.xregisters.read_nz(nz::a1), rs1);
            // now rs1 is in register a1
            run_sub(&mut state, nz::a0, nz::a1, nz::a1);
            assert_eq!(state.hart.xregisters.read_nz(nz::a1), imm as u64);
        }
    });

    backend_test!(test_set_less_than, F, {
        let mut core = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

        let v1_v2_exp_expu = [
            (0, 0, 0, 0),
            (-1_i64 as u64, 0, 1, 0),
            (123123123, -1_i64 as u64, 0, 1),
            (123, 123123, 1, 1),
        ];

        for (val1, val2, expected, expected_unsigned) in v1_v2_exp_expu {
            core.hart.xregisters.write(a1, val1);
            core.hart.xregisters.write(a2, val2);
            super::run_set_less_than_signed(&mut core, a1, a2, nz::t0);
            assert_eq!(core.hart.xregisters.read(t0), expected);
            super::run_set_less_than_unsigned(&mut core, a1, a2, nz::t1);
            assert_eq!(core.hart.xregisters.read(t1), expected_unsigned);
            super::run_set_less_than_immediate_signed(&mut core, val2 as i64, a1, nz::t0);
            assert_eq!(core.hart.xregisters.read(t0), expected);
            super::run_set_less_than_immediate_unsigned(&mut core, val2 as i64, a1, nz::t0);
            assert_eq!(core.hart.xregisters.read(t0), expected_unsigned);
        }
    });

    macro_rules! test_shift_instr {
        ($state:ident, $shift:expr, $imm:expr,
            $rs1:ident, $r1_val:expr,
            $rd:ident, $expected_val:expr
        ) => {
            $state.hart.xregisters.write_nz(nz::$rs1, $r1_val);
            run_shift_immediate(&mut $state, $shift, $imm, nz::$rs1, nz::$rd);
            let new_val = $state.hart.xregisters.read($rd);
            assert_eq!(new_val, $expected_val);
        };
    }

    macro_rules! test_shift_reg_instr {
        ($state:ident, $shift:expr,
            $rs2:ident, $r2_val:expr,
            $rs1:ident, $r1_val:expr,
            $rd:ident, $expected_val:expr
        ) => {
            $state.hart.xregisters.write($rs2, $r2_val);
            $state.hart.xregisters.write($rs1, $r1_val);
            run_shift(&mut $state, $shift, nz::$rs1, nz::$rs2, nz::$rd);
            let new_val = $state.hart.xregisters.read($rd);
            assert_eq!(new_val, $expected_val);
        };
    }

    macro_rules! test_both_shift_instr {
        ($state:ident, $shift_reg:expr,
            $rs2:ident, $r2_val:expr,
            $rs1:ident, $r1_val:expr,
            $rd:ident, $expected_val:expr
        ) => {
            test_shift_instr!(
                $state,
                $shift_reg,
                $r2_val,
                $rs1,
                $r1_val,
                $rd,
                $expected_val
            );
            test_shift_reg_instr!(
                $state,
                $shift_reg,
                $rs2,
                $r2_val,
                $rs1,
                $r1_val,
                $rd,
                $expected_val
            );
        };
    }

    backend_test!(test_shift, F, {
        let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

        // imm = 0
        test_both_shift_instr!(state, Shift::Left, t0, 0, a0, 0x1234_ABEF, a1, 0x1234_ABEF);
        test_both_shift_instr!(
            state,
            Shift::RightUnsigned,
            t1,
            0,
            a0,
            0x1234_ABEF,
            a0,
            0x1234_ABEF
        );
        test_both_shift_instr!(
            state,
            Shift::RightSigned,
            t3,
            0,
            a0,
            0xFFFF_DEAD_1234_ABEF,
            a1,
            0xFFFF_DEAD_1234_ABEF
        );

        // small imm (< 32))
        test_both_shift_instr!(
            state,
            Shift::Left,
            a2,
            20,
            a0,
            0x1234_ABEF,
            a1,
            0x1_234A_BEF0_0000
        );
        test_both_shift_instr!(
            state,
            Shift::RightUnsigned,
            a2,
            10,
            a0,
            0x44_1234_ABEF,
            a1,
            0x1104_8D2A
        );
        test_both_shift_instr!(
            state,
            Shift::RightUnsigned,
            a2,
            14,
            t0,
            -1_i64 as u64,
            a0,
            0x0003_FFFF_FFFF_FFFF
        );
        test_both_shift_instr!(
            state,
            Shift::RightSigned,
            t0,
            10,
            a0,
            0xFFFF_F0FF_FFF0_FF00,
            a0,
            0xFFFF_FFFC_3FFF_FC3F
        );

        // big imm (>= 32))
        test_both_shift_instr!(
            state,
            Shift::Left,
            t0,
            40,
            a0,
            0x1234_ABEF,
            a0,
            0x34AB_EF00_0000_0000
        );
        test_both_shift_instr!(
            state,
            Shift::RightUnsigned,
            a1,
            40,
            a0,
            0x1234_ABEF,
            a0,
            0x0
        );
        test_both_shift_instr!(
            state,
            Shift::RightSigned,
            a2,
            40,
            a0,
            0x8000_FAFF_1234_ABEF,
            a1,
            0xFFFF_FFFF_FF80_00FA
        );

        // Use same register for shift and source
        test_shift_reg_instr!(
            state,
            Shift::Left,
            a1,
            0b1001_0101,
            a1,
            0b1001_0101,
            a2,
            0x12A0_0000
        );
        // Use same register for shift and destination
        test_shift_reg_instr!(
            state,
            Shift::Left,
            a1,
            0b1001_0101,
            a2,
            0b1101_0101,
            a1,
            0x1AA0_0000
        );
        // Use same register for shift, source and destination
        test_shift_reg_instr!(
            state,
            Shift::Left,
            a1,
            0b1101_0101,
            a1,
            0b1101_0101,
            a1,
            0x1AA0_0000
        );
    });
}
