// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of integer arithmetic operations for RISC-V over the ICB.

use crate::instruction_context::ICB;
use crate::instruction_context::MulHighType;
use crate::instruction_context::Predicate;
use crate::instruction_context::Shift;
use crate::instruction_context::arithmetic::Arithmetic;
use crate::instruction_context::comparable::Comparable;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegister;
use crate::machine_state::registers::XValue;
use crate::parser::SHIFT_BITMASK;

/// Moves the two's complement of `val(rs1)` into `rd`.
///
/// Relevant RISC-V opcodes:
/// - SUB
pub fn run_neg(icb: &mut impl ICB, rd: NonZeroXRegister, rs1: NonZeroXRegister) {
    let rs1_val = icb.xregister_read_nz(rs1);
    let result = rs1_val.negate(icb);
    icb.xregister_write_nz(rd, result)
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
    let result = lhs.add(rhs, icb);
    icb.xregister_write_nz(rd, result)
}

/// Perform `val(rs1) + val(rs2)` but only on lowest 32 bits
/// and store the sign-extended result in `rd`.
pub fn run_add_word(icb: &mut impl ICB, rs1: XRegister, rs2: XRegister, rd: NonZeroXRegister) {
    let lhs = icb.xregister_read(rs1);
    let rhs = icb.xregister_read(rs2);

    let sum = lhs.add(rhs, icb);

    let res = icb.narrow(sum);
    let res = icb.extend_signed(res);

    icb.xregister_write_nz(rd, res)
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
    let result = lhs.sub(rhs, icb);
    icb.xregister_write_nz(rd, result)
}

/// Perform `val(rs1) - val(rs2)` but only on lowest 32 bits
/// and store the sign-extended result in `rd`.
///
/// Relevant RISC-V opcodes:
/// - SUBW
/// - C.SUBW
pub fn run_sub_word(icb: &mut impl ICB, rs1: XRegister, rs2: XRegister, rd: NonZeroXRegister) {
    // We do not need to explicitly truncate for the lower bits since wrapping_sub
    // has the same semantics & result on the lower 32 bits irrespective of bit width
    let lhs = icb.xregister_read(rs1);
    let rhs = icb.xregister_read(rs2);
    let subtraction = lhs.sub(rhs, icb);

    // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
    let res = icb.narrow(subtraction);
    let res = icb.extend_signed(res);
    icb.xregister_write_nz(rd, res)
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

    let res = lhs.and(rhs, icb);
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

    let res = lhs.or(rhs, icb);
    icb.xregister_write_nz(rd, res);
}

/// Perform `val(rs1) | imm` and store the result in `rd`
pub fn run_x64_or_immediate(
    icb: &mut impl ICB,
    imm: i64,
    rs1: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(imm);
    let res = lhs.or(rhs, icb);
    icb.xregister_write_nz(rd, res);
}

/// Perform `val(rs1) ^ val(rs2)` and store the result in `rd`
pub fn run_x64_xor(
    icb: &mut impl ICB,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);
    let res = lhs.xor(rhs, icb);
    icb.xregister_write_nz(rd, res);
}

/// Perform `val(rs1) ^ imm` and store the result in `rd`
pub fn run_x64_xor_immediate(
    icb: &mut impl ICB,
    imm: i64,
    rs1: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(imm);
    let res = lhs.xor(rhs, icb);
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
    let result = lhs.add(rhs, icb);
    icb.xregister_write_nz(rd, result)
}

/// Perform `val(rs1) + imm` but only on lowest 32 bits
/// and store the sign-extended result in `rd`
pub fn run_add_word_immediate(icb: &mut impl ICB, imm: i64, rs1: XRegister, rd: NonZeroXRegister) {
    let lhs = icb.xregister_read(rs1);
    let rhs = icb.xvalue_of_imm(imm);

    let sum = lhs.add(rhs, icb);

    let res = icb.narrow(sum);
    let res = icb.extend_signed(res);

    icb.xregister_write_nz(rd, res)
}

/// Saves in `rd` the bitwise AND between the value in `rs1` and `imm`
///
/// Relevant RISC-V opcodes:
/// - `ANDI`
/// - `C.ANDI`
pub fn run_andi(icb: &mut impl ICB, imm: i64, rs1: NonZeroXRegister, rd: NonZeroXRegister) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(imm);
    let res = lhs.and(rhs, icb);
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

    let cmp = lhs.compare(rhs, Predicate::LessThanSigned, icb);
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

    let cmp = lhs.compare(rhs, Predicate::LessThanUnsigned, icb);
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

    let cmp = lhs.compare(rhs, Predicate::LessThanSigned, icb);
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

    let cmp = lhs.compare(rhs, Predicate::LessThanUnsigned, icb);
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
    let result = lhs.mul(rhs, icb);
    icb.xregister_write_nz(rd, result)
}

/// Multiply `val(rs1)` with `val(rs2)` and store the lower 32 bits of the result
/// in register `rd`.
pub fn run_x32_mul(icb: &mut impl ICB, rs1: XRegister, rs2: XRegister, rd: NonZeroXRegister) {
    let lhs = icb.xregister_read(rs1);
    let lhs = icb.narrow(lhs);

    let rhs = icb.xregister_read(rs2);
    let rhs = icb.narrow(rhs);

    let result = lhs.mul(rhs, icb);

    let result = icb.extend_signed(result);

    icb.xregister_write_nz(rd, result)
}

// Extend `val(rs1)` and `val(rs2)` to 128 bits, multiply the 128 bits value and store the upper 64 bits in `rd`
#[inline]
pub fn run_x64_mul_high(
    icb: &mut impl ICB,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
    mul_high_type: MulHighType,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);

    let result = icb.mul_high(lhs, rhs, mul_high_type);

    icb.xregister_write_nz(rd, result)
}

/// Signed integer division `⌊ val(rs1) / val(rs2) ⌋`, storing the result in `rd`.
///
/// If `val(rs2) == 0`, the result is `-1`.
/// If `val(rs2) == -1` and `val(rs1) == i64::MIN`, the result is `i64::MIN`.
///
/// All values are _signed integers_.  
pub fn run_x64_div_signed(
    icb: &mut impl ICB,
    rs1: XRegister,
    rs2: XRegister,
    rd: NonZeroXRegister,
) {
    let rval1 = icb.xregister_read(rs1);
    let rval2 = icb.xregister_read(rs2);
    let zero = icb.xvalue_of_imm(0);
    let cond = rval2.compare(zero, Predicate::Equal, icb);

    let result = icb.branch_merge::<XValue, _, _>(
        cond,
        |icb| icb.xvalue_of_imm(-1),
        |icb| {
            let minimum = icb.xvalue_of_imm(i64::MIN);
            let minus_one = icb.xvalue_of_imm(-1);

            let cond1 = rval2.compare(minus_one, Predicate::Equal, icb);
            let cond2 = rval1.compare(minimum, Predicate::Equal, icb);
            let cond = icb.bool_and(cond1, cond2);

            icb.branch_merge::<XValue, _, _>(
                cond,
                |icb| icb.xvalue_of_imm(i64::MIN),
                |icb| rval1.div_signed(rval2, icb),
            )
        },
    );

    icb.xregister_write_nz(rd, result);
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
    let lhs = icb.xregister_read_nz(rs2);
    let shift_amount = lhs.and(bitmask, icb);

    let lhs = icb.xregister_read_nz(rs1);
    let result = lhs.shift(shift, shift_amount, icb);

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
    let lhs = icb.xregister_read_nz(rs1);
    let result = lhs.shift(shift, shift_amount, icb);

    icb.xregister_write_nz(rd, result);
}

/// Shift only lowest 32 bits in `rs1` by `shift_amount = val(rs2)\[5:0\]` in the method specified by `shift`
/// saving the result in `rd`.
pub fn run_x32_shift(
    icb: &mut impl ICB,
    shift: Shift,
    rs1: XRegister,
    rs2: XRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read(rs1);
    let shift_amount = icb.xregister_read(rs2);
    let shift_amount = shift_amount.and(icb.xvalue_of_imm(0b1_1111), icb);
    let shift_amount = icb.narrow(shift_amount);

    let lhs = icb.narrow(lhs);

    let result = lhs.shift(shift, shift_amount, icb);

    let result = icb.extend_signed(result);

    icb.xregister_write_nz(rd, result);
}

/// Shift only lowest 32 bits in `rs1` by `shift_amount = imm` in the method specified by `shift`
/// saving the result in `rd`.
pub fn run_x32_shift_immediate(
    icb: &mut impl ICB,
    shift: Shift,
    rs1: NonZeroXRegister,
    imm: i64,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let shift_amount = icb.xvalue_of_imm(imm);
    let shift_amount = shift_amount.and(icb.xvalue_of_imm(0b1_1111), icb);
    let shift_amount = icb.narrow(shift_amount);

    let lhs = icb.narrow(lhs);

    let result = lhs.shift(shift, shift_amount, icb);

    let result = icb.extend_signed(result);

    icb.xregister_write_nz(rd, result);
}

#[cfg(test)]
mod tests {
    use proptest::arbitrary::any;
    use proptest::prop_assert_eq;
    use proptest::proptest;

    use super::*;
    use crate::backend_test;
    use crate::instruction_context::Shift;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::a0;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::a2;
    use crate::machine_state::registers::a3;
    use crate::machine_state::registers::nz;
    use crate::machine_state::registers::t0;
    use crate::machine_state::registers::t1;
    use crate::machine_state::registers::t2;
    use crate::machine_state::registers::t3;
    use crate::state::NewState;

    backend_test!(test_negate, F, {
        let rs2val_rd_res = [
            (0_u64, nz::t3, 0_u64),
            (0xFFFF_FFFF_FFF0_0420, nz::t2, 0x0000_0000_000F_FBE0),
            (0xFFFF_FFFF_FFFF_FFFF, nz::t4, 0x0000_0000_0000_0001),
        ];

        for (rs2, rd, res) in rs2val_rd_res {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

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
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

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
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

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

    backend_test!(test_sub_word, F, {
        proptest!(|(
            v1 in any::<i64>(),
            v2 in any::<i64>())|
        {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());
            state.hart.xregisters.write(t0, v1 as u64);
            state.hart.xregisters.write(a0, v2 as u64);
            run_sub_word(&mut state, t0, a0, nz::a1);
            // check against wrapping subtraction performed on the lowest 32 bits
            let v1_u32 = v1 as u32;
            let v2_u32 = v2 as u32;
            prop_assert_eq!(
                state.hart.xregisters.read(a1),
                v1_u32.wrapping_sub(v2_u32) as i32 as i64 as u64
            );
        });
    });

    backend_test!(test_set_less_than, F, {
        let mut core = MachineCoreState::<M4K, _>::new(&mut F::manager());

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
        let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

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

    macro_rules! test_shift_word_imm_instr {
        ($state:ident, $shift:expr, $imm:expr,
            $rs1:ident, $r1_val:expr,
            $rd:ident, $expected_val:expr
        ) => {
            $state.hart.xregisters.write_nz(nz::$rs1, $r1_val);
            run_x32_shift_immediate(&mut $state, $shift, nz::$rs1, $imm, nz::$rd);
            let new_val = $state.hart.xregisters.read($rd);
            assert_eq!(new_val, $expected_val);
        };
    }

    macro_rules! test_shift_word_reg_instr {
        ($state:ident, $shift:expr,
            $rs2:ident, $r2_val:expr,
            $rs1:ident, $r1_val:expr,
            $rd:ident, $expected_val:expr
        ) => {
            $state.hart.xregisters.write($rs2, $r2_val);
            $state.hart.xregisters.write($rs1, $r1_val);
            run_x32_shift(&mut $state, $shift, $rs1, $rs2, nz::$rd);
            let new_val = $state.hart.xregisters.read($rd);
            assert_eq!(new_val, $expected_val);
        };
    }

    macro_rules! test_both_shift_word_instr {
        ($state:ident, $shift:expr,
            $rs2:ident, $r2_val:expr,
            $rs1:ident, $r1_val:expr,
            $rd:ident, $expected_val:expr
        ) => {
            test_shift_word_imm_instr!($state, $shift, $r2_val, $rs1, $r1_val, $rd, $expected_val);
            test_shift_word_reg_instr!(
                $state,
                $shift,
                $rs2,
                $r2_val,
                $rs1,
                $r1_val,
                $rd,
                $expected_val
            );
        };
    }

    backend_test!(test_shift_word, F, {
        let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

        test_both_shift_word_instr!(
            state,
            Shift::Left,
            t0,
            0,
            a0,
            0xEDDD_1234_ABEF,
            a1,
            0x1234_ABEF
        );
        test_both_shift_word_instr!(
            state,
            Shift::RightUnsigned,
            t0,
            0,
            a0,
            0x1234_ABEF,
            a0,
            0x1234_ABEF
        );
        test_both_shift_word_instr!(
            state,
            Shift::RightSigned,
            a2,
            0,
            a0,
            0xFFFF_DEAD_1234_ABEF,
            a1,
            0x1234_ABEF
        );
        test_both_shift_word_instr!(
            state,
            Shift::Left,
            a3,
            20,
            a0,
            0x1F0B_FFFF,
            a0,
            0xFFFF_FFFF_FFF0_0000
        );
        test_both_shift_word_instr!(
            state,
            Shift::RightUnsigned,
            t0,
            10,
            a0,
            0x44_1234_ABEF,
            a1,
            0x4_8D2A
        );
        test_both_shift_word_instr!(
            state,
            Shift::RightUnsigned,
            a1,
            16,
            t0,
            -1_i64 as u64,
            a0,
            0xFFFF
        );
        test_both_shift_word_instr!(
            state,
            Shift::RightSigned,
            a1,
            10,
            a0,
            0xFFFF_F0FF_FFF0_FF00,
            a0,
            0xFFFF_FFFF_FFFF_FC3F
        );
        test_both_shift_word_instr!(
            state,
            Shift::Left,
            t0,
            31,
            a0,
            0x1234_ABEF,
            a0,
            0xFFFF_FFFF_8000_0000
        );
        test_both_shift_word_instr!(
            state,
            Shift::RightSigned,
            t2,
            31,
            a0,
            0x8234_ABEF,
            a1,
            0xFFFF_FFFF_FFFF_FFFF
        );
        test_shift_word_reg_instr!(
            state,
            Shift::Left,
            a1,
            0b1001_0101,
            a1,
            0b1001_0101,
            a2,
            0x12A0_0000
        );
        test_shift_word_reg_instr!(
            state,
            Shift::Left,
            a1,
            0b1001_0101,
            a2,
            0b1101_0101,
            a1,
            0x1AA0_0000
        );
        test_shift_word_reg_instr!(
            state,
            Shift::Left,
            a1,
            0b0100_1101_0101,
            a1,
            0b0100_1101_0101,
            a1,
            0xFFFF_FFFF_9AA0_0000
        );
    });

    backend_test!(test_bitwise_intruction, F, {
        proptest!(|(val1 in any::<u64>(), val2 in any::<u64>())| {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            // The sign-extension of an immediate on 12 bits has bits 31:11 equal the sign-bit
            let prefix_mask = 0xFFFF_FFFF_FFFF_F800;
            let negative_imm = val2 | prefix_mask;
            let positive_imm = val2 & !prefix_mask;

            state.hart.xregisters.write(a0, val1);
            run_x64_or_immediate(&mut state, negative_imm as i64, nz::a0, nz::a0);
            prop_assert_eq!(state.hart.xregisters.read(a0), val1 | negative_imm);

            state.hart.xregisters.write(a0, val1);
            run_x64_or_immediate(&mut state, positive_imm as i64, nz::a0, nz::a1);
            prop_assert_eq!(state.hart.xregisters.read(a1), val1 | positive_imm);

            state.hart.xregisters.write(t2, val1);
            run_x64_xor_immediate(&mut state, negative_imm as i64, nz::t2, nz::t2);
            prop_assert_eq!(state.hart.xregisters.read(t2), val1 ^ negative_imm);

            state.hart.xregisters.write(t2, val1);
            run_x64_xor_immediate(&mut state, positive_imm as i64, nz::t2, nz::t1);
            prop_assert_eq!(state.hart.xregisters.read(t1), val1 ^ positive_imm);

            state.hart.xregisters.write(t2, val1);
            state.hart.xregisters.write(t3, val2);
            run_x64_xor(&mut state, nz::t3, nz::t2, nz::t1);
            prop_assert_eq!(state.hart.xregisters.read(t1), val1 ^ val2);
        })
    });

    backend_test!(test_x64_div_signed, F, {
        proptest!(|(
            r1_val in any::<u64>(),
            r2_val in any::<u64>(),
        )| {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            state.hart.xregisters.write(a0, r1_val);
            state.hart.xregisters.write(a1, r2_val);
            run_x64_div_signed(&mut state, a0, a1, nz::a2);
            state.hart.xregisters.run_rem(a0, a1, a3);

            prop_assert_eq!(
                state.hart.xregisters.read(a0),
                state.hart.xregisters.read(a1)
                    .wrapping_mul(state.hart.xregisters.read(a2))
                    .wrapping_add(state.hart.xregisters.read(a3)));
        })
    });

    macro_rules! test_x64_mul_high {
        ($state:ident, $r1_val:expr, $r2_val:expr, $mul_high_type:expr, $expected_val:expr) => {
            $state.hart.xregisters.write_nz(nz::a0, $r1_val);
            $state.hart.xregisters.write_nz(nz::a1, $r2_val);
            run_x64_mul_high(&mut $state, nz::a0, nz::a1, nz::a2, $mul_high_type);
            assert_eq!($state.hart.xregisters.read_nz(nz::a2), $expected_val);
        };
    }

    backend_test!(test_x64_mul_high, F, {
        let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

        // MULH (Signed × Signed)
        test_x64_mul_high!(
            state,
            i64::MAX as u64,
            i64::MAX as u64,
            MulHighType::Signed,
            (((i64::MAX as i128) * (i64::MAX as i128)) >> 64) as u64
        );
        test_x64_mul_high!(
            state,
            i64::MIN as u64,
            i64::MIN as u64,
            MulHighType::Signed,
            (((i64::MIN as i128) * (i64::MIN as i128)) >> 64) as u64
        );
        test_x64_mul_high!(
            state,
            i64::MIN as u64,
            i64::MAX as u64,
            MulHighType::Signed,
            (((i64::MIN as i128) * (i64::MAX as i128)) >> 64) as u64
        );

        // MULHSU (Signed × Unsigned)
        test_x64_mul_high!(
            state,
            i64::MAX as u64,
            u64::MAX,
            MulHighType::SignedUnsigned,
            (((i64::MAX as i128) * (u64::MAX as u128) as i128) >> 64) as u64
        );
        test_x64_mul_high!(
            state,
            i64::MIN as u64,
            u64::MAX,
            MulHighType::SignedUnsigned,
            (((i64::MIN as i128) * (u64::MAX as u128) as i128) >> 64) as u64
        );
        test_x64_mul_high!(
            state,
            -1i64 as u64,
            u64::MAX,
            MulHighType::SignedUnsigned,
            (-((u64::MAX as u128) as i128) >> 64) as u64
        );

        // MULHU (Unsigned × Unsigned)
        test_x64_mul_high!(
            state,
            u64::MAX,
            u64::MAX,
            MulHighType::Unsigned,
            (((u64::MAX as u128) * (u64::MAX as u128)) >> 64) as u64
        );
        test_x64_mul_high!(
            state,
            i64::MIN as u64,
            2u64,
            MulHighType::Unsigned,
            (((i64::MIN as u64 as u128) * (2u128)) >> 64) as u64
        );
        test_x64_mul_high!(state, 0u64, u64::MAX, MulHighType::Unsigned, 0u64);
    });
}
