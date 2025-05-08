// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of branching and jumping instructions for RISC-V over the ICB.
// TODO: RV-520: Update remaining 'jump' handlers in the file to work over the ICB.

use crate::instruction_context::ICB;
use crate::instruction_context::Predicate;
use crate::instruction_context::arithmetic::Arithmetic;
use crate::instruction_context::comparable::Comparable;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::registers::NonZeroXRegister;
use crate::parser::instruction::InstrWidth;

/// Performs an unconditional control transfer. The immediate is added to
/// the pc to form the jump target address.
///
/// Relevant RISC-V opcodes:
/// - C.J
/// - JAL
/// - BEQ
/// - C.BEQZ
pub fn run_j<I: ICB>(icb: &mut I, imm: i64) -> <I as ICB>::XValue {
    let imm = icb.xvalue_of_imm(imm);
    let current_pc = icb.pc_read();
    current_pc.add(imm, icb)
}

/// Performs an unconditional control transfer to the address in register `rs1`.
pub fn run_jr<I: ICB>(icb: &mut I, rs1: NonZeroXRegister) -> <I as ICB>::XValue {
    // The target address is obtained by setting the
    // least-significant bit of the address in rs1 to zero
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(!1);
    lhs.and(rhs, icb)
}

/// Performs an unconditional control transfer to the target address,
pub fn run_jr_imm<I: ICB>(icb: &mut I, imm: i64, rs1: NonZeroXRegister) -> <I as ICB>::XValue {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(imm);
    let lhs = lhs.add(rhs, icb);

    // The target address is obtained by setting the
    // least-significant bit of the address in rs1 to zero
    let rhs = icb.xvalue_of_imm(!1);
    lhs.and(rhs, icb)
}

/// Jump to Absolute Address `imm`.
/// Performs an unconditional control transfer to the target address,
/// formed by setting the least significant bit to zero.
pub fn run_j_absolute<I: ICB>(icb: &mut I, imm: i64) -> <I as ICB>::XValue {
    let imm = icb.xvalue_of_imm(imm);
    let mask = icb.xvalue_of_imm(!1);
    imm.and(mask, icb)
}

/// Store the next instruction address in `rd` and jump to the target address.
/// Always returns the target address (current program counter + imm)
pub fn run_jal<I: ICB>(
    icb: &mut I,
    imm: i64,
    rd: NonZeroXRegister,
    width: InstrWidth,
) -> <I as ICB>::XValue {
    // The return address to be saved in `rd` is that of the instruction following this one
    let current_pc = icb.pc_read();
    let width = icb.xvalue_of_imm(width as i64);
    let return_address = current_pc.add(width, icb);

    let imm = icb.xvalue_of_imm(imm);
    // The target address is obtained by adding the imm to the current PC
    let target_address = current_pc.add(imm, icb);

    // Store the return address in rd
    icb.xregister_write_nz(rd, return_address);

    target_address
}

/// Performs an unconditional control transfer to the address in register `rs1`
/// and stores the address of the instruction following the jump in register `rd`.
pub fn run_jalr<I: ICB>(
    icb: &mut I,
    rd: NonZeroXRegister,
    rs1: NonZeroXRegister,
    width: InstrWidth,
) -> <I as ICB>::XValue {
    // The return address to be saved in `rd` is that of the instruction following this one
    let current_pc = icb.pc_read();
    let width = icb.xvalue_of_imm(width as i64);
    let return_address = current_pc.add(width, icb);

    // The target address is obtained by setting the
    // least-significant bit of the address in rs1 to zero
    let target_address = icb.xregister_read_nz(rs1);
    let mask = icb.xvalue_of_imm(!1);
    let target_address = target_address.and(mask, icb);

    // Store the return address in rd
    icb.xregister_write_nz(rd, return_address);

    target_address
}

/// Performs an unconditional control transfer to the target address,
/// `target_address = val(rs1) + imm` and stores the address of the instruction
/// following the jump in register `rd`.
pub fn run_jalr_imm<I: ICB>(
    icb: &mut I,
    imm: i64,
    rs1: NonZeroXRegister,
    rd: NonZeroXRegister,
    width: InstrWidth,
) -> <I as ICB>::XValue {
    // The return address to be saved in `rd` is that of the instruction following this one
    let current_pc = icb.pc_read();
    let width = icb.xvalue_of_imm(width as i64);
    let return_address = current_pc.add(width, icb);

    // The target address is obtained by adding the sign-extended
    // 12-bit I-immediate to the register rs1, then setting
    // the least-significant bit of the result to zero
    let target_address = icb.xregister_read_nz(rs1);
    let imm = icb.xvalue_of_imm(imm);
    let target_address = target_address.add(imm, icb);
    let mask = icb.xvalue_of_imm(!1);
    let target_address = target_address.and(mask, icb);

    // Store the return address in rd
    icb.xregister_write_nz(rd, return_address);

    target_address
}

/// Jump to absolute address `imm` and link register.
/// Store the next instruction address in `rd` and jump to the target address.
/// Always returns the target address formed by sign extending the immediate and setting
/// the least significant bit to 0.
pub fn run_jalr_absolute<I: ICB>(
    icb: &mut I,
    imm: i64,
    rd: NonZeroXRegister,
    width: InstrWidth,
) -> <I as ICB>::XValue {
    // The return address to be saved in `rd` is that of the instruction following this one
    let current_pc = icb.pc_read();
    let width = icb.xvalue_of_imm(width as i64);
    let return_address = current_pc.add(width, icb);

    // The target address is obtained by setting the
    // least-significant bit of the immediate to zero
    let target_address = icb.xvalue_of_imm(imm);
    let mask = icb.xvalue_of_imm(!1);
    let target_address = target_address.and(mask, icb);

    // Store the return address in rd
    icb.xregister_write_nz(rd, return_address);

    target_address
}

/// Add the immediate `imm` to the PC and store the result in `rd`.
///
/// Relevant RISC-V opcodes:
/// - AUIPC
pub fn run_add_immediate_to_pc(icb: &mut impl ICB, imm: i64, rd: NonZeroXRegister) {
    let lhs = icb.pc_read();
    let rhs = icb.xvalue_of_imm(imm);
    let lhs = lhs.add(rhs, icb);
    icb.xregister_write_nz(rd, lhs);
}

/// Performs a conditional ( `predicate(val(rs1), val(rs2))` ) control transfer.
/// If condition met, the offset is sign-extended and added to the pc to form the branch
/// target address that is then set, otherwise indicates to proceed to the next instruction.
///
/// Relevant RISC-V opcodes:
/// - `BEQ`
/// - `BNE`
/// - `BLT`
/// - `BLTU`
/// - `BGE`
/// - `BGEU`
#[inline(always)]
pub fn run_branch<I: ICB>(
    icb: &mut I,
    predicate: Predicate,
    imm: i64,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    width: InstrWidth,
) -> ProgramCounterUpdate<<I as ICB>::XValue> {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);
    let cond = lhs.compare(rhs, predicate, icb);

    icb.branch(cond, imm, width)
}

/// Performs a conditional ( `predicate(val(rs1), 0)` ) control transfer.
/// If condition met, the offset is sign-extended and added to the pc to form the branch
/// target address that is then set, otherwise indicates to proceed to the next instruction.
///
/// Relevant RISC-V opcodes:
/// - `BEQ`
/// - `BNE`
/// - `BLT`
/// - `BLTU`
/// - `BGE`
/// - `BGEU`
/// - `C.BEQZ`
/// - `C.BNEZ`
#[inline(always)]
pub fn run_branch_compare_zero<I: ICB>(
    icb: &mut I,
    predicate: Predicate,
    imm: i64,
    rs1: NonZeroXRegister,
    width: InstrWidth,
) -> ProgramCounterUpdate<<I as ICB>::XValue> {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(0);
    let cond = lhs.compare(rhs, predicate, icb);

    icb.branch(cond, imm, width)
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use crate::backend_test;
    use crate::instruction_context::Predicate;
    use crate::interpreter::branching::run_j_absolute;
    use crate::interpreter::branching::run_jal;
    use crate::interpreter::branching::run_jalr;
    use crate::interpreter::branching::run_jalr_absolute;
    use crate::interpreter::branching::run_jalr_imm;
    use crate::interpreter::branching::run_jr_imm;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::ProgramCounterUpdate;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::nz;
    use crate::parser::instruction::InstrWidth;
    use crate::state::NewState;

    backend_test!(test_jalr, F, {
        let ipc_imm_irs1_rs1_rd_fpc_frd = [
            (42, 42, 4, nz::a2, nz::t1, 46, 46),
            (0, 1001, 100, nz::a1, nz::t1, 1100, 4),
            (
                u64::MAX - 1,
                100,
                -200_i64 as u64,
                nz::a2,
                nz::a2,
                -100_i64 as u64,
                2,
            ),
            (
                1_000_000_000_000,
                1_000_000_000_000,
                u64::MAX - 1_000_000_000_000 + 3,
                nz::a2,
                nz::t2,
                2,
                1_000_000_000_004,
            ),
        ];
        for (init_pc, imm, init_rs1, rs1, rd, res_pc, res_rd) in ipc_imm_irs1_rs1_rd_fpc_frd {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            // TEST JalrImm
            state.hart.pc.write(init_pc);
            state.hart.xregisters.write_nz(rs1, init_rs1);
            let new_pc = run_jalr_imm(&mut state, imm, rs1, rd, InstrWidth::Uncompressed);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.hart.xregisters.read_nz(rd), res_rd);

            // TEST JAbsolute
            state.hart.pc.write(init_pc);
            let new_pc = run_j_absolute(&mut state, imm);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, imm as u64 & !1);

            // TEST Jal
            state.hart.pc.write(init_pc);
            let new_pc = run_jal(&mut state, imm, rd, InstrWidth::Uncompressed);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, init_pc.wrapping_add(imm as u64));
            assert_eq!(state.hart.xregisters.read_nz(rd), res_rd);

            // TEST JalrAbsolute
            state.hart.pc.write(init_pc);
            let new_pc = run_jalr_absolute(&mut state, imm, rd, InstrWidth::Uncompressed);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, imm as u64 & !1);
            assert_eq!(state.hart.xregisters.read_nz(rd), res_rd);

            // TEST JrImm
            state.hart.pc.write(init_pc);
            state.hart.xregisters.write_nz(rs1, init_rs1);
            let new_pc = run_jr_imm(&mut state, imm, rs1);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);

            // TEST Jalr
            state.hart.pc.write(init_pc);
            state.hart.xregisters.write_nz(rs1, init_rs1);
            let new_pc = run_jalr(&mut state, rd, rs1, InstrWidth::Uncompressed);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, init_rs1 & !1);
            assert_eq!(state.hart.xregisters.read_nz(rd), res_rd);
        }
    });

    backend_test!(test_auipc, F, {
        let pc_imm_res_rd = [
            (0, 0, 0, nz::a2),
            (0, 0xFF_FFF0_0000, 0xFF_FFF0_0000, nz::a0),
            (0x000A_AAAA, 0xFF_FFF0_0000, 0xFF_FFFA_AAAA, nz::a1),
            (0xABCD_AAAA_FBC0_D3FE, 0, 0xABCD_AAAA_FBC0_D3FE, nz::t5),
            (0xFFFF_FFFF_FFF0_0000, 0x10_000F, 15, nz::t6),
        ];

        for (init_pc, imm, res, rd) in pc_imm_res_rd {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            state.hart.pc.write(init_pc);
            super::run_add_immediate_to_pc(&mut state, imm, rd);

            let read_pc = state.hart.xregisters.read_nz(rd);

            assert_eq!(read_pc, res);
        }
    });

    macro_rules! test_branch_compare_zero {
        ($state:ident, $predicate:expr, $imm:expr,
         $rs1:path, $r1_val:expr, $width:expr,
         $init_pc:ident, $expected_pc:expr
        ) => {
            $state.hart.pc.write($init_pc);
            $state.hart.xregisters.write_nz($rs1, $r1_val);

            let new_pc =
                super::run_branch_compare_zero(&mut $state, $predicate, $imm, $rs1, $width);
            prop_assert_eq!(&new_pc, $expected_pc);
        };
    }

    macro_rules! test_branch {
        ($state:ident, $predicate:expr, $imm:expr,
            $rs1:path, $r1_val:expr,
            $rs2:path, $r2_val:expr, $width:expr,
            $init_pc:ident, $expected_pc:expr
           ) => {
            $state.hart.pc.write($init_pc);
            $state.hart.xregisters.write_nz($rs1, $r1_val);
            $state.hart.xregisters.write_nz($rs2, $r2_val);

            let new_pc = super::run_branch(&mut $state, $predicate, $imm, $rs1, $rs2, $width);
            prop_assert_eq!(&new_pc, $expected_pc);
        };
    }

    backend_test!(test_beq_bne, F, {
        proptest!(|(
            init_pc in any::<u64>(),
            imm in any::<i64>(),
            r1_val in any::<u64>(),
            r2_val in any::<u64>(),
        )| {
            // to ensure different behaviour for tests
            prop_assume!(r1_val != r2_val);
            // to ensure branch_pc, init_pc, next_pc are different
            prop_assume!(imm > 10);
            let branch_pcu = ProgramCounterUpdate::Set(init_pc.wrapping_add(imm as u64));
            let width = InstrWidth::Uncompressed;
            let next_pcu = ProgramCounterUpdate::Next(InstrWidth::Uncompressed);
            let init_pcu = ProgramCounterUpdate::Set(init_pc);

            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            // BEQ: different
            test_branch!(state, Predicate::Equal, imm, nz::t1, r1_val, nz::t2, r2_val, width, init_pc, &next_pcu);
            // BEQ: equal
            test_branch!(state, Predicate::Equal, imm, nz::t1, r1_val, nz::t2, r1_val, width, init_pc, &branch_pcu);

            // BNE: different
            test_branch!(state, Predicate::NotEqual, imm, nz::t1, r1_val, nz::t2, r2_val, width, init_pc, &branch_pcu);
            // BNE: equal
            test_branch!(state, Predicate::NotEqual, imm, nz::t1, r1_val, nz::t2, r1_val, width, init_pc, &next_pcu);

            // BEQ: different - imm = 0
            test_branch!(state, Predicate::Equal, 0, nz::t1, r1_val, nz::t2, r2_val, width, init_pc, &next_pcu);
            // BEQ: equal - imm = 0
            test_branch!(state, Predicate::Equal, 0, nz::t1, r1_val, nz::t2, r1_val, width, init_pc, &init_pcu);

            // BNE: different - imm = 0
            test_branch!(state, Predicate::NotEqual, 0, nz::t1, r1_val, nz::t2, r2_val, width, init_pc, &init_pcu);
            // BNE: equal - imm = 0
            test_branch!(state, Predicate::NotEqual, 0, nz::t1, r1_val, nz::t2, r1_val, width, init_pc, &next_pcu);

            // BEQ: same register - imm = 0
            test_branch!(state, Predicate::Equal, 0, nz::t1, r1_val, nz::t1, r2_val, width, init_pc, &init_pcu);
            // BEQ: same register
            test_branch!(state, Predicate::Equal, imm, nz::t1, r1_val, nz::t1, r2_val, width, init_pc, &branch_pcu);

            // BNE: same register - imm = 0
            test_branch!(state, Predicate::NotEqual, 0, nz::t1, r1_val, nz::t1, r2_val, width, init_pc, &next_pcu);
            // BNE: same register
            test_branch!(state, Predicate::NotEqual, imm, nz::t1, r1_val, nz::t1, r2_val, width, init_pc, &next_pcu);
        });
    });

    backend_test!(test_bge_blt, F, {
        proptest!(|(
            init_pc in any::<u64>(),
            imm in any::<i64>(),
        )| {
            // to ensure branch_pc and init_pc are different
            prop_assume!(imm > 10);
            let branch_pcu = ProgramCounterUpdate::Set(init_pc.wrapping_add(imm as u64));
            let width = InstrWidth::Uncompressed;
            let next_pcu = ProgramCounterUpdate::Next(InstrWidth::Uncompressed);
            let init_pcu = ProgramCounterUpdate::Set(init_pc);

            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            // lhs < rhs
            test_branch!(state, Predicate::LessThanSigned, imm, nz::t1, 0, nz::t2, 1, width, init_pc, &branch_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualSigned, imm, nz::t1, i64::MIN as u64, nz::t2, i64::MAX as u64, width, init_pc, &next_pcu);

            // lhs > rhs
            test_branch!(state, Predicate::LessThanSigned, imm, nz::t1, -1_i64 as u64, nz::t2, i64::MAX as u64, width, init_pc, &branch_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualSigned, imm, nz::t1, 0, nz::t2, -123_123i64 as u64, width, init_pc, &branch_pcu);

            // lhs = rhs
            test_branch!(state, Predicate::LessThanSigned, imm, nz::t1, 0, nz::t2, 0, width, init_pc, &next_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualSigned, imm, nz::t1, i64::MAX as u64, nz::t2, i64::MAX as u64, width, init_pc, &branch_pcu);

            // same register
            test_branch!(state, Predicate::LessThanSigned, imm, nz::t1, -1_i64 as u64, nz::t1, -1_i64 as u64, width, init_pc, &next_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualSigned, imm, nz::t2, 0, nz::t2, 0, width, init_pc, &branch_pcu);

            // imm 0
            // lhs < rhs
            test_branch!(state, Predicate::LessThanSigned, 0, nz::t1, 100, nz::t2, i64::MAX as u64, width, init_pc, &init_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualSigned, 0, nz::t1, -1_i64 as u64, nz::t2, i64::MIN as u64, width, init_pc, &init_pcu);

            // same register
            test_branch!(state, Predicate::LessThanSigned, 0, nz::t1, 123_123_123, nz::t1, 123_123_123, width, init_pc, &next_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualSigned, 0, nz::t2, -1_i64 as u64, nz::t2, -1_i64 as u64, width, init_pc, &init_pcu);
        });
    });

    backend_test!(test_b, F, {
        proptest!(|(
            init_pc in any::<u64>(),
            imm in any::<i64>(),
        )| {
            // to ensure branch_pc and init_pc are different
            prop_assume!(imm > 10);
            let branch_pcu = ProgramCounterUpdate::Set(init_pc.wrapping_add(imm as u64));
            let width = InstrWidth::Uncompressed;
            let next_pcu = ProgramCounterUpdate::Next(InstrWidth::Uncompressed);

            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            // lhs < 0
            test_branch_compare_zero!(state, Predicate::LessThanSigned, imm, nz::t1, -1_i64 as u64, width, init_pc, &branch_pcu);
            test_branch_compare_zero!(state, Predicate::GreaterThanOrEqualSigned, imm, nz::t1, -1_i64 as u64, width, init_pc, &next_pcu);
            test_branch_compare_zero!(state, Predicate::LessThanOrEqualSigned, imm, nz::t1, -1_i64 as u64, width, init_pc, &branch_pcu);
            test_branch_compare_zero!(state, Predicate::GreaterThanSigned, imm, nz::t1, -1_i64 as u64, width, init_pc, &next_pcu);

            // lhs > 0
            test_branch_compare_zero!(state, Predicate::LessThanSigned, imm, nz::t1, 1, width, init_pc, &next_pcu);
            test_branch_compare_zero!(state, Predicate::GreaterThanOrEqualSigned, imm, nz::t1, 1, width, init_pc, &branch_pcu);
            test_branch_compare_zero!(state, Predicate::LessThanOrEqualSigned, imm, nz::t1, 1, width, init_pc, &next_pcu);
            test_branch_compare_zero!(state, Predicate::GreaterThanSigned, imm, nz::t1, 1, width, init_pc, &branch_pcu);

            // lhs = 0
            test_branch_compare_zero!(state, Predicate::LessThanSigned, imm, nz::t1, 0, width, init_pc, &next_pcu);
            test_branch_compare_zero!(state, Predicate::GreaterThanOrEqualSigned, imm, nz::t1, 0, width, init_pc, &branch_pcu);
            test_branch_compare_zero!(state, Predicate::LessThanOrEqualSigned, imm, nz::t1, 0, width, init_pc, &branch_pcu);
            test_branch_compare_zero!(state, Predicate::GreaterThanSigned, imm, nz::t1, 0, width, init_pc, &next_pcu);
        })
    });

    backend_test!(test_bge_ble_u, F, {
        proptest!(|(
            init_pc in any::<u64>(),
            imm in any::<i64>(),
            r1_val in any::<u64>(),
            r2_val in any::<u64>(),
        )| {
            prop_assume!(r1_val < r2_val);
            // to ensure branch_pc and init_pc are different
            prop_assume!(imm > 10);
            let branch_pcu = ProgramCounterUpdate::Set(init_pc.wrapping_add(imm as u64));
            let width = InstrWidth::Uncompressed;
            let next_pcu = ProgramCounterUpdate::Next(InstrWidth::Uncompressed);
            let pc_update_init_pcu = ProgramCounterUpdate::Set(init_pc);

            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            // lhs < rhs
            test_branch!(state, Predicate::LessThanUnsigned, imm, nz::t1, r1_val, nz::t2, r2_val, width, init_pc, &branch_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualUnsigned, imm, nz::t1, r1_val, nz::t2, r2_val, width, init_pc, &next_pcu);

            // lhs > rhs
            test_branch!(state, Predicate::LessThanUnsigned, imm, nz::t1, r2_val, nz::t2, r1_val, width, init_pc, &next_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualUnsigned, imm, nz::t1, r2_val, nz::t2, r1_val, width, init_pc, &branch_pcu);

            // lhs = rhs
            test_branch!(state, Predicate::LessThanUnsigned, imm, nz::t1, r1_val, nz::t2, r1_val, width, init_pc, &next_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualUnsigned, imm, nz::t1, r2_val, nz::t2, r2_val, width, init_pc, &branch_pcu);

            // same register
            test_branch!(state, Predicate::LessThanUnsigned, imm, nz::t1, r1_val, nz::t1, r1_val, width, init_pc, &next_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualUnsigned, imm, nz::t2, r1_val, nz::t2, r1_val, width, init_pc, &branch_pcu);

            // imm 0
            // lhs < rhs
            test_branch!(state, Predicate::LessThanUnsigned, 0, nz::t1, r1_val, nz::t2, r2_val, width, init_pc, &pc_update_init_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualUnsigned, 0, nz::t1, r1_val, nz::t2, r2_val, width, init_pc, &next_pcu);

            // lhs > rhs
            test_branch!(state, Predicate::LessThanUnsigned, 0, nz::t1, r2_val, nz::t2, r1_val, width, init_pc, &next_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualUnsigned, 0, nz::t1, r2_val, nz::t2, r1_val, width, init_pc, &pc_update_init_pcu);

            // lhs = rhs
            test_branch!(state, Predicate::LessThanUnsigned, 0, nz::t1, r1_val, nz::t2, r1_val, width, init_pc, &next_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualUnsigned, 0, nz::t2, r2_val, nz::t1, r2_val, width, init_pc, &pc_update_init_pcu);

            // same register
            test_branch!(state, Predicate::LessThanUnsigned, 0, nz::t1, r1_val, nz::t1, r1_val, width, init_pc, &next_pcu);
            test_branch!(state, Predicate::GreaterThanOrEqualUnsigned, 0, nz::t2, r1_val, nz::t2, r1_val, width, init_pc, &pc_update_init_pcu);

        });
    });

    backend_test!(test_beqz_bnez, F, {
        proptest!(|(
            init_pc in any::<u64>(),
            imm in any::<i64>(),
            r1_val in any::<u64>(),
        )| {
            // to ensure branch_pc, init_pc, next_pc are different
            prop_assume!(imm > 10);
            let branch_pcu = ProgramCounterUpdate::Set(init_pc.wrapping_add(imm as u64));
            let width = InstrWidth::Uncompressed;
            let next_pcu = ProgramCounterUpdate::Next(InstrWidth::Uncompressed);
            let init_pcu = ProgramCounterUpdate::Set(init_pc);

            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            // BEQZ
            if r1_val == 0 {
                test_branch_compare_zero!(state, Predicate::Equal, imm, nz::t1, r1_val, width, init_pc, &branch_pcu);
                test_branch_compare_zero!(state, Predicate::NotEqual, imm, nz::t1, r1_val, width, init_pc, &next_pcu);
            } else {
                test_branch_compare_zero!(state, Predicate::Equal, imm, nz::t1, r1_val, width, init_pc, &next_pcu);
                test_branch_compare_zero!(state, Predicate::NotEqual, imm, nz::t1, r1_val, width, init_pc, &branch_pcu);
            }

            // BEQZ when imm = 0
            if r1_val == 0 {
                test_branch_compare_zero!(state, Predicate::Equal, 0, nz::t1, r1_val, width, init_pc, &init_pcu);
                test_branch_compare_zero!(state, Predicate::NotEqual, 0, nz::t1, r1_val, width, init_pc, &next_pcu);
            } else {
                test_branch_compare_zero!(state, Predicate::Equal, 0, nz::t1, r1_val, width, init_pc, &next_pcu);
                test_branch_compare_zero!(state, Predicate::NotEqual, 0, nz::t1, r1_val, width, init_pc, &init_pcu);
            }
        });
    });
}
