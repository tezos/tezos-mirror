// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of load and store instructions for RISC-V over the ICB.

use crate::instruction_context::ICB;
use crate::instruction_context::LoadStoreWidth;
use crate::instruction_context::arithmetic::Arithmetic;
use crate::machine_state::registers::NonZeroXRegister;

/// Loads the immediate `imm` into register `rd_rs1`.
///
/// Relevant RISC-V opcodes:
/// - C.LI
/// - C.LUI
/// - ADD
/// - ADDI
/// - ANDI
/// - ORI
/// - XORI
/// - SLLI
/// - SRLI
/// - SRAI
/// - AND
/// - C.AND
/// - OR
/// - XOR
/// - SLL
/// - SRL
/// - SRA
/// - SUB
pub fn run_li(icb: &mut impl ICB, imm: i64, rd_rs1: NonZeroXRegister) {
    let imm = icb.xvalue_of_imm(imm);
    icb.xregister_write_nz(rd_rs1, imm)
}

/// Stores a value to the address starting at `val(rs1) + imm`.
///
/// The value is taken from `rs2`, but only the lowest `width` bytes
/// are written to memory.
#[inline(always)]
pub fn run_store<I: ICB>(
    icb: &mut I,
    imm: i64,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    width: LoadStoreWidth,
) -> I::IResult<()> {
    let base_address = icb.xregister_read_nz(rs1);
    let offset = icb.xvalue_of_imm(imm);

    let address = base_address.add(offset, icb);

    let value = icb.xregister_read_nz(rs2);

    icb.main_memory_store(address, value, width)
}

#[cfg(test)]
mod test {
    use proptest::arbitrary::any;
    use proptest::prop_assert_eq;
    use proptest::proptest;

    use crate::backend_test;
    use crate::create_state;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::a2;
    use crate::machine_state::registers::a3;
    use crate::machine_state::registers::a4;
    use crate::machine_state::registers::nz;

    backend_test!(test_run_li, F, {
        let imm_rdrs1_res = [
            (0_i64, nz::t3, 0_u64),
            (0xFFF0_0420, nz::t2, 0xFFF0_0420),
            (-1, nz::t4, 0xFFFF_FFFF_FFFF_FFFF),
        ];

        for (imm, rd_rs1, res) in imm_rdrs1_res {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);
            super::run_li(&mut state, imm, rd_rs1);
            assert_eq!(state.hart.xregisters.read_nz(rd_rs1), res);
        }
    });

    backend_test!(test_lui, F, {
        proptest!(|(imm in any::<i64>())| {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);
            state.hart.xregisters.write(a2, 0);
            state.hart.xregisters.write(a4, 0);

            // U-type immediate sets imm[31:20]
            let imm = imm & 0xFFFF_F000;
            super::run_li(&mut state, imm, nz::a3);
            // read value is the expected one
            prop_assert_eq!(state.hart.xregisters.read(a3), imm as u64);
            // it doesn't modify other registers
            prop_assert_eq!(state.hart.xregisters.read(a2), 0);
            prop_assert_eq!(state.hart.xregisters.read(a4), 0);
        });
    });
}
