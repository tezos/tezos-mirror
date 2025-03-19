// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of branching and jumping instructions for RISC-V over the ICB.
// TODO: RV-520: Update remaining 'jump' handlers in the file to work over the ICB.

use crate::instruction_context::ICB;
use crate::machine_state::hart_state::HartState;
use crate::machine_state::memory::Address;
use crate::machine_state::registers::NonZeroXRegister;
use crate::parser::instruction::InstrWidth;
use crate::state_backend as backend;

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
    /// Jump to Absolute Address `imm`.
    /// Performs an unconditional control transfer to the target address,
    /// formed by setting the least significant bit to zero.
    ///
    /// Relevant RISC-V opcodes:
    /// - JALR
    pub fn run_j_absolute(&mut self, imm: i64) -> Address {
        imm as u64 & !1
    }

    /// Store the next instruction address in `rd` and jump to the target address.
    /// Always returns the target address (val(rs1) + imm)
    ///
    /// Relevant RISC-V opcodes:
    /// - `JALR`
    pub fn run_jalr_imm(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        rd: NonZeroXRegister,
        width: InstrWidth,
    ) -> Address {
        // The return address to be saved in rd
        let return_address = self.pc.read().wrapping_add(width as u64);

        // The target address is obtained by adding the sign-extended
        // 12-bit I-immediate to the register rs1, then setting
        // the least-significant bit of the result to zero
        let target_address = self.xregisters.read_nz(rs1).wrapping_add(imm as u64) & !1;

        self.xregisters.write_nz(rd, return_address);

        target_address
    }

    /// Jump to absolute address `imm` and link register.
    /// Store the next instruction address in `rd` and jump to the target address.
    /// Always returns the target address formed by sign extending the immediate and setting
    /// the least significant bit to 0.
    ///
    /// Relevant RISC-V opcodes:
    /// - `JALR`
    pub fn run_jalr_absolute(
        &mut self,
        imm: i64,
        rd: NonZeroXRegister,
        width: InstrWidth,
    ) -> Address {
        // The return address to be saved in rd
        let return_address = self.pc.read().wrapping_add(width as u64);
        self.xregisters.write_nz(rd, return_address);

        imm as u64 & !1
    }
}

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
    icb.xvalue_wrapping_add(current_pc, imm)
}

/// Performs an unconditional control transfer to the address in register `rs1`.
///
/// Relevant RISC-V opcodes:
/// - JALR
/// - C.JR
pub fn run_jr<I: ICB>(icb: &mut I, rs1: NonZeroXRegister) -> <I as ICB>::XValue {
    // The target address is obtained by setting the
    // least-significant bit of the address in rs1 to zero
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(!1);
    icb.xvalue_bitwise_and(lhs, rhs)
}

/// Performs an unconditional control transfer to the target address,
/// `target_address = val(rs1) + imm`
///
/// Relevant RISC-V opcodes:
/// - JALR
pub fn run_jr_imm<I: ICB>(icb: &mut I, imm: i64, rs1: NonZeroXRegister) -> <I as ICB>::XValue {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xvalue_of_imm(imm);
    let intermediate: <I as ICB>::XValue = icb.xvalue_wrapping_add(lhs, rhs);
    let new_rhs = icb.xvalue_of_imm(!1);
    icb.xvalue_bitwise_and(intermediate, new_rhs)
}

/// Add the immediate `imm` to the PC and store the result in `rd`.
///
/// Relevant RISC-V opcodes:
/// - AUIPC
pub fn run_add_immediate_to_pc(icb: &mut impl ICB, imm: i64, rd: NonZeroXRegister) {
    let lhs = icb.pc_read();
    let rhs = icb.xvalue_of_imm(imm);
    let result = icb.xvalue_wrapping_add(lhs, rhs);
    icb.xregister_write_nz(rd, result);
}

#[cfg(test)]
mod tests {
    use crate::backend_test;
    use crate::create_state;
    use crate::interpreter::branching::run_jr_imm;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::nz;
    use crate::parser::instruction::InstrWidth;

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
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            // TEST JalrImm
            state.hart.pc.write(init_pc);
            state.hart.xregisters.write_nz(rs1, init_rs1);
            let new_pc = state
                .hart
                .run_jalr_imm(imm, rs1, rd, InstrWidth::Uncompressed);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.hart.xregisters.read_nz(rd), res_rd);

            // TEST JAbsolute
            state.hart.pc.write(init_pc);
            let new_pc = state.hart.run_j_absolute(imm);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, imm as u64 & !1);

            // TEST JalrAbsolute
            state.hart.pc.write(init_pc);
            let new_pc = state
                .hart
                .run_jalr_absolute(imm, rd, InstrWidth::Uncompressed);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, imm as u64 & !1);
            assert_eq!(state.hart.xregisters.read_nz(rd), res_rd);

            // TEST JrImm
            state.hart.pc.write(init_pc);
            state.hart.xregisters.write_nz(rs1, init_rs1);
            let new_pc = run_jr_imm(&mut state, imm, rs1);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
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
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            state.hart.pc.write(init_pc);
            super::run_add_immediate_to_pc(&mut state, imm, rd);

            let read_pc = state.hart.xregisters.read_nz(rd);

            assert_eq!(read_pc, res);
        }
    });
}
