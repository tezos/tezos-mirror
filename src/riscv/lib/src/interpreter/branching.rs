// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of internal branching opcodes.

use crate::{
    machine_state::{hart_state::HartState, memory::Address, registers::NonZeroXRegister},
    parser::instruction::InstrWidth,
    state_backend as backend,
};

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

    /// Performs an unconditional control transfer to the target address,
    /// `target_address = val(rs1) + imm`
    ///
    /// Relevant RISC-V opcodes:
    /// - JALR
    pub fn run_jr_imm(&mut self, imm: i64, rs1: NonZeroXRegister) -> Address {
        self.xregisters.read_nz(rs1).wrapping_add(imm as u64) & !1
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

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_state,
        machine_state::{
            hart_state::{HartState, HartStateLayout},
            registers::nz,
        },
        parser::instruction::InstrWidth,
    };

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
            let mut state = create_state!(HartState, F);

            // TEST JalrImm
            state.pc.write(init_pc);
            state.xregisters.write_nz(rs1, init_rs1);
            let new_pc = state.run_jalr_imm(imm, rs1, rd, InstrWidth::Uncompressed);

            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.xregisters.read_nz(rd), res_rd);

            // TEST JAbsolute
            state.pc.write(init_pc);
            let new_pc = state.run_j_absolute(imm);

            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, imm as u64 & !1);

            // TEST JalrAbsolute
            state.pc.write(init_pc);
            let new_pc = state.run_jalr_absolute(imm, rd, InstrWidth::Uncompressed);

            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, imm as u64 & !1);
            assert_eq!(state.xregisters.read_nz(rd), res_rd);

            // TEST JrImm
            state.pc.write(init_pc);
            state.xregisters.write_nz(rs1, init_rs1);
            let new_pc = state.run_jr_imm(imm, rs1);

            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });
}
