// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_C extension for RISC-V
//!
//! U:C-16

use crate::{
    machine_state::{
        MachineCoreState, ProgramCounterUpdate,
        hart_state::HartState,
        memory::{self, Address},
        registers::{NonZeroXRegister, XRegister},
    },
    parser::instruction::InstrWidth,
    state_backend as backend,
    traps::Exception,
};

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
    /// Performs an unconditional control transfer to the address in register `rs1`.
    ///
    /// Relevant RISC-V opcodes:
    /// - JALR
    /// - C.JR
    pub fn run_jr(&mut self, rs1: NonZeroXRegister) -> Address {
        // The target address is obtained by setting the
        // least-significant bit of the address in rs1 to zero
        self.xregisters.read_nz(rs1) & !1
    }

    /// Writes the address of the instruction following the jump (pc+2) to `rd`.
    /// Jumps to the target address `val(rs1)`.
    ///
    /// Relevant RISC-V opcodes:
    /// - JALR
    /// - C.JALR
    pub fn run_jalr(
        &mut self,
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        width: InstrWidth,
    ) -> Address {
        // The return address to be saved in rd is the next instruction after this one.
        let return_address = self.pc.read().wrapping_add(width as u64);

        // The target address is obtained by setting the
        // least-significant bit of the address in rs1 to zero
        let target_address = self.xregisters.read_nz(rs1) & !1;

        self.xregisters.write_nz(rd, return_address);

        target_address
    }

    /// Performs a conditional ( val(`rs1`) == 0 ) control transfer.
    /// If condition met, the offset is sign-extended and added to the pc to form the branch
    /// target address that is then set, otherwise indicates to proceed to the next instruction.
    ///
    /// Relevant RISC-V opcodes:
    /// - C.BEQZ
    /// - BEQ
    /// - BGEU
    pub fn run_beqz(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let current_pc = self.pc.read();

        if self.xregisters.read_nz(rs1) == 0 {
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// Performs a conditional ( val(`rs1`) != 0 ) control transfer.
    /// If condition met, the offset is sign-extended and added to the pc to form the branch
    /// target address that is then set, otherwise indicates to proceed to the next instruction.
    ///
    /// Relevant RISC-V opcodes:
    /// - C.BNEZ
    /// - BNE
    /// - BLTU
    pub fn run_bnez(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let current_pc = self.pc.read();

        // Branch if `val(rs1) != val(rs2)`, jumping `imm` bytes ahead.
        // Otherwise, jump the width of current instruction
        if self.xregisters.read_nz(rs1) != 0 {
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// `C.EBREAK` compressed instruction
    ///
    /// Equivalent to `EBREAK`.
    pub fn run_cebreak(&self) -> Exception {
        Exception::Breakpoint
    }
}

impl<MC, M> MachineCoreState<MC, M>
where
    MC: memory::MemoryConfig,
    M: backend::ManagerReadWrite,
{
    /// `C.SW` CS-type compressed instruction
    ///
    /// Stores a 32-bit value in register `rs2` to memory. It computes
    /// an effective address by adding the immediate to the base address
    /// in register `rs1`.
    /// The immediate is obtained by zero-extending and scaling by 4 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_csw(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 4 == 0);
        self.run_sw(imm, rs1, rs2)
    }
}

#[cfg(test)]
mod tests {
    use proptest::{prelude::*, prop_assert_eq, proptest};

    use crate::{
        backend_test, create_state,
        interpreter::branching::run_j,
        machine_state::{
            MachineCoreState, MachineCoreStateLayout, ProgramCounterUpdate,
            hart_state::{HartState, HartStateLayout},
            memory::M4K,
            registers::{
                nz::{self, a0},
                t1,
            },
        },
        parser::instruction::InstrWidth,
    };

    backend_test!(test_run_j, F, {
        let test_case = [
            (42, 42, 84),
            (0, 1000, 1000),
            (100, -50, 50),
            (50, -100, -50_i64 as u64),
            (u64::MAX - 1, 100, 98_i64 as u64),
        ];
        for (init_pc, imm, res_pc) in test_case {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            state.hart.pc.write(init_pc);
            let new_pc = run_j(&mut state, imm);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });

    backend_test!(test_cjr_cjalr, F, {
        let scenarios = [
            (42, 2, nz::a2, 2, 44),
            (u64::MAX - 1, -200_i64 as u64, nz::a2, -200_i64 as u64, 0),
            (
                1_000_000_000_000,
                u64::MAX - 1_000_000_000_000 + 3,
                nz::a2,
                u64::MAX - 1_000_000_000_000 + 3,
                1_000_000_000_002,
            ),
        ];
        for (init_pc, init_rs1, rs1, res_pc, res_rd) in scenarios {
            let mut state = create_state!(HartState, F);

            // Test C.JALR
            // save program counter and value for rs1.
            state.pc.write(init_pc);
            state.xregisters.write_nz(rs1, init_rs1);
            let new_pc = state.run_jalr(nz::ra, rs1, InstrWidth::Compressed);

            // check the program counter hasn't changed, the returned
            // value for the program counter is correct, and that the link
            // register has been updated.
            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.xregisters.read_nz(nz::ra), res_rd);

            // Test C.JR
            // save program counter and value for rs1.
            state.pc.write(init_pc);
            state.xregisters.write_nz(rs1, init_rs1);
            let new_pc = state.run_jr(rs1);

            // check the program counter hasn't changed and the returned
            // value for the program counter is correct.
            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });

    macro_rules! test_shift_instr {
        ($state:ident, $shift_fn:tt, $imm:expr,
            $rd_rs1:ident, $r1_val:expr, $expected_val:expr
        ) => {
            $state.xregisters.write_nz($rd_rs1, $r1_val);
            $state.xregisters.$shift_fn($imm, $rd_rs1, $rd_rs1);
            let new_val = $state.xregisters.read_nz($rd_rs1);
            assert_eq!(new_val, $expected_val);
        };
    }

    backend_test!(test_shift, F, {
        let mut state = create_state!(HartState, F);

        // imm = 0
        test_shift_instr!(state, run_slli, 0, a0, 0x1234_ABEF, 0x1234_ABEF);

        // small imm (< 32))
        test_shift_instr!(state, run_slli, 20, a0, 0x1234_ABEF, 0x1_234A_BEF0_0000);
        // big imm (>= 32))
        test_shift_instr!(state, run_slli, 40, a0, 0x1234_ABEF, 0x34AB_EF00_0000_0000);
    });

    macro_rules! test_branch_instr {
        ($state:ident, $branch_fn:tt, $imm:expr,
         $rs1:ident, $r1_val:expr, $width:expr,
         $init_pc:ident, $expected_pc:expr
        ) => {
            $state.pc.write($init_pc);
            $state.xregisters.write($rs1, $r1_val);

            let new_pc = $state.$branch_fn($imm, nz::$rs1, $width);
            prop_assert_eq!(&new_pc, $expected_pc);
        };
    }

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

            let mut state = create_state!(HartState, F);

            // BEQZ
            if r1_val == 0 {
                test_branch_instr!(state, run_beqz, imm, t1, r1_val, width, init_pc, &branch_pcu);
                test_branch_instr!(state, run_bnez, imm, t1, r1_val, width, init_pc, &next_pcu);
            } else {
                test_branch_instr!(state, run_beqz, imm, t1, r1_val, width, init_pc, &next_pcu);
                test_branch_instr!(state, run_bnez, imm, t1, r1_val, width, init_pc, &branch_pcu);
            }

            // BEQZ when imm = 0
            if r1_val == 0 {
                test_branch_instr!(state, run_beqz, 0, t1, r1_val, width, init_pc, &init_pcu);
                test_branch_instr!(state, run_bnez, 0, t1, r1_val, width, init_pc, &next_pcu);
            } else {
                test_branch_instr!(state, run_beqz, 0, t1, r1_val, width, init_pc, &next_pcu);
                test_branch_instr!(state, run_bnez, 0, t1, r1_val, width, init_pc, &init_pcu);
            }
        });
    });
}
