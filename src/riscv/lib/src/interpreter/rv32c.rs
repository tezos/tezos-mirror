// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_C extension for RISC-V
//!
//! U:C-16

use crate::machine_state::MachineCoreState;
use crate::machine_state::hart_state::HartState;
use crate::machine_state::memory;
use crate::machine_state::memory::Address;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegister;
use crate::parser::instruction::InstrWidth;
use crate::state_backend as backend;
use crate::traps::Exception;

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
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
    use crate::backend_test;
    use crate::create_state;
    use crate::interpreter::branching::run_j;
    use crate::interpreter::branching::run_jr;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::nz;
    use crate::parser::instruction::InstrWidth;

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
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            // Test C.JALR
            // save program counter and value for rs1.
            state.hart.pc.write(init_pc);
            state.hart.xregisters.write_nz(rs1, init_rs1);
            let new_pc = state.hart.run_jalr(nz::ra, rs1, InstrWidth::Compressed);

            // check the program counter hasn't changed, the returned
            // value for the program counter is correct, and that the link
            // register has been updated.
            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.hart.xregisters.read_nz(nz::ra), res_rd);

            // Test C.JR
            // save program counter and value for rs1.
            state.hart.pc.write(init_pc);
            state.hart.xregisters.write_nz(rs1, init_rs1);
            let new_pc = run_jr(&mut state, rs1);

            // check the program counter hasn't changed and the returned
            // value for the program counter is correct.
            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });
}
