// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_C extension for RISC-V
//!
//! U:C-16

use crate::{
    machine_state::{
        bus::{main_memory::MainMemoryLayout, Address},
        hart_state::HartState,
        registers::{sp, x0, x1, x2, XRegister, XRegisters},
        MachineCoreState,
    },
    state_backend as backend,
    traps::Exception,
};

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
    /// `C.J` CJ-type compressed instruction
    ///
    /// Performs an unconditional control transfer. The immediate is added to
    /// the pc to form the jump target address.
    pub fn run_cj(&mut self, imm: i64) -> Address {
        self.run_jal(imm, x0)
    }

    /// `C.JR` CR-type compressed instruction
    ///
    /// Performs an unconditional control transfer to the address in register `rs1`.
    pub fn run_cjr(&mut self, rs1: XRegister) -> Address {
        debug_assert!(rs1 != x0);
        self.run_jalr_impl::<2>(0, rs1, x0)
    }

    /// `C.JALR` CR-type compressed instruction
    ///
    /// Performs the same operation as `C.JR`, but additionally writes the
    /// address of the instruction following the jump (pc+2) to the
    /// link register (`x1`).
    pub fn run_cjalr(&mut self, rs1: XRegister) -> Address {
        debug_assert!(rs1 != x0);
        self.run_jalr_impl::<2>(0, rs1, x1)
    }

    /// `C.BEQZ` CB-type compressed instruction
    ///
    /// Performs a conditional ( val(`rs1`) == 0 ) control transfer.
    /// The offset is sign-extended and added to the pc to form the branch
    /// target address.
    pub fn run_cbeqz(&mut self, imm: i64, rs1: XRegister) -> Address {
        self.run_beq_impl::<2>(imm, rs1, x0)
    }

    /// `C.BNEZ` CB-type compressed instruction
    ///
    /// Performs a conditional ( val(`rs1`) != 0 ) control transfer.
    /// The offset is sign-extended and added to the pc to form the branch
    /// target address.
    pub fn run_cbnez(&mut self, imm: i64, rs1: XRegister) -> Address {
        self.run_bne_impl::<2>(imm, rs1, x0)
    }

    /// `C.EBREAK` compressed instruction
    ///
    /// Equivalent to `EBREAK`.
    pub fn run_cebreak(&self) -> Exception {
        Exception::Breakpoint
    }
}

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
    /// `C.ADDI` CI-type compressed instruction
    ///
    /// Adds the non-zero sign-extended 6-bit `imm` to the value in `rd_rs1` then
    /// writes the result to `rd_rs1`.
    pub fn run_caddi(&mut self, imm: i64, rd_rs1: XRegister) {
        debug_assert!(rd_rs1 != x0);
        self.run_addi(imm, rd_rs1, rd_rs1)
    }

    /// `C.ADDI16SP` CI-type compressed instruction
    ///
    /// Adds the non-zero immediate to the value in the stack pointer.
    /// The immediate is obtained by sign-extending and scaling by 16 the value
    /// encoded in the instruction (see U:C-16.5).
    pub fn run_caddi16sp(&mut self, imm: i64) {
        debug_assert!(imm != 0 && imm % 16 == 0);
        self.run_addi(imm, sp, sp)
    }

    /// `C.ADDI4SPN` CIW-type compressed instruction
    ///
    /// Adds the non-zero immediate to the stack pointer and writes the result
    /// to `rd`.
    /// The immediate is obtained by zero-extending and scaling by 4 the value
    /// encoded in the instruction (see U:C-16.5).
    pub fn run_caddi4spn(&mut self, imm: i64, rd: XRegister) {
        debug_assert!(imm > 0 && imm % 4 == 0);
        self.run_addi(imm, sp, rd)
    }

    /// `C.SLLI` CI-type compressed instruction
    ///
    /// Performs a logical left shift of the value in register `rd_rs1`
    /// then writes the result back to `rd_rs1`.
    pub fn run_cslli(&mut self, imm: i64, rd_rs1: XRegister) {
        debug_assert!(rd_rs1 != x0);
        self.run_slli(imm, rd_rs1, rd_rs1)
    }

    /// `C.SRLI` CB-type compressed instruction
    ///
    /// Performs a logical right shift of the value in register `rd_rs1`
    /// then writes the result back to `rd_rs1`.
    pub fn run_csrli(&mut self, imm: i64, rd_rs1: XRegister) {
        self.run_srli(imm, rd_rs1, rd_rs1)
    }

    /// `C.SRAI` CB-type compressed instruction
    ///
    /// Performs an arithmetic right shift of the value in register `rd_rs1`
    /// then writes the result back to `rd_rs1`.
    pub fn run_csrai(&mut self, imm: i64, rd_rs1: XRegister) {
        self.run_srai(imm, rd_rs1, rd_rs1)
    }

    /// `C.ANDI` CB-format instruction
    ///
    /// Computes the bitwise AND of the value in register `rd_rs1` and
    /// the sign-extended 6-bit immediate, then writes the result back to `rd_rs1`.
    pub fn run_candi(&mut self, imm: i64, rd_rs1: XRegister) {
        self.run_andi(imm, rd_rs1, rd_rs1)
    }

    /// `C.MV` CR-type compressed instruction
    ///
    /// Copies the value in register `rs2` into register `rd_rs1`.
    pub fn run_cmv(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        debug_assert!(rd_rs1 != x0 && rs2 != x0);
        self.run_add(rs2, x0, rd_rs1)
    }

    /// `C.ADD` CR-type compressed instruction
    ///
    /// Adds the values in registers `rd_rs1` and `rs2` and writes the result
    /// back to register `rd_rs1`.
    pub fn run_cadd(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        debug_assert!(rd_rs1 != x0 && rs2 != x0);
        self.run_add(rd_rs1, rs2, rd_rs1)
    }

    /// `C.AND` CA-type compressed instruction
    ///
    /// Computes the bitwise AND of the values in registers `rd_rs1` and `rs2`,
    /// then writes the result back to register rd `rd_rs1`.
    pub fn run_cand(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_and(rd_rs1, rs2, rd_rs1)
    }

    /// `C.XOR` CA-type compressed instruction
    ///
    /// Computes the bitwise XOR of the values in registers `rd_rs1` and `rs2`,
    /// then writes the result back to register rd `rd_rs1`.
    pub fn run_cxor(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_xor(rd_rs1, rs2, rd_rs1)
    }

    /// `C.OR` CA-type compressed instruction
    ///
    /// Computes the bitwise OR of the values in registers `rd_rs1` and `rs2`,
    /// then writes the result back to register rd `rd_rs1`.
    pub fn run_cor(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_or(rd_rs1, rs2, rd_rs1)
    }

    /// `C.SUB` CA-type compressed instruction
    ///
    /// Subtracts the value in register `rs2` from the value in register `rd_rs1`,
    /// then writes the result to register `rd_rs1`.
    pub fn run_csub(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_sub(rd_rs1, rs2, rd_rs1)
    }

    /// `C.LI` CI-type compressed instruction
    ///
    /// Loads the sign-extended 6-bit immediate into register `rd_rs1`.
    pub fn run_cli(&mut self, imm: i64, rd_rs1: XRegister) {
        debug_assert!(rd_rs1 != x0);
        self.run_addi(imm, x0, rd_rs1)
    }

    /// `C.LUI` CI-type compressed instruction
    ///
    /// Loads the non-zero 6-bit immediate into bits 17–12 of the
    /// register `rd_rs1`, clears the bottom 12 bits, and sign-extends bit 17
    /// into all higher bits of `rd_rs1`.
    pub fn run_clui(&mut self, imm: i64, rd_rs1: XRegister) {
        debug_assert!(rd_rs1 != x0 && rd_rs1 != x2);
        self.run_lui(imm, rd_rs1)
    }
}

impl<ML, M> MachineCoreState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::ManagerReadWrite,
{
    /// `C.LW` CL-type compressed instruction
    ///
    /// Loads a 32-bit value from memory into register `rd`. It computes
    /// an effective address by adding the immediate to the base address
    /// in register `rs1`.
    /// The immediate is obtained by zero-extending and scaling by 4 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_clw(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 4 == 0);
        self.run_lw(imm, rs1, rd)
    }

    /// `C.LWSP` CI-type compressed instruction
    ///
    /// Loads a 32-bit value from memory into register `rd`. It computes
    /// an effective address by adding the immediate to the stack pointer.
    /// The immediate is obtained by zero-extending and scaling by 4 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_clwsp(&mut self, imm: i64, rd_rs1: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 4 == 0);
        debug_assert!(rd_rs1 != x0);
        self.run_lw(imm, sp, rd_rs1)
    }

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

    /// `C.SWSP` CSS-type compressed instruction
    ///
    /// Stores a 32-bit value in register `rs2` to memory. It computes
    /// an effective address by adding the immediate to the stack pointer.
    /// The immediate is obtained by zero-extending and scaling by 4 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_cswsp(&mut self, imm: i64, rs2: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 4 == 0);
        self.run_sw(imm, sp, rs2)
    }

    /// C.NOP CI-type compressed instruction
    ///
    /// Does not change any user-visible state, except for advancing the pc and
    /// incrementing any applicable performance counters. Equivalent to `NOP`.
    pub fn run_cnop(&self) {}
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_state,
        machine_state::{
            bus::main_memory::tests::T1K, registers::a4, MachineCoreState, MachineCoreStateLayout,
        },
    };
    use proptest::{prelude::*, prop_assert_eq, proptest};

    backend_test!(run_cj, F, {
        let test_case = [
            (42, 42, 84),
            (0, 1000, 1000),
            (100, -50, 50),
            (50, -100, -50_i64 as u64),
            (u64::MAX - 1, 100, 98_i64 as u64),
        ];
        for (init_pc, imm, res_pc) in test_case {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);

            state.hart.pc.write(init_pc);
            let new_pc = state.hart.run_cj(imm);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });

    backend_test!(run_caddi, F, {
        let state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            rd_val in any::<u64>(),
            imm in any::<i64>(),
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

            state.hart.xregisters.write(a4, rd_val);
            state.hart.xregisters.run_caddi(imm, a4);
            let res = state.hart.xregisters.read(a4);
            prop_assert_eq!(res, rd_val.wrapping_add(imm as u64));
        });
    });
}
