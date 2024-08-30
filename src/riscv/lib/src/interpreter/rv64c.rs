// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_C extension for RISC-V
//!
//! U:C-16

use crate::{
    machine_state::{
        bus::main_memory::MainMemoryLayout,
        registers::{sp, x0, XRegister, XRegisters},
        MachineState,
    },
    state_backend as backend,
    traps::Exception,
};

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
    /// `C.ADDIW` CI-type compressed instruction
    ///
    /// Adds the non-zero sign-extended 6-bit `imm` to the value in `rd_rs1`,
    /// producing a 32-bit result which is then sign-extended to 64 bits and
    /// written back to `rd_rs1`.
    pub fn run_caddiw(&mut self, imm: i64, rd_rs1: XRegister) {
        debug_assert!(rd_rs1 != x0);
        self.run_addiw(imm, rd_rs1, rd_rs1)
    }

    /// `C.ADDW` CA-type compressed instruction
    ///
    /// Adds the values in registers `rd_rs1` and `rs2` then sign-extends the
    /// lower 32 bits of the sum and writes the result to register `rd_rs1`.
    pub fn run_caddw(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_addw(rd_rs1, rs2, rd_rs1)
    }

    /// `C.SUBW` CA-type compressed instruction
    ///
    /// Subtracts the value in register `rs2` from the value in register `rd_rs1`,
    /// then sign-extends the lower 32 bits of the difference and writes
    /// the result to register `rd_rs1`.
    pub fn run_csubw(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_subw(rd_rs1, rs2, rd_rs1)
    }
}

impl<ML, M> MachineState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::ManagerReadWrite,
{
    /// `C.LD` CL-type compressed instruction
    ///
    /// Loads a 64-bit value from memory into register `rd`. It computes
    /// an effective address by adding the immediate to the base address
    /// in register `rs1`.
    /// The immediate is obtained by zero-extending and scaling by 8 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_cld(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 8 == 0);
        self.run_ld(imm, rs1, rd)
    }

    /// `C.LDSP` CI-type compressed instruction
    ///
    /// Loads a 64-bit value from memory into register `rd`. It computes
    /// an effective address by adding the immediate to the stack pointer.
    /// The immediate is obtained by zero-extending and scaling by 8 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_cldsp(&mut self, imm: i64, rd_rs1: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 8 == 0);
        debug_assert!(rd_rs1 != x0);
        self.run_ld(imm, sp, rd_rs1)
    }

    /// `C.SD` CS-type compressed instruction
    ///
    /// Stores a 64-bit value in register `rs2` to memory. It computes
    /// an effective address by adding the immediate to the base address
    /// in register `rs1`.
    /// The immediate is obtained by zero-extending and scaling by 8 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_csd(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 8 == 0);
        self.run_sd(imm, rs1, rs2)
    }

    /// `C.SDSP` CSS-type compressed instruction
    ///
    /// Stores a 64-bit value in register `rs2` to memory. It computes
    /// an effective address by adding the immediate to the stack pointer.
    /// The immediate is obtained by zero-extending and scaling by 8 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_csdsp(&mut self, imm: i64, rs2: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 8 == 0);
        self.run_sd(imm, sp, rs2)
    }
}
