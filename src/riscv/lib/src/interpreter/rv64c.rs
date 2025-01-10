// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_C extension for RISC-V
//!
//! U:C-16

use crate::{
    machine_state::{
        main_memory::MainMemoryLayout,
        registers::{sp, x0, NonZeroXRegister, XRegister, XRegisters},
        MachineCoreState,
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
    pub fn run_caddiw(&mut self, imm: i64, rd_rs1: NonZeroXRegister) {
        // We do not need to explicitly truncate for the lower bits since wrapping_add
        // has the same semantics & result on the lower 32 bits irrespective of bit width
        let rval = self.read_nz(rd_rs1);
        let result = rval.wrapping_add(imm as u64);
        // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
        let result = result as i32 as u64;
        self.write_nz(rd_rs1, result);
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

impl<ML, M> MachineCoreState<ML, M>
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

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_state,
        machine_state::{
            hart_state::{HartState, HartStateLayout},
            registers::nz,
        },
    };
    use proptest::{arbitrary::any, prop_assert_eq, proptest};

    backend_test!(test_caddiw, F, {
        proptest!(|(
            imm in any::<i64>(),
            reg_val in any::<i64>())|
        {
            let mut state = create_state!(HartState, F);

            state.xregisters.write_nz(nz::a0, reg_val as u64);
            state.xregisters.run_caddiw(imm, nz::a0);
            // check against wrapping addition performed on the lowest 32 bits
            let r_val = reg_val as u32;
            let i_val = imm as u32;
            prop_assert_eq!(
                state.xregisters.read_nz(nz::a0),
                r_val.wrapping_add(i_val) as i32 as i64 as u64
            );
        });
    });
}
