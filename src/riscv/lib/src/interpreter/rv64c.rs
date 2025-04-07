// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_C extension for RISC-V
//!
//! U:C-16

use crate::{
    machine_state::{
        MachineCoreState,
        main_memory::MainMemoryLayout,
        registers::{NonZeroXRegister, XRegister, XRegisters, sp},
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
        // We do not need to explicitly truncate for the lower bits since wrapping_add
        // has the same semantics & result on the lower 32 bits irrespective of bit width
        let lhs = self.read(rd_rs1);
        let rhs = self.read(rs2);
        // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
        let result = lhs.wrapping_add(rhs) as i32 as u64;
        self.write(rd_rs1, result)
    }

    /// `C.SUBW` CA-type compressed instruction
    ///
    /// Subtracts the value in register `rs2` from the value in register `rd_rs1`,
    /// then sign-extends the lower 32 bits of the difference and writes
    /// the result to register `rd_rs1`.
    pub fn run_csubw(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        // We do not need to explicitly truncate for the lower bits since wrapping_sub
        // has the same semantics & result on the lower 32 bits irrespective of bit width
        let lhs = self.read(rd_rs1);
        let rhs = self.read(rs2);
        // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
        let result = lhs.wrapping_sub(rhs) as i32 as u64;
        self.write(rd_rs1, result)
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
    pub fn run_cldsp(&mut self, imm: i64, rd_rs1: NonZeroXRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 8 == 0);
        let value: i64 = self.read_from_bus(imm, sp)?;
        // i64 as u64 is a no-op
        self.hart.xregisters.write_nz(rd_rs1, value as u64);
        Ok(())
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
            MachineCoreState, MachineCoreStateLayout,
            hart_state::{HartState, HartStateLayout},
            main_memory::tests::T1K,
            registers::{a3, a4, nz, t0},
        },
        traps::Exception,
    };
    use proptest::{arbitrary::any, prop_assert, prop_assert_eq, proptest};
    use std::panic::{AssertUnwindSafe, catch_unwind};

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

    backend_test!(test_run_cldsp_clwsp, F, {
        let state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            v_d in any::<u64>(),
            v_w in any::<u32>(),
        )|
        {
            let mut state = state_cell.borrow_mut();
            state.reset();

            let mut perform_test = |offset: u64| -> Result<(), Exception> {

                state.hart.xregisters.write(a4, v_d);
                state.hart.xregisters.write(a3, v_w as u64);

                // t0 will hold the "global" offset of all loads / stores we are going to make
                state.hart.xregisters.write(t0, offset);

                state.run_sd(0, t0, a4)?;
                state.run_sw(8, t0, a3)?;

                state.run_cldsp(offset as i64, nz::t4)?;
                state.run_clwsp((offset + 8) as i64, nz::t3)?;
                assert_eq!(state.hart.xregisters.read_nz(nz::t4), v_d);
                assert_eq!(state.hart.xregisters.read_nz(nz::t3), v_w as i32 as u64);

                Ok(())
            };

            let invalid_offset = 0u64.wrapping_sub(1024);
            let aligned_offset = 512;
            let misaligned_offset = 513;

            // Out of bounds loads / stores
            prop_assert!(perform_test(invalid_offset).is_err_and(|e|
                matches!(e, Exception::StoreAMOAccessFault(_))
            ));

            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset).is_ok());

            // Misaligned loads / stores
            let result = catch_unwind(AssertUnwindSafe(|| perform_test(misaligned_offset)));
            prop_assert!(result.is_err());
        });
    });
}
