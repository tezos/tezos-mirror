// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_C extension for RISC-V
//!
//! U:C-16

use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegisters;
use crate::state_backend as backend;

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
}

#[cfg(test)]
mod tests {
    use proptest::arbitrary::any;
    use proptest::prop_assert_eq;
    use proptest::proptest;

    use crate::backend_test;
    use crate::machine_state::hart_state::HartState;
    use crate::machine_state::registers::nz;
    use crate::state::NewState;

    backend_test!(test_caddiw, F, {
        proptest!(|(
            imm in any::<i64>(),
            reg_val in any::<i64>())|
        {
            let mut state = HartState::new(&mut F::manager());

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
