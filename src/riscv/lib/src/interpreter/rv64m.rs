// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_M extension for RISC-V
//!
//! Chapter 7 - Unprivileged spec

use crate::machine_state::registers::XRegister;
use crate::machine_state::registers::XRegisters;
use crate::state_backend as backend;

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
    /// `REMW` R-type instruction
    ///
    /// Compute the remainder of val(rs1) divided by val(rs2), but only consider
    /// the lower 32 bits of each value. Store result in `rd`. In case the lower
    /// 32 bits of val(rs2) is zero, the result is val(rs1). In case of overflow
    /// the result is zero. All values used in the operation are _signed integers_.
    pub fn run_remw(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as i32;
        let rval2 = self.read(rs2) as i32;

        let result = if rval2 == 0 {
            rval1
        } else if rval2 == -1 && rval1 == i32::MIN {
            0
        } else {
            rval1 % rval2
        };

        self.write(rd, result as u64);
    }

    /// `REMUW` R-type instruction
    ///
    /// Compute the remainder of val(rs1) divided by val(rs2) and store the result
    /// in register `rd`. In case val(rs2) is zero, the result is val(rs1). All
    /// values are _unsigned integers_.
    pub fn run_remuw(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as u32;
        let rval2 = self.read(rs2) as u32;

        let result = if rval2 == 0 { rval1 } else { rval1 % rval2 };

        self.write(rd, result as i32 as u64);
    }

    /// `DIVW` R-type instruction
    ///
    /// Divide the lower 32 bits of val(rs1) by the lower 32 bits of val(rs2).
    /// The result is stored in `rd`. In case the divisor is zero, the result is
    /// `-1`. In case the dividend is `i32::MIN`, and the divisor is `-1`, then
    /// the result is `i32::MIN` as well. All values are _signed integers_.
    pub fn run_divw(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as i32;
        let rval2 = self.read(rs2) as i32;

        let result = if rval2 == 0 {
            -1
        } else if rval2 == -1 && rval1 == i32::MIN {
            i32::MIN
        } else {
            rval1 / rval2
        };

        self.write(rd, result as u64);
    }

    /// `DIVUW` R-type instruction
    ///
    /// Divide lower 32 bits of val(rs1) by the lower 32 bits of val(rs2).
    /// The result is stored in `rd`. In case the divisor is zero, the result is
    /// u32::MAX. All values are _unsigned integers_.
    pub fn run_divuw(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as u32;
        let rval2 = self.read(rs2) as u32;

        let result = if rval2 == 0 { u32::MAX } else { rval1 / rval2 };

        self.write(rd, result as i32 as u64);
    }
}

#[cfg(test)]
mod test {
    use proptest::prelude::any;
    use proptest::prop_assert_eq;
    use proptest::proptest;

    use crate::backend_test;
    use crate::machine_state::registers::XRegisters;
    use crate::machine_state::registers::a0;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::a2;
    use crate::machine_state::registers::a3;
    use crate::state::NewState;

    backend_test!(test_div_rem_invariant, F, {
        proptest!(|(
            r1_val in any::<u64>(),
            r2_val in any::<u64>(),
        )| {
            let mut state = XRegisters::new(&mut F::manager());

            state.write(a0, r1_val);
            state.write(a1, r2_val);
            state.run_divw(a0, a1, a2);
            state.run_remw(a0, a1, a3);

            prop_assert_eq!(
                state.read(a0) as i32,
                (state.read(a1) as i32)
                    .wrapping_mul(state.read(a2) as i32)
                    .wrapping_add(state.read(a3) as i32));
        })
    });

    backend_test!(test_divu_remu_invariant, F, {
        proptest!(|(
            r1_val in any::<u64>(),
            r2_val in any::<u64>(),
        )| {
            let mut state = XRegisters::new(&mut F::manager());

            state.write(a0, r1_val);
            state.write(a1, r2_val);
            state.run_divuw(a0, a1, a2);
            state.run_remuw(a0, a1, a3);

            prop_assert_eq!(
                state.read(a0) as u32,
                (state.read(a1) as u32)
                    .wrapping_mul(state.read(a2) as u32)
                    .wrapping_add(state.read(a3) as u32));
        })
    });
}
