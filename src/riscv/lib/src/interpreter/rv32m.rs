// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_M extension for RISC-V
//!
//! Chapter 7 - Unprivileged spec

use crate::{
    machine_state::registers::{XRegister, XRegisters},
    state_backend as backend,
};

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
    /// `REM` R-type instruction
    ///
    /// Compute the remainder of val(rs1) divided by val(rs2). Store result in `rd`.
    /// In case val(rs2) is zero, result is val(rs1). In case of overflow, when
    /// val(rs2) is -1 and val(rs1) is the minimum of signed 32 bit integer, result is
    /// zero. All values are _signed integers_.
    pub fn run_rem(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as i64;
        let rval2 = self.read(rs2) as i64;

        let result = if rval2 == 0 {
            rval1
        } else if rval2 == -1 && rval1 == i64::MIN {
            0
        } else {
            rval1 % rval2
        };

        self.write(rd, result as u64);
    }

    /// `REMU` R-type instruction
    ///
    /// Compute the remainder of val(rs1) divided by val(rs2) and store the result
    /// in register `rd`. In case val(rs2) is zero, the result is val(rs1). All
    /// values are _unsigned integers_.
    pub fn run_remu(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1);
        let rval2 = self.read(rs2);

        let result = if rval2 == 0 { rval1 } else { rval1 % rval2 };

        self.write(rd, result);
    }

    /// `DIV` R-type instruction
    ///
    /// Divide val(rs1) by val(rs2). The result is stored in `rd`. In case val(rs2)
    /// is zero, the result is `-1`. In case val(rs2) is `-1` and val(rs1) is the
    /// minimum of signed 64 bit integer, the result is this minimum value as well.
    /// All values are _signed integers_.
    pub fn run_div(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as i64;
        let rval2 = self.read(rs2) as i64;

        let result = if rval2 == 0 {
            -1
        } else if rval2 == -1 && rval1 == i64::MIN {
            i64::MIN
        } else {
            rval1 / rval2
        };

        self.write(rd, result as u64);
    }

    /// `DIVU` R-type instruction
    ///
    /// Divide val(rs1) by val(rs2). The result is stored in `rd`. In case val(rs2)
    /// is zero, the result is `u64::MAX`. All values are _unsigned integers_.
    pub fn run_divu(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1);
        let rval2 = self.read(rs2);

        let result = if rval2 == 0 { u64::MAX } else { rval1 / rval2 };

        self.write(rd, result);
    }

    /// `MUL` R-type instruction
    ///
    /// Multiply val(rs1) with val(rs2) and store the lower 64 bits of the result
    /// in register `rd`.
    pub fn run_mul(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as i64;
        let rval2 = self.read(rs2) as i64;

        let result = rval1.wrapping_mul(rval2);

        self.write(rd, result as u64);
    }

    /// `MULH` R-type instruction
    ///
    /// Multiply val(rs1) with val(rs2) and store the upper 64 bits of the result
    /// in register `rd`.
    pub fn run_mulh(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as i64 as i128;
        let rval2 = self.read(rs2) as i64 as i128;

        let result = rval1.wrapping_mul(rval2);

        self.write(rd, (result >> 64) as i64 as u64);
    }

    /// `MULHSU` R-type instruction
    ///
    /// Multiply val(rs1) with val(rs2) and store the upper 64 bits of the result
    /// in register `rd`. val(rs1) is treated as a _signed integer_, while val(rs2)
    /// is treated as an _unsigned integer_.
    pub fn run_mulhsu(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as i64 as i128 as u128;
        let rval2 = self.read(rs2) as u128;

        let result = rval1.wrapping_mul(rval2);

        self.write(rd, (result >> 64) as u64);
    }

    /// `MULHU` R-type instruction
    ///
    /// Multiply val(rs1) with val(rs2) and store the upper 64 bits of the result
    /// in register `rd`. Both val(rs1) and val(rs2) are treated as
    /// _unsigned integers_.
    pub fn run_mulhu(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        let rval1 = self.read(rs1) as u128;
        let rval2 = self.read(rs2) as u128;

        let result = rval1.wrapping_mul(rval2);

        self.write(rd, (result >> 64) as u64);
    }
}

#[cfg(test)]
mod test {
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::registers::{a0, a1, a2, a3, XRegisters, XRegistersLayout},
    };
    use proptest::{prelude::any, prop_assert_eq, proptest};

    backend_test!(test_div_rem_invariant, F, {
        proptest!(|(
            r1_val in any::<u64>(),
            r2_val in any::<u64>(),
        )| {
            let mut backend = create_backend!(XRegistersLayout, F);
            let mut state = create_state!(XRegisters, F, backend);

            state.write(a0, r1_val);
            state.write(a1, r2_val);
            state.run_div(a0, a1, a2);
            state.run_rem(a0, a1, a3);

            prop_assert_eq!(
                state.read(a0),
                state.read(a1)
                    .wrapping_mul(state.read(a2))
                    .wrapping_add(state.read(a3)));
        })
    });

    backend_test!(test_divu_remu_invariant, F, {
        proptest!(|(
            r1_val in any::<u64>(),
            r2_val in any::<u64>(),
        )| {
            let mut backend = create_backend!(XRegistersLayout, F);
            let mut state = create_state!(XRegisters, F, backend);

            state.write(a0, r1_val);
            state.write(a1, r2_val);
            state.run_divu(a0, a1, a2);
            state.run_remu(a0, a1, a3);

            prop_assert_eq!(
                state.read(a0),
                state.read(a1)
                    .wrapping_mul(state.read(a2))
                    .wrapping_add(state.read(a3)));
        })
    });
}
