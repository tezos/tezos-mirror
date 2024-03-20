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
    M: backend::Manager,
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
}
