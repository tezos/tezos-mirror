// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_M extension for RISC-V
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
}
