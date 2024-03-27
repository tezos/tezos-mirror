// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Core-logic implementation of F/D instructions.

use crate::{
    machine_state::{
        csregisters::{CSRegister, CSRegisters},
        hart_state::HartState,
        registers::{FRegister, FValue, XRegister},
    },
    state_backend as backend,
};
use rustc_apfloat::Float;

pub trait FloatExt: Float + Into<FValue>
where
    FValue: Into<Self>,
{
}

impl<F> FloatExt for F
where
    F: Float + Into<FValue>,
    FValue: Into<Self>,
{
}

impl<M> HartState<M>
where
    M: backend::Manager,
{
    /// `FCLASS.*` instruction.
    ///
    /// Examines the value in the floating-point register rs1 and writes
    /// a 10-bit mask to the integer register `rd`, which indicates the
    /// class of the floating-point number.
    ///
    /// Exactly one bit in `rd` will be set, all other bits are cleared.
    ///
    /// Does not set the floating-point exception flags.
    pub fn run_fclass<F: FloatExt>(&mut self, rs1: FRegister, rd: XRegister)
    where
        FValue: Into<F>,
    {
        let rval: F = self.fregisters.read(rs1).into();

        let is_neg = rval.is_negative();

        let res: u64 = match rval {
            _ if rval.is_neg_infinity() => 1,
            _ if is_neg && rval.is_normal() => 1 << 1,
            _ if is_neg && rval.is_denormal() => 1 << 2,
            _ if rval.is_neg_zero() => 1 << 3,
            _ if rval.is_pos_zero() => 1 << 4,
            _ if rval.is_denormal() => 1 << 5,
            _ if rval.is_normal() => 1 << 6,
            _ if rval.is_pos_infinity() => 1 << 7,
            _ if rval.is_signaling() => 1 << 8,
            _ => 1 << 9,
        };

        self.xregisters.write(rd, res);
    }

    /// `FEQ.*` instruction.
    ///
    /// Writes `1` to `rd` if equal, `0` if not.
    ///
    /// Performs a quiet comparison: only sets the invalid operation exception flag
    /// if either input is a signalling NaN.
    ///
    /// If either input is `NaN`, the result is `0`.
    pub fn run_feq<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister)
    where
        FValue: Into<F>,
    {
        let rval1: F = self.fregisters.read(rs1).into();
        let rval2: F = self.fregisters.read(rs2).into();

        if rval1.is_signaling() || rval2.is_signaling() {
            self.csregisters.set_exception_flag(Fflag::NV);
        }

        let res = if rval1 == rval2 { 1 } else { 0 };

        self.xregisters.write(rd, res);
    }

    /// `FLT.*` instruction.
    ///
    /// Writes `1` to `rd` if `rs1 < rs2`, `0` if not.
    ///
    /// If either input is `NaN`, the result is `0`, and the invalid operation exception
    /// flag is set.
    pub fn run_flt<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister)
    where
        FValue: Into<F>,
    {
        let rval1: F = self.fregisters.read(rs1).into();
        let rval2: F = self.fregisters.read(rs2).into();

        if rval1.is_nan() || rval2.is_nan() {
            self.csregisters.set_exception_flag(Fflag::NV);
        }

        let res = if rval1 < rval2 { 1 } else { 0 };

        self.xregisters.write(rd, res);
    }

    /// `FLE.*` instruction.
    ///
    /// Writes `1` to `rd` if `rs1 <= rs2`, `0` if not.
    ///
    /// If either input is `NaN`, the result is `0`, and the invalid operation exception
    /// flag is set.
    pub fn run_fle<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister)
    where
        FValue: Into<F>,
    {
        let rval1: F = self.fregisters.read(rs1).into();
        let rval2: F = self.fregisters.read(rs2).into();

        if rval1.is_nan() || rval2.is_nan() {
            self.csregisters.set_exception_flag(Fflag::NV);
        }

        let res = if rval1 <= rval2 { 1 } else { 0 };

        self.xregisters.write(rd, res);
    }
}

#[allow(unused)]
pub enum Fflag {
    /// Inexact
    NX = 0,
    /// Underflow
    UF = 1,
    /// Overflow
    OF = 2,
    /// Divide by Zero
    DZ = 3,
    /// Invalid Operation
    NV = 4,
}

impl<M: backend::Manager> CSRegisters<M> {
    fn set_exception_flag(&mut self, mask: Fflag) {
        self.set_bits(CSRegister::fflags, 1 << mask as usize);
    }
}
