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
use rustc_apfloat::{ieee::Double, ieee::Single, Float};

pub trait FloatExt: Float + Into<FValue> + Copy
where
    FValue: Into<Self>,
{
    /// The canonical NaN has a positive sign and all
    /// significand bits clear expect the MSB (the quiet bit).
    fn canonical_nan() -> Self;
}

impl FloatExt for Single {
    fn canonical_nan() -> Self {
        Self::from_bits(0x7fc00000_u32 as u128)
    }
}

impl FloatExt for Double {
    fn canonical_nan() -> Self {
        Self::from_bits(0x7ff8000000000000_u64 as u128)
    }
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
    pub(super) fn run_fclass<F: FloatExt>(&mut self, rs1: FRegister, rd: XRegister)
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
    pub(super) fn run_feq<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister)
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
    pub(super) fn run_flt<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister)
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
    pub(super) fn run_fle<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister)
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

    /// `FMIN.*` instruction.
    ///
    /// Writes the smaller of `rs1`, `rs2` to `rd`. **NB** `-0.0 < +0.0`.
    ///
    /// If both inputs are NaNs, the result is the canonical NaN. If only one is a NaN,
    /// the result is the non-NaN operand.
    ///
    /// Signaling NaNs set the invalid operation exception flag.
    pub(super) fn run_fmin<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister)
    where
        FValue: Into<F>,
    {
        self.min_max(rs1, rs2, rd, F::minimum)
    }

    /// `FMAX.*` instruction.
    ///
    /// Writes the larger of `rs1`, `rs2` to `rd`. **NB** `-0.0 < +0.0`.
    ///
    /// If both inputs are NaNs, the result is the canonical NaN. If only one is a NaN,
    /// the result is the non-NaN operand.
    ///
    /// Signaling NaNs set the invalid operation exception flag.
    pub(super) fn run_fmax<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister)
    where
        FValue: Into<F>,
    {
        self.min_max(rs1, rs2, rd, F::maximum)
    }

    /// `FSGNJ.*` instruction.
    ///
    /// Writes all the bits of `rs1`, except for the sign bit, to `rd`.
    /// The sign bit is taken from `rs2`.
    pub fn run_fsgnj<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister)
    where
        FValue: Into<F>,
    {
        self.f_sign_injection(rs1, rs2, rd, |_x, y| y);
    }

    /// `FSGNJN.*` instruction.
    ///
    /// Writes all the bits of `rs1`, except for the sign bit, to `rd`.
    /// The sign bit is taken from the negative of `rs2`.
    pub fn run_fsgnjn<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister)
    where
        FValue: Into<F>,
    {
        self.f_sign_injection(rs1, rs2, rd, |_x, y| !y);
    }

    /// `FSGNJX.*` instruction.
    ///
    /// Writes all the bits of `rs1`, except for the sign bit, to `rd`.
    /// The sign bit is taken from the bitwise XOR of the sign bits from `rs1` & `rs2`.
    pub fn run_fsgnjx<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister)
    where
        FValue: Into<F>,
    {
        self.f_sign_injection(rs1, rs2, rd, |x, y| x ^ y);
    }

    fn f_sign_injection<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
        pick_sign: fn(bool, bool) -> bool,
    ) where
        FValue: Into<F>,
    {
        let rval1: F = self.fregisters.read(rs1).into();
        let rval2: F = self.fregisters.read(rs2).into();

        let sign_bit_1 = rval1.is_negative();
        let sign_bit_2 = rval2.is_negative();

        let sign_bit = pick_sign(sign_bit_1, sign_bit_2);

        let res = if sign_bit == sign_bit_1 {
            rval1
        } else {
            -rval1
        };

        self.fregisters.write(rd, res.into());
    }

    fn min_max<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
        cmp: fn(F, F) -> F,
    ) where
        FValue: Into<F>,
    {
        let rval1: F = self.fregisters.read(rs1).into();
        let rval2: F = self.fregisters.read(rs2).into();

        let rval1_nan = rval1.is_nan();
        let rval2_nan = rval2.is_nan();

        let res = match (rval1_nan, rval2_nan) {
            (true, true) => F::canonical_nan(),
            (true, false) => rval2,
            (false, true) => rval1,
            (false, false) => cmp(rval1, rval2),
        };

        if (rval1_nan || rval2_nan) && (rval1.is_signaling() || rval2.is_signaling()) {
            self.csregisters.set_exception_flag(Fflag::NV);
        }

        self.fregisters.write(rd, res.into());
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
