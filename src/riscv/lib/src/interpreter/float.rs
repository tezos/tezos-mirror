// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Core-logic implementation of F/D instructions.

use std::fmt;
use std::fmt::Display;
use std::ops::Neg;

use rustc_apfloat::Float;
use rustc_apfloat::FloatConvert;
use rustc_apfloat::Round;
use rustc_apfloat::Status;
use rustc_apfloat::StatusAnd;

use crate::machine_state::csregisters::CSRRepr;
use crate::machine_state::csregisters::CSRegister;
use crate::machine_state::csregisters::CSRegisters;
use crate::machine_state::hart_state::HartState;
use crate::machine_state::registers::FRegister;
use crate::machine_state::registers::FValue;
use crate::machine_state::registers::XRegister;
use crate::parser::instruction::InstrRoundingMode;
use crate::state_backend as backend;
use crate::traps::Exception;

pub trait FloatExt: Float + Into<FValue> + Copy + Neg + From<FValue> {
    /// The canonical NaN has a positive sign and all
    /// significand bits clear expect the MSB (the quiet bit).
    fn canonical_nan() -> Self;

    /// Canonicalise floating-point values to the canonical nan.
    fn canonicalise(self) -> Self {
        if self.is_nan() {
            Self::canonical_nan()
        } else {
            self
        }
    }
}

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
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
    pub(super) fn run_fclass<F: FloatExt>(&mut self, rs1: FRegister, rd: XRegister) {
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
    pub(super) fn run_feq<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister) {
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
    pub(super) fn run_flt<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister) {
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
    pub(super) fn run_fle<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister) {
        let rval1: F = self.fregisters.read(rs1).into();
        let rval2: F = self.fregisters.read(rs2).into();

        if rval1.is_nan() || rval2.is_nan() {
            self.csregisters.set_exception_flag(Fflag::NV);
        }

        let res = if rval1 <= rval2 { 1 } else { 0 };

        self.xregisters.write(rd, res);
    }

    /// `FADD.*` instruction.
    ///
    /// Adds `rs1` to `rs2`, writing the result in `rd`.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fadd<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.f_arith_2(rs1, rs2, rm, rd, F::add_r)
    }

    /// `FSUB.*` instruction.
    ///
    /// Subtracts `rs2` from `rs1`, writing the result in `rd`.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fsub<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.f_arith_2(rs1, rs2, rm, rd, F::sub_r)
    }

    /// `FMUL.*` instruction.
    ///
    /// Multiplies `rs1` by `rs2`, writing the result in `rd`.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fmul<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.f_arith_2(rs1, rs2, rm, rd, F::mul_r)
    }

    /// `FDIV.*` instruction.
    ///
    /// Divides `rs1` by `rs2`, writing the result in `rd`.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fdiv<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.f_arith_2(rs1, rs2, rm, rd, F::div_r)
    }

    /// `FMIN.*` instruction.
    ///
    /// Writes the smaller of `rs1`, `rs2` to `rd`. **NB** `-0.0 < +0.0`.
    ///
    /// If both inputs are NaNs, the result is the canonical NaN. If only one is a NaN,
    /// the result is the non-NaN operand.
    ///
    /// Signaling NaNs set the invalid operation exception flag.
    pub(super) fn run_fmin<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister) {
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
    pub(super) fn run_fmax<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister) {
        self.min_max(rs1, rs2, rd, F::maximum)
    }

    /// `FMADD.*` instruction.
    ///
    /// `(rs1 x rs2) + rs3`, writing the result to `rd`.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fmadd<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.f_arith_3(rs1, rs2, rs3, rm, rd, F::mul_add_r)
    }

    /// `FMSUB.*` instruction.
    ///
    /// `(rs1 x rs2) - rs3`, writing the result to `rd`.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fmsub<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        let f = |rv1, rv2, rv3: F, rm| F::mul_add_r(rv1, rv2, -rv3, rm);
        self.f_arith_3(rs1, rs2, rs3, rm, rd, f)
    }

    /// `FNMSUB.*` instruction.
    ///
    /// `- (rs1 x rs2) + rs3`, writing the result to `rd`.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fnmsub<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        let f = |rv1: F, rv2, rv3, rm| F::mul_add_r(-rv1, rv2, rv3, rm);
        self.f_arith_3(rs1, rs2, rs3, rm, rd, f)
    }

    /// `FNMADD.*` instruction.
    ///
    /// `- (rs1 x rs2) - rs3`, writing the result to `rd`.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fnmadd<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        let f = |rv1: F, rv2, rv3: F, rm| F::mul_add_r(-rv1, rv2, -rv3, rm);
        self.f_arith_3(rs1, rs2, rs3, rm, rd, f)
    }

    /// `FCVT.int.fmt` instruction.
    ///
    /// Converts a 32 or 64 bit float, into a 32 or 64 bit integer, with rounding.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fcvt_int_fmt<F: FloatExt, T>(
        &mut self,
        rs1: XRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
        cast: fn(u64) -> T,
        cvt: fn(T, Round) -> StatusAnd<F>,
    ) -> Result<(), Exception> {
        let rval = self.xregisters.read(rs1);
        let rval = cast(rval);

        let rm = self.f_rounding_mode(rm)?;

        let StatusAnd { status, value } = cvt(rval, rm);

        if status != Status::OK {
            self.csregisters.set_exception_flag_status(status);
        }

        self.fregisters.write(rd, value.into());

        Ok(())
    }

    /// `FCVT.fmt.fmt` instruction.
    ///
    /// Conversion between f32/f64 values.
    ///
    /// Returns `Exception::IllegalInstruction` on an invalid rounding mode.
    pub(super) fn run_fcvt_fmt_fmt<F: FloatExt + FloatConvert<T>, T: FloatExt>(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        let rval: F = self.fregisters.read(rs1).into();

        let rm = self.f_rounding_mode(rm)?;

        // ignored - all information comes from status.
        let mut loses_info = false;

        let StatusAnd { status, value } = rval.convert_r(rm, &mut loses_info).map(T::canonicalise);

        if status != Status::OK {
            self.csregisters.set_exception_flag_status(status);
        }

        self.fregisters.write(rd, value.into());

        Ok(())
    }

    pub(super) fn run_fcvt_fmt_int<F: FloatExt, T>(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: XRegister,
        cast: fn(T) -> u64,
        cvt: fn(F, Round) -> StatusAnd<T>,
    ) -> Result<(), Exception> {
        let rval: F = self.fregisters.read(rs1).into();

        let rm = self.f_rounding_mode(rm)?;

        // spec requires returning the same as for +ve infinity for nans,
        // which differs from impl in rustc_apfloat
        let rval = if rval.is_nan() { F::INFINITY } else { rval };

        let StatusAnd { status, value } = cvt(rval, rm);

        if status != Status::OK {
            self.csregisters.set_exception_flag_status(status);
        }

        self.xregisters.write(rd, cast(value));

        Ok(())
    }

    /// `FSGNJ.*` instruction.
    ///
    /// Writes all the bits of `rs1`, except for the sign bit, to `rd`.
    /// The sign bit is taken from `rs2`.
    pub(super) fn run_fsgnj<F: FloatExt>(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister) {
        self.f_sign_injection::<F>(rs1, rs2, rd, |_x, y| y);
    }

    /// `FSGNJN.*` instruction.
    ///
    /// Writes all the bits of `rs1`, except for the sign bit, to `rd`.
    /// The sign bit is taken from the negative of `rs2`.
    pub(super) fn run_fsgnjn<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) {
        self.f_sign_injection::<F>(rs1, rs2, rd, |_x, y| !y);
    }

    /// `FSGNJX.*` instruction.
    ///
    /// Writes all the bits of `rs1`, except for the sign bit, to `rd`.
    /// The sign bit is taken from the bitwise XOR of the sign bits from `rs1` & `rs2`.
    pub(super) fn run_fsgnjx<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) {
        self.f_sign_injection::<F>(rs1, rs2, rd, |x, y| x ^ y);
    }

    // perform fused 3-argument floating-point arithmetic
    fn f_arith_3<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
        f: fn(F, F, F, Round) -> StatusAnd<F>,
    ) -> Result<(), Exception> {
        let rval1: F = self.fregisters.read(rs1).into();
        let rval2: F = self.fregisters.read(rs2).into();
        let rval3: F = self.fregisters.read(rs3).into();

        let rm = self.f_rounding_mode(rm)?;

        let StatusAnd { status, value } = f(rval1, rval2, rval3, rm).map(F::canonicalise);

        if status != Status::OK {
            self.csregisters.set_exception_flag_status(status);
        }

        self.fregisters.write(rd, value.into());
        Ok(())
    }

    // perform 2-argument floating-point arithmetic
    fn f_arith_2<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
        f: fn(F, F, Round) -> StatusAnd<F>,
    ) -> Result<(), Exception> {
        let rval1: F = self.fregisters.read(rs1).into();
        let rval2: F = self.fregisters.read(rs2).into();

        let rm = self.f_rounding_mode(rm)?;

        let StatusAnd { status, value } = f(rval1, rval2, rm).map(F::canonicalise);

        if status != Status::OK {
            self.csregisters.set_exception_flag_status(status);
        }

        self.fregisters.write(rd, value.into());
        Ok(())
    }

    pub(super) fn f_rounding_mode(&self, rm: InstrRoundingMode) -> Result<Round, Exception> {
        let rm = match rm {
            InstrRoundingMode::Static(rm) => rm,
            InstrRoundingMode::Dynamic => self
                .csregisters
                .read::<CSRRepr>(CSRegister::frm)
                .try_into()?,
        };

        Ok(rm.into())
    }

    fn f_sign_injection<F: FloatExt>(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
        pick_sign: fn(bool, bool) -> bool,
    ) {
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
    ) {
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

    /// FS bits must not be set to 'Off' in mstatus register.
    pub(super) fn check_fs_on(&self) -> Result<(), Exception> {
        if self.csregisters.floating_disabled() {
            Err(Exception::IllegalInstruction)
        } else {
            Ok(())
        }
    }
}

/// There are 5 supported rounding modes
#[expect(clippy::upper_case_acronyms, reason = "Matches the RISC-V spec")]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub enum RoundingMode {
    /// Round to Nearest, ties to Even
    RNE,
    /// Round towards Zero
    RTZ,
    /// Round Down (towards -∞)
    RDN,
    /// Round Up (towrads +∞)
    RUP,
    /// Round to Nearest, ties to Max Magnitude
    RMM,
}

impl RoundingMode {
    pub const fn from_csrrepr(value: CSRRepr) -> Result<Self, Exception> {
        match value {
            0b000 => Ok(Self::RNE),
            0b001 => Ok(Self::RTZ),
            0b010 => Ok(Self::RDN),
            0b011 => Ok(Self::RUP),
            0b100 => Ok(Self::RMM),
            _ => Err(Exception::IllegalInstruction),
        }
    }
}

impl Display for RoundingMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Self::RNE => "rne",
            Self::RTZ => "rtz",
            Self::RDN => "rdn",
            Self::RUP => "rup",
            Self::RMM => "rmm",
        };

        f.write_str(res)
    }
}

impl TryFrom<CSRRepr> for RoundingMode {
    type Error = Exception;

    fn try_from(value: CSRRepr) -> Result<Self, Self::Error> {
        Self::from_csrrepr(value)
    }
}

impl From<RoundingMode> for Round {
    fn from(value: RoundingMode) -> Round {
        match value {
            RoundingMode::RNE => Round::NearestTiesToEven,
            RoundingMode::RTZ => Round::TowardZero,
            RoundingMode::RUP => Round::TowardPositive,
            RoundingMode::RDN => Round::TowardNegative,
            RoundingMode::RMM => Round::NearestTiesToAway,
        }
    }
}

#[cfg_attr(not(test), expect(dead_code, reason = "Only used in tests"))]
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

impl<M: backend::ManagerReadWrite> CSRegisters<M> {
    fn set_exception_flag(&mut self, mask: Fflag) {
        self.set_bits(CSRegister::fflags, 1 << mask as usize);
    }

    pub(super) fn set_exception_flag_status(&mut self, status: Status) {
        let bits = status_to_bits(status);
        self.set_bits(CSRegister::fflags, bits as u64);
    }
}

const fn status_to_bits(status: Status) -> u8 {
    status.bits().reverse_bits() >> 3
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_status_to_bits() {
        assert_eq!(0, status_to_bits(Status::OK));
        assert_eq!(1 << Fflag::NX as usize, status_to_bits(Status::INEXACT));
        assert_eq!(1 << Fflag::UF as usize, status_to_bits(Status::UNDERFLOW));
        assert_eq!(1 << Fflag::OF as usize, status_to_bits(Status::OVERFLOW));
        assert_eq!(1 << Fflag::DZ as usize, status_to_bits(Status::DIV_BY_ZERO));
        assert_eq!(1 << Fflag::NV as usize, status_to_bits(Status::INVALID_OP));
    }
}
