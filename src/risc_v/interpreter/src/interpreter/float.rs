// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Core-logic implementation of F/D instructions.

use crate::{
    machine_state::{
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
}
