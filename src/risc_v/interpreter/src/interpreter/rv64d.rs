// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_UF extension for RISC-V
//!
//! Chapter 12 - "D" Standard Extension for Double-Precision Floating-Point

use crate::{
    machine_state::{
        hart_state::HartState,
        registers::{FRegister, FValue, XRegister},
    },
    state_backend as backend,
};

use softfloat_wrapper::{Float, F64};

impl From<F64> for FValue {
    fn from(f: F64) -> Self {
        f.to_bits().into()
    }
}

#[allow(clippy::from_over_into)]
impl Into<F64> for FValue {
    fn into(self) -> F64 {
        let val = self.into();
        F64::from_bits(val)
    }
}

impl<M> HartState<M>
where
    M: backend::Manager,
{
    /// `FCLASS.D` D-type instruction.
    ///
    /// See [Self::run_fclass].
    pub fn run_fclass_d(&mut self, rs1: FRegister, rd: XRegister) {
        self.run_fclass::<F64>(rs1, rd);
    }

    /// `FMV.D.X` D-type instruction
    ///
    /// Moves the single-precision value encoded in IEEE 754-2008 standard
    /// encoding from the integer register `rs1` to the floating-point register
    /// `rd`.
    ///
    /// The bits are not modified in the transfer, the payloads of non-canonical
    /// NaNs are preserved.
    pub fn run_fmv_d_x(&mut self, rs1: XRegister, rd: FRegister) {
        let rval = self.xregisters.read(rs1).into();
        self.fregisters.write(rd, rval);
    }

    /// `FMV.X.D` D-type instruction
    ///
    /// Moves the double-precision value in floating-point register `rs1`
    /// represented in IEEE 754-2008 encoding to the integer register `rd`.
    ///
    /// The bits are not modified in the transfer, the payloads of non-canonical
    /// NaNs are preserved.
    pub fn run_fmv_x_d(&mut self, rs1: FRegister, rd: XRegister) {
        let rval = self.fregisters.read(rs1).into();
        self.xregisters.write(rd, rval);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::{
            hart_state::{HartState, HartStateLayout},
            registers::{parse_fregister, parse_xregister},
        },
    };
    use proptest::prelude::*;

    backend_test!(test_fmv_d, F, {
        proptest!(|(
            d in any::<f64>().prop_map(f64::to_bits),
            rs1 in (1_u32..31).prop_map(parse_xregister),
            rs1_f in (1_u32..31).prop_map(parse_fregister),
            rs2 in (1_u32..31).prop_map(parse_xregister),
        )| {
            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, HartStateLayout, F, backend);

            state.xregisters.write(rs1, d);
            state.run_fmv_d_x(rs1, rs1_f);

            assert_eq!(d, state.fregisters.read(rs1_f).into(), "Expected bits to be moved to fregister");

            state.run_fmv_x_d(rs1_f, rs2);

            assert_eq!(d, state.xregisters.read(rs2), "Expected bits to be moved to xregister");
        });
    });
}
