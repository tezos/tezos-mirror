// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_UF extension for RISC-V
//!
//! Chapter 11 - "F" Standard Extension for Single-Precision Floating-Point

use crate::{
    machine_state::{
        hart_state::HartState,
        registers::{FRegister, XRegister},
    },
    state_backend as backend,
};

impl<M> HartState<M>
where
    M: backend::Manager,
{
    /// `FMV.X.W` F-type instruction
    ///
    /// Moves the single-precision value in floating-point register `rs1`
    /// represented in IEEE 754-2008 encoding to the lower 32 bits of
    /// integer register `rd`.
    ///
    /// The bits are not modified in the transfer,
    /// and in particular, the payloads of non-canonical NaNs are preserved.
    ///
    /// The higher 32 bits of the destination register are filled with copies
    /// of the floating-point number’s sign bit.
    pub fn run_fmv_x_w(&mut self, rs1: FRegister, rd: XRegister) {
        let rval: u64 = self.fregisters.read(rs1).into();
        let rval = rval as i32 as u64;

        self.xregisters.write(rd, rval);
    }

    /// `FMV.W.X` F-type instruction
    ///
    /// Moves the single-precision value encoded in IEEE 754-2008 standard
    /// encoding from the lower 32 bits of integer register `rs1` to the
    /// floating-point register `rd`.
    ///
    /// The bits are not modified in the transfer,
    /// and in particular, the payloads of non-canonical NaNs are preserved.
    ///
    /// *NB* the higher 32 bits are set to `1`, as with any f32 write.
    pub fn run_fmv_w_x(&mut self, rs1: XRegister, rd: FRegister) {
        let rval = self.xregisters.read(rs1) as u32 as u64 | 0xffffffff00000000;
        self.fregisters.write(rd, rval.into());
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
    use softfloat_wrapper::{Float, F32, F64};

    backend_test!(test_fmv_f, F, {
        proptest!(|(
            f in any::<f32>().prop_map(f32::to_bits),
            rs1 in (1_u32..31).prop_map(parse_xregister),
            rs1_f in (1_u32..31).prop_map(parse_fregister),
            rs2 in (1_u32..31).prop_map(parse_xregister),
        )| {
            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, HartStateLayout, F, backend);

            println!("f as u64 {:16x}", f as u64);
            state.xregisters.write(rs1, f as u64);

            state.run_fmv_w_x(rs1, rs1_f);

            let read: u64 = state.fregisters.read(rs1_f).into();
            assert_eq!(f, read as u32, "Expected bits to be moved to fregister");

            let f_64 = F64::from_bits(read);
            assert!(f_64.is_nan() && !f_64.is_signaling_nan());

            state.run_fmv_x_w(rs1_f, rs2);

            let read = state.xregisters.read(rs2);
            assert_eq!(f, read as u32, "Expected bits to be moved to xregister");

            let f_32 = F32::from_bits(f);
            if f_32.sign() == 0 {
                assert_eq!(read, f as u64, "Expected no sign byte to be extended");
            } else {
                assert_eq!(read, (f as u64) | 0xffffffff00000000, "Expected sign byte to be extended");
            }
        });
    });
}
