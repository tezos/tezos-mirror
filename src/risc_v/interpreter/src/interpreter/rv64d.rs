// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_UF extension for RISC-V
//!
//! Chapter 12 - "D" Standard Extension for Double-Precision Floating-Point

use crate::{
    machine_state::{
        bus::main_memory::MainMemoryLayout,
        hart_state::HartState,
        registers::{FRegister, FValue, XRegister},
        MachineState,
    },
    state_backend as backend,
    traps::Exception,
};

use rustc_apfloat::{ieee::Double, Float};

impl From<Double> for FValue {
    fn from(f: Double) -> Self {
        (f.to_bits() as u64).into()
    }
}

#[allow(clippy::from_over_into)]
impl Into<Double> for FValue {
    fn into(self) -> Double {
        let val: u64 = self.into();
        Double::from_bits(val as u128)
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
        self.run_fclass::<Double>(rs1, rd);
    }

    /// `FEQ.D` R-type instruction.
    ///
    /// See [Self::run_feq].
    pub fn run_feq_d(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister) {
        self.run_feq::<Double>(rs1, rs2, rd);
    }

    /// `FLE.D` R-type instruction.
    ///
    /// See [Self::run_fle].
    pub fn run_fle_d(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister) {
        self.run_fle::<Double>(rs1, rs2, rd);
    }

    /// `FLT.D` R-type instruction.
    ///
    /// See [Self::run_flt].
    pub fn run_flt_d(&mut self, rs1: FRegister, rs2: FRegister, rd: XRegister) {
        self.run_flt::<Double>(rs1, rs2, rd);
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

impl<ML, M> MachineState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::Manager,
{
    /// `FLD` I-type instruction.
    ///
    /// Loads a double-precision floating point value from memory into `rd`.
    /// It uses the same address format as integer-base ISA.
    pub fn run_fld(&mut self, imm: i64, rs1: XRegister, rd: FRegister) -> Result<(), Exception> {
        let val: u64 = self.read_from_bus(imm, rs1)?;

        self.hart.fregisters.write(rd, val.into());
        Ok(())
    }

    /// `FSD` S-type instruction.
    ///
    /// Stores a double-precision floating point value into memory from `rs2`.
    /// It uses the same address format as integer-base ISA.
    pub fn run_fsd(&mut self, imm: i64, rs1: XRegister, rs2: FRegister) -> Result<(), Exception> {
        let val: u64 = self.hart.fregisters.read(rs2).into();
        self.write_to_bus(imm, rs1, val)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::{
            bus::{devices::DEVICES_ADDRESS_SPACE_LENGTH, main_memory::tests::T1K},
            hart_state::{HartState, HartStateLayout},
            registers::{fa2, fa3, parse_fregister, parse_xregister, t0},
            MachineState, MachineStateLayout,
        },
        traps::Exception,
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

    backend_test!(test_load_store, F, {
        proptest!(|(
            val in any::<f64>().prop_map(f64::to_bits),
        )|
        {
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            let mut perform_test = |offset: u64| -> Result<(), Exception> {
                // Save test values v_i in registers ai
                state.hart.fregisters.write(fa2, val.into());

                // t0 will hold the "global" offset of all loads / stores we are going to make
                state.hart.xregisters.write(t0, offset);

                // Perform the stores
                state.run_fsd(-4, t0, fa2)?;

                state.run_fld(-4, t0, fa3)?;

                assert_eq!(state.hart.fregisters.read(fa3), val.into());
                Ok(())
            };

            let invalid_offset = DEVICES_ADDRESS_SPACE_LENGTH - 1024;
            let aligned_offset = DEVICES_ADDRESS_SPACE_LENGTH + 512;
            let misaligned_offset = DEVICES_ADDRESS_SPACE_LENGTH + 513;

            // Out of bounds loads / stores
            prop_assert!(perform_test(invalid_offset).is_err_and(|e|
                matches!(e, Exception::StoreAccessFault(_))
            ));
            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset).is_ok());
            // Unaligned loads / stores
            prop_assert!(perform_test(misaligned_offset).is_ok());

            // Out of bounds loads / stores
            prop_assert!(perform_test(invalid_offset).is_err_and(|e|
                matches!(e, Exception::StoreAccessFault(_))
            ));
            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset).is_ok());
            // Unaligned loads / stores
            prop_assert!(perform_test(misaligned_offset).is_ok());
        });
    });
}
