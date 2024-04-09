// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_UF extension for RISC-V
//!
//! Chapter 12 - "D" Standard Extension for Double-Precision Floating-Point

use super::float::FloatExt;
use crate::{
    machine_state::{
        bus::main_memory::MainMemoryLayout,
        hart_state::HartState,
        registers::{FRegister, FValue, XRegister},
        MachineState,
    },
    parser::instruction::InstrRoundingMode,
    state_backend as backend,
    traps::Exception,
};
use rustc_apfloat::{
    ieee::{Double, Single},
    Float, Status, StatusAnd,
};

impl From<Double> for FValue {
    fn from(f: Double) -> Self {
        (f.to_bits() as u64).into()
    }
}

impl From<FValue> for Double {
    fn from(f: FValue) -> Self {
        let val: u64 = f.into();
        Double::from_bits(val as u128)
    }
}

const CANONICAL_NAN_BITS: u64 = 0x7ff8000000000000;

impl FloatExt for Double {
    fn canonical_nan() -> Self {
        Self::from_bits(CANONICAL_NAN_BITS as u128)
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

    /// `FADD.D` R-type instruction.
    ///
    /// See [Self::run_fadd].
    pub fn run_fadd_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fadd::<Double>(rs1, rs2, rm, rd)
    }

    /// `FSUB.D` R-type instruction.
    ///
    /// See [Self::run_fsub].
    pub fn run_fsub_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fsub::<Double>(rs1, rs2, rm, rd)
    }

    /// `FMUL.D` R-type instruction.
    ///
    /// See [Self::run_fmul].
    pub fn run_fmul_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fmul::<Double>(rs1, rs2, rm, rd)
    }

    /// `FDIV.D` R-type instruction.
    ///
    /// See [Self::run_fdiv].
    pub fn run_fdiv_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fdiv::<Double>(rs1, rs2, rm, rd)
    }

    /// `FSQRT.D` R-type instruction.
    pub fn run_fsqrt_d(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        let rval: u64 = self.fregisters.read(rs1).into();

        let rm = self.f_rounding_mode(rm)?;

        let (StatusAnd { status, value }, _iterations) = ieee_apsqrt::sqrt_accurate(rval, rm);

        if status != Status::OK {
            self.csregisters.set_exception_flag_status(status);
        }

        self.fregisters.write(rd, value.into());

        Ok(())
    }

    /// `FMIN.D` R-type instruction.
    ///
    /// See [Self::run_fmin].
    pub fn run_fmin_d(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister) {
        self.run_fmin::<Double>(rs1, rs2, rd);
    }

    /// `FMAX.D` R-type instruction.
    ///
    /// See [Self::run_fmax].
    pub fn run_fmax_d(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister) {
        self.run_fmax::<Double>(rs1, rs2, rd);
    }

    /// `FMADD.D` instruction.
    ///
    /// See [Self::run_fmadd].
    pub fn run_fmadd_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fmadd::<Double>(rs1, rs2, rs3, rm, rd)
    }

    /// `FMSUB.D` instruction.
    ///
    /// See [Self::run_fmsub].
    pub fn run_fmsub_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fmsub::<Double>(rs1, rs2, rs3, rm, rd)
    }

    /// `FNMSUB.D` instruction.
    ///
    /// See [Self::run_fnmsub].
    pub fn run_fnmsub_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fnmsub::<Double>(rs1, rs2, rs3, rm, rd)
    }

    /// `FNMADD.D` instruction.
    ///
    /// See [Self::run_fnmadd].
    pub fn run_fnmadd_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fnmadd::<Double>(rs1, rs2, rs3, rm, rd)
    }

    /// `FCVT.D.W` R-type instruction.
    ///
    /// See [Self::fcvt_int_fmt].
    pub fn run_fcvt_d_w(
        &mut self,
        rs1: XRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fcvt_int_fmt(
            rs1,
            rm,
            rd,
            |u| u as i64 as i32 as i128,
            Double::from_i128_r,
        )
    }

    /// `FCVT.D.WU` R-type instruction.
    ///
    /// See [Self::fcvt_int_fmt].
    pub fn run_fcvt_d_wu(
        &mut self,
        rs1: XRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fcvt_int_fmt(rs1, rm, rd, |u| u as u32 as u128, Double::from_u128_r)
    }

    /// `FCVT.D.W` R-type instruction.
    ///
    /// See [Self::fcvt_int_fmt].
    pub fn run_fcvt_d_l(
        &mut self,
        rs1: XRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fcvt_int_fmt(rs1, rm, rd, |u| u as i64 as i128, Double::from_i128_r)
    }

    /// `FCVT.D.WU` R-type instruction.
    ///
    /// See [Self::fcvt_int_fmt].
    pub fn run_fcvt_d_lu(
        &mut self,
        rs1: XRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fcvt_int_fmt(rs1, rm, rd, |u| u as u128, Double::from_u128_r)
    }

    /// `FCVT.D.S` R-type instruction.
    ///
    /// See [Self::run_fcvt_fmt_fmt].
    pub fn run_fcvt_d_s(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fcvt_fmt_fmt::<Single, Double>(rs1, rm, rd)
    }

    /// `FCVT.S.D` R-type instruction.
    ///
    /// See [Self::run_fcvt_fmt_fmt].
    pub fn run_fcvt_s_d(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.run_fcvt_fmt_fmt::<Double, Single>(rs1, rm, rd)
    }

    /// `FSGNJ.D` R-type instruction.
    ///
    /// See [Self::run_fsgnj].
    pub fn run_fsgnj_d(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister) {
        self.run_fsgnj::<Double>(rs1, rs2, rd)
    }

    /// `FSGNJN.D` R-type instruction.
    ///
    /// See [Self::run_fsgnjn].
    pub fn run_fsgnjn_d(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister) {
        self.run_fsgnjn::<Double>(rs1, rs2, rd)
    }

    /// `FSGNJX.D` R-type instruction.
    ///
    /// See [Self::run_fsgnjx].
    pub fn run_fsgnjx_d(&mut self, rs1: FRegister, rs2: FRegister, rd: FRegister) {
        self.run_fsgnjx::<Double>(rs1, rs2, rd)
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
                matches!(e, Exception::StoreAMOAccessFault(_))
            ));
            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset).is_ok());
            // Unaligned loads / stores
            prop_assert!(perform_test(misaligned_offset).is_ok());

            // Out of bounds loads / stores
            prop_assert!(perform_test(invalid_offset).is_err_and(|e|
                matches!(e, Exception::StoreAMOAccessFault(_))
            ));
            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset).is_ok());
            // Unaligned loads / stores
            prop_assert!(perform_test(misaligned_offset).is_ok());
        });
    });
}
