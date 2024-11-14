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
        MachineCoreState,
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
    M: backend::ManagerReadWrite,
{
    /// `FCLASS.D` D-type instruction.
    ///
    /// See [Self::run_fclass].
    pub fn run_fclass_d(&mut self, rs1: FRegister, rd: XRegister) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fclass::<Double>(rs1, rd);
        Ok(())
    }

    /// `FEQ.D` R-type instruction.
    ///
    /// See [Self::run_feq].
    pub fn run_feq_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_feq::<Double>(rs1, rs2, rd);
        Ok(())
    }

    /// `FLE.D` R-type instruction.
    ///
    /// See [Self::run_fle].
    pub fn run_fle_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fle::<Double>(rs1, rs2, rd);
        Ok(())
    }

    /// `FLT.D` R-type instruction.
    ///
    /// See [Self::run_flt].
    pub fn run_flt_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_flt::<Double>(rs1, rs2, rd);
        Ok(())
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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

        self.run_fdiv::<Double>(rs1, rs2, rm, rd)
    }

    /// `FSQRT.D` R-type instruction.
    pub fn run_fsqrt_d(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

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
    pub fn run_fmin_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fmin::<Double>(rs1, rs2, rd);
        Ok(())
    }

    /// `FMAX.D` R-type instruction.
    ///
    /// See [Self::run_fmax].
    pub fn run_fmax_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fmax::<Double>(rs1, rs2, rd);
        Ok(())
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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

        self.run_fcvt_int_fmt(rs1, rm, rd, |u| u as i32 as i128, Double::from_i128_r)
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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

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
        self.check_fs_on()?;

        self.run_fcvt_fmt_fmt::<Double, Single>(rs1, rm, rd)
    }

    /// `FCVT.S.W` R-type instruction.
    pub fn run_fcvt_w_d(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fcvt_fmt_int(
            rs1,
            rm,
            rd,
            |u| u as i32 as u64,
            |f, rm| Double::to_i128_r(f, 32, rm, &mut false),
        )
    }

    /// `FCVT.S.WU` R-type instruction.
    pub fn run_fcvt_wu_d(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fcvt_fmt_int(
            rs1,
            rm,
            rd,
            |u| u as i32 as u64,
            |f, rm| Double::to_u128_r(f, 32, rm, &mut false),
        )
    }

    /// `FCVT.S.W` R-type instruction.
    pub fn run_fcvt_l_d(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fcvt_fmt_int(
            rs1,
            rm,
            rd,
            |u| u as u64,
            |f, rm| Double::to_i128_r(f, 64, rm, &mut false),
        )
    }

    /// `FCVT.S.WU` R-type instruction.
    pub fn run_fcvt_lu_d(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fcvt_fmt_int(
            rs1,
            rm,
            rd,
            |u| u as u64,
            |f, rm| Double::to_u128_r(f, 64, rm, &mut false),
        )
    }

    /// `FSGNJ.D` R-type instruction.
    ///
    /// See [Self::run_fsgnj].
    pub fn run_fsgnj_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fsgnj::<Double>(rs1, rs2, rd);
        Ok(())
    }

    /// `FSGNJN.D` R-type instruction.
    ///
    /// See [Self::run_fsgnjn].
    pub fn run_fsgnjn_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fsgnjn::<Double>(rs1, rs2, rd);
        Ok(())
    }

    /// `FSGNJX.D` R-type instruction.
    ///
    /// See [Self::run_fsgnjx].
    pub fn run_fsgnjx_d(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fsgnjx::<Double>(rs1, rs2, rd);
        Ok(())
    }

    /// `FMV.D.X` D-type instruction
    ///
    /// Moves the single-precision value encoded in IEEE 754-2008 standard
    /// encoding from the integer register `rs1` to the floating-point register
    /// `rd`.
    ///
    /// The bits are not modified in the transfer, the payloads of non-canonical
    /// NaNs are preserved.
    pub fn run_fmv_d_x(&mut self, rs1: XRegister, rd: FRegister) -> Result<(), Exception> {
        self.check_fs_on()?;

        let rval = self.xregisters.read(rs1).into();
        self.fregisters.write(rd, rval);
        Ok(())
    }

    /// `FMV.X.D` D-type instruction
    ///
    /// Moves the double-precision value in floating-point register `rs1`
    /// represented in IEEE 754-2008 encoding to the integer register `rd`.
    ///
    /// The bits are not modified in the transfer, the payloads of non-canonical
    /// NaNs are preserved.
    pub fn run_fmv_x_d(&mut self, rs1: FRegister, rd: XRegister) -> Result<(), Exception> {
        self.check_fs_on()?;

        let rval = self.fregisters.read(rs1).into();
        self.xregisters.write(rd, rval);
        Ok(())
    }
}

impl<ML, M> MachineCoreState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::ManagerReadWrite,
{
    /// `FLD` I-type instruction.
    ///
    /// Loads a double-precision floating point value from memory into `rd`.
    /// It uses the same address format as integer-base ISA.
    pub fn run_fld(&mut self, imm: i64, rs1: XRegister, rd: FRegister) -> Result<(), Exception> {
        self.hart.check_fs_on()?;

        let val: u64 = self.read_from_bus(imm, rs1)?;

        self.hart.fregisters.write(rd, val.into());
        Ok(())
    }

    /// `FSD` S-type instruction.
    ///
    /// Stores a double-precision floating point value into memory from `rs2`.
    /// It uses the same address format as integer-base ISA.
    pub fn run_fsd(&mut self, imm: i64, rs1: XRegister, rs2: FRegister) -> Result<(), Exception> {
        self.hart.check_fs_on()?;

        let val: u64 = self.hart.fregisters.read(rs2).into();
        self.write_to_bus(imm, rs1, val)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test,
        bits::Bits64,
        create_backend, create_state,
        machine_state::{
            bus::{devices::DEVICES_ADDRESS_SPACE_LENGTH, main_memory::tests::T1K},
            csregisters::{
                xstatus::{ExtensionValue, MStatus},
                CSRegister,
            },
            hart_state::{HartState, HartStateLayout},
            registers::{fa2, fa3, parse_fregister, parse_xregister, t0},
            MachineCoreState, MachineCoreStateLayout,
        },
        traps::Exception,
    };
    use arbitrary_int::u5;
    use proptest::prelude::*;

    backend_test!(test_fmv_d, F, {
        let mut backend = create_backend!(HartStateLayout, F);
        let state = create_state!(HartState, HartStateLayout, F, backend);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            d in any::<f64>().prop_map(f64::to_bits),
            rs1 in (1_u8..31).prop_map(u5::new).prop_map(parse_xregister),
            rs1_f in (0_u8..31).prop_map(u5::new).prop_map(parse_fregister),
            rs2 in (1_u8..31).prop_map(u5::new).prop_map(parse_xregister),
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset(0);

            // Turn fs on
            let mstatus = MStatus::from_bits(0u64).with_fs(ExtensionValue::Dirty);
            state.csregisters.write(CSRegister::mstatus, mstatus);

            state.xregisters.write(rs1, d);
            assert!(state.run_fmv_d_x(rs1, rs1_f).is_ok());

            assert_eq!(d, u64::from(state.fregisters.read(rs1_f)), "Expected bits to be moved to fregister");

            assert!(state.run_fmv_x_d(rs1_f, rs2).is_ok());

            assert_eq!(d, state.xregisters.read(rs2), "Expected bits to be moved to xregister");
        });
    });

    backend_test!(test_load_store, F, {
        let mut backend = create_backend!(MachineCoreStateLayout<T1K>, F);
        let state = create_state!(
            MachineCoreState,
            MachineCoreStateLayout<T1K>,
            F,
            backend,
            T1K
        );
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            val in any::<f64>().prop_map(f64::to_bits),
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

            // Turn fs on
            let mstatus = MStatus::from_bits(0u64).with_fs(ExtensionValue::Dirty);
            state.hart.csregisters.write(CSRegister::mstatus, mstatus);

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
