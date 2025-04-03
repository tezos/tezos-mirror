// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_UF extension for RISC-V
//!
//! Chapter 11 - "F" Standard Extension for Single-Precision Floating-Point

use rustc_apfloat::Float;
use rustc_apfloat::Status;
use rustc_apfloat::StatusAnd;
use rustc_apfloat::ieee::Single;

use super::float::FloatExt;
use crate::machine_state::MachineCoreState;
use crate::machine_state::hart_state::HartState;
use crate::machine_state::memory;
use crate::machine_state::registers::FRegister;
use crate::machine_state::registers::FValue;
use crate::machine_state::registers::XRegister;
use crate::parser::instruction::InstrRoundingMode;
use crate::state_backend as backend;
use crate::traps::Exception;

impl From<Single> for FValue {
    fn from(f: Single) -> Self {
        let val = f.to_bits();
        f32_to_fvalue(val as u32)
    }
}

impl From<FValue> for Single {
    fn from(f: FValue) -> Self {
        let val: u64 = f.into();

        // Check value correctly NaN boxed:
        // all upper bits must be set to 1
        if val >> 32 != 0xffffffff {
            Single::canonical_nan()
        } else {
            Single::from_bits(val as u32 as u128)
        }
    }
}

const CANONICAL_NAN_BITS: u32 = 0x7fc00000;

impl FloatExt for Single {
    fn canonical_nan() -> Self {
        Self::from_bits(CANONICAL_NAN_BITS as u128)
    }
}

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
    /// `FCLASS.S` F-type instruction.
    ///
    /// See [Self::run_fclass].
    pub fn run_fclass_s(&mut self, rs1: FRegister, rd: XRegister) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fclass::<Single>(rs1, rd);
        Ok(())
    }

    /// `FEQ.S` R-type instruction.
    ///
    /// See [Self::run_feq].
    pub fn run_feq_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_feq::<Single>(rs1, rs2, rd);
        Ok(())
    }

    /// `FLE.S` R-type instruction.
    ///
    /// See [Self::run_fle].
    pub fn run_fle_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fle::<Single>(rs1, rs2, rd);
        Ok(())
    }

    /// `FLT.S` R-type instruction.
    ///
    /// See [Self::run_flt].
    pub fn run_flt_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: XRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_flt::<Single>(rs1, rs2, rd);
        Ok(())
    }

    /// `FADD.S` R-type instruction.
    ///
    /// See [Self::run_fadd].
    pub fn run_fadd_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fadd::<Single>(rs1, rs2, rm, rd)
    }

    /// `FSUB.S` R-type instruction.
    ///
    /// See [Self::run_fsub].
    pub fn run_fsub_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fsub::<Single>(rs1, rs2, rm, rd)
    }

    /// `FMUL.S` R-type instruction.
    ///
    /// See [Self::run_fmul].
    pub fn run_fmul_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fmul::<Single>(rs1, rs2, rm, rd)
    }

    /// `FDIV.S` R-type instruction.
    ///
    /// See [Self::run_fdiv].
    pub fn run_fdiv_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fdiv::<Single>(rs1, rs2, rm, rd)
    }

    /// `FSQRT.S` R-type instruction.
    pub fn run_fsqrt_s(
        &mut self,
        rs1: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        let rval = self.fregisters.read(rs1);
        let rm = self.f_rounding_mode(rm)?;

        let rval = fvalue_to_f32_bits(rval);

        let (StatusAnd { status, value }, _iterations) = ieee_apsqrt::sqrt_accurate(rval, rm);

        if status != Status::OK {
            self.csregisters.set_exception_flag_status(status);
        }

        self.fregisters.write(rd, f32_to_fvalue(value));

        Ok(())
    }

    /// `FMIN.S` R-type instruction.
    ///
    /// See [Self::run_fmin].
    pub fn run_fmin_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fmin::<Single>(rs1, rs2, rd);
        Ok(())
    }

    /// `FMAX.S` R-type instruction.
    ///
    /// See [Self::run_fmax].
    pub fn run_fmax_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fmax::<Single>(rs1, rs2, rd);
        Ok(())
    }

    /// `FMADD.S` instruction.
    ///
    /// See [Self::run_fmadd].
    pub fn run_fmadd_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fmadd::<Single>(rs1, rs2, rs3, rm, rd)
    }

    /// `FMSUB.S` instruction.
    ///
    /// See [Self::run_fmsub].
    pub fn run_fmsub_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fmsub::<Single>(rs1, rs2, rs3, rm, rd)
    }

    /// `FNMSUB.S` instruction.
    ///
    /// See [Self::run_fnmsub].
    pub fn run_fnmsub_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fnmsub::<Single>(rs1, rs2, rs3, rm, rd)
    }

    /// `FNMADD.S` instruction.
    ///
    /// See [Self::run_fnmadd].
    pub fn run_fnmadd_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rs3: FRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fnmadd::<Single>(rs1, rs2, rs3, rm, rd)
    }

    /// `FCVT.S.W` R-type instruction.
    ///
    /// See [Self::run_fcvt_int_fmt].
    pub fn run_fcvt_s_w(
        &mut self,
        rs1: XRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fcvt_int_fmt(rs1, rm, rd, |u| u as i32 as i128, Single::from_i128_r)
    }

    /// `FCVT.S.WU` R-type instruction.
    ///
    /// See [Self::run_fcvt_int_fmt].
    pub fn run_fcvt_s_wu(
        &mut self,
        rs1: XRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fcvt_int_fmt(rs1, rm, rd, |u| u as u32 as u128, Single::from_u128_r)
    }

    /// `FCVT.S.W` R-type instruction.
    ///
    /// See [Self::run_fcvt_int_fmt].
    pub fn run_fcvt_s_l(
        &mut self,
        rs1: XRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fcvt_int_fmt(rs1, rm, rd, |u| u as i64 as i128, Single::from_i128_r)
    }

    /// `FCVT.S.WU` R-type instruction.
    ///
    /// See [Self::run_fcvt_int_fmt].
    pub fn run_fcvt_s_lu(
        &mut self,
        rs1: XRegister,
        rm: InstrRoundingMode,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fcvt_int_fmt(rs1, rm, rd, |u| u as u128, Single::from_u128_r)
    }

    /// `FCVT.S.W` R-type instruction.
    pub fn run_fcvt_w_s(
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
            |f, rm| Single::to_i128_r(f, 32, rm, &mut false),
        )
    }

    /// `FCVT.S.WU` R-type instruction.
    pub fn run_fcvt_wu_s(
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
            |f, rm| Single::to_u128_r(f, 32, rm, &mut false),
        )
    }

    /// `FCVT.S.W` R-type instruction.
    pub fn run_fcvt_l_s(
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
            |f, rm| Single::to_i128_r(f, 64, rm, &mut false),
        )
    }

    /// `FCVT.S.WU` R-type instruction.
    pub fn run_fcvt_lu_s(
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
            |f, rm| Single::to_u128_r(f, 64, rm, &mut false),
        )
    }

    /// `FSGNJ.S` R-type instruction.
    ///
    /// See [Self::run_fsgnj].
    pub fn run_fsgnj_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fsgnj::<Single>(rs1, rs2, rd);
        Ok(())
    }

    /// `FSGNJN.S` R-type instruction.
    ///
    /// See [Self::run_fsgnjn].
    pub fn run_fsgnjn_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fsgnjn::<Single>(rs1, rs2, rd);
        Ok(())
    }

    /// `FSGNJX.S` R-type instruction.
    ///
    /// See [Self::run_fsgnjx].
    pub fn run_fsgnjx_s(
        &mut self,
        rs1: FRegister,
        rs2: FRegister,
        rd: FRegister,
    ) -> Result<(), Exception> {
        self.check_fs_on()?;

        self.run_fsgnjx::<Single>(rs1, rs2, rd);
        Ok(())
    }

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
    pub fn run_fmv_x_w(&mut self, rs1: FRegister, rd: XRegister) -> Result<(), Exception> {
        self.check_fs_on()?;

        let rval: u64 = self.fregisters.read(rs1).into();
        let rval = rval as i32 as u64;

        self.xregisters.write(rd, rval);
        Ok(())
    }

    /// `FMV.W.X` F-type instruction
    ///
    /// Moves the single-precision value encoded in IEEE 754-2008 standard
    /// encoding from the lower 32 bits of integer register `rs1` to the
    /// floating-point register `rd`.
    ///
    /// The bits are not modified in the transfer,
    /// and in particular, the payloads of non-canonical NaNs are preserved.
    pub fn run_fmv_w_x(&mut self, rs1: XRegister, rd: FRegister) -> Result<(), Exception> {
        self.check_fs_on()?;

        let rval = self.xregisters.read(rs1) as u32;
        let rval = f32_to_fvalue(rval);

        self.fregisters.write(rd, rval);
        Ok(())
    }
}

impl<MC, M> MachineCoreState<MC, M>
where
    MC: memory::MemoryConfig,
    M: backend::ManagerReadWrite,
{
    /// `FLW` I-type instruction.
    ///
    /// Loads a single-precision floating point value from memory into `rd`.
    /// It uses the same address format as integer-base ISA.
    pub fn run_flw(&mut self, imm: i64, rs1: XRegister, rd: FRegister) -> Result<(), Exception> {
        self.hart.check_fs_on()?;

        let val: u32 = self.read_from_bus(imm, rs1)?;
        let val = f32_to_fvalue(val);

        self.hart.fregisters.write(rd, val);
        Ok(())
    }

    /// `FSW` S-type instruction.
    ///
    /// Stores a single-precision floating point value into memory from `rs2`.
    /// It uses the same address format as integer-base ISA.
    pub fn run_fsw(&mut self, imm: i64, rs1: XRegister, rs2: FRegister) -> Result<(), Exception> {
        self.hart.check_fs_on()?;

        let val: u64 = self.hart.fregisters.read(rs2).into();
        self.write_to_bus(imm, rs1, val as u32)
    }
}

/// The upper 32 bits are set to `1` for any `f32` write.
#[inline(always)]
fn f32_to_fvalue(val: u32) -> FValue {
    (val as u64 | 0xffffffff00000000).into()
}

fn fvalue_to_f32_bits(f: FValue) -> u32 {
    let val: u64 = f.into();

    // Check value correctly NaN boxed:
    // all upper bits must be set to 1
    if val >> 32 != 0xffffffff {
        CANONICAL_NAN_BITS
    } else {
        val as u32
    }
}

#[cfg(test)]
mod tests {
    use arbitrary_int::u5;
    use proptest::prelude::*;
    use rustc_apfloat::Float;
    use rustc_apfloat::ieee::Double;
    use rustc_apfloat::ieee::Single;

    use super::f32_to_fvalue;
    use crate::backend_test;
    use crate::bits::Bits64;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::csregisters::CSRegister;
    use crate::machine_state::csregisters::xstatus::ExtensionValue;
    use crate::machine_state::csregisters::xstatus::MStatus;
    use crate::machine_state::hart_state::HartState;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::fa1;
    use crate::machine_state::registers::fa4;
    use crate::machine_state::registers::parse_fregister;
    use crate::machine_state::registers::parse_xregister;
    use crate::machine_state::registers::t0;
    use crate::state::NewState;
    use crate::traps::Exception;

    backend_test!(test_fmv_f, F, {
        proptest!(|(
            f in any::<f32>().prop_map(f32::to_bits),
            rs1 in (1_u8..31).prop_map(u5::new).prop_map(parse_xregister),
            rs1_f in (1_u8..31).prop_map(u5::new).prop_map(parse_fregister),
            rs2 in (1_u8..31).prop_map(u5::new).prop_map(parse_xregister),
        )| {
            let mut state = HartState::new(&mut F::manager());

            // Turn fs on
            let mstatus = MStatus::from_bits(0u64).with_fs(ExtensionValue::Dirty);
            state.csregisters.write(CSRegister::mstatus, mstatus);

            state.xregisters.write(rs1, f as u64);

            assert!(state.run_fmv_w_x(rs1, rs1_f).is_ok());

            let read: u64 = state.fregisters.read(rs1_f).into();
            assert_eq!(f, read as u32, "Expected bits to be moved to fregister");

            let f_64 = Double::from_bits(read as u128);
            assert!(f_64.is_nan() && !f_64.is_signaling());

            assert!(state.run_fmv_x_w(rs1_f, rs2).is_ok());

            let read = state.xregisters.read(rs2);
            assert_eq!(f, read as u32, "Expected bits to be moved to xregister");

            let f_32 = Single::from_bits(f as u128);
            if f_32.is_negative() {
                assert_eq!(read, (f as u64) | 0xffffffff00000000, "Expected sign byte to be extended");
            } else {
                assert_eq!(read, f as u64, "Expected no sign byte to be extended");
            }
        });
    });

    backend_test!(test_load_store, F, {
        let state = MachineCoreState::<M4K, _>::new(&mut F::manager());
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            val in any::<f32>().prop_map(f32::to_bits),
        )|
        {
            let mut state = state_cell.borrow_mut();
            state.reset();
            state.main_memory.set_all_readable_writeable();

            // Turn fs on
            let mstatus = MStatus::from_bits(0u64).with_fs(ExtensionValue::Dirty);
            state.hart.csregisters.write(CSRegister::mstatus, mstatus);

            let mut perform_test = |offset: u64| -> Result<(), Exception> {
                // Save test values v_i in registers ai
                state.hart.fregisters.write(fa1, f32_to_fvalue(val));

                // t0 will hold the "global" offset of all loads / stores we are going to make
                state.hart.xregisters.write(t0, offset);

                // Perform the stores
                state.run_fsw(4, t0, fa1)?;

                state.run_flw(4, t0, fa4)?;

                assert_eq!(state.hart.fregisters.read(fa4), f32_to_fvalue(val));
                Ok(())
            };

            let invalid_offset = 0u64.wrapping_sub(1024);
            let aligned_offset = 512;
            let misaligned_offset = 513;

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
