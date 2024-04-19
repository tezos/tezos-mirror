// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Module containing helpers for `mstatus` and `sstatus` registers.
//!
//! The `sstatus` register is a subset of the `mstatus` register.
//! This mechanism is described as "shadow" CSRs in RISC-V spec.

// Allow unused setters & getters
#![allow(dead_code)]
// Allow non snake case for setters & getters
#![allow(non_snake_case)]

use super::{fields::FieldProps, ones};
use crate::{
    bits::Bits64,
    create_field,
    machine_state::{csregisters::CSRRepr, mode::Mode},
};

#[derive(PartialEq, Clone, Copy, Debug)]
#[repr(u8)]
pub enum MPPValue {
    User = 0b00,
    Supervisor = 0b01,
    Machine = 0b11,
}

impl From<MPPValue> for Mode {
    fn from(other: MPPValue) -> Mode {
        match other {
            MPPValue::User => Mode::User,
            MPPValue::Supervisor => Mode::Supervisor,
            MPPValue::Machine => Mode::Machine,
        }
    }
}

impl Bits64 for MPPValue {
    const WIDTH: usize = 2;

    fn from_bits(value: u64) -> Self {
        match value & 0b11 {
            0b00 => MPPValue::User,
            0b01 => MPPValue::Supervisor,
            0b11 => MPPValue::Machine,
            // WARL field, invalid value `10`is considered User
            0b10 => MPPValue::User,
            _ => unreachable!(),
        }
    }

    fn to_bits(&self) -> u64 {
        *self as u8 as u64
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
#[repr(u8)]
pub enum SPPValue {
    User = 0b0,
    Supervisor = 0b1,
}

impl Bits64 for SPPValue {
    const WIDTH: usize = 1;

    fn from_bits(value: u64) -> Self {
        match value & 1 {
            0b0 => SPPValue::User,
            0b1 => SPPValue::Supervisor,
            _ => unreachable!(),
        }
    }

    fn to_bits(&self) -> u64 {
        *self as u8 as u64
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
#[repr(u8)]
pub enum XLenValue {
    MXL32 = 0b01,
    MXL64 = 0b10,
    MXL128 = 0b11,
}

impl Bits64 for XLenValue {
    const WIDTH: usize = 2;

    fn to_bits(&self) -> u64 {
        *self as u8 as u64
    }

    fn from_bits(value: u64) -> Self {
        match value & 0b11 {
            0b01 => XLenValue::MXL32,
            0b10 => XLenValue::MXL64,
            0b11 => XLenValue::MXL128,
            // WARL field, invalid value considered 64 bits
            0b00 => XLenValue::MXL64,
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
#[repr(u8)]
pub enum ExtensionValue {
    Off = 0b00,
    Initial = 0b01,
    Clean = 0b10,
    Dirty = 0b11,
}

impl Bits64 for ExtensionValue {
    const WIDTH: usize = 2;

    fn from_bits(value: u64) -> Self {
        match value & 0b11 {
            0b00 => ExtensionValue::Off,
            0b01 => ExtensionValue::Initial,
            0b10 => ExtensionValue::Clean,
            0b11 => ExtensionValue::Dirty,
            _ => unreachable!(),
        }
    }

    fn to_bits(&self) -> u64 {
        *self as u8 as u64
    }
}

// MSTATUS & SSTATUS fields
create_field!(SD, bool, 63, 1);
create_field!(MBE, bool, 37, 1);
create_field!(SBE, bool, 36, 1);
create_field!(SXL, XLenValue, 34, 2);
create_field!(UXL, XLenValue, 32, 2);
create_field!(TSR, bool, 22, 1);
create_field!(TW, bool, 21, 1);
create_field!(TVM, bool, 20, 1);
create_field!(MXR, bool, 19, 1);
create_field!(SUM, bool, 18, 1);
create_field!(MPRV, bool, 17, 1);
create_field!(XS, ExtensionValue, 15, 2);
create_field!(FS, ExtensionValue, 13, 2);
create_field!(MPP, MPPValue, 11, 2);
create_field!(VS, ExtensionValue, 9, 2);
create_field!(SPP, SPPValue, 8, 1);
create_field!(MPIE, bool, 7, 1);
create_field!(UBE, bool, 6, 1);
create_field!(SPIE, bool, 5, 1);
create_field!(MIE, bool, 3, 1);
create_field!(SIE, bool, 1, 1);

// MNSTATUS fields (SMRNMI extension)
// MNPP field behaves similarly as MPP for mstatus
create_field!(MNPP, MPPValue, 11, 2);
// Field specifically used in mnstatus, holds previous virtualization mode,
create_field!(MNPV, bool, 7, 1);
// NMIE - Non-maskable interrupt enable bit
// When 0 it disables all interrupts globally (absolutely no interrupts are handled)
// When 1 - Non-maskable interrupts are enabled (and all other interrupts behave as normal)
create_field!(NMIE, bool, 3, 1);

const fn field_mask(field_data: FieldProps) -> CSRRepr {
    ones(field_data.width) << field_data.offset
}

pub const SSTATUS_FIELDS_MASK: CSRRepr = field_mask(SD)
    | field_mask(UXL)
    | field_mask(MXR)
    | field_mask(SUM)
    | field_mask(XS)
    | field_mask(FS)
    | field_mask(VS)
    | field_mask(SPP)
    | field_mask(UBE)
    | field_mask(SPIE)
    | field_mask(SIE);

pub const MSTATUS_FIELDS_MASK: CSRRepr = SSTATUS_FIELDS_MASK
    | field_mask(MBE)
    | field_mask(SBE)
    | field_mask(SXL)
    | field_mask(TSR)
    | field_mask(TW)
    | field_mask(TVM)
    | field_mask(MPRV)
    | field_mask(MPP)
    | field_mask(MPIE)
    | field_mask(MIE);

pub fn apply_warl_mstatus(mstatus: CSRRepr) -> CSRRepr {
    let mstatus = apply_warl_sstatus(mstatus);

    // set SXL as 64 (our implementation fixes MXL, SXL, UXL as 64)
    let mstatus = set_SXL(mstatus, XLenValue::MXL64);

    // reset MPP in case new value is invalid
    let mpp = get_MPP(mstatus);
    set_MPP(mstatus, mpp)
}

pub fn apply_warl_sstatus(mut mstatus: CSRRepr) -> CSRRepr {
    use ExtensionValue::Dirty;

    // set sd = (FS==11) OR (XS==11) OR (VS=11)
    let xs = get_XS(mstatus);
    let vs = get_VS(mstatus);
    let mut fs = get_FS(mstatus);

    if fs != ExtensionValue::Off {
        fs = Dirty;
        mstatus = set_FS(mstatus, Dirty);
    }

    let mstatus = set_SD(mstatus, xs == Dirty || fs == Dirty || vs == Dirty);

    // set UXL as 64 (our implementation fixes MXL, SXL, UXL as 64)
    set_UXL(mstatus, XLenValue::MXL64)
}

pub fn apply_warl_mnstatus(mnstatus: CSRRepr) -> CSRRepr {
    // Since we don't support virtualization mode it is read-only 0 WARL
    let mnstatus = set_MNPV(mnstatus, false);

    // Our interpreter does not have any source of non-maskable interrupts
    // but we still have other interrupts that need to be handled, so this is read-only 1
    let mnstatus = set_NMIE(mnstatus, true);

    // Similar to MPP field
    let mnpp = get_MNPP(mnstatus);
    set_MNPP(mnstatus, mnpp)
}

pub fn sstatus_from_mstatus(mstatus: u64) -> u64 {
    mstatus & SSTATUS_FIELDS_MASK
}

#[cfg(test)]
mod tests {
    use crate::{
        bits::Bits64,
        machine_state::csregisters::xstatus::{ExtensionValue, MPPValue, SPPValue, XLenValue},
    };

    #[test]
    fn test_status_fields() {
        let field = bool::from_bits(0xF0F0_0000_AAAA_0001);
        assert!(field);

        let field = bool::from_bits(0x0002);
        assert!(!field);

        let field = ExtensionValue::from_bits(0b1111_0010);
        assert_eq!(field, ExtensionValue::Clean);
        assert_eq!(field.to_bits(), 0b10);

        let field = XLenValue::from_bits(0b01);
        assert_eq!(field, XLenValue::MXL32);
        assert_eq!(field.to_bits(), 0b01);

        let field = MPPValue::from_bits(0b1010);
        assert_eq!(field, MPPValue::User);
        assert_eq!(field.to_bits(), 0b00);

        let field = SPPValue::from_bits(0b111);
        assert_eq!(field, SPPValue::Supervisor);
        assert_eq!(field.to_bits(), 0b1);
    }
}
