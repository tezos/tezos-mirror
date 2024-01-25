//! Module containing helpers for `mstatus` and `sstatus` registers.
//!
//! `sstatus` is a restricted view of `mstatus` being a shadow of it at the same time

// Allow unused setters & getters
#![allow(dead_code)]
// Allow non snake case for setters & getters
#![allow(non_snake_case)]

use crate::csregisters::{ones, CSRValue};
use paste::paste;

/// Common trait for fields in `mstatus` register
pub trait FieldValue {
    /// Returns a typed field based on raw `u64` value with relevant bits shifted to the lowest bits
    // See tests for more details & example
    fn new(value: u64) -> Self;
    fn raw_bits(self) -> u64;
}

/// Field in `mstatus` for a boolean value
impl FieldValue for bool {
    fn new(value: u64) -> Self {
        (value & 1) == 1
    }

    fn raw_bits(self) -> u64 {
        self as u64
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
#[repr(u8)]
pub enum MPPValue {
    User = 0b00,
    Supervisor = 0b01,
    Machine = 0b11,
}

impl FieldValue for MPPValue {
    fn new(value: u64) -> Self {
        match value & 0b11 {
            0b00 => MPPValue::User,
            0b01 => MPPValue::Supervisor,
            0b11 => MPPValue::Machine,
            // WARL field, invalid value considered User
            0b10 => MPPValue::User,
            _ => unreachable!(),
        }
    }

    fn raw_bits(self) -> u64 {
        self as u8 as u64
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
#[repr(u8)]
pub enum SPPValue {
    User = 0b0,
    Supervisor = 0b1,
}

impl FieldValue for SPPValue {
    fn new(value: u64) -> Self {
        match value & 1 {
            0b0 => SPPValue::User,
            0b1 => SPPValue::Supervisor,
            // WARL field, invalid value considered User
            _ => unreachable!(),
        }
    }

    fn raw_bits(self) -> u64 {
        self as u8 as u64
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
#[repr(u8)]
pub enum XLenValue {
    MXL32 = 0b01,
    MXL64 = 0b10,
    MXL128 = 0b11,
}

impl FieldValue for XLenValue {
    fn raw_bits(self) -> u64 {
        self as u8 as u64
    }

    fn new(value: u64) -> Self {
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

impl FieldValue for ExtensionValue {
    fn new(value: u64) -> Self {
        match value & 0b11 {
            0b00 => ExtensionValue::Off,
            0b01 => ExtensionValue::Initial,
            0b10 => ExtensionValue::Clean,
            0b11 => ExtensionValue::Dirty,
            // WARL field, invalid value considered Off
            _ => unreachable!(),
        }
    }

    fn raw_bits(self) -> u64 {
        self as u8 as u64
    }
}

macro_rules! create_field_constant {
    ($name:ident, $offset:expr, $width:expr) => {
        const $name: (u64, u64) = ($offset, $width);
    };
}

macro_rules! create_get_field {
    ($name:ident, $f_type:ty, $offset:expr, $width:expr) => {
        paste! { pub fn [<get_ $name>] (mstatus: CSRValue) -> $f_type {
            <$f_type>::new((mstatus >> $offset) & ones($width))
        } }
    };
}

macro_rules! create_set_field {
    ($name:ident, $f_type:ty, $offset:expr, $width:expr) => {
        paste! {pub fn [<set_ $name>] (mstatus: CSRValue, new_value: $f_type) -> CSRValue {
            // Get only the last width bits
            let field_bits = new_value.raw_bits() & ones($width);
            // clear the field bits
            let mstatus = mstatus & !(ones($width) << $offset);
            // set the field with the new bits
            mstatus | (field_bits << $offset)
        } }
    };
}

macro_rules! create_field {
    // offset, width represent the offset in the register and bit width
    ($field:ident, $f_type:ty, $offset:expr, $width:expr) => {
        create_field_constant!($field, $offset, $width);
        create_set_field!($field, $f_type, $offset, $width);
        create_get_field!($field, $f_type, $offset, $width);
    };
}

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
create_field!(SPP, bool, 8, 1);
create_field!(MPIE, bool, 7, 1);
create_field!(UBE, bool, 6, 1);
create_field!(SPIE, bool, 5, 1);
create_field!(MIE, bool, 3, 1);
create_field!(SIE, bool, 1, 1);

const fn field_mask(field_data: (u64, u64)) -> CSRValue {
    ones(field_data.1) << field_data.0
}

pub const SSTATUS_FIELDS_MASK: CSRValue = field_mask(SD)
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

pub const MSTATUS_FIELDS_MASK: CSRValue = SSTATUS_FIELDS_MASK
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

pub fn apply_warl_mstatus(mstatus: CSRValue) -> CSRValue {
    let mstatus = apply_warl_sstatus(mstatus);

    // set SXL as 64 (our implementation fixes MXL, SXL, UXL as 64)
    let mstatus = set_SXL(mstatus, XLenValue::MXL64);

    // reset MPP in case new value is invalid
    let mpp = get_MPP(mstatus);
    set_MPP(mstatus, mpp)
}

pub fn apply_warl_sstatus(mstatus: CSRValue) -> CSRValue {
    use ExtensionValue::Dirty;

    // set sd = (FS==11) OR (XS==11) OR (VS=11)
    let xs = get_XS(mstatus);
    let vs = get_VS(mstatus);
    let fs = get_FS(mstatus);

    let mstatus = set_SD(mstatus, xs == Dirty || fs == Dirty || vs == Dirty);

    // set UXL as 64 (our implementation fixes MXL, SXL, UXL as 64)
    set_UXL(mstatus, XLenValue::MXL64)
}

pub fn sstatus_from_mstatus(mstatus: u64) -> u64 {
    mstatus & SSTATUS_FIELDS_MASK
}

#[cfg(test)]
pub mod tests {
    #[test]
    fn test_status_fields() {
        use crate::csregisters::xstatus::{ExtensionValue, FieldValue};

        let field = bool::new(0xF0F0_0000_AAAA_0001);
        assert!(field);

        let field = bool::new(0x0002);
        assert!(!field);

        let field = ExtensionValue::new(0b1111_0010);
        assert!(field == ExtensionValue::Clean);
        assert_eq!(field.raw_bits(), 0b10);
    }
}
