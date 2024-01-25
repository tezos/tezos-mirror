//! Module containing helpers for `mstatus` and `sstatus` registers.
//!
//! `sstatus` is a restricted view of `mstatus` being a shadow of it at the same time

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
