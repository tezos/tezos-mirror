// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// Common trait for fields in registers.
pub trait FieldValue {
    /// Returns a typed field based on raw `u64` value where
    /// the relevant bits are shifted to the lowest bits.
    // See tests for more details & example
    fn new(value: u64) -> Self;

    /// Returns the underlying bits of the field.
    fn raw_bits(&self) -> u64;
}

/// Generic field of a register which holds an unsigned value
pub struct UnsignedValue {
    bits: u64,
}

impl UnsignedValue {
    pub fn value(&self) -> u64 {
        self.raw_bits()
    }
}

impl FieldValue for UnsignedValue {
    fn new(value: u64) -> Self {
        UnsignedValue { bits: value }
    }

    fn raw_bits(&self) -> u64 {
        self.bits
    }
}

pub struct FieldProps {
    pub offset: u64,
    pub width: u64,
}

impl FieldProps {
    pub const fn new(offset: u64, width: u64) -> Self {
        Self { offset, width }
    }
}

#[macro_export]
macro_rules! create_field_constant {
    ($name:ident, $offset:expr, $width:expr) => {
        pub const $name: $crate::machine_state::csregisters::fields::FieldProps =
            $crate::machine_state::csregisters::fields::FieldProps::new($offset, $width);
    };
}

#[macro_export]
macro_rules! create_get_field {
    ($name:ident, $f_type:ty, $offset:expr, $width:expr) => {
        paste::paste! { pub fn [<get_ $name>]<I: Into<$crate::machine_state::csregisters::values::CSRRepr>>(reg_value: I) -> $f_type {
            use $crate::machine_state::csregisters::ones;
            use $crate::machine_state::csregisters::fields::FieldValue;
            <$f_type>::new((reg_value.into() >> $offset) & ones($width))
        } }
    };
}

#[macro_export]
macro_rules! create_set_field {
    ($name:ident, $f_type:ty, $offset:expr, $width:expr) => {
        paste::paste! {pub fn [<set_ $name>]<I: Into<$crate::machine_state::csregisters::values::CSRRepr>>(reg_value: I, new_value: $f_type) -> $crate::machine_state::csregisters::values::CSRRepr {
            use $crate::machine_state::csregisters::ones;
            use $crate::machine_state::csregisters::fields::FieldValue;
            // Get only the last width bits
            let field_bits = new_value.raw_bits() & ones($width);
            // clear the field bits
            let reg_value = reg_value.into() & !(ones($width) << $offset);
            // set the field with the new bits
            reg_value | (field_bits << $offset)
        } }
    };
}

#[macro_export]
macro_rules! create_field {
    // offset, width represent the offset in the register and bit width
    ($field:ident, $f_type:ty, $offset:expr, $width:expr) => {
        $crate::create_field_constant!($field, $offset, $width);
        $crate::create_set_field!($field, $f_type, $offset, $width);
        $crate::create_get_field!($field, $f_type, $offset, $width);
    };
}
