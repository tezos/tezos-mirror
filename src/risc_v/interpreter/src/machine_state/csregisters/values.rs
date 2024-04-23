// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::bits::Bits64;

/// Representation of a value in a CSR
pub type CSRRepr = u64;

/// Value of a Control or State register
#[derive(
    Copy,
    Clone,
    Debug,
    derive_more::Display,
    derive_more::From,
    derive_more::Into,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
)]
#[repr(transparent)]
pub struct CSRValue(CSRRepr);

impl CSRValue {
    /// Access the underlying representation.
    pub fn repr(self) -> CSRRepr {
        self.0
    }
}

impl Bits64 for CSRValue {
    const WIDTH: usize = CSRRepr::WIDTH;

    fn from_bits(value: u64) -> Self {
        Self(value)
    }

    fn to_bits(&self) -> u64 {
        self.repr()
    }
}
