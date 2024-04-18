// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

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
