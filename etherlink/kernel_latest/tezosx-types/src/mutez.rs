// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! `Mutez` — the Tezos native-token unit, matching L1's `Tez_repr` and
//! MIR's `Mutez(i64)`: a non-negative amount bounded at `i64::MAX`. An
//! unbounded `u64` would be wider than both domains, leaving
//! `BALANCE`/`AMOUNT` conversions fallible for otherwise-valid values.

use std::fmt;

use tezos_data_encoding::enc::{BinResult, BinWriter};
use tezos_data_encoding::encoding::{Encoding, HasEncoding};
use tezos_data_encoding::nom::error::{BoundedEncodingKind, DecodeError};
use tezos_data_encoding::nom::{NomReader, NomResult};
use tezos_data_encoding::types::Narith;
use thiserror::Error;

/// Errors constructing or computing with a [`Mutez`].
#[derive(Debug, Error, PartialEq, Eq)]
pub enum MutezError {
    /// A raw `u64` does not fit in the mutez domain (`i64::MAX`).
    #[error("amount {0} does not fit in the mutez domain (> i64::MAX)")]
    OutOfDomain(u64),
    /// A decoded `Narith` does not fit in `u64` at all, let alone the
    /// mutez domain. Expected-unreachable: total supply is far below
    /// `u64::MAX` mutez.
    #[error("Narith amount {} does not fit in u64", .0.0)]
    NarithTooLarge(Narith),
    /// A signed amount (e.g. a MIR `Mutez(i64)`) is negative.
    #[error("amount {0} is negative")]
    Negative(i64),
    /// Checked addition/subtraction over- or under-flowed the domain.
    #[error("mutez arithmetic overflow")]
    Overflow,
}

/// A non-negative amount of mutez, bounded at `i64::MAX`. Every constructor
/// and arithmetic operation is checked: there is no way to build or reach a
/// `Mutez` outside the domain.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default, Hash)]
pub struct Mutez(u64);

impl Mutez {
    /// The zero amount.
    pub const ZERO: Mutez = Mutez(0);

    /// The upper bound of the mutez domain (`i64::MAX`, matching L1's
    /// `Tez_repr` and MIR's `Mutez(i64)`).
    pub const MAX: Mutez = Mutez(i64::MAX as u64);

    /// Checked addition; errs on overflow past [`Mutez::MAX`].
    pub fn checked_add(self, other: Mutez) -> Result<Mutez, MutezError> {
        self.0
            .checked_add(other.0)
            .filter(|&sum| sum <= Self::MAX.0)
            .map(Mutez)
            .ok_or(MutezError::Overflow)
    }

    /// Checked subtraction; errs on underflow below zero.
    pub fn checked_sub(self, other: Mutez) -> Result<Mutez, MutezError> {
        self.0
            .checked_sub(other.0)
            .map(Mutez)
            .ok_or(MutezError::Overflow)
    }

    /// Raw digits, for interop with call sites not yet ported to `Mutez`.
    pub fn as_u64(self) -> u64 {
        self.0
    }

    /// Encodes to the wire `Narith` representation used by Michelson
    /// balances and transfer amounts.
    pub fn to_narith(self) -> Narith {
        Narith::from(self.0)
    }
}

impl TryFrom<u64> for Mutez {
    type Error = MutezError;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        if value > Self::MAX.0 {
            Err(MutezError::OutOfDomain(value))
        } else {
            Ok(Mutez(value))
        }
    }
}

impl TryFrom<&Narith> for Mutez {
    type Error = MutezError;

    /// The read policy for narrowing a decoded `Narith` to the mutez
    /// domain: a hard error, never a panic. An out-of-domain value read
    /// from storage or the wire is a corruption to report, not a state to
    /// reach.
    fn try_from(value: &Narith) -> Result<Self, Self::Error> {
        let raw = u64::try_from(&value.0)
            .map_err(|_| MutezError::NarithTooLarge(value.clone()))?;
        Mutez::try_from(raw)
    }
}

impl TryFrom<i64> for Mutez {
    type Error = MutezError;

    /// Narrows a signed amount (e.g. a MIR `Mutez(i64)`) to the mutez
    /// domain: a negative value is a hard error, never a panic or a
    /// silent absolute value.
    fn try_from(value: i64) -> Result<Self, Self::Error> {
        u64::try_from(value)
            .map_err(|_| MutezError::Negative(value))
            .and_then(Mutez::try_from)
    }
}

impl From<Mutez> for i64 {
    /// Exact embedding into `i64`: every `Mutez` is representable, since
    /// the domain is bounded at `i64::MAX`.
    fn from(value: Mutez) -> Self {
        value.0 as i64
    }
}

/// Encoded as a `Narith`, the encoding of a mutez amount on L1.
impl HasEncoding for Mutez {
    fn encoding() -> Encoding {
        Encoding::N
    }
}

/// Decodes a `Narith`, and fails on a value outside the mutez domain.
impl NomReader<'_> for Mutez {
    fn nom_read(bytes: &[u8]) -> NomResult<'_, Self> {
        let (rest, narith) = Narith::nom_read(bytes)?;
        match Mutez::try_from(&narith) {
            Ok(mutez) => Ok((rest, mutez)),
            Err(_) => Err(nom::Err::Error(DecodeError::limit(
                bytes,
                BoundedEncodingKind::Bounded,
            ))),
        }
    }
}

/// Encodes as a `Narith`.
impl BinWriter for Mutez {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        self.to_narith().bin_write(output)
    }
}

impl fmt::Display for Mutez {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_encoding_roundtrip() {
        for value in [0u64, 1, 127, 128, 1_000_000, i64::MAX as u64] {
            let mutez = Mutez::try_from(value).unwrap();
            let bytes = mutez.to_bytes().unwrap();
            assert_eq!(
                Mutez::nom_read_exact(&bytes),
                Ok(mutez),
                "round-trip failed for {value}"
            );
            assert_eq!(
                bytes,
                Narith::from(value).to_bytes().unwrap(),
                "not the Narith encoding of {value}"
            );
        }
    }

    #[test]
    fn binary_decoding_rejects_above_max() {
        let bytes = Narith::from(i64::MAX as u64 + 1).to_bytes().unwrap();
        let error = Mutez::nom_read_exact(&bytes).unwrap_err();
        assert!(error.to_string().contains("boundary violation"));
    }

    #[test]
    fn try_from_u64_accepts_max() {
        assert_eq!(Mutez::try_from(i64::MAX as u64), Ok(Mutez::MAX));
    }

    #[test]
    fn try_from_u64_rejects_above_max() {
        let value = i64::MAX as u64 + 1;
        assert_eq!(Mutez::try_from(value), Err(MutezError::OutOfDomain(value)));
    }

    #[test]
    fn try_from_u64_accepts_zero() {
        assert_eq!(Mutez::try_from(0u64), Ok(Mutez::ZERO));
    }

    #[test]
    fn checked_add_within_domain() {
        let a = Mutez::try_from(10u64).unwrap();
        let b = Mutez::try_from(20u64).unwrap();
        assert_eq!(a.checked_add(b), Ok(Mutez::try_from(30u64).unwrap()));
    }

    #[test]
    fn checked_add_rejects_overflow_past_max() {
        let one = Mutez::try_from(1u64).unwrap();
        assert_eq!(Mutez::MAX.checked_add(one), Err(MutezError::Overflow));
    }

    #[test]
    fn checked_add_rejects_u64_overflow() {
        let a = Mutez(u64::MAX);
        let b = Mutez::try_from(1u64).unwrap();
        assert_eq!(a.checked_add(b), Err(MutezError::Overflow));
    }

    #[test]
    fn checked_sub_within_domain() {
        let a = Mutez::try_from(20u64).unwrap();
        let b = Mutez::try_from(5u64).unwrap();
        assert_eq!(a.checked_sub(b), Ok(Mutez::try_from(15u64).unwrap()));
    }

    #[test]
    fn checked_sub_rejects_underflow() {
        let a = Mutez::try_from(5u64).unwrap();
        let b = Mutez::try_from(6u64).unwrap();
        assert_eq!(a.checked_sub(b), Err(MutezError::Overflow));
    }

    #[test]
    fn narith_round_trip() {
        let m = Mutez::try_from(42u64).unwrap();
        let narith = m.to_narith();
        assert_eq!(Mutez::try_from(&narith), Ok(m));
    }

    #[test]
    fn narith_out_of_domain_is_hard_error() {
        let value = i64::MAX as u64 + 1;
        let narith = Narith::from(value);
        assert_eq!(
            Mutez::try_from(&narith),
            Err(MutezError::OutOfDomain(value))
        );
    }

    #[test]
    fn narith_too_large_for_u64_is_hard_error() {
        let huge = num_bigint::BigUint::from(u64::MAX) * num_bigint::BigUint::from(2u64);
        let narith = Narith(huge);
        assert_eq!(
            Mutez::try_from(&narith),
            Err(MutezError::NarithTooLarge(narith.clone()))
        );
    }

    #[test]
    fn display_matches_raw_digits() {
        let m = Mutez::try_from(12345u64).unwrap();
        assert_eq!(m.to_string(), "12345");
    }

    #[test]
    fn try_from_i64_accepts_non_negative() {
        assert_eq!(Mutez::try_from(42i64), Ok(Mutez::try_from(42u64).unwrap()));
    }

    #[test]
    fn try_from_i64_rejects_negative() {
        assert_eq!(Mutez::try_from(-1i64), Err(MutezError::Negative(-1)));
    }

    #[test]
    fn try_from_i64_accepts_max() {
        assert_eq!(Mutez::try_from(i64::MAX), Ok(Mutez::MAX));
    }

    #[test]
    fn i64_round_trip_from_mutez() {
        let m = Mutez::try_from(42u64).unwrap();
        assert_eq!(i64::from(m), 42i64);
    }

    #[test]
    fn i64_from_mutez_max_matches_i64_max() {
        assert_eq!(i64::from(Mutez::MAX), i64::MAX);
    }
}
