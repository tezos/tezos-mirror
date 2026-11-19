// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! `Wei` — the EVM-side unit of the wei amounts the kernel authors outside
//! the EVM execution: bridge deposits, cross-runtime header amounts, fees,
//! and the credits the kernel writes to the EVM ledger. The EVM execution
//! itself stays on alloy's `U256`; the kernel converts at the calls into it.

use std::fmt;

use primitive_types::U256;
use thiserror::Error;

use crate::Mutez;

/// `10^12`: the number of wei in one mutez.
pub const ONE_MUTEZ_WEI: u64 = 1_000_000_000_000;

/// Errors narrowing a [`Wei`] amount to [`Mutez`].
#[derive(Debug, Error, PartialEq, Eq)]
pub enum WeiToMutezError {
    /// Bridge policy: the amount has a non-null sub-mutez remainder, which
    /// would otherwise be silently lost.
    #[error("wei amount has a non-null sub-mutez remainder")]
    NonNullRemainder,
    /// The amount does not fit in the mutez domain (`i64::MAX`, per
    /// L1/MIR).
    #[error("wei amount does not fit in the mutez domain (> i64::MAX)")]
    AmountTooLarge,
}

/// Errors computing with a [`Wei`].
#[derive(Debug, Error, PartialEq, Eq)]
pub enum WeiError {
    /// Checked addition or multiplication overflowed `U256`.
    #[error("wei arithmetic overflow")]
    Overflow,
    /// Checked subtraction underflowed zero.
    #[error("wei arithmetic underflow")]
    Underflow,
}

/// A non-negative amount of wei.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default, Hash)]
pub struct Wei(U256);

impl Wei {
    /// The zero amount.
    pub const ZERO: Wei = Wei(U256::zero());

    /// Wraps a raw `U256` wei amount.
    pub fn from_u256(value: U256) -> Wei {
        Wei(value)
    }

    /// The raw `U256` wei amount, for interop at existing EVM-side seams
    /// (e.g. `u256_to_alloy`).
    pub fn as_u256(self) -> U256 {
        self.0
    }

    /// Whether the amount is zero.
    pub const fn is_zero(self) -> bool {
        self.0.is_zero()
    }

    /// Checked addition; errs on overflow.
    pub fn checked_add(self, other: Wei) -> Result<Wei, WeiError> {
        self.0
            .checked_add(other.0)
            .map(Wei)
            .ok_or(WeiError::Overflow)
    }

    /// Checked subtraction; errs on underflow below zero.
    pub fn checked_sub(self, other: Wei) -> Result<Wei, WeiError> {
        self.0
            .checked_sub(other.0)
            .map(Wei)
            .ok_or(WeiError::Underflow)
    }

    /// Saturating addition; stays at the `U256` maximum on overflow.
    pub fn saturating_add(self, other: Wei) -> Wei {
        Wei(self.0.saturating_add(other.0))
    }

    /// Checked multiplication by a count; errs on overflow.
    pub fn checked_mul(self, factor: u64) -> Result<Wei, WeiError> {
        self.0
            .checked_mul(U256::from(factor))
            .map(Wei)
            .ok_or(WeiError::Overflow)
    }

    /// Bridge policy (today: deposit amount parsing): refuse any sub-mutez
    /// remainder rather than lose it.
    pub fn to_mutez_exact(self) -> Result<Mutez, WeiToMutezError> {
        let scale = U256::from(ONE_MUTEZ_WEI);
        let (quotient, remainder) = self.0.div_mod(scale);
        if !remainder.is_zero() {
            return Err(WeiToMutezError::NonNullRemainder);
        }
        u64::try_from(quotient)
            .ok()
            .and_then(|raw| Mutez::try_from(raw).ok())
            .ok_or(WeiToMutezError::AmountTooLarge)
    }

    /// CRAC policy, for gas-derived and header-carried amounts: floor to
    /// mutez, discarding any sub-mutez remainder.
    ///
    /// Fallible: unlike the bridge's mutez-denominated deposits (bounded by
    /// the total XTZ supply), this policy's callers derive their wei amount
    /// from a user-chosen quantity (e.g. a gas limit) multiplied by a base
    /// fee, which is not supply-bounded — silently saturating at
    /// [`Mutez::MAX`] would misstate the amount rather than surface the
    /// anomaly.
    pub fn to_mutez_floor(self) -> Result<Mutez, WeiToMutezError> {
        let scale = U256::from(ONE_MUTEZ_WEI);
        let quotient = self.0 / scale;
        u64::try_from(quotient)
            .ok()
            .and_then(|raw| Mutez::try_from(raw).ok())
            .ok_or(WeiToMutezError::AmountTooLarge)
    }
}

impl fmt::Display for Wei {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Mutez {
    /// Exact embedding into wei: every mutez amount is representable in
    /// wei, since `i64::MAX * 10^12` fits comfortably in a `U256`.
    pub fn to_wei(self) -> Wei {
        Wei(U256::from(self.as_u64()) * U256::from(ONE_MUTEZ_WEI))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mutez_to_wei_scales_by_one_mutez_wei() {
        let mutez = Mutez::try_from(3u64).unwrap();
        assert_eq!(
            mutez.to_wei(),
            Wei::from_u256(U256::from(3u64 * ONE_MUTEZ_WEI))
        );
    }

    #[test]
    fn mutez_to_wei_zero() {
        assert_eq!(Mutez::ZERO.to_wei(), Wei::ZERO);
    }

    #[test]
    fn is_zero_only_for_zero() {
        assert!(Wei::ZERO.is_zero());
        assert!(!Wei::from_u256(U256::one()).is_zero());
    }

    #[test]
    fn to_mutez_exact_round_trips_from_mutez() {
        let mutez = Mutez::try_from(42u64).unwrap();
        assert_eq!(mutez.to_wei().to_mutez_exact(), Ok(mutez));
    }

    #[test]
    fn to_mutez_exact_rejects_non_null_remainder() {
        let wei = Wei::from_u256(U256::from(ONE_MUTEZ_WEI) + U256::one());
        assert_eq!(wei.to_mutez_exact(), Err(WeiToMutezError::NonNullRemainder));
    }

    #[test]
    fn to_mutez_exact_rejects_amount_too_large() {
        let wei =
            Wei::from_u256(Mutez::MAX.to_wei().as_u256() + U256::from(ONE_MUTEZ_WEI));
        assert_eq!(wei.to_mutez_exact(), Err(WeiToMutezError::AmountTooLarge));
    }

    #[test]
    fn to_mutez_floor_round_trips_from_mutez() {
        let mutez = Mutez::try_from(42u64).unwrap();
        assert_eq!(mutez.to_wei().to_mutez_floor(), Ok(mutez));
    }

    #[test]
    fn to_mutez_floor_discards_remainder() {
        let wei = Wei::from_u256(U256::from(ONE_MUTEZ_WEI) + U256::one());
        assert_eq!(wei.to_mutez_floor(), Ok(Mutez::try_from(1u64).unwrap()));
    }

    #[test]
    fn to_mutez_floor_rejects_past_domain() {
        // No honest way to floor an amount past the mutez domain: unlike
        // the bridge's `to_mutez_exact`, this policy's input is not
        // supply-bounded, so it must error rather than saturate.
        let wei =
            Wei::from_u256(Mutez::MAX.to_wei().as_u256() + U256::from(ONE_MUTEZ_WEI));
        assert_eq!(wei.to_mutez_floor(), Err(WeiToMutezError::AmountTooLarge));
    }

    #[test]
    fn to_mutez_floor_truncation_boundary_at_max() {
        // Mutez::MAX in wei, plus a sub-mutez remainder, must still floor
        // to Mutez::MAX rather than tipping into AmountTooLarge.
        let wei =
            Wei::from_u256(Mutez::MAX.to_wei().as_u256() + U256::from(ONE_MUTEZ_WEI - 1));
        assert_eq!(wei.to_mutez_floor(), Ok(Mutez::MAX));
    }

    #[test]
    fn to_mutez_floor_truncation_boundary_mid_range() {
        // x * 10^12 + r (0 < r < 10^12) must floor to x for a mid-range x.
        let x = 1_234_567u64;
        let r = 999_999u64;
        let wei =
            Wei::from_u256(U256::from(x) * U256::from(ONE_MUTEZ_WEI) + U256::from(r));
        assert_eq!(wei.to_mutez_floor(), Ok(Mutez::try_from(x).unwrap()));
    }

    #[test]
    fn checked_add_sums() {
        let wei = Wei::from_u256(U256::from(2u64))
            .checked_add(Wei::from_u256(U256::from(3u64)));
        assert_eq!(wei, Ok(Wei::from_u256(U256::from(5u64))));
    }

    #[test]
    fn checked_add_rejects_overflow() {
        let wei = Wei::from_u256(U256::MAX).checked_add(Wei::from_u256(U256::one()));
        assert_eq!(wei, Err(WeiError::Overflow));
    }

    #[test]
    fn saturating_add_sums() {
        let wei = Wei::from_u256(U256::from(2u64))
            .saturating_add(Wei::from_u256(U256::from(3u64)));
        assert_eq!(wei, Wei::from_u256(U256::from(5u64)));
    }

    #[test]
    fn saturating_add_saturates() {
        let wei = Wei::from_u256(U256::MAX).saturating_add(Wei::from_u256(U256::one()));
        assert_eq!(wei, Wei::from_u256(U256::MAX));
    }

    #[test]
    fn checked_sub_subtracts() {
        let wei = Wei::from_u256(U256::from(5u64))
            .checked_sub(Wei::from_u256(U256::from(3u64)));
        assert_eq!(wei, Ok(Wei::from_u256(U256::from(2u64))));
    }

    #[test]
    fn checked_sub_rejects_underflow() {
        let wei = Wei::ZERO.checked_sub(Wei::from_u256(U256::one()));
        assert_eq!(wei, Err(WeiError::Underflow));
    }

    #[test]
    fn checked_mul_scales() {
        let wei = Wei::from_u256(U256::from(4u64)).checked_mul(3);
        assert_eq!(wei, Ok(Wei::from_u256(U256::from(12u64))));
    }

    #[test]
    fn checked_mul_rejects_overflow() {
        let wei = Wei::from_u256(U256::MAX).checked_mul(2);
        assert_eq!(wei, Err(WeiError::Overflow));
    }

    #[test]
    fn display_prints_the_wei_digits() {
        let wei = Mutez::try_from(3u64).unwrap().to_wei();
        assert_eq!(wei.to_string(), (3 * ONE_MUTEZ_WEI).to_string());
    }

    #[test]
    fn round_trip_property_small_amounts() {
        for raw in [0u64, 1, 2, 1_000, 1_000_000, i64::MAX as u64] {
            let mutez = Mutez::try_from(raw).unwrap();
            assert_eq!(mutez.to_wei().to_mutez_exact(), Ok(mutez));
            assert_eq!(mutez.to_wei().to_mutez_floor(), Ok(mutez));
        }
    }
}
