// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 PK Lab <contact@pklab.io>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;

pub type Wei = U256;

pub const ETH_AS_WEI: u64 = 1_000_000_000_000_000_000;

pub fn from_eth(eth: u64) -> Wei {
    Wei::from(eth) * Wei::from(ETH_AS_WEI)
}

pub fn eth_from_mutez(mutez: u64) -> Wei {
    // Mutez is 10^6, Wei is 10^18
    U256::from(mutez) * U256::exp10(12)
}

#[derive(Debug)]
pub enum ErrorMutezFromWei {
    AmountTooLarge,
    NonNullRemainder,
}

/// Convert Michelson gas to mutez using the current base fee.
///
/// Units: base_fee_per_gas (wei/evm_gas) * multiplier (evm_gas/michelson_gas)
///        * gas (michelson_gas) = wei, then / 10^12 (wei/mutez) = mutez.
pub fn michelson_gas_to_mutez(base_fee_per_gas: U256, multiplier: u64, gas: u64) -> u64 {
    let wei = base_fee_per_gas * U256::from(multiplier) * U256::from(gas);
    // NB: Convert back to mutez with a floor division.
    // (precision loss if gas_fee_wei < 1 mutez)
    (wei / U256::exp10(12)).low_u64()
}

/// Ceiling division of `a` by `b` on `U256`.
///
/// The caller must ensure `b != 0`.
pub fn ceil_div(a: U256, b: U256) -> U256 {
    match a.div_mod(b) {
        (quotient, remainder) if remainder.is_zero() => quotient,
        (quotient, _) => quotient.saturating_add(U256::one()),
    }
}

/// EVM gas equivalent of a wei amount at the given base fee (ceiling).
///
/// Returns `None` when `base_fee_per_gas` is zero or the gas amount
/// overflows `u64`.
pub fn gas_from_wei(wei: U256, base_fee_per_gas: U256) -> Option<u64> {
    if base_fee_per_gas.is_zero() {
        return None;
    }
    u64::try_from(ceil_div(wei, base_fee_per_gas)).ok()
}

/// Convert a mutez cost to EVM gas using the current base fee.
///
/// Units: mutez * 10^12 (wei/mutez) / base_fee_per_gas (wei/evm_gas)
///        = evm_gas, with ceiling division.
pub fn mutez_to_evm_gas(cost_mutez: u64, base_fee_per_gas: U256) -> Option<u64> {
    gas_from_wei(eth_from_mutez(cost_mutez), base_fee_per_gas)
}

/// Convert wei to mutez, strict: returns `NonNullRemainder` if
/// `(wei mod 10^12) != 0`.
///
/// The reject is a safety net originating from the EVM->L1
/// withdrawal precompile and deliberately diverges from ADR
/// L2-1004 (round-down at runtime boundaries). Use
/// [`tezosx_interfaces::headers::parse_tez_to_mutez`] for
/// CRAC paths instead.
pub fn mutez_from_wei(wei: Wei) -> Result<u64, ErrorMutezFromWei> {
    // Wei is 10^18, Mutez is 10^6
    let amount: U256 = wei / U256::exp10(12);
    // Check that remainder is 0 to make sure we don't lose Wei when
    // rounding to mutez.
    let remainder: U256 = wei % U256::exp10(12);

    if !remainder.is_zero() {
        Err(ErrorMutezFromWei::NonNullRemainder)
    } else if amount >= U256::from(u64::MAX) {
        Err(ErrorMutezFromWei::AmountTooLarge)
    } else {
        Ok(amount.as_u64())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ceil_div_rounds_up_on_remainder() {
        assert_eq!(
            ceil_div(U256::from(4u64), U256::from(2u64)),
            U256::from(2u64)
        );
        assert_eq!(
            ceil_div(U256::from(5u64), U256::from(2u64)),
            U256::from(3u64)
        );
        assert_eq!(ceil_div(U256::zero(), U256::from(2u64)), U256::zero());
    }

    #[test]
    fn gas_from_wei_ceils() {
        // 3e12 wei / 2e12 (wei/gas) → 1.5, ceil → 2.
        assert_eq!(
            gas_from_wei(
                U256::exp10(12) * U256::from(3u64),
                U256::exp10(12) * U256::from(2u64)
            ),
            Some(2)
        );
        // exact: 2e12 / 1e12 → 2.
        assert_eq!(
            gas_from_wei(U256::exp10(12) * U256::from(2u64), U256::exp10(12)),
            Some(2)
        );
    }

    #[test]
    fn gas_from_wei_overflow_is_none() {
        // U256::MAX / 1 overflows u64.
        assert_eq!(gas_from_wei(U256::MAX, U256::one()), None);
    }

    #[test]
    fn gas_from_wei_zero_base_fee_is_none() {
        assert_eq!(gas_from_wei(U256::exp10(12), U256::zero()), None);
    }

    #[test]
    fn mutez_to_evm_gas_normalizes_mutez_to_wei() {
        // 1 mutez = 10^12 wei; base_fee = 1 GWei = 10^9 wei/gas.
        // ceil(10^12 / 10^9) = 1000. Guards against the mutez-as-wei
        // unit bug, which would yield 1 / 10^9 = 0.
        assert_eq!(mutez_to_evm_gas(1, U256::exp10(9)), Some(1000));
    }

    #[test]
    fn mutez_to_evm_gas_exact_division() {
        // cost_wei = 10^12, base_fee = 10^12 → exactly 1.
        assert_eq!(mutez_to_evm_gas(1, U256::exp10(12)), Some(1));
    }

    #[test]
    fn mutez_to_evm_gas_rounds_up() {
        // cost_wei = 3 * 10^12, base_fee = 2 * 10^12 → 1.5, ceil → 2.
        assert_eq!(
            mutez_to_evm_gas(3, U256::exp10(12) * U256::from(2u64)),
            Some(2)
        );
    }

    #[test]
    fn mutez_to_evm_gas_sub_base_fee_rounds_up_to_one() {
        // cost_wei = 10^12 < base_fee = 3 * 10^12 → ceil(1/3) = 1.
        assert_eq!(
            mutez_to_evm_gas(1, U256::exp10(12) * U256::from(3u64)),
            Some(1)
        );
    }

    #[test]
    fn mutez_to_evm_gas_zero_mutez_is_zero() {
        assert_eq!(mutez_to_evm_gas(0, U256::one()), Some(0));
    }

    #[test]
    fn mutez_to_evm_gas_overflow_is_none() {
        // 20_000_000 mutez * 10^12 = 2e19 > u64::MAX at base_fee 1.
        assert_eq!(mutez_to_evm_gas(20_000_000, U256::one()), None);
    }

    #[test]
    fn mutez_to_evm_gas_zero_base_fee_is_none() {
        assert_eq!(mutez_to_evm_gas(1, U256::zero()), None);
    }
}
