// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Pricing conversions between gas and mutez at the current base fee:
//! typed equivalents of the conversions historically implemented in
//! `ethereum::wei`.

use primitive_types::U256;

use crate::{EvmGas, Mutez, Wei, WeiToMutezError};

/// Convert Michelson gas to mutez using the current base fee.
///
/// Units: `base_fee_per_gas` (wei/evm_gas) * `multiplier`
/// (evm_gas/michelson_gas) * `gas` (michelson_gas) = wei, then floored to
/// mutez (the CRAC policy — see [`Wei::to_mutez_floor`]).
///
/// A product that overflows `U256` is, a fortiori, past the mutez domain
/// and reported as [`WeiToMutezError::AmountTooLarge`] rather than
/// panicking.
pub fn michelson_gas_to_mutez(
    base_fee_per_gas: U256,
    multiplier: u64,
    gas: u64,
) -> Result<Mutez, WeiToMutezError> {
    let wei = base_fee_per_gas
        .checked_mul(U256::from(multiplier))
        .and_then(|wei_per_michelson_gas| {
            wei_per_michelson_gas.checked_mul(U256::from(gas))
        })
        .ok_or(WeiToMutezError::AmountTooLarge)?;
    Wei::from_u256(wei).to_mutez_floor()
}

/// Ceiling division of `a` by `b` on `U256`.
///
/// The caller must ensure `b != 0`.
fn ceil_div(a: U256, b: U256) -> U256 {
    match a.div_mod(b) {
        (quotient, remainder) if remainder.is_zero() => quotient,
        (quotient, _) => quotient.saturating_add(U256::one()),
    }
}

/// Convert a mutez cost to EVM gas at the given base fee, with ceiling
/// division.
///
/// Units: `cost` (mutez) * 10^12 (wei/mutez) / `base_fee_per_gas`
/// (wei/evm_gas) = evm_gas, with ceiling division.
///
/// Returns `None` when `base_fee_per_gas` is zero or the gas amount
/// overflows `u64`.
pub fn mutez_to_evm_gas(cost: Mutez, base_fee_per_gas: U256) -> Option<EvmGas> {
    if base_fee_per_gas.is_zero() {
        return None;
    }
    u64::try_from(ceil_div(cost.to_wei().as_u256(), base_fee_per_gas))
        .ok()
        .map(EvmGas::new)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn michelson_gas_to_mutez_floors() {
        // base_fee 1 wei/gas * multiplier 1 * gas 1 = 1 wei < 1 mutez.
        let mutez = michelson_gas_to_mutez(U256::one(), 1, 1).unwrap();
        assert_eq!(mutez, Mutez::ZERO);
    }

    #[test]
    fn michelson_gas_to_mutez_exact() {
        let mutez = michelson_gas_to_mutez(U256::exp10(12), 1, 3).unwrap();
        assert_eq!(mutez, Mutez::try_from(3u64).unwrap());
    }

    #[test]
    fn michelson_gas_to_mutez_rejects_past_domain() {
        // A gas-limit-derived wei amount past the mutez domain must error
        // rather than silently wrap (the legacy helper's `low_u64()`) or
        // saturate.
        let huge_base_fee = U256::from(10u64).pow(U256::from(40u64));
        let result = michelson_gas_to_mutez(huge_base_fee, 1, 1);
        assert_eq!(result, Err(WeiToMutezError::AmountTooLarge));
    }

    #[test]
    fn michelson_gas_to_mutez_rejects_u256_overflow() {
        // A product that does not even fit in U256 must surface as the
        // same domain error rather than panic on overflow.
        assert_eq!(
            michelson_gas_to_mutez(U256::MAX, 2, 1),
            Err(WeiToMutezError::AmountTooLarge)
        );
        assert_eq!(
            michelson_gas_to_mutez(U256::MAX, 1, 2),
            Err(WeiToMutezError::AmountTooLarge)
        );
    }

    #[test]
    fn mutez_to_evm_gas_normalizes_mutez_to_wei() {
        // 1 mutez = 10^12 wei; base_fee = 1 GWei = 10^9 wei/gas.
        // ceil(10^12 / 10^9) = 1000. Guards against the mutez-as-wei unit
        // bug, which would yield 1 / 10^9 = 0.
        let cost = Mutez::try_from(1u64).unwrap();
        assert_eq!(
            mutez_to_evm_gas(cost, U256::exp10(9)),
            Some(EvmGas::new(1000))
        );
    }

    #[test]
    fn mutez_to_evm_gas_exact_division() {
        let cost = Mutez::try_from(1u64).unwrap();
        assert_eq!(
            mutez_to_evm_gas(cost, U256::exp10(12)),
            Some(EvmGas::new(1))
        );
    }

    #[test]
    fn mutez_to_evm_gas_rounds_up() {
        let cost = Mutez::try_from(3u64).unwrap();
        assert_eq!(
            mutez_to_evm_gas(cost, U256::exp10(12) * U256::from(2u64)),
            Some(EvmGas::new(2))
        );
    }

    #[test]
    fn mutez_to_evm_gas_sub_base_fee_rounds_up_to_one() {
        let cost = Mutez::try_from(1u64).unwrap();
        assert_eq!(
            mutez_to_evm_gas(cost, U256::exp10(12) * U256::from(3u64)),
            Some(EvmGas::new(1))
        );
    }

    #[test]
    fn mutez_to_evm_gas_zero_mutez_is_zero() {
        assert_eq!(
            mutez_to_evm_gas(Mutez::ZERO, U256::one()),
            Some(EvmGas::new(0))
        );
    }

    #[test]
    fn mutez_to_evm_gas_zero_base_fee_is_none() {
        let cost = Mutez::try_from(1u64).unwrap();
        assert_eq!(mutez_to_evm_gas(cost, U256::zero()), None);
    }

    #[test]
    fn mutez_to_evm_gas_overflow_is_none() {
        // Mutez::MAX wei / 1 overflows u64.
        assert_eq!(mutez_to_evm_gas(Mutez::MAX, U256::one()), None);
    }
}
