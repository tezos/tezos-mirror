// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 PK Lab <contact@pklab.io>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;

/// Convert Michelson gas to mutez using the current base fee.
///
/// Units: base_fee_per_gas (wei/evm_gas) * multiplier (evm_gas/michelson_gas)
///        * gas (michelson_gas) = wei, then / 10^12 (wei/mutez) = mutez.
///
/// Note that this wraps on `u64` overflow (`low_u64`), unlike its typed
/// `tezosx_types` counterpart, which errors on the mutez domain bound.
// TODO: https://linear.app/tezos/issue/L2-2021
//   Port the last caller (`tezos_execution`) to the typed
//   `tezosx_types::michelson_gas_to_mutez`, then delete this helper.
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
}
