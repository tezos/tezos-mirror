// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Pricing conversions between gas and mutez at the current base fee:
//! typed equivalents of the conversions historically implemented in
//! `ethereum::wei`.

use primitive_types::U256;

use crate::{Mutez, Wei, WeiToMutezError};

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
}
