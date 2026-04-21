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
