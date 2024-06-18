// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 PK Lab <contact@pklab.io>
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

pub fn mutez_from_wei(wei: Wei) -> Result<u64, primitive_types::Error> {
    // Wei is 10^18, Mutez is 10^6
    let amount: U256 = U256::checked_div(wei, U256::exp10(12)).unwrap();

    if amount < U256::from(u64::max_value()) {
        Ok(amount.as_u64())
    } else {
        Err(primitive_types::Error::Overflow)
    }
}
