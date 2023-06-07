// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Generating arbitrary data for testing

use primitive_types::{H256, U256};
use proptest::prelude::any;
use proptest::prelude::BoxedStrategy;
use proptest::strategy::Strategy;
use tezos_ethereum::address::EthereumAddress;

pub fn arb_u256() -> BoxedStrategy<U256> {
    any::<[u8; 32]>()
        .prop_map(|x| U256::from_big_endian(&x))
        .boxed()
}

pub fn arb_h256() -> BoxedStrategy<H256> {
    any::<[u8; 32]>().prop_map(H256::from).boxed()
}

/// Generate an arbitrary Ethereum address for testing
pub fn arb_eth_address() -> BoxedStrategy<EthereumAddress> {
    any::<[u8; 20]>().prop_map(|x| x.into()).boxed()
}
