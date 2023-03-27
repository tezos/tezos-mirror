// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Generating arbitrary data for testing

use proptest::prelude::*;

use super::address::*;
use super::basic::*;

impl U256 {
    /// Create arbitrary 256 bit unsigned integer for testing
    pub fn arb() -> BoxedStrategy<U256> {
        any::<[u8; 32]>()
            .prop_map(|x| Self::from_slice_be(&x))
            .boxed()
    }
}

impl H256 {
    /// Create arbitrary 256 bit hash for testing
    pub fn arb() -> BoxedStrategy<H256> {
        any::<[u8; 32]>().prop_map(H256::from).boxed()
    }
}

impl GasPrice {
    /// Generate an arbitrary gas price for testing
    pub fn arb() -> BoxedStrategy<GasPrice> {
        U256::arb().prop_map(Self::new).boxed()
    }
}

impl GasLimit {
    /// Generate an arbitrary gas limit for testing
    pub fn arb() -> BoxedStrategy<GasLimit> {
        U256::arb().prop_map(Self::new).boxed()
    }
}

impl Wei {
    /// Generate arbitrary value in Wei for testing
    pub fn arb() -> BoxedStrategy<Wei> {
        U256::arb().prop_map(Self::new).boxed()
    }
}

impl EthereumAddress {
    /// Generate an arbitrary Ethereum address for testing
    pub fn arb() -> BoxedStrategy<EthereumAddress> {
        any::<[u8; 20]>().prop_map(|x| x.into()).boxed()
    }
}
