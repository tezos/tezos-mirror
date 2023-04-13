// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! All that is constant for a block
//!
//! The Ethereum blocks on the rollup correspond to rollup inbox levels.
//! When then kernel "sees" an EOL from the PVM, it finalizes the current block
//! and starts creating the next one.

use super::storage::blocks::get_current_number_n_timestamp;
use host::runtime::Runtime;
use primitive_types::{H160, U256};
use tezos_ethereum::basic::{GasLimit, GasPrice, Wei};
/// All data for an Ethereum block.
///
/// This data does not change for the duration of the block. All balues are
/// updated when the block is finalized and may change for the next block.
pub struct BlockConstants {
    /// Price of one unit of gas in Wei
    pub gas_price: GasPrice,
    /// The number of the current block
    pub number: U256,
    /// Who is the beneficiary of the current block
    pub coinbase: H160,
    /// Unix date/time of the current block - when was the previous block finished
    pub timestamp: U256,
    /// Mining difficulty of the current block. This relates to PoW, and we can set
    /// the value to an arbitrary value.
    pub difficulty: U256,
    /// Gas limit for the current block.
    pub gas_limit: GasLimit,
    /// The base fee per gas for doing a transaction.
    pub base_fee_per_gas: Wei,
    /// Identifier for the chain. Normally this would identify the chain (Ethereum
    /// main net, or some other net). We can use it to identify rollup EVM kernel.
    pub chain_id: U256,
}

impl BlockConstants {
    /// Return the first block of the chain (genisis).
    /// TODO find suitable values for gas_limit et.c.
    /// To be done in <https://gitlab.com/tezos/tezos/-/milestones/114>.
    pub fn first_block() -> Self {
        Self {
            gas_price: GasPrice::one(),
            number: U256::zero(),
            coinbase: H160::zero(),
            timestamp: U256::zero(),
            difficulty: U256::zero(),
            gas_limit: GasLimit::one(),
            base_fee_per_gas: Wei::one(),
            chain_id: U256::zero(),
        }
    }

    /// Retrieve the current block constants from storage
    pub fn from_storage<Host: Runtime>(host: &Host) -> Self {
        // This is a stub for now - just return the first block
        let (number, timestamp) = get_current_number_n_timestamp(host)
            .expect("Number and timestamp expected to exist before messages processing");
        BlockConstants {
            number,
            timestamp,
            ..BlockConstants::first_block()
        }
    }
}
