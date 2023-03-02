// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::blueprint::Queue;
use crate::error::Error;
use crate::eth_gen::{L2Level, OwnedHash, Quantity, RawTransactions};
use crate::inbox;
use crate::storage;
use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

pub struct L2Block {
    // This choice of a L2 block representation is totally
    // arbitrarily based on what is an Ethereum block and is
    // subject to change.
    pub number: L2Level,
    pub hash: OwnedHash, // 32 bytes
    pub parent_hash: OwnedHash,
    pub nonce: Quantity,
    pub sha3_uncles: OwnedHash,
    pub logs_bloom: Option<OwnedHash>,
    pub transactions_root: OwnedHash,
    pub state_root: OwnedHash,
    pub receipts_root: OwnedHash,
    pub miner: OwnedHash,
    pub difficulty: Quantity,
    pub total_difficulty: Quantity,
    pub extra_data: OwnedHash,
    pub size: Quantity,
    pub gas_limit: Quantity,
    pub gas_used: Quantity,
    pub timestamp: Quantity,
    pub transactions: RawTransactions,
    pub uncles: Vec<OwnedHash>,
}

impl L2Block {
    // dead code is allowed in this implementation because the following constants
    // are not used outside the scope of L2Block
    #![allow(dead_code)]

    const DUMMY_QUANTITY: Quantity = 0;
    const DUMMY_HASH: &str = "0000000000000000000000000000000000000000";

    fn dummy_hash() -> OwnedHash {
        L2Block::DUMMY_HASH.into()
    }

    pub fn new(number: L2Level, transactions: RawTransactions) -> Self {
        L2Block {
            number,
            hash: L2Block::dummy_hash(),
            parent_hash: L2Block::dummy_hash(),
            nonce: L2Block::DUMMY_QUANTITY,
            sha3_uncles: L2Block::dummy_hash(),
            logs_bloom: None,
            transactions_root: L2Block::dummy_hash(),
            state_root: L2Block::dummy_hash(),
            receipts_root: L2Block::dummy_hash(),
            miner: L2Block::dummy_hash(),
            difficulty: L2Block::DUMMY_QUANTITY,
            total_difficulty: L2Block::DUMMY_QUANTITY,
            extra_data: L2Block::dummy_hash(),
            size: L2Block::DUMMY_QUANTITY,
            gas_limit: L2Block::DUMMY_QUANTITY,
            gas_used: L2Block::DUMMY_QUANTITY,
            timestamp: L2Block::DUMMY_QUANTITY,
            transactions,
            uncles: Vec::new(),
        }
    }
}

fn validate(block: L2Block) -> Result<L2Block, Error> {
    // TODO: https://gitlab.com/tezos/tezos/-/issues/4749
    Ok(block)
}

pub fn produce<Host: Runtime + RawRollupCore>(host: &mut Host, queue: Queue) {
    for proposal in queue.proposals.iter() {
        let current_level = storage::read_current_block_number(host);
        let next_level = match current_level {
            Ok(current_level) => current_level + 1,
            Err(_) => 0,
        };
        let raw_transactions = proposal
            .transactions
            .iter()
            .map(inbox::Transaction::to_raw_transaction)
            .collect();
        let candidate_block = L2Block::new(next_level, raw_transactions);
        if let Ok(valid_block) = validate(candidate_block) {
            storage::store_current_block(host, valid_block).unwrap_or_else(|_| {
                panic!("Error while storing the current block: stopping the daemon.")
            })
        }
    }
}
