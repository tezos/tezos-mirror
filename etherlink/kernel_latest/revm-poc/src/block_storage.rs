// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::B256;
use tezos_smart_rollup_host::{path::OwnedPath, runtime::Runtime};

// Ref. https://www.evm.codes/?fork=cancun#40
// (opcode 0x40: BLOCKHASH)
pub const BLOCKS_STORED: u64 = 256;

fn to_block_hash_path(block_number: u64) -> OwnedPath {
    let path: Vec<u8> =
        format!("/evm/world_state/indexes/blocks/{}", block_number).into();
    OwnedPath::try_from(path).unwrap()
}

/// Get block hash by block number.
pub fn get_block_hash(host: &impl Runtime, block_number: u64) -> B256 {
    let block_path = to_block_hash_path(block_number);
    let block_hash = host.store_read_all(&block_path).unwrap();

    if block_hash.len() == 32 {
        B256::from_slice(&block_hash)
    } else {
        panic!("Malformed block hash")
    }
}
