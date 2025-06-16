// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::B256;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::OwnedPath;

use crate::Error;

// Ref. https://www.evm.codes/?fork=cancun#40
// (opcode 0x40: BLOCKHASH)
pub const BLOCKS_STORED: u64 = 256;

fn to_block_hash_path(block_number: u64) -> Result<OwnedPath, Error> {
    let path: Vec<u8> =
        format!("/evm/world_state/indexes/blocks/{}", block_number).into();
    OwnedPath::try_from(path).map_err(|err| Error::Custom(err.to_string()))
}

/// Get block hash by block number.
pub fn get_block_hash(host: &impl Runtime, block_number: u64) -> Result<B256, Error> {
    let block_path = to_block_hash_path(block_number)?;
    let block_hash = host.store_read_all(&block_path)?;

    if block_hash.len() == 32 {
        Ok(B256::from_slice(&block_hash))
    } else {
        Err(Error::Custom("Malformed block hash".to_owned()))
    }
}
