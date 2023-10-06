// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::storage::{
    block_path, index_block, init_blocks_index, read_current_block_number,
    read_storage_version, store_rlp, store_storage_version, STORAGE_VERSION,
};
use primitive_types::{H256, U256};
use tezos_ethereum::block::L2Block;
use tezos_ethereum::rlp_helpers::FromRlpBytes;
use tezos_smart_rollup_host::runtime::Runtime;

pub enum MigrationStatus {
    None,
    InProgress,
    Done,
}

// The genesis block hash 0x00..00, its parent hash is itself.
// Therefore block.0.parent_hash is 0x00..00
//           block.1.parent_hash is 0x00..00
// It's a problem for indexers, they want parent hashes to be unique.
fn replace_genesis_parent_hash<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    // Get the genesis block.
    let hash = H256::zero();
    let path = block_path(hash)?;
    let bytes = host.store_read_all(&path)?;
    let block = L2Block::from_rlp_bytes(&bytes)?;
    // Replace genesis.parent_hash.
    let block = L2Block {
        parent_hash: H256::from_slice(
            &hex::decode(
                "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
            )
            .unwrap(),
        ),
        ..block
    };
    // Write again genesis block. We go through `store_rlp` to avoid
    // an additional indexing of the block.
    store_rlp(&block, host, &path)
}

// When we migrated the blocks in the previous upgrade, we re-indexed by mistake
// all the blocks.
// Therefore:
// - /evm/blocks/indexes/0 -> 0x00..00
// - /evm/blocks/indexes/772940 -> 0x00..00
//
// One other way to understand is: |indexes| = HEAD * 2.
//
// There are two ways to fix this problem.
// 1. Remove the extra indexes and relocate the misplaced ones. Closer to
//    what we would do in production.
// 2. Recompute all the indexes because we know how block hashes work. Much
//    easier to implement because you start from scratch again.
//
// This function fixes the block indexes using the second approach.
fn fix_block_indexes<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let mut index = init_blocks_index()?;

    // Remove current indexes.
    host.store_delete(&index.path)?;

    // Repush all blocks to indexes.
    let head = read_current_block_number(host)?.as_u32();
    for number in 0..(head + 1) {
        let hash: H256 = H256(U256::from(number).into());
        index_block(host, &hash, &mut index)?;
    }

    Ok(())
}

// The workflow for migration is the following:
//
// - bump `storage::STORAGE_VERSION` by one
// - fill the scope inside the conditional in `storage_migration` with all the
//   needed migration functions
// - compile the kernel and run all the E2E migration tests to make sure all the
//   data is still available from the EVM proxy-node.
fn migration<Host: Runtime>(host: &mut Host) -> Result<MigrationStatus, Error> {
    let current_version = read_storage_version(host)?;
    if STORAGE_VERSION == current_version + 1 {
        // MIGRATION CODE - START

        replace_genesis_parent_hash(host)?;

        fix_block_indexes(host)?;

        // MIGRATION CODE - END
        store_storage_version(host, STORAGE_VERSION)?;
        return Ok(MigrationStatus::Done);
    }
    Ok(MigrationStatus::None)
}

pub fn storage_migration<Host: Runtime>(
    host: &mut Host,
) -> Result<MigrationStatus, Error> {
    let migration_result = migration(host);
    migration_result.map_err(|_| {
        // Something went wrong during the migration.
        // The fallback mechanism is triggered to retrograde to the previous kernel.

        Error::UpgradeError(Fallback)
    })
}
