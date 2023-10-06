// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::storage::{
    block_path, read_storage_version, store_rlp, store_storage_version, STORAGE_VERSION,
};
use primitive_types::H256;
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
