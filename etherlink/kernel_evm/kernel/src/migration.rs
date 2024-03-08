// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT
use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::indexable_storage::IndexableStorage;
use crate::storage::{
    block_path, init_blocks_index, read_rlp, read_storage_version, store_rlp,
    store_storage_version, KERNEL_GOVERNANCE, KERNEL_SECURITY_GOVERNANCE,
    SEQUENCER_GOVERNANCE, STORAGE_VERSION,
};
use primitive_types::H256;
use tezos_ethereum::block::L2Block;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};

pub enum MigrationStatus {
    None,
    InProgress,
    Done,
}

fn read_block<Host: Runtime>(
    host: &mut Host,
    blocks_index: &mut IndexableStorage,
    block_number: u64,
) -> anyhow::Result<L2Block> {
    let hash = H256::from_slice(&blocks_index.unsafe_get_value(host, block_number)?);
    let block_path = block_path(hash)?;
    let block = read_rlp(host, &block_path)?;
    Ok(block)
}

fn allow_path_not_found(res: Result<(), RuntimeError>) -> Result<(), RuntimeError> {
    match res {
        Ok(()) => Ok(()),
        Err(RuntimeError::PathNotFound) => Ok(()),
        Err(err) => Err(err),
    }
}

// The workflow for migration is the following:
//
// - bump `storage::STORAGE_VERSION` by one
// - fill the scope inside the conditional in `storage_migration` with all the
//   needed migration functions
// - compile the kernel and run all the E2E migration tests to make sure all the
//   data is still available from the EVM proxy-node.
//
// /!\
//     If the migration takes more than 999 reboots, we will lose the inbox
//     of a level. At least one reboot must be allocated to the stage one
//     to consume the inbox. Therefore, if the migration happens to take more
//     than 999 reboots, you have to rethink this. This limitation exists
//     because we consider that the inbox should not be collected during
//     a migration because it impacts the storage. We could in theory end up
//     in an inconsistent storage.
// /!\
//
fn migration<Host: Runtime>(host: &mut Host) -> anyhow::Result<MigrationStatus> {
    let current_version = read_storage_version(host)?;
    if STORAGE_VERSION == current_version + 1 {
        // MIGRATION CODE - START
        allow_path_not_found(
            host.store_delete(&RefPath::assert_from(b"/evm/blueprints/last")),
        )?;
        allow_path_not_found(
            host.store_delete(&RefPath::assert_from(b"/evm/sequencer_admin")),
        )?;
        host.store_write_all(
            &KERNEL_GOVERNANCE,
            b"KT1RPmPCBGztHpNWHPmyzo7k5YqVapYoryvg",
        )?;
        host.store_write_all(
            &KERNEL_SECURITY_GOVERNANCE,
            b"KT1PH48LrVFLvHPHnAVhmKAYGAp1Z2Ure5R4",
        )?;
        host.store_write_all(
            &SEQUENCER_GOVERNANCE,
            b"KT1ECwsLV29BjuuzHtFeNs84tarB7ryYcpRR",
        )?;

        // If it exists, we are on ghostnet.
        let mut index = init_blocks_index()?;
        if let Ok(block_813) = read_block(host, &mut index, 1232813) {
            log!(host, Info, "Block 813: {:?}", block_813);
            let block_814 = read_block(host, &mut index, 1232814)?;
            log!(host, Info, "Block 814: {:?}", block_814);
            let patched_block_814 = L2Block {
                parent_hash: block_813.hash,
                ..block_814
            };
            let path_814 = block_path(patched_block_814.hash)?;
            store_rlp(&patched_block_814, host, &path_814)?;
            log!(host, Info, "Block 814 replaced by: {:?}", patched_block_814);
        }

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
    migration_result.map_err(|_| Error::UpgradeError(Fallback))
}
