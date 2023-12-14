// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::blueprint_storage;
use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::storage::{
    self, read_storage_version, store_storage_version, STORAGE_VERSION,
};
use tezos_smart_rollup_host::runtime::Runtime;

pub enum MigrationStatus {
    None,
    InProgress,
    Done,
}

// In proxy mode, the value of `EVM_LAST_BLUEPRINT` indicates the number of
// the last stored blueprint. This is used to store the next blueprint when
// created from the inbox. If missing, the implementation of
// `blueprint_storage` assumes the next blueprint should be number 0.
// Therefore, migration needs to set this value to the current block's
// number.
fn migrate_blueprint_storage<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let head_number = storage::read_current_block_number(host)?;
    blueprint_storage::store_last_blueprint_number(host, head_number)
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
        migrate_blueprint_storage(host)?;
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
