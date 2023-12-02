// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::storage::{read_storage_version, store_storage_version, STORAGE_VERSION};
use tezos_smart_rollup_host::runtime::Runtime;

pub enum MigrationStatus {
    None,
    InProgress,
    Done,
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
