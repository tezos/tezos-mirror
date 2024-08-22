// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::error::StorageError;
use crate::error::UpgradeProcessError;
use crate::storage::ENABLE_FA_BRIDGE;
use crate::storage::{
    read_chain_id, read_storage_version, store_storage_version, StorageVersion,
};
use evm_execution::NATIVE_TOKEN_TICKETER_PATH;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup::storage::path::RefPath;
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};

#[derive(Eq, PartialEq)]
pub enum MigrationStatus {
    None,
    InProgress,
    Done,
}

// /!\ the following functions are migratin helpers, do not remove them /!\

#[allow(dead_code)]
fn is_etherlink_ghostnet(host: &impl Runtime) -> Result<bool, Error> {
    match read_chain_id(host) {
        Ok(chain_id) => Ok(chain_id == 128123.into()),
        Err(Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))) => {
            Ok(false)
        }
        Err(err) => Err(err),
    }
}

#[allow(dead_code)]
fn allow_path_not_found(res: Result<(), RuntimeError>) -> Result<(), RuntimeError> {
    match res {
        Ok(()) => Ok(()),
        Err(RuntimeError::PathNotFound) => Ok(()),
        Err(err) => Err(err),
    }
}

fn migrate_to<Host: Runtime>(
    host: &mut Host,
    version: StorageVersion,
) -> anyhow::Result<MigrationStatus> {
    log!(host, Info, "Migrating to {:?}", version);
    match version {
        StorageVersion::V11 => anyhow::bail!(Error::UpgradeError(
            UpgradeProcessError::InternalUpgrade("V11 has no predecessor"),
        )),
        StorageVersion::V12 => {
            let legacy_ticketer_path = RefPath::assert_from(b"/evm/ticketer");
            if host.store_has(&legacy_ticketer_path)?.is_some() {
                host.store_move(&legacy_ticketer_path, &NATIVE_TOKEN_TICKETER_PATH)?;
            }

            Ok(MigrationStatus::Done)
        }
        StorageVersion::V13 => Ok(MigrationStatus::Done),
        StorageVersion::V14 => {
            if is_etherlink_ghostnet(host)? {
                host.store_write_all(&ENABLE_FA_BRIDGE, &[1u8])?;
                Ok(MigrationStatus::Done)
            } else {
                // Not applicable for other networks
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V15 => {
            // Starting version 15, the entrypoint `populate_delayed_inbox`
            // is available.
            Ok(MigrationStatus::Done)
        }
    }
}

// The workflow for migration is the following:
//
// - add a new variant to `storage::StorageVersion`, update `STORAGE_VERSION`
//   accordingly.
// - update `migrate_to` pattern matching  with all the needed migration functions
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
    match read_storage_version(host)?.next() {
        Some(next_version) => {
            let status = migrate_to(host, next_version)?;

            // Record the migration was applied. Even if the migration for `next_version` returns
            // `None`, we consider it done. A good use case for `None` is for instance for a
            // migration that does not apply to the current network.
            if status != MigrationStatus::InProgress {
                store_storage_version(host, next_version)?;
                // `InProgress` so that we reboot and try apply the next migration, if any.
                return Ok(MigrationStatus::InProgress);
            }

            Ok(status)
        }
        None => Ok(MigrationStatus::None),
    }
}

pub fn storage_migration<Host: Runtime>(
    host: &mut Host,
) -> Result<MigrationStatus, Error> {
    let migration_result = migration(host);
    migration_result.map_err(|_| Error::UpgradeError(UpgradeProcessError::Fallback))
}
