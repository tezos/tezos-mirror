// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::block_storage;
use crate::error::Error;
use crate::error::StorageError;
use crate::error::UpgradeProcessError;
use crate::storage::DELAYED_BRIDGE;
use crate::storage::ENABLE_FA_BRIDGE;
use crate::storage::{
    read_chain_id, read_storage_version, store_storage_version, StorageVersion,
};
use evm_execution::account_storage::account_path;
use evm_execution::account_storage::init_account_storage;
use evm_execution::precompiles::SYSTEM_ACCOUNT_ADDRESS;
use evm_execution::precompiles::WITHDRAWAL_ADDRESS;
use evm_execution::NATIVE_TOKEN_TICKETER_PATH;
use primitive_types::U256;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::storage::path::RefPath;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::runtime::RuntimeError;

#[derive(Eq, PartialEq)]
pub enum MigrationStatus {
    None,
    InProgress,
    Done,
}

// /!\ the following functions are migratin helpers, do not remove them /!\

#[allow(dead_code)]
const TESTNET_CHAIN_ID: u64 = 128123;

#[allow(dead_code)]
const MAINNET_CHAIN_ID: u64 = 42793;

#[allow(dead_code)]
fn is_etherlink_network(
    host: &impl Runtime,
    expected_chain_id: u64,
) -> Result<bool, Error> {
    match read_chain_id(host) {
        Ok(chain_id) => Ok(chain_id == expected_chain_id.into()),
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
            if is_etherlink_network(host, TESTNET_CHAIN_ID)? {
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
        StorageVersion::V16 => {
            // Allow path not found in case the migration is performed
            // on a context with no blocks or no transactions.
            allow_path_not_found(host.store_delete(&RefPath::assert_from(
                b"/evm/world_state/indexes/accounts",
            )))?;
            allow_path_not_found(host.store_delete(&RefPath::assert_from(
                b"/evm/world_state/indexes/transactions",
            )))?;
            // Starting version 16, the `callTracer` configuration is available
            // for tracing.
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V17 => {
            // Starting version 17 the kernel no longer needs all transactions
            // in its storage to produce the receipts and transactions root.
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V18 => {
            // Blocks were indexed twice in the storage.
            // [/evm/world_state/indexes/blocks] is the mapping of all block
            // numbers to hashes.
            // [/evm/world_state/blocks/<number>/hash] is the mapgping of the
            // last 256 blocks to hashes
            //
            // We need only the former.

            let current_number = block_storage::read_current_number(host)?;
            let to_clean = U256::min(
                current_number + 1,
                evm_execution::storage::blocks::BLOCKS_STORED.into(),
            );
            for i in 0..to_clean.as_usize() {
                let number = current_number - i;
                let path: Vec<u8> =
                    format!("/evm/world_state/blocks/{}/hash", number).into();
                let owned_path = OwnedPath::try_from(path)?;
                host.store_delete(&owned_path)?;
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V19 => {
            // We do not support EIP161 yet. If we start doing it, we
            // might clean the zero account by accident, and the account
            // must always exist as the ticket table is stored in the
            // zero address.
            let account_storage = init_account_storage()?;
            let account_path = account_path(&SYSTEM_ACCOUNT_ADDRESS)?;
            let mut account = account_storage.get_or_create(host, &account_path)?;
            account.increment_nonce(host)?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V20 => {
            let account_storage = init_account_storage()?;
            let mut withdrawal_precompiled = account_storage
                .get_or_create(host, &account_path(&WITHDRAWAL_ADDRESS)?)?;
            let balance = withdrawal_precompiled.balance(host)?;
            if !balance.is_zero() {
                withdrawal_precompiled.balance_remove(host, balance)?;
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V21 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                host.store_write_all(
                    &DELAYED_BRIDGE,
                    b"KT1Vocor3bL5ZSgsYH9ztt42LNhqFK64soR4",
                )?;
                Ok(MigrationStatus::Done)
            } else if is_etherlink_network(host, TESTNET_CHAIN_ID)? {
                host.store_write_all(
                    &DELAYED_BRIDGE,
                    b"KT1X1M4ywyz9cHvUgBLTUUdz3GTiYJhPcyPh",
                )?;
                Ok(MigrationStatus::Done)
            } else {
                // Not applicable for other networks
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V22 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                host.store_write_all(&ENABLE_FA_BRIDGE, &[1u8])?;
                Ok(MigrationStatus::Done)
            } else {
                // Not applicable for other networks
                Ok(MigrationStatus::None)
            }
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
// - upgrade the failed_migration.wasm kernel, see tests/ressources/README.md
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
