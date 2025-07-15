// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::block_storage;
use crate::blueprint_storage::{
    blueprint_path, clear_all_blueprints, store_current_block_header,
};
use crate::error::Error;
use crate::error::StorageError;
use crate::error::UpgradeProcessError;
use crate::storage::{
    read_chain_id, read_storage_version, store_backlog, store_dal_slots,
    store_storage_version, tweak_dal_activation, StorageVersion, DELAYED_BRIDGE,
    ENABLE_FA_BRIDGE, KERNEL_GOVERNANCE, KERNEL_SECURITY_GOVERNANCE,
    SEQUENCER_GOVERNANCE,
};
use evm_execution::account_storage::account_path;
use evm_execution::account_storage::init_account_storage;
use evm_execution::configuration::EVMVersion;
use evm_execution::precompiles::SYSTEM_ACCOUNT_ADDRESS;
use evm_execution::precompiles::WITHDRAWAL_ADDRESS;
use evm_execution::{
    store_evm_version, ENABLE_FAST_FA_WITHDRAWAL, ENABLE_FAST_WITHDRAWAL,
    NATIVE_TOKEN_TICKETER_PATH,
};
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
pub fn allow_path_not_found(res: Result<(), RuntimeError>) -> Result<(), RuntimeError> {
    match res {
        Ok(()) => Ok(()),
        Err(RuntimeError::PathNotFound) => Ok(()),
        Err(err) => Err(err),
    }
}

const TMP_NEXT_BLUEPRINT_PATH: RefPath =
    RefPath::assert_from(b"/__tmp_next_blueprint_path");

mod legacy {
    // This module contains copies of old implementations of some
    // functions. The legacy semantics of these functions is needed in
    // some migration step to access the storage using the fields
    // which were present at the time.

    use super::*;
    use tezos_storage::error::Error as GenStorageError;

    pub fn read_next_blueprint_number<Host: Runtime>(
        host: &Host,
    ) -> anyhow::Result<U256> {
        match block_storage::read_current_number(host) {
            Err(err) => match err.downcast_ref() {
                Some(GenStorageError::Runtime(RuntimeError::PathNotFound)) => {
                    Ok(U256::zero())
                }
                _ => Err(err),
            },
            Ok(block_number) => Ok(block_number.saturating_add(U256::one())),
        }
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
        StorageVersion::V23 => {
            // Clear all the blueprints, we accumulated a lot of old
            // blueprints without cleaning them.
            //
            // As we remove everything that means the sequencer will
            // have to republish some.
            //
            // However we need to keep the next blueprint as it
            // trigerred the upgrade.

            let next_blueprint_number = legacy::read_next_blueprint_number(host)?;
            let blueprint_path = blueprint_path(next_blueprint_number)?;
            allow_path_not_found(
                host.store_move(&blueprint_path, &TMP_NEXT_BLUEPRINT_PATH),
            )?;
            clear_all_blueprints(host)?;
            allow_path_not_found(
                host.store_move(&TMP_NEXT_BLUEPRINT_PATH, &blueprint_path),
            )?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V24 => {
            const EVM_BASE_FEE_PER_GAS: RefPath =
                RefPath::assert_from(b"/evm/world_state/fees/base_fee_per_gas");
            host.store_delete(&EVM_BASE_FEE_PER_GAS)?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V25 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                const REGULAR_GOVERNANCE_KT: &[u8] =
                    b"KT1FPG4NApqTJjwvmhWvqA14m5PJxu9qgpBK";
                const SECURITY_GOVERNANCE_KT: &[u8] =
                    b"KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2";
                const SEQUENCER_GOVERNANCE_KT: &[u8] =
                    b"KT1UvCsnXpLAssgeJmrbQ6qr3eFkYXxsTG9U";

                host.store_write_all(&KERNEL_GOVERNANCE, REGULAR_GOVERNANCE_KT)?;
                host.store_write_all(
                    &KERNEL_SECURITY_GOVERNANCE,
                    SECURITY_GOVERNANCE_KT,
                )?;
                host.store_write_all(&SEQUENCER_GOVERNANCE, SEQUENCER_GOVERNANCE_KT)?;

                Ok(MigrationStatus::Done)
            } else {
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V26 => {
            host.store_write_all(&ENABLE_FAST_WITHDRAWAL, &[1_u8])?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V27 => {
            // Initialize the next_blueprint_info field
            match block_storage::read_current(host, &crate::chains::ChainFamily::Evm) {
                Ok(block) => {
                    store_current_block_header(host, &block.into())?;
                    Ok(MigrationStatus::Done)
                }
                Err(err) => match err.downcast_ref() {
                    Some(tezos_storage::error::Error::Runtime(
                        RuntimeError::PathNotFound,
                    )) => Ok(MigrationStatus::Done),
                    _ => Err(err),
                },
            }
        }
        StorageVersion::V28 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)?
                || is_etherlink_network(host, TESTNET_CHAIN_ID)?
            {
                store_evm_version(host, &EVMVersion::Cancun)?;
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V29 => {
            /// Path used to store to the number of timestamps read.
            const EVM_INFO_PER_LEVEL_STATS_NUMBERS: RefPath =
                RefPath::assert_from(b"/evm/info_per_level/stats/numbers");
            /// Path used to store the sum of distance between blocks.
            const EVM_INFO_PER_LEVEL_STATS_TOTAL: RefPath =
                RefPath::assert_from(b"/evm/info_per_level/stats/total");
            host.store_delete(&EVM_INFO_PER_LEVEL_STATS_NUMBERS)?;
            host.store_delete(&EVM_INFO_PER_LEVEL_STATS_TOTAL)?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V30 => {
            host.store_write_all(&ENABLE_FAST_FA_WITHDRAWAL, &[1_u8])?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V31 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)?
                || is_etherlink_network(host, TESTNET_CHAIN_ID)?
            {
                tweak_dal_activation(host, true)?;
                // We allow 8 slots in order to have around twice the size of the inbox.
                // NB:
                // * One slot is 127kb.
                // * The size of the inbox is 512kb.
                store_dal_slots(host, &[0, 1, 2, 3, 4, 5, 6, 7])?
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V32 => {
            // We clear the gas price backlog. This is because the backlog’s unit has changed from
            // ticks to gas, and there is easily a factor 1,000 between the two. Clearing backlog
            // will prevent an undesired gas price spike at migration time.
            store_backlog(host, 0)?;

            Ok(MigrationStatus::Done)
        }
        StorageVersion::V33 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                const REGULAR_GOVERNANCE_KT: &[u8] =
                    b"KT1XdSAYGXrUDE1U5GNqUKKscLWrMhzyjNeh";
                const SECURITY_GOVERNANCE_KT: &[u8] =
                    b"KT1D1fRgZVdjTj5sUZKcSTPPnuR7LRxVYnDL";
                const SEQUENCER_GOVERNANCE_KT: &[u8] =
                    b"KT1NnH9DCAoY1pfPNvb9cw9XPKQnHAFYFHXa";

                host.store_write_all(&KERNEL_GOVERNANCE, REGULAR_GOVERNANCE_KT)?;
                host.store_write_all(
                    &KERNEL_SECURITY_GOVERNANCE,
                    SECURITY_GOVERNANCE_KT,
                )?;
                host.store_write_all(&SEQUENCER_GOVERNANCE, SEQUENCER_GOVERNANCE_KT)?;

                Ok(MigrationStatus::Done)
            } else {
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V34 => {
            // Dummy migration allowing to version-gate the gas limit
            // validation in the EVM node
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V35 => {
            // Dummy migration allowing the node to decide what gas target to use when trying to
            // predict the gas price
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V36 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                const REGULAR_GOVERNANCE_KT: &[u8] =
                    b"KT1VZVNCNnhUp7s15d9RsdycP7C1iwYhAQ8r";
                const SECURITY_GOVERNANCE_KT: &[u8] =
                    b"KT1DxndcFitAbxLdJCN3C1pPivqbC3RJxD1R";
                const SEQUENCER_GOVERNANCE_KT: &[u8] =
                    b"KT1WckZ2uiLfHCfQyNp1mtqeRcC1X6Jg2Qzf";

                host.store_write_all(&KERNEL_GOVERNANCE, REGULAR_GOVERNANCE_KT)?;
                host.store_write_all(
                    &KERNEL_SECURITY_GOVERNANCE,
                    SECURITY_GOVERNANCE_KT,
                )?;
                host.store_write_all(&SEQUENCER_GOVERNANCE, SEQUENCER_GOVERNANCE_KT)?;

                Ok(MigrationStatus::Done)
            } else {
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
