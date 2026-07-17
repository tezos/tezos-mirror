// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::block_storage::read_current_number;
use crate::chains::ETHERLINK_SAFE_STORAGE_ROOT_PATH;
use crate::error::Error;
use crate::error::StorageError;
use crate::error::UpgradeProcessError;
use crate::storage::{
    read_evm_chain_id, read_storage_version, store_storage_version, StorageVersion,
};
use revm_etherlink::storage::block::BLOCKS_STORED;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::IsEvmNode;
use tezos_smart_rollup::storage::path::RefPath;
use tezos_smart_rollup_host::path::{concat, OwnedPath};
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::KeySpaceLoader;

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
    host: &impl StorageV1,
    expected_chain_id: u64,
) -> Result<bool, Error> {
    match read_evm_chain_id(host) {
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

mod legacy {
    use super::*;

    pub const TMP_NEXT_BLUEPRINT_PATH: RefPath =
        RefPath::assert_from(b"/__tmp_next_blueprint_path");
}

fn migrate_to<Host>(
    host: &mut Host,
    version: StorageVersion,
) -> anyhow::Result<MigrationStatus>
where
    Host: StorageV1 + IsEvmNode + KeySpaceLoader,
{
    log!(Info, "Migrating to {:?}", version);
    match version {
        StorageVersion::V47 => Ok(MigrationStatus::Done),
        StorageVersion::V48 => {
            // Clean remaining leftover block indexes from V41.
            if let Ok(current_number) =
                read_current_number(host, &ETHERLINK_SAFE_STORAGE_ROOT_PATH)
            {
                let last_to_keep = current_number.as_u64().saturating_sub(BLOCKS_STORED);
                let paths_indexed_blocks = concat(
                    &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
                    &RefPath::assert_from(b"/indexes/blocks"),
                )?;
                // We keep the last BLOCKS_STORED block hashes.
                let mut block_hashes: Vec<(OwnedPath, Vec<u8>)> =
                    Vec::with_capacity(BLOCKS_STORED.try_into().unwrap_or(256));
                for i in last_to_keep..=current_number.as_u64() {
                    let path: Vec<u8> = format!("/{i}").into();
                    let owned_path = RefPath::assert_from(&path);
                    let read_path = concat(&paths_indexed_blocks, &owned_path)?;
                    match host.store_read_all(&read_path) {
                        Ok(hash) => {
                            block_hashes.push((read_path, hash));
                            Ok(())
                        }
                        Err(RuntimeError::PathNotFound) => Ok(()),
                        Err(err) => Err(err),
                    }?;
                }

                // We dump all the remaining block hashes.
                allow_path_not_found(host.store_delete(&paths_indexed_blocks))?;

                // We rewrite the last BLOCKS_STORED block hashes.
                for (path, hash) in block_hashes {
                    host.store_write_all(&path, &hash)?;
                }
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V49 => {
            // Starting version 49, blueprints from the sequencer can
            // have a version field
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V50 => {
            // Starting version 50, the kernel produce blocks for Michelson Runtime
            // when TezosX feature is activated.

            Ok(MigrationStatus::Done)
        }
        StorageVersion::V51 => {
            // Starting version 51, the sequencer upgrade is stored in the world state, and the sequencer key
            // is also stored in the world state. We move them from their legacy location to the new one.
            let legacy_sequencer_upgrade =
                RefPath::assert_from(b"/evm/sequencer_upgrade");
            let legacy_sequencer_key = RefPath::assert_from(b"/evm/sequencer");

            // Both paths are optional: the sequencer upgrade only exists when a
            // governance upgrade is pending, and the sequencer key is absent in
            // proxy mode or after a governance removal.
            allow_path_not_found(host.store_move(
                &legacy_sequencer_upgrade,
                &RefPath::assert_from(b"/evm/world_state/sequencer_upgrade"),
            ))?;
            allow_path_not_found(host.store_move(
                &legacy_sequencer_key,
                &RefPath::assert_from(b"/evm/world_state/sequencer"),
            ))?;

            Ok(MigrationStatus::Done)
        }
        StorageVersion::V52 => {
            // Phase 1 of the durable storage reorganization: move IPC
            // paths from root-level locations to /base/ prefix.
            // Note: tezosx paths (__simulation, tezosx_entrypoints) are
            // not in production and do not need migration.
            allow_path_not_found(host.store_move(
                &RefPath::assert_from(b"/__evm_node"),
                &RefPath::assert_from(b"/base/__evm_node"),
            ))?;
            allow_path_not_found(host.store_move(
                &RefPath::assert_from(b"/__delayed_input"),
                &RefPath::assert_from(b"/base/__delayed_input"),
            ))?;
            allow_path_not_found(host.store_move(
                &RefPath::assert_from(b"/__backup_kernel"),
                &RefPath::assert_from(b"/base/__backup_kernel"),
            ))?;
            allow_path_not_found(host.store_move(
                &RefPath::assert_from(b"/__tmp"),
                &RefPath::assert_from(b"/base/__tmp"),
            ))?;

            // Clean up dead migration artifact from V23.
            allow_path_not_found(host.store_delete(&legacy::TMP_NEXT_BLUEPRINT_PATH))?;

            Ok(MigrationStatus::Done)
        }
        StorageVersion::V53 => {
            // Starting version 53, the EVM node uses the TezosX
            // envelope format (List [runtime_id, inner_transaction]).
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V54 => {
            // Phase 2 of the durable storage reorganization: move
            // governance / sequencing / config paths from /evm/ to
            // /base/.
            //
            // Move paths from /evm/ to /base/. /base/blueprints is kept as
            // a pure blueprint subtree (no config scalars nested inside),
            // so that a future wholesale delete of /base/blueprints (e.g.
            // Irmin slim-commit) can safely discard it.
            let moves: &[(&[u8], &[u8])] = &[
                (b"/evm/blueprints", b"/base/blueprints"),
                (
                    b"/evm/max_blueprint_lookahead_in_seconds",
                    b"/base/max_blueprint_lookahead_in_seconds",
                ),
                (
                    b"/evm/max_delayed_inbox_blueprint_length",
                    b"/base/max_delayed_inbox_blueprint_length",
                ),
                (b"/evm/kernel_version", b"/base/kernel_version"),
                (b"/evm/kernel_root_hash", b"/base/kernel_root_hash"),
                (b"/evm/kernel_upgrade", b"/base/kernel_upgrade"),
                (b"/evm/logging_verbosity", b"/base/logging_verbosity"),
                (b"/evm/l1_level", b"/base/l1_level"),
                (b"/evm/messages", b"/base/messages"),
                (b"/evm/admin", b"/base/admin"),
                (b"/evm/kernel_governance", b"/base/kernel_governance"),
                (
                    b"/evm/kernel_security_governance",
                    b"/base/kernel_security_governance",
                ),
                (b"/evm/delayed_bridge", b"/base/delayed_bridge"),
                (b"/evm/remove_whitelist", b"/base/remove_whitelist"),
                (
                    b"/evm/maximum_allowed_ticks",
                    b"/base/maximum_allowed_ticks",
                ),
                (b"/evm/dal_slots", b"/base/dal_slots"),
                (
                    b"/evm/dal_publishers_whitelist",
                    b"/base/dal_publishers_whitelist",
                ),
                (
                    b"/evm/delayed_inbox_timeout",
                    b"/base/delayed_inbox_timeout",
                ),
                (
                    b"/evm/delayed_inbox_min_levels",
                    b"/base/delayed_inbox_min_levels",
                ),
                (b"/evm/current_block_header", b"/base/current_block_header"),
                // Subtrees
                (b"/evm/delayed-inbox", b"/base/delayed-inbox"),
                (b"/evm/info_per_level", b"/base/info_per_level"),
                (b"/evm/chain_configurations", b"/base/chain_configurations"),
                // Renamed: events -> rollup_events
                (b"/evm/events", b"/base/rollup_events"),
                (b"/evm/keep_events", b"/base/keep_rollup_events"),
            ];
            for (old, new) in moves {
                allow_path_not_found(
                    host.store_move(
                        &RefPath::assert_from(old),
                        &RefPath::assert_from(new),
                    ),
                )?;
            }

            Ok(MigrationStatus::Done)
        }
        StorageVersion::V55 => {
            // Phase 3 of the durable storage reorganization: consolidate
            // all feature flags under /base/feature_flags/.
            //
            // - Move the 5 flags at /evm/feature_flags/* (outside safe
            //   storage) by moving the whole subtree.
            // - Drop the 3 flags at /evm/world_state/feature_flags/
            //   (enable_revm, enable_fast_withdrawal,
            //   enable_fast_fa_withdrawal): none is read by
            //   kernel_latest. revm and the fast-withdrawal code paths
            //   became unconditional in pre-Farfadet kernels, so there
            //   is no live data worth migrating.

            // Move the /evm/feature_flags subtree wholesale.
            allow_path_not_found(host.store_move(
                &RefPath::assert_from(b"/evm/feature_flags"),
                &RefPath::assert_from(b"/base/feature_flags"),
            ))?;

            // Drop dead flags rather than migrating them.
            let legacy_drops: &[&[u8]] = &[
                b"/evm/world_state/feature_flags/enable_revm",
                b"/evm/world_state/feature_flags/enable_fast_withdrawal",
                b"/evm/world_state/feature_flags/enable_fast_fa_withdrawal",
            ];
            for path in legacy_drops {
                allow_path_not_found(host.store_delete(&RefPath::assert_from(path)))?;
            }

            Ok(MigrationStatus::Done)
        }
        StorageVersion::V56 => {
            // L2-1296: clear the persisted balance of TEZOSX_CALLER_ADDRESS
            // (0x7e20580000000000000000000000000000000001) on networks that
            // run the Michelson runtime.
            //
            // Earlier kernels funded the internal TezosX caller used by
            // `ensure_alias` by writing `U256::MAX` to durable storage,
            // because they assumed REVM's pre-flight balance check would
            // require a positive balance. The surrounding `run_transaction`
            // is `TransactionOrigin::CrossRuntime` so its EVM journal is
            // never committed: only the manual storage write persisted, and
            // the address ended up showing balance ≈ 2^256 - 1 on
            // Blockscout. The funding has been removed (gas_price = 0 and
            // value = 0 mean no balance is required); this migration
            // discards the residue from networks that already imprinted it.
            //
            // We restrict the cleanup to TezosX networks because
            // TEZOSX_CALLER_ADDRESS is only ever written to by
            // `init_tezosx_alias`, which is unreachable when
            // `enable_tezos_runtime` is unset.
            let tezos_runtime_enabled = {
                let base = crate::storage::load_base_keyspace(host)?;
                crate::storage::enable_tezos_runtime(&base)
            };
            if tezos_runtime_enabled {
                use revm_etherlink::precompiles::constants::TEZOSX_CALLER_ADDRESS;
                use revm_etherlink::storage::world_state_handler::StorageAccount;

                // The internal caller has no nonce / code / storage to
                // preserve — only the manually-written `info` (and a
                // possible legacy `/balance` path). Drop the whole subtree
                // so subsequent reads fall back to `AccountInfo::default()`
                // (balance = 0, nonce = 0, empty code).
                let mut account = StorageAccount::from_address(&TEZOSX_CALLER_ADDRESS)
                    .map_err(|e| {
                        anyhow::anyhow!(
                            "TEZOSX_CALLER_ADDRESS account_path failed: {e:?}"
                        )
                    })?;
                account.delete_info(host).map_err(|e| {
                    anyhow::anyhow!("delete_info on TEZOSX_CALLER_ADDRESS failed: {e:?}")
                })?;
                // Legacy per-field layout used by very old kernels.
                let legacy_balance_path = OwnedPath::try_from(format!(
                    "/evm/world_state/eth_accounts/{TEZOSX_CALLER_ADDRESS:x}/balance"
                ))?;
                allow_path_not_found(host.store_delete(&legacy_balance_path))?;
                Ok(MigrationStatus::Done)
            } else {
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V57 => {
            // Phase 5.5 of the durable storage reorganization: move the
            // two Michelson-runtime activation slots that slipped past
            // the Phase 1–6 keyspace reorg under /tez/world_state/.
            //
            // TezosX-only paths: target_sunrise_level is written by the
            // installer when scheduling a Michelson activation, and
            // sunrise_level is written by the kernel at the sunrise
            // block. Mainnet has neither path. Gate on
            // [enable_tezos_runtime] so we skip cleanly there.
            let tezos_runtime_enabled = {
                let base = crate::storage::load_base_keyspace(host)?;
                crate::storage::enable_tezos_runtime(&base)
            };
            if tezos_runtime_enabled {
                let moves: &[(&[u8], &[u8])] = &[
                    (
                        b"/evm/michelson_runtime/target_sunrise_level",
                        b"/tez/world_state/michelson_runtime/target_sunrise_level",
                    ),
                    (
                        b"/evm/michelson_runtime/sunrise_level",
                        b"/tez/world_state/michelson_runtime/sunrise_level",
                    ),
                ];
                for (old, new) in moves {
                    allow_path_not_found(host.store_move(
                        &RefPath::assert_from(old),
                        &RefPath::assert_from(new),
                    ))?;
                }
                // Drop the now-empty /evm/michelson_runtime parent so
                // /evm/ doesn't keep a stale subtree.
                allow_path_not_found(
                    host.store_delete(&RefPath::assert_from(b"/evm/michelson_runtime")),
                )?;
                Ok(MigrationStatus::Done)
            } else {
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V58 => {
            // Phase 6 of the durable storage reorganization: move the
            // remaining EVM config scalars under /evm/world_state/, and
            // reorganize the block-index path so /evm/world_state/indexes/
            // disappears.
            //
            // /evm/sequencer_key_change is already at /evm/world_state/.
            // /evm/simulation_result and /evm/trace are not migrated here —
            // they remain at their legacy paths.
            let moves: &[(&[u8], &[u8])] = &[
                (b"/evm/chain_id", b"/evm/world_state/chain_id"),
                (b"/evm/evm_version", b"/evm/world_state/evm_version"),
                (
                    b"/evm/sequencer_governance",
                    b"/evm/world_state/sequencer_governance",
                ),
                (
                    b"/evm/sequencer_pool_address",
                    b"/evm/world_state/sequencer_pool_address",
                ),
                (
                    b"/evm/maximum_gas_per_transaction",
                    b"/evm/world_state/maximum_gas_per_transaction",
                ),
                // Internal reorg: move the contents of indexes/blocks/
                // under blocks/indexes/. We move /evm/world_state/indexes/blocks
                // (not /evm/world_state/indexes) to avoid an extra blocks/
                // nesting at the destination.
                (
                    b"/evm/world_state/indexes/blocks",
                    b"/evm/world_state/blocks/indexes",
                ),
            ];
            for (old, new) in moves {
                allow_path_not_found(
                    host.store_move(
                        &RefPath::assert_from(old),
                        &RefPath::assert_from(new),
                    ),
                )?;
            }
            // Drop the now-empty /evm/world_state/indexes parent.
            allow_path_not_found(
                host.store_delete(&RefPath::assert_from(b"/evm/world_state/indexes")),
            )?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V59 => {
            // Phase 7: isolate EVM accounts into /evm/eth_accounts/.
            // Order is load-bearing, do not alphabetise: the eth_accounts
            // move overwrites the destination subtree, so it must run
            // before eth_codes is placed at /evm/eth_accounts/eth_codes.
            let moves: &[(&[u8], &[u8])] = &[
                (b"/evm/world_state/eth_accounts", b"/evm/eth_accounts"),
                (
                    b"/evm/world_state/eth_codes",
                    b"/evm/eth_accounts/eth_codes",
                ),
            ];
            for (old, new) in moves {
                allow_path_not_found(
                    host.store_move(
                        &RefPath::assert_from(old),
                        &RefPath::assert_from(new),
                    ),
                )?;
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V60 => {
            // Phase 7.5: simulation & trace IPC moved under /base/.
            // No store_move needed — every affected path
            // (`/evm/simulation_result`, `/evm/simulation_http_traces`,
            // `/evm/trace/input`, `/evm/world_state/__http_trace/traces`,
            // `/tez/world_state/simulation_result`) is ephemeral IPC,
            // populated only inside `simulate_and_read*` host calls and
            // never persisted across rollup blocks. The version bump is
            // sufficient.
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V61 => {
            // L2-1526: seed the shared Michelson alias implementation at
            // /tez/tez_accounts/tezosx/__system__/alias_implementation for
            // networks that already activated the Michelson runtime (the
            // activation-time seeding only fires on fresh activations). The
            // slot receives the forwarder code every alias already runs, so
            // this is a semantic no-op; it just gives existing aliases a single
            // shared implementation to resolve to. Idempotent.
            //
            // The path and code are frozen here as literals rather than read
            // from the live constants: a migration can run long after it was
            // written, so it must keep writing these exact bytes even if the
            // live `ALIAS_IMPLEMENTATION_PATH` / `forwarder_code()` change later
            // (migration discipline).
            const ALIAS_IMPLEMENTATION_PATH: RefPath = RefPath::assert_from(
                b"/tez/tez_accounts/tezosx/__system__/alias_implementation",
            );
            const FORWARDER_CODE_HEX_V61: &str = "02000000740500036c0501036805020200000065031703210743036e01000000244b5431386f444a4a4b584d4b68664531625375415047703932705963775644697173507705550368072f02000000120743036801000000076761746577617903270200000000031505700002034d053d036d034c031b0342";
            let tezos_runtime_enabled = {
                let base = crate::storage::load_base_keyspace(host)?;
                crate::storage::enable_tezos_runtime(&base)
            };
            if tezos_runtime_enabled {
                if host.store_has(&ALIAS_IMPLEMENTATION_PATH)?.is_none() {
                    let code = hex::decode(FORWARDER_CODE_HEX_V61)?;
                    host.store_write_all(&ALIAS_IMPLEMENTATION_PATH, &code)?;
                }
                Ok(MigrationStatus::Done)
            } else {
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V62 => {
            // Michelson blocks move from /tez/world_state/tez_blocks up to the
            // world-state root /tez/world_state (mirroring the EVM layout).
            // Move the existing block subtrees so a network that already
            // produced Michelson blocks keeps them across the upgrade; the
            // moves are no-ops (path not found) on a network that never
            // activated the Michelson runtime.
            let moves: &[(&[u8], &[u8])] = &[
                (
                    b"/tez/world_state/tez_blocks/blocks",
                    b"/tez/world_state/blocks",
                ),
                (
                    b"/tez/world_state/tez_blocks/live_blocks",
                    b"/tez/world_state/live_blocks",
                ),
                (
                    b"/tez/world_state/tez_blocks/current_chain_header",
                    b"/tez/world_state/current_chain_header",
                ),
            ];
            for (old, new) in moves {
                allow_path_not_found(
                    host.store_move(
                        &RefPath::assert_from(old),
                        &RefPath::assert_from(new),
                    ),
                )?;
            }
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
fn migration<Host>(host: &mut Host) -> anyhow::Result<MigrationStatus>
where
    Host: StorageV1 + IsEvmNode + KeySpaceLoader,
{
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

pub fn storage_migration<Host>(host: &mut Host) -> Result<MigrationStatus, Error>
where
    Host: StorageV1 + IsEvmNode + KeySpaceLoader,
{
    let migration_result = migration(host);
    migration_result.map_err(|_| Error::UpgradeError(UpgradeProcessError::Fallback))
}

#[cfg(test)]
mod tests {
    use super::*;
    use revm::primitives::U256 as AlloyU256;
    use revm_etherlink::precompiles::constants::TEZOSX_CALLER_ADDRESS;
    use revm_etherlink::storage::world_state_handler::{AccountInfo, StorageAccount};
    use tezos_evm_runtime::runtime::MockKernelHost;

    /// L2-1296: the V56 migration must drop the persisted balance of
    /// [`TEZOSX_CALLER_ADDRESS`] on networks where the Michelson runtime
    /// has been enabled.
    #[test]
    fn v56_migration_clears_tezosx_caller_balance_on_tezosx_networks() {
        let mut host = MockKernelHost::default();

        // Mark this host as a TezosX network — the migration is gated on
        // [`enable_tezos_runtime`] reading [`ValueType::Value`] at this
        // path.
        host.store_write_all(&crate::storage::ENABLE_TEZOS_RUNTIME, &[1u8])
            .unwrap();

        // Reproduce the bug state: write a `U256::MAX` balance to the
        // internal caller's `info` path the way pre-fix kernels did via
        // `set_info_without_code`.
        let mut account = StorageAccount::from_address(&TEZOSX_CALLER_ADDRESS).unwrap();
        account
            .set_info_without_code(
                &mut host,
                AccountInfo {
                    balance: AlloyU256::MAX,
                    nonce: 0,
                    code_hash: revm::primitives::B256::ZERO,
                    ..AccountInfo::default()
                },
            )
            .unwrap();
        // Sanity: the bug state is observable before the migration.
        let info_before = account.info(&mut host).unwrap();
        assert_eq!(info_before.balance, AlloyU256::MAX);

        // Run the V56 step directly. We dispatch via `migrate_to` rather
        // than the full `migration` pipeline so the assertion is scoped
        // to V56's behaviour and is not affected by other version steps.
        let status = migrate_to(&mut host, StorageVersion::V56).unwrap();
        assert!(matches!(status, MigrationStatus::Done));

        // After migration the `info` path is gone: reading it must fall
        // back to `AccountInfo::default()` (balance 0, nonce 0, empty
        // code), which is what RPC consumers see as a "non-existent"
        // account.
        let info_after = account.info(&mut host).unwrap();
        assert_eq!(info_after.balance, AlloyU256::ZERO);
        assert_eq!(info_after.nonce, 0);
    }

    /// L2-1296: the V56 migration must be a no-op on non-TezosX networks
    /// (TEZOSX_CALLER_ADDRESS is only ever written by code that runs
    /// under `enable_tezos_runtime`, so there is nothing to clean up
    /// elsewhere — and we don't want to touch unrelated state).
    #[test]
    fn v56_migration_is_a_no_op_on_non_tezosx_networks() {
        let mut host = MockKernelHost::default();
        // Do NOT set ENABLE_TEZOS_RUNTIME.

        let status = migrate_to(&mut host, StorageVersion::V56).unwrap();
        assert!(matches!(status, MigrationStatus::None));

        // The caller account info path must remain absent (the migration
        // must not have created one as a side effect of the no-op).
        let account = StorageAccount::from_address(&TEZOSX_CALLER_ADDRESS).unwrap();
        assert!(account.info_without_migration(&host).unwrap().is_none());
    }

    /// L2-1526: the V61 migration seeds the shared Michelson alias
    /// implementation on networks where the Michelson runtime is enabled.
    ///
    /// The expected durable path is spelled out as a literal here on purpose:
    /// a migration is a frozen historical record and must keep targeting these
    /// exact bytes even if the live path constant is later moved or renamed
    /// (etherlink/AGENTS.md migration discipline). If this assertion ever
    /// drifts from the code, the migration — not the test — is wrong.
    #[test]
    fn v61_migration_seeds_alias_implementation_on_tezosx_networks() {
        const ALIAS_IMPLEMENTATION_PATH: RefPath = RefPath::assert_from(
            b"/tez/tez_accounts/tezosx/__system__/alias_implementation",
        );

        let mut host = MockKernelHost::default();
        host.store_write_all(&crate::storage::ENABLE_TEZOS_RUNTIME, &[1u8])
            .unwrap();

        // Absent before the migration.
        assert!(matches!(
            host.store_read_all(&ALIAS_IMPLEMENTATION_PATH),
            Err(RuntimeError::PathNotFound)
        ));

        let status = migrate_to(&mut host, StorageVersion::V61).unwrap();
        assert!(matches!(status, MigrationStatus::Done));

        // Seeded with exactly the forwarder bytes frozen in the V61 arm. We
        // assert against the frozen literal (not the live `forwarder_code()`)
        // so a future drift of the live constant cannot mask a divergence.
        const FORWARDER_CODE_HEX_V61: &str = "02000000740500036c0501036805020200000065031703210743036e01000000244b5431386f444a4a4b584d4b68664531625375415047703932705963775644697173507705550368072f02000000120743036801000000076761746577617903270200000000031505700002034d053d036d034c031b0342";
        let seeded = host.store_read_all(&ALIAS_IMPLEMENTATION_PATH).unwrap();
        assert_eq!(seeded, hex::decode(FORWARDER_CODE_HEX_V61).unwrap());
    }

    /// L2-1526: the V61 migration is a no-op on non-TezosX networks — it must
    /// not write the slot (the activation-time seeding covers those networks if
    /// they ever enable the runtime).
    #[test]
    fn v61_migration_is_a_no_op_on_non_tezosx_networks() {
        const ALIAS_IMPLEMENTATION_PATH: RefPath = RefPath::assert_from(
            b"/tez/tez_accounts/tezosx/__system__/alias_implementation",
        );

        let mut host = MockKernelHost::default();
        // Do NOT enable the Michelson runtime.

        let status = migrate_to(&mut host, StorageVersion::V61).unwrap();
        assert!(matches!(status, MigrationStatus::None));
        assert!(matches!(
            host.store_read_all(&ALIAS_IMPLEMENTATION_PATH),
            Err(RuntimeError::PathNotFound)
        ));
    }

    /// Sanity: the cleanup is safe even if the bug state was never
    /// imprinted (fresh TezosX network that activated post-fix).
    #[test]
    fn v56_migration_is_safe_when_no_bug_state_exists() {
        let mut host = MockKernelHost::default();
        host.store_write_all(&crate::storage::ENABLE_TEZOS_RUNTIME, &[1u8])
            .unwrap();

        // No prior `set_info_without_code` write here — the account info
        // path doesn't exist before the migration runs.
        let account = StorageAccount::from_address(&TEZOSX_CALLER_ADDRESS).unwrap();
        assert!(account.info_without_migration(&host).unwrap().is_none());

        let status = migrate_to(&mut host, StorageVersion::V56).unwrap();
        assert!(matches!(status, MigrationStatus::Done));

        // Still no info path — `delete_info`'s PathNotFound is swallowed
        // by `StorageAccount::delete_info`, and the legacy `/balance`
        // delete is wrapped in `allow_path_not_found`.
        assert!(account.info_without_migration(&host).unwrap().is_none());
    }

    /// Phase 5.5: V57 must move both Michelson-runtime activation
    /// paths from `/evm/michelson_runtime/...` to
    /// `/tez/world_state/michelson_runtime/...` on TezosX networks.
    #[test]
    fn v57_migration_moves_michelson_runtime_paths_on_tezosx_networks() {
        let mut host = MockKernelHost::default();
        host.store_write_all(&crate::storage::ENABLE_TEZOS_RUNTIME, &[1u8])
            .unwrap();

        let legacy_target =
            RefPath::assert_from(b"/evm/michelson_runtime/target_sunrise_level");
        let legacy_sunrise =
            RefPath::assert_from(b"/evm/michelson_runtime/sunrise_level");
        let new_target = RefPath::assert_from(
            b"/tez/world_state/michelson_runtime/target_sunrise_level",
        );
        let new_sunrise =
            RefPath::assert_from(b"/tez/world_state/michelson_runtime/sunrise_level");

        // Sentinel bytes so we can verify the values survive the move.
        let target_bytes = [0xAA; 32];
        let sunrise_bytes = [0xBB; 32];
        host.store_write_all(&legacy_target, &target_bytes).unwrap();
        host.store_write_all(&legacy_sunrise, &sunrise_bytes)
            .unwrap();

        let status = migrate_to(&mut host, StorageVersion::V57).unwrap();
        assert!(matches!(status, MigrationStatus::Done));

        // Legacy paths are gone.
        assert!(matches!(
            host.store_read_all(&legacy_target),
            Err(RuntimeError::PathNotFound)
        ));
        assert!(matches!(
            host.store_read_all(&legacy_sunrise),
            Err(RuntimeError::PathNotFound)
        ));
        // The legacy parent is dropped too.
        assert!(host
            .store_has(&RefPath::assert_from(b"/evm/michelson_runtime"))
            .unwrap()
            .is_none());

        // New paths hold the original bytes.
        assert_eq!(host.store_read_all(&new_target).unwrap(), target_bytes);
        assert_eq!(host.store_read_all(&new_sunrise).unwrap(), sunrise_bytes);
    }

    /// Phase 1 of the durable-storage reorg: V52 must move the EVM-node
    /// flag from [/__evm_node] to [/base/__evm_node].
    ///
    /// This is only testable at the unit level: seeding [/__evm_node] in
    /// an E2E test would flip the mainnet kernel into EVM-node mode
    /// (the runtime checks both paths as a legacy fallback — see
    /// [evm_node_flag] in runtime/src/runtime.rs).  The mock host has no
    /// such side effect, so we can seed and verify freely here.
    #[test]
    fn v52_migration_moves_evm_node_flag() {
        let mut host = MockKernelHost::default();
        let legacy = RefPath::assert_from(b"/__evm_node");
        let new = RefPath::assert_from(b"/base/__evm_node");

        host.store_write_all(&legacy, b"evm_node_sentinel").unwrap();

        let status = migrate_to(&mut host, StorageVersion::V52).unwrap();
        assert!(matches!(status, MigrationStatus::Done));

        // Source must be gone.
        assert!(host.store_has(&legacy).unwrap().is_none());
        // Destination must carry the original bytes.
        assert_eq!(host.store_read_all(&new).unwrap(), b"evm_node_sentinel");
    }

    /// V52 must tolerate a missing [/__evm_node] (the path is absent on
    /// rollup-node sequencers that never set the flag) — the
    /// [allow_path_not_found] wrapper must not propagate the error.
    #[test]
    fn v52_migration_is_safe_when_evm_node_flag_absent() {
        let mut host = MockKernelHost::default();

        let status = migrate_to(&mut host, StorageVersion::V52).unwrap();
        assert!(matches!(status, MigrationStatus::Done));

        // Nothing created as a side-effect.
        assert!(host
            .store_has(&RefPath::assert_from(b"/__evm_node"))
            .unwrap()
            .is_none());
        assert!(host
            .store_has(&RefPath::assert_from(b"/base/__evm_node"))
            .unwrap()
            .is_none());
    }

    /// Phase 5.5: V57 is a no-op on non-TezosX networks (mainnet has
    /// never written these paths).
    #[test]
    fn v57_migration_is_a_no_op_on_non_tezosx_networks() {
        let mut host = MockKernelHost::default();
        // Do NOT set ENABLE_TEZOS_RUNTIME.

        let status = migrate_to(&mut host, StorageVersion::V57).unwrap();
        assert!(matches!(status, MigrationStatus::None));
    }

    /// Phase 5.5: V57 is safe on a fresh TezosX network where neither
    /// path was ever written (covers the `allow_path_not_found`
    /// idempotency contract).
    #[test]
    fn v57_migration_is_safe_when_paths_are_absent() {
        let mut host = MockKernelHost::default();
        host.store_write_all(&crate::storage::ENABLE_TEZOS_RUNTIME, &[1u8])
            .unwrap();

        let status = migrate_to(&mut host, StorageVersion::V57).unwrap();
        assert!(matches!(status, MigrationStatus::Done));

        // No paths created as a side effect of the no-op moves.
        assert!(host
            .store_has(&RefPath::assert_from(b"/tez/world_state/michelson_runtime"))
            .unwrap()
            .is_none());
    }

    /// Phase 6 (V58): EVM config scalars and block-index are moved under
    /// /evm/world_state/.
    #[test]
    fn v58_migration_moves_evm_config_paths() {
        let mut host = MockKernelHost::default();

        let old_paths: &[&[u8]] = &[
            b"/evm/chain_id",
            b"/evm/evm_version",
            b"/evm/sequencer_governance",
            b"/evm/sequencer_pool_address",
            b"/evm/maximum_gas_per_transaction",
        ];
        let new_paths: &[&[u8]] = &[
            b"/evm/world_state/chain_id",
            b"/evm/world_state/evm_version",
            b"/evm/world_state/sequencer_governance",
            b"/evm/world_state/sequencer_pool_address",
            b"/evm/world_state/maximum_gas_per_transaction",
        ];
        for (i, old) in old_paths.iter().enumerate() {
            host.store_write_all(&RefPath::assert_from(old), &[i as u8; 4])
                .unwrap();
        }

        // Seed indexes subtree.
        let old_index = RefPath::assert_from(b"/evm/world_state/indexes/blocks/0");
        host.store_write_all(&old_index, b"block_hash_0").unwrap();

        // Simulate the pre-existing blocks/current subtree that the kernel always
        // writes before this migration runs (block_storage.rs writes current/number
        // and current/hash). The indexes move must land as a sibling, not clobber it.
        let current_number =
            RefPath::assert_from(b"/evm/world_state/blocks/current/number");
        host.store_write_all(&current_number, b"\x00\x00\x00\x01")
            .unwrap();

        let status = migrate_to(&mut host, StorageVersion::V58).unwrap();
        assert!(matches!(status, MigrationStatus::Done));

        // Legacy paths are gone.
        for old in old_paths {
            assert!(
                host.store_has(&RefPath::assert_from(old))
                    .unwrap()
                    .is_none(),
                "legacy path {} should be empty after V58",
                std::str::from_utf8(old).unwrap()
            );
        }
        // New paths have the values.
        for (i, new) in new_paths.iter().enumerate() {
            assert_eq!(
                host.store_read_all(&RefPath::assert_from(new)).unwrap(),
                vec![i as u8; 4],
                "new path {} should hold the migrated value",
                std::str::from_utf8(new).unwrap()
            );
        }
        // Block-index reorganized.
        let new_index = RefPath::assert_from(b"/evm/world_state/blocks/indexes/0");
        assert_eq!(
            host.store_read_all(&new_index).unwrap(),
            b"block_hash_0",
            "new index path should hold the migrated value"
        );
        assert!(
            host.store_has(&old_index).unwrap().is_none(),
            "legacy index path {} should be empty after V58",
            "/evm/world_state/indexes/blocks/0"
        );
        // The empty /evm/world_state/indexes parent is removed.
        assert!(host
            .store_has(&RefPath::assert_from(b"/evm/world_state/indexes"))
            .unwrap()
            .is_none());
        // The pre-existing blocks/current subtree is untouched — indexes landed as a
        // sibling, not a replacement.
        assert_eq!(
            host.store_read_all(&current_number).unwrap(),
            b"\x00\x00\x00\x01",
            "blocks/current/number must survive the indexes move"
        );
        assert!(
            host.store_has(&RefPath::assert_from(b"/evm/world_state/blocks/current"))
                .unwrap()
                .is_some(),
            "blocks/current subtree must still exist after migration"
        );
        assert!(
            host.store_has(&RefPath::assert_from(b"/evm/world_state/blocks/indexes"))
                .unwrap()
                .is_some(),
            "blocks/indexes subtree must exist after migration"
        );
    }

    /// Phase 6 (V58) migration is idempotent on a host with no legacy paths.
    #[test]
    fn v58_migration_is_safe_when_paths_are_absent() {
        let mut host = MockKernelHost::default();
        let status = migrate_to(&mut host, StorageVersion::V58).unwrap();
        assert!(matches!(status, MigrationStatus::Done));
    }
}
