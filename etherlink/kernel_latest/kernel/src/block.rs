// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023, 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::apply::{ExecutionResult, WITHDRAWAL_OUTBOX_QUEUE};
use crate::block_in_progress;
use crate::block_storage;
use crate::blueprint::Blueprint;
use crate::blueprint_storage::{
    drop_blueprint, read_blueprint, read_current_block_header,
    store_current_block_header, BlockHeader, ChainHeader, EVMBlockHeader,
};
use crate::chains::{
    EvmLimits, TezosXBlockConstants, TezosXChainConfig, TezosXTransaction,
};
use crate::configuration::ConfigurationMode;
use crate::delayed_inbox::{DelayedInbox, Hash};
use crate::error::Error;
use crate::event::Event;
use crate::l2block::L2Block;
use crate::migration::allow_path_not_found;
use crate::storage::{self, EVM_BLOCK_IN_PROGRESS};
use crate::storage::{inside_stage_one, read_block_in_progress};
use crate::transaction::TransactionContent;
use crate::upgrade;
use crate::upgrade::KernelUpgrade;
use crate::Configuration;
use anyhow::Context;
use block_in_progress::BlockInProgress;
use evm_inspectors::TracerInput;
use primitive_types::{H160, H256, U256};
use tezos_ethereum::transaction::TransactionHash;
use tezos_evm_logging::{__trace_kernel, log, Level::*};
use tezos_evm_runtime::extensions::WithGas;
use tezos_evm_runtime::runtime::IsEvmNode;
use tezos_evm_runtime::safe_storage::{SafeStorage, TMP_PATH};
use tezos_smart_rollup::outbox::OutboxQueue;
use tezos_smart_rollup::types::Timestamp;
use tezos_smart_rollup_host::path::{OwnedPath, Path};
use tezos_smart_rollup_host::reveal::HostReveal;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::wasm::WasmHost;
use tezos_smart_rollup_keyspace::KeySpaceLoader;
use tezos_tracing::trace_kernel;
use tezosx_interfaces::Registry;

pub const GENESIS_PARENT_HASH: H256 = H256([0xff; 32]);

pub const GAS_LIMIT: u64 = 1 << 50;

#[derive(PartialEq, Debug)]
pub enum BlockInProgressComputationResult {
    RebootNeeded,
    Finished {
        included_delayed_transactions: Vec<TransactionHash>,
    },
}

#[derive(PartialEq, Debug)]
pub enum BlockComputationResult {
    RebootNeeded,
    Finished {
        included_delayed_transactions: Vec<TransactionHash>,
        block: L2Block,
    },
}

#[derive(PartialEq, Debug)]
pub enum ComputationResult {
    RebootNeeded,
    Finished,
}

// A block in progress can either come directly from the storage (when the previous run did not have enough ticks to apply the full block) or from a blueprint
pub enum BlockInProgressProvenance {
    Storage,
    Blueprint,
}

fn on_invalid_transaction(
    is_delayed: bool,
    tx_hash: TransactionHash,
    block_in_progress: &mut BlockInProgress,
) {
    if is_delayed {
        block_in_progress.register_delayed_transaction(tx_hash);
    }
}

fn max_gas_per_reboot(limits: &EvmLimits) -> u64 {
    // Arbitrarily defined as to give 500k gas in addition to `maximum_gas_limit` in our production
    // case (i.e., when `maximum_gas_limit` is 30M).
    limits.maximum_gas_limit * 61 / 60
}

pub fn can_fit_in_reboot(
    limits: &EvmLimits,
    used_gas_in_run: U256,
    tx_gas_limit: u64,
) -> bool {
    let max_gas_per_reboot = U256::from(max_gas_per_reboot(limits));
    let capped_gas_limit = u64::min(tx_gas_limit, limits.maximum_gas_limit);
    used_gas_in_run + U256::from(capped_gas_limit) <= max_gas_per_reboot
}

#[allow(clippy::too_many_arguments)]
pub fn compute<Host>(
    host: &mut Host,
    registry: &impl Registry,
    chain_config: &TezosXChainConfig,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_in_progress: &mut BlockInProgress,
    block_constants: &TezosXBlockConstants,
    sequencer_pool_address: Option<H160>,
    tracer_input: Option<TracerInput>,
    // Per-replay flag threaded down from [produce] / [handle_run_transaction]:
    // when [true], each transaction's HTTP CRAC traces are persisted under
    // [HTTP_TRACES_ROOT] via [maybe_store_http_traces_for_tx]. Plumbed as a
    // parameter (rather than read from durable storage inside apply_*) so the
    // flag doesn't need to live inside the [SafeStorage]-mirrored world state
    // subtree, and so the check happens exactly once per replay.
    http_trace_enabled: bool,
) -> Result<BlockInProgressComputationResult, anyhow::Error>
where
    Host: StorageV1 + WithGas,
{
    log!(Debug, "Queue length {}.", block_in_progress.queue_length());
    // iteration over all remaining transaction in the block
    while block_in_progress.has_tx() {
        let transaction = block_in_progress.pop_tx().ok_or(Error::Reboot)?;
        let tx_hash = transaction.tx_hash();
        let is_delayed = transaction.is_delayed();
        let data_size: u64 = transaction.data_size();

        log!(Benchmarking, "Transaction data size: {}", data_size);

        if !chain_config.can_fit_in_reboot(
            host.executed_gas().into(),
            &transaction,
            block_constants,
        )? {
            log!(
                Debug,
                "There are not enough gas left in the current kernel run \
                 to try the transaction, but it will be retried after reboot."
            );
            block_in_progress.repush_tx(transaction);

            return Ok(BlockInProgressComputationResult::RebootNeeded);
        }

        let skip_signature_check = false;
        let skip_fees_check = false;
        // If `apply_transaction` returns `ExecutionResult::Invalid`, the
        // transaction should be ignored, i.e. invalid signature or nonce.
        let apply_result = __trace_kernel!(
            "apply_transaction",
            chain_config.apply_transaction(
                block_in_progress,
                host,
                registry,
                outbox_queue,
                block_constants,
                transaction,
                block_in_progress.index,
                sequencer_pool_address,
                tracer_input,
                skip_signature_check,
                skip_fees_check,
                http_trace_enabled,
            )
        );

        // Invariant: a delayed (forced-inclusion) transaction must NEVER revert
        // the blueprint. The sequencer cannot drop a delayed transaction, so if
        // applying one returned an error and we propagated it with `?`, the
        // whole block computation would abort, the blueprint would be reverted
        // (see [revert_block]), and the offending transaction would remain in
        // the delayed inbox to be force-included again on the next reboot --
        // halting the chain. We therefore demote any application error for a
        // delayed transaction to `Invalid`: it is skipped and, through
        // [on_invalid_transaction] -> [register_delayed_transaction], removed
        // from the delayed inbox when the block is promoted.
        //
        // This cannot swallow a legitimate reboot request: reboots are raised
        // before [apply_transaction] is called (see [can_fit_in_reboot] above
        // and [Error::Reboot] on the [pop_tx] path), never from within it. In
        // practice such errors are raised before any state is committed, so
        // skipping leaves no partial writes behind.
        let apply_result = match apply_result {
            Err(err) if is_delayed => {
                log!(
                    Error,
                    "Delayed transaction {} failed to apply with '{:?}'; \
                     skipping it to keep the blueprint valid.",
                    hex::encode(tx_hash),
                    err
                );
                ExecutionResult::Invalid
            }
            result => result?,
        };

        match apply_result {
            ExecutionResult::Valid(execution_info) => {
                if is_delayed {
                    block_in_progress.register_delayed_transaction(tx_hash);
                }

                let multiplier =
                    chain_config.michelson_to_evm_gas_multiplier(block_constants);
                block_in_progress.register_valid_transaction(
                    execution_info,
                    host,
                    multiplier,
                    tracer_input,
                )?;
            }
            ExecutionResult::Invalid => {
                on_invalid_transaction(is_delayed, tx_hash, block_in_progress)
            }
        };
    }
    Ok(BlockInProgressComputationResult::Finished {
        included_delayed_transactions: block_in_progress.delayed_txs.clone(),
    })
}

enum BlueprintParsing<BIP> {
    Next(Box<BIP>),
    Postponed,
    None,
}

pub fn bip_from_blueprint<Host>(
    host: &Host,
    chain_config: &TezosXChainConfig,
    next_bip_number: U256,
    hash: H256,
    tezos_parent_hash: H256,
    blueprint: Blueprint,
) -> BlockInProgress
where
    Host: StorageV1,
{
    let gas_price = chain_config.base_fee_per_gas(host, blueprint.timestamp);

    let bip = BlockInProgress::from_blueprint(
        blueprint,
        next_bip_number,
        hash,
        tezos_parent_hash,
        gas_price,
    );

    tezos_evm_logging::log!(tezos_evm_logging::Level::Debug, "bip: {bip:?}");
    bip
}

fn get_next_bip_info<Host>(host: &mut Host) -> (U256, Timestamp, EVMBlockHeader)
where
    Host: StorageV1 + KeySpaceLoader,
{
    let current_block_header = crate::storage::load_base_keyspace(host)
        .map_err(crate::error::Error::from)
        .and_then(|base| read_current_block_header(&base));
    match current_block_header {
        Err(_) => (
            U256::zero(),
            Timestamp::from(0),
            EVMBlockHeader::genesis_header(),
        ),
        Ok(BlockHeader {
            blueprint_header,
            chain_header,
        }) => (
            blueprint_header.number + 1,
            blueprint_header.timestamp,
            chain_header,
        ),
    }
}

#[cfg_attr(feature = "benchmark", inline(never))]
fn build_next_bip_from_blueprints<Host>(
    host: &mut Host,
    chain_config: &TezosXChainConfig,
    next_bip_number: U256,
    timestamp: Timestamp,
    chain_header: &EVMBlockHeader,
    config: &mut Configuration,
    kernel_upgrade: &Option<KernelUpgrade>,
) -> anyhow::Result<BlueprintParsing<BlockInProgress>>
where
    Host: HostReveal + StorageV1 + WasmHost + KeySpaceLoader,
{
    log!(Debug, "Next blueprint number: {:?}", next_bip_number);
    let (blueprint, size) =
        read_blueprint(host, config, next_bip_number, timestamp, chain_header)?;
    log!(Benchmarking, "Size of blueprint: {}", size);
    match blueprint {
        Some(blueprint) => {
            if let Some(kernel_upgrade) = kernel_upgrade {
                if blueprint.timestamp >= kernel_upgrade.activation_timestamp {
                    upgrade::upgrade(host, kernel_upgrade.preimage_hash)?;
                    // We abort the call, as there is no blueprint to execute,
                    // the kernel will reboot.
                    return Ok(BlueprintParsing::Postponed);
                }
            }
            let tezos_parent_hash = block_storage::read_current_hash(
                host,
                &crate::chains::TEZ_SAFE_STORAGE_ROOT_PATH,
            )
            .unwrap_or_else(|_| {
                H256(*tezos_tezlink::block::TezBlock::genesis_block_hash())
            });
            let bip: BlockInProgress = bip_from_blueprint(
                host,
                chain_config,
                next_bip_number,
                chain_header.hash(),
                tezos_parent_hash,
                blueprint,
            );
            Ok(BlueprintParsing::Next(Box::new(bip)))
        }
        None => Ok(BlueprintParsing::None),
    }
}

#[allow(clippy::too_many_arguments)]
pub fn compute_bip<Host>(
    host: &mut Host,
    registry: &impl Registry,
    chain_config: &TezosXChainConfig,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    mut block_in_progress: BlockInProgress,
    sequencer_pool_address: Option<H160>,
    tracer_input: Option<TracerInput>,
    da_fee_per_byte: U256,
    coinbase: H160,
    chain_header: EVMBlockHeader,
    http_trace_enabled: bool,
) -> anyhow::Result<BlockComputationResult>
where
    Host: StorageV1 + WithGas,
{
    let constants =
        chain_config.constants(host, &block_in_progress, da_fee_per_byte, coinbase)?;
    let result = compute(
        host,
        registry,
        chain_config,
        outbox_queue,
        &mut block_in_progress,
        &constants,
        sequencer_pool_address,
        tracer_input,
        http_trace_enabled,
    )?;
    match result {
        BlockInProgressComputationResult::RebootNeeded => {
            log!(Info, "Ask for reboot.");
            storage::store_block_in_progress(host, &block_in_progress)?;
            Ok(BlockComputationResult::RebootNeeded)
        }
        BlockInProgressComputationResult::Finished {
            included_delayed_transactions,
        } => {
            crate::gas_price::register_block(
                host,
                block_in_progress.cumulative_execution_gas,
                block_in_progress.timestamp,
                block_in_progress.queue_length(),
            )?;
            let new_block = chain_config
                .finalize_and_store(host, block_in_progress, &constants, chain_header)
                .context("Failed to finalize the block in progress")?;
            Ok(BlockComputationResult::Finished {
                included_delayed_transactions,
                block: new_block,
            })
        }
    }
}

fn revert_block<Host>(
    safe_host: &mut SafeStorage<&mut Host>,
    block_in_progress_provenance: &BlockInProgressProvenance,
    number: U256,
    error: anyhow::Error,
) -> anyhow::Result<()>
where
    Host: StorageV1 + KeySpaceLoader,
{
    log!(
        Error,
        "Block{} {} failed with '{:?}'. Reverting.",
        match block_in_progress_provenance {
            BlockInProgressProvenance::Storage => {
                "InProgress"
            }
            BlockInProgressProvenance::Blueprint => {
                ""
            }
        },
        number,
        error
    );
    safe_host.revert()?;
    drop_blueprint(
        &mut crate::storage::load_base_keyspace(safe_host.host)?,
        number,
    )?;
    Ok(())
}

pub fn health_check<Host>(
    host: &mut Host,
    config: &mut Configuration,
) -> Result<(), anyhow::Error>
where
    Host: StorageV1 + WasmHost + IsEvmNode + KeySpaceLoader,
{
    if host.last_run_aborted()? {
        log!(Error, "Something went wrong during previous kernel_run");

        if !inside_stage_one(host) {
            // Something went wrong outside stage one, leading us to assume this is most certainly
            // related to stage 2. We clean-up potential leftovers of the interrupted execution.

            allow_path_not_found(host.store_delete(&TMP_PATH))?;
            allow_path_not_found(host.store_delete(&EVM_BLOCK_IN_PROGRESS))?;

            let (number, previous_timestamp, ref previous_chain_header) =
                get_next_bip_info::<Host>(host);

            match read_blueprint(
                host,
                config,
                number,
                previous_timestamp,
                previous_chain_header,
            )? {
                (Some(blueprint), _) if blueprint.transactions.len() == 1 => {
                    // Blueprints with one transaction can be treated as certificates that given
                    // transactions indeed trigger WASM traps. If said transaction is part of the
                    // delayed inbox, it can never been included in a valid blueprint by
                    // construction and should be dropped to protect the kernel.
                    if let ConfigurationMode::Sequencer {
                        ref mut delayed_inbox,
                        ..
                    } = config.mode
                    {
                        let potential_culprits: Vec<_> = blueprint
                            .transactions
                            .iter()
                            .filter_map(|txn| match txn {
                                TezosXTransaction::Ethereum(tx) => match tx.content {
                                    TransactionContent::TezosDelayed(_)
                                    | TransactionContent::EthereumDelayed(_) => {
                                        Some(tx.tx_hash)
                                    }
                                    _ => None,
                                },
                                _ => None,
                            })
                            .collect();

                        for hash in potential_culprits {
                            let mut base = crate::storage::load_base_keyspace(host)?;
                            delayed_inbox.delete(&mut base, Hash(hash))?;
                            Event::DroppedDelayedTransaction(hash)
                                .store(host, &mut base)?;
                        }
                    }
                }
                _ => (),
            }

            drop_blueprint(&mut crate::storage::load_base_keyspace(host)?, number)?;
        }

        return Ok(());
    }

    Ok(())
}

fn clean_delayed_transactions<Host>(
    host: &mut Host,
    delayed_inbox: &mut DelayedInbox,
    delayed_txs: Vec<TransactionHash>,
) -> anyhow::Result<()>
where
    Host: StorageV1 + KeySpaceLoader,
{
    for hash in delayed_txs {
        delayed_inbox
            .delete(&mut crate::storage::load_base_keyspace(host)?, hash.into())?;
    }
    Ok(())
}

pub fn promote_block<Host>(
    safe_host: &mut SafeStorage<&mut Host>,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_in_progress_provenance: &BlockInProgressProvenance,
    block_header: BlockHeader<ChainHeader>,
    config: &mut Configuration,
    delayed_txs: Vec<TransactionHash>,
) -> anyhow::Result<()>
where
    Host: StorageV1 + WasmHost + IsEvmNode + KeySpaceLoader,
{
    if let BlockInProgressProvenance::Storage = block_in_progress_provenance {
        storage::delete_block_in_progress(safe_host)?;
    }
    safe_host.promote()?;
    safe_host.promote_trace()?;
    safe_host.promote_http_trace()?;
    {
        let mut base = crate::storage::load_base_keyspace(safe_host.host)?;
        drop_blueprint(&mut base, block_header.blueprint_header.number)?;
        store_current_block_header(&mut base, &block_header)?;
    }

    let event = Event::blueprint_applied(block_header);

    {
        let mut base = crate::storage::load_base_keyspace(safe_host.host)?;
        event.store(safe_host.host, &mut base)?;
    }

    let written = outbox_queue.flush_queue(safe_host.host);
    // Log to Info only if we flushed messages.
    let level = if written > 0 { Info } else { Debug };
    log!(level, "Flushed outbox queue messages ({} flushed)", written);

    if let ConfigurationMode::Sequencer { delayed_inbox, .. } = &mut config.mode {
        clean_delayed_transactions(safe_host.host, delayed_inbox, delayed_txs)?;
    }

    Ok(())
}

#[trace_kernel("stage_two")]
pub fn produce<Host>(
    host: &mut Host,
    chain_config: &TezosXChainConfig,
    config: &mut Configuration,
    sequencer_pool_address: Option<H160>,
    tracer_input: Option<TracerInput>,
) -> Result<ComputationResult, anyhow::Error>
where
    Host: HostReveal + StorageV1 + WasmHost + WithGas + IsEvmNode + KeySpaceLoader,
{
    let da_fee_per_byte = crate::retrieve_da_fee(host)?;

    // Read the HTTP-trace replay flag once, before any [SafeStorage]
    // wrapping, and thread the resulting boolean through [compute_bip] →
    // [compute] → apply sites. The flag therefore doesn't need to live in
    // the [SafeStorage]-mirrored world state subtree, and the common
    // (unset) case costs a single [store_has] per block instead of one
    // per transaction.
    let http_trace_enabled = crate::storage::is_http_trace_enabled(host);

    let kernel_upgrade = upgrade::read_kernel_upgrade(host)?;

    // If there's a pool address, the coinbase in block constants and miner
    // in blocks is set to the pool address.
    let coinbase = sequencer_pool_address.unwrap_or_default();

    let (next_bip_number, timestamp, chain_header) = get_next_bip_info(host);

    let mut safe_host = SafeStorage {
        host,
        world_states: chain_config
            .storage_root_paths(next_bip_number)
            .iter()
            .map(OwnedPath::from)
            .collect(),
    };
    let outbox_queue = OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX)?;

    let registry = chain_config.init_registry();

    // Check if there's a BIP in storage to resume its execution
    let (block_in_progress_provenance, block_in_progress) =
        match read_block_in_progress(&safe_host)? {
            Some(block_in_progress) => {
                log!(Debug, "Restauring BIP from storage.");
                (BlockInProgressProvenance::Storage, block_in_progress)
            }
            None => {
                // Using `safe_host.host` allows to escape from the failsafe storage, which is necessary
                // because the sequencer pool address is located outside of `/evm/world_state`.
                upgrade::possible_sequencer_upgrade(safe_host.host)?;

                log!(Debug, "Creating BIP from Blueprint.");
                // Execute at most one of the stored blueprints
                let block_in_progress = match build_next_bip_from_blueprints(
                    safe_host.host,
                    chain_config,
                    next_bip_number,
                    timestamp,
                    &chain_header,
                    config,
                    &kernel_upgrade,
                )? {
                    BlueprintParsing::Next(bip) => {
                        log!(Debug, "Creating BIP from Blueprint: Success.");
                        bip
                    }
                    BlueprintParsing::Postponed => {
                        log!(
                            Debug,
                            "Creating BIP from Blueprint: Postponed to apply an upgrade."
                        );
                        return Ok(ComputationResult::RebootNeeded);
                    }
                    BlueprintParsing::None => {
                        log!(Debug, "Creating BIP from Blueprint: Failure.");
                        return Ok(ComputationResult::Finished);
                    }
                };

                // We are going to execute a new block, we copy the storage to allow
                // to revert if the block fails.
                safe_host.start()?;
                (BlockInProgressProvenance::Blueprint, *block_in_progress)
            }
        };

    let processed_blueprint = block_in_progress.number;
    let computation_result = compute_bip(
        &mut safe_host,
        &registry,
        chain_config,
        &outbox_queue,
        block_in_progress,
        sequencer_pool_address,
        tracer_input,
        da_fee_per_byte,
        coinbase,
        chain_header,
        http_trace_enabled,
    );

    match computation_result {
        Ok(BlockComputationResult::Finished {
            included_delayed_transactions,
            block,
        }) => {
            let timestamp = block.timestamp();
            promote_block(
                &mut safe_host,
                &outbox_queue,
                &block_in_progress_provenance,
                block.header(),
                config,
                included_delayed_transactions,
            )?;
            // Write sunrise_level only after the block has been committed, so
            // it is atomic with the Tezos genesis block existing in storage.
            if chain_config.is_tezos_runtime_enabled(processed_blueprint)
                && crate::storage::read_michelson_runtime_sunrise_level(safe_host.host)
                    .is_none()
            {
                crate::storage::store_michelson_runtime_sunrise_level(
                    safe_host.host,
                    processed_blueprint,
                )?;
                // L2-1526: seed the shared Michelson alias implementation
                // when the runtime activates on a fresh network. Migrations
                // only run on upgrades, so this is the genesis seeding point.
                // Like the sunrise_level write above, this runs after the
                // block has been promoted, so the slot is first reflected in
                // the *next* block's Michelson state_root (its content is
                // rooted under /tez/tez_accounts). It is identical across all
                // replicas and idempotent.
                tezosx_tezos_runtime::alias_forwarder::init_alias_implementation(
                    safe_host.host,
                )
                .map_err(|e| {
                    anyhow::anyhow!("seeding alias implementation failed: {e}")
                })?;
                // Seed the address registry (null address at index 0) when
                // the runtime activates on a fresh network. Activation is the
                // only seeding point.
                tezos_execution::mir_ctx::init_address_registry(safe_host.host).map_err(
                    |e| anyhow::anyhow!("seeding address registry failed: {e}"),
                )?;
            }
            upgrade::possible_sequencer_key_change(safe_host.host, timestamp)?;

            if safe_host.is_evm_node() {
                Ok(ComputationResult::Finished)
            } else {
                Ok(ComputationResult::RebootNeeded)
            }
        }
        Ok(BlockComputationResult::RebootNeeded) => {
            // The computation will resume at next reboot, we leave the
            // storage untouched.
            Ok(ComputationResult::RebootNeeded)
        }
        Err(err) => {
            revert_block(
                &mut safe_host,
                &block_in_progress_provenance,
                processed_blueprint,
                err,
            )?;
            // The block was reverted because it failed. We don't know at
            // which point did it fail nor why. We cannot make assumption
            // on how many ticks it consumed before failing. Therefore
            // the safest solution is to simply reboot after a failure.
            Ok(ComputationResult::RebootNeeded)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_in_progress::BlockInProgress;
    use crate::block_storage::internal_for_tests::{
        read_transaction_receipt, read_transaction_receipt_status,
    };
    use crate::block_storage::{self, read_tez_current_block};
    use crate::blueprint::Blueprint;
    use crate::blueprint_storage::store_inbox_blueprint_by_number;
    use crate::blueprint_storage::{
        store_current_tez_block_header, store_inbox_blueprint, TezBlockHeader,
    };
    use crate::chains::TezlinkOperation;
    use crate::chains::{DebugFeatures, TezlinkContent};
    use crate::chains::{
        ExperimentalFeatures, TezlinkBlockConstants, TezosXBlockConstants,
        TezosXChainConfig, TezosXTransaction, EVM_ETH_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
        TEZOS_ACCOUNTS_ROOT, TEZ_SAFE_STORAGE_ROOT_PATH,
    };
    use crate::configuration::fetch_evm_chain_id;
    use crate::fees::MINIMUM_BASE_FEE_PER_GAS;
    use crate::fees::{DA_FEE_PER_BYTE, DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER};
    use crate::registry_impl::RegistryImpl;
    use crate::retrieve_block_fees;
    use crate::storage::read_block_in_progress;
    use crate::storage::read_last_info_per_level_timestamp;
    use crate::transaction::Transaction;
    use crate::transaction::TransactionContent;
    use crate::transaction::TransactionContent::Ethereum;
    use crate::transaction::TransactionContent::EthereumDelayed;
    use primitive_types::{H160, U256};
    use revm::primitives::hardfork::SpecId;
    use revm_etherlink::helpers::legacy::{alloy_to_u256, h160_to_alloy, u256_to_alloy};
    use revm_etherlink::storage::world_state_handler::StorageAccount;
    use sha3::{Digest, Keccak256};
    use std::str::FromStr;
    use tezos_crypto_rs::hash::ChainId;
    use tezos_crypto_rs::hash::HashTrait;
    use tezos_crypto_rs::hash::SecretKeyEd25519;
    use tezos_data_encoding::types::Narith;
    use tezos_ethereum::block::{BlockConstants, BlockFees};
    use tezos_ethereum::transaction::{
        TransactionHash, TransactionStatus, TransactionType, TRANSACTION_HASH_SIZE,
    };
    use tezos_ethereum::tx_common::EthereumTransactionCommon;
    use tezos_evm_runtime::extensions::WithGas;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::safe_storage::ETHERLINK_SAFE_STORAGE_ROOT_PATH;
    use tezos_execution::context;

    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup::types::PublicKey;
    use tezos_smart_rollup::types::PublicKeyHash;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;
    use tezos_smart_rollup_host::path::{concat, RefPath};
    use tezos_tezlink::operation::sign_operation;
    use tezos_tezlink::operation::Parameters;
    use tezos_tezlink::protocol::{Protocol, TARGET_TEZOS_PROTOCOL};

    fn read_current_number<Host>(host: &mut Host) -> anyhow::Result<U256>
    where
        Host: StorageV1 + KeySpaceLoader,
    {
        Ok(crate::blueprint_storage::read_current_blueprint_header(
            &crate::storage::load_base_keyspace(host)?,
        )?
        .number)
    }

    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_host::wasm::WasmHost;
    use tezos_tezlink::block::TezBlock;
    use tezos_tezlink::operation::ManagerOperation;
    use tezos_tezlink::operation::ManagerOperationContent;
    use tezos_tezlink::operation::ManagerOperationContentConv;
    use tezos_tezlink::operation::Operation;
    use tezos_tezlink::operation::OperationContent;
    use tezos_tezlink::operation::OriginationContent;
    use tezos_tezlink::operation::RevealContent;
    use tezos_tezlink::operation::Script;
    use tezos_tezlink::operation::TransferContent;

    #[derive(Clone)]
    struct Bootstrap {
        pkh: PublicKeyHash,
        pk: PublicKey,
        sk: SecretKeyEd25519,
    }

    fn bootstrap1() -> Bootstrap {
        Bootstrap {
            pkh: PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                .unwrap(),
            pk: PublicKey::from_b58check(
                "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
            )
            .unwrap(),
            sk: SecretKeyEd25519::from_base58_check(
                "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh",
            )
            .unwrap(),
        }
    }

    fn bootstrap2() -> Bootstrap {
        Bootstrap {
            pkh: PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN")
                .unwrap(),
            pk: PublicKey::from_b58check(
                "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9",
            )
            .unwrap(),
            sk: SecretKeyEd25519::from_base58_check(
                "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo",
            )
            .unwrap(),
        }
    }

    fn make_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        content: Vec<OperationContent>,
    ) -> Operation {
        let branch = TezBlock::genesis_block_hash();
        let content = content
            .into_iter()
            .map(|c| -> ManagerOperationContent {
                ManagerOperation {
                    source: source.pkh.clone(),
                    fee: fee.into(),
                    counter: counter.into(),
                    operation: c,
                    gas_limit: gas_limit.into(),
                    storage_limit: storage_limit.into(),
                }
                .into_manager_operation_content()
            })
            .collect::<Vec<ManagerOperationContent>>();

        let signature = sign_operation(&source.sk, &branch, &content).unwrap();

        Operation {
            branch,
            content,
            signature,
        }
    }
    fn make_reveal_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source.clone(),
            vec![OperationContent::Reveal(RevealContent {
                pk: source.pk,
                proof: None,
            })],
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn make_transaction_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        amount: Narith,
        destination: Contract,
        parameters: Parameters,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source,
            vec![OperationContent::Transfer(TransferContent {
                amount,
                destination,
                parameters,
            })],
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn make_origination_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        balance: Narith,
        code: Vec<u8>,
        storage: Vec<u8>,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source,
            vec![OperationContent::Origination(OriginationContent {
                balance,
                delegate: None,
                script: Script { code, storage },
            })],
        )
    }

    fn blueprint(transactions: Vec<Transaction>) -> Blueprint {
        Blueprint {
            transactions: transactions
                .into_iter()
                .map(TezosXTransaction::from)
                .collect(),
            timestamp: Timestamp::from(0i64),
        }
    }

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    fn set_balance(host: &mut impl StorageV1, address: &H160, balance: U256) {
        let mut account = StorageAccount::from_address(&h160_to_alloy(address)).unwrap();
        let mut info = account.info(host).unwrap();
        info.balance = u256_to_alloy(&balance);
        account.set_info(host, info).unwrap();
    }

    fn get_balance(host: &mut impl StorageV1, address: &H160) -> U256 {
        let account = StorageAccount::from_address(&h160_to_alloy(address)).unwrap();
        let info = account.info(host).unwrap();
        alloy_to_u256(&info.balance)
    }

    const DUMMY_CHAIN_ID: U256 = U256::one();
    const DUMMY_BASE_FEE_PER_GAS: u64 = MINIMUM_BASE_FEE_PER_GAS;
    const DUMMY_DA_FEE: u64 = DA_FEE_PER_BYTE;

    fn dummy_tezosx_config(spec_id: SpecId) -> TezosXChainConfig {
        TezosXChainConfig::create_config(
            DUMMY_CHAIN_ID,
            EvmLimits::default(),
            spec_id,
            ExperimentalFeatures::default(),
            DebugFeatures::default(),
            ChainId::try_from_bytes(&1u32.to_le_bytes()).unwrap(),
        )
    }

    /// Pre-populate all safe storage roots so that `SafeStorage::start()`'s
    /// `store_copy` of each root succeeds. Required by every test that
    /// produces blocks. Mirrors the production bootstrap performed by
    /// [`crate::kernel`].
    fn init_safe_storage_roots(host: &mut impl StorageV1) {
        host.store_write_all(&ETHERLINK_SAFE_STORAGE_ROOT_PATH, b"placeholder")
            .expect("Write in durable storage should have succeeded");
        host.store_write_all(&EVM_ETH_ACCOUNTS_SAFE_STORAGE_ROOT_PATH, b"placeholder")
            .expect("Write in durable storage should have succeeded");
        host.store_write_all(&TEZ_SAFE_STORAGE_ROOT_PATH, b"placeholder")
            .expect("Write in durable storage should have succeeded");
        host.store_write_all(&TEZOS_ACCOUNTS_ROOT, b"placeholder")
            .expect("Write in durable storage should have succeeded");
    }

    fn dummy_configuration() -> Configuration {
        Configuration::default()
    }

    fn dummy_block_fees() -> BlockFees {
        BlockFees::new(
            U256::from(DUMMY_BASE_FEE_PER_GAS),
            U256::from(DUMMY_BASE_FEE_PER_GAS),
            U256::from(DUMMY_DA_FEE),
        )
    }

    fn dummy_eth_gen_transaction(
        nonce: u64,
        type_: TransactionType,
    ) -> EthereumTransactionCommon {
        let chain_id = Some(DUMMY_CHAIN_ID);
        let gas_price = U256::from(DUMMY_BASE_FEE_PER_GAS);
        let gas_limit = 21000u64;

        let gas_for_fees = crate::fees::gas_for_fees(
            DUMMY_DA_FEE.into(),
            DUMMY_BASE_FEE_PER_GAS.into(),
            &[],
            &[],
            0,
        )
        .unwrap();
        let gas_limit = gas_limit + gas_for_fees;

        let to = address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea");
        let value = U256::from(500000000u64);
        let data: Vec<u8> = vec![];
        EthereumTransactionCommon::new(
            type_,
            chain_id,
            nonce,
            gas_price,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            vec![],
            None,
            None,
        )
    }

    // When updating the dummy txs, you can use the `resign` function to get the updated
    // signatures (see bottom of test module)
    fn dummy_eth_caller() -> H160 {
        H160::from_str("f95abdf6ede4c3703e0e9453771fbee8592d31e9").unwrap()
    }

    fn sign_transaction(tx: EthereumTransactionCommon) -> EthereumTransactionCommon {
        let private_key =
            "e922354a3e5902b5ac474f3ff08a79cff43533826b8f451ae2190b65a9d26158";

        tx.sign_transaction(private_key.to_string()).unwrap()
    }

    fn make_dummy_transaction(
        nonce: u64,
        type_: TransactionType,
    ) -> EthereumTransactionCommon {
        let tx = dummy_eth_gen_transaction(nonce, type_);
        sign_transaction(tx)
    }

    // A type-4 (EIP-7702) transaction whose authorization list is present but
    // empty. REVM rejects this shape with "Authorization list cannot be empty
    // per EIP-7702" while building the transaction environment, before any
    // state is touched (see `revm/src/lib.rs`). Signed with the same key as the
    // other dummy transactions, so the caller is `dummy_eth_caller()`.
    fn dummy_eip7702_empty_authorization_list_tx() -> EthereumTransactionCommon {
        let gas_price = U256::from(DUMMY_BASE_FEE_PER_GAS);
        let to = address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea");
        let tx = EthereumTransactionCommon::new(
            TransactionType::Eip7702,
            Some(DUMMY_CHAIN_ID),
            0,
            gas_price,
            gas_price,
            100_000u64,
            to,
            U256::zero(),
            vec![],
            vec![],
            // The poison shape: an EIP-7702 authorization list that is present
            // but empty.
            Some(vec![]),
            None,
        );
        sign_transaction(tx)
    }

    fn dummy_eth_transaction_zero() -> EthereumTransactionCommon {
        // corresponding caller's address is 0xf95abdf6ede4c3703e0e9453771fbee8592d31e9
        // private key 0xe922354a3e5902b5ac474f3ff08a79cff43533826b8f451ae2190b65a9d26158
        let nonce = 0;
        make_dummy_transaction(nonce, TransactionType::Legacy)
    }

    fn dummy_eth_transaction_one() -> EthereumTransactionCommon {
        // corresponding caller's address is 0xf95abdf6ede4c3703e0e9453771fbee8592d31e9
        // private key 0xe922354a3e5902b5ac474f3ff08a79cff43533826b8f451ae2190b65a9d26158
        let nonce = 1;
        make_dummy_transaction(nonce, TransactionType::Legacy)
    }

    fn dummy_eth_transaction_deploy_from_nonce_and_pk(
        nonce: u64,
        private_key: &str,
    ) -> EthereumTransactionCommon {
        let gas_price = U256::from(DUMMY_BASE_FEE_PER_GAS);
        // gas limit was estimated using Remix on Shanghai network (256,842)
        // plus a safety margin for gas accounting discrepancies
        let gas_limit = 300_000u64;
        let value = U256::zero();
        // corresponding contract is kernel_benchmark/scripts/benchmarks/contracts/storage.sol
        let data: Vec<u8> = hex::decode("608060405234801561001057600080fd5b5061017f806100206000396000f3fe608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c63430008120033").unwrap();

        let gas_for_fees =
            crate::fees::gas_for_fees(DUMMY_DA_FEE.into(), gas_price, &data, &[], 0)
                .unwrap();
        let gas_limit = gas_limit + gas_for_fees;

        let tx = EthereumTransactionCommon::new(
            tezos_ethereum::transaction::TransactionType::Legacy,
            Some(DUMMY_CHAIN_ID),
            nonce,
            gas_price,
            gas_price,
            gas_limit,
            None,
            value,
            data,
            vec![],
            None,
            None,
        );

        tx.sign_transaction(private_key.to_string()).unwrap()
    }

    fn dummy_eth_transaction_deploy() -> EthereumTransactionCommon {
        // corresponding caller's address is 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        dummy_eth_transaction_deploy_from_nonce_and_pk(
            0,
            "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701",
        )
    }

    fn store_blueprints_from_number<Host>(
        host: &mut Host,
        start_number: U256,
        blueprints: Vec<Blueprint>,
    ) where
        Host: StorageV1 + KeySpaceLoader,
    {
        for (i, blueprint) in blueprints.into_iter().enumerate() {
            store_inbox_blueprint_by_number(
                &mut crate::storage::load_base_keyspace(host).unwrap(),
                blueprint,
                start_number + U256::from(i),
            )
            .expect("Should have stored blueprint");
        }
    }

    fn store_blueprints<Host>(host: &mut Host, blueprints: Vec<Blueprint>)
    where
        Host: StorageV1 + KeySpaceLoader,
    {
        store_blueprints_from_number::<Host>(host, U256::zero(), blueprints)
    }

    fn store_block_fees(
        host: &mut impl StorageV1,
        block_fees: &BlockFees,
    ) -> anyhow::Result<()> {
        storage::store_minimum_base_fee_per_gas(
            host,
            block_fees.minimum_base_fee_per_gas(),
        )?;
        storage::store_da_fee(host, block_fees.da_fee_per_byte())?;
        Ok(())
    }

    fn produce_block_with_several_valid_txs<Host>(host: &mut Host)
    where
        Host: HostReveal + StorageV1 + WasmHost + WithGas + IsEvmNode + KeySpaceLoader,
    {
        let tx_hash_0 = [0; TRANSACTION_HASH_SIZE];
        let tx_hash_1 = [1; TRANSACTION_HASH_SIZE];

        let transactions = vec![
            Transaction {
                tx_hash: tx_hash_0,
                content: Ethereum(dummy_eth_transaction_zero()),
            },
            Transaction {
                tx_hash: tx_hash_1,
                content: Ethereum(dummy_eth_transaction_one()),
            },
        ];

        store_blueprints(host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        set_balance(host, &sender, U256::from(10000000000000000000u64));
        store_block_fees(host, &dummy_block_fees()).unwrap();

        produce(
            host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");
    }

    fn assert_current_block_reading_validity(host: &mut impl StorageV1) {
        match block_storage::read_current_etherlink_block(host) {
            Ok(_) => (),
            Err(e) => {
                panic!("Block reading failed: {e:?}\n")
            }
        }
    }

    fn dummy_tezosx_config_with_tezos_runtime(
        host: &mut (impl StorageV1 + KeySpaceLoader),
    ) -> TezosXChainConfig {
        host.store_write(&crate::storage::ENABLE_TEZOS_RUNTIME, &[], 0)
            .expect("Should have written feature flag");
        init_safe_storage_roots(host);
        let experimental_features = ExperimentalFeatures::read_from_storage(host);
        let debug_features = DebugFeatures::read_from_storage(host);
        TezosXChainConfig::create_config(
            DUMMY_CHAIN_ID,
            EvmLimits::default(),
            SpecId::default(),
            experimental_features,
            debug_features,
            ChainId::try_from_bytes(&1u32.to_le_bytes()).unwrap(),
        )
    }

    #[test]
    // Test that a TezBlock is created and stored when Tezos operations are executed via EVM chain config
    fn test_tezblock_stored_after_tezos_operation() {
        use tezos_execution::account_storage::{
            set_tezos_account_info, TezosAccountInfo,
        };

        let mut host = MockKernelHost::default();

        // Store bootstrap2 in the tezlink context to ensure the
        // Tezlink context is not empty and can thus be backed up
        context::implicit_from_public_key_hash(&bootstrap2().pkh)
            .expect("Account interface should be correct")
            .allocate(&mut host)
            .expect("Contract initialization should have succeed");

        let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);
        let mut config = dummy_configuration();

        let bootstrap = bootstrap1();

        // Initialize the Tezos account in the TezosX storage path
        // (read by the Michelson runtime in apply.rs for TezosDelayed operations)
        let account_info = TezosAccountInfo {
            balance: U256::from(10),
            nonce: 0,
            pub_key: None,
        };
        set_tezos_account_info(&mut host, &bootstrap.pkh, account_info)
            .expect("Should have set account info");

        // Create a Tezos reveal operation wrapped as TezosDelayed for EVM chain
        let reveal = make_reveal_operation(1, 1, 500, 0, bootstrap);
        let tx_hash = reveal.hash().unwrap().into();
        let tezos_tx = Transaction {
            tx_hash,
            content: TransactionContent::TezosDelayed(reveal),
        };

        store_blueprints(&mut host, vec![blueprint(vec![tezos_tx])]);
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        // Produce the block
        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);

        // Verify that a TezBlock was stored (under /tez/world_state)
        let tez_block_number =
            block_storage::read_current_number(&host, &TEZ_SAFE_STORAGE_ROOT_PATH)
                .expect("TezBlock number should be readable");
        // Block number is 0 for the first block (same as EVM block numbering)
        assert_eq!(U256::from(0), tez_block_number);

        let tez_block_hash =
            block_storage::read_current_hash(&host, &TEZ_SAFE_STORAGE_ROOT_PATH)
                .expect("TezBlock hash should be readable");
        // The hash should not be zero (it's computed from the block content)
        assert_ne!(H256::zero(), tez_block_hash);
    }

    #[test]
    fn test_tezblocks_are_chained() {
        use tezos_execution::account_storage::{
            set_tezos_account_info, TezosAccountInfo,
        };

        let mut host = MockKernelHost::default();

        // Store bootstrap2 in the tezlink context to ensure the
        // Tezlink context is not empty and can thus be backed up
        context::implicit_from_public_key_hash(&bootstrap2().pkh)
            .expect("Account interface should be correct")
            .allocate(&mut host)
            .expect("Contract initialization should have succeed");

        let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);
        let mut config = dummy_configuration();

        let bootstrap = bootstrap1();

        let account_info = TezosAccountInfo {
            balance: U256::from(100),
            nonce: 0,
            pub_key: None,
        };
        set_tezos_account_info(&mut host, &bootstrap.pkh, account_info)
            .expect("Should have set account info");

        // Block 0: reveal operation
        let reveal = make_reveal_operation(1, 1, 500, 0, bootstrap.clone());
        let tx_hash = reveal.hash().unwrap().into();
        let tezos_tx = Transaction {
            tx_hash,
            content: TransactionContent::TezosDelayed(reveal),
        };

        store_blueprints(&mut host, vec![blueprint(vec![tezos_tx])]);
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);

        // Record block 0 hash
        let block_0_hash =
            block_storage::read_current_hash(&host, &TEZ_SAFE_STORAGE_ROOT_PATH)
                .expect("TezBlock 0 hash should be readable");
        assert_ne!(H256::zero(), block_0_hash);

        // Block 1: transfer operation
        let bootstrap2 = bootstrap2();
        let transfer = make_transaction_operation(
            1,
            2,
            3000,
            0,
            bootstrap,
            10_u64.into(),
            Contract::Implicit(bootstrap2.pkh),
            Parameters::default(),
        );
        let tx_hash = transfer.hash().unwrap().into();
        let tezos_tx = Transaction {
            tx_hash,
            content: TransactionContent::TezosDelayed(transfer),
        };

        store_inbox_blueprint_by_number(
            &mut crate::storage::load_base_keyspace(&mut host).unwrap(),
            blueprint(vec![tezos_tx]),
            U256::from(1),
        )
        .expect("Should have stored blueprint");

        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);

        // Block 1 hash should be different from block 0
        let block_1_hash =
            block_storage::read_current_hash(&host, &TEZ_SAFE_STORAGE_ROOT_PATH)
                .expect("TezBlock 1 hash should be readable");
        assert_ne!(block_0_hash, block_1_hash);

        // Read block 1 from storage and check its previous_hash equals block 0's hash
        let block_path = concat(
            &TEZ_SAFE_STORAGE_ROOT_PATH,
            &RefPath::assert_from(b"/blocks/current/block"),
        )
        .expect("Block path should be valid");
        let block_bytes = host
            .store_read_all(&block_path)
            .expect("Should read block bytes");
        let rlp = rlp::Rlp::new(&block_bytes);
        // TezBlock RLP layout: [version, hash, number, previous_hash, timestamp, operations]
        let previous_hash: H256 = rlp
            .at(3)
            .expect("Should decode previous_hash element")
            .as_val()
            .expect("Should decode previous_hash");
        assert_eq!(
            block_0_hash, previous_hash,
            "Block 1's previous_hash should be block 0's hash"
        );
    }

    /// Build a blueprint carrying Tezos operations wrapped as
    /// [`TransactionContent::TezosDelayed`] with an explicit timestamp.
    /// Mirrors the production path taken by delayed Tezos operations on
    /// the EVM chain config (see `apply.rs`), while letting a test pin the
    /// block timestamp observed by `NOW`.
    fn tezos_blueprint(operations: Vec<Operation>, timestamp: Timestamp) -> Blueprint {
        let transactions = operations
            .into_iter()
            .map(|op| {
                let tx_hash = op.hash().unwrap().into();
                Transaction {
                    tx_hash,
                    content: TransactionContent::TezosDelayed(op),
                }
            })
            .map(TezosXTransaction::from)
            .collect();
        Blueprint {
            transactions,
            timestamp,
        }
    }

    /// Ports the standalone `test_produce_tezlink_block_with_reveal_operation`
    /// to the TezosX (embedded Michelson runtime) path: a manager must go
    /// from `NotRevealed` to `Revealed` after a reveal operation is applied
    /// through the `produce` / `TezosDelayed` pipeline. Implicit-account
    /// state in TezosX lives in the RLP `/info` blob, so the manager is read
    /// back via `get_tezos_account_info`.
    #[test]
    fn test_tezosx_michelson_reveal_sets_manager() {
        use tezos_execution::account_storage::{
            get_tezos_account_info, set_tezos_account_info, TezosAccountInfo,
        };

        let mut host = MockKernelHost::default();
        // Disable DA fees so the low-fee reveal operation is not rejected.
        storage::store_da_fee(&mut host, U256::zero()).unwrap();

        // Allocate bootstrap2 in the Tezlink context so the SafeStorage
        // backup of TEZOS_ACCOUNTS_ROOT succeeds.
        context::implicit_from_public_key_hash(&bootstrap2().pkh)
            .expect("Account interface should be correct")
            .allocate(&mut host)
            .expect("Contract initialization should have succeeded");

        let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);
        let mut config = dummy_configuration();

        let bootstrap = bootstrap1();
        let pkh = bootstrap.pkh.clone();
        let expected_pk = bootstrap.pk.clone();

        // Seed bootstrap1 in the TezosX storage with a small balance and no
        // revealed public key.
        set_tezos_account_info(
            &mut host,
            &pkh,
            TezosAccountInfo {
                balance: U256::from(10000),
                nonce: 0,
                pub_key: None,
            },
        )
        .expect("Should have set bootstrap1 account info");

        // Before the reveal, the manager is not revealed.
        let info_before = get_tezos_account_info(&host, &pkh)
            .expect("account info read should succeed")
            .expect("bootstrap1 should be allocated");
        assert_eq!(
            info_before.pub_key, None,
            "manager must be unrevealed before the reveal operation"
        );

        let reveal = make_reveal_operation(1000, 1, 500, 0, bootstrap);
        store_blueprints(
            &mut host,
            vec![tezos_blueprint(vec![reveal], Timestamp::from(0i64))],
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);

        // After the reveal, the manager is the bootstrap's public key.
        let info_after = get_tezos_account_info(&host, &pkh)
            .expect("account info read should succeed")
            .expect("bootstrap1 should still be allocated");
        assert_eq!(
            info_after.pub_key,
            Some(expected_pk),
            "manager must be revealed to bootstrap1's public key after the reveal"
        );
    }

    /// Ports the standalone `test_produce_tezlink_block_with_reveal_and_transfer`
    /// to the TezosX path: bootstrap1 reveals its manager and transfers 35
    /// mutez to a fresh bootstrap2. Asserts the exact post-execution balances
    /// of both implicit accounts (read from the TezosX `/info` blob) and that
    /// the manager is revealed.
    #[test]
    fn test_tezosx_michelson_reveal_and_transfer_balances() {
        use tezos_execution::account_storage::{
            get_tezos_account_info, set_tezos_account_info, TezosAccountInfo,
        };

        let mut host = MockKernelHost::default();
        // Disable DA fees so the low-fee operations are not rejected.
        storage::store_da_fee(&mut host, U256::zero()).unwrap();

        let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);
        let mut config = dummy_configuration();

        let bootstrap1 = bootstrap1();
        let src_pkh = bootstrap1.pkh.clone();
        let expected_pk = bootstrap1.pk.clone();

        let bootstrap2 = bootstrap2();
        let dst_pkh = bootstrap2.pkh.clone();

        // Allocate bootstrap2 in the Tezlink context so the SafeStorage
        // backup of TEZOS_ACCOUNTS_ROOT succeeds.
        // (bootstrap2's TezosX balance is established below.)
        context::implicit_from_public_key_hash(&dst_pkh)
            .expect("Account interface should be correct")
            .allocate(&mut host)
            .expect("Contract initialization should have succeeded");

        // Seed bootstrap1 with enough mutez to cover both fees and the
        // transfer. With DA fees disabled and the destination already
        // present in the TezosX storage, there is no slot burn:
        // bootstrap1 pays 1000 (reveal fee) + 1000 (transfer fee) + 35 (transfer).
        let initial_balance = 10000_u64;
        set_tezos_account_info(
            &mut host,
            &src_pkh,
            TezosAccountInfo {
                balance: U256::from(initial_balance),
                nonce: 0,
                pub_key: None,
            },
        )
        .expect("Should have set bootstrap1 account info");

        // bootstrap2 starts with a zero balance.
        set_tezos_account_info(
            &mut host,
            &dst_pkh,
            TezosAccountInfo {
                balance: U256::zero(),
                nonce: 0,
                pub_key: None,
            },
        )
        .expect("Should have set bootstrap2 account info");

        // bootstrap1 is not revealed yet.
        let info_before = get_tezos_account_info(&host, &src_pkh)
            .expect("account info read should succeed")
            .expect("bootstrap1 should be allocated");
        assert_eq!(info_before.pub_key, None);

        let reveal_fee = 1000_u64;
        let transfer_fee = 1000_u64;
        let transfer_amount = 35_u64;

        let reveal = make_reveal_operation(reveal_fee, 1, 500, 0, bootstrap1.clone());
        let transfer = make_transaction_operation(
            transfer_fee,
            2,
            3000,
            0,
            bootstrap1,
            transfer_amount.into(),
            Contract::Implicit(dst_pkh.clone()),
            Parameters::default(),
        );

        store_blueprints(
            &mut host,
            vec![tezos_blueprint(
                vec![reveal, transfer],
                Timestamp::from(0i64),
            )],
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);

        // bootstrap1 should be revealed.
        let src_info = get_tezos_account_info(&host, &src_pkh)
            .expect("account info read should succeed")
            .expect("bootstrap1 should be allocated");
        assert_eq!(
            src_info.pub_key,
            Some(expected_pk),
            "bootstrap1 manager must be revealed after the reveal operation"
        );

        // bootstrap1 paid reveal fee + transfer fee + transfer amount.
        let expected_src_balance =
            initial_balance - reveal_fee - transfer_fee - transfer_amount;
        assert_eq!(
            src_info.balance,
            U256::from(expected_src_balance),
            "bootstrap1 balance must reflect fees and the transferred amount"
        );

        // bootstrap2 received exactly the transferred amount.
        let dst_info = get_tezos_account_info(&host, &dst_pkh)
            .expect("account info read should succeed")
            .expect("bootstrap2 should be allocated");
        assert_eq!(
            dst_info.balance,
            U256::from(transfer_amount),
            "bootstrap2 balance must equal the transferred amount"
        );
    }

    /// Regression (L2-1748): delayed native operations are branch-checked only
    /// at delayed-inbox entry, not at inclusion. A transfer whose branch is a
    /// foreign hash (not a recent block of this instance) that reaches inclusion
    /// via the delayed inbox is still applied, so a branch aging out of the
    /// window cannot drop a force-included operation. The entry check itself is
    /// covered by `delayed_inbox::tests::test_delayed_inbox_tezos_dropped_on_stale_branch`.
    #[test]
    fn test_tezosx_delayed_op_foreign_branch_applied_at_inclusion() {
        use tezos_execution::account_storage::{
            get_tezos_account_info, set_tezos_account_info, TezosAccountInfo,
        };

        let mut host = MockKernelHost::default();
        storage::store_da_fee(&mut host, U256::zero()).unwrap();

        let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);
        let mut config = dummy_configuration();

        let bootstrap1 = bootstrap1();
        let src_pkh = bootstrap1.pkh.clone();
        let bootstrap2 = bootstrap2();
        let dst_pkh = bootstrap2.pkh.clone();

        context::implicit_from_public_key_hash(&dst_pkh)
            .expect("Account interface should be correct")
            .allocate(&mut host)
            .expect("Contract initialization should have succeeded");

        let initial_balance = 10000_u64;
        set_tezos_account_info(
            &mut host,
            &src_pkh,
            TezosAccountInfo {
                balance: U256::from(initial_balance),
                nonce: 0,
                pub_key: None,
            },
        )
        .expect("Should have set bootstrap1 account info");
        set_tezos_account_info(
            &mut host,
            &dst_pkh,
            TezosAccountInfo {
                balance: U256::zero(),
                nonce: 0,
                pub_key: None,
            },
        )
        .expect("Should have set bootstrap2 account info");

        let transfer_amount = 35_u64;
        let reveal = make_reveal_operation(1000, 1, 500, 0, bootstrap1.clone());
        let mut transfer = make_transaction_operation(
            1000,
            2,
            3000,
            0,
            bootstrap1.clone(),
            transfer_amount.into(),
            Contract::Implicit(dst_pkh.clone()),
            Parameters::default(),
        );
        // Re-branch the transfer on a foreign hash (not genesis, not a block of
        // this instance) and re-sign, so only its branch is stale.
        let foreign_branch = tezos_crypto_rs::hash::BlockHash::from([0x11u8; 32]);
        transfer.signature =
            sign_operation(&bootstrap1.sk, &foreign_branch, &transfer.content).unwrap();
        transfer.branch = foreign_branch;

        store_blueprints(
            &mut host,
            vec![tezos_blueprint(
                vec![reveal, transfer],
                Timestamp::from(0i64),
            )],
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);

        // The foreign-branch transfer was force-included and applied.
        let dst_info = get_tezos_account_info(&host, &dst_pkh)
            .expect("account info read should succeed")
            .expect("bootstrap2 should be allocated");
        assert_eq!(
            dst_info.balance,
            U256::from(transfer_amount),
            "delayed foreign-branch transfer must be applied at inclusion"
        );
    }

    /// Ports the standalone `test_tezlink_level_now_chain_id_instructions` to
    /// the TezosX path: originate a Michelson contract whose code records the
    /// chain id, level and timestamp at the time of a call, then call it and
    /// assert the resulting contract storage. Originated (KT1) contracts share
    /// the unified `/tez/tez_accounts/contracts/` path in both modes, so the
    /// storage is read back through `context::originated_from_contract`.
    /// The chain id the contract observes is the `michelson_runtime_chain_id`
    /// (1u32 LE = 0x01000000).
    #[test]
    fn test_tezosx_michelson_chain_id_now_level() {
        use mir::gas::Gas;
        use tezos_execution::account_storage::{
            set_tezos_account_info, TezosAccountInfo,
        };

        let mut host = MockKernelHost::default();
        // Disable DA fees so the test operations are not rejected.
        storage::store_da_fee(&mut host, U256::zero()).unwrap();

        // Allocate bootstrap2 in the Tezlink context so the SafeStorage
        // backup of TEZOS_ACCOUNTS_ROOT succeeds.
        context::implicit_from_public_key_hash(&bootstrap2().pkh)
            .expect("Account interface should be correct")
            .allocate(&mut host)
            .expect("Contract initialization should have succeeded");

        let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);
        let mut config = dummy_configuration();

        // A contract storing the chain id, the level and the timestamp of the
        // last call.
        let parser = mir::parser::Parser::new();
        let code = parser
            .parse_top_level(
                "
                   parameter unit;
                   storage (pair nat timestamp chain_id);
                   code { DROP; CHAIN_ID; NOW; LEVEL; PAIR 3; NIL operation; PAIR }
            ",
            )
            .expect("Should have succeeded to parse the script")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let storage = parser
            .parse("Pair 0 0 0x00000000")
            .expect("Should have succeeded to parse the storage")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        let bootstrap1 = bootstrap1();
        let src_pkh = bootstrap1.pkh.clone();

        // Seed bootstrap1 with mutez and a revealed manager so it can
        // originate and call without a separate reveal operation.
        set_tezos_account_info(
            &mut host,
            &src_pkh,
            TezosAccountInfo {
                balance: U256::from(500_000u64),
                nonce: 0,
                pub_key: Some(bootstrap1.pk.clone()),
            },
        )
        .expect("Should have set bootstrap1 account info");

        let origination = make_origination_operation(
            1000,
            1,
            5000,
            500,
            bootstrap1.clone(),
            35_u64.into(),
            code,
            storage,
        );

        // Address generated by the origination (index 0, L1-canonical)
        let generated_contract = Contract::Originated(
            mir::interpreter::compute_contract_address(&origination.hash().unwrap(), 0),
        );

        let call = make_transaction_operation(
            1000,
            2,
            2000,
            300,
            bootstrap1,
            0.into(),
            generated_contract.clone(),
            Parameters::default(),
        );

        let timestamp_of_call = 10i64;
        store_blueprints(
            &mut host,
            vec![
                tezos_blueprint(vec![origination], Timestamp::from(0i64)),
                tezos_blueprint(vec![call], Timestamp::from(timestamp_of_call)),
            ],
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        // Block 0 (origination), block 1 (call), then drain.
        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);

        let expected_level = 1;
        assert_eq!(
            U256::from(expected_level),
            read_current_number(&mut host).unwrap()
        );
        let expected_timestamp = timestamp_of_call;
        // The chain id observed by the contract is the michelson runtime
        // chain id, set to 1u32 little-endian by
        // `dummy_tezosx_config_with_tezos_runtime`.
        let expected_chain_id = "0x01000000";
        let expected_storage = format!(
            "Pair {expected_level} (Pair {expected_timestamp} {expected_chain_id})"
        );

        let stored = context::originated_from_contract(&generated_contract)
            .expect("originated account interface should be correct")
            .storage(&host)
            .expect("contract storage should be readable");

        assert_eq!(
            parser.parse(&expected_storage).unwrap(),
            mir::ast::Micheline::decode_raw(&parser.arena, &stored, &mut Gas::default(),)
                .unwrap()
                .unwrap(),
            "contract storage must hold the call's (level, timestamp, chain_id)"
        )
    }

    /// L2-1727: a delayed-inbox Tezos operation is subject to L1's per-block
    /// internal-operation cap *cumulatively*. Delayed handling now lives in
    /// [`TezosXChainConfig::apply_transaction`] (chains.rs), which derives the
    /// block's prior internal-op count from
    /// [`BlockInProgress::cumulative_tezos_operation_receipts`] and threads it
    /// into the Michelson `BlockCtx`, so a sub-operation crossing the cap fails
    /// the delayed op *in isolation* (backtracked) instead of being applied.
    ///
    /// Fail-on-revert guard: with the base forced back to 0 the call below
    /// stays `Applied` even when seeded at 65_535, so this test goes red.
    #[test]
    fn delayed_internal_op_cap_is_cumulative() {
        use crate::apply::tests::{dummy_crac_receipt, make_transfer};
        use crate::apply::RuntimeExecutionInfo;
        use crate::block_in_progress::BlockInProgress;
        use mir::gas::Gas;
        use mir::interpreter::{compute_contract_address, MAX_INTERNAL_OPERATIONS};
        use std::collections::VecDeque;
        use tezos_execution::account_storage::{
            set_tezos_account_info, TezosAccountInfo,
        };
        use tezos_tezlink::block::OperationsWithReceipts;
        use tezos_tezlink::operation_result::{
            ContentResult, OperationDataAndMetadata, OperationResult, OperationResultSum,
        };

        // Apply, via the real delayed-inbox dispatch
        // ([`TezosXChainConfig::apply_transaction`]), a call to a contract that
        // EMITs exactly one internal operation, with the block's prior
        // internal-op count seeded to `base`. Returns whether the top-level
        // operation was `Applied`.
        let run = |base: u128| {
            let mut host = MockKernelHost::default();
            storage::store_da_fee(&mut host, U256::zero()).unwrap();
            store_block_fees(&mut host, &dummy_block_fees()).unwrap();

            // Allocate bootstrap2 so the SafeStorage backup of the Tezos
            // accounts root succeeds.
            context::implicit_from_public_key_hash(&bootstrap2().pkh)
                .unwrap()
                .allocate(&mut host)
                .unwrap();

            let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);
            let mut config = dummy_configuration();

            let bootstrap = bootstrap1();
            set_tezos_account_info(
                &mut host,
                &bootstrap.pkh,
                TezosAccountInfo {
                    balance: U256::from(500_000u64),
                    nonce: 0,
                    pub_key: Some(bootstrap.pk.clone()),
                },
            )
            .unwrap();

            // A contract that emits exactly one internal Event operation.
            let parser = mir::parser::Parser::new();
            let code = parser
                .parse_top_level(
                    "parameter unit; storage unit;
                     code { CDR; PUSH string \"ping\"; EMIT %ping string;
                            NIL operation; SWAP; CONS; PAIR }",
                )
                .unwrap()
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap();
            let storage = parser
                .parse("Unit")
                .unwrap()
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap();

            // Originate the contract (block 0) through the production pipeline.
            // The origination is fee-checked by [produce], so it must cover the
            // execution fees a Tezos operation now pays.
            let origination = make_origination_operation(
                1000,
                1,
                5000,
                500,
                bootstrap.clone(),
                0_u64.into(),
                code,
                storage,
            );
            let emitter = Contract::Originated(compute_contract_address(
                &origination.hash().unwrap(),
                0,
            ));
            store_blueprints(
                &mut host,
                vec![tezos_blueprint(vec![origination], Timestamp::from(0i64))],
            );
            produce(&mut host, &chain_config, &mut config, None, None)
                .expect("origination block must be produced");

            // Build the delayed call to the emitter (counter 2: after the
            // origination at counter 1).
            let call = make_transaction_operation(
                1,
                2,
                5000,
                300,
                bootstrap,
                0.into(),
                emitter,
                Parameters::default(),
            );
            let transaction = TezosXTransaction::Ethereum(Box::new(Transaction {
                tx_hash: call.hash().unwrap().into(),
                content: TransactionContent::TezosDelayed(call),
            }));

            let mut block_constants = first_block(&mut host);
            block_constants.michelson_runtime_block_constants.safe_roots = vec![
                TEZ_SAFE_STORAGE_ROOT_PATH.into(),
                TEZOS_ACCOUNTS_ROOT.into(),
            ];

            // Seed the block's prior internal-op count to `base`: a single
            // synthetic applied operation carrying `base` internal results is
            // exactly what [`apply::count_internal_operations`] tallies when
            // the delayed dispatch derives the cumulative base.
            let mut block_in_progress = BlockInProgress::new(
                block_constants.evm_runtime_block_constants.number,
                VecDeque::new(),
                U256::from(MINIMUM_BASE_FEE_PER_GAS),
            );
            block_in_progress.cumulative_tezos_operation_receipts =
                OperationsWithReceipts {
                    list: vec![dummy_crac_receipt(vec![
                        make_transfer(0, 0);
                        base as usize
                    ])],
                };

            let registry = RegistryImpl::default();
            let outbox_queue =
                OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX).unwrap();

            // skip_fees_check keeps the assertion focused on the internal-op
            // cap; fee payment on the delayed path is covered elsewhere.
            let result = chain_config
                .apply_transaction(
                    &block_in_progress,
                    &mut host,
                    &registry,
                    &outbox_queue,
                    &block_constants,
                    transaction,
                    0,
                    None,
                    None,
                    false, // skip_signature_check
                    true,  // skip_fees_check
                    false, // http_trace_enabled
                )
                .expect("apply_transaction must not raise a kernel error");

            let info = match result {
                ExecutionResult::Valid(info) => info,
                ExecutionResult::Invalid => panic!("delayed op must be valid"),
            };
            let RuntimeExecutionInfo::Tezos { op, .. } = info else {
                panic!("delayed op must yield Tezos execution info");
            };
            let OperationDataAndMetadata::OperationWithMetadata(batch) =
                op.op_and_receipt;
            matches!(
                batch.operations[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            )
        };

        // Just below the cap: the EMIT takes a counter under the limit and the
        // delayed op is applied.
        assert!(
            run(MAX_INTERNAL_OPERATIONS - 1),
            "below the cap the delayed op must be applied"
        );
        // At the cap: the EMIT would allocate the limiting counter, so the
        // delayed op fails in isolation (backtracked), not applied.
        assert!(
            !run(MAX_INTERNAL_OPERATIONS),
            "at the cap the delayed op must fail in isolation"
        );
    }

    #[test]
    // Test if the invalid transactions are producing receipts
    fn test_invalid_transactions_receipt_status() {
        let mut host = MockKernelHost::default();
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        let invalid_tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_zero()),
        };

        let transactions: Vec<Transaction> = vec![invalid_tx];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        set_balance(&mut host, &sender, U256::from(30000u64));
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");

        assert!(
            read_transaction_receipt_status(&mut host, &tx_hash).is_err(),
            "Invalid transaction should not have a receipt"
        );
    }

    #[test]
    // Test if a valid transaction is producing a receipt with a success status
    fn test_valid_transactions_receipt_status() {
        let mut host = MockKernelHost::default();
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        let valid_tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_zero()),
        };

        let transactions: Vec<Transaction> = vec![valid_tx];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        set_balance(&mut host, &sender, U256::from(1_000_000_000_000_000_000u64));
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");

        let status = read_transaction_receipt_status(&mut host, &tx_hash)
            .expect("Should have found receipt");
        assert_eq!(TransactionStatus::Success, status);
    }

    #[test]
    // Test if a valid transaction is producing a receipt with a contract address
    fn test_valid_transactions_receipt_contract_address() {
        let mut host = MockKernelHost::default();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];
        let tx = dummy_eth_transaction_deploy();
        assert_eq!(
            H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap(),
            tx.caller().unwrap()
        );
        let valid_tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_deploy()),
        };

        let transactions: Vec<Transaction> = vec![valid_tx];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        set_balance(&mut host, &sender, U256::from(5000000000000000u64));
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");
        let receipt = read_transaction_receipt(&mut host, &tx_hash)
            .expect("should have found receipt");
        assert_eq!(TransactionStatus::Success, receipt.status);
        assert_eq!(
            H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap(),
            receipt.from
        );
        assert_eq!(
            Some(H160::from_str("d9d427235f5746ffd1d5a0d850e77880a94b668f").unwrap()),
            receipt.contract_address
        );
    }

    #[test]
    // Test if several valid transactions can be performed
    fn test_several_valid_transactions() {
        let mut host = MockKernelHost::default();
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        produce_block_with_several_valid_txs(&mut host);

        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let dest_balance = get_balance(&mut host, &dest_address);

        assert_eq!(dest_balance, U256::from(1000000000u64))
    }

    #[test]
    // Test if several valid proposals can produce valid blocks
    fn test_several_valid_proposals() {
        let mut host = MockKernelHost::default();
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash_0 = [0; TRANSACTION_HASH_SIZE];
        let tx_hash_1 = [1; TRANSACTION_HASH_SIZE];

        let transaction_0 = vec![Transaction {
            tx_hash: tx_hash_0,
            content: Ethereum(dummy_eth_transaction_zero()),
        }];

        let transaction_1 = vec![Transaction {
            tx_hash: tx_hash_1,
            content: Ethereum(dummy_eth_transaction_one()),
        }];

        store_blueprints(
            &mut host,
            vec![blueprint(transaction_0), blueprint(transaction_1)],
        );

        let sender = dummy_eth_caller();
        set_balance(&mut host, &sender, U256::from(10000000000000000000u64));
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        // Produce block for blueprint containing transaction_0
        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");
        // Produce block for blueprint containing transaction_1
        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");

        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let dest_balance = get_balance(&mut host, &dest_address);

        assert_eq!(dest_balance, U256::from(1000000000u64))
    }

    #[test]
    // Test transfers gas consumption consistency
    fn test_cumulative_transfers_gas_consumption() {
        let mut host = MockKernelHost::default();

        let base_gas = U256::from(21000);
        let dummy_block_fees = dummy_block_fees();
        let gas_for_fees = crate::fees::gas_for_fees(
            dummy_block_fees.da_fee_per_byte(),
            dummy_block_fees.base_fee_per_gas(),
            &[],
            &[],
            0,
        )
        .unwrap();

        let tx_hash_0 = [0; TRANSACTION_HASH_SIZE];
        let tx_hash_1 = [1; TRANSACTION_HASH_SIZE];

        let transactions = vec![
            Transaction {
                tx_hash: tx_hash_0,
                content: Ethereum(dummy_eth_transaction_zero()),
            },
            Transaction {
                tx_hash: tx_hash_1,
                content: Ethereum(dummy_eth_transaction_one()),
            },
        ];

        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        set_balance(&mut host, &sender, U256::from(10000000000000000000u64));
        store_block_fees(&mut host, &dummy_block_fees).unwrap();

        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");
        let receipt0 = read_transaction_receipt(&mut host, &tx_hash_0)
            .expect("should have found receipt");
        let receipt1 = read_transaction_receipt(&mut host, &tx_hash_1)
            .expect("should have found receipt");

        assert_eq!(receipt0.cumulative_gas_used, base_gas + gas_for_fees);
        assert_eq!(
            receipt1.cumulative_gas_used,
            receipt0.cumulative_gas_used + base_gas + gas_for_fees
        );
    }

    #[test]
    // Test if we're able to read current block (with a filled queue) after
    // a block production
    fn test_read_storage_current_block_after_block_production_with_filled_queue() {
        let mut host = MockKernelHost::default();

        produce_block_with_several_valid_txs(&mut host);

        assert_current_block_reading_validity(&mut host);
    }

    #[test]
    // Test that the same transaction can not be replayed twice
    fn test_replay_attack() {
        let mut host = MockKernelHost::default();

        let tx = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: Ethereum(dummy_eth_transaction_zero()),
        };

        let transactions = vec![tx.clone(), tx];
        store_blueprints(
            &mut host,
            vec![blueprint(transactions.clone()), blueprint(transactions)],
        );

        let sender = dummy_eth_caller();
        let initial_sender_balance = U256::from(10000000000000000000u64);
        set_balance(&mut host, &sender, initial_sender_balance);
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");

        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let sender_balance = get_balance(&mut host, &sender);
        let dest_balance = get_balance(&mut host, &dest_address);

        let expected_dest_balance = U256::from(500000000u64);
        let expected_gas = 21000;
        let da_fee = crate::fees::da_fee(DUMMY_DA_FEE.into(), &[], &[], 0);
        let expected_fees = dummy_block_fees().base_fee_per_gas() * expected_gas + da_fee;
        let expected_sender_balance =
            initial_sender_balance - expected_dest_balance - expected_fees;

        assert_eq!(dest_balance, expected_dest_balance);
        assert_eq!(sender_balance, expected_sender_balance, "sender balance");
    }

    fn first_block<MockHost>(host: &mut MockHost) -> TezosXBlockConstants
    where
        MockHost: StorageV1 + KeySpaceLoader,
    {
        let timestamp =
            read_last_info_per_level_timestamp(host).unwrap_or(Timestamp::from(0));
        let timestamp = U256::from(timestamp.as_u64());
        let evm_chain_id = fetch_evm_chain_id(host);
        let block_fees = retrieve_block_fees(host);
        assert!(block_fees.is_ok(), "block fees should be defined");
        TezosXBlockConstants {
            evm_runtime_block_constants: BlockConstants::first_block(
                timestamp,
                evm_chain_id,
                block_fees.unwrap(),
                crate::block::GAS_LIMIT,
                H160::zero(),
            ),
            michelson_runtime_block_constants: TezlinkBlockConstants {
                level: (0.into()),
                da_fee_per_byte_mutez: 0,
                michelson_to_evm_gas_multiplier: DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER,
                safe_roots: vec![],
            },
        }
    }

    #[test]
    fn test_stop_computation() {
        // init host
        let mut host = MockKernelHost::default();
        let registry = RegistryImpl::default();
        let block_constants = first_block(&mut host);

        //provision sender account
        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        set_balance(&mut host, &sender, U256::from(10000000000000000000u64));

        // tx is valid because correct nonce and account provisionned
        let valid_tx = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: TransactionContent::Ethereum(dummy_eth_transaction_zero()),
        }
        .into();
        let transactions = vec![valid_tx].into();

        // init block in progress
        let mut block_in_progress = BlockInProgress::new(
            U256::from(1),
            transactions,
            block_constants
                .evm_runtime_block_constants
                .block_fees
                .base_fee_per_gas(),
        );
        // run is almost full wrt gas consumption in the current run
        let limits = EvmLimits::default();
        let cumulative_gas_in_run = max_gas_per_reboot(&limits) - 1000;
        host.add_execution_gas(cumulative_gas_in_run);

        let chain_config = dummy_tezosx_config(SpecId::default());

        // act
        let result = compute(
            &mut host,
            &registry,
            &chain_config,
            &OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX).unwrap(),
            &mut block_in_progress,
            &block_constants,
            None,
            None,
            false,
        )
        .expect("Should safely ask for a reboot");

        // assert

        assert_eq!(
            result,
            BlockInProgressComputationResult::RebootNeeded,
            "Should have asked for a reboot"
        );
        // block in progress should not have registered any gas or ticks
        assert_eq!(
            block_in_progress.cumulative_gas,
            U256::from(0),
            "should not have consumed any gas"
        );
        assert_eq!(
            host.executed_gas(),
            cumulative_gas_in_run,
            "should not have consumed any gas"
        );

        // the transaction should not have been processed
        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let sender_balance = get_balance(&mut host, &sender);
        let dest_balance = get_balance(&mut host, &dest_address);
        assert_eq!(sender_balance, U256::from(10000000000000000000u64));
        assert_eq!(dest_balance, U256::from(0u64))
    }

    #[test]
    fn test_two_tezos_ops_second_rejected_after_first_consumed_budget() {
        // Regression test for the consumption-tracking half of the
        // `Tezos X: Fix can_fit_in_reboot for the Michelson runtime`
        // MR. The Michelson branch of `register_valid_transaction`
        // must call `host.add_execution_gas(...)`; otherwise the
        // host's per-run counter never advances on Tezos ops and a
        // stream of medium-sized ops slips past the per-reboot budget.
        //
        // Setup: queue two Tezos ops sized so that exactly one fits
        // in the run's remaining capacity (against its declared
        // gas_limit). After op1 applies, its actual consumption must
        // push `host.executed_gas()` over the threshold so op2 is
        // rejected and repushed. Revert `host.add_execution_gas` and
        // op2 instead sees the original (still-fitting) counter and
        // is incorrectly applied — this test fails.
        use tezos_execution::account_storage::{
            set_tezos_account_info, TezosAccountInfo,
        };

        let mut host = MockKernelHost::default();

        // Allocate bootstrap2 in Tezlink storage so the SafeStorage
        // backup of TEZOS_ACCOUNTS_ROOT succeeds,
        // mirroring `test_tezblock_stored_after_tezos_operation`.
        context::implicit_from_public_key_hash(&bootstrap2().pkh)
            .expect("Account interface should be correct")
            .allocate(&mut host)
            .expect("Contract initialization should have succeeded");

        let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);
        let registry = RegistryImpl::default();
        let mut block_constants = first_block(&mut host);
        // Match production safe_roots so SafeStorage::start/revert in
        // `validate_and_apply_operation` operates on initialised paths.
        block_constants.michelson_runtime_block_constants.safe_roots = vec![
            TEZ_SAFE_STORAGE_ROOT_PATH.into(),
            TEZOS_ACCOUNTS_ROOT.into(),
        ];

        // Allocate bootstrap1 in TezosX storage with enough balance to
        // cover fees for both ops.
        let bootstrap = bootstrap1();
        set_tezos_account_info(
            &mut host,
            &bootstrap.pkh,
            TezosAccountInfo {
                balance: U256::from(1_000_000),
                nonce: 0,
                pub_key: None,
            },
        )
        .expect("Should have set bootstrap1 account info");

        // Two reveal ops with sequential counters. Each declared
        // gas_limit of 5_000 michelson maps to `5_000 * multiplier`
        // EVM gas via `michelson_gas_to_evm_gas`. The second op would
        // fail validation (account already revealed by op1), but it
        // never reaches apply — it gets repushed by
        // `can_fit_in_reboot`.
        //
        // `fee` must cover the required execution gas fees (which
        // scale with the multiplier), otherwise op1 is dropped for
        // insufficient fees and never advances the gas counter.
        let michelson_gas_limit = 5_000_u64;
        let fee = 10_000_u64;
        let evm_gas_limit_per_op =
            michelson_gas_limit * DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER;

        let op1 =
            make_reveal_operation(fee, 1, michelson_gas_limit, 0, bootstrap.clone());
        let tx_hash_1 = op1.hash().unwrap().into();
        let tezos_tx_1 = TezosXTransaction::Tezos(TezlinkOperation {
            tx_hash: tx_hash_1,
            content: TezlinkContent::Tezos(op1),
        });

        let op2 = make_reveal_operation(fee, 2, michelson_gas_limit, 0, bootstrap);
        let tx_hash_2 = op2.hash().unwrap().into();
        let tezos_tx_2 = TezosXTransaction::Tezos(TezlinkOperation {
            tx_hash: tx_hash_2,
            content: TezlinkContent::Tezos(op2),
        });

        let transactions = vec![tezos_tx_1, tezos_tx_2].into();
        let mut block_in_progress = BlockInProgress::new(
            U256::from(1),
            transactions,
            block_constants
                .evm_runtime_block_constants
                .block_fees
                .base_fee_per_gas(),
        );

        // Headroom = exactly op1's declared limit. Op1 fits at the
        // edge; once its actual consumption advances the host
        // counter, op2's declared limit no longer fits.
        let limits = EvmLimits::default();
        let cumulative_gas_in_run = max_gas_per_reboot(&limits) - evm_gas_limit_per_op;
        host.add_execution_gas(cumulative_gas_in_run);

        let result = compute(
            &mut host,
            &registry,
            &chain_config,
            &OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX).unwrap(),
            &mut block_in_progress,
            &block_constants,
            None,
            None,
            false,
        )
        .expect("compute should not error");

        assert_eq!(
            result,
            BlockInProgressComputationResult::RebootNeeded,
            "second op must trigger a reboot once op1's consumption is \
             reflected in host.executed_gas()"
        );
        assert_eq!(
            block_in_progress.queue_length(),
            1,
            "the second op should have been re-pushed"
        );
        assert_eq!(
            block_in_progress.michelson_index, 1,
            "exactly one Tezos op should have been applied"
        );
        assert!(
            host.executed_gas() > cumulative_gas_in_run,
            "host gas counter must advance from op1's Tezos consumption \
             — this assertion fails if `host.add_execution_gas(...)` is \
             missing from the Michelson branch of \
             `register_valid_transaction`. Got executed_gas = {}, \
             started at {}",
            host.executed_gas(),
            cumulative_gas_in_run,
        );
    }

    #[test]
    fn invalid_transaction_should_bump_nonce() {
        let mut host = MockKernelHost::default();

        let caller =
            address_from_str("f95abdf6ede4c3703e0e9453771fbee8592d31e9").unwrap();

        // Get the balance before the transaction, i.e. 0.
        let caller_account =
            StorageAccount::from_address(&h160_to_alloy(&caller)).unwrap();
        let info = caller_account.info(&mut host).unwrap();
        let default_nonce = info.nonce;
        assert_eq!(default_nonce, 0, "default nonce should be 0");

        let tx = dummy_eth_transaction_zero();
        // Ensures the caller has enough balance to pay for the fees, but not
        // the transaction itself, otherwise the transaction will not even be
        // taken into account.
        let fees = U256::from(21000) * tx.gas_limit_with_fees();
        set_balance(&mut host, &caller, fees);

        // Prepare a invalid transaction, i.e. with not enough funds.
        let tx_hash = [0; TRANSACTION_HASH_SIZE];
        let transaction = Transaction {
            tx_hash,
            content: Ethereum(tx),
        };
        store_blueprints(&mut host, vec![blueprint(vec![transaction])]);

        // Apply the transaction
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");
        assert!(
            read_transaction_receipt(&mut host, &tx_hash).is_err(),
            "Transaction is invalid, so should not have a receipt"
        );

        // Nonce should not have been bumped
        let info = caller_account.info(&mut host).unwrap();
        let nonce = info.nonce;
        assert_eq!(nonce, default_nonce, "nonce should not have been bumped");
    }

    /// A blueprint that should produce 1 block with an invalid transaction
    fn almost_empty_blueprint() -> Blueprint {
        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        // transaction should be invalid
        let tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_one()),
        };

        let transactions = vec![tx];

        blueprint(transactions)
    }

    fn check_current_block_number<Host>(host: &mut Host, nb: usize)
    where
        Host: StorageV1 + KeySpaceLoader,
    {
        let current_nb =
            read_current_number(host).expect("Should have manage to check block number");
        assert_eq!(current_nb, U256::from(nb), "Incorrect block number");
    }

    #[test]
    fn test_first_blocks() {
        let mut host = MockKernelHost::default();
        // TezosXChainConfig::storage_root_paths lists EVM_ETH_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
        // so SafeStorage::start()'s store_copy needs each safe root to exist.
        init_safe_storage_roots(&mut host);

        let chain_config = dummy_tezosx_config(SpecId::default());
        // first block should be 0
        let blueprint = almost_empty_blueprint();
        store_inbox_blueprint(
            &mut crate::storage::load_base_keyspace(&mut host).unwrap(),
            blueprint,
        )
        .expect("Should store a blueprint");
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        produce(
            &mut host,
            &chain_config,
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("Empty block should have been produced");
        check_current_block_number(&mut host, 0);

        // second block
        let blueprint = almost_empty_blueprint();
        store_inbox_blueprint(
            &mut crate::storage::load_base_keyspace(&mut host).unwrap(),
            blueprint,
        )
        .expect("Should store a blueprint");
        produce(
            &mut host,
            &chain_config,
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("Empty block should have been produced");
        check_current_block_number(&mut host, 1);

        // third block
        let blueprint = almost_empty_blueprint();
        store_inbox_blueprint(
            &mut crate::storage::load_base_keyspace(&mut host).unwrap(),
            blueprint,
        )
        .expect("Should store a blueprint");
        produce(
            &mut host,
            &chain_config,
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("Empty block should have been produced");
        check_current_block_number(&mut host, 2);
    }

    fn hash_from_nonce(nonce: u64) -> TransactionHash {
        let nonce = u64::to_le_bytes(nonce);
        let mut hash = [0; 32];
        hash[..8].copy_from_slice(&nonce);
        hash
    }

    const CREATE_LOOP_DATA: &str = "608060405234801561001057600080fd5b506101d0806100206000396000f3fe608060405234801561001057600080fd5b506004361061002b5760003560e01c80630b7d796e14610030575b600080fd5b61004a600480360381019061004591906100c2565b61004c565b005b60005b81811015610083576001600080828254610069919061011e565b92505081905550808061007b90610152565b91505061004f565b5050565b600080fd5b6000819050919050565b61009f8161008c565b81146100aa57600080fd5b50565b6000813590506100bc81610096565b92915050565b6000602082840312156100d8576100d7610087565b5b60006100e6848285016100ad565b91505092915050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052601160045260246000fd5b60006101298261008c565b91506101348361008c565b925082820190508082111561014c5761014b6100ef565b5b92915050565b600061015d8261008c565b91507fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff820361018f5761018e6100ef565b5b60018201905091905056fea26469706673582212200cd6584173dbec22eba4ce6cc7cc4e702e00e018d340f84fc0ff197faf980ad264736f6c63430008150033";

    const LOOP_300: &str =
        "0b7d796e000000000000000000000000000000000000000000000000000000000000012c";

    const TEST_SK: &str =
        "84e147b8bc36d99cc6b1676318a0635d8febc9f02897b0563ad27358589ee502";

    const TEST_ADDR: &str = "f0affc80a5f69f4a9a3ee01a640873b6ba53e539";

    fn create_and_sign_transaction(
        data: &str,
        nonce: u64,
        gas_limit: u64,
        to: Option<H160>,
        secret_key: &str,
    ) -> EthereumTransactionCommon {
        let data = hex::decode(data).unwrap();

        let gas_for_fees = crate::fees::gas_for_fees(
            DUMMY_DA_FEE.into(),
            DUMMY_BASE_FEE_PER_GAS.into(),
            &data,
            &[],
            0,
        )
        .unwrap();

        let unsigned_tx = EthereumTransactionCommon::new(
            TransactionType::Eip1559,
            Some(DUMMY_CHAIN_ID),
            nonce,
            U256::from(DUMMY_BASE_FEE_PER_GAS),
            U256::from(DUMMY_BASE_FEE_PER_GAS),
            gas_limit + gas_for_fees,
            to,
            U256::zero(),
            data,
            vec![],
            None,
            None,
        );
        unsigned_tx
            .sign_transaction(String::from(secret_key))
            .unwrap()
    }

    fn wrap_transaction(nonce: u64, tx: EthereumTransactionCommon) -> Transaction {
        Transaction {
            tx_hash: hash_from_nonce(nonce),
            content: TransactionContent::Ethereum(tx),
        }
    }

    #[test]
    fn test_reboot_many_tx_one_proposal() {
        // init host
        let mut host = MockKernelHost::default();

        // sanity check: no current block
        assert!(
            read_current_number(&mut host).is_err(),
            "Should not have found current block number"
        );

        //provision sender account
        let sender = H160::from_str(TEST_ADDR).unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        set_balance(&mut host, &sender, sender_initial_balance);

        // These transactions are generated with the loop.sol contract, which are:
        // - create the contract
        // - call `loop(300)`
        // - call `loop(300)`
        let create_transaction =
            create_and_sign_transaction(CREATE_LOOP_DATA, 0, 160_000, None, TEST_SK);
        let loop_addr: H160 = {
            let mut stream = rlp::RlpStream::new_list(2);
            stream.append(&sender);
            stream.append(&0u64);
            H256::from_slice(Keccak256::digest(stream.out()).as_slice()).into()
        };
        let loop_300_tx =
            create_and_sign_transaction(LOOP_300, 1, 230_000, Some(loop_addr), TEST_SK);
        let loop_300_tx2 =
            create_and_sign_transaction(LOOP_300, 2, 230_000, Some(loop_addr), TEST_SK);

        let proposals = vec![
            wrap_transaction(0, create_transaction),
            wrap_transaction(1, loop_300_tx),
            wrap_transaction(2, loop_300_tx2),
        ];

        store_blueprints(&mut host, vec![blueprint(proposals)]);

        host.reboot_left().expect("should be some reboot left");

        let mut chain_config = dummy_tezosx_config(SpecId::default());
        chain_config.limits_mut().maximum_gas_limit = 560_000;
        let mut configuration = dummy_configuration();

        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        let computation_result =
            produce(&mut host, &chain_config, &mut configuration, None, None)
                .expect("Should have produced");

        // test no new block
        assert!(
            read_current_number(&mut host).is_err(),
            "Should not have found current block number"
        );

        // test reboot is set
        matches!(computation_result, ComputationResult::RebootNeeded);

        // The block is in progress, therefore it is in the safe storage.
        let safe_host = SafeStorage {
            host: &mut host,
            world_states: chain_config
                .storage_root_paths(U256::zero())
                .iter()
                .map(OwnedPath::from)
                .collect(),
        };
        let bip = read_block_in_progress(&safe_host)
            .expect("Should be able to read the block in progress")
            .expect("The reboot context should have a block in progress");

        assert_eq!(
            bip.number,
            U256::zero(),
            "The block in progress should be number 0"
        );

        assert_eq!(bip.queue_length(), 1, "There should be a transaction left");

        assert_eq!(
            bip.valid_txs().len(),
            2,
            "Two transactions should have been already applied"
        );
    }

    #[test]
    fn test_reboot_many_tx_many_proposal() {
        // init host
        let mut host = MockKernelHost::default();

        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        // sanity check: no current block
        assert!(
            read_current_number(&mut host).is_err(),
            "Should not have found current block number"
        );
        //provision sender account
        let sender = H160::from_str(TEST_ADDR).unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        set_balance(&mut host, &sender, sender_initial_balance);

        // These transactions are generated with the loop.sol contract, which are:
        // - create the contract
        // - call `loop(300)`
        // - call `loop(300)`
        let create_transaction =
            create_and_sign_transaction(CREATE_LOOP_DATA, 0, 160_000, None, TEST_SK);
        let loop_addr: H160 = {
            let mut stream = rlp::RlpStream::new_list(2);
            stream.append(&sender);
            stream.append(&0u64);
            H256::from_slice(Keccak256::digest(stream.out()).as_slice()).into()
        };

        let loop_300_tx =
            create_and_sign_transaction(LOOP_300, 1, 230_000, Some(loop_addr), TEST_SK);
        let loop_300_tx2 =
            create_and_sign_transaction(LOOP_300, 2, 230_000, Some(loop_addr), TEST_SK);

        let proposals = vec![
            blueprint(vec![wrap_transaction(0, create_transaction)]),
            blueprint(vec![
                wrap_transaction(1, loop_300_tx),
                wrap_transaction(2, loop_300_tx2),
            ]),
        ];

        store_blueprints(&mut host, proposals);

        let mut chain_config = dummy_tezosx_config(SpecId::default());
        chain_config.limits_mut().maximum_gas_limit = 560_000;
        let mut configuration = dummy_configuration();

        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        let computation_result =
            produce(&mut host, &chain_config, &mut configuration, None, None)
                .expect("Should have produced");
        // test reboot is set
        matches!(computation_result, ComputationResult::RebootNeeded);

        let computation_result =
            produce(&mut host, &chain_config, &mut configuration, None, None)
                .expect("Should have produced");

        // test no new block
        assert_eq!(
            read_current_number(&mut host).expect("should have found a block number"),
            U256::zero(),
            "There should have been one block registered"
        );

        // test reboot is set again
        matches!(computation_result, ComputationResult::RebootNeeded);

        // The block is in progress, therefore it is in the safe storage.
        let safe_host = SafeStorage {
            host: &mut host,
            world_states: chain_config
                .storage_root_paths(U256::zero())
                .iter()
                .map(OwnedPath::from)
                .collect(),
        };
        let bip = read_block_in_progress(&safe_host)
            .expect("Should be able to read the block in progress")
            .expect("The reboot context should have a block in progress");

        assert_eq!(
            bip.number,
            U256::from(1),
            "The block in progress should be number 1"
        );

        assert_eq!(bip.queue_length(), 1, "There should be a transaction left");

        assert_eq!(
            bip.valid_txs().len(),
            1,
            "One transaction should have been already applied"
        );
    }

    #[test]
    fn test_transaction_pre_eip155() {
        // This test injects a presigned transaction defined by
        // https://github.com/mds1/multicall#new-deployments, which is a well
        // known contract on multiple EVM chains. The transaction has been
        // signed without a chain id, so that it can be reproduced on multiple
        // networks (and the contract address is the same on any chain).
        //
        // The purpose of this test is to check the kernel accepts such a
        // transaction, with the expected hash and the expected contract
        // address.

        // init host
        let mut host = MockKernelHost::default();

        // see
        // https://basescan.org/tx/0x07471adfe8f4ec553c1199f495be97fc8be8e0626ae307281c22534460184ed1
        // for example, as the transaction has the same hash on every EVM Chain.
        let expected_tx_hash = hex::decode(
            "07471adfe8f4ec553c1199f495be97fc8be8e0626ae307281c22534460184ed1",
        )
        .unwrap();

        // Extracted from https://github.com/mds1/multicall#new-deployments
        let signed_transaction = hex::decode("f90f538085174876e800830f42408080b90f00608060405234801561001057600080fd5b50610ee0806100206000396000f3fe6080604052600436106100f35760003560e01c80634d2301cc1161008a578063a8b0574e11610059578063a8b0574e1461025a578063bce38bd714610275578063c3077fa914610288578063ee82ac5e1461029b57600080fd5b80634d2301cc146101ec57806372425d9d1461022157806382ad56cb1461023457806386d516e81461024757600080fd5b80633408e470116100c65780633408e47014610191578063399542e9146101a45780633e64a696146101c657806342cbb15c146101d957600080fd5b80630f28c97d146100f8578063174dea711461011a578063252dba421461013a57806327e86d6e1461015b575b600080fd5b34801561010457600080fd5b50425b6040519081526020015b60405180910390f35b61012d610128366004610a85565b6102ba565b6040516101119190610bbe565b61014d610148366004610a85565b6104ef565b604051610111929190610bd8565b34801561016757600080fd5b50437fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0140610107565b34801561019d57600080fd5b5046610107565b6101b76101b2366004610c60565b610690565b60405161011193929190610cba565b3480156101d257600080fd5b5048610107565b3480156101e557600080fd5b5043610107565b3480156101f857600080fd5b50610107610207366004610ce2565b73ffffffffffffffffffffffffffffffffffffffff163190565b34801561022d57600080fd5b5044610107565b61012d610242366004610a85565b6106ab565b34801561025357600080fd5b5045610107565b34801561026657600080fd5b50604051418152602001610111565b61012d610283366004610c60565b61085a565b6101b7610296366004610a85565b610a1a565b3480156102a757600080fd5b506101076102b6366004610d18565b4090565b60606000828067ffffffffffffffff8111156102d8576102d8610d31565b60405190808252806020026020018201604052801561031e57816020015b6040805180820190915260008152606060208201528152602001906001900390816102f65790505b5092503660005b8281101561047757600085828151811061034157610341610d60565b6020026020010151905087878381811061035d5761035d610d60565b905060200281019061036f9190610d8f565b6040810135958601959093506103886020850185610ce2565b73ffffffffffffffffffffffffffffffffffffffff16816103ac6060870187610dcd565b6040516103ba929190610e32565b60006040518083038185875af1925050503d80600081146103f7576040519150601f19603f3d011682016040523d82523d6000602084013e6103fc565b606091505b50602080850191909152901515808452908501351761046d577f08c379a000000000000000000000000000000000000000000000000000000000600052602060045260176024527f4d756c746963616c6c333a2063616c6c206661696c656400000000000000000060445260846000fd5b5050600101610325565b508234146104e6576040517f08c379a000000000000000000000000000000000000000000000000000000000815260206004820152601a60248201527f4d756c746963616c6c333a2076616c7565206d69736d6174636800000000000060448201526064015b60405180910390fd5b50505092915050565b436060828067ffffffffffffffff81111561050c5761050c610d31565b60405190808252806020026020018201604052801561053f57816020015b606081526020019060019003908161052a5790505b5091503660005b8281101561068657600087878381811061056257610562610d60565b90506020028101906105749190610e42565b92506105836020840184610ce2565b73ffffffffffffffffffffffffffffffffffffffff166105a66020850185610dcd565b6040516105b4929190610e32565b6000604051808303816000865af19150503d80600081146105f1576040519150601f19603f3d011682016040523d82523d6000602084013e6105f6565b606091505b5086848151811061060957610609610d60565b602090810291909101015290508061067d576040517f08c379a000000000000000000000000000000000000000000000000000000000815260206004820152601760248201527f4d756c746963616c6c333a2063616c6c206661696c656400000000000000000060448201526064016104dd565b50600101610546565b5050509250929050565b43804060606106a086868661085a565b905093509350939050565b6060818067ffffffffffffffff8111156106c7576106c7610d31565b60405190808252806020026020018201604052801561070d57816020015b6040805180820190915260008152606060208201528152602001906001900390816106e55790505b5091503660005b828110156104e657600084828151811061073057610730610d60565b6020026020010151905086868381811061074c5761074c610d60565b905060200281019061075e9190610e76565b925061076d6020840184610ce2565b73ffffffffffffffffffffffffffffffffffffffff166107906040850185610dcd565b60405161079e929190610e32565b6000604051808303816000865af19150503d80600081146107db576040519150601f19603f3d011682016040523d82523d6000602084013e6107e0565b606091505b506020808401919091529015158083529084013517610851577f08c379a000000000000000000000000000000000000000000000000000000000600052602060045260176024527f4d756c746963616c6c333a2063616c6c206661696c656400000000000000000060445260646000fd5b50600101610714565b6060818067ffffffffffffffff81111561087657610876610d31565b6040519080825280602002602001820160405280156108bc57816020015b6040805180820190915260008152606060208201528152602001906001900390816108945790505b5091503660005b82811015610a105760008482815181106108df576108df610d60565b602002602001015190508686838181106108fb576108fb610d60565b905060200281019061090d9190610e42565b925061091c6020840184610ce2565b73ffffffffffffffffffffffffffffffffffffffff1661093f6020850185610dcd565b60405161094d929190610e32565b6000604051808303816000865af19150503d806000811461098a576040519150601f19603f3d011682016040523d82523d6000602084013e61098f565b606091505b506020830152151581528715610a07578051610a07576040517f08c379a000000000000000000000000000000000000000000000000000000000815260206004820152601760248201527f4d756c746963616c6c333a2063616c6c206661696c656400000000000000000060448201526064016104dd565b506001016108c3565b5050509392505050565b6000806060610a2b60018686610690565b919790965090945092505050565b60008083601f840112610a4b57600080fd5b50813567ffffffffffffffff811115610a6357600080fd5b6020830191508360208260051b8501011115610a7e57600080fd5b9250929050565b60008060208385031215610a9857600080fd5b823567ffffffffffffffff811115610aaf57600080fd5b610abb85828601610a39565b90969095509350505050565b6000815180845260005b81811015610aed57602081850181015186830182015201610ad1565b81811115610aff576000602083870101525b50601f017fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe0169290920160200192915050565b600082825180855260208086019550808260051b84010181860160005b84811015610bb1578583037fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe001895281518051151584528401516040858501819052610b9d81860183610ac7565b9a86019a9450505090830190600101610b4f565b5090979650505050505050565b602081526000610bd16020830184610b32565b9392505050565b600060408201848352602060408185015281855180845260608601915060608160051b870101935082870160005b82811015610c52577fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa0888703018452610c40868351610ac7565b95509284019290840190600101610c06565b509398975050505050505050565b600080600060408486031215610c7557600080fd5b83358015158114610c8557600080fd5b9250602084013567ffffffffffffffff811115610ca157600080fd5b610cad86828701610a39565b9497909650939450505050565b838152826020820152606060408201526000610cd96060830184610b32565b95945050505050565b600060208284031215610cf457600080fd5b813573ffffffffffffffffffffffffffffffffffffffff81168114610bd157600080fd5b600060208284031215610d2a57600080fd5b5035919050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052604160045260246000fd5b7f4e487b7100000000000000000000000000000000000000000000000000000000600052603260045260246000fd5b600082357fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff81833603018112610dc357600080fd5b9190910192915050565b60008083357fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe1843603018112610e0257600080fd5b83018035915067ffffffffffffffff821115610e1d57600080fd5b602001915036819003821315610a7e57600080fd5b8183823760009101908152919050565b600082357fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc1833603018112610dc357600080fd5b600082357fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa1833603018112610dc357600080fdfea2646970667358221220bb2b5c71a328032f97c676ae39a1ec2148d3e5d6f73d95e9b17910152d61f16264736f6c634300080c00331ca0edce47092c0f398cebf3ffc267f05c8e7076e3b89445e0fe50f6332273d4569ba01b0b9d000e19b24c5869b0fc3b22b0d6fa47cd63316875cbbd577d76e6fde086").unwrap();

        let transaction = EthereumTransactionCommon::from_bytes(&signed_transaction)
            .expect("The MultiCall3 transaction shouldn't be unparsable");

        let mut tx_hash = [0; TRANSACTION_HASH_SIZE];
        tx_hash.copy_from_slice(&expected_tx_hash);

        // *NB*: due to the da fee, this will fail by default - so we inject it through the
        // delayed inbox instead, so that it doesn't pay the da fee.
        let tx = Transaction {
            tx_hash,
            content: EthereumDelayed(transaction),
        };

        let transactions: Vec<Transaction> = vec![tx];

        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = H160::from_str("05f32b3cc3888453ff71b01135b34ff8e41263f2").unwrap();
        set_balance(&mut host, &sender, U256::from(1_000_000_000_000_000_000u64));

        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");

        // See address at https://www.multicall3.com/ on in the github repository linked above
        let expected_created_contract =
            H160::from_str("0xcA11bde05977b3631167028862bE2a173976CA11").unwrap();

        let receipt = read_transaction_receipt(&mut host, &tx_hash)
            .expect("Should have found receipt");
        assert_eq!(TransactionStatus::Success, receipt.status);
        assert_eq!(Some(expected_created_contract), receipt.contract_address);
    }

    #[test]
    // Regression test for L2-1407 / F-1357: a forced-inclusion (delayed) type-4
    // transaction whose authorization list is present but empty must NOT abort
    // block production. REVM rejects the shape with "Authorization list cannot
    // be empty per EIP-7702"; because the transaction is delayed and cannot be
    // dropped by the sequencer, that error is demoted to an invalid transaction
    // and skipped, keeping the forced blueprint valid. Before this was
    // hardened, the error propagated as a block-level failure and the whole
    // forced blueprint was reverted, halting the chain.
    fn test_delayed_empty_eip7702_authorization_list_does_not_abort_block() {
        let mut host = MockKernelHost::default();
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        // Injected through the delayed inbox: a delayed transaction cannot be
        // dropped by the sequencer, so its failure must never revert the
        // blueprint.
        let poison_tx = Transaction {
            tx_hash,
            content: EthereumDelayed(dummy_eip7702_empty_authorization_list_tx()),
        };

        store_blueprints(&mut host, vec![blueprint(vec![poison_tx])]);

        let sender = dummy_eth_caller();
        set_balance(&mut host, &sender, U256::from(1_000_000_000_000_000_000u64));
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        // The block must be produced despite the poison transaction.
        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("Block production must not be aborted by a delayed transaction");

        // The poison transaction is skipped as invalid: it leaves no receipt.
        assert!(
            read_transaction_receipt_status(&mut host, &tx_hash).is_err(),
            "The poison delayed transaction must be skipped without a receipt"
        );
    }

    // Comment out `ignore` when resigning the dummy transactions
    #[test]
    #[ignore = "Only run when re-signing required"]
    fn resign() {
        println!("Dummy eth transactions");
        // corresponding caller's address is 0xf95abdf6ede4c3703e0e9453771fbee8592d31e9
        let private_key =
            "e922354a3e5902b5ac474f3ff08a79cff43533826b8f451ae2190b65a9d26158";

        let zero = dummy_eth_transaction_zero();
        let one = dummy_eth_transaction_one();

        let zero_resigned = zero.sign_transaction(private_key.to_string()).unwrap();
        let one_resigned = one.sign_transaction(private_key.to_string()).unwrap();

        assert_eq!(zero, zero_resigned);
        assert_eq!(one, one_resigned);
    }

    #[test]
    // Test if a valid transaction is producing a receipt with a success status
    fn test_type_propagation() {
        let mut host = MockKernelHost::default();
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];
        let tx_hash_eip1559 = [1; TRANSACTION_HASH_SIZE];
        let tx_hash_eip2930 = [2; TRANSACTION_HASH_SIZE];

        let valid_tx = Transaction {
            tx_hash,
            content: Ethereum(make_dummy_transaction(0, TransactionType::Legacy)),
        };

        let valid_tx_eip1559 = Transaction {
            tx_hash: tx_hash_eip1559,
            content: Ethereum(make_dummy_transaction(1, TransactionType::Eip1559)),
        };

        let valid_tx_eip2930 = Transaction {
            tx_hash: tx_hash_eip2930,
            content: Ethereum(make_dummy_transaction(2, TransactionType::Eip2930)),
        };

        let transactions: Vec<Transaction> =
            vec![valid_tx, valid_tx_eip1559, valid_tx_eip2930];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        set_balance(&mut host, &sender, U256::from(1_000_000_000_000_000_000u64));
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(
            &mut host,
            &dummy_tezosx_config(SpecId::default()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");

        let receipt = read_transaction_receipt(&mut host, &tx_hash)
            .expect("Should have found receipt");
        assert_eq!(receipt.type_, TransactionType::Legacy);

        let receipt_eip1559 = read_transaction_receipt(&mut host, &tx_hash_eip1559)
            .expect("Should have found receipt");
        assert_eq!(receipt_eip1559.type_, TransactionType::Eip1559);

        let receipt_eip2930 = read_transaction_receipt(&mut host, &tx_hash_eip2930)
            .expect("Should have found receipt");
        assert_eq!(receipt_eip2930.type_, TransactionType::Eip2930);
    }

    #[test]
    fn test_tezos_x_upgrade_tez_protocol() {
        fn read_protocol_and_next_protocol(tez_block: &[u8]) -> (Protocol, Protocol) {
            let rlp = rlp::Rlp::new(tez_block);
            let protocol: Protocol = rlp.val_at(5).unwrap();
            let next_protocol: Protocol = rlp.val_at(6).unwrap();
            (protocol, next_protocol)
        }

        let mut host = MockKernelHost::default();

        let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);
        let mut config = dummy_configuration();

        let previous_protocol = Protocol::S023;
        let current_protocol = TARGET_TEZOS_PROTOCOL;

        // Store a TezBlockHeader with next_protocol set to the previous
        // protocol to simulate that the previous block was produced from
        // a kernel whose protocol is the previous one.
        let header = TezBlockHeader {
            hash: H256(*TezBlock::genesis_block_hash()),
            next_protocol: previous_protocol,
        };
        store_current_tez_block_header(&mut host, &header).unwrap();

        // First block: protocol = previous (from stored header),
        // next_protocol = current (TARGET_TEZOS_PROTOCOL)
        store_blueprints(&mut host, vec![blueprint(vec![])]);
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);

        let block = read_tez_current_block(&mut host).unwrap();
        let (protocol, next_protocol) = read_protocol_and_next_protocol(&block);
        assert_eq!(protocol, previous_protocol);
        assert_eq!(next_protocol, current_protocol);

        // Second block: both protocol and next_protocol should be current
        store_inbox_blueprint_by_number(
            &mut crate::storage::load_base_keyspace(&mut host).unwrap(),
            blueprint(vec![]),
            U256::from(1),
        )
        .expect("Should have stored blueprint");

        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);

        let block = read_tez_current_block(&mut host).unwrap();
        let (protocol, next_protocol) = read_protocol_and_next_protocol(&block);
        assert_eq!(protocol, current_protocol);
        assert_eq!(next_protocol, current_protocol);
    }

    #[test]
    fn test_can_fit_in_reboot_tezos_operation() {
        // Test that can_fit_in_reboot accounts for the gas of Tezos
        // operations.  Before the fix, TezosXChainConfig::can_fit_in_reboot
        // returned Ok(true) for all Tezos operations regardless of gas,
        // so this test would see Finished instead of RebootNeeded.

        let mut host = MockKernelHost::default();
        let registry = RegistryImpl::default();
        let block_constants = first_block(&mut host);

        // Build a Tezos reveal operation with gas_limit = 500.
        let reveal = make_reveal_operation(1, 1, 500, 0, bootstrap1());
        let tx_hash = reveal.hash().unwrap().into();
        let tezos_op = TezlinkOperation {
            tx_hash,
            content: TezlinkContent::Tezos(reveal),
        };
        let tezos_tx = TezosXTransaction::Tezos(tezos_op);

        let transactions = vec![tezos_tx].into();
        let mut block_in_progress = BlockInProgress::new(
            U256::from(1),
            transactions,
            block_constants
                .evm_runtime_block_constants
                .block_fees
                .base_fee_per_gas(),
        );

        // Fill the run's gas to just below the reboot limit so that
        // the Tezos operation's converted gas pushes it over.
        let limits = EvmLimits::default();
        let cumulative_gas_in_run = max_gas_per_reboot(&limits) - 1;
        host.add_execution_gas(cumulative_gas_in_run);

        let chain_config = dummy_tezosx_config_with_tezos_runtime(&mut host);

        let result = compute(
            &mut host,
            &registry,
            &chain_config,
            &OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX).unwrap(),
            &mut block_in_progress,
            &block_constants,
            None,
            None,
            false,
        )
        .expect("compute should succeed");

        assert_eq!(
            result,
            BlockInProgressComputationResult::RebootNeeded,
            "Tezos operation should not fit when gas is nearly exhausted"
        );
    }
}
