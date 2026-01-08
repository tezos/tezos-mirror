// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023, 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::apply::{
    apply_transaction, ExecutionInfo, ExecutionResult, WITHDRAWAL_OUTBOX_QUEUE,
};
use crate::blueprint::Blueprint;
use crate::blueprint_storage::{
    drop_blueprint, read_blueprint, read_current_block_header,
    store_current_block_header, BlockHeader, ChainHeader, EVMBlockHeader,
};
use crate::chains::{
    BlockInProgressTrait, ChainConfigTrait, ChainHeaderTrait, EvmChainConfig, EvmLimits,
};
use crate::configuration::ConfigurationMode;
use crate::delayed_inbox::DelayedInbox;
use crate::error::Error;
use crate::event::Event;
use crate::l2block::L2Block;
use crate::storage;
use crate::transaction::{Transaction, Transactions};
use crate::upgrade;
use crate::upgrade::KernelUpgrade;
use crate::Configuration;
use crate::{block_in_progress, tick_model};
use anyhow::Context;
use block_in_progress::EthBlockInProgress;
use evm::Config;
use evm_execution::account_storage::{init_account_storage, EthereumAccountStorage};
use evm_execution::precompiles::PrecompileBTreeMap;
use evm_execution::trace::TracerInput;
use primitive_types::{H160, H256, U256};
use tezos_ethereum::block::BlockConstants;
use tezos_ethereum::transaction::TransactionHash;
use tezos_evm_logging::{log, Level::*, Verbosity};
use tezos_evm_runtime::runtime::Runtime;
use tezos_evm_runtime::safe_storage::SafeStorage;
use tezos_smart_rollup::outbox::OutboxQueue;
use tezos_smart_rollup::types::Timestamp;
use tezos_smart_rollup_host::path::Path;

pub const GENESIS_PARENT_HASH: H256 = H256([0xff; 32]);

pub const GAS_LIMIT: u64 = 1 << 50;

/// Struct used to allow the compiler to check that the tick counter value is
/// correctly moved and updated. Copy and Clone should NOT be derived.
pub struct TickCounter {
    pub c: u64,
}

impl TickCounter {
    pub fn new(c: u64) -> Self {
        Self { c }
    }
    pub fn finalize(consumed_ticks: u64) -> Self {
        Self {
            c: consumed_ticks + tick_model::constants::FINALIZE_UPPER_BOUND,
        }
    }
}

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
enum BlockInProgressProvenance {
    Storage,
    Blueprint,
}

fn on_invalid_transaction<Host: Runtime>(
    host: &mut Host,
    transaction: &Transaction,
    block_in_progress: &mut EthBlockInProgress,
    data_size: u64,
) {
    if transaction.is_delayed() {
        block_in_progress.register_delayed_transaction(transaction.tx_hash);
    }

    block_in_progress.account_for_invalid_transaction(data_size);
    log!(
        host,
        Benchmarking,
        "Estimated ticks after tx: {}",
        block_in_progress.estimated_ticks_in_run
    );
}

fn max_gas_per_reboot(limits: &EvmLimits) -> u64 {
    // Arbitrarily defined as to give 500k gas in addition to `maximum_gas_limit` in our production
    // case (i.e., when `maximum_gas_limit` is 30M).
    limits.maximum_gas_limit * 61 / 60
}

fn can_fit_in_reboot(
    limits: &EvmLimits,
    used_gas_in_run: U256,
    tx_gas_limit: u64,
) -> bool {
    let max_gas_per_reboot = U256::from(max_gas_per_reboot(limits));
    let capped_gas_limit = u64::min(tx_gas_limit, limits.maximum_gas_limit);
    used_gas_in_run + U256::from(capped_gas_limit) <= max_gas_per_reboot
}

#[allow(clippy::too_many_arguments)]
fn compute<Host: Runtime>(
    host: &mut Host,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_in_progress: &mut EthBlockInProgress,
    block_constants: &BlockConstants,
    precompiles: &PrecompileBTreeMap<Host>,
    evm_account_storage: &mut EthereumAccountStorage,
    sequencer_pool_address: Option<H160>,
    limits: &EvmLimits,
    tracer_input: Option<TracerInput>,
    evm_configuration: &Config,
) -> Result<BlockInProgressComputationResult, anyhow::Error> {
    log!(
        host,
        Debug,
        "Queue length {}.",
        block_in_progress.queue_length()
    );
    // iteration over all remaining transaction in the block
    while block_in_progress.has_tx() {
        let transaction = block_in_progress.pop_tx().ok_or(Error::Reboot)?;
        let data_size: u64 = transaction.data_size();

        log!(host, Benchmarking, "Transaction data size: {}", data_size);

        if !can_fit_in_reboot(
            limits,
            host.executed_gas().into(),
            transaction.execution_gas_limit(&block_constants.block_fees)?,
        ) {
            log!(
                host,
                Debug,
                "There are not enough gas left in the current kernel run \
                 to try the transaction, but it will be retried after reboot."
            );
            block_in_progress.repush_tx(transaction);

            return Ok(BlockInProgressComputationResult::RebootNeeded);
        }

        // If `apply_transaction` returns `None`, the transaction should be
        // ignored, i.e. invalid signature or nonce.
        match apply_transaction(
            host,
            outbox_queue,
            block_constants,
            precompiles,
            &transaction,
            block_in_progress.index,
            evm_account_storage,
            sequencer_pool_address,
            tracer_input,
            evm_configuration,
            limits,
        )? {
            ExecutionResult::Valid(ExecutionInfo {
                receipt_info,
                object_info,
                estimated_ticks_used,
                execution_gas_used,
            }) => {
                if transaction.is_delayed() {
                    block_in_progress.register_delayed_transaction(transaction.tx_hash);
                }

                block_in_progress.register_valid_transaction(
                    &transaction,
                    object_info,
                    receipt_info,
                    estimated_ticks_used,
                    execution_gas_used,
                    host,
                )?;
                log!(
                    host,
                    Benchmarking,
                    "Estimated ticks after tx: {}",
                    block_in_progress.estimated_ticks_in_run
                );
            }
            ExecutionResult::Invalid => {
                on_invalid_transaction(host, &transaction, block_in_progress, data_size)
            }
        };
    }
    Ok(BlockInProgressComputationResult::Finished {
        included_delayed_transactions: block_in_progress.delayed_txs.clone(),
    })
}

enum BlueprintParsing<BIP> {
    Next(Box<BIP>),
    None,
}

pub fn eth_bip_from_blueprint<Host: Runtime>(
    host: &Host,
    chain_config: &EvmChainConfig,
    tick_counter: &TickCounter,
    next_bip_number: U256,
    header: EVMBlockHeader,
    blueprint: Blueprint<Transactions>,
) -> EthBlockInProgress {
    let gas_price = crate::gas_price::base_fee_per_gas(
        host,
        blueprint.timestamp,
        chain_config.get_limits().minimum_base_fee_per_gas,
    );

    let bip = EthBlockInProgress::from_blueprint(
        blueprint,
        next_bip_number,
        header.hash,
        tick_counter.c,
        gas_price,
        header.receipts_root,
        header.transactions_root,
    );

    tezos_evm_logging::log!(host, tezos_evm_logging::Level::Debug, "bip: {bip:?}");
    bip
}

#[cfg_attr(feature = "benchmark", inline(never))]
fn next_bip_from_blueprints<Host: Runtime, ChainConfig: ChainConfigTrait>(
    host: &mut Host,
    tick_counter: &TickCounter,
    chain_config: &ChainConfig,
    config: &mut Configuration,
    kernel_upgrade: &Option<KernelUpgrade>,
) -> Result<BlueprintParsing<ChainConfig::BlockInProgress>, anyhow::Error> {
    let (next_bip_number, timestamp, chain_header) = match read_current_block_header(host)
    {
        Err(_) => (
            U256::zero(),
            Timestamp::from(0),
            ChainConfig::ChainHeader::genesis_header(),
        ),
        Ok(BlockHeader {
            blueprint_header,
            chain_header,
        }) => (
            blueprint_header.number + 1,
            blueprint_header.timestamp,
            chain_header,
        ),
    };
    log!(host, Debug, "Next blueprint number: {:?}", next_bip_number);
    let (blueprint, size) = read_blueprint::<_, ChainConfig>(
        host,
        config,
        next_bip_number,
        timestamp,
        &chain_header,
    )?;
    log!(host, Benchmarking, "Size of blueprint: {}", size);
    match blueprint {
        Some(blueprint) => {
            if let Some(kernel_upgrade) = kernel_upgrade {
                if blueprint.timestamp >= kernel_upgrade.activation_timestamp {
                    upgrade::upgrade(host, kernel_upgrade.preimage_hash)?;
                    // We abort the call, as there is no blueprint to execute,
                    // the kernel will reboot.
                    return Ok(BlueprintParsing::None);
                }
            }
            let bip: ChainConfig::BlockInProgress = chain_config
                .block_in_progress_from_blueprint(
                    host,
                    tick_counter,
                    next_bip_number,
                    chain_header,
                    blueprint,
                );
            Ok(BlueprintParsing::Next(Box::new(bip)))
        }
        None => Ok(BlueprintParsing::None),
    }
}

#[allow(clippy::too_many_arguments)]
pub fn compute_bip<Host: Runtime>(
    host: &mut Host,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    mut block_in_progress: EthBlockInProgress,
    precompiles: &PrecompileBTreeMap<Host>,
    tick_counter: &mut TickCounter,
    sequencer_pool_address: Option<H160>,
    limits: &EvmLimits,
    tracer_input: Option<TracerInput>,
    chain_id: U256,
    da_fee_per_byte: U256,
    coinbase: H160,
    evm_configuration: &Config,
) -> anyhow::Result<BlockComputationResult> {
    let mut evm_account_storage =
        init_account_storage().context("Failed to initialize EVM account storage")?;
    let constants: BlockConstants = block_in_progress.constants(
        chain_id,
        limits.minimum_base_fee_per_gas,
        da_fee_per_byte,
        GAS_LIMIT,
        coinbase,
    );
    let result = compute(
        host,
        outbox_queue,
        &mut block_in_progress,
        &constants,
        precompiles,
        &mut evm_account_storage,
        sequencer_pool_address,
        limits,
        tracer_input,
        evm_configuration,
    )?;
    match result {
        BlockInProgressComputationResult::RebootNeeded => {
            log!(host, Info, "Ask for reboot.");
            log!(
                host,
                Benchmarking,
                "Ask for reboot. Estimated ticks: {}",
                &block_in_progress.estimated_ticks_in_run
            );
            storage::store_block_in_progress(host, &block_in_progress)?;
            Ok(BlockComputationResult::RebootNeeded)
        }
        BlockInProgressComputationResult::Finished {
            included_delayed_transactions,
        } => {
            crate::gas_price::register_block(host, &block_in_progress)?;
            *tick_counter =
                TickCounter::finalize(block_in_progress.estimated_ticks_in_run);
            let new_block = block_in_progress
                .finalize_and_store(host, &constants)
                .context("Failed to finalize the block in progress")?;
            Ok(BlockComputationResult::Finished {
                included_delayed_transactions,
                block: new_block,
            })
        }
    }
}

fn revert_block<Host: Runtime>(
    safe_host: &mut SafeStorage<&mut Host>,
    block_in_progress_provenance: &BlockInProgressProvenance,
    number: U256,
    error: anyhow::Error,
) -> anyhow::Result<()> {
    log!(
        safe_host,
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
    drop_blueprint(safe_host.host, number)?;
    Ok(())
}

fn clean_delayed_transactions(
    host: &mut impl Runtime,
    delayed_inbox: &mut DelayedInbox,
    delayed_txs: Vec<TransactionHash>,
) -> anyhow::Result<()> {
    for hash in delayed_txs {
        delayed_inbox.delete(host, hash.into())?;
    }
    Ok(())
}

fn promote_block<Host: Runtime>(
    safe_host: &mut SafeStorage<&mut Host>,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_in_progress_provenance: &BlockInProgressProvenance,
    block_header: BlockHeader<ChainHeader>,
    config: &mut Configuration,
    delayed_txs: Vec<TransactionHash>,
) -> anyhow::Result<()> {
    if let BlockInProgressProvenance::Storage = block_in_progress_provenance {
        storage::delete_block_in_progress(safe_host)?;
    }
    safe_host.promote()?;
    safe_host.promote_trace()?;
    drop_blueprint(safe_host.host, block_header.blueprint_header.number)?;
    store_current_block_header(safe_host.host, &block_header)?;

    let event = Event::blueprint_applied(block_header);

    event.store(safe_host.host)?;

    let written = outbox_queue.flush_queue(safe_host.host);
    // Log to Info only if we flushed messages.
    let level = if written > 0 { Info } else { Debug };
    log!(
        safe_host,
        level,
        "Flushed outbox queue messages ({} flushed)",
        written
    );

    if let ConfigurationMode::Sequencer { delayed_inbox, .. } = &mut config.mode {
        clean_delayed_transactions(safe_host.host, delayed_inbox, delayed_txs)?;
    }

    Ok(())
}

pub fn produce<Host: Runtime, ChainConfig: ChainConfigTrait>(
    host: &mut Host,
    chain_config: &ChainConfig,
    config: &mut Configuration,
    sequencer_pool_address: Option<H160>,
    tracer_input: Option<TracerInput>,
) -> Result<ComputationResult, anyhow::Error> {
    let da_fee_per_byte = crate::retrieve_da_fee(host)?;

    let kernel_upgrade = upgrade::read_kernel_upgrade(host)?;

    // If there's a pool address, the coinbase in block constants and miner
    // in blocks is set to the pool address.
    let coinbase = sequencer_pool_address.unwrap_or_default();

    let mut tick_counter = TickCounter::new(0u64);

    let mut safe_host = SafeStorage { host };
    let outbox_queue = OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX)?;
    let precompiles = chain_config.precompiles_set(config.enable_fa_bridge);

    // Check if there's a BIP in storage to resume its execution
    let (block_in_progress_provenance, block_in_progress) =
        match ChainConfig::read_block_in_progress(&safe_host)? {
            Some(block_in_progress) => {
                log!(safe_host, Debug, "Restauring BIP from storage.");
                (BlockInProgressProvenance::Storage, block_in_progress)
            }
            None => {
                // Using `safe_host.host` allows to escape from the failsafe storage, which is necessary
                // because the sequencer pool address is located outside of `/evm/world_state`.
                upgrade::possible_sequencer_upgrade(safe_host.host)?;

                log!(safe_host, Debug, "Creating BIP from Blueprint.");
                // Execute at most one of the stored blueprints
                let block_in_progress = match next_bip_from_blueprints(
                    safe_host.host,
                    &tick_counter,
                    chain_config,
                    config,
                    &kernel_upgrade,
                )? {
                    BlueprintParsing::Next(bip) => {
                        log!(safe_host, Debug, "Creating BIP from Blueprint: Success.");
                        bip
                    }
                    BlueprintParsing::None => {
                        log!(
                            safe_host,
                            Benchmarking,
                            "Estimated ticks: {}",
                            tick_counter.c
                        );
                        log!(safe_host, Debug, "Creating BIP from Blueprint: Failure.");
                        return Ok(ComputationResult::Finished);
                    }
                };
                // We are going to execute a new block, we copy the storage to allow
                // to revert if the block fails.
                safe_host.start()?;
                (BlockInProgressProvenance::Blueprint, *block_in_progress)
            }
        };

    let processed_blueprint = block_in_progress.number();
    let computation_result = chain_config.compute_bip(
        &mut safe_host,
        &outbox_queue,
        block_in_progress,
        &precompiles,
        &mut tick_counter,
        sequencer_pool_address,
        config.maximum_allowed_ticks,
        tracer_input,
        da_fee_per_byte,
        coinbase,
    );

    match computation_result {
        Ok(BlockComputationResult::Finished {
            included_delayed_transactions,
            block,
        }) => {
            promote_block(
                &mut safe_host,
                &outbox_queue,
                &block_in_progress_provenance,
                block.header(),
                config,
                included_delayed_transactions,
            )?;
            Ok(ComputationResult::RebootNeeded)
        }
        Ok(BlockComputationResult::RebootNeeded) => {
            // The computation will resume at next reboot, we leave the
            // storage untouched.
            if let BlockInProgressProvenance::Blueprint = &block_in_progress_provenance {
                log!(
                    safe_host,
                    Benchmarking,
                    "Estimated ticks: {}",
                    tick_counter.c
                )
            };
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
            if let BlockInProgressProvenance::Blueprint = &block_in_progress_provenance {
                log!(
                    safe_host,
                    Benchmarking,
                    "Estimated ticks: {}",
                    tick_counter.c
                )
            };
            Ok(ComputationResult::RebootNeeded)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_storage;
    use crate::block_storage::read_current_number;
    use crate::blueprint::Blueprint;
    use crate::blueprint_storage::store_inbox_blueprint;
    use crate::blueprint_storage::store_inbox_blueprint_by_number;
    use crate::chains::{
        ChainFamily, EvmChainConfig, MichelsonChainConfig, TezTransactions,
    };
    use crate::fees::DA_FEE_PER_BYTE;
    use crate::fees::MINIMUM_BASE_FEE_PER_GAS;
    use crate::storage::read_block_in_progress;
    use crate::storage::read_last_info_per_level_timestamp;
    use crate::storage::{read_transaction_receipt, read_transaction_receipt_status};
    use crate::transaction::Transaction;
    use crate::transaction::TransactionContent;
    use crate::transaction::TransactionContent::Ethereum;
    use crate::transaction::TransactionContent::EthereumDelayed;
    use crate::transaction::Transactions;
    use crate::{retrieve_block_fees, retrieve_chain_id};
    use evm_execution::account_storage::{
        account_path, init_account_storage, EthereumAccountStorage,
    };
    use evm_execution::configuration::EVMVersion;
    use evm_execution::precompiles::precompile_set;
    use primitive_types::{H160, U256};
    use std::str::FromStr;
    use tezos_ethereum::block::BlockFees;
    use tezos_ethereum::transaction::{
        TransactionHash, TransactionStatus, TransactionType, TRANSACTION_HASH_SIZE,
    };
    use tezos_ethereum::tx_common::EthereumTransactionCommon;
    use tezos_evm_runtime::extensions::WithGas;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime::Runtime;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;
    use tezos_smart_rollup_host::runtime::Runtime as SdkRuntime;

    fn blueprint(transactions: Vec<Transaction>) -> Blueprint<Transactions> {
        Blueprint {
            transactions: Transactions::EthTxs(transactions),
            timestamp: Timestamp::from(0i64),
        }
    }

    fn tezlink_blueprint() -> Blueprint<TezTransactions> {
        Blueprint {
            transactions: TezTransactions {},
            timestamp: Timestamp::from(0i64),
        }
    }

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    fn set_balance<Host: Runtime>(
        host: &mut Host,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
        balance: U256,
    ) {
        let mut account = evm_account_storage
            .get_or_create(host, &account_path(address).unwrap())
            .unwrap();
        let current_balance = account.balance(host).unwrap();
        if current_balance > balance {
            account
                .balance_remove(host, current_balance - balance)
                .unwrap();
        } else {
            account
                .balance_add(host, balance - current_balance)
                .unwrap();
        }
    }

    fn get_balance<Host: Runtime>(
        host: &mut Host,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
    ) -> U256 {
        let account = evm_account_storage
            .get_or_create(host, &account_path(address).unwrap())
            .unwrap();
        account.balance(host).unwrap()
    }

    const DUMMY_CHAIN_ID: U256 = U256::one();
    const DUMMY_BASE_FEE_PER_GAS: u64 = MINIMUM_BASE_FEE_PER_GAS;
    const DUMMY_DA_FEE: u64 = DA_FEE_PER_BYTE;

    fn dummy_evm_config(evm_configuration: Config) -> EvmChainConfig {
        EvmChainConfig::create_config(
            DUMMY_CHAIN_ID,
            EvmLimits::default(),
            evm_configuration,
        )
    }

    fn dummy_tez_config() -> MichelsonChainConfig {
        MichelsonChainConfig::create_config(DUMMY_CHAIN_ID)
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
            crate::fees::gas_for_fees(DUMMY_DA_FEE.into(), gas_price, &data, &[])
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

    fn store_blueprints<Host: Runtime, ChainConfig: ChainConfigTrait>(
        host: &mut Host,
        blueprints: Vec<Blueprint<ChainConfig::Transactions>>,
    ) {
        for (i, blueprint) in blueprints.into_iter().enumerate() {
            store_inbox_blueprint_by_number(host, blueprint, U256::from(i))
                .expect("Should have stored blueprint");
        }
    }

    fn store_block_fees<Host: Runtime>(
        host: &mut Host,
        block_fees: &BlockFees,
    ) -> anyhow::Result<()> {
        storage::store_minimum_base_fee_per_gas(
            host,
            block_fees.minimum_base_fee_per_gas(),
        )?;
        storage::store_da_fee(host, block_fees.da_fee_per_byte())?;
        Ok(())
    }

    fn produce_block_with_several_valid_txs<Host: Runtime>(
        host: &mut Host,
        evm_account_storage: &mut EthereumAccountStorage,
    ) {
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

        store_blueprints::<_, EvmChainConfig>(host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        set_balance(
            host,
            evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );
        store_block_fees(host, &dummy_block_fees()).unwrap();

        produce(
            host,
            &dummy_evm_config(EVMVersion::current_test_config()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");
    }

    fn assert_current_block_reading_validity<Host: Runtime>(host: &mut Host) {
        match block_storage::read_current(host, &ChainFamily::Evm) {
            Ok(_) => (),
            Err(e) => {
                panic!("Block reading failed: {:?}\n", e)
            }
        }
    }

    #[test]
    // Test if tezlink block production works
    fn test_produce_tezlink_block() {
        let mut host = MockKernelHost::default();

        let chain_config = dummy_tez_config();
        let mut config = dummy_configuration();

        store_blueprints::<_, MichelsonChainConfig>(
            &mut host,
            vec![
                tezlink_blueprint(),
                tezlink_blueprint(),
                tezlink_blueprint(),
            ],
        );

        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        let computation = produce(&mut host, &chain_config, &mut config, None, None)
            .expect("The block production should have succeeded.");
        assert_eq!(ComputationResult::Finished, computation);
        assert_eq!(U256::from(2), read_current_number(&host).unwrap());
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
        store_blueprints::<_, EvmChainConfig>(&mut host, vec![blueprint(transactions)]);

        let mut evm_account_storage = init_account_storage().unwrap();
        let sender = dummy_eth_caller();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(30000u64),
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
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
        store_blueprints::<_, EvmChainConfig>(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(1_000_000_000_000_000_000u64),
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
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
        store_blueprints::<_, EvmChainConfig>(&mut host, vec![blueprint(transactions)]);

        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(5000000000000000u64),
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
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

        let mut evm_account_storage = init_account_storage().unwrap();

        produce_block_with_several_valid_txs(&mut host, &mut evm_account_storage);

        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let dest_balance =
            get_balance(&mut host, &mut evm_account_storage, &dest_address);

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

        store_blueprints::<_, EvmChainConfig>(
            &mut host,
            vec![blueprint(transaction_0), blueprint(transaction_1)],
        );

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        // Produce block for blueprint containing transaction_0
        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");
        // Produce block for blueprint containing transaction_1
        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");

        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let dest_balance =
            get_balance(&mut host, &mut evm_account_storage, &dest_address);

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

        store_blueprints::<_, EvmChainConfig>(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );
        store_block_fees(&mut host, &dummy_block_fees).unwrap();

        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
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

        let mut evm_account_storage = init_account_storage().unwrap();

        produce_block_with_several_valid_txs(&mut host, &mut evm_account_storage);

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
        store_blueprints::<_, EvmChainConfig>(
            &mut host,
            vec![blueprint(transactions.clone()), blueprint(transactions)],
        );

        let sender = dummy_eth_caller();
        let initial_sender_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            initial_sender_balance,
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");

        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let sender_balance = get_balance(&mut host, &mut evm_account_storage, &sender);
        let dest_balance =
            get_balance(&mut host, &mut evm_account_storage, &dest_address);

        let expected_dest_balance = U256::from(500000000u64);
        let expected_gas = 21000;
        let da_fee = crate::fees::da_fee(DUMMY_DA_FEE.into(), &[], &[]);
        let expected_fees = dummy_block_fees().base_fee_per_gas() * expected_gas + da_fee;
        let expected_sender_balance =
            initial_sender_balance - expected_dest_balance - expected_fees;

        assert_eq!(dest_balance, expected_dest_balance);
        assert_eq!(sender_balance, expected_sender_balance, "sender balance");
    }

    #[test]
    fn test_blocks_are_indexed() {
        let mut host = MockKernelHost::default();
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let blocks_index =
            block_storage::internal_for_tests::init_blocks_index().unwrap();

        store_blueprints::<_, EvmChainConfig>(&mut host, vec![blueprint(vec![])]);

        let number_of_blocks_indexed = blocks_index.length(&host).unwrap();
        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("The block production failed.");

        let new_number_of_blocks_indexed = blocks_index.length(&host).unwrap();

        let current_block_hash =
            block_storage::read_current(&mut host, &ChainFamily::Evm)
                .unwrap()
                .hash()
                .as_bytes()
                .to_vec();

        assert_eq!(number_of_blocks_indexed + 1, new_number_of_blocks_indexed);

        assert_eq!(
            Ok(current_block_hash),
            blocks_index.get_value(&host, new_number_of_blocks_indexed - 1)
        );
    }

    fn first_block<MockHost: Runtime>(host: &mut MockHost) -> BlockConstants {
        let timestamp =
            read_last_info_per_level_timestamp(host).unwrap_or(Timestamp::from(0));
        let timestamp = U256::from(timestamp.as_u64());
        let chain_id = retrieve_chain_id(host);
        let block_fees = retrieve_block_fees(host);
        assert!(chain_id.is_ok(), "chain_id should be defined");
        assert!(block_fees.is_ok(), "block fees should be defined");
        BlockConstants::first_block(
            timestamp,
            chain_id.unwrap(),
            block_fees.unwrap(),
            crate::block::GAS_LIMIT,
            H160::zero(),
        )
    }

    #[test]
    fn test_stop_computation() {
        // init host
        let mut host = MockKernelHost::default();

        let block_constants = first_block(&mut host);
        let precompiles = precompile_set(false);

        //provision sender account
        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );

        // tx is valid because correct nonce and account provisionned
        let valid_tx = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: TransactionContent::Ethereum(dummy_eth_transaction_zero()),
        };
        let transactions = vec![valid_tx].into();

        // init block in progress
        let mut block_in_progress = EthBlockInProgress::new(
            U256::from(1),
            transactions,
            block_constants.block_fees.base_fee_per_gas(),
            vec![0; 32],
            vec![0; 32],
        );
        // run is almost full wrt gas consumption in the current run
        let limits = EvmLimits::default();
        let cumulative_gas_in_run = max_gas_per_reboot(&limits) - 1000;
        host.add_execution_gas(cumulative_gas_in_run);

        // act
        let result = compute(
            &mut host,
            &OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX).unwrap(),
            &mut block_in_progress,
            &block_constants,
            &precompiles,
            &mut evm_account_storage,
            None,
            &EvmLimits::default(),
            None,
            &EVMVersion::current_test_config(),
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
        assert_eq!(
            block_in_progress.estimated_ticks_in_block, 0,
            "should not have consumed any tick"
        );

        // the transaction should not have been processed
        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let sender_balance = get_balance(&mut host, &mut evm_account_storage, &sender);
        let dest_balance =
            get_balance(&mut host, &mut evm_account_storage, &dest_address);
        assert_eq!(sender_balance, U256::from(10000000000000000000u64));
        assert_eq!(dest_balance, U256::from(0u64))
    }

    #[test]
    fn invalid_transaction_should_bump_nonce() {
        let mut host = MockKernelHost::default();

        let mut evm_account_storage = init_account_storage().unwrap();

        let caller =
            address_from_str("f95abdf6ede4c3703e0e9453771fbee8592d31e9").unwrap();

        // Get the balance before the transaction, i.e. 0.
        let caller_account = evm_account_storage
            .get_or_create(&host, &account_path(&caller).unwrap())
            .unwrap();
        let default_nonce = caller_account.nonce(&host).unwrap();
        assert_eq!(default_nonce, 0, "default nonce should be 0");

        let tx = dummy_eth_transaction_zero();
        // Ensures the caller has enough balance to pay for the fees, but not
        // the transaction itself, otherwise the transaction will not even be
        // taken into account.
        let fees = U256::from(21000) * tx.gas_limit_with_fees();
        set_balance(&mut host, &mut evm_account_storage, &caller, fees);

        // Prepare a invalid transaction, i.e. with not enough funds.
        let tx_hash = [0; TRANSACTION_HASH_SIZE];
        let transaction = Transaction {
            tx_hash,
            content: Ethereum(tx),
        };
        store_blueprints::<_, EvmChainConfig>(
            &mut host,
            vec![blueprint(vec![transaction])],
        );

        // Apply the transaction
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
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
        let nonce = caller_account.nonce(&host).unwrap();
        assert_eq!(nonce, default_nonce, "nonce should not have been bumped");
    }

    /// A blueprint that should produce 1 block with an invalid transaction
    fn almost_empty_blueprint() -> Blueprint<Transactions> {
        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        // transaction should be invalid
        let tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_one()),
        };

        let transactions = vec![tx];

        blueprint(transactions)
    }

    fn check_current_block_number<Host: Runtime>(host: &mut Host, nb: usize) {
        let current_nb = block_storage::read_current_number(host)
            .expect("Should have manage to check block number");
        assert_eq!(current_nb, U256::from(nb), "Incorrect block number");
    }

    #[test]
    fn test_first_blocks() {
        let mut host = MockKernelHost::default();

        // first block should be 0
        let blueprint = almost_empty_blueprint();
        store_inbox_blueprint(&mut host, blueprint).expect("Should store a blueprint");
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("Empty block should have been produced");
        check_current_block_number(&mut host, 0);

        // second block
        let blueprint = almost_empty_blueprint();
        store_inbox_blueprint(&mut host, blueprint).expect("Should store a blueprint");
        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
            &mut dummy_configuration(),
            None,
            None,
        )
        .expect("Empty block should have been produced");
        check_current_block_number(&mut host, 1);

        // third block
        let blueprint = almost_empty_blueprint();
        store_inbox_blueprint(&mut host, blueprint).expect("Should store a blueprint");
        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
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
            block_storage::read_current_number(&host).is_err(),
            "Should not have found current block number"
        );

        //provision sender account
        let sender = H160::from_str(TEST_ADDR).unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            sender_initial_balance,
        );

        // These transactions are generated with the loop.sol contract, which are:
        // - create the contract
        // - call `loop(300)`
        // - call `loop(300)`
        let create_transaction =
            create_and_sign_transaction(CREATE_LOOP_DATA, 0, 160_000, None, TEST_SK);
        let loop_addr: H160 =
            evm_execution::utilities::create_address_legacy(&sender, &0);
        let loop_300_tx =
            create_and_sign_transaction(LOOP_300, 1, 230_000, Some(loop_addr), TEST_SK);
        let loop_300_tx2 =
            create_and_sign_transaction(LOOP_300, 2, 230_000, Some(loop_addr), TEST_SK);

        let proposals = vec![
            wrap_transaction(0, create_transaction),
            wrap_transaction(1, loop_300_tx),
            wrap_transaction(2, loop_300_tx2),
        ];

        store_blueprints::<_, EvmChainConfig>(&mut host, vec![blueprint(proposals)]);

        host.reboot_left().expect("should be some reboot left");

        let mut chain_config = dummy_evm_config(EVMVersion::current_test_config());
        chain_config.limits_mut().maximum_gas_limit = 560_000;
        let mut configuration = dummy_configuration();

        store_block_fees(&mut host, &dummy_block_fees()).unwrap();
        let computation_result =
            produce(&mut host, &chain_config, &mut configuration, None, None)
                .expect("Should have produced");

        // test no new block
        assert!(
            block_storage::read_current_number(&host).is_err(),
            "Should not have found current block number"
        );

        // test reboot is set
        matches!(computation_result, ComputationResult::RebootNeeded);

        // The block is in progress, therefore it is in the safe storage.
        let safe_host = SafeStorage { host: &mut host };
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
            block_storage::read_current_number(&host).is_err(),
            "Should not have found current block number"
        );
        //provision sender account
        let sender = H160::from_str(TEST_ADDR).unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            sender_initial_balance,
        );

        // These transactions are generated with the loop.sol contract, which are:
        // - create the contract
        // - call `loop(300)`
        // - call `loop(300)`
        let create_transaction =
            create_and_sign_transaction(CREATE_LOOP_DATA, 0, 160_000, None, TEST_SK);
        let loop_addr: H160 =
            evm_execution::utilities::create_address_legacy(&sender, &0);

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

        store_blueprints::<_, EvmChainConfig>(&mut host, proposals);

        let mut chain_config = dummy_evm_config(EVMVersion::current_test_config());
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
            block_storage::read_current_number(&host)
                .expect("should have found a block number"),
            U256::zero(),
            "There should have been one block registered"
        );

        // test reboot is set again
        matches!(computation_result, ComputationResult::RebootNeeded);

        // The block is in progress, therefore it is in the safe storage.
        let safe_host = SafeStorage { host: &mut host };
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

        store_blueprints::<_, EvmChainConfig>(&mut host, vec![blueprint(transactions)]);

        let sender = H160::from_str("05f32b3cc3888453ff71b01135b34ff8e41263f2").unwrap();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(1_000_000_000_000_000_000u64),
        );

        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
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
        store_blueprints::<_, EvmChainConfig>(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(1_000_000_000_000_000_000u64),
        );
        store_block_fees(&mut host, &dummy_block_fees()).unwrap();

        produce(
            &mut host,
            &dummy_evm_config(EVMVersion::current_test_config()),
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
}
