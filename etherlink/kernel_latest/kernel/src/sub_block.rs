// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::collections::VecDeque;

use crate::{
    apply::WITHDRAWAL_OUTBOX_QUEUE,
    block::{
        compute, promote_block, BlockInProgressComputationResult,
        BlockInProgressProvenance, GAS_LIMIT,
    },
    block_in_progress::BlockInProgress,
    block_storage,
    chains::{ChainConfigTrait, EvmChainConfig, ETHERLINK_SAFE_STORAGE_ROOT_PATH},
    configuration::{fetch_configuration, fetch_pure_evm_config},
    error::{Error, StorageError},
    gas_price::base_fee_per_gas,
    registry_impl::RegistryImpl,
    retrieve_chain_id, retrieve_da_fee,
    storage::{self, read_sequencer_pool_address},
    transaction::Transaction,
    upgrade,
};
use anyhow::anyhow;
use ethbloom::Bloom;
use primitive_types::{H160, H256, U256};
use rlp::{Decodable, Encodable};
use tezos_ethereum::{
    block::{BlockConstants, BlockFees},
    rlp_helpers::{
        append_timestamp, decode_field, decode_field_u256_le, decode_timestamp, next,
        FromRlpBytes,
    },
};
use tezos_evm_logging::__trace_kernel_add_attrs;
use tezos_evm_runtime::{runtime::Runtime, safe_storage::SafeStorage};
use tezos_smart_rollup::{host::RuntimeError, outbox::OutboxQueue, types::Timestamp};
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
use tezos_tracing::trace_kernel;

const SINGLE_TX_EXECUTION_INPUT: RefPath =
    RefPath::assert_from(b"/evm/world_state/single_tx/input_tx");

const ASSEMBLE_BLOCK_INPUT: RefPath =
    RefPath::assert_from(b"/evm/world_state/assemble_block/input");

pub struct SingleTxExecutionInput {
    pub tx: Transaction,
    pub timestamp: Timestamp,
    pub block_number: U256,
}

impl Encodable for SingleTxExecutionInput {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(3);
        stream.append(&self.tx);
        append_timestamp(stream, self.timestamp);
        stream.append(&self.block_number);
    }
}

impl Decodable for SingleTxExecutionInput {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        if decoder.item_count()? != 3 {
            return Err(rlp::DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let tx = decode_field(&next(&mut it)?, "Transaction")?;
        let timestamp = decode_timestamp(&next(&mut it)?)?;
        let block_number = decode_field_u256_le(&next(&mut it)?, "Block number")?;
        Ok(SingleTxExecutionInput {
            tx,
            timestamp,
            block_number,
        })
    }
}

pub struct AssembleBlockInput {
    pub timestamp: Timestamp,
    pub block_number: U256,
}

impl Encodable for AssembleBlockInput {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        append_timestamp(stream, self.timestamp);
        stream.append(&self.block_number);
    }
}

impl Decodable for AssembleBlockInput {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        if decoder.item_count()? != 2 {
            return Err(rlp::DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let timestamp = decode_timestamp(&next(&mut it)?)?;
        let block_number = decode_field_u256_le(&next(&mut it)?, "Block number")?;
        Ok(AssembleBlockInput {
            timestamp,
            block_number,
        })
    }
}

pub fn read_assemble_block_input<Host: Runtime>(
    host: &mut Host,
) -> Result<Option<AssembleBlockInput>, Error> {
    match host.store_read_all(&ASSEMBLE_BLOCK_INPUT) {
        Ok(bytes) => {
            let input = AssembleBlockInput::from_rlp_bytes(&bytes)?;
            host.store_delete(&ASSEMBLE_BLOCK_INPUT)?;
            Ok(Some(input))
        }
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(err.into()),
    }
}

pub fn read_single_tx_execution_input<Host: Runtime>(
    host: &mut Host,
) -> Result<Option<SingleTxExecutionInput>, Error> {
    match host.store_read_all(&SINGLE_TX_EXECUTION_INPUT) {
        Ok(bytes) => {
            let input = SingleTxExecutionInput::from_rlp_bytes(&bytes)?;
            host.store_delete(&SINGLE_TX_EXECUTION_INPUT)?;
            Ok(Some(input))
        }
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(err.into()),
    }
}

fn get_evm_config<Host: Runtime>(host: &mut Host) -> Result<EvmChainConfig, Error> {
    let chain_id = retrieve_chain_id(host)?;
    Ok(fetch_pure_evm_config(host, chain_id))
}

fn block_constants<Host: Runtime>(
    host: &mut Host,
    config: &EvmChainConfig,
    timestamp: Timestamp,
    number: U256,
) -> Result<BlockConstants, Error> {
    let coinbase = read_sequencer_pool_address(host).unwrap_or_default();
    let da_fee_per_byte = retrieve_da_fee(host)?;
    let minimum_base_fee_per_gas = config.get_limits().minimum_base_fee_per_gas;
    let base_fee_per_gas = base_fee_per_gas(host, timestamp, minimum_base_fee_per_gas);
    let block_fees =
        BlockFees::new(minimum_base_fee_per_gas, base_fee_per_gas, da_fee_per_byte);
    Ok(BlockConstants {
        number,
        coinbase,
        timestamp: timestamp.as_u64().into(),
        gas_limit: GAS_LIMIT,
        tezos_experimental_features: config.enable_tezos_runtime(),
        block_fees,
        chain_id: config.get_chain_id(),
        prevrandao: None,
    })
}

#[trace_kernel]
pub fn handle_run_transaction<Host: Runtime>(
    host: &mut Host,
    input_data: SingleTxExecutionInput,
) -> Result<(), anyhow::Error> {
    let __attrs = [
        (
            "etherlink.transaction.hash".to_string(),
            tezos_evm_logging::OTelAttrValue::String(format!(
                "{}",
                revm::primitives::B256::from(input_data.tx.tx_hash)
            )),
        ),
        (
            "etherlink.block.number".to_string(),
            tezos_evm_logging::OTelAttrValue::Int(
                input_data.block_number.try_into().unwrap_or_default(),
            ),
        ),
        (
            "etherlink.sbl".to_string(),
            tezos_evm_logging::OTelAttrValue::Bool(true),
        ),
    ];
    __trace_kernel_add_attrs!(host, __attrs);

    let config = get_evm_config(host)?;
    let block_constants =
        block_constants(host, &config, input_data.timestamp, input_data.block_number)?;
    let sequencer_pool_address =
        (block_constants.coinbase != H160::default()).then_some(block_constants.coinbase);

    let mut safe_host = SafeStorage {
        host,
        world_state: OwnedPath::from(ETHERLINK_SAFE_STORAGE_ROOT_PATH),
    };
    let registry = RegistryImpl::new(config.get_chain_id());
    let outbox_queue = OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX)?;

    let mut block_in_progress = match crate::storage::read_block_in_progress(&safe_host)?
    {
        Some(bip) => {
            if input_data.block_number != bip.number {
                return Err(anyhow!(
                    "Critical: Transaction and BIP block numbers do not match"
                ));
            }
            bip
        }
        None => {
            safe_host.start()?;

            BlockInProgress {
                number: input_data.block_number,
                tx_queue: VecDeque::new(),
                valid_txs: Vec::new(),
                delayed_txs: Vec::new(),
                cumulative_gas: U256::zero(),
                index: 0,
                parent_hash: read_current_block_hash(&safe_host)?,
                logs_bloom: Bloom::default(),
                logs_offset: 0,
                timestamp: input_data.timestamp,
                base_fee_per_gas: block_constants.base_fee_per_gas(),
                cumulative_execution_gas: U256::zero(),
                cumulative_receipts: Vec::new(),
                cumulative_tx_objects: Vec::new(),
            }
        }
    };

    block_in_progress.repush_tx(input_data.tx);

    let result = compute(
        &mut safe_host,
        &registry,
        &outbox_queue,
        &mut block_in_progress,
        &block_constants,
        sequencer_pool_address,
        &config.limits,
        None,
        &config.spec_id,
    )?;

    match result {
        BlockInProgressComputationResult::RebootNeeded => Err(anyhow!(
            "Critical: Reboot is required by a single transaction execution"
        )),
        BlockInProgressComputationResult::Finished { .. } => {
            storage::store_block_in_progress(&mut safe_host, &block_in_progress)?;
            block_storage::store_current_transactions_receipts(
                &mut safe_host,
                &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
                &block_in_progress.cumulative_receipts,
            )?;
            Ok(())
        }
    }
}

fn read_current_block_hash<Host: Runtime>(host: &Host) -> Result<H256, Error> {
    match block_storage::read_current_hash(host, &ETHERLINK_SAFE_STORAGE_ROOT_PATH) {
        Ok(block_hash) => Ok(block_hash),
        Err(Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))) => {
            Ok(H256::zero())
        }
        Err(err) => Err(err),
    }
}

#[trace_kernel]
pub fn assemble_block<Host: Runtime>(
    host: &mut Host,
    input_data: AssembleBlockInput,
) -> Result<(), anyhow::Error> {
    let __attrs = [
        (
            "etherlink.block.number".to_string(),
            tezos_evm_logging::OTelAttrValue::Int(
                input_data.block_number.try_into().unwrap_or_default(),
            ),
        ),
        (
            "etherlink.sbl".to_string(),
            tezos_evm_logging::OTelAttrValue::Bool(true),
        ),
    ];
    __trace_kernel_add_attrs!(host, __attrs);

    let config = get_evm_config(host)?;
    let block_constants =
        block_constants(host, &config, input_data.timestamp, input_data.block_number)?;

    let mut configuration = fetch_configuration(host);
    let mut safe_host = SafeStorage {
        host,
        world_state: OwnedPath::from(ETHERLINK_SAFE_STORAGE_ROOT_PATH),
    };
    let outbox_queue = OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX)?;
    let block_in_progress = crate::storage::read_block_in_progress(&safe_host)?
        .ok_or_else(|| anyhow!("Critical: BIP is not available for assemble block"))?;
    let delayed_hashes = block_in_progress.delayed_txs.clone();
    let block = block_in_progress.finalize_and_store(&mut safe_host, &block_constants)?;

    let timestamp = block.timestamp();
    promote_block(
        &mut safe_host,
        &outbox_queue,
        &BlockInProgressProvenance::Storage,
        block.header(),
        &mut configuration,
        delayed_hashes,
    )?;
    upgrade::possible_sequencer_key_change(safe_host.host, timestamp)?;

    Ok(())
}
