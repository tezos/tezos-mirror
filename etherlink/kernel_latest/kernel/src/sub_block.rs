// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::collections::VecDeque;

use crate::{
    apply::WITHDRAWAL_OUTBOX_QUEUE,
    block::{compute, record_applied_block, BlockInProgressComputationResult, GAS_LIMIT},
    block_in_progress::BlockInProgress,
    block_storage,
    chains::{
        TezlinkBlockConstants, TezosXBlockConstants, TezosXChainConfig, TezosXTransaction,
    },
    configuration::{fetch_configuration, fetch_tezosx_configuration},
    error::Error,
    fees::DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER,
    gas_price::base_fee_per_gas,
    retrieve_da_fee,
    storage::{self, read_michelson_to_evm_gas_multiplier, read_sequencer_pool_address},
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
use tezos_evm_runtime::extensions::WithGas;
use tezos_evm_runtime::runtime_keyspaces::{
    BlueprintKeyspaces, RuntimeKeyspaces, COMMON_WORLD_STATE_KEYSPACE_NAME,
};
use tezos_evm_runtime::safe_storage::SafeStorage;
use tezos_smart_rollup::{outbox::OutboxQueue, types::Timestamp};
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::wasm::WasmHost;
use tezos_smart_rollup_keyspace::{Key, KeySpace, KeySpaceLoader};
use tezos_tezlink::block::OperationsWithReceipts;
use tezos_tracing::trace_kernel;
use tezosx_types::Wei;

// Key to the single-transaction execution input, inside the `/base` keyspace.
// Resolves to the durable path `/base/instant_confirmation/input_tx`.
const SINGLE_TX_EXECUTION_INPUT_KEY: Key =
    Key::from_static(b"/instant_confirmation/input_tx");

// Key to the assemble-block input, inside the `/base` keyspace. Resolves to the
// durable path `/base/instant_confirmation/assemble_block/input`.
const ASSEMBLE_BLOCK_INPUT_KEY: Key =
    Key::from_static(b"/instant_confirmation/assemble_block/input");

pub struct SingleTxExecutionInput {
    pub tx: TezosXTransaction,
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
        let tx = decode_field(&next(&mut it)?, "TezosXTransaction")?;
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

pub fn read_assemble_block_input(
    base: &mut impl KeySpace,
) -> Result<Option<AssembleBlockInput>, Error> {
    match base.get(&ASSEMBLE_BLOCK_INPUT_KEY) {
        Some(bytes) => {
            let input = AssembleBlockInput::from_rlp_bytes(&bytes)?;
            base.delete(&ASSEMBLE_BLOCK_INPUT_KEY);
            Ok(Some(input))
        }
        None => Ok(None),
    }
}

pub fn read_single_tx_execution_input(
    base: &mut impl KeySpace,
) -> Result<Option<SingleTxExecutionInput>, Error> {
    match base.get(&SINGLE_TX_EXECUTION_INPUT_KEY) {
        Some(bytes) => {
            let input = SingleTxExecutionInput::from_rlp_bytes(&bytes)?;
            base.delete(&SINGLE_TX_EXECUTION_INPUT_KEY);
            Ok(Some(input))
        }
        None => Ok(None),
    }
}

fn block_constants(
    bk: &mut BlueprintKeyspaces<impl KeySpace>,
    config: &TezosXChainConfig,
    timestamp: Timestamp,
    number: U256,
) -> Result<TezosXBlockConstants, Error> {
    let coinbase = read_sequencer_pool_address(bk.evm_world_state()).unwrap_or_default();
    let da_fee_per_byte = retrieve_da_fee(bk.evm_world_state_mut())?;
    let minimum_base_fee_per_gas = config.get_limits().minimum_base_fee_per_gas;
    let base_fee_per_gas =
        base_fee_per_gas(bk.common_world_state(), timestamp, minimum_base_fee_per_gas);
    let block_fees =
        BlockFees::new(minimum_base_fee_per_gas, base_fee_per_gas, da_fee_per_byte);
    // The fee per byte is read as the amount one byte costs.
    let da_fee_per_byte_mutez = Wei::from_u256(da_fee_per_byte)
        .to_mutez_exact()
        .map_err(|_| Error::InvalidConversion)?;
    let michelson_to_evm_gas_multiplier =
        read_michelson_to_evm_gas_multiplier(bk.evm_world_state())
            .unwrap_or(DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER);
    let michelson_runtime_block_constants = TezlinkBlockConstants {
        level: number.try_into()?,
        da_fee_per_byte_mutez,
        michelson_to_evm_gas_multiplier,
    };
    Ok(TezosXBlockConstants {
        evm_runtime_block_constants: BlockConstants {
            number,
            coinbase,
            timestamp: timestamp.as_u64().into(),
            gas_limit: GAS_LIMIT,
            tezos_experimental_features: config.is_tezos_runtime_enabled(number),
            block_fees,
            chain_id: config.get_evm_chain_id(),
            prevrandao: None,
            spec_id: config.spec_id,
        },
        michelson_runtime_block_constants,
    })
}

#[trace_kernel]
pub fn handle_run_transaction<Host>(
    host: &mut Host,
    mut bk: BlueprintKeyspaces<impl KeySpace>,
    input_data: SingleTxExecutionInput,
) -> Result<(), anyhow::Error>
where
    Host: StorageV1 + WithGas + KeySpaceLoader,
{
    let __attrs = [
        (
            "etherlink.transaction.hash".to_string(),
            tezos_evm_logging::OTelAttrValue::String(format!(
                "{}",
                revm::primitives::B256::from(input_data.tx.tx_hash())
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
    __trace_kernel_add_attrs!(__attrs);

    let config = fetch_tezosx_configuration(&mut bk)?;
    let block_constants = block_constants(
        &mut bk,
        &config,
        input_data.timestamp,
        input_data.block_number,
    )?;
    let sequencer_pool_address = (block_constants.evm_runtime_block_constants.coinbase
        != H160::default())
    .then_some(block_constants.evm_runtime_block_constants.coinbase);

    // Read the HTTP-trace replay flag once, on the live host, as in
    // [block::produce].
    let http_trace_enabled = crate::storage::is_http_trace_enabled(bk.base());

    let mut safe_host =
        SafeStorage::new(host, config.world_states(input_data.block_number));
    let registry = config.init_registry();
    let outbox_queue = OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX)?;

    let stored_block_in_progress = storage::read_block_in_progress(
        &safe_host.load_or_create(COMMON_WORLD_STATE_KEYSPACE_NAME)?,
    )?;
    let mut block_in_progress = match stored_block_in_progress {
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
                michelson_index: 0,
                ethereum_parent_hash: match block_storage::read_current_hash(
                    bk.evm_world_state(),
                ) {
                    Some(hash) => hash,
                    None => H256::zero(),
                },
                logs_bloom: Bloom::default(),
                logs_offset: 0,
                timestamp: input_data.timestamp,
                base_fee_per_gas: block_constants
                    .evm_runtime_block_constants
                    .base_fee_per_gas(),
                cumulative_execution_gas: U256::zero(),
                cumulative_receipts: Vec::new(),
                cumulative_tx_objects: Vec::new(),
                cumulative_tezos_operation_receipts: OperationsWithReceipts::default(),
                tezos_parent_hash: match block_storage::read_current_hash(
                    bk.tez_world_state(),
                ) {
                    Some(hash) => hash,
                    None => H256(*tezos_tezlink::block::TezBlock::genesis_block_hash()),
                },
            }
        }
    };

    block_in_progress.repush_tx(input_data.tx);

    drop(bk);
    let computed = RuntimeKeyspaces::init(&mut safe_host)
        .map_err(anyhow::Error::from)
        .and_then(|mut rk| {
            let computed = compute(
                &mut rk,
                &registry,
                &config,
                &outbox_queue,
                &mut block_in_progress,
                &block_constants,
                sequencer_pool_address,
                None,
                http_trace_enabled,
            )?;

            if let BlockInProgressComputationResult::Finished { .. } = computed {
                block_storage::store_current_transactions_receipts(
                    rk.evm_world_state_mut(),
                    &block_in_progress.cumulative_receipts,
                )?;
            }

            Ok(computed)
        });

    let err = match computed {
        Ok(BlockInProgressComputationResult::Finished { .. }) => {
            let stored = safe_host
                .load_or_create(COMMON_WORLD_STATE_KEYSPACE_NAME)
                .map_err(anyhow::Error::from)
                .and_then(|mut common_world_state| {
                    Ok(storage::store_block_in_progress(
                        &mut common_world_state,
                        &block_in_progress,
                    )?)
                });
            match stored {
                Ok(()) => return Ok(()),
                Err(err) => err,
            }
        }
        Ok(BlockInProgressComputationResult::RebootNeeded) => {
            anyhow!("Critical: Reboot is required by a single transaction execution")
        }
        Err(err) => err,
    };

    // A run that fails, in the transaction or in a write after it, takes the
    // whole block with it: the mirror holds the block in progress and every
    // write since its start.
    match safe_host.revert() {
        Ok(()) => Err(err),
        Err(why) => Err(err.context(why)),
    }
}

#[trace_kernel]
pub fn assemble_block<Host>(
    host: &mut Host,
    mut bk: BlueprintKeyspaces<impl KeySpace>,
    input_data: AssembleBlockInput,
) -> Result<(), anyhow::Error>
where
    Host: StorageV1 + KeySpaceLoader + WasmHost,
{
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
    __trace_kernel_add_attrs!(__attrs);

    let config = fetch_tezosx_configuration(&mut bk)?;
    let block_constants = block_constants(
        &mut bk,
        &config,
        input_data.timestamp,
        input_data.block_number,
    )?;

    let mut configuration = fetch_configuration(&bk);
    let mut base = bk.into_base();
    let mut safe_host =
        SafeStorage::new(host, config.world_states(input_data.block_number));
    let outbox_queue = OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX)?;
    let mut rk = RuntimeKeyspaces::init(&mut safe_host)?;
    let block_in_progress = storage::read_block_in_progress(rk.common_world_state())?
        .ok_or_else(|| anyhow!("Critical: BIP is not available for assemble block"))?;
    let delayed_hashes = block_in_progress.delayed_txs.clone();
    crate::gas_price::register_block(
        rk.common_world_state_mut(),
        block_in_progress.cumulative_execution_gas,
        block_in_progress.timestamp,
        block_in_progress.queue_length(),
    )?;
    let number = block_in_progress.number;
    let block = block_in_progress.finalize_and_store(
        &mut rk,
        &block_constants,
        config.is_tezos_runtime_enabled(number),
    )?;

    let timestamp = block.timestamp();
    // The key change writes into the block mirror, which `promote` moves
    // to `/evm/world_state`. Its error is returned after the promotion: a
    // rejected block fails again on each retry.
    let key_change = upgrade::possible_sequencer_key_change(
        rk.evm_world_state_mut(),
        &mut base,
        timestamp,
    );
    // Drop the block in progress from the mirror so that `promote` does not
    // carry it.
    storage::delete_block_in_progress(rk.common_world_state_mut());
    let safe_host = rk.into_host();
    safe_host.promote()?;
    record_applied_block(
        safe_host.host_mut(),
        &mut base,
        &outbox_queue,
        block.header(),
        &mut configuration,
        delayed_hashes,
    )?;
    key_change?;

    Ok(())
}
