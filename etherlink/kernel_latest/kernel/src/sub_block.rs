// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    apply::{pure_fa_deposit, revm_run_transaction},
    block::GAS_LIMIT,
    block_storage,
    bridge::{execute_etherlink_deposit, DepositResult},
    chains::{ChainConfigTrait, EvmChainConfig, ETHERLINK_SAFE_STORAGE_ROOT_PATH},
    configuration::fetch_pure_evm_config,
    error::{Error, StorageError, TransferError},
    gas_price::base_fee_per_gas,
    l2block::L2Block,
    retrieve_chain_id, retrieve_da_fee,
    storage::read_sequencer_pool_address,
    transaction::{Transaction, TransactionContent},
};
use alloy_consensus::{proofs::ordered_trie_root_with_encoder, EMPTY_ROOT_HASH};
use ethbloom::Bloom;
use ethereum::Log;
use primitive_types::{H160, H256, U256};
use revm::context::result::{ExecutionResult, Output};
use revm_etherlink::{
    precompiles::constants::SYSTEM_SOL_ADDR,
    storage::world_state_handler::EVM_ACCOUNTS_PATH, ExecutionOutcome,
};
use rlp::{Decodable, Encodable};
use tezos_ethereum::{
    block::{BlockConstants, BlockFees, EthBlock},
    rlp_helpers::{
        append_timestamp, decode_field, decode_field_u256_le, decode_timestamp, next,
        FromRlpBytes,
    },
    transaction::{
        IndexedLog, TransactionObject, TransactionReceipt, TransactionStatus,
        TransactionType, TRANSACTION_HASH_SIZE,
    },
};
use tezos_evm_logging::__trace_kernel_add_attrs;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::{host::RuntimeError, types::Timestamp};
use tezos_smart_rollup_host::path::RefPath;
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

struct IterReceiptData {
    last_log: usize,
    current_cumulative_gas: U256,
}

fn get_iter_receipt_data(receipts: &[TransactionReceipt]) -> IterReceiptData {
    let mut last_log = 0;

    for receipt in receipts {
        last_log += receipt.logs.len();
    }

    let current_cumulative_gas = match receipts.last() {
        Some(TransactionReceipt {
            cumulative_gas_used,
            ..
        }) => *cumulative_gas_used,
        None => U256::zero(),
    };

    IterReceiptData {
        last_log,
        current_cumulative_gas,
    }
}

fn get_current_transaction_receipts<Host: Runtime>(
    host: &Host,
) -> Result<Vec<TransactionReceipt>, Error> {
    match block_storage::read_current_transactions_receipts(
        host,
        &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
    ) {
        Ok(receipts) => Ok(receipts),
        Err(Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))) => {
            Ok(vec![])
        }
        Err(err) => Err(err),
    }
}

fn get_current_transactions_objects<Host: Runtime>(
    host: &Host,
) -> Result<Vec<TransactionObject>, Error> {
    match block_storage::read_current_transactions_objects(
        host,
        &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
    ) {
        Ok(transactions_objects) => Ok(transactions_objects),
        Err(Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))) => {
            Ok(vec![])
        }
        Err(err) => Err(err),
    }
}

fn store_current_transaction_receipts<Host: Runtime>(
    host: &mut Host,
    receipts: &[TransactionReceipt],
) -> Result<(), Error> {
    block_storage::store_current_transactions_receipts(
        host,
        &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
        receipts,
    )
}

fn store_current_transactions_objects<Host: Runtime>(
    host: &mut Host,
    transactions_objects: &[TransactionObject],
) -> Result<(), Error> {
    block_storage::store_current_transactions_objects(
        host,
        &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
        transactions_objects,
    )
}

struct ReceiptData {
    to: Option<H160>,
    type_: TransactionType,
}

struct ResultData {
    status: TransactionStatus,
    contract_address: Option<H160>,
    logs: Vec<revm::primitives::Log>,
    gas_used: u64,
}

fn get_result_data(result: ExecutionResult) -> ResultData {
    match result {
        ExecutionResult::Success {
            gas_used,
            logs,
            output,
            ..
        } => {
            let contract_address = match output {
                Output::Call(_) => None,
                Output::Create(_, address) => address.map(|addr| H160(*addr.0)),
            };

            ResultData {
                status: TransactionStatus::Success,
                contract_address,
                logs,
                gas_used,
            }
        }
        ExecutionResult::Revert { gas_used, .. }
        | ExecutionResult::Halt { gas_used, .. } => ResultData {
            status: TransactionStatus::Failure,
            contract_address: None,
            logs: vec![],
            gas_used,
        },
    }
}

#[trace_kernel]
fn handle_receipt<Host: Runtime>(
    host: &mut Host,
    input_data: &SingleTxExecutionInput,
    effective_gas_price: U256,
    from: H160,
    receipt_data: ReceiptData,
    result_data: &ResultData,
) -> Result<(), Error> {
    let mut receipts = get_current_transaction_receipts(host)?;

    // We might be handling a new block since last execution.
    // In this case, we need to empty the receipts before adding the
    // new one.
    if !receipts.is_empty()
        && receipts.first().unwrap().block_number != input_data.block_number
    {
        receipts = vec![];
    }

    let IterReceiptData {
        last_log,
        current_cumulative_gas,
    } = get_iter_receipt_data(&receipts);

    let log_index = (last_log + 1)
        .try_into()
        .map_err(|_| Error::InvalidConversion)?;

    let logs: Vec<IndexedLog> = result_data
        .logs
        .iter()
        .map(|revm::primitives::Log { address, data }| IndexedLog {
            log: Log {
                address: H160(*address.0),
                topics: data
                    .topics()
                    .iter()
                    .map(|raw_topic| H256(raw_topic.0))
                    .collect(),
                data: data.data.to_vec(),
            },
            index: log_index,
        })
        .collect();

    let logs_bloom = TransactionReceipt::logs_to_bloom(&logs);

    let receipt = TransactionReceipt {
        hash: input_data.tx.tx_hash,
        index: receipts
            .len()
            .try_into()
            .map_err(|_| Error::InvalidConversion)?,
        block_number: input_data.block_number,
        from,
        to: receipt_data.to,
        cumulative_gas_used: current_cumulative_gas + result_data.gas_used,
        effective_gas_price,
        gas_used: result_data.gas_used.into(),
        contract_address: result_data.contract_address,
        logs,
        logs_bloom,
        type_: receipt_data.type_,
        status: result_data.status,
    };

    receipts.push(receipt);

    store_current_transaction_receipts(host, &receipts)
}

#[trace_kernel]
fn handle_transaction_object<Host: Runtime>(
    host: &mut Host,
    from: H160,
    input_data: &SingleTxExecutionInput,
    gas_price: U256,
    gas: U256,
) -> Result<(), Error> {
    let mut txs_objects = get_current_transactions_objects(host)?;

    // We might be handling a new block since last execution.
    // In this case, we need to empty the txs objects before adding the
    // new one.
    if !txs_objects.is_empty()
        && txs_objects.first().unwrap().block_number != input_data.block_number
    {
        txs_objects = vec![];
    }

    let tx_object = TransactionObject {
        block_number: input_data.block_number,
        from,
        gas_used: gas,
        gas_price,
        hash: input_data.tx.tx_hash,
        input: input_data.tx.data(),
        nonce: input_data.tx.nonce(),
        to: input_data.tx.to()?,
        index: txs_objects
            .len()
            .try_into()
            .map_err(|_| Error::InvalidConversion)?,
        value: input_data.tx.value(),
        signature: input_data.tx.signature(),
    };

    txs_objects.push(tx_object);

    store_current_transactions_objects(host, &txs_objects)
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
        block_fees,
        chain_id: config.get_chain_id(),
        prevrandao: None,
    })
}

struct RunOutcome {
    result: ExecutionResult,
    caller: H160,
    receipt_data: ReceiptData,
    gas: Option<U256>,
}

#[trace_kernel]
pub fn handle_run_transaction<Host: Runtime>(
    host: &mut Host,
    input_data: SingleTxExecutionInput,
) -> Result<(), Error> {
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

    let RunOutcome {
        result,
        caller,
        receipt_data,
        gas,
    } = match &input_data.tx.content {
        TransactionContent::Ethereum(ethx)
        | TransactionContent::EthereumDelayed(ethx) => {
            let caller = ethx.caller().map_err(|_| Error::InvalidSignatureCheck)?;
            let ExecutionOutcome { result, .. } = revm_run_transaction(
                host,
                &block_constants,
                Some(input_data.tx.tx_hash),
                caller,
                ethx.to,
                ethx.value,
                ethx.data.clone(),
                ethx.gas_limit,
                block_constants.base_fee_per_gas(),
                config.limits.maximum_gas_limit,
                ethx.access_list.clone(),
                ethx.authorization_list.clone(),
                &config.spec_id,
                None,
                false,
            )
            .map_err(|err| {
                Error::InvalidRunTransaction(revm_etherlink::Error::Custom(
                    err.to_string(),
                ))
            })?;

            RunOutcome {
                result,
                caller,
                receipt_data: ReceiptData {
                    to: ethx.to,
                    type_: ethx.type_,
                },
                gas: Some(ethx.gas_limit_with_fees().into()),
            }
        }
        TransactionContent::Deposit(deposit) => {
            let DepositResult { outcome, .. } = execute_etherlink_deposit(host, deposit)
                .map_err(|e| {
                    Error::InvalidRunTransaction(revm_etherlink::Error::Custom(
                        e.to_string(),
                    ))
                })?;

            let receiver = deposit
                .receiver
                .to_h160()
                .map_err(|_| Error::InvalidConversion)?;
            RunOutcome {
                result: outcome.result,
                caller: H160::from(SYSTEM_SOL_ADDR.into_array()),
                receipt_data: ReceiptData {
                    to: Some(receiver),
                    type_: TransactionType::Legacy,
                },
                gas: None,
            }
        }
        TransactionContent::FaDeposit(fa_deposit) => {
            let outcome = pure_fa_deposit(
                host,
                fa_deposit,
                &block_constants,
                input_data.tx.tx_hash,
                config.limits.maximum_gas_limit,
                &config.spec_id,
                None,
            )?;

            RunOutcome {
                result: outcome.result,
                caller: H160::from(SYSTEM_SOL_ADDR.into_array()),
                receipt_data: ReceiptData {
                    to: Some(fa_deposit.receiver),
                    type_: TransactionType::Legacy,
                },
                gas: None,
            }
        }
    };

    let result_data = get_result_data(result);

    let fee_updates = input_data
        .tx
        .content
        .fee_updates(&block_constants.block_fees, result_data.gas_used.into());

    fee_updates
        .apply(host, caller, Some(block_constants.coinbase))
        .map_err(|_| {
            Error::Transfer(TransferError::Custom(
                "Applying fees to the sequencer pool address failed",
            ))
        })?;

    handle_receipt(
        host,
        &input_data,
        block_constants.base_fee_per_gas(),
        caller,
        receipt_data,
        &result_data,
    )?;

    handle_transaction_object(
        host,
        caller,
        &input_data,
        block_constants.base_fee_per_gas(),
        match gas {
            Some(gas) => gas,
            None => fee_updates.overall_gas_used,
        },
    )
}

fn state_root_hash<Host: Runtime>(host: &mut Host) -> Result<Vec<u8>, Error> {
    match host.store_get_hash(&EVM_ACCOUNTS_PATH) {
        Ok(hash) => Ok(hash),
        _ => Ok(vec![0u8; 32]),
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

fn receipts_root(receipts: &[TransactionReceipt]) -> Vec<u8> {
    if receipts.is_empty() {
        EMPTY_ROOT_HASH.to_vec()
    } else {
        ordered_trie_root_with_encoder(receipts, |obj, buf| obj.encode_2718(buf)).to_vec()
    }
}

fn objects_root(objects: &[TransactionObject]) -> Vec<u8> {
    if objects.is_empty() {
        EMPTY_ROOT_HASH.to_vec()
    } else {
        ordered_trie_root_with_encoder(objects, |obj, buf| obj.encode_2718(buf)).to_vec()
    }
}

fn get_block_receipt_info(receipts: &[TransactionReceipt]) -> BlockReceiptInfo {
    let mut valid_txs = vec![];
    let mut logs_bloom = Bloom::default();
    let mut cumulative_gas = U256::zero();

    for receipt in receipts {
        valid_txs.push(receipt.hash);
        logs_bloom.accrue_bloom(&receipt.logs_bloom);
        cumulative_gas += receipt.gas_used;
    }

    BlockReceiptInfo {
        valid_txs,
        logs_bloom,
        cumulative_gas,
    }
}

#[derive(Default)]
struct BlockReceiptInfo {
    valid_txs: Vec<[u8; TRANSACTION_HASH_SIZE]>,
    logs_bloom: Bloom,
    cumulative_gas: U256,
}

#[trace_kernel]
pub fn assemble_block<Host: Runtime>(
    host: &mut Host,
    input_data: AssembleBlockInput,
) -> Result<(), Error> {
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
    let state_root = state_root_hash(host)?;
    let receipts = get_current_transaction_receipts(host)?;
    let objects = get_current_transactions_objects(host)?;
    let receipts_root = receipts_root(&receipts);
    let transactions_root = objects_root(&objects);
    let parent_hash = read_current_block_hash(host)?;
    let block_constants =
        block_constants(host, &config, input_data.timestamp, input_data.block_number)?;
    let base_fee_per_gas = base_fee_per_gas(
        host,
        input_data.timestamp,
        block_constants.block_fees.minimum_base_fee_per_gas(),
    );

    let BlockReceiptInfo {
        valid_txs,
        logs_bloom,
        cumulative_gas,
    } = get_block_receipt_info(&receipts);

    let sub_block = EthBlock::new(
        input_data.block_number,
        valid_txs,
        input_data.timestamp,
        parent_hash,
        logs_bloom,
        transactions_root,
        state_root,
        receipts_root,
        cumulative_gas,
        &block_constants,
        base_fee_per_gas,
    );

    block_storage::store_current(
        host,
        &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
        &L2Block::Etherlink(Box::new(sub_block)),
    )?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{
        block_storage,
        chains::{ChainFamily, ETHERLINK_SAFE_STORAGE_ROOT_PATH},
        l2block::L2Block,
        storage::store_chain_id,
        sub_block::{
            assemble_block, get_current_transaction_receipts,
            get_current_transactions_objects, handle_run_transaction,
            read_assemble_block_input, read_single_tx_execution_input,
            AssembleBlockInput, SingleTxExecutionInput, ASSEMBLE_BLOCK_INPUT,
            SINGLE_TX_EXECUTION_INPUT,
        },
    };
    use alloy_primitives::{keccak256, Address};
    use primitive_types::U256;
    use revm_etherlink::{
        helpers::legacy::{alloy_to_h160, u256_to_alloy},
        storage::world_state_handler::StorageAccount,
    };
    use tezos_ethereum::{
        transaction::TransactionType, tx_common::EthereumTransactionCommon,
    };
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};
    use tezos_smart_rollup::{host::RuntimeError, types::Timestamp};
    use tezos_smart_rollup_host::runtime::Runtime as SdkRuntime; // Used to put traits interface in the scope
    const DUMMY_CHAIN_ID: U256 = U256::one();

    fn set_balance<Host: Runtime>(host: &mut Host, address: &Address, balance: U256) {
        let mut account = StorageAccount::from_address(address).unwrap();
        let mut info = account.info(host).unwrap();
        info.balance = u256_to_alloy(&balance);
        account.set_info(host, info).unwrap();
    }

    fn store_tx_execution_input(
        host: &mut MockKernelHost,
        tx: &SingleTxExecutionInput,
    ) -> Result<(), RuntimeError> {
        let encoded_tx = rlp::encode(tx);
        host.store_write_all(&SINGLE_TX_EXECUTION_INPUT, &encoded_tx)
    }

    fn store_assemble_block_input(
        host: &mut MockKernelHost,
        assemble_block_input: &AssembleBlockInput,
    ) -> Result<(), RuntimeError> {
        let encoded_input = rlp::encode(assemble_block_input);
        host.store_write_all(&ASSEMBLE_BLOCK_INPUT, &encoded_input)
    }

    // This test will:
    // * execute a single isolated transaction
    // * check for the receipt and object under the current block
    // * assemble the block
    // * check that the receipt/object are consistent
    #[test]
    fn base_sub_block_flow() {
        let mut mock_host = MockKernelHost::default();
        let sender =
            Address::from_str("0xaf1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        set_balance(&mut mock_host, &sender, sender_initial_balance);
        store_chain_id(&mut mock_host, DUMMY_CHAIN_ID).unwrap();
        let gas_price = U256::from(40000000000u64);
        let receiver =
            Address::from_str("0xff00000000000000000000000000000000000001").unwrap();
        let block_number = U256::from(42u64);
        let timestamp = Timestamp::from(10);

        let tx = EthereumTransactionCommon::new(
            TransactionType::Legacy,
            Some(DUMMY_CHAIN_ID),
            0,
            gas_price,
            gas_price,
            30_000_000,
            Some(alloy_to_h160(&receiver)),
            U256::from(1000000000000000000u64),
            vec![],
            vec![],
            None,
            None,
        );
        // corresponding caller's address is 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        let tx = tx
            .sign_transaction(
                "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701"
                    .to_string(),
            )
            .unwrap();
        let tx_hash = keccak256(tx.to_bytes());
        let single_tx_input = SingleTxExecutionInput {
            tx: crate::transaction::Transaction {
                content: crate::transaction::TransactionContent::Ethereum(tx),
                tx_hash: *tx_hash,
            },
            timestamp,
            block_number,
        };
        let assemble_block_input = AssembleBlockInput {
            timestamp,
            block_number,
        };
        store_tx_execution_input(&mut mock_host, &single_tx_input).unwrap();
        store_assemble_block_input(&mut mock_host, &assemble_block_input).unwrap();

        let read_single_tx_input = read_single_tx_execution_input(&mut mock_host)
            .unwrap()
            .unwrap();
        assert_eq!(read_single_tx_input.tx.tx_hash, single_tx_input.tx.tx_hash);
        assert_eq!(read_single_tx_input.timestamp, single_tx_input.timestamp);
        assert_eq!(
            read_single_tx_input.block_number,
            single_tx_input.block_number
        );

        // Execute single isolated transaction
        handle_run_transaction(&mut mock_host, read_single_tx_input).unwrap();

        // Check for receipts and objects
        let receipts = get_current_transaction_receipts(&mock_host).unwrap();
        assert_eq!(receipts.len(), 1);
        let receipt = receipts.first().unwrap();
        assert_eq!(receipt.block_number, block_number);
        assert_eq!(receipt.hash, tx_hash);
        assert!(receipt.cumulative_gas_used > U256::zero());

        let objects = get_current_transactions_objects(&mock_host).unwrap();
        assert_eq!(objects.len(), 1);
        let object = objects.first().unwrap();
        assert_eq!(object.block_number, block_number);
        assert_eq!(object.hash, tx_hash);
        assert!(object.gas_used > U256::zero());

        // Assemble block
        let read_assemble_block_input =
            read_assemble_block_input(&mut mock_host).unwrap().unwrap();
        assemble_block(&mut mock_host, read_assemble_block_input).unwrap();

        // Check that current block is consistent with receipts and objects
        let current_block = block_storage::read_current(
            &mut mock_host,
            &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
            &ChainFamily::Evm,
        )
        .unwrap();

        match current_block {
            L2Block::Etherlink(eth_block) => {
                let nb_txs = eth_block.transactions.len();
                assert_eq!(nb_txs, receipts.len());
                assert_eq!(nb_txs, objects.len());
                let first_tx = eth_block.transactions.first().unwrap();
                assert_eq!(first_tx, &receipt.hash);
                assert_eq!(first_tx, &object.hash);
                assert!(eth_block.gas_used > U256::zero());
            }
            L2Block::Tezlink(_) => panic!("Etherlink block was expected"),
        }
    }
}
