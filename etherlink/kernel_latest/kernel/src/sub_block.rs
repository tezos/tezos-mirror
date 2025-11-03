// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    apply::{pure_fa_deposit, revm_run_transaction},
    bridge::{execute_deposit, DepositResult},
    chains::{ChainConfigTrait, EvmChainConfig},
    configuration::fetch_pure_evm_config,
    error::Error,
    gas_price::base_fee_per_gas,
    retrieve_chain_id, retrieve_da_fee,
    storage::read_sequencer_pool_address,
    transaction::{Transaction, TransactionContent},
};
use ethereum::Log;
use primitive_types::{H160, H256, U256};
use revm::context::result::{ExecutionResult, Output};
use revm_etherlink::{precompiles::constants::SYSTEM_SOL_ADDR, ExecutionOutcome};
use rlp::{Decodable, Encodable};
use tezos_ethereum::{
    block::{BlockConstants, BlockFees},
    rlp_helpers::{
        append_timestamp, decode_field, decode_field_u256_le, decode_timestamp, next,
        FromRlpBytes,
    },
    transaction::{
        IndexedLog, TransactionHash, TransactionReceipt, TransactionStatus,
        TransactionType,
    },
};
use tezos_evm_runtime::{runtime::Runtime, safe_storage::SafeStorage};
use tezos_indexable_storage::IndexableStorage;
use tezos_smart_rollup::{host::RuntimeError, types::Timestamp};
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};

const SINGLE_TX_EXECUTION_INPUT: RefPath =
    RefPath::assert_from(b"/evm/world_state/single_tx/input_tx");
const SINGLE_TX_EXECUTION_RECEIPTS: RefPath =
    RefPath::assert_from(b"/evm/world_state/single_tx/receipts");

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

fn get_evm_safe_host<'a, Host: Runtime>(
    host: &'a mut Host,
    config: &EvmChainConfig,
) -> SafeStorage<&'a mut Host> {
    SafeStorage {
        host,
        world_state: OwnedPath::from(&config.storage_root_path()),
    }
}

fn get_receipt_index<Host: Runtime>(host: &mut Host) -> Result<u64, Error> {
    let receipts_storage =
        IndexableStorage::new_owned_path(SINGLE_TX_EXECUTION_RECEIPTS.into());

    let length = receipts_storage.length(host)?;

    Ok(length)
}

struct IterReceiptData {
    last_log: usize,
    current_cumulative_gas: U256,
}

fn get_iter_receipt_data<Host: Runtime>(
    host: &mut Host,
    nb_receipts: u64,
) -> Result<IterReceiptData, Error> {
    let receipts_storage =
        IndexableStorage::new_owned_path(SINGLE_TX_EXECUTION_RECEIPTS.into());
    let mut current_cumulative_gas = U256::zero();
    let mut last_log = 0;

    for i in 0..nb_receipts {
        let receipt_bytes = receipts_storage.get_value(host, i)?;
        let receipt: TransactionReceipt = rlp::decode(&receipt_bytes)?;
        current_cumulative_gas += receipt.cumulative_gas_used;
        last_log += receipt.logs.len();
    }

    Ok(IterReceiptData {
        last_log,
        current_cumulative_gas,
    })
}

fn store_receipt<Host: Runtime>(
    host: &mut Host,
    receipt: TransactionReceipt,
) -> Result<(), Error> {
    let encoded_receipt = rlp::encode(&receipt);

    let speculative_receipts_storage =
        IndexableStorage::new_owned_path(SINGLE_TX_EXECUTION_RECEIPTS.into());

    speculative_receipts_storage.push_value(host, &encoded_receipt)?;

    Ok(())
}

struct ReceiptData {
    to: Option<H160>,
    type_: TransactionType,
}

fn handle_receipt<Host: Runtime>(
    host: &mut Host,
    block_constants: &BlockConstants,
    from: H160,
    receipt_data: ReceiptData,
    hash: TransactionHash,
    result: ExecutionResult,
) -> Result<(), Error> {
    let (status, contract_address, logs, gas_used) = match result {
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
            (TransactionStatus::Success, contract_address, logs, gas_used)
        }
        ExecutionResult::Revert { gas_used, .. }
        | ExecutionResult::Halt { gas_used, .. } => {
            (TransactionStatus::Failure, None, vec![], gas_used)
        }
    };

    let index = get_receipt_index(host)?;
    let IterReceiptData {
        last_log,
        current_cumulative_gas,
    } = get_iter_receipt_data(host, index)?;

    let logs: Vec<IndexedLog> = logs
        .into_iter()
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
            index: (last_log + 1).try_into().unwrap_or(u64::MAX),
        })
        .collect();

    let logs_bloom = TransactionReceipt::logs_to_bloom(&logs);

    let receipt = TransactionReceipt {
        hash,
        index: index.try_into().unwrap_or(u32::MAX),
        block_number: block_constants.number,
        from,
        to: receipt_data.to,
        cumulative_gas_used: current_cumulative_gas + gas_used,
        effective_gas_price: block_constants.base_fee_per_gas(),
        gas_used: gas_used.into(),
        contract_address,
        logs,
        logs_bloom,
        type_: receipt_data.type_,
        status,
    };

    store_receipt(host, receipt)
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
        gas_limit: config.get_limits().maximum_gas_limit,
        block_fees,
        chain_id: config.get_chain_id(),
        prevrandao: None,
    })
}

struct RunOutcome {
    result: ExecutionResult,
    caller: H160,
    receipt_data: ReceiptData,
}

pub fn handle_run_transaction<Host: Runtime>(
    host: &mut Host,
    input_data: SingleTxExecutionInput,
) -> Result<(), Error> {
    let config = get_evm_config(host)?;
    // Safe storage isn't necessary as it can be managed by the node
    // but using it on kernel allow us to make cache and in-memory in it.
    // See !19515.
    let mut safe_host = get_evm_safe_host(host, &config);
    safe_host.start()?;
    let mut block_constants = block_constants(
        &mut safe_host,
        &config,
        input_data.timestamp,
        input_data.block_number,
    )?;
    block_constants.timestamp = input_data.timestamp.as_u64().into();
    block_constants.number = input_data.block_number;

    let RunOutcome {
        result,
        caller,
        receipt_data,
    } = match &input_data.tx.content {
        TransactionContent::Ethereum(ethx)
        | TransactionContent::EthereumDelayed(ethx) => {
            let caller = ethx.caller().map_err(|_| Error::InvalidSignatureCheck)?;
            let ExecutionOutcome { result, .. } = revm_run_transaction(
                &mut safe_host,
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
            }
        }
        TransactionContent::Deposit(deposit) => {
            let DepositResult { outcome, .. } = execute_deposit(&mut safe_host, deposit)
                .map_err(|e| {
                    Error::InvalidRunTransaction(revm_etherlink::Error::Custom(
                        e.to_string(),
                    ))
                })?;

            RunOutcome {
                result: outcome.result,
                caller: H160::from(SYSTEM_SOL_ADDR.into_array()),
                receipt_data: ReceiptData {
                    to: Some(deposit.receiver),
                    type_: TransactionType::Legacy,
                },
            }
        }
        TransactionContent::FaDeposit(fa_deposit) => {
            let outcome = pure_fa_deposit(
                &mut safe_host,
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
            }
        }
    };

    // Don't pass `safe_host` because we don't want this to be cached
    // but we still want it to be accessible by the node.
    handle_receipt(
        host,
        &block_constants,
        caller,
        receipt_data,
        input_data.tx.tx_hash,
        result,
    )
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{
        storage::store_chain_id,
        sub_block::{
            SingleTxExecutionInput, SINGLE_TX_EXECUTION_INPUT,
            SINGLE_TX_EXECUTION_RECEIPTS,
        },
    };
    use alloy_primitives::{keccak256, Address};
    use primitive_types::U256;
    use revm_etherlink::{
        helpers::legacy::{alloy_to_h160, u256_to_alloy},
        storage::world_state_handler::StorageAccount,
    };
    use rlp::{Decodable, Rlp};
    use tezos_ethereum::{
        transaction::{TransactionReceipt, TransactionType},
        tx_common::EthereumTransactionCommon,
    };
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};
    use tezos_indexable_storage::IndexableStorage;
    use tezos_smart_rollup::types::Timestamp;
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
    ) -> Result<(), tezos_smart_rollup::host::RuntimeError> {
        let encoded_tx = rlp::encode(tx);
        host.store_write_all(&SINGLE_TX_EXECUTION_INPUT, encoded_tx.as_ref())
    }

    #[test]
    fn basic_single_tx_execution() {
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
        store_tx_execution_input(&mut mock_host, &single_tx_input).unwrap();

        let read_input = crate::sub_block::read_single_tx_execution_input(&mut mock_host)
            .unwrap()
            .unwrap();
        assert_eq!(read_input.tx.tx_hash, single_tx_input.tx.tx_hash);
        assert_eq!(read_input.timestamp, single_tx_input.timestamp);
        assert_eq!(read_input.block_number, single_tx_input.block_number);
        crate::sub_block::handle_run_transaction(&mut mock_host, read_input).unwrap();

        let receipts_storage =
            IndexableStorage::new_owned_path(SINGLE_TX_EXECUTION_RECEIPTS.into());
        assert_eq!(receipts_storage.length(&mock_host).unwrap(), 1);
        let receipt = TransactionReceipt::decode(&Rlp::new(
            &receipts_storage.get_value(&mock_host, 0).unwrap(),
        ))
        .unwrap();
        assert_eq!(receipt.block_number, block_number);
        assert_eq!(receipt.hash, tx_hash);
        assert!(receipt.cumulative_gas_used > U256::zero());
    }
}
