// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::panic::catch_unwind;

use crate::blueprint::Queue;
use crate::error::Error;
use crate::error::StorageError::AccountInitialisation;
use crate::error::TransferError::CumulativeGasUsedOverflow;
use crate::error::TransferError::InvalidCallerAddress;
use crate::storage;
use evm_execution::account_storage::init_account_storage;
use evm_execution::handler::ExecutionOutcome;
use evm_execution::{precompiles, run_transaction};

use tezos_ethereum::address::EthereumAddress;
use tezos_ethereum::transaction::TransactionHash;
use tezos_smart_rollup_host::runtime::Runtime;

use primitive_types::U256;
use tezos_ethereum::block::L2Block;
use tezos_ethereum::transaction::{
    TransactionReceipt, TransactionStatus, TransactionType,
};

struct TransactionReceiptInfo {
    tx_hash: TransactionHash,
    index: u32,
    execution_outcome: Option<ExecutionOutcome>,
    caller: EthereumAddress,
    to: EthereumAddress,
}

fn make_receipt_info(
    tx_hash: TransactionHash,
    index: u32,
    execution_outcome: Option<ExecutionOutcome>,
    caller: EthereumAddress,
    to: EthereumAddress,
) -> TransactionReceiptInfo {
    TransactionReceiptInfo {
        tx_hash,
        index,
        execution_outcome,
        caller,
        to,
    }
}

fn make_receipt(
    block: &L2Block,
    receipt_info: TransactionReceiptInfo,
    cumulative_gas_used: &mut U256,
) -> Result<TransactionReceipt, Error> {
    let hash = receipt_info.tx_hash;
    let index = receipt_info.index;
    let block_hash = block.hash;
    let block_number = block.number;
    let from = receipt_info.caller;
    let to = Some(receipt_info.to);
    let effective_gas_price = block.constants().gas_price;

    let tx_receipt = match receipt_info.execution_outcome {
        Some(outcome) => TransactionReceipt {
            hash,
            index,
            block_hash,
            block_number,
            from,
            to,
            cumulative_gas_used: cumulative_gas_used
                .checked_add(U256::from(outcome.gas_used))
                .ok_or(Error::Transfer(CumulativeGasUsedOverflow))?,
            effective_gas_price,
            gas_used: U256::from(outcome.gas_used),
            contract_address: outcome.new_address,
            type_: TransactionType::Legacy,
            status: if outcome.is_success {
                TransactionStatus::Success
            } else {
                TransactionStatus::Failure
            },
        },
        None => TransactionReceipt {
            hash,
            index,
            block_hash,
            block_number,
            from,
            to,
            cumulative_gas_used: *cumulative_gas_used,
            effective_gas_price,
            gas_used: U256::zero(),
            contract_address: None,
            type_: TransactionType::Legacy,
            status: TransactionStatus::Failure,
        },
    };

    Ok(tx_receipt)
}

fn make_receipts(
    block: &L2Block,
    receipt_infos: Vec<TransactionReceiptInfo>,
) -> Result<Vec<TransactionReceipt>, Error> {
    let mut cumulative_gas_used = U256::zero();
    receipt_infos
        .into_iter()
        .map(|receipt_info| make_receipt(block, receipt_info, &mut cumulative_gas_used))
        .collect()
}

pub fn produce<Host: Runtime>(host: &mut Host, queue: Queue) -> Result<(), Error> {
    let mut current_block = storage::read_current_block(host)?;
    let mut evm_account_storage =
        init_account_storage().map_err(|_| Error::Storage(AccountInitialisation))?;
    let precompiles = precompiles::precompile_set::<Host>();

    for proposal in queue.proposals {
        let mut valid_txs = Vec::new();
        let mut receipts_infos = Vec::new();
        let transactions = proposal.transactions;

        for (transaction, index) in transactions.into_iter().zip(0u32..) {
            // TODO: https://gitlab.com/tezos/tezos/-/issues/5487
            // gas_limit representation should be the same through all modules to avoid
            // odd conversions
            let gas_limit = catch_unwind(|| transaction.tx.gas_limit.as_u64()).ok();
            let caller = transaction
                .tx
                .caller()
                .map_err(|_| Error::Transfer(InvalidCallerAddress))?;
            let receipt_info = match run_transaction(
                host,
                &current_block.constants(),
                &mut evm_account_storage,
                &precompiles,
                transaction.tx.to.into(),
                caller.into(),
                transaction.tx.data,
                gas_limit,
                Some(transaction.tx.value),
            ) {
                Ok(outcome) => {
                    valid_txs.push(transaction.tx_hash);
                    make_receipt_info(
                        transaction.tx_hash,
                        index,
                        Some(outcome),
                        caller,
                        transaction.tx.to,
                    )
                }
                Err(_) => make_receipt_info(
                    transaction.tx_hash,
                    index,
                    None,
                    caller,
                    transaction.tx.to,
                ),
            };
            receipts_infos.push(receipt_info)
        }

        let new_block = L2Block::new(current_block.number + 1, valid_txs);
        storage::store_current_block(host, &new_block)?;
        storage::store_transaction_receipts(
            host,
            &make_receipts(&new_block, receipts_infos)?,
        )?;
        current_block = new_block;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::blueprint::Blueprint;
    use crate::genesis;
    use crate::inbox::Transaction;
    use crate::storage::read_transaction_receipt_status;
    use evm_execution::account_storage::{account_path, EthereumAccountStorage};
    use primitive_types::{H160, H256};
    use std::str::FromStr;
    use tezos_ethereum::address::EthereumAddress;
    use tezos_ethereum::signatures::EthereumTransactionCommon;
    use tezos_ethereum::transaction::{TransactionStatus, TRANSACTION_HASH_SIZE};
    use tezos_smart_rollup_mock::MockHost;

    fn string_to_h256_unsafe(s: &str) -> H256 {
        let mut v: [u8; 32] = [0; 32];
        hex::decode_to_slice(s, &mut v).expect("Could not parse to 256 hex value.");
        H256::from(v)
    }

    fn set_balance(
        host: &mut MockHost,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
        balance: U256,
    ) {
        let mut account = evm_account_storage
            .get_or_create_account(host, &account_path(address).unwrap())
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

    fn dummy_eth_transaction() -> EthereumTransactionCommon {
        // corresponding caller's address is 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        let nonce = U256::from(0);
        let gas_price = U256::from(40000000000u64);
        let gas_limit = U256::from(21000);
        let to =
            EthereumAddress::from("423163e58aabec5daa3dd1130b759d24bef0f6ea".to_string());
        let value = U256::from(5000000000000000u64);
        let data: Vec<u8> = hex::decode("deace8f5000000000000000000000000000000000000000000000000000000000000a4b100000000000000000000000041bca408a6b4029b42883aeb2c25087cab76cb58000000000000000000000000000000000000000000000000002386f26fc10000000000000000000000000000000000000000000000000000002357a49c7d75f600000000000000000000000000000000000000000000000000000000640b5549000000000000000000000000710bda329b2a6224e4b44833de30f38e7f81d5640000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let v = U256::from(37);
        let r = string_to_h256_unsafe(
            "25dd6c973368c45ddfc17f5148e3f468a2e3f2c51920cbe9556a64942b0ab2eb",
        );
        let s = string_to_h256_unsafe(
            "31da07ce40c24b0a01f46fb2abc028b5ccd70dbd1cb330725323edc49a2a9558",
        );
        EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            v,
            r,
            s,
        }
    }

    #[test]
    // Test if the invalid transactions are producing receipts with invalid status
    fn test_invalid_transactions_receipt_status() {
        let mut host = MockHost::default();
        let _ = genesis::init_block(&mut host);

        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        let invalid_tx = Transaction {
            tx_hash,
            tx: dummy_eth_transaction(),
        };

        let transactions: Vec<Transaction> = vec![invalid_tx];
        let queue = Queue {
            proposals: vec![Blueprint { transactions }],
        };

        produce(&mut host, queue).expect("The block production failed.");

        match read_transaction_receipt_status(&mut host, &tx_hash) {
            Ok(TransactionStatus::Failure) => (),
            Ok(TransactionStatus::Success) => {
                panic!("The receipt should have a failing status.")
            }
            Err(_) => panic!("Reading the receipt failed."),
        }
    }

    #[test]
    // Test if a valid transaction is producing a receipt with a success status
    fn test_valid_transactions_receipt_status() {
        let mut host = MockHost::default();
        let _ = genesis::init_block(&mut host);

        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        let valid_tx = Transaction {
            tx_hash,
            tx: dummy_eth_transaction(),
        };

        let transactions: Vec<Transaction> = vec![valid_tx];
        let queue = Queue {
            proposals: vec![Blueprint { transactions }],
        };

        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(5000000000000000u64),
        );

        produce(&mut host, queue).expect("The block production failed.");

        match read_transaction_receipt_status(&mut host, &tx_hash) {
            Ok(TransactionStatus::Failure) => {
                panic!("The receipt should have a success status.")
            }
            Ok(TransactionStatus::Success) => (),
            Err(_) => panic!("Reading the receipt failed."),
        }
    }
}
