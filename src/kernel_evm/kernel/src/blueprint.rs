// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::block_in_progress::BlockInProgress;
use crate::inbox::{read_inbox, KernelUpgrade, Transaction, TransactionContent};
use crate::tick_model::constants::MAX_TRANSACTION_GAS_LIMIT;
use primitive_types::U256;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_smart_rollup_host::runtime::Runtime;

/// The blueprint of a block is a list of transactions.
pub struct Blueprint {
    pub transactions: Vec<Transaction>,
}
pub enum QueueElement {
    Blueprint(Blueprint),
    BlockInProgress(Box<BlockInProgress>),
}
#[derive(Default)]
pub struct Queue {
    // In our case, to make it simple and straightforward it will be
    // an array of pendings transactions even though it'll be only a
    // singleton for our needs.
    pub proposals: Vec<QueueElement>,
    pub kernel_upgrade: Option<KernelUpgrade>,
}

impl Queue {
    pub fn new() -> Queue {
        Queue {
            proposals: Vec::new(),
            kernel_upgrade: None,
        }
    }

    pub fn add(queue: &mut Queue, transactions: Vec<Transaction>) {
        queue
            .proposals
            .push(QueueElement::Blueprint(Blueprint { transactions }))
    }
}

fn filter_invalid_transactions(
    transactions: Vec<Transaction>,
    chain_id: U256,
) -> Vec<Transaction> {
    let filter_chain_id = |transaction: &Transaction| match &transaction.content {
        TransactionContent::Deposit(_) => true,
        TransactionContent::Ethereum(transaction) => {
            U256::eq(&transaction.chain_id, &chain_id)
        }
    };

    let filter_max_gas_limit = |transaction: &Transaction| match &transaction.content {
        TransactionContent::Deposit(_) => true,
        TransactionContent::Ethereum(transaction) => {
            transaction.gas_limit <= MAX_TRANSACTION_GAS_LIMIT
        }
    };

    transactions
        .into_iter()
        .filter(|transaction| {
            filter_chain_id(transaction) && filter_max_gas_limit(transaction)
        })
        .collect()
}

pub fn fetch<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    chain_id: U256,
    ticketer: Option<ContractKt1Hash>,
    admin: Option<ContractKt1Hash>,
) -> Result<Queue, anyhow::Error> {
    let inbox_content = read_inbox(host, smart_rollup_address, ticketer, admin)?;
    let transactions = filter_invalid_transactions(inbox_content.transactions, chain_id);
    let blueprint = QueueElement::Blueprint(Blueprint { transactions });
    Ok(Queue {
        proposals: vec![blueprint],
        kernel_upgrade: inbox_content.kernel_upgrade,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inbox::TransactionContent::Ethereum;
    use primitive_types::{H160, U256};
    use tezos_ethereum::{
        transaction::TRANSACTION_HASH_SIZE, tx_common::EthereumTransactionCommon,
    };

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    fn tx() -> EthereumTransactionCommon {
        EthereumTransactionCommon {
            type_: tezos_ethereum::transaction::TransactionType::Legacy,
            chain_id: U256::one(),
            nonce: U256::from(40000000u64),
            max_priority_fee_per_gas: U256::from(40000000u64),
            max_fee_per_gas: U256::from(40000000u64),
            gas_limit: 21000u64,
            to: address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea"),
            value: U256::from(500000000u64),
            data: vec![],
            access_list: vec![],
            signature: None,
        }
    }

    #[test]
    fn test_filter_invalid_chain_id() {
        let chain_id = U256::one();

        let valid_content = Ethereum(EthereumTransactionCommon { chain_id, ..tx() });
        let valid_transaction = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: valid_content,
        };

        let invalid_content = Ethereum(EthereumTransactionCommon {
            chain_id: U256::from(1312321),
            ..tx()
        });
        let invalid_transaction = Transaction {
            tx_hash: [1; TRANSACTION_HASH_SIZE],
            content: invalid_content,
        };

        let filtered_transactions = filter_invalid_transactions(
            vec![valid_transaction.clone(), invalid_transaction],
            chain_id,
        );
        assert_eq!(vec![valid_transaction], filtered_transactions)
    }

    #[test]
    fn test_filter_large_gas_limit() {
        let valid_content = Ethereum(EthereumTransactionCommon {
            gas_limit: MAX_TRANSACTION_GAS_LIMIT,
            ..tx()
        });
        let valid_transaction = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: valid_content,
        };

        let invalid_content = Ethereum(EthereumTransactionCommon {
            gas_limit: MAX_TRANSACTION_GAS_LIMIT + 1,
            ..tx()
        });
        let invalid_transaction = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: invalid_content,
        };

        let filtered_transactions = filter_invalid_transactions(
            vec![valid_transaction.clone(), invalid_transaction],
            U256::one(),
        );
        assert_eq!(vec![valid_transaction], filtered_transactions)
    }
}
