// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::inbox::{read_inbox, KernelUpgrade, Transaction, TransactionContent};
use crate::Error;
use primitive_types::U256;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_smart_rollup_host::runtime::Runtime;

/// The blueprint of a block is a list of transactions.
pub struct Blueprint {
    pub transactions: Vec<Transaction>,
}

#[derive(Default)]
pub struct Queue {
    // In our case, to make it simple and straightforward it will be
    // an array of pendings transactions even though it'll be only a
    // singleton for our needs.
    pub proposals: Vec<Blueprint>,
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
        queue.proposals.push(Blueprint { transactions })
    }
}

fn filter_invalid_chain_id(
    transactions: Vec<Transaction>,
    chain_id: U256,
) -> Vec<Transaction> {
    transactions
        .into_iter()
        .filter(|transaction| match &transaction.content {
            TransactionContent::Deposit(_) => true,
            TransactionContent::Ethereum(transaction) => {
                U256::eq(&transaction.chain_id, &chain_id)
            }
        })
        .collect()
}

pub fn fetch<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    chain_id: U256,
    ticketer: Option<ContractKt1Hash>,
) -> Result<Queue, Error> {
    let inbox_content = read_inbox(host, smart_rollup_address, ticketer)?;
    let transactions = filter_invalid_chain_id(inbox_content.transactions, chain_id);
    let blueprint = Blueprint { transactions };
    Ok(Queue {
        proposals: vec![blueprint],
        kernel_upgrade: inbox_content.kernel_upgrade,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inbox::TransactionContent::Ethereum;
    use primitive_types::{H160, H256, U256};
    use tezos_ethereum::{
        signatures::EthereumTransactionCommon, transaction::TRANSACTION_HASH_SIZE,
    };

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    #[test]
    fn test_filter_invalid_chain_id() {
        let chain_id = U256::one();

        let tx = EthereumTransactionCommon {
            chain_id,
            nonce: U256::from(40000000u64),
            gas_price: U256::from(40000000u64),
            gas_limit: 21000u64,
            to: address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea"),
            value: U256::from(500000000u64),
            data: vec![],
            v: U256::from(0),
            r: H256::from_low_u64_be(0),
            s: H256::from_low_u64_be(0),
        };

        let valid_transaction = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: Ethereum(tx.clone()),
        };
        let invalid_transaction = Transaction {
            tx_hash: [1; TRANSACTION_HASH_SIZE],
            content: Ethereum(EthereumTransactionCommon {
                chain_id: U256::from(1312321),
                ..tx
            }),
        };

        let filtered_transactions = filter_invalid_chain_id(
            vec![valid_transaction.clone(), invalid_transaction],
            chain_id,
        );
        assert_eq!(vec![valid_transaction], filtered_transactions)
    }
}
