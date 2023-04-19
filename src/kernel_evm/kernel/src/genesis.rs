// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::storage;
use crate::storage::receipt_path;
use crate::L2Block;
use primitive_types::U256;
use tezos_ethereum::account::Account;
use tezos_ethereum::address::EthereumAddress;
use tezos_ethereum::eth_gen::L2Level;
use tezos_ethereum::transaction::TransactionHash;
use tezos_ethereum::transaction::TransactionReceipt;
use tezos_ethereum::transaction::TransactionStatus;
use tezos_ethereum::transaction::TransactionType;
use tezos_ethereum::transaction::TRANSACTION_HASH_SIZE;
use tezos_ethereum::wei::{self, Wei};
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::runtime::Runtime;

struct MintAccount {
    mint_address: &'static str,
    genesis_tx_hash: TransactionHash,
    eth_amount: u64,
}

const GENESIS_ADDRESSS: [u8; 20] = [0; 20];

const GENESIS_LEVEL: L2Level = 0;

const MINT_ACCOUNTS_NUMBER: usize = 3;

const MINT_ACCOUNTS: [MintAccount; MINT_ACCOUNTS_NUMBER] = [
    MintAccount {
        mint_address: "6ce4d79d4e77402e1ef3417fdda433aa744c6e1c",
        genesis_tx_hash: [0; TRANSACTION_HASH_SIZE],
        eth_amount: 9999,
    },
    MintAccount {
        mint_address: "b53dc01974176e5dff2298c5a94343c2585e3c54",
        genesis_tx_hash: [1; TRANSACTION_HASH_SIZE],
        eth_amount: 9999,
    },
    MintAccount {
        mint_address: "9b49c988b5817be31dfb00f7a5a4671772dcce2b",
        genesis_tx_hash: [2; TRANSACTION_HASH_SIZE],
        eth_amount: 9999,
    },
];

fn store_genesis_mint_account<Host: Runtime>(
    host: &mut Host,
    account: &Account,
    path: &OwnedPath,
) -> Result<(), Error> {
    storage::store_account(host, account, path)
}

fn forge_genesis_mint_account<Host: Runtime>(
    host: &mut Host,
    mint_address: &str,
    balance: Wei,
) -> Result<(), Error> {
    let account = Account::with_assets(balance);
    let path = storage::account_path(&mint_address.as_bytes().to_vec())?;
    store_genesis_mint_account(host, &account, &path)
}

fn collect_mint_transactions<T, E>(
    slice: [Result<T, E>; MINT_ACCOUNTS_NUMBER],
) -> Result<Vec<T>, E> {
    let mut new_vec = Vec::new();
    for v in slice.into_iter() {
        match v {
            Ok(v) => new_vec.push(v),
            Err(e) => return Err(e),
        }
    }
    Ok(new_vec)
}

fn bootstrap_genesis_accounts<Host: Runtime>(
    host: &mut Host,
) -> Result<Vec<TransactionHash>, Error> {
    let transactions_hashes = MINT_ACCOUNTS.map(
        |MintAccount {
             mint_address,
             genesis_tx_hash,
             eth_amount,
         }| {
            forge_genesis_mint_account(host, mint_address, wei::from_eth(eth_amount))?;
            Ok(genesis_tx_hash)
        },
    );

    collect_mint_transactions(transactions_hashes)
}

fn craft_mint_address(genesis_mint_address: &str) -> Option<EthereumAddress> {
    let encoded_genesis_mint_address: Vec<u8> = hex::decode(genesis_mint_address).ok()?;
    let encoded_genesis_mint_address: [u8; 20] =
        encoded_genesis_mint_address.try_into().ok()?;
    Some(encoded_genesis_mint_address.into())
}

fn store_genesis_receipts<Host: Runtime>(
    host: &mut Host,
    genesis_block: L2Block,
) -> Result<(), Error> {
    let genesis_address: EthereumAddress = GENESIS_ADDRESSS.into();

    for (i, hash) in genesis_block.transactions.iter().enumerate() {
        let mint_account = &MINT_ACCOUNTS[i];
        // There are very few genesis transactions, the conversion from usize to u32 is safe
        // and truncate won't lead to a loss of data
        let index = i as u32;

        let receipt = TransactionReceipt {
            hash: *hash,
            index,
            block_hash: genesis_block.hash,
            block_number: genesis_block.number,
            from: genesis_address,
            to: craft_mint_address(mint_account.mint_address),
            cumulative_gas_used: U256::zero(),
            effective_gas_price: U256::zero(),
            gas_used: U256::zero(),
            contract_address: None,
            type_: TransactionType::Legacy,
            status: TransactionStatus::Success,
        };

        let receipt_path = receipt_path(&receipt)?;
        storage::store_transaction_receipt(&receipt_path, host, &receipt)?;
    }

    Ok(())
}

pub fn init_block<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    // Forge the genesis' transactions that will mint the very first accounts
    let transaction_hashes = bootstrap_genesis_accounts(host)?;

    // Produce and store genesis' block
    let genesis_block = L2Block::new(GENESIS_LEVEL, transaction_hashes);
    storage::store_current_block(host, &genesis_block)?;
    store_genesis_receipts(host, genesis_block)?;

    debug_msg!(host, "Genesis block was initialized.\n");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    use tezos_smart_rollup_mock::MockHost;

    #[test]
    // Test if the genesis block can be initialized
    fn test_init_genesis_block() {
        let mut host = MockHost::default();

        match init_block(&mut host) {
            Ok(()) => (),
            Err(_) => panic!("The initialization of block genesis failed."),
        }
    }
}
