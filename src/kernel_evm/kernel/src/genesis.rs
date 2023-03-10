// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::storage;
use crate::L2Block;
use debug::debug_msg;
use host::path::OwnedPath;
use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;
use tezos_ethereum::account::Account;
use tezos_ethereum::transaction::TransactionHash;
use tezos_ethereum::wei::{self, Wei};

struct MintAccount {
    mint_address: &'static str,
    genesis_tx_hash: &'static str,
    eth_amount: u64,
}

const MINT_ACCOUNTS_NUMBER: usize = 3;

const MINT_ACCOUNTS: [MintAccount; MINT_ACCOUNTS_NUMBER] = [
    MintAccount {
        mint_address: "6ce4d79d4e77402e1ef3417fdda433aa744c6e1c",
        genesis_tx_hash: "47454e4553495341444452455353000000000000000000000000000000000001",
        eth_amount: 9999,
    },
    MintAccount {
        mint_address: "b53dc01974176e5dff2298c5a94343c2585e3c54",
        genesis_tx_hash: "47454e4553495341444452455353000000000000000000000000000000000002",
        eth_amount: 9999,
    },
    MintAccount {
        mint_address: "9b49c988b5817be31dfb00f7a5a4671772dcce2b",
        genesis_tx_hash: "47454e4553495341444452455353000000000000000000000000000000000003",
        eth_amount: 9999,
    },
];

fn store_genesis_mint_account<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account: Account,
    path: OwnedPath,
) -> Result<(), Error> {
    match storage::store_account(host, account, &path) {
        Ok(_) => Ok(()),
        Err(_) => {
            debug_msg!(host; "Error, cannot initialize genesis' mint account.");
            Err(Error::Generic)
        }
    }
}

fn forge_genesis_mint_account<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    mint_address: &str,
    balance: Wei,
) -> Result<(), Error> {
    let account = Account::with_assets(balance);

    match storage::account_path(&mint_address.as_bytes().to_vec()) {
        Ok(path) => store_genesis_mint_account(host, account, path),
        Err(_) => {
            debug_msg!(host; "Error, cannot forge genesis' mint account path.");
            Err(Error::Generic)
        }
    }
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

fn bootstrap_genesis_accounts<Host: Runtime + RawRollupCore>(
    host: &mut Host,
) -> Result<Vec<TransactionHash>, Error> {
    let transactions_hashes = MINT_ACCOUNTS.map(
        |MintAccount {
             mint_address,
             genesis_tx_hash,
             eth_amount,
         }| {
            forge_genesis_mint_account(host, mint_address, wei::from_eth(eth_amount))?;

            match hex::decode(genesis_tx_hash) {
                Ok(raw_tx_hash) => raw_tx_hash.try_into().map_err(|_| Error::Generic),
                Err(_) => {
                    debug_msg!(host; "Error while decoding raw transaction hash.");
                    Err(Error::Generic)
                }
            }
        },
    );

    collect_mint_transactions(transactions_hashes)
}

pub fn init_block<Host: Runtime + RawRollupCore>(host: &mut Host) -> Result<(), Error> {
    // Forge the genesis' transactions that will mint the very first accounts
    let transaction_hashes = bootstrap_genesis_accounts(host)?;

    // Produce and store genesis' block
    let genesis_block = L2Block::new(0, transaction_hashes);
    storage::store_current_block(host, &genesis_block).map_err(|_| {
        debug_msg!(host; "Error while storing the genesis block.");
        Error::Generic
    })?;

    debug_msg!(host; "Genesis block was initialized.\n");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    use mock_runtime::host::MockHost;

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
