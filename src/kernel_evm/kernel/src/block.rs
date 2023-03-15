// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::blueprint::Queue;
use crate::error::{Error, TransferError};
use crate::helpers::address_to_hash;
use crate::inbox::Transaction;
use crate::storage;
use host::path::OwnedPath;
use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

use primitive_types::U256;
use tezos_ethereum::account::Account;
use tezos_ethereum::eth_gen::{BlockHash, L2Level, OwnedHash, Quantity, BLOCK_HASH_SIZE};
use tezos_ethereum::transaction::{RawTransaction, TransactionHash};

use tezos_ethereum::wei::Wei;

use debug::debug_msg;

// Used by Transaction.nonce.to_u64, the trait must be in scope for the function
// to be available
use num_traits::ToPrimitive;

pub struct L2Block {
    // This choice of a L2 block representation is totally
    // arbitrarily based on what is an Ethereum block and is
    // subject to change.
    pub number: L2Level,
    pub hash: BlockHash,
    pub parent_hash: BlockHash,
    pub nonce: Quantity,
    pub sha3_uncles: OwnedHash,
    pub logs_bloom: Option<OwnedHash>,
    pub transactions_root: OwnedHash,
    pub state_root: OwnedHash,
    pub receipts_root: OwnedHash,
    pub miner: OwnedHash,
    pub difficulty: Quantity,
    pub total_difficulty: Quantity,
    pub extra_data: OwnedHash,
    pub size: Quantity,
    pub gas_limit: Quantity,
    pub gas_used: Quantity,
    pub timestamp: Quantity,
    pub transactions: Vec<TransactionHash>,
    pub uncles: Vec<OwnedHash>,
}

impl L2Block {
    const DUMMY_QUANTITY: Quantity = 0;
    const DUMMY_HASH: &str = "0000000000000000000000000000000000000000";

    fn dummy_hash() -> OwnedHash {
        L2Block::DUMMY_HASH.into()
    }

    fn dummy_block_hash() -> BlockHash {
        [0; BLOCK_HASH_SIZE]
    }

    pub fn new(number: L2Level, transactions: Vec<TransactionHash>) -> Self {
        let hash: BlockHash = {
            let number: U256 = number.into();
            number.into()
        };
        L2Block {
            number,
            hash,
            parent_hash: L2Block::dummy_block_hash(),
            nonce: L2Block::DUMMY_QUANTITY,
            sha3_uncles: L2Block::dummy_hash(),
            logs_bloom: None,
            transactions_root: L2Block::dummy_hash(),
            state_root: L2Block::dummy_hash(),
            receipts_root: L2Block::dummy_hash(),
            miner: L2Block::dummy_hash(),
            difficulty: L2Block::DUMMY_QUANTITY,
            total_difficulty: L2Block::DUMMY_QUANTITY,
            extra_data: L2Block::dummy_hash(),
            size: L2Block::DUMMY_QUANTITY,
            gas_limit: L2Block::DUMMY_QUANTITY,
            gas_used: L2Block::DUMMY_QUANTITY,
            timestamp: L2Block::DUMMY_QUANTITY,
            transactions,
            uncles: Vec::new(),
        }
    }
}

fn get_tx_sender(tx: &RawTransaction) -> Result<OwnedPath, Error> {
    match tx.sender() {
        // We reencode in hexadecimal, since the accounts hash are encoded in
        // hexadecimal in the storage.
        Ok(address) => {
            let hash = address_to_hash(address);
            storage::account_path(&hash)
        }
        Err(_) => Err(Error::Generic),
    }
}

fn get_tx_receiver(tx: &RawTransaction) -> Result<OwnedPath, Error> {
    let hash = address_to_hash(tx.to);
    storage::account_path(&hash)
}

// A transaction is valid if the signature is valid, its nonce is valid and it
// can pay for the gas
fn validate_transaction<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    transaction: &Transaction,
) -> Result<(), Error> {
    let tx = &transaction.tx;
    let src_path = get_tx_sender(tx)?;
    let src_balance =
        storage::read_account_balance(host, &src_path).unwrap_or_else(|_| Wei::zero());
    let src_nonce = storage::read_account_nonce(host, &src_path).unwrap_or(0u64);
    let nonce = tx.nonce.to_u64().ok_or(Error::Generic)?;
    // For now, we consider there's no gas to pay
    let gas = Wei::zero();

    if !tx.is_valid() {
        Err(Error::Transfer(TransferError::InvalidSignature))
    } else if src_nonce != nonce {
        Err(Error::Transfer(TransferError::InvalidNonce))
    } else if src_balance < gas {
        Err(Error::Transfer(TransferError::NotEnoughBalance))
    } else {
        Ok(())
    }
}

// Update an account with the given balance and nonce (if one is given), and
// initialize it if it doesn't already appear in the storage.
fn update_account<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
    balance: Wei,
    nonce: Option<u64>, // if none is given, only the balance is updated. This
                        // avoids updating the storage with the same value.
) -> Result<(), Error> {
    if storage::has_account(host, account_path)? {
        storage::store_balance(host, account_path, balance)?;
        if let Some(nonce) = nonce {
            storage::store_nonce(host, account_path, nonce)?
        };
        Ok(())
    } else {
        storage::store_account(host, Account::default_account(balance), account_path)
    }
}

// invariant: the transaction is valid
fn apply_transaction<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    transaction: &Transaction,
) -> Result<bool, Error> {
    let tx = &transaction.tx;
    let src_path = get_tx_sender(tx)?;
    let src_balance =
        storage::read_account_balance(host, &src_path).unwrap_or_else(|_| Wei::zero());
    let nonce = tx.nonce.to_u64().ok_or(Error::Generic)?;
    let value = Wei::from_little_endian(&tx.value.to_le_bytes());
    let gas = Wei::zero();

    // First pay for the gas
    let src_balance_without_gas = src_balance - gas;
    // The gas is paid even if there's not enough balance for the total
    // transaction
    let (src_balance, succeed) = if src_balance_without_gas < value {
        (src_balance_without_gas, false)
    } else {
        (src_balance_without_gas - value, true)
    };
    update_account(host, &src_path, src_balance, Some(nonce + 1))?;

    if succeed {
        let dst_path = get_tx_receiver(tx)?;
        let dst_balance =
            storage::read_account_balance(host, &dst_path).unwrap_or_else(|_| Wei::zero());
        update_account(host, &dst_path, dst_balance + value, None)?;
    };
    Ok(succeed)
}

fn validate<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    transactions: &[Transaction],
) -> Result<(), Error> {
    transactions
        .iter()
        .try_for_each(|tx| validate_transaction(host, tx))
}

// This function is only available in nightly, hence the need for redefinition
fn try_collect<T, E>(vec: Vec<Result<T, E>>) -> Result<Vec<T>, E> {
    let mut new_vec = Vec::new();
    for v in vec {
        match v {
            Ok(v) => new_vec.push(v),
            Err(e) => return Err(e),
        }
    }
    Ok(new_vec)
}

fn apply_transactions<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    transactions: &[Transaction],
) -> Result<Vec<bool>, Error> {
    try_collect(
        transactions
            .iter()
            .map(|tx| (apply_transaction(host, tx)))
            .collect(),
    )
}

pub fn produce<Host: Runtime + RawRollupCore>(host: &mut Host, queue: Queue) {
    for proposal in queue.proposals.iter() {
        let current_level = storage::read_current_block_number(host);
        let next_level = match current_level {
            Ok(current_level) => current_level + 1,
            Err(_) => 0,
        };

        let transaction_hashes = proposal.transactions.iter().map(|tx| tx.tx_hash).collect();

        match validate(host, &proposal.transactions) {
            Ok(()) => {
                let valid_block = L2Block::new(next_level, transaction_hashes);
                storage::store_current_block(host, &valid_block).unwrap_or_else(|_| {
                    panic!("Error while storing the current block: stopping the daemon.")
                });
                apply_transactions(host, &proposal.transactions)
                    .unwrap_or_else(|_| panic!("Error while applying the transactions."));
            }
            Err(e) => debug_msg!(host; "Blueprint is invalid: {:?}\n", e),
        }
    }
}
