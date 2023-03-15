// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT
#![allow(dead_code)]

use host::path::*;
use host::rollup_core::RawRollupCore;
use host::runtime::{load_value_slice, Runtime, ValueType};

use std::str::from_utf8;

use crate::block::L2Block;
use crate::error::Error;
use tezos_ethereum::account::*;
use tezos_ethereum::eth_gen::{BlockHash, Hash, L2Level, TransactionHash, TRANSACTION_HASH_SIZE};
use tezos_ethereum::wei::Wei;

use primitive_types::U256;

const SMART_ROLLUP_ADDRESS: RefPath = RefPath::assert_from(b"/metadata/smart_rollup_address");

const EVM_ACCOUNTS: RefPath = RefPath::assert_from(b"/eth_accounts");

const EVM_ACCOUNT_BALANCE: RefPath = RefPath::assert_from(b"/balance");
const EVM_ACCOUNT_NONCE: RefPath = RefPath::assert_from(b"/nonce");
const EVM_ACCOUNT_CODE_HASH: RefPath = RefPath::assert_from(b"/code_hash");

const EVM_CURRENT_BLOCK: RefPath = RefPath::assert_from(b"/evm/blocks/current");
const EVM_BLOCKS: RefPath = RefPath::assert_from(b"/evm/blocks");
const EVM_BLOCKS_NUMBER: RefPath = RefPath::assert_from(b"/number");
const EVM_BLOCKS_HASH: RefPath = RefPath::assert_from(b"/hash");
const EVM_BLOCKS_TRANSACTIONS: RefPath = RefPath::assert_from(b"/transactions");

const HASH_MAX_SIZE: usize = 32;

// We can read/store at most [128] transaction hashes per block.
// TRANSACTION_HASH_SIZE * 128 = 4096.
const MAX_TRANSACTION_HASHES: usize = TRANSACTION_HASH_SIZE * 128;

pub fn read_smart_rollup_address<Host: Runtime + RawRollupCore>(
    host: &mut Host,
) -> Result<[u8; 20], Error> {
    let mut buffer = [0u8; 20];

    match load_value_slice(host, &SMART_ROLLUP_ADDRESS, &mut buffer) {
        Ok(20) => Ok(buffer),
        _ => Err(Error::Generic),
    }
}

pub fn store_smart_rollup_address<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
) -> Result<(), Error> {
    host.store_write(&SMART_ROLLUP_ADDRESS, &smart_rollup_address, 0)
        .map_err(Error::from)
}

/// The size of one 256 bit word. Size in bytes
pub const WORD_SIZE: usize = 32usize;

/// Read a single unsigned 256 bit value from storage at the path given.
fn read_u256(host: &impl Runtime, path: &OwnedPath) -> Result<U256, Error> {
    let bytes = host.store_read(path, 0, WORD_SIZE).map_err(Error::from)?;
    Ok(Wei::from_little_endian(&bytes))
}

fn write_u256(host: &mut impl Runtime, path: &OwnedPath, value: U256) -> Result<(), Error> {
    let mut bytes: [u8; WORD_SIZE] = value.into();
    value.to_little_endian(&mut bytes);
    host.store_write(path, &bytes, 0).map_err(Error::from)
}

fn address_path(address: Hash) -> Result<OwnedPath, Error> {
    let address: &str = from_utf8(address)?;
    let address_path: Vec<u8> = format!("/{}", &address).into();
    OwnedPath::try_from(address_path).map_err(Error::from)
}

pub fn account_path(address: Hash) -> Result<OwnedPath, Error> {
    let address_hash = address_path(address)?;
    concat(&EVM_ACCOUNTS, &address_hash).map_err(Error::from)
}

fn block_path(number: L2Level) -> Result<OwnedPath, Error> {
    let number: &str = &number.to_string();
    let raw_number_path: Vec<u8> = format!("/{}", &number).into();
    let number_path = OwnedPath::try_from(raw_number_path).map_err(Error::from)?;
    concat(&EVM_BLOCKS, &number_path).map_err(Error::from)
}

pub fn has_account<Host: Runtime>(
    host: &mut Host,
    account_path: &OwnedPath,
) -> Result<bool, Error> {
    match host.store_has(account_path).map_err(Error::from)? {
        Some(ValueType::Subtree | ValueType::ValueWithSubtree) => Ok(true),
        _ => Ok(false),
    }
}

pub fn read_account_nonce<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
) -> Result<u64, Error> {
    let path = concat(account_path, &EVM_ACCOUNT_NONCE)?;
    let mut buffer = [0_u8; 8];

    match load_value_slice(host, &path, &mut buffer) {
        Ok(8) => Ok(u64::from_le_bytes(buffer)),
        _ => Err(Error::Generic),
    }
}

pub fn read_account_balance<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
) -> Result<Wei, Error> {
    let path = concat(account_path, &EVM_ACCOUNT_BALANCE)?;
    read_u256(host, &path)
}

pub fn read_account_code_hash<Host: Runtime>(
    host: &mut Host,
    account_path: &OwnedPath,
) -> Result<Vec<u8>, Error> {
    let path = concat(account_path, &EVM_ACCOUNT_CODE_HASH)?;
    host.store_read(&path, 0, HASH_MAX_SIZE)
        .map_err(Error::from)
}

pub fn read_account<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    address: Hash,
) -> Result<Account, Error> {
    let account_path = account_path(address)?;
    let nonce = read_account_nonce(host, &account_path)?;
    let balance = read_account_balance(host, &account_path)?;
    let code_hash = read_account_code_hash(host, &account_path)?;

    Ok(Account {
        hash: address.to_vec(),
        nonce,
        balance,
        code_hash,
    })
}

pub fn store_nonce<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
    nonce: u64,
) -> Result<(), Error> {
    let path = concat(account_path, &EVM_ACCOUNT_NONCE)?;
    host.store_write(&path, &nonce.to_le_bytes(), 0)
        .map_err(Error::from)
}

pub fn store_balance<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
    balance: Wei,
) -> Result<(), Error> {
    let path = concat(account_path, &EVM_ACCOUNT_BALANCE)?;
    write_u256(host, &path, balance)
}

fn store_code_hash<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account_path: &OwnedPath,
    code_hash: Hash,
) -> Result<(), Error> {
    let path = concat(account_path, &EVM_ACCOUNT_CODE_HASH)?;
    host.store_write(&path, code_hash, 0).map_err(Error::from)
}

pub fn store_account<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    account: Account,
) -> Result<(), Error> {
    let account_path = account_path(&account.hash)?;
    store_nonce(host, &account_path, account.nonce)?;
    store_balance(host, &account_path, account.balance)?;
    store_code_hash(host, &account_path, &account.code_hash)
}

pub fn read_current_block_number<Host: Runtime + RawRollupCore>(
    host: &mut Host,
) -> Result<u64, Error> {
    let path = concat(&EVM_CURRENT_BLOCK, &EVM_BLOCKS_NUMBER)?;
    let mut buffer = [0_u8; 8];

    match load_value_slice(host, &path, &mut buffer) {
        Ok(8) => Ok(u64::from_le_bytes(buffer)),
        _ => Err(Error::Generic),
    }
}

fn read_current_block_transactions<Host: Runtime>(
    host: &mut Host,
) -> Result<Vec<TransactionHash>, Error> {
    let path = concat(&EVM_CURRENT_BLOCK, &EVM_BLOCKS_TRANSACTIONS)?;

    let transactions_bytes = host
        .store_read(&path, 0, MAX_TRANSACTION_HASHES)
        .map_err(Error::from)?;

    Ok(transactions_bytes
        .chunks(TRANSACTION_HASH_SIZE)
        .filter_map(|tx_hash_bytes: &[u8]| -> Option<TransactionHash> {
            tx_hash_bytes.try_into().ok()
        })
        .collect::<Vec<TransactionHash>>())
}

pub fn read_current_block<Host: Runtime + RawRollupCore>(
    host: &mut Host,
) -> Result<L2Block, Error> {
    let number = read_current_block_number(host)?;
    let transactions = read_current_block_transactions(host)?;

    Ok(L2Block::new(number, transactions))
}

fn store_block_number<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    block_path: &OwnedPath,
    block_number: L2Level,
) -> Result<(), Error> {
    let path = concat(block_path, &EVM_BLOCKS_NUMBER)?;
    host.store_write(&path, &u64::to_le_bytes(block_number), 0)
        .map_err(Error::from)
}

fn store_block_hash<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    block_path: &OwnedPath,
    block_hash: &BlockHash,
) -> Result<(), Error> {
    let path = concat(block_path, &EVM_BLOCKS_HASH)?;
    host.store_write(&path, block_hash, 0).map_err(Error::from)
}

fn store_block_transactions<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    block_path: &OwnedPath,
    block_transactions: &[TransactionHash],
) -> Result<(), Error> {
    let path = concat(block_path, &EVM_BLOCKS_TRANSACTIONS)?;
    let block_transactions = &block_transactions.concat()[..];
    host.store_write(&path, block_transactions, 0)
        .map_err(Error::from)
}

fn store_block<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    block: &L2Block,
    block_path: OwnedPath,
) -> Result<(), Error> {
    store_block_number(host, &block_path, block.number)?;
    store_block_hash(host, &block_path, &block.hash)?;
    store_block_transactions(host, &block_path, &block.transactions)
}

pub fn store_block_by_number<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    block: &L2Block,
) -> Result<(), Error> {
    let block_path = block_path(block.number)?;
    store_block(host, block, block_path)
}

pub fn store_current_block<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    block: L2Block,
) -> Result<(), Error> {
    let current_block_path = OwnedPath::from(EVM_CURRENT_BLOCK);
    store_block(host, &block, current_block_path)?;
    /* When storing the current block's infos we need to store it under the [evm/blocks/<block_number>]
    path as well, thus the following line: */
    store_block_by_number(host, &block)
}
