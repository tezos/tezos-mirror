// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT
#![allow(dead_code)]

use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_host::path::*;
use tezos_smart_rollup_host::runtime::{Runtime, ValueType};

use std::str::from_utf8;

use crate::block::L2Block;
use crate::error::{Error, StorageError};
use tezos_ethereum::account::*;
use tezos_ethereum::eth_gen::{BlockHash, Hash, L2Level};
use tezos_ethereum::transaction::{
    TransactionHash, TransactionReceipt, TransactionStatus, TRANSACTION_HASH_SIZE,
};
use tezos_ethereum::wei::Wei;

use primitive_types::{H160, U256};

const SMART_ROLLUP_ADDRESS: RefPath =
    RefPath::assert_from(b"/metadata/smart_rollup_address");

const EVM_ACCOUNTS: RefPath = RefPath::assert_from(b"/eth_accounts");

const EVM_ACCOUNT_BALANCE: RefPath = RefPath::assert_from(b"/balance");
const EVM_ACCOUNT_NONCE: RefPath = RefPath::assert_from(b"/nonce");
const EVM_ACCOUNT_CODE_HASH: RefPath = RefPath::assert_from(b"/code_hash");

const EVM_CURRENT_BLOCK: RefPath = RefPath::assert_from(b"/evm/blocks/current");
const EVM_BLOCKS: RefPath = RefPath::assert_from(b"/evm/blocks");
const EVM_BLOCKS_NUMBER: RefPath = RefPath::assert_from(b"/number");
const EVM_BLOCKS_HASH: RefPath = RefPath::assert_from(b"/hash");
const EVM_BLOCKS_TRANSACTIONS: RefPath = RefPath::assert_from(b"/transactions");

const TRANSACTIONS_RECEIPTS: RefPath = RefPath::assert_from(b"/transactions_receipts");
const TRANSACTION_RECEIPT_HASH: RefPath = RefPath::assert_from(b"/hash");
const TRANSACTION_RECEIPT_INDEX: RefPath = RefPath::assert_from(b"/index");
const TRANSACTION_RECEIPT_BLOCK_HASH: RefPath = RefPath::assert_from(b"/block_hash");
const TRANSACTION_RECEIPT_BLOCK_NUMBER: RefPath = RefPath::assert_from(b"/block_number");
const TRANSACTION_RECEIPT_FROM: RefPath = RefPath::assert_from(b"/from");
const TRANSACTION_RECEIPT_TO: RefPath = RefPath::assert_from(b"/to");
const TRANSACTION_RECEIPT_TYPE: RefPath = RefPath::assert_from(b"/type");
const TRANSACTION_RECEIPT_STATUS: RefPath = RefPath::assert_from(b"/status");

const HASH_MAX_SIZE: usize = 32;
const TRANSACTION_RECEIPT_STATUS_SIZE: usize = 1;

// We can read/store at most [128] transaction hashes per block.
// TRANSACTION_HASH_SIZE * 128 = 4096.
const MAX_TRANSACTION_HASHES: usize = TRANSACTION_HASH_SIZE * 128;

fn store_read_slice<Host: Runtime, T: Path>(
    host: &mut Host,
    path: &T,
    buffer: &mut [u8],
    expected_size: usize,
) -> Result<(), Error> {
    let size = Runtime::store_read_slice(host, path, 0, buffer)?;
    if size == expected_size {
        Ok(())
    } else {
        Err(Error::Storage(StorageError::InvalidLoadValue {
            expected: expected_size,
            actual: size,
        }))
    }
}

pub fn read_smart_rollup_address<Host: Runtime>(
    host: &mut Host,
) -> Result<[u8; 20], Error> {
    let mut buffer = [0u8; 20];
    store_read_slice(host, &SMART_ROLLUP_ADDRESS, &mut buffer, 20)?;
    Ok(buffer)
}

pub fn store_smart_rollup_address<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: &[u8; 20],
) -> Result<(), Error> {
    host.store_write(&SMART_ROLLUP_ADDRESS, smart_rollup_address, 0)
        .map_err(Error::from)
}

/// The size of one 256 bit word. Size in bytes
pub const WORD_SIZE: usize = 32usize;

/// Read a single unsigned 256 bit value from storage at the path given.
fn read_u256(host: &impl Runtime, path: &OwnedPath) -> Result<U256, Error> {
    let bytes = host.store_read(path, 0, WORD_SIZE)?;
    Ok(Wei::from_little_endian(&bytes))
}

fn write_u256(
    host: &mut impl Runtime,
    path: &OwnedPath,
    value: U256,
) -> Result<(), Error> {
    let mut bytes: [u8; WORD_SIZE] = value.into();
    value.to_little_endian(&mut bytes);
    host.store_write(path, &bytes, 0).map_err(Error::from)
}

fn address_path(address: Hash) -> Result<OwnedPath, Error> {
    let address: &str =
        from_utf8(address).map_err(crate::error::TransferError::InvalidAddressFormat)?;
    let address_path: Vec<u8> = format!("/{}", &address.to_ascii_lowercase()).into();
    OwnedPath::try_from(address_path).map_err(Error::from)
}

pub fn account_path(address: Hash) -> Result<OwnedPath, Error> {
    let address_hash = address_path(address)?;
    concat(&EVM_ACCOUNTS, &address_hash).map_err(Error::from)
}

pub fn block_path(number: L2Level) -> Result<OwnedPath, Error> {
    let number: &str = &number.to_string();
    let raw_number_path: Vec<u8> = format!("/{}", &number).into();
    let number_path = OwnedPath::try_from(raw_number_path)?;
    concat(&EVM_BLOCKS, &number_path).map_err(Error::from)
}
pub fn receipt_path(receipt_hash: &TransactionHash) -> Result<OwnedPath, Error> {
    let hash = hex::encode(receipt_hash);
    let raw_receipt_path: Vec<u8> = format!("/{}", &hash).into();
    let receipt_path = OwnedPath::try_from(raw_receipt_path)?;
    concat(&TRANSACTIONS_RECEIPTS, &receipt_path).map_err(Error::from)
}

pub fn has_account<Host: Runtime>(
    host: &mut Host,
    account_path: &OwnedPath,
) -> Result<bool, Error> {
    match host.store_has(account_path)? {
        Some(ValueType::Subtree | ValueType::ValueWithSubtree) => Ok(true),
        _ => Ok(false),
    }
}

pub fn read_account_nonce<Host: Runtime>(
    host: &mut Host,
    account_path: &OwnedPath,
) -> Result<U256, Error> {
    let path = concat(account_path, &EVM_ACCOUNT_NONCE)?;
    read_u256(host, &path)
}

pub fn read_account_balance<Host: Runtime>(
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

pub fn read_account<Host: Runtime>(
    host: &mut Host,
    address: Hash,
) -> Result<Account, Error> {
    let account_path = account_path(address)?;
    let nonce = read_account_nonce(host, &account_path)?;
    let balance = read_account_balance(host, &account_path)?;
    let code_hash = read_account_code_hash(host, &account_path)?;

    Ok(Account {
        nonce,
        balance,
        code_hash,
    })
}

pub fn store_nonce<Host: Runtime>(
    host: &mut Host,
    account_path: &OwnedPath,
    nonce: U256,
) -> Result<(), Error> {
    let path = concat(account_path, &EVM_ACCOUNT_NONCE)?;
    write_u256(host, &path, nonce)
}

pub fn store_balance<Host: Runtime>(
    host: &mut Host,
    account_path: &OwnedPath,
    balance: Wei,
) -> Result<(), Error> {
    let path = concat(account_path, &EVM_ACCOUNT_BALANCE)?;
    write_u256(host, &path, balance)
}

fn store_code_hash<Host: Runtime>(
    host: &mut Host,
    account_path: &OwnedPath,
    code_hash: Hash,
) -> Result<(), Error> {
    let path = concat(account_path, &EVM_ACCOUNT_CODE_HASH)?;
    host.store_write(&path, code_hash, 0).map_err(Error::from)
}

pub fn store_account<Host: Runtime>(
    host: &mut Host,
    account: &Account,
    account_path: &OwnedPath,
) -> Result<(), Error> {
    store_nonce(host, account_path, account.nonce)?;
    store_balance(host, account_path, account.balance)?;
    store_code_hash(host, account_path, &account.code_hash)
}

pub fn read_current_block_number<Host: Runtime>(host: &mut Host) -> Result<u64, Error> {
    let path = concat(&EVM_CURRENT_BLOCK, &EVM_BLOCKS_NUMBER)?;
    let mut buffer = [0_u8; 8];
    store_read_slice(host, &path, &mut buffer, 8)?;
    Ok(u64::from_le_bytes(buffer))
}

fn read_nth_block_transactions<Host: Runtime>(
    host: &mut Host,
    block_path: &OwnedPath,
) -> Result<Vec<TransactionHash>, Error> {
    let path = concat(block_path, &EVM_BLOCKS_TRANSACTIONS)?;

    let transactions_bytes = host.store_read(&path, 0, MAX_TRANSACTION_HASHES)?;

    Ok(transactions_bytes
        .chunks(TRANSACTION_HASH_SIZE)
        .filter_map(|tx_hash_bytes: &[u8]| -> Option<TransactionHash> {
            tx_hash_bytes.try_into().ok()
        })
        .collect::<Vec<TransactionHash>>())
}

pub fn read_current_block<Host: Runtime>(host: &mut Host) -> Result<L2Block, Error> {
    let number = read_current_block_number(host)?;
    let block_path = block_path(number)?;
    let transactions = read_nth_block_transactions(host, &block_path)?;

    Ok(L2Block::new(number, transactions))
}

fn store_block_number<Host: Runtime>(
    host: &mut Host,
    block_path: &OwnedPath,
    block_number: L2Level,
) -> Result<(), Error> {
    let path = concat(block_path, &EVM_BLOCKS_NUMBER)?;
    host.store_write(&path, &u64::to_le_bytes(block_number), 0)
        .map_err(Error::from)
}

fn store_block_hash<Host: Runtime>(
    host: &mut Host,
    block_path: &OwnedPath,
    block_hash: &BlockHash,
) -> Result<(), Error> {
    let path = concat(block_path, &EVM_BLOCKS_HASH)?;
    host.store_write(&path, block_hash, 0).map_err(Error::from)
}

fn store_block_transactions<Host: Runtime>(
    host: &mut Host,
    block_path: &OwnedPath,
    block_transactions: &[TransactionHash],
) -> Result<(), Error> {
    let path = concat(block_path, &EVM_BLOCKS_TRANSACTIONS)?;
    let block_transactions = &block_transactions.concat()[..];
    host.store_write(&path, block_transactions, 0)
        .map_err(Error::from)
}

fn store_block<Host: Runtime>(
    host: &mut Host,
    block: &L2Block,
    block_path: &OwnedPath,
) -> Result<(), Error> {
    store_block_number(host, block_path, block.number)?;
    store_block_hash(host, block_path, &block.hash)?;
    store_block_transactions(host, block_path, &block.transactions)
}

pub fn store_block_by_number<Host: Runtime>(
    host: &mut Host,
    block: &L2Block,
) -> Result<(), Error> {
    let block_path = block_path(block.number)?;
    store_block(host, block, &block_path)
}

pub fn store_current_block<Host: Runtime>(
    host: &mut Host,
    block: &L2Block,
) -> Result<(), Error> {
    let current_block_path = OwnedPath::from(EVM_CURRENT_BLOCK);
    // We only need to store current block's number so we avoid the storage of duplicate informations.
    store_block_number(host, &current_block_path, block.number)?;
    // When storing the current block's infos we need to store it under the [evm/blocks/<block_number>]
    store_block_by_number(host, block)
}

// TODO: This store a transaction receipt with multiple subkeys, it could
// be stored in a single encoded value. However, this is for now easier
// for the (OCaml) proxy server to do as is.
pub fn store_transaction_receipt<Host: Runtime>(
    receipt_path: &OwnedPath,
    host: &mut Host,
    receipt: &TransactionReceipt,
) -> Result<(), Error> {
    // Transaction hash
    let hash_path = concat(receipt_path, &TRANSACTION_RECEIPT_HASH)?;
    host.store_write(&hash_path, &receipt.hash, 0)?;
    // Index
    let index_path = concat(receipt_path, &TRANSACTION_RECEIPT_INDEX)?;
    host.store_write(&index_path, &receipt.index.to_le_bytes(), 0)?;
    // Block hash
    let block_hash_path = concat(receipt_path, &TRANSACTION_RECEIPT_BLOCK_HASH)?;
    host.store_write(&block_hash_path, &receipt.block_hash, 0)?;
    // Block number
    let block_number_path = concat(receipt_path, &TRANSACTION_RECEIPT_BLOCK_NUMBER)?;
    host.store_write(
        &block_number_path,
        &u64::to_le_bytes(receipt.block_number),
        0,
    )?;
    // From
    let from_path = concat(receipt_path, &TRANSACTION_RECEIPT_FROM)?;
    let from: H160 = receipt.from.into();
    host.store_write(&from_path, from.as_bytes(), 0)?;
    // Type
    let type_path = concat(receipt_path, &TRANSACTION_RECEIPT_TYPE)?;
    host.store_write(&type_path, (&receipt.type_).into(), 0)?;
    // Status
    let status_path = concat(receipt_path, &TRANSACTION_RECEIPT_STATUS)?;
    host.store_write(&status_path, (&receipt.status).into(), 0)?;
    // To
    if let Some(to) = receipt.to {
        let to: H160 = to.into();
        let to_path = concat(receipt_path, &TRANSACTION_RECEIPT_TO)?;
        host.store_write(&to_path, to.as_bytes(), 0)?;
    };

    Ok(())
}

pub fn store_transaction_receipts<Host: Runtime>(
    host: &mut Host,
    receipts: &[TransactionReceipt],
) -> Result<(), Error> {
    for receipt in receipts {
        let receipt_path = receipt_path(&receipt.hash)?;
        store_transaction_receipt(&receipt_path, host, receipt)?;
    }
    Ok(())
}

pub fn read_transaction_receipt_status<Host: Runtime>(
    host: &mut Host,
    tx_hash: &TransactionHash,
) -> Result<TransactionStatus, Error> {
    let receipt_path = receipt_path(tx_hash)?;
    let status_path = concat(&receipt_path, &TRANSACTION_RECEIPT_STATUS)?;
    let raw_status = host
        .store_read(&status_path, 0, TRANSACTION_RECEIPT_STATUS_SIZE)
        .map_err(Error::from)?;
    TransactionStatus::try_from(&raw_status).map_err(|_| {
        Error::Storage(StorageError::InvalidEncoding {
            path: status_path,
            value: raw_status,
        })
    })
}

const CHUNKED_TRANSACTIONS: RefPath = RefPath::assert_from(b"/chunked_transactions");
const CHUNKED_TRANSACTION_NUM_CHUNKS: RefPath = RefPath::assert_from(b"/num_chunks");

fn chunked_transaction_path(tx_hash: &TransactionHash) -> Result<OwnedPath, Error> {
    let hash = hex::encode(tx_hash);
    let raw_chunked_transaction_path: Vec<u8> = format!("/{}", hash).into();
    let chunked_transaction_path = OwnedPath::try_from(raw_chunked_transaction_path)?;
    concat(&CHUNKED_TRANSACTIONS, &chunked_transaction_path).map_err(Error::from)
}

fn chunked_transaction_num_chunks_path(
    chunked_transaction_path: &OwnedPath,
) -> Result<OwnedPath, Error> {
    concat(chunked_transaction_path, &CHUNKED_TRANSACTION_NUM_CHUNKS).map_err(Error::from)
}

fn transaction_chunk_path(
    chunked_transaction_path: &OwnedPath,
    i: u16,
) -> Result<OwnedPath, Error> {
    let raw_i_path: Vec<u8> = format!("/{}", i).into();
    let i_path = OwnedPath::try_from(raw_i_path)?;
    concat(chunked_transaction_path, &i_path).map_err(Error::from)
}

fn is_transaction_complete<Host: Runtime>(
    host: &mut Host,
    chunked_transaction_path: &OwnedPath,
    num_chunks: u16,
) -> Result<bool, Error> {
    let n_subkeys = host.store_count_subkeys(chunked_transaction_path)? as u16;
    // `n_subkeys` includes the key `num_chunks`. The transaction is complete if
    // number of chunks = num_chunks - 1, the last chunk is not written on disk
    // and is kept in memory instead.
    Ok(n_subkeys >= num_chunks)
}

fn chunked_transaction_num_chunks_by_path<Host: Runtime>(
    host: &mut Host,
    chunked_transaction_path: &OwnedPath,
) -> Result<u16, Error> {
    let chunked_transaction_num_chunks_path =
        chunked_transaction_num_chunks_path(chunked_transaction_path)?;
    let mut buffer = [0u8; 2];
    store_read_slice(host, &chunked_transaction_num_chunks_path, &mut buffer, 2)?;
    Ok(u16::from_le_bytes(buffer))
}

pub fn chunked_transaction_num_chunks<Host: Runtime>(
    host: &mut Host,
    tx_hash: &TransactionHash,
) -> Result<u16, Error> {
    let chunked_transaction_path = chunked_transaction_path(tx_hash)?;
    chunked_transaction_num_chunks_by_path(host, &chunked_transaction_path)
}

fn store_transaction_chunk_data<Host: Runtime>(
    host: &mut Host,
    transaction_chunk_path: &OwnedPath,
    data: Vec<u8>,
) -> Result<(), Error> {
    match host.store_has(transaction_chunk_path)? {
        Some(ValueType::Value | ValueType::ValueWithSubtree) => Ok(()),
        _ => {
            if data.len() > MAX_FILE_CHUNK_SIZE {
                // It comes from an input so it's maximum 4096 bytes (with the message header).
                let (data1, data2) = data.split_at(MAX_FILE_CHUNK_SIZE);
                host.store_write(transaction_chunk_path, data1, 0)?;
                host.store_write(transaction_chunk_path, data2, MAX_FILE_CHUNK_SIZE)
            } else {
                host.store_write(transaction_chunk_path, &data, 0)
            }?;
            Ok(())
        }
    }
}

fn read_transaction_chunk_data<Host: Runtime>(
    host: &mut Host,
    transaction_chunk_path: &OwnedPath,
) -> Result<Vec<u8>, Error> {
    let data_size = host.store_value_size(transaction_chunk_path)?;

    if data_size > MAX_FILE_CHUNK_SIZE {
        let mut data1 =
            host.store_read(transaction_chunk_path, 0, MAX_FILE_CHUNK_SIZE)?;
        let mut data2 = host.store_read(
            transaction_chunk_path,
            MAX_FILE_CHUNK_SIZE,
            MAX_FILE_CHUNK_SIZE,
        )?;
        let _ = &mut data1.append(&mut data2);
        Ok(data1)
    } else {
        Ok(host.store_read(transaction_chunk_path, 0, MAX_FILE_CHUNK_SIZE)?)
    }
}

fn get_full_transaction<Host: Runtime>(
    host: &mut Host,
    chunked_transaction_path: &OwnedPath,
    num_chunks: u16,
    missing_data: &[u8],
) -> Result<Vec<u8>, Error> {
    let mut buffer = Vec::new();
    for i in 0..num_chunks {
        let transaction_chunk_path = transaction_chunk_path(chunked_transaction_path, i)?;
        // If the transaction is complete and a chunk doesn't exist, it means that it is
        // the last missing chunk, that was not stored in the storage.
        match host.store_has(&transaction_chunk_path)? {
            None => buffer.extend_from_slice(missing_data),
            Some(_) => {
                let mut data =
                    read_transaction_chunk_data(host, &transaction_chunk_path)?;
                let _ = &mut buffer.append(&mut data);
            }
        }
    }
    Ok(buffer)
}

pub fn remove_chunked_transaction_by_path<Host: Runtime>(
    host: &mut Host,
    path: &OwnedPath,
) -> Result<(), Error> {
    host.store_delete(path).map_err(Error::from)
}

pub fn remove_chunked_transaction<Host: Runtime>(
    host: &mut Host,
    tx_hash: &TransactionHash,
) -> Result<(), Error> {
    let chunked_transaction_path = chunked_transaction_path(tx_hash)?;
    remove_chunked_transaction_by_path(host, &chunked_transaction_path)
}

/// Store the transaction chunk in the storage. Returns the full transaction
/// if the last chunk to store is the last missing chunk.
pub fn store_transaction_chunk<Host: Runtime>(
    host: &mut Host,
    tx_hash: &TransactionHash,
    i: u16,
    data: Vec<u8>,
) -> Result<Option<Vec<u8>>, Error> {
    let chunked_transaction_path = chunked_transaction_path(tx_hash)?;
    let num_chunks =
        chunked_transaction_num_chunks_by_path(host, &chunked_transaction_path)?;

    if is_transaction_complete(host, &chunked_transaction_path, num_chunks)? {
        let data =
            get_full_transaction(host, &chunked_transaction_path, num_chunks, &data)?;
        host.store_delete(&chunked_transaction_path)?;
        Ok(Some(data))
    } else {
        let transaction_chunk_path =
            transaction_chunk_path(&chunked_transaction_path, i)?;
        store_transaction_chunk_data(host, &transaction_chunk_path, data)?;

        Ok(None)
    }
}

pub fn create_chunked_transaction<Host: Runtime>(
    host: &mut Host,
    tx_hash: &TransactionHash,
    num_chunks: u16,
) -> Result<(), Error> {
    let chunked_transaction_path = chunked_transaction_path(tx_hash)?;
    let chunked_transaction_num_chunks_path =
        chunked_transaction_num_chunks_path(&chunked_transaction_path)?;
    host.store_write(
        &chunked_transaction_num_chunks_path,
        &u16::to_le_bytes(num_chunks),
        0,
    )
    .map_err(Error::from)
}
