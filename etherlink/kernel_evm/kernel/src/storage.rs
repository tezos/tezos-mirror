// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::block_in_progress::BlockInProgress;
use crate::event::Event;
use crate::indexable_storage::IndexableStorage;
use crate::simulation::SimulationResult;
use anyhow::Context;
use evm_execution::account_storage::EthereumAccount;
use evm_execution::storage::blocks::add_new_block_hash;
use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::*;
use tezos_smart_rollup_host::runtime::{Runtime, ValueType};

use crate::error::{Error, StorageError};
use rlp::{Decodable, Encodable, Rlp};
use tezos_ethereum::block::L2Block;
use tezos_ethereum::rlp_helpers::FromRlpBytes;
use tezos_ethereum::transaction::{
    TransactionHash, TransactionObject, TransactionReceipt,
};
use tezos_ethereum::wei::Wei;

use primitive_types::{H160, H256, U256};

pub const STORAGE_VERSION: u64 = 7;
pub const STORAGE_VERSION_PATH: RefPath = RefPath::assert_from(b"/storage_version");

const KERNEL_VERSION_PATH: RefPath = RefPath::assert_from(b"/kernel_version");

const TICKETER: RefPath = RefPath::assert_from(b"/ticketer");
pub const ADMIN: RefPath = RefPath::assert_from(b"/admin");
const SEQUENCER_ADMIN: RefPath = RefPath::assert_from(b"/sequencer_admin");
const KERNEL_GOVERNANCE: RefPath = RefPath::assert_from(b"/kernel_governance");
const DELAYED_BRIDGE: RefPath = RefPath::assert_from(b"/delayed_bridge");

// Path to the block in progress, used between reboots
const EVM_BLOCK_IN_PROGRESS: RefPath = RefPath::assert_from(b"/blocks/in_progress");

const EVM_CURRENT_BLOCK: RefPath = RefPath::assert_from(b"/world_state/blocks/current");
pub const EVM_BLOCKS: RefPath = RefPath::assert_from(b"/world_state/blocks");
const BLOCK_NUMBER: RefPath = RefPath::assert_from(b"/number");
const BLOCK_HASH: RefPath = RefPath::assert_from(b"/hash");

const EVENTS: RefPath = RefPath::assert_from(b"/events");

pub const EVM_TRANSACTIONS_RECEIPTS: RefPath =
    RefPath::assert_from(b"/world_state/transactions_receipts");

pub const EVM_TRANSACTIONS_OBJECTS: RefPath =
    RefPath::assert_from(b"/world_state/transactions_objects");

const EVM_CHAIN_ID: RefPath = RefPath::assert_from(b"/chain_id");

const EVM_BASE_FEE_PER_GAS: RefPath = RefPath::assert_from(b"/base_fee_per_gas");
const EVM_MINIMUM_BASE_FEE_PER_GAS: RefPath =
    RefPath::assert_from(b"/fees/minimum_base_fee_per_gas");
const EVM_DA_FEE: RefPath = RefPath::assert_from(b"/fees/da_fee_per_byte");

/// The sequencer pool is the designated account that the data-availability fees are sent to.
///
/// This may be updated by the governance mechanism over time. If it is not set, the data-availability
/// fees are instead burned.
pub const SEQUENCER_POOL_PATH: RefPath =
    RefPath::assert_from(b"/fees/sequencer_pool_address");

/// Path to the last L1 level seen.
const EVM_L1_LEVEL: RefPath = RefPath::assert_from(b"/l1_level");

const EVM_BURNED_FEES: RefPath = RefPath::assert_from(b"/fees/burned");

/// Path to the last info per level timestamp seen.
const EVM_INFO_PER_LEVEL_TIMESTAMP: RefPath =
    RefPath::assert_from(b"/info_per_level/timestamp");
/// Path to the number of timestamps read, use to compute the average block time.
const EVM_INFO_PER_LEVEL_STATS_NUMBERS: RefPath =
    RefPath::assert_from(b"/info_per_level/stats/numbers");
/// Path to the sum of distance between blocks, used to compute the average block time.
const EVM_INFO_PER_LEVEL_STATS_TOTAL: RefPath =
    RefPath::assert_from(b"/info_per_level/stats/total");

pub const SIMULATION_RESULT: RefPath = RefPath::assert_from(b"/simulation_result");

pub const DEPOSIT_NONCE: RefPath = RefPath::assert_from(b"/deposit_nonce");

/// Path where all indexes are stored.
const EVM_INDEXES: RefPath = RefPath::assert_from(b"/indexes");

/// Path where Ethereum accounts are stored.
const ACCOUNTS_INDEX: RefPath = RefPath::assert_from(b"/accounts");

/// Subpath where blocks are indexed.
const BLOCKS_INDEX: RefPath = EVM_BLOCKS;

/// Subpath where transactions are indexed
const TRANSACTIONS_INDEX: RefPath = RefPath::assert_from(b"/transactions");

// Path to the number of seconds until delayed txs are timed out.
const EVM_DELAYED_INBOX_TIMEOUT: RefPath =
    RefPath::assert_from(b"/delayed_inbox_timeout");

// Path to the number of l1 levels that need to pass for a
// delayed tx to be timed out.
const EVM_DELAYED_INBOX_MIN_LEVELS: RefPath =
    RefPath::assert_from(b"/delayed_inbox_min_levels");

/// The size of one 256 bit word. Size in bytes
pub const WORD_SIZE: usize = 32usize;

// Path to the tz1 administrating the sequencer. If there is nothing
// at this path, the kernel is in proxy mode.
pub const SEQUENCER: RefPath = RefPath::assert_from(b"/sequencer");

pub fn store_read_slice<Host: Runtime, T: Path>(
    host: &Host,
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

/// Read a single unsigned 256 bit value from storage at the path given.
fn read_u256(host: &impl Runtime, path: &OwnedPath) -> Result<U256, Error> {
    let bytes = host.store_read(path, 0, WORD_SIZE)?;
    Ok(Wei::from_little_endian(&bytes))
}

pub fn write_u256(
    host: &mut impl Runtime,
    path: &OwnedPath,
    value: U256,
) -> Result<(), Error> {
    let mut bytes: [u8; WORD_SIZE] = value.into();
    value.to_little_endian(&mut bytes);
    host.store_write(path, &bytes, 0).map_err(Error::from)
}

pub fn block_path(hash: H256) -> Result<OwnedPath, Error> {
    let hash = hex::encode(hash);
    let raw_hash_path: Vec<u8> = format!("/{}", &hash).into();
    let hash_path = OwnedPath::try_from(raw_hash_path)?;
    concat(&EVM_BLOCKS, &hash_path).map_err(Error::from)
}

pub fn receipt_path(receipt_hash: &TransactionHash) -> Result<OwnedPath, Error> {
    let hash = hex::encode(receipt_hash);
    let raw_receipt_path: Vec<u8> = format!("/{}", &hash).into();
    let receipt_path = OwnedPath::try_from(raw_receipt_path)?;
    concat(&EVM_TRANSACTIONS_RECEIPTS, &receipt_path).map_err(Error::from)
}

pub fn object_path(object_hash: &TransactionHash) -> Result<OwnedPath, Error> {
    let hash = hex::encode(object_hash);
    let raw_object_path: Vec<u8> = format!("/{}", &hash).into();
    let object_path = OwnedPath::try_from(raw_object_path)?;
    concat(&EVM_TRANSACTIONS_OBJECTS, &object_path).map_err(Error::from)
}

pub fn read_current_block_number<Host: Runtime>(host: &Host) -> Result<U256, Error> {
    let path = concat(&EVM_CURRENT_BLOCK, &BLOCK_NUMBER)?;
    let mut buffer = [0_u8; 8];
    store_read_slice(host, &path, &mut buffer, 8)?;
    Ok(U256::from_little_endian(&buffer))
}

pub fn read_current_block_hash<Host: Runtime>(host: &Host) -> Result<H256, Error> {
    let path = concat(&EVM_CURRENT_BLOCK, &BLOCK_HASH)?;
    let mut buffer = [0_u8; 32];
    store_read_slice(host, &path, &mut buffer, 32)?;
    Ok(H256::from_slice(&buffer))
}

pub fn store_rlp<T: Encodable, Host: Runtime>(
    src: &T,
    host: &mut Host,
    path: &impl Path,
) -> Result<(), Error> {
    let bytes = src.rlp_bytes();
    host.store_write_all(path, &bytes).map_err(Error::from)
}

pub fn read_rlp<T: Decodable, Host: Runtime>(
    host: &Host,
    path: &impl Path,
) -> Result<T, Error> {
    let bytes = host.store_read_all(path)?;
    FromRlpBytes::from_rlp_bytes(&bytes).map_err(Error::from)
}

/// Read data from the durable storage under the given path.
///
/// If there is no data, None is returned.
pub fn read_optional_rlp<T: Decodable>(
    host: &impl Runtime,
    path: &impl Path,
) -> Result<Option<T>, anyhow::Error> {
    if let Some(ValueType::Value) = host.store_has(path)? {
        let elt = read_rlp(host, path)?;
        Ok(Some(elt))
    } else {
        Ok(None)
    }
}

pub fn read_current_block<Host: Runtime>(host: &mut Host) -> Result<L2Block, Error> {
    let hash = read_current_block_hash(host)?;
    let block_path = block_path(hash)?;
    let block = read_rlp(host, &block_path)?;
    Ok(block)
}

fn store_current_block_number_and_hash<Host: Runtime>(
    host: &mut Host,
    block_path: &OwnedPath,
    block_number: U256,
    block_hash: H256,
) -> Result<(), Error> {
    let number_path = concat(block_path, &BLOCK_NUMBER)?;
    let hash_path = concat(block_path, &BLOCK_HASH)?;
    let mut le_block_number: [u8; 32] = [0; 32];
    block_number.to_little_endian(&mut le_block_number);
    host.store_write(&number_path, &le_block_number, 0)?;
    host.store_write(&hash_path, block_hash.as_bytes(), 0)
        .map_err(Error::from)
}

fn store_block<Host: Runtime>(
    host: &mut Host,
    block: &L2Block,
    block_path: &OwnedPath,
) -> Result<(), Error> {
    let mut index = init_blocks_index()?;
    index_block(host, &block.hash, &mut index)?;
    store_rlp(block, host, block_path)
}

pub fn store_block_by_hash<Host: Runtime>(
    host: &mut Host,
    block: &L2Block,
) -> Result<(), Error> {
    let block_path = block_path(block.hash)?;
    store_block(host, block, &block_path)
}

fn store_current_block_nodebug<Host: Runtime>(
    host: &mut Host,
    block: &L2Block,
) -> Result<(), Error> {
    let current_block_path = OwnedPath::from(EVM_CURRENT_BLOCK);
    // We only need to store current block's number and hash so we avoid the storage of duplicate informations.
    store_current_block_number_and_hash(
        host,
        &current_block_path,
        block.number,
        block.hash,
    )?;
    // We store the current block hash so the BLOCKHASH opcode can retrieve the block hash
    // by its number and return it in the execution flow.
    // The routine to clean the outdated hashes (see BLOCKHASH's spec.) is within [add_new_block_hash].
    add_new_block_hash(host, block.number, block.hash)
        .map_err(|_| Error::Storage(StorageError::BlockHashStorageFailed))?;
    // When storing the current block's infos we need to store it under the [evm/blocks/<block_hash>]
    store_block_by_hash(host, block)
}

pub fn store_current_block<Host: Runtime>(
    host: &mut Host,
    block: &L2Block,
) -> Result<(), Error> {
    match store_current_block_nodebug(host, block) {
        Ok(()) => {
            log!(
                host,
                Info,
                "Storing block {} at {} containing {} transaction(s) for {} gas used.",
                block.number,
                block.timestamp,
                block.transactions.len(),
                U256::to_string(&block.gas_used)
            );
            Ok(())
        }
        Err(e) => {
            log!(host, Error, "Block storing failed: {:?}", e);
            Err(e)
        }
    }
}
pub fn store_simulation_result<Host: Runtime, T: Encodable>(
    host: &mut Host,
    result: SimulationResult<T, String>,
) -> Result<(), anyhow::Error> {
    let encoded = result.rlp_bytes();
    host.store_write(&SIMULATION_RESULT, &encoded, 0)
        .context("Failed to write the simulation result.")
}

pub fn store_transaction_receipt<Host: Runtime>(
    host: &mut Host,
    receipt: &TransactionReceipt,
) -> Result<u64, anyhow::Error> {
    // For each transaction hash there exists a receipt and an object. As
    // such the indexing must be done either with the objects or the
    // receipts otherwise they would be indexed twice. We chose to index
    // hashes using the receipts.
    let mut transaction_hashes_index = init_transaction_hashes_index()?;
    index_transaction_hash(host, &receipt.hash, &mut transaction_hashes_index)?;
    let receipt_path = receipt_path(&receipt.hash)?;
    let src: &[u8] = &receipt.rlp_bytes();
    log!(host, Debug, "Storing receipt of size {}", src.len());
    host.store_write_all(&receipt_path, src)?;
    Ok(src.len().try_into()?)
}

pub fn store_transaction_object<Host: Runtime>(
    host: &mut Host,
    object: &TransactionObject,
) -> Result<u64, anyhow::Error> {
    let object_path = object_path(&object.hash)?;
    let encoded: &[u8] = &object.rlp_bytes();
    log!(
        host,
        Debug,
        "Storing transaction object of size {}",
        encoded.len()
    );
    host.store_write_all(&object_path, encoded)?;
    Ok(encoded.len().try_into()?)
}

const CHUNKED_TRANSACTIONS: RefPath = RefPath::assert_from(b"/chunked_transactions");
const CHUNKED_TRANSACTION_NUM_CHUNKS: RefPath = RefPath::assert_from(b"/num_chunks");
const CHUNKED_HASHES: RefPath = RefPath::assert_from(b"/chunk_hashes");

pub fn chunked_transaction_path(tx_hash: &TransactionHash) -> Result<OwnedPath, Error> {
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

pub fn chunked_hash_transaction_path(
    chunked_hash: &[u8],
    chunked_transaction_path: &OwnedPath,
) -> Result<OwnedPath, Error> {
    let hash = hex::encode(chunked_hash);
    let raw_chunked_hash_key: Vec<u8> = format!("/{}", hash).into();
    let chunked_hash_key = OwnedPath::try_from(raw_chunked_hash_key)?;
    let chunked_hash_path = concat(&CHUNKED_HASHES, &chunked_hash_key)?;
    concat(chunked_transaction_path, &chunked_hash_path).map_err(Error::from)
}

pub fn transaction_chunk_path(
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
    // `n_subkeys` includes `num_chunks` and `chunk_hashes` keys
    Ok(n_subkeys >= num_chunks + 2)
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

pub fn read_transaction_chunk_data<Host: Runtime>(
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
) -> Result<Vec<u8>, Error> {
    let mut buffer = Vec::new();
    for i in 0..num_chunks {
        let transaction_chunk_path = transaction_chunk_path(chunked_transaction_path, i)?;
        let mut data = read_transaction_chunk_data(host, &transaction_chunk_path)?;
        let _ = &mut buffer.append(&mut data);
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

    // Store the new transaction chunk.
    let transaction_chunk_path = transaction_chunk_path(&chunked_transaction_path, i)?;
    store_transaction_chunk_data(host, &transaction_chunk_path, data)?;

    // If the chunk was the last one, we gather all the chunks and remove the
    // sub elements.
    if is_transaction_complete(host, &chunked_transaction_path, num_chunks)? {
        let data = get_full_transaction(host, &chunked_transaction_path, num_chunks)?;
        host.store_delete(&chunked_transaction_path)?;
        Ok(Some(data))
    } else {
        Ok(None)
    }
}

pub fn create_chunked_transaction<Host: Runtime>(
    host: &mut Host,
    tx_hash: &TransactionHash,
    num_chunks: u16,
    chunk_hashes: Vec<TransactionHash>,
) -> Result<(), Error> {
    let chunked_transaction_path = chunked_transaction_path(tx_hash)?;

    // A new chunked transaction creates the `../<tx_hash>/num_chunks`, if there
    // is at least one key, it was already created.
    if host
        .store_count_subkeys(&chunked_transaction_path)
        .unwrap_or(0)
        > 0
    {
        log!(
            host,
            Info,
            "The chunked transaction {} already exist, ignoring the message.\n",
            hex::encode(tx_hash)
        );
        return Ok(());
    }

    let chunked_transaction_num_chunks_path =
        chunked_transaction_num_chunks_path(&chunked_transaction_path)?;
    host.store_write(
        &chunked_transaction_num_chunks_path,
        &u16::to_le_bytes(num_chunks),
        0,
    )?;

    for chunk_hash in chunk_hashes.iter() {
        let chunk_hash_path =
            chunked_hash_transaction_path(chunk_hash, &chunked_transaction_path)?;
        host.store_write(&chunk_hash_path, &[0], 0)?
    }

    Ok(())
}

pub fn store_chain_id<Host: Runtime>(
    host: &mut Host,
    chain_id: U256,
) -> Result<(), Error> {
    write_u256(host, &EVM_CHAIN_ID.into(), chain_id)
}

pub fn read_chain_id<Host: Runtime>(host: &mut Host) -> Result<U256, Error> {
    read_u256(host, &EVM_CHAIN_ID.into())
}

pub fn store_base_fee_per_gas<Host: Runtime>(
    host: &mut Host,
    base_fee_per_gas: U256,
) -> Result<(), Error> {
    write_u256(host, &EVM_BASE_FEE_PER_GAS.into(), base_fee_per_gas)
}

pub fn read_base_fee_per_gas<Host: Runtime>(host: &mut Host) -> Result<U256, Error> {
    read_u256(host, &EVM_BASE_FEE_PER_GAS.into())
}

pub fn read_minimum_base_fee_per_gas<Host: Runtime>(
    host: &mut Host,
) -> Result<U256, Error> {
    read_u256(host, &EVM_MINIMUM_BASE_FEE_PER_GAS.into())
}

pub fn store_da_fee(
    host: &mut impl Runtime,
    base_fee_per_gas: U256,
) -> Result<(), Error> {
    write_u256(host, &EVM_DA_FEE.into(), base_fee_per_gas)
}

pub fn read_da_fee(host: &impl Runtime) -> Result<U256, Error> {
    read_u256(host, &EVM_DA_FEE.into())
}

pub fn update_burned_fees(
    host: &mut impl Runtime,
    burned_fee: U256,
) -> Result<(), Error> {
    let path = &EVM_BURNED_FEES.into();
    let current = read_u256(host, path).unwrap_or_else(|_| U256::zero());
    let new = current.saturating_add(burned_fee);
    write_u256(host, path, new)
}

#[cfg(test)]
pub fn read_burned_fees(host: &mut impl Runtime) -> U256 {
    let path = &EVM_BURNED_FEES.into();
    read_u256(host, path).unwrap_or_else(|_| U256::zero())
}

pub fn read_sequencer_pool_address(host: &impl Runtime) -> Option<H160> {
    let mut bytes = [0; std::mem::size_of::<H160>()];
    let Ok(20) = host.store_read_slice(&SEQUENCER_POOL_PATH, 0, bytes.as_mut_slice()) else {
        log!(host, Debug, "No sequencer pool address set");
        return None;
    };
    Some(bytes.into())
}

#[cfg(test)]
pub fn store_sequencer_pool_address(
    host: &mut impl Runtime,
    address: H160,
) -> Result<(), Error> {
    let bytes = address.to_fixed_bytes();
    host.store_write_all(&SEQUENCER_POOL_PATH, bytes.as_slice())?;
    Ok(())
}

pub fn store_timestamp_path<Host: Runtime>(
    host: &mut Host,
    path: &OwnedPath,
    timestamp: &Timestamp,
) -> Result<(), Error> {
    host.store_write(path, &timestamp.i64().to_le_bytes(), 0)?;
    Ok(())
}

#[allow(dead_code)]
pub fn read_l1_level<Host: Runtime>(host: &mut Host) -> Result<u32, Error> {
    let mut buffer = [0u8; 4];
    store_read_slice(host, &EVM_L1_LEVEL, &mut buffer, 4)?;
    let level = u32::from_le_bytes(buffer);
    Ok(level)
}

pub fn store_l1_level<Host: Runtime>(host: &mut Host, level: u32) -> Result<(), Error> {
    host.store_write(&EVM_L1_LEVEL, &level.to_le_bytes(), 0)?;
    Ok(())
}

pub fn read_last_info_per_level_timestamp_stats<Host: Runtime>(
    host: &mut Host,
) -> Result<(i64, i64), Error> {
    let mut buffer = [0u8; 8];
    store_read_slice(host, &EVM_INFO_PER_LEVEL_STATS_NUMBERS, &mut buffer, 8)?;
    let numbers = i64::from_le_bytes(buffer);

    let mut buffer = [0u8; 8];
    store_read_slice(host, &EVM_INFO_PER_LEVEL_STATS_TOTAL, &mut buffer, 8)?;
    let total = i64::from_le_bytes(buffer);

    Ok((numbers, total))
}

fn store_info_per_level_stats<Host: Runtime>(
    host: &mut Host,
    new_timestamp: Timestamp,
) -> Result<(), Error> {
    let old_timestamp =
        read_last_info_per_level_timestamp(host).unwrap_or_else(|_| Timestamp::from(0));
    let diff = new_timestamp.i64() - old_timestamp.i64();
    let (numbers, total) =
        read_last_info_per_level_timestamp_stats(host).unwrap_or((0i64, 0i64));
    let total = total + diff;
    let numbers = numbers + 1;

    host.store_write(&EVM_INFO_PER_LEVEL_STATS_TOTAL, &total.to_le_bytes(), 0)?;
    host.store_write(&EVM_INFO_PER_LEVEL_STATS_NUMBERS, &numbers.to_le_bytes(), 0)?;

    Ok(())
}

pub fn store_last_info_per_level_timestamp<Host: Runtime>(
    host: &mut Host,
    timestamp: Timestamp,
) -> Result<(), Error> {
    store_timestamp_path(host, &EVM_INFO_PER_LEVEL_TIMESTAMP.into(), &timestamp)?;
    store_info_per_level_stats(host, timestamp)
}

pub fn read_timestamp_path<Host: Runtime>(
    host: &Host,
    path: &OwnedPath,
) -> Result<Timestamp, Error> {
    let mut buffer = [0u8; 8];
    store_read_slice(host, path, &mut buffer, 8)?;
    let timestamp_as_i64 = i64::from_le_bytes(buffer);
    Ok(timestamp_as_i64.into())
}

pub fn read_last_info_per_level_timestamp<Host: Runtime>(
    host: &Host,
) -> Result<Timestamp, Error> {
    read_timestamp_path(host, &EVM_INFO_PER_LEVEL_TIMESTAMP.into())
}

/// Get the index of accounts.
pub fn init_account_index() -> Result<IndexableStorage, StorageError> {
    let path = concat(&EVM_INDEXES, &ACCOUNTS_INDEX)?;
    IndexableStorage::new(&RefPath::from(&path))
}

/// Get the index of blocks.
pub fn init_blocks_index() -> Result<IndexableStorage, StorageError> {
    let path = concat(&EVM_INDEXES, &BLOCKS_INDEX)?;
    IndexableStorage::new(&RefPath::from(&path))
}

/// Get the index of transactions
pub fn init_transaction_hashes_index() -> Result<IndexableStorage, StorageError> {
    let path = concat(&EVM_INDEXES, &TRANSACTIONS_INDEX)?;
    IndexableStorage::new(&RefPath::from(&path))
}

pub fn index_account(
    host: &mut impl Runtime,
    address: &H160,
    index: &mut IndexableStorage,
) -> Result<(), Error> {
    let account = EthereumAccount::from_address(address)?;
    // A new account is created whether when it receives assets for the
    // first time, or when it is created through a contract deployment, by
    // construction it should be indexed at this specific times.
    if account.indexed(host)? {
        Ok(())
    } else {
        index.push_value(host, address.as_bytes())?;
        account.set_indexed(host).map_err(Error::from)
    }
}

fn read_b58_kt1<Host: Runtime>(host: &Host, path: &OwnedPath) -> Option<ContractKt1Hash> {
    let mut buffer = [0; 36];
    store_read_slice(host, path, &mut buffer, 36).ok()?;
    let kt1_b58 = String::from_utf8(buffer.to_vec()).ok()?;
    ContractKt1Hash::from_b58check(&kt1_b58).ok()
}

/// Reads the ticketer address set by the installer, if any, encoded in b58.
pub fn read_ticketer<Host: Runtime>(host: &mut Host) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &TICKETER.into())
}

pub fn read_admin<Host: Runtime>(host: &mut Host) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &ADMIN.into())
}

pub fn read_sequencer_admin<Host: Runtime>(host: &mut Host) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &SEQUENCER_ADMIN.into())
}

pub fn read_kernel_governance<Host: Runtime>(host: &mut Host) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &KERNEL_GOVERNANCE.into())
}

pub fn get_and_increment_deposit_nonce<Host: Runtime>(
    host: &mut Host,
) -> Result<u32, Error> {
    let current_nonce = || -> Option<u32> {
        let bytes = host.store_read_all(&DEPOSIT_NONCE).ok()?;
        let slice_of_bytes: [u8; 4] = bytes[..]
            .try_into()
            .map_err(|_| Error::InvalidConversion)
            .ok()?;
        Some(u32::from_le_bytes(slice_of_bytes))
    };

    let nonce = current_nonce().unwrap_or(0u32);
    let new_nonce = nonce + 1;
    host.store_write_all(&DEPOSIT_NONCE, &new_nonce.to_le_bytes())?;
    Ok(nonce)
}

pub fn index_block(
    host: &mut impl Runtime,
    block_hash: &H256,
    blocks_index: &mut IndexableStorage,
) -> Result<(), Error> {
    blocks_index
        .push_value(host, block_hash.as_bytes())
        .map_err(Error::from)
}

pub fn index_transaction_hash(
    host: &mut impl Runtime,
    transaction_hash: &[u8],
    transaction_hashes_index: &mut IndexableStorage,
) -> Result<(), Error> {
    transaction_hashes_index
        .push_value(host, transaction_hash)
        .map_err(Error::from)
}

pub fn store_storage_version<Host: Runtime>(
    host: &mut Host,
    storage_version: u64,
) -> Result<(), Error> {
    host.store_write_all(&STORAGE_VERSION_PATH, &storage_version.to_le_bytes())
        .map_err(Error::from)
}

pub fn read_storage_version<Host: Runtime>(host: &mut Host) -> Result<u64, Error> {
    match host.store_read_all(&STORAGE_VERSION_PATH) {
        Ok(bytes) => {
            let slice_of_bytes: [u8; 8] =
                bytes[..].try_into().map_err(|_| Error::InvalidConversion)?;
            Ok(u64::from_le_bytes(slice_of_bytes))
        }
        Err(e) => Err(e.into()),
    }
}

pub fn read_kernel_version<Host: Runtime>(host: &mut Host) -> Result<String, Error> {
    match host.store_read_all(&KERNEL_VERSION_PATH) {
        Ok(bytes) => {
            let kernel_version =
                std::str::from_utf8(&bytes).map_err(|_| Error::InvalidConversion)?;
            Ok(kernel_version.to_owned())
        }
        Err(e) => Err(e.into()),
    }
}

pub fn store_kernel_version<Host: Runtime>(
    host: &mut Host,
    kernel_version: &str,
) -> Result<(), Error> {
    let kernel_version = kernel_version.as_bytes();
    host.store_write_all(&KERNEL_VERSION_PATH, kernel_version)
        .map_err(Error::from)
}

pub fn store_block_in_progress<Host: Runtime>(
    host: &mut Host,
    bip: &BlockInProgress,
) -> anyhow::Result<()> {
    let path = OwnedPath::from(EVM_BLOCK_IN_PROGRESS);
    let bytes = &bip.rlp_bytes();
    log!(
        host,
        Debug,
        "Storing Block In Progress of size {}",
        bytes.len()
    );
    host.store_write_all(&path, bytes)
        .context("Failed to store current block in progress")
}

pub fn read_block_in_progress<Host: Runtime>(
    host: &Host,
) -> anyhow::Result<Option<BlockInProgress>> {
    let path = OwnedPath::from(EVM_BLOCK_IN_PROGRESS);
    if let Some(ValueType::Value) = host.store_has(&path)? {
        let bytes = host
            .store_read_all(&path)
            .context("Failed to read current block in progress")?;
        log!(
            host,
            Debug,
            "Reading Block In Progress of size {}",
            bytes.len()
        );
        let decoder = Rlp::new(bytes.as_slice());
        let bip = BlockInProgress::decode(&decoder)
            .context("Failed to decode current block in progress")?;
        Ok(Some(bip))
    } else {
        Ok(None)
    }
}

pub fn delete_block_in_progress<Host: Runtime>(host: &mut Host) -> anyhow::Result<()> {
    host.store_delete(&EVM_BLOCK_IN_PROGRESS)
        .context("Failed to delete block in progress")
}

pub fn sequencer<Host: Runtime>(host: &Host) -> anyhow::Result<Option<PublicKey>> {
    if host.store_has(&SEQUENCER)?.is_some() {
        let bytes = host.store_read_all(&SEQUENCER)?;
        let Ok(tz1_b58) = String::from_utf8(bytes) else { return Ok(None) };
        let Ok(tz1) = PublicKey::from_b58check(&tz1_b58) else { return Ok(None)};
        Ok(Some(tz1))
    } else {
        Ok(None)
    }
}

pub fn remove_sequencer<Host: Runtime>(host: &mut Host) -> anyhow::Result<()> {
    host.store_delete(&SEQUENCER).map_err(Into::into)
}

pub fn store_sequencer<Host: Runtime>(
    host: &mut Host,
    public_key: &PublicKey,
) -> anyhow::Result<()> {
    let pk_b58 = PublicKey::to_b58check(public_key);
    let bytes = String::as_bytes(&pk_b58);
    host.store_write_all(&SEQUENCER, bytes).map_err(Into::into)
}

pub fn clear_events<Host: Runtime>(host: &mut Host) -> anyhow::Result<()> {
    if host.store_has(&EVENTS)?.is_some() {
        host.store_delete(&EVENTS)
            .context("Failed to delete old events")
    } else {
        Ok(())
    }
}

pub fn store_event<Host: Runtime>(host: &mut Host, event: &Event) -> anyhow::Result<()> {
    let index = IndexableStorage::new(&EVENTS)?;
    index
        .push_value(host, &event.rlp_bytes())
        .map_err(Into::into)
}

pub fn delayed_inbox_timeout<Host: Runtime>(host: &Host) -> anyhow::Result<u64> {
    // The default timeout is 12 hours
    let default_timeout = 43200;
    if host.store_has(&EVM_DELAYED_INBOX_TIMEOUT)?.is_some() {
        let mut buffer = [0u8; 8];
        store_read_slice(host, &EVM_DELAYED_INBOX_TIMEOUT, &mut buffer, 8)?;
        let timeout = u64::from_le_bytes(buffer);
        log!(
            host,
            Info,
            "Using delayed inbox timeout of {} seconds ({} hours)",
            timeout,
            timeout / 3600
        );
        Ok(timeout)
    } else {
        log!(
            host,
            Info,
            "Using default delayed inbox timeout of {} seconds ({} hours)",
            default_timeout,
            default_timeout / 3600
        );
        Ok(default_timeout)
    }
}

pub fn delayed_inbox_min_levels<Host: Runtime>(host: &Host) -> anyhow::Result<u32> {
    let default_min_levels = 720;
    if host.store_has(&EVM_DELAYED_INBOX_MIN_LEVELS)?.is_some() {
        let mut buffer = [0u8; 4];
        store_read_slice(host, &EVM_DELAYED_INBOX_MIN_LEVELS, &mut buffer, 4)?;
        let min_levels = u32::from_le_bytes(buffer);
        log!(
            host,
            Info,
            "Using delayed inbox minimum levels: {}",
            min_levels
        );
        Ok(min_levels)
    } else {
        log!(
            host,
            Info,
            "Using default delayed inbox minimum levels: {}",
            default_min_levels
        );
        Ok(default_min_levels)
    }
}

#[cfg(test)]
mod internal_for_tests {
    use super::*;

    use tezos_ethereum::transaction::TransactionStatus;

    /// Reads status from the receipt in storage.
    pub fn read_transaction_receipt_status<Host: Runtime>(
        host: &mut Host,
        tx_hash: &TransactionHash,
    ) -> Result<TransactionStatus, Error> {
        let receipt = read_transaction_receipt(host, tx_hash)?;
        Ok(receipt.status)
    }

    /// Reads a transaction receipt from storage.
    pub fn read_transaction_receipt<Host: Runtime>(
        host: &mut Host,
        tx_hash: &TransactionHash,
    ) -> Result<TransactionReceipt, Error> {
        let receipt_path = receipt_path(tx_hash)?;
        let bytes = host.store_read_all(&receipt_path)?;
        let receipt = TransactionReceipt::from_rlp_bytes(&bytes)?;
        Ok(receipt)
    }
}

#[cfg(test)]
pub use internal_for_tests::*;

/// Smart Contract of the delayed bridge
///
/// This smart contract is used to submit transactions to the rollup
/// when in sequencer mode
pub fn read_delayed_transaction_bridge<Host: Runtime>(
    host: &Host,
) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &DELAYED_BRIDGE.into())
}

#[cfg(test)]
mod tests {
    use tezos_smart_rollup_mock::MockHost;

    #[test]
    fn update_burned_fees() {
        // Arrange
        let mut host = MockHost::default();

        let fst = 17.into();
        let snd = 19.into();

        // Act
        let result_fst = super::update_burned_fees(&mut host, fst);
        let result_snd = super::update_burned_fees(&mut host, snd);

        // Assert
        assert!(result_fst.is_ok());
        assert!(result_snd.is_ok());

        let burned = super::read_burned_fees(&mut host);
        assert_eq!(fst + snd, burned);
    }
}
