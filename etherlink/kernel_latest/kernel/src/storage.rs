// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::block_in_progress::BlockInProgress;
use crate::chains::ChainFamily;
use crate::event::Event;
use crate::simulation::SimulationResult;
use crate::tick_model::constants::MAXIMUM_GAS_LIMIT;
use anyhow::Context;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use revm_etherlink::inspectors::call_tracer::CallTracerInput;
use revm_etherlink::inspectors::struct_logger::StructLoggerInput;
use revm_etherlink::inspectors::{TracerInput, CALL_TRACER_CONFIG_PREFIX};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_indexable_storage::IndexableStorage;
use tezos_smart_rollup::host::RuntimeError;
use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::*;
use tezos_smart_rollup_host::runtime::ValueType;
use tezos_storage::{
    read_b58_kt1, read_u256_le, read_u64_le, store_read_slice, write_u256_le,
    write_u64_le,
};

use crate::error::{Error, StorageError};
use rlp::{Decodable, Encodable, Rlp};
use tezos_ethereum::rlp_helpers::{FromRlpBytes, VersionedEncoding};
use tezos_ethereum::transaction::TransactionHash;

use primitive_types::{H160, U256};

#[derive(
    FromPrimitive, ToPrimitive, Copy, Debug, Clone, PartialEq, Eq, PartialOrd, Ord,
)]
#[repr(u64)]
pub enum StorageVersion {
    V11 = 11,
    V12,
    V13,
    V14,
    V15,
    V16,
    V17,
    V18,
    V19,
    V20,
    V21,
    V22,
    V23,
    V24,
    V25,
    V26,
    V27,
    V28,
    V29,
    V30,
    V31,
    V32,
    V33,
    V34,
    V35,
    V36,
    V37,
    V38,
    V39,
    V40,
    V41,
    V42,
    V43,
    V44,
    V45,
    V46,
    V47,
    V48,
    V49,
}

impl From<StorageVersion> for u64 {
    fn from(value: StorageVersion) -> Self {
        ToPrimitive::to_u64(&value).expect("StorageVersion fits in `u64` primitive")
    }
}

impl StorageVersion {
    pub fn next(self) -> Option<StorageVersion> {
        FromPrimitive::from_u64(u64::from(self) + 1)
    }
}

pub const STORAGE_VERSION: StorageVersion = StorageVersion::V49;

pub const PRIVATE_FLAG_PATH: RefPath = RefPath::assert_from(b"/evm/remove_whitelist");

pub const STORAGE_VERSION_PATH: RefPath = RefPath::assert_from(b"/evm/storage_version");

const KERNEL_VERSION_PATH: RefPath = RefPath::assert_from(b"/evm/kernel_version");

pub const ADMIN: RefPath = RefPath::assert_from(b"/evm/admin");
pub const SEQUENCER_GOVERNANCE: RefPath =
    RefPath::assert_from(b"/evm/sequencer_governance");
pub const KERNEL_GOVERNANCE: RefPath = RefPath::assert_from(b"/evm/kernel_governance");
pub const KERNEL_SECURITY_GOVERNANCE: RefPath =
    RefPath::assert_from(b"/evm/kernel_security_governance");
pub const DELAYED_BRIDGE: RefPath = RefPath::assert_from(b"/evm/delayed_bridge");

pub const MAXIMUM_ALLOWED_TICKS: RefPath =
    RefPath::assert_from(b"/evm/maximum_allowed_ticks");

pub const MAXIMUM_GAS_PER_TRANSACTION: RefPath =
    RefPath::assert_from(b"/evm/maximum_gas_per_transaction");

// Path to the block in progress, used between reboots
const EVM_BLOCK_IN_PROGRESS: RefPath =
    RefPath::assert_from(b"/evm/world_state/blocks/in_progress");

const EVENTS: RefPath = RefPath::assert_from(b"/evm/events");

pub const EVM_TRANSACTIONS_RECEIPTS: RefPath =
    RefPath::assert_from(b"/evm/world_state/transactions_receipts");

pub const EVM_TRANSACTIONS_OBJECTS: RefPath =
    RefPath::assert_from(b"/evm/world_state/transactions_objects");

const EVM_CHAIN_ID: RefPath = RefPath::assert_from(b"/evm/chain_id");

// Path to the Multichain feature flag. If there is nothing at this path,
// a single chain is used.
#[allow(dead_code)]
pub const ENABLE_MULTICHAIN: RefPath =
    RefPath::assert_from(b"/evm/feature_flags/enable_multichain");

pub const ENABLE_TEZOS_RUNTIME: RefPath =
    RefPath::assert_from(b"/evm/feature_flags/enable_tezos_runtime");

// Root for chain configurations. Informations about a chain are available by appending its chain ID.
pub const CHAIN_CONFIGURATIONS: RefPath =
    RefPath::assert_from(b"/evm/chain_configurations");

const EVM_MINIMUM_BASE_FEE_PER_GAS: RefPath =
    RefPath::assert_from(b"/evm/world_state/fees/minimum_base_fee_per_gas");
const EVM_DA_FEE: RefPath =
    RefPath::assert_from(b"/evm/world_state/fees/da_fee_per_byte");
const BACKLOG_PATH: RefPath = RefPath::assert_from(b"/evm/world_state/fees/backlog");
const BACKLOG_TIMESTAMP_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/fees/last_timestamp");

/// The sequencer pool is the designated account that the data-availability fees are sent to.
///
/// This may be updated by the governance mechanism over time. If it is not set, the data-availability
/// fees are instead burned.
pub const SEQUENCER_POOL_PATH: RefPath =
    RefPath::assert_from(b"/evm/sequencer_pool_address");

/// Path to the last L1 level seen.
const EVM_L1_LEVEL: RefPath = RefPath::assert_from(b"/evm/l1_level");

const EVM_BURNED_FEES: RefPath = RefPath::assert_from(b"/evm/world_state/fees/burned");

/// Path to the last info per level timestamp seen.
const EVM_INFO_PER_LEVEL_TIMESTAMP: RefPath =
    RefPath::assert_from(b"/evm/info_per_level/timestamp");

pub const SIMULATION_RESULT: RefPath = RefPath::assert_from(b"/evm/simulation_result");

// Path to the number of seconds until delayed txs are timed out.
const EVM_DELAYED_INBOX_TIMEOUT: RefPath =
    RefPath::assert_from(b"/evm/delayed_inbox_timeout");

// Path to the number of l1 levels that need to pass for a
// delayed tx to be timed out.
const EVM_DELAYED_INBOX_MIN_LEVELS: RefPath =
    RefPath::assert_from(b"/evm/delayed_inbox_min_levels");

// Path to the tz1 administrating the sequencer. If there is nothing
// at this path, the kernel is in proxy mode.
pub const SEQUENCER: RefPath = RefPath::assert_from(b"/evm/sequencer");

pub const KEEP_EVENTS: RefPath = RefPath::assert_from(b"/evm/keep_events");

// Path to the DAL feature flag. If there is nothing at this path, DAL
// is not used.
pub const ENABLE_DAL: RefPath = RefPath::assert_from(b"/evm/feature_flags/enable_dal");

// Path to the flag that disables legacy DAL slot import signals.
// If there is something at this path, the kernel ignores DalSlotImportSignals
// external messages and instead relies on DalAttestedSlots internal messages
// from the protocol.
pub const DISABLE_LEGACY_DAL_SIGNALS: RefPath =
    RefPath::assert_from(b"/evm/feature_flags/disable_legacy_dal_signals");

// Path to the DAL slot indices to use.
pub const DAL_SLOTS: RefPath = RefPath::assert_from(b"/evm/dal_slots");

// Path where the input for the tracer is stored by the sequencer.
const TRACER_INPUT: RefPath = RefPath::assert_from(b"/evm/trace/input");

// If this path contains a value, the fa bridge is enabled in the kernel.
pub const ENABLE_FA_BRIDGE: RefPath =
    RefPath::assert_from(b"/evm/feature_flags/enable_fa_bridge");

const MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS: RefPath =
    RefPath::assert_from(b"/evm/max_blueprint_lookahead_in_seconds");

pub fn chain_config_path(chain_id: &U256) -> Result<OwnedPath, Error> {
    let raw_chain_id_path: Vec<u8> = format!("/{chain_id}").into();
    let chain_id_path = OwnedPath::try_from(raw_chain_id_path)?;
    concat(&CHAIN_CONFIGURATIONS, &chain_id_path).map_err(Error::from)
}

pub fn store_simulation_result<Host: Runtime, T: Decodable + Encodable>(
    host: &mut Host,
    result: SimulationResult<T, String>,
) -> Result<(), anyhow::Error> {
    let encoded = result.to_bytes();
    host.store_write(&SIMULATION_RESULT, &encoded, 0)
        .context("Failed to write the simulation result.")
}

const CHUNKED_TRANSACTIONS: RefPath = RefPath::assert_from(b"/chunked_transactions");
const CHUNKED_TRANSACTION_NUM_CHUNKS: RefPath = RefPath::assert_from(b"/num_chunks");
const CHUNKED_HASHES: RefPath = RefPath::assert_from(b"/chunk_hashes");

pub fn chunked_transaction_path(tx_hash: &TransactionHash) -> Result<OwnedPath, Error> {
    let hash = hex::encode(tx_hash);
    let raw_chunked_transaction_path: Vec<u8> = format!("/{hash}").into();
    let chunked_transaction_path = OwnedPath::try_from(raw_chunked_transaction_path)?;
    concat(&CHUNKED_TRANSACTIONS, &chunked_transaction_path).map_err(Error::from)
}

fn chunked_transaction_num_chunks_path(
    chunked_transaction_path: &OwnedPath,
) -> Result<OwnedPath, Error> {
    concat(chunked_transaction_path, &CHUNKED_TRANSACTION_NUM_CHUNKS).map_err(Error::from)
}

pub fn chunked_hashes_transaction_path(
    chunked_transaction_path: &OwnedPath,
) -> Result<OwnedPath, Error> {
    concat(chunked_transaction_path, &CHUNKED_HASHES).map_err(Error::from)
}

pub fn chunked_transaction_hash_exists<Host: Runtime>(
    host: &mut Host,
    chunked_transaction_path: &OwnedPath,
    hash: &TransactionHash,
) -> Result<bool, Error> {
    let path = chunked_hashes_transaction_path(chunked_transaction_path)?;
    let value = host.store_read_all(&path)?;
    let rlp = rlp::Rlp::new(value.as_slice());
    for e in rlp.as_list::<Vec<u8>>()? {
        let h = TransactionHash::try_from(e.as_slice())
            .map_err(|_| Error::RlpDecoderError(rlp::DecoderError::RlpInvalidLength))?;
        if h == *hash {
            return Ok(true);
        }
    }
    Ok(false)
}

pub fn transaction_chunk_path(
    chunked_transaction_path: &OwnedPath,
    i: u16,
) -> Result<OwnedPath, Error> {
    let raw_i_path: Vec<u8> = format!("/{i}").into();
    let i_path = OwnedPath::try_from(raw_i_path)?;
    concat(chunked_transaction_path, &i_path).map_err(Error::from)
}

fn is_transaction_complete<Host: Runtime>(
    host: &mut Host,
    chunked_transaction_path: &OwnedPath,
    num_chunks: u16,
) -> Result<bool, Error> {
    for i in 0..num_chunks {
        let transaction_chunk_path = transaction_chunk_path(chunked_transaction_path, i)?;
        if host.store_has(&transaction_chunk_path)?.is_none() {
            return Ok(false);
        }
    }
    Ok(true)
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

pub fn remove_chunked_transaction<Host: Runtime>(
    host: &mut Host,
    tx_hash: &TransactionHash,
) -> Result<(), Error> {
    let chunked_transaction_path = chunked_transaction_path(tx_hash)?;
    let nb_chunks =
        match chunked_transaction_num_chunks_by_path(host, &chunked_transaction_path) {
            Ok(n) => n,
            Err(Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))) => {
                return Ok(())
            }
            Err(e) => return Err(e),
        };
    for i in 0..nb_chunks {
        let transaction_chunk_path =
            transaction_chunk_path(&chunked_transaction_path, i)?;
        host.store_delete(&transaction_chunk_path)?;
    }
    host.store_delete(&chunked_transaction_num_chunks_path(
        &chunked_transaction_path,
    )?)?;
    host.store_delete(&chunked_hashes_transaction_path(&chunked_transaction_path)?)?;
    Ok(())
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
        remove_chunked_transaction(host, tx_hash)?;
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
    if chunked_transaction_num_chunks_by_path(host, &chunked_transaction_path)
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

    let mut rlp = rlp::RlpStream::new_list(chunk_hashes.len());
    for chunk_hash in chunk_hashes.iter() {
        rlp.append(&chunk_hash.as_slice());
    }
    host.store_write(
        &chunked_hashes_transaction_path(&chunked_transaction_path)?,
        &rlp.out(),
        0,
    )?;
    Ok(())
}

pub fn store_chain_id<Host: Runtime>(
    host: &mut Host,
    chain_id: U256,
) -> Result<(), Error> {
    write_u256_le(host, &EVM_CHAIN_ID, chain_id).map_err(Error::from)
}

pub fn read_chain_id<Host: Runtime>(host: &Host) -> Result<U256, Error> {
    read_u256_le(host, &EVM_CHAIN_ID).map_err(Error::from)
}

pub fn read_minimum_base_fee_per_gas<Host: Runtime>(host: &Host) -> Result<U256, Error> {
    read_u256_le(host, &EVM_MINIMUM_BASE_FEE_PER_GAS).map_err(Error::from)
}

pub fn read_backlog(host: &impl Runtime) -> Result<u64, Error> {
    read_u64_le(host, &BACKLOG_PATH).map_err(Error::from)
}

pub fn store_backlog(host: &mut impl Runtime, value: u64) -> Result<(), Error> {
    write_u64_le(host, &BACKLOG_PATH, value).map_err(Error::from)
}

pub fn read_backlog_timestamp(host: &impl Runtime) -> Result<u64, Error> {
    read_u64_le(host, &BACKLOG_TIMESTAMP_PATH).map_err(Error::from)
}

pub fn store_backlog_timestamp(host: &mut impl Runtime, value: u64) -> Result<(), Error> {
    write_u64_le(host, &BACKLOG_TIMESTAMP_PATH, value)?;
    Ok(())
}

pub fn store_minimum_base_fee_per_gas<Host: Runtime>(
    host: &mut Host,
    price: U256,
) -> Result<(), Error> {
    write_u256_le(host, &EVM_MINIMUM_BASE_FEE_PER_GAS, price).map_err(Error::from)
}

pub fn store_da_fee(
    host: &mut impl Runtime,
    base_fee_per_gas: U256,
) -> Result<(), Error> {
    write_u256_le(host, &EVM_DA_FEE, base_fee_per_gas).map_err(Error::from)
}

pub fn read_da_fee(host: &impl Runtime) -> Result<U256, Error> {
    read_u256_le(host, &EVM_DA_FEE).map_err(Error::from)
}

pub fn update_burned_fees(
    host: &mut impl Runtime,
    burned_fee: U256,
) -> Result<(), Error> {
    let path = &EVM_BURNED_FEES;
    let current = read_u256_le(host, path).unwrap_or_else(|_| U256::zero());
    let new = current.saturating_add(burned_fee);
    write_u256_le(host, path, new).map_err(Error::from)
}

#[cfg(test)]
pub fn read_burned_fees(host: &mut impl Runtime) -> U256 {
    let path = &EVM_BURNED_FEES;
    read_u256_le(host, path).unwrap_or_else(|_| U256::zero())
}

pub fn read_sequencer_pool_address(host: &impl Runtime) -> Option<H160> {
    let mut bytes = [0; std::mem::size_of::<H160>()];
    let Ok(20) = host.store_read_slice(&SEQUENCER_POOL_PATH, 0, bytes.as_mut_slice())
    else {
        log!(host, Debug, "No sequencer pool address set");
        return None;
    };
    Some(bytes.into())
}

pub fn store_sequencer_pool_address(
    host: &mut impl Runtime,
    address: H160,
) -> Result<(), Error> {
    let bytes = address.to_fixed_bytes();
    host.store_write(&SEQUENCER_POOL_PATH, bytes.as_slice(), 0)?;
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

pub fn store_last_info_per_level_timestamp<Host: Runtime>(
    host: &mut Host,
    timestamp: Timestamp,
) -> Result<(), Error> {
    store_timestamp_path(host, &EVM_INFO_PER_LEVEL_TIMESTAMP.into(), &timestamp)
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

pub fn read_admin<Host: Runtime>(host: &mut Host) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &ADMIN)
}

pub fn read_sequencer_governance<Host: Runtime>(
    host: &mut Host,
) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &SEQUENCER_GOVERNANCE)
}

pub fn read_kernel_governance<Host: Runtime>(host: &mut Host) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &KERNEL_GOVERNANCE)
}

pub fn read_kernel_security_governance<Host: Runtime>(
    host: &mut Host,
) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &KERNEL_SECURITY_GOVERNANCE)
}

pub fn read_maximum_allowed_ticks<Host: Runtime>(host: &mut Host) -> Option<u64> {
    read_u64_le(host, &MAXIMUM_ALLOWED_TICKS).ok()
}

/// Reads the maximum gas per transaction. If the value cannot found in the storage,
/// we write the kernel default value in the storage. The value becomes accessible
/// from outside the kernel.
pub fn read_or_set_maximum_gas_per_transaction<Host: Runtime>(
    host: &mut Host,
) -> anyhow::Result<u64> {
    match read_u64_le(host, &MAXIMUM_GAS_PER_TRANSACTION) {
        Ok(gas_limit) => Ok(gas_limit),
        Err(_) => {
            write_u64_le(host, &MAXIMUM_GAS_PER_TRANSACTION, MAXIMUM_GAS_LIMIT)?;
            Ok(MAXIMUM_GAS_LIMIT)
        }
    }
}

pub fn store_storage_version<Host: Runtime>(
    host: &mut Host,
    storage_version: StorageVersion,
) -> Result<(), Error> {
    let storage_version = u64::from(storage_version);
    host.store_write(&STORAGE_VERSION_PATH, &storage_version.to_le_bytes(), 0)
        .map_err(Error::from)
}

pub fn read_storage_version<Host: Runtime>(
    host: &mut Host,
) -> Result<StorageVersion, Error> {
    match host.store_read(&STORAGE_VERSION_PATH, 0, std::mem::size_of::<u64>()) {
        Ok(bytes) => {
            let slice_of_bytes: [u8; 8] =
                bytes[..].try_into().map_err(|_| Error::InvalidConversion)?;
            let version_u64 = u64::from_le_bytes(slice_of_bytes);
            let version =
                FromPrimitive::from_u64(version_u64).ok_or(Error::InvalidConversion)?;
            log!(host, Debug, "Current storage version: {:?}", version);
            Ok(version)
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

// DO NOT RENAME: function name is used during benchmark
// Never inlined when the kernel is compiled for benchmarks, to ensure the
// function is visible in the profiling results.
#[cfg_attr(feature = "benchmark", inline(never))]
pub fn store_block_in_progress<Host: Runtime, Tx: Encodable>(
    host: &mut Host,
    bip: &BlockInProgress<Tx>,
) -> anyhow::Result<()> {
    let path = OwnedPath::from(EVM_BLOCK_IN_PROGRESS);
    let bytes = &bip.rlp_bytes();
    log!(
        host,
        Benchmarking,
        "Storing Block In Progress of size {}",
        bytes.len()
    );
    host.store_write_all(&path, bytes)
        .context("Failed to store current block in progress")
}

// DO NOT RENAME: function name is used during benchmark
// Never inlined when the kernel is compiled for benchmarks, to ensure the
// function is visible in the profiling results.
#[cfg_attr(feature = "benchmark", inline(never))]
pub fn read_block_in_progress<Host: Runtime, Tx: Decodable>(
    host: &Host,
) -> anyhow::Result<Option<BlockInProgress<Tx>>> {
    let path = OwnedPath::from(EVM_BLOCK_IN_PROGRESS);
    if let Some(ValueType::Value) = host.store_has(&path)? {
        let bytes = host
            .store_read_all(&path)
            .context("Failed to read current block in progress")?;
        log!(
            host,
            Benchmarking,
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
        let Ok(tz1_b58) = String::from_utf8(bytes) else {
            return Ok(None);
        };
        let Ok(tz1) = PublicKey::from_b58check(&tz1_b58) else {
            return Ok(None);
        };
        Ok(Some(tz1))
    } else {
        Ok(None)
    }
}

pub fn enable_dal<Host: Runtime>(host: &Host) -> anyhow::Result<bool> {
    if let Some(ValueType::Value) = host.store_has(&ENABLE_DAL)? {
        // When run from the EVM node, the DAL feature is always
        // considered as disabled.
        let b = host.is_evm_node();
        Ok(!b)
    } else {
        Ok(false)
    }
}

pub fn enable_tezos_runtime<Host: Runtime>(host: &Host) -> bool {
    Some(ValueType::Value) == host.store_has(&ENABLE_TEZOS_RUNTIME).unwrap_or(None)
}

pub fn tweak_dal_activation<Host: Runtime>(
    host: &mut Host,
    activate_dal: bool,
) -> anyhow::Result<()> {
    if activate_dal {
        host.store_write(&ENABLE_DAL, &[], 0)?
    } else {
        host.store_delete(&ENABLE_DAL)?
    }
    Ok(())
}

pub fn store_dal_slots<Host: Runtime>(
    host: &mut Host,
    slots: &[u8],
) -> anyhow::Result<()> {
    Ok(host.store_write_all(&DAL_SLOTS, slots)?)
}

/// Returns true if legacy DAL slot import signals are disabled.
/// When disabled, the kernel ignores `DalSlotImportSignals` external messages
/// and instead relies on `DalAttestedSlots` internal messages.
pub fn is_legacy_dal_signals_disabled<Host: Runtime>(
    host: &Host,
) -> anyhow::Result<bool> {
    Ok(host.store_has(&DISABLE_LEGACY_DAL_SIGNALS)?.is_some())
}

pub fn dal_slots<Host: Runtime>(host: &Host) -> anyhow::Result<Option<Vec<u8>>> {
    if host.store_has(&DAL_SLOTS)?.is_some() {
        let bytes = host.store_read_all(&DAL_SLOTS)?;
        Ok(Some(bytes))
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
    host.store_write(&SEQUENCER, bytes, 0).map_err(Into::into)
}

pub fn clear_events<Host: Runtime>(host: &mut Host) -> anyhow::Result<()> {
    if host.store_has(&KEEP_EVENTS)?.is_some() {
        host.store_delete(&KEEP_EVENTS)?;
        Ok(())
    } else {
        let index = IndexableStorage::new(&EVENTS)?;
        index.clear(host).map_err(Into::into)
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
            Debug,
            "Using delayed inbox timeout of {} seconds ({} hours)",
            timeout,
            timeout / 3600
        );
        Ok(timeout)
    } else {
        log!(
            host,
            Debug,
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
            Debug,
            "Using delayed inbox minimum levels: {}",
            min_levels
        );
        Ok(min_levels)
    } else {
        log!(
            host,
            Debug,
            "Using default delayed inbox minimum levels: {}",
            default_min_levels
        );
        Ok(default_min_levels)
    }
}

pub fn read_tracer_input<Host: Runtime>(
    host: &mut Host,
) -> anyhow::Result<Option<TracerInput>> {
    if let Some(ValueType::Value) = host.store_has(&TRACER_INPUT).map_err(Error::from)? {
        let bytes = host
            .store_read_all(&TRACER_INPUT)
            .context("Cannot read tracer input")?;

        let tracer = if bytes[0] == CALL_TRACER_CONFIG_PREFIX {
            let call_tracer_input: CallTracerInput =
                FromRlpBytes::from_rlp_bytes(&bytes[1..])?;
            TracerInput::CallTracer(call_tracer_input)
        } else {
            let struct_logger_input: StructLoggerInput =
                FromRlpBytes::from_rlp_bytes(&bytes)?;
            TracerInput::StructLogger(struct_logger_input)
        };
        log!(host, Debug, "Tracer input found: {:?}", tracer);

        Ok(Some(tracer))
    } else {
        Ok(None)
    }
}

pub fn is_enable_fa_bridge(host: &impl Runtime) -> anyhow::Result<bool> {
    if let Some(ValueType::Value) = host.store_has(&ENABLE_FA_BRIDGE)? {
        Ok(true)
    } else {
        Ok(false)
    }
}

pub fn max_blueprint_lookahead_in_seconds(host: &impl Runtime) -> anyhow::Result<i64> {
    let bytes = host.store_read(&MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS, 0, 8)?;
    let bytes: [u8; 8] = bytes.as_slice().try_into()?;
    Ok(i64::from_le_bytes(bytes))
}

// Storage functions related to a chain configuration

pub fn read_chain_family(
    host: &impl Runtime,
    chain_id: U256,
) -> anyhow::Result<ChainFamily> {
    let chain_configurations_path = chain_config_path(&chain_id)?;
    let chain_family_path = RefPath::assert_from(b"/chain_family");
    let path = concat(&chain_configurations_path, &chain_family_path)?;
    let bytes = host
        .store_read_all(&path)
        .context(format!("Cannot read chain family for chain {chain_id}"))?;
    let chain_family = String::from_utf8(bytes)?;
    Ok(chain_family.into())
}

/// Smart Contract of the delayed bridge
///
/// This smart contract is used to submit transactions to the rollup
/// when in sequencer mode
pub fn read_delayed_transaction_bridge<Host: Runtime>(
    host: &Host,
) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &DELAYED_BRIDGE)
}

#[cfg(test)]
mod tests {
    use tezos_evm_runtime::runtime::MockKernelHost;

    #[test]
    fn update_burned_fees() {
        // Arrange
        let mut host = MockKernelHost::default();

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
