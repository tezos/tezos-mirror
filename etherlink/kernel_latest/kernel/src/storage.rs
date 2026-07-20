// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::block_in_progress::BlockInProgress;
use crate::event::Event;
use crate::simulation::SimulationResult;
use crate::tick_model::constants::MAXIMUM_GAS_LIMIT;
use anyhow::Context;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use revm_etherlink::inspectors::call_tracer::CallTracerInput;
use revm_etherlink::inspectors::struct_logger::StructLoggerInput;
use revm_etherlink::inspectors::{TracerInput, CALL_TRACER_CONFIG_PREFIX};
use tezos_crypto_rs::hash::ChainId;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_data_encoding::nom::NomReader;
use tezos_evm_logging::{log, Level::*};
use tezos_indexable_storage::KeyspaceIndexableStorage;
use tezos_smart_rollup::host::RuntimeError;
use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_encoding::public_key_hash::PublicKeyHash;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::*;
use tezos_smart_rollup_host::runtime::ValueType;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{Key, KeySpace, KeySpaceLoader, Name};
use tezos_storage::{
    keyspace, read_b58_kt1, read_optional_nom_value, read_u256_le, read_u64_le,
    store_bin, store_read_slice, write_u256_le, write_u64_le,
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
    // V47 is the lowest storage version any live network holds. Keep the
    // variant (its migration body is cleared) so `read_storage_version` can map
    // the on-chain `47` and step to V48; otherwise `FromPrimitive::from_u64`
    // fails with `InvalidConversion` and migration aborts. Pre-V47 variants are
    // dropped as no live network is below V47. Don't remove until all are >= V48.
    V47 = 47,
    V48,
    V49,
    V50,
    V51,
    V52,
    V53,
    V54,
    V55,
    V56,
    V57,
    V58,
    V59,
    V60,
    V61,
    V62,
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

pub const STORAGE_VERSION: StorageVersion = StorageVersion::V62;

/// Name of the `/base` keyspace, holding kernel configuration and
/// node-interaction values that do not belong to any world state.
pub const BASE_KEYSPACE_NAME: Name = Name::from_static("/base");

/// Load the `/base` keyspace.
///
/// Scoping contract (single-owner rule): within any phase (configuration
/// fetch, stage one, block production), exactly one live `/base` handle may
/// exist. Load it once at the top of the phase, thread it down as
/// `&impl KeySpace` (or `&mut impl KeySpace` for writers) and let it drop at
/// the end of the phase — helpers must receive the handle, never re-load.
/// Isolated leaf accessors running outside any such phase may load
/// transiently instead. A failure therefore signals a programming error
/// (another `/base` handle is still alive, or a keyspace overlapping `/base`
/// was loaded earlier in the run), never a normal runtime condition.
pub fn load_base_keyspace<L: KeySpaceLoader>(
    loader: &mut L,
) -> Result<L::KeySpace, StorageError> {
    loader
        .load_or_create(BASE_KEYSPACE_NAME)
        .map_err(StorageError::KeySpaceLoad)
}

pub const PRIVATE_FLAG_KEY: Key = Key::from_static(b"/remove_whitelist");

// Canonical durable path of the storage version. The reader/writer go through
// the `/base` keyspace via [`STORAGE_VERSION_KEY`]; this absolute form is kept
// for documentation and tests asserting the resolved path.
#[allow(dead_code)]
pub const STORAGE_VERSION_PATH: RefPath = RefPath::assert_from(b"/base/storage_version");
// Legacy storage version path, outside the `/base` keyspace. Still read raw as
// a fallback for state written before the value moved under `/base`.
pub const LEGACY_STORAGE_VERSION_PATH: RefPath =
    RefPath::assert_from(b"/evm/storage_version");

// Key to the storage version, inside the `/base` keyspace. Resolves to the
// durable path `/base/storage_version`.
const STORAGE_VERSION_KEY: Key = Key::from_static(b"/storage_version");

// Key to the kernel version, inside the `/base` keyspace. Resolves to the
// durable path `/base/kernel_version`.
const KERNEL_VERSION_KEY: Key = Key::from_static(b"/kernel_version");

// `/base/admin` keeps an absolute form next to the relative key: the
// reveal-storage bootstrap writes it through the raw host while readers go
// through the `/base` keyspace.
pub const ADMIN: RefPath = RefPath::assert_from(b"/base/admin");
const ADMIN_KEY: Key = Key::from_static(b"/admin");
pub const SEQUENCER_GOVERNANCE: RefPath =
    RefPath::assert_from(b"/evm/world_state/sequencer_governance");
const KERNEL_GOVERNANCE_KEY: Key = Key::from_static(b"/kernel_governance");
const KERNEL_SECURITY_GOVERNANCE_KEY: Key =
    Key::from_static(b"/kernel_security_governance");
const DELAYED_BRIDGE_KEY: Key = Key::from_static(b"/delayed_bridge");

const MAXIMUM_ALLOWED_TICKS_KEY: Key = Key::from_static(b"/maximum_allowed_ticks");

pub const STAGE_ONE_WITNESS_PATH: RefPath =
    RefPath::assert_from(b"/base/stage_one_witness");

pub const MAXIMUM_GAS_PER_TRANSACTION: RefPath =
    RefPath::assert_from(b"/evm/world_state/maximum_gas_per_transaction");

// Path to the block in progress, used between reboots
pub const EVM_BLOCK_IN_PROGRESS: RefPath =
    RefPath::assert_from(b"/evm/world_state/blocks/in_progress");

/// Relative key of the rollup events index inside the `/base` keyspace. It
/// resolves to the absolute durable path `/base/rollup_events`, where the
/// push-only counter (`length`) and indexed values are stored.
const EVENTS_KEY: Key = Key::from_static(b"/rollup_events");

const EVM_CHAIN_ID_PATH: RefPath = RefPath::assert_from(b"/evm/world_state/chain_id");

const MICHELSON_RUNTIME_CHAIN_ID: RefPath =
    RefPath::assert_from(b"/tez/world_state/chain_id");

// Path to the Multichain feature flag. If there is nothing at this path,
// a single chain is used.
#[allow(dead_code)]
pub const ENABLE_MULTICHAIN: RefPath =
    RefPath::assert_from(b"/base/feature_flags/enable_multichain");

// The absolute form remains for its writers (migrations and tests write the
// flag through the raw host); the reader goes through the `/base` keyspace.
#[allow(dead_code)]
pub const ENABLE_TEZOS_RUNTIME: RefPath =
    RefPath::assert_from(b"/base/feature_flags/enable_tezos_runtime");
const ENABLE_TEZOS_RUNTIME_KEY: Key =
    Key::from_static(b"/feature_flags/enable_tezos_runtime");

// Target EVM block number for the Michelson runtime sunrise. Written by the
// installer when scheduling a future activation.
const MICHELSON_RUNTIME_TARGET_SUNRISE_LEVEL: RefPath =
    RefPath::assert_from(b"/tez/world_state/michelson_runtime/target_sunrise_level");

// EVM block number where the Michelson runtime first started producing
// Tezos blocks. Written once by the kernel at the sunrise block.
const MICHELSON_RUNTIME_SUNRISE_LEVEL: RefPath =
    RefPath::assert_from(b"/tez/world_state/michelson_runtime/sunrise_level");

// Key to the Michelson gas-refund feature flag, inside the `/base` keyspace.
// Written by the installer/node; the reader goes through the `/base` keyspace.
// Resolves to `/base/feature_flags/enable_michelson_gas_refund`.
const ENABLE_MICHELSON_GAS_REFUND_KEY: Key =
    Key::from_static(b"/feature_flags/enable_michelson_gas_refund");

// Debug Features
pub const ENABLE_DEBUG_PRECOMPILES: RefPath =
    RefPath::assert_from(b"/base/debug_features_flags/enable_debug_precompiles");
// EOF Debug Features

const EVM_MINIMUM_BASE_FEE_PER_GAS: RefPath =
    RefPath::assert_from(b"/evm/world_state/fees/minimum_base_fee_per_gas");
const EVM_MICHELSON_TO_EVM_GAS_MULTIPLIER: RefPath =
    RefPath::assert_from(b"/evm/world_state/fees/michelson_to_evm_gas_multiplier");
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
    RefPath::assert_from(b"/evm/world_state/sequencer_pool_address");

/// Key to the last L1 level seen, inside the `/base` keyspace. Resolves to
/// the durable path `/base/l1_level`.
const L1_LEVEL_KEY: Key = Key::from_static(b"/l1_level");

const EVM_BURNED_FEES: RefPath = RefPath::assert_from(b"/evm/world_state/fees/burned");

/// Key to the last info per level timestamp seen, inside the `/base`
/// keyspace. Resolves to the durable path `/base/info_per_level/timestamp`.
const INFO_PER_LEVEL_TIMESTAMP_KEY: Key = Key::from_static(b"/info_per_level/timestamp");

// Canonical durable paths of the simulation outputs. The writers go through
// the `/base` keyspace via the relative keys below; these absolute forms are
// kept for documentation and tests asserting the resolved path.
#[allow(dead_code)]
pub const SIMULATION_RESULT: RefPath =
    RefPath::assert_from(b"/base/evm_simulation_result");
#[allow(dead_code)]
pub const SIMULATION_HTTP_TRACES: RefPath =
    RefPath::assert_from(b"/base/simulation_http_traces");

// Key to the simulation result, inside the `/base` keyspace. Resolves to the
// durable path `/base/evm_simulation_result`.
const SIMULATION_RESULT_KEY: Key = Key::from_static(b"/evm_simulation_result");
// Key to the aggregate simulation HTTP traces, inside the `/base` keyspace.
// Resolves to the durable path `/base/simulation_http_traces`.
const SIMULATION_HTTP_TRACES_KEY: Key = Key::from_static(b"/simulation_http_traces");

/// Flag path enabling per-transaction HTTP trace capture during block
/// replay. The `__` prefix follows the existing convention for node-driven
/// control keys — cf. `/base/__evm_node`, `/base/__simulation/...`,
/// `/base/__delayed_input`: the node writes the key via `alter_evm_state`
/// just before the replay and the kernel reads it exactly once at the top
/// of `block::produce` (outside the `SafeStorage` wrapper), then threads
/// the resulting boolean through `compute_bip` → `compute` →
/// `TezosXChainConfig::apply_transaction` → the three apply sites. The
/// flag therefore has no lifetime beyond a single replay and nothing
/// else in the kernel touches this path.
pub const HTTP_TRACE_ENABLED: RefPath =
    RefPath::assert_from(b"/base/__http_trace_enabled");

/// Storage root under which traces are persisted per transaction.
///
/// The writes happen through the `SafeStorage`-wrapped host (the apply
/// sites have no direct access to the underlying host), so they land in
/// `/tmp/base/__http_trace/traces`. The block-finalization path calls
/// [`SafeStorage::promote_http_trace`] to move that subtree out of
/// `/tmp/` to the real `/base/__http_trace/traces` location. We do *not*
/// add `/base/__http_trace` to the SafeStorage roots — it would force a
/// `store_copy` of the subtree on every block, while HTTP traces only
/// exist during the dedicated per-tx replay driven by
/// [`HTTP_TRACE_ENABLED`].
const HTTP_TRACES_ROOT: RefPath = RefPath::assert_from(b"/base/__http_trace/traces");

// Key to the number of seconds until delayed txs are timed out, inside the
// `/base` keyspace. Resolves to the durable path `/base/delayed_inbox_timeout`.
const DELAYED_INBOX_TIMEOUT_KEY: Key = Key::from_static(b"/delayed_inbox_timeout");

// Key to the number of l1 levels that need to pass for a delayed tx to be
// timed out, inside the `/base` keyspace. Resolves to the durable path
// `/base/delayed_inbox_min_levels`.
const DELAYED_INBOX_MIN_LEVELS_KEY: Key = Key::from_static(b"/delayed_inbox_min_levels");

// Path to the tz1 administrating the sequencer. If there is nothing
// at this path, the kernel is in proxy mode.
use revm_etherlink::storage::world_state_handler::SEQUENCER_KEY_PATH;

// Key to the one-shot "keep rollup events" flag, inside the `/base` keyspace.
// Written by the installer/node; the reader goes through the `/base` keyspace.
// Resolves to the durable path `/base/keep_rollup_events`.
const KEEP_EVENTS_KEY: Key = Key::from_static(b"/keep_rollup_events");

// Key to the DAL feature flag, inside the `/base` keyspace. If there is
// nothing at this key, DAL is not used. Both reader (`enable_dal`) and writer
// (`tweak_dal_activation`, migration only) go through the keyspace; the key
// resolves to the durable path `/base/feature_flags/enable_dal`.
const ENABLE_DAL_KEY: Key = Key::from_static(b"/feature_flags/enable_dal");

// Key to the flag that disables legacy DAL slot import signals.
// If there is something at this key, the kernel ignores DalSlotImportSignals
// external messages and instead relies on DalAttestedSlots internal messages
// from the protocol.
const DISABLE_LEGACY_DAL_SIGNALS_KEY: Key =
    Key::from_static(b"/feature_flags/disable_legacy_dal_signals");

// Key to the DAL slot indices to use, inside the `/base` keyspace. Both
// reader (`dal_slots`) and writer (`store_dal_slots`, migration only) go
// through the keyspace; the key resolves to the durable path `/base/dal_slots`.
const DAL_SLOTS_KEY: Key = Key::from_static(b"/dal_slots");

// Key to the whitelist of authorized DAL publishers (public key hashes).
// These are the keys authorized to publish DAL slots that the kernel will accept.
// NOTE: Empty whitelist means reject all publishers (therefore all slots).
const DAL_PUBLISHERS_WHITELIST_KEY: Key = Key::from_static(b"/dal_publishers_whitelist");

// Path where the input for the tracer is stored by the sequencer.
const TRACER_INPUT: RefPath = RefPath::assert_from(b"/base/trace/input");

#[cfg(test)]
pub const ENABLE_FA_BRIDGE: RefPath =
    RefPath::assert_from(b"/base/feature_flags/enable_fa_bridge");

const ENABLE_FA_BRIDGE_KEY: Key = Key::from_static(b"/feature_flags/enable_fa_bridge");

const MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS_KEY: Key =
    Key::from_static(b"/max_blueprint_lookahead_in_seconds");

pub fn store_simulation_result<T>(
    host: &mut (impl StorageV1 + KeySpaceLoader),
    result: SimulationResult<T, String>,
) -> Result<(), anyhow::Error>
where
    T: Decodable + Encodable,
{
    let mut base = load_base_keyspace(host)?;
    let encoded = result.to_bytes();
    base.set(&SIMULATION_RESULT_KEY, encoded)
        .context("Failed to write the simulation result.")
}

pub fn store_simulation_http_traces(
    host: &mut (impl StorageV1 + KeySpaceLoader),
    traces: &[tezosx_journal::HttpTrace],
) -> Result<(), anyhow::Error> {
    let mut stream = rlp::RlpStream::new_list(traces.len());
    for trace in traces {
        stream.append(trace);
    }
    let encoded = stream.out();
    let mut base = load_base_keyspace(host)?;
    base.set(&SIMULATION_HTTP_TRACES_KEY, encoded)
        .context("Failed to write the simulation HTTP traces.")
}

/// Returns whether per-transaction HTTP trace capture is enabled for the
/// current kernel run. Called exactly once per block, at the top of
/// `block::produce` (and of `sub_block::handle_run_transaction`) before
/// any `SafeStorage` wrapping, so the lookup is a single host call per
/// block and the result is passed down through the apply chain as a
/// plain boolean. A missing key (the common case) returns `false`; an
/// error is treated as "flag absent" so a transient storage failure
/// cannot crash the replay.
pub fn is_http_trace_enabled(host: &impl StorageV1) -> bool {
    matches!(host.store_has(&HTTP_TRACE_ENABLED), Ok(Some(_)))
}

/// Returns the durable storage path under which the HTTP traces for the
/// transaction `tx_hash` are written when HTTP trace capture is enabled.
pub fn http_traces_path(tx_hash: &TransactionHash) -> Result<OwnedPath, Error> {
    let raw_suffix: Vec<u8> = format!("/{}", hex::encode(tx_hash)).into();
    let suffix = OwnedPath::try_from(raw_suffix)?;
    concat(&HTTP_TRACES_ROOT, &suffix).map_err(Error::from)
}

/// Store the HTTP traces collected for a single transaction during a replay
/// driven by the `http_trace*` RPCs. The traces are RLP-encoded as a list,
/// using the same shape as [`store_simulation_http_traces`] so both outputs
/// share the EVM-node-side decoder.
pub fn store_http_traces_for_tx(
    host: &mut impl StorageV1,
    tx_hash: &TransactionHash,
    traces: &[tezosx_journal::HttpTrace],
) -> Result<(), anyhow::Error> {
    let path = http_traces_path(tx_hash)?;
    let mut stream = rlp::RlpStream::new_list(traces.len());
    for trace in traces {
        stream.append(trace);
    }
    let encoded = stream.out();
    host.store_write_all(&path, &encoded)
        .context("Failed to write the per-transaction HTTP traces.")
}

/// When `enabled` is set and the journal captured at least one HTTP
/// exchange, persist the traces for `tx_hash`. `enabled` comes from a
/// single read of [`HTTP_TRACE_ENABLED`] done once per block outside the
/// `SafeStorage` wrap (see `block::produce`), so the apply sites do not
/// re-read the flag per transaction and the flag itself does not need
/// to live inside the world-state subtree.
///
/// Transactions that performed no cross-runtime HTTP call write nothing
/// — the EVM node side treats a missing key as the empty list, so there
/// is no observable difference and we avoid filling the trace subtree
/// with empty-list entries on blocks where only a minority of
/// transactions perform CRACs. Failures are logged but never
/// propagated, so instrumentation cannot break the replay itself.
pub fn maybe_store_http_traces_for_tx(
    host: &mut impl StorageV1,
    enabled: bool,
    tx_hash: &TransactionHash,
    journal: &tezosx_journal::TezosXJournal,
) {
    if !enabled {
        return;
    }
    let traces = journal.http_traces();
    if traces.is_empty() {
        return;
    }
    if let Err(err) = store_http_traces_for_tx(host, tx_hash, traces) {
        log!(
            Error,
            "Failed to store HTTP traces for tx {}: {:?}",
            hex::encode(tx_hash),
            err
        );
    }
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

pub fn chunked_transaction_hash_exists(
    host: &mut impl StorageV1,
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

fn is_transaction_complete(
    host: &mut impl StorageV1,
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

fn chunked_transaction_num_chunks_by_path(
    host: &mut impl StorageV1,
    chunked_transaction_path: &OwnedPath,
) -> Result<u16, Error> {
    let chunked_transaction_num_chunks_path =
        chunked_transaction_num_chunks_path(chunked_transaction_path)?;
    let mut buffer = [0u8; 2];
    store_read_slice(host, &chunked_transaction_num_chunks_path, &mut buffer, 2)?;
    Ok(u16::from_le_bytes(buffer))
}

pub fn chunked_transaction_num_chunks(
    host: &mut impl StorageV1,
    tx_hash: &TransactionHash,
) -> Result<u16, Error> {
    let chunked_transaction_path = chunked_transaction_path(tx_hash)?;
    chunked_transaction_num_chunks_by_path(host, &chunked_transaction_path)
}

fn store_transaction_chunk_data(
    host: &mut impl StorageV1,
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

pub fn read_transaction_chunk_data(
    host: &mut impl StorageV1,
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

fn get_full_transaction(
    host: &mut impl StorageV1,
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

pub fn remove_chunked_transaction(
    host: &mut impl StorageV1,
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
pub fn store_transaction_chunk(
    host: &mut impl StorageV1,
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

pub fn create_chunked_transaction<Host>(
    host: &mut Host,
    tx_hash: &TransactionHash,
    num_chunks: u16,
    chunk_hashes: Vec<TransactionHash>,
) -> Result<(), Error>
where
    Host: StorageV1,
{
    let chunked_transaction_path = chunked_transaction_path(tx_hash)?;

    // A new chunked transaction creates the `../<tx_hash>/num_chunks`, if there
    // is at least one key, it was already created.
    if chunked_transaction_num_chunks_by_path(host, &chunked_transaction_path)
        .unwrap_or(0)
        > 0
    {
        log!(
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

pub fn store_evm_chain_id(
    host: &mut impl StorageV1,
    evm_chain_id: U256,
) -> Result<(), Error> {
    write_u256_le(host, &EVM_CHAIN_ID_PATH, evm_chain_id).map_err(Error::from)
}

pub fn read_evm_chain_id(host: &impl StorageV1) -> Result<U256, Error> {
    read_u256_le(host, &EVM_CHAIN_ID_PATH).map_err(Error::from)
}

pub fn store_michelson_runtime_chain_id(
    host: &mut impl StorageV1,
    chain_id: &ChainId,
) -> Result<(), Error> {
    store_bin(chain_id, host, &MICHELSON_RUNTIME_CHAIN_ID).map_err(Error::from)
}

pub fn read_michelson_runtime_chain_id(
    host: &impl StorageV1,
) -> Result<Option<ChainId>, Error> {
    read_optional_nom_value(host, &MICHELSON_RUNTIME_CHAIN_ID).map_err(Error::from)
}

pub fn read_minimum_base_fee_per_gas(host: &impl StorageV1) -> Result<U256, Error> {
    read_u256_le(host, &EVM_MINIMUM_BASE_FEE_PER_GAS).map_err(Error::from)
}

pub fn read_backlog(host: &impl StorageV1) -> Result<u64, Error> {
    read_u64_le(host, &BACKLOG_PATH).map_err(Error::from)
}

pub fn store_backlog(host: &mut impl StorageV1, value: u64) -> Result<(), Error> {
    write_u64_le(host, &BACKLOG_PATH, value).map_err(Error::from)
}

pub fn read_backlog_timestamp(host: &impl StorageV1) -> Result<u64, Error> {
    read_u64_le(host, &BACKLOG_TIMESTAMP_PATH).map_err(Error::from)
}

pub fn store_backlog_timestamp(
    host: &mut impl StorageV1,
    value: u64,
) -> Result<(), Error> {
    write_u64_le(host, &BACKLOG_TIMESTAMP_PATH, value)?;
    Ok(())
}

pub fn store_minimum_base_fee_per_gas(
    host: &mut impl StorageV1,
    price: U256,
) -> Result<(), Error> {
    write_u256_le(host, &EVM_MINIMUM_BASE_FEE_PER_GAS, price).map_err(Error::from)
}

pub fn read_michelson_to_evm_gas_multiplier(host: &impl StorageV1) -> Result<u64, Error> {
    read_u64_le(host, &EVM_MICHELSON_TO_EVM_GAS_MULTIPLIER).map_err(Error::from)
}

pub fn store_michelson_to_evm_gas_multiplier(
    host: &mut impl StorageV1,
    value: u64,
) -> Result<(), Error> {
    write_u64_le(host, &EVM_MICHELSON_TO_EVM_GAS_MULTIPLIER, value).map_err(Error::from)
}

pub fn store_da_fee(
    host: &mut impl StorageV1,
    base_fee_per_gas: U256,
) -> Result<(), Error> {
    write_u256_le(host, &EVM_DA_FEE, base_fee_per_gas).map_err(Error::from)
}

pub fn read_da_fee(host: &impl StorageV1) -> Result<U256, Error> {
    read_u256_le(host, &EVM_DA_FEE).map_err(Error::from)
}

pub fn update_burned_fees(
    host: &mut impl StorageV1,
    burned_fee: U256,
) -> Result<(), Error> {
    let path = &EVM_BURNED_FEES;
    let current = read_u256_le(host, path).unwrap_or_else(|_| U256::zero());
    let new = current.saturating_add(burned_fee);
    write_u256_le(host, path, new).map_err(Error::from)
}

#[cfg(test)]
pub fn read_burned_fees(host: &mut impl StorageV1) -> U256 {
    let path = &EVM_BURNED_FEES;
    read_u256_le(host, path).unwrap_or_else(|_| U256::zero())
}

pub fn read_sequencer_pool_address<Host>(host: &Host) -> Option<H160>
where
    Host: StorageV1,
{
    let mut bytes = [0; std::mem::size_of::<H160>()];
    let Ok(20) = host.store_read_slice(&SEQUENCER_POOL_PATH, 0, bytes.as_mut_slice())
    else {
        log!(Debug, "No sequencer pool address set");
        return None;
    };
    Some(bytes.into())
}

pub fn store_sequencer_pool_address(
    host: &mut impl StorageV1,
    address: H160,
) -> Result<(), Error> {
    let bytes = address.to_fixed_bytes();
    host.store_write(&SEQUENCER_POOL_PATH, bytes.as_slice(), 0)?;
    Ok(())
}

#[allow(dead_code)]
pub fn read_l1_level(host: &mut (impl StorageV1 + KeySpaceLoader)) -> Result<u32, Error> {
    let base = load_base_keyspace(host)?;
    Ok(keyspace::read_u32_le(&base, &L1_LEVEL_KEY)?)
}

pub fn store_l1_level(
    host: &mut (impl StorageV1 + KeySpaceLoader),
    level: u32,
) -> Result<(), Error> {
    let mut base = load_base_keyspace(host)?;
    keyspace::write_u32_le(&mut base, &L1_LEVEL_KEY, level)?;
    Ok(())
}

pub fn store_last_info_per_level_timestamp(
    host: &mut (impl StorageV1 + KeySpaceLoader),
    timestamp: Timestamp,
) -> Result<(), Error> {
    let mut base = load_base_keyspace(host)?;
    keyspace::write_i64_le(&mut base, &INFO_PER_LEVEL_TIMESTAMP_KEY, timestamp.i64())?;
    Ok(())
}

pub fn read_last_info_per_level_timestamp(
    host: &mut (impl StorageV1 + KeySpaceLoader),
) -> Result<Timestamp, Error> {
    let base = load_base_keyspace(host)?;
    let timestamp = keyspace::read_i64_le(&base, &INFO_PER_LEVEL_TIMESTAMP_KEY)?;
    Ok(timestamp.into())
}

pub fn read_admin(base: &impl KeySpace) -> Option<ContractKt1Hash> {
    keyspace::read_b58_kt1(base, &ADMIN_KEY)
}

pub fn read_sequencer_governance(host: &mut impl StorageV1) -> Option<ContractKt1Hash> {
    read_b58_kt1(host, &SEQUENCER_GOVERNANCE)
}

pub fn read_kernel_governance(base: &impl KeySpace) -> Option<ContractKt1Hash> {
    keyspace::read_b58_kt1(base, &KERNEL_GOVERNANCE_KEY)
}

pub fn read_kernel_security_governance(base: &impl KeySpace) -> Option<ContractKt1Hash> {
    keyspace::read_b58_kt1(base, &KERNEL_SECURITY_GOVERNANCE_KEY)
}

pub fn read_maximum_allowed_ticks(base: &impl KeySpace) -> Option<u64> {
    keyspace::read_u64_le(base, &MAXIMUM_ALLOWED_TICKS_KEY).ok()
}

pub fn enter_stage_one<Host>(host: &mut Host) -> Result<(), Error>
where
    Host: StorageV1,
{
    Ok(host.store_write(&STAGE_ONE_WITNESS_PATH, b"", 0)?)
}

pub fn leave_stage_one<Host>(host: &mut Host) -> Result<(), Error>
where
    Host: StorageV1,
{
    Ok(host.store_delete_value(&STAGE_ONE_WITNESS_PATH)?)
}

pub fn inside_stage_one<Host>(host: &Host) -> bool
where
    Host: StorageV1,
{
    host.store_has(&STAGE_ONE_WITNESS_PATH)
        .unwrap_or(None)
        .is_some()
}

/// Reads the maximum gas per transaction. If the value cannot found in the storage,
/// we write the kernel default value in the storage. The value becomes accessible
/// from outside the kernel.
pub fn read_or_set_maximum_gas_per_transaction(
    host: &mut impl StorageV1,
) -> anyhow::Result<u64> {
    match read_u64_le(host, &MAXIMUM_GAS_PER_TRANSACTION) {
        Ok(gas_limit) => Ok(gas_limit),
        Err(_) => {
            write_u64_le(host, &MAXIMUM_GAS_PER_TRANSACTION, MAXIMUM_GAS_LIMIT)?;
            Ok(MAXIMUM_GAS_LIMIT)
        }
    }
}

pub fn store_storage_version(
    host: &mut (impl StorageV1 + KeySpaceLoader),
    storage_version: StorageVersion,
) -> Result<(), Error> {
    let storage_version = u64::from(storage_version);
    {
        let mut base = load_base_keyspace(host)?;
        base.set(&STORAGE_VERSION_KEY, storage_version.to_le_bytes())?;
    }
    // Clean up the legacy /evm/ path (outside `/base`) so only
    // /base/storage_version remains.
    let _ = host.store_delete(&LEGACY_STORAGE_VERSION_PATH);
    Ok(())
}

pub fn read_storage_version<Host>(host: &mut Host) -> Result<StorageVersion, Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    // Try the `/base` keyspace first, falling back to the legacy /evm/ path
    // (outside `/base`) only when the new key does not exist yet.
    let from_base = {
        let base = load_base_keyspace(host)?;
        keyspace::read_u64_le(&base, &STORAGE_VERSION_KEY).ok()
    };
    let version_u64 = match from_base {
        Some(version_u64) => version_u64,
        None => {
            let size = std::mem::size_of::<u64>();
            let bytes = host.store_read(&LEGACY_STORAGE_VERSION_PATH, 0, size)?;
            let slice_of_bytes: [u8; 8] =
                bytes[..].try_into().map_err(|_| Error::InvalidConversion)?;
            u64::from_le_bytes(slice_of_bytes)
        }
    };
    let version = FromPrimitive::from_u64(version_u64).ok_or(Error::InvalidConversion)?;
    log!(Debug, "Current storage version: {:?}", version);
    Ok(version)
}

/// Whether the storage version is already set in the `/base` keyspace
/// (resolving to `/base/storage_version`). Used by stage zero to decide
/// whether storage versioning needs initialising.
pub fn is_storage_version_initialised(base: &impl KeySpace) -> bool {
    base.contains(&STORAGE_VERSION_KEY)
}

pub fn read_kernel_version(
    host: &mut (impl StorageV1 + KeySpaceLoader),
) -> Result<String, Error> {
    let base = load_base_keyspace(host)?;
    match base.get(&KERNEL_VERSION_KEY) {
        Some(bytes) => {
            let kernel_version =
                std::str::from_utf8(&bytes).map_err(|_| Error::InvalidConversion)?;
            Ok(kernel_version.to_owned())
        }
        None => Err(RuntimeError::PathNotFound.into()),
    }
}

pub fn store_kernel_version(
    host: &mut (impl StorageV1 + KeySpaceLoader),
    kernel_version: &str,
) -> Result<(), Error> {
    let mut base = load_base_keyspace(host)?;
    base.set(&KERNEL_VERSION_KEY, kernel_version.as_bytes())
        .map_err(Error::from)
}

// DO NOT RENAME: function name is used during benchmark
// Never inlined when the kernel is compiled for benchmarks, to ensure the
// function is visible in the profiling results.
#[cfg_attr(feature = "benchmark", inline(never))]
pub fn store_block_in_progress<Host>(
    host: &mut Host,
    bip: &BlockInProgress,
) -> anyhow::Result<()>
where
    Host: StorageV1,
{
    let path = OwnedPath::from(EVM_BLOCK_IN_PROGRESS);
    let bytes = &bip.rlp_bytes();
    log!(
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
pub fn read_block_in_progress<Host>(
    host: &Host,
) -> anyhow::Result<Option<BlockInProgress>>
where
    Host: StorageV1,
{
    let path = OwnedPath::from(EVM_BLOCK_IN_PROGRESS);
    if let Some(ValueType::Value) = host.store_has(&path)? {
        let bytes = host
            .store_read_all(&path)
            .context("Failed to read current block in progress")?;
        log!(
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

pub fn delete_block_in_progress(host: &mut impl StorageV1) -> anyhow::Result<()> {
    host.store_delete(&EVM_BLOCK_IN_PROGRESS)
        .context("Failed to delete block in progress")
}

pub fn sequencer(host: &impl StorageV1) -> anyhow::Result<Option<PublicKey>> {
    if host.store_has(&SEQUENCER_KEY_PATH)?.is_some() {
        let bytes = host.store_read_all(&SEQUENCER_KEY_PATH)?;
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

pub fn enable_dal(base: &impl KeySpace, is_evm_node: bool) -> bool {
    // When run from the EVM node, the DAL feature is always considered as
    // disabled. The flag is passed as a boolean because it is read through
    // the raw host at bootstrap, before any keyspace exists.
    base.contains(&ENABLE_DAL_KEY) && !is_evm_node
}

pub fn enable_tezos_runtime(base: &impl KeySpace) -> bool {
    base.contains(&ENABLE_TEZOS_RUNTIME_KEY)
}

pub fn read_michelson_runtime_target_sunrise_level(
    host: &impl StorageV1,
) -> Option<U256> {
    read_u256_le(host, &MICHELSON_RUNTIME_TARGET_SUNRISE_LEVEL).ok()
}

pub fn store_michelson_runtime_sunrise_level(
    host: &mut impl StorageV1,
    level: U256,
) -> Result<(), Error> {
    write_u256_le(host, &MICHELSON_RUNTIME_SUNRISE_LEVEL, level).map_err(Error::from)
}

pub fn read_michelson_runtime_sunrise_level(host: &impl StorageV1) -> Option<U256> {
    read_u256_le(host, &MICHELSON_RUNTIME_SUNRISE_LEVEL).ok()
}

pub fn enable_debug_precompiles(host: &impl StorageV1) -> bool {
    Ok(Some(ValueType::Value)) == host.store_has(&ENABLE_DEBUG_PRECOMPILES)
}

pub fn enable_michelson_gas_refund(base: &impl KeySpace) -> bool {
    base.contains(&ENABLE_MICHELSON_GAS_REFUND_KEY)
}

/// Returns true if legacy DAL slot import signals are disabled.
/// When disabled, the kernel ignores `DalSlotImportSignals` external messages
/// and instead relies on `DalAttestedSlots` internal messages.
pub fn is_legacy_dal_signals_disabled(base: &impl KeySpace) -> bool {
    base.contains(&DISABLE_LEGACY_DAL_SIGNALS_KEY)
}

pub fn dal_slots(base: &impl KeySpace) -> Option<Vec<u8>> {
    base.get(&DAL_SLOTS_KEY)
}

/// Read the whitelist of authorized DAL publishers from storage.
/// Returns an empty vector if no whitelist is configured.
/// The whitelist is stored as an RLP-encoded list of binary-encoded public key hashes.
///
/// NOTE: An empty whitelist means NO publishers are authorized
/// The kernel will reject all DAL slots if the whitelist is empty.
pub fn read_dal_publishers_whitelist(
    base: &impl KeySpace,
) -> anyhow::Result<Vec<tezos_smart_rollup_encoding::public_key_hash::PublicKeyHash>> {
    if let Some(rlp_bytes) = base.get(&DAL_PUBLISHERS_WHITELIST_KEY) {
        let rlp = rlp::Rlp::new(&rlp_bytes);

        let mut whitelist = Vec::new();
        for item in rlp.iter() {
            let pkh_bytes = item.as_val::<Vec<u8>>()?;
            let (remaining, pkh) = PublicKeyHash::nom_read(&pkh_bytes).map_err(|e| {
                anyhow::anyhow!("Failed to decode public key hash: {:?}", e)
            })?;
            if !remaining.is_empty() {
                return Err(anyhow::anyhow!(
                    "Unexpected trailing bytes after public key hash"
                ));
            }
            whitelist.push(pkh);
        }
        Ok(whitelist)
    } else {
        Ok(Vec::new())
    }
}

pub fn remove_sequencer(host: &mut impl StorageV1) -> anyhow::Result<()> {
    host.store_delete(&SEQUENCER_KEY_PATH).map_err(Into::into)
}

pub fn store_sequencer(
    host: &mut impl StorageV1,
    public_key: &PublicKey,
) -> anyhow::Result<()> {
    let pk_b58 = PublicKey::to_b58check(public_key);
    let bytes = String::as_bytes(&pk_b58);
    host.store_write_all(&SEQUENCER_KEY_PATH, bytes)
        .map_err(Into::into)
}

pub fn clear_events<Host>(host: &mut Host) -> anyhow::Result<()>
where
    Host: StorageV1 + KeySpaceLoader,
{
    // Load `/base` once: the `keep_rollup_events` flag and the events index
    // both live under it, so a single handle covers the whole operation.
    let mut base = load_base_keyspace(host)?;
    if base.contains(&KEEP_EVENTS_KEY) {
        // One-shot flag: keep this run's events and consume the flag so they
        // are cleared on the next call.
        base.delete(&KEEP_EVENTS_KEY);
        Ok(())
    } else {
        let index = KeyspaceIndexableStorage::new(EVENTS_KEY);
        index.clear(&mut base).map_err(Into::into)
    }
}

pub fn store_event(
    host: &mut (impl StorageV1 + KeySpaceLoader),
    event: &Event,
) -> anyhow::Result<()> {
    let mut base = load_base_keyspace(host)?;
    let index = KeyspaceIndexableStorage::new(EVENTS_KEY);
    index
        .push_value(&mut base, &event.rlp_bytes())
        .map_err(Into::into)
}

pub fn delayed_inbox_timeout<Host>(host: &mut Host) -> anyhow::Result<u64>
where
    Host: StorageV1 + KeySpaceLoader,
{
    // The default timeout is 12 hours
    let default_timeout = 43200;
    let base = load_base_keyspace(host)?;
    let timeout = keyspace::read_u64_le_default(
        &base,
        &DELAYED_INBOX_TIMEOUT_KEY,
        default_timeout,
    )?;
    if timeout == default_timeout {
        log!(
            Debug,
            "Using default delayed inbox timeout of {} seconds ({} hours)",
            default_timeout,
            default_timeout / 3600
        );
    } else {
        log!(
            Debug,
            "Using delayed inbox timeout of {} seconds ({} hours)",
            timeout,
            timeout / 3600
        );
    }
    Ok(timeout)
}

pub fn delayed_inbox_min_levels<Host>(host: &mut Host) -> anyhow::Result<u32>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let default_min_levels = 720;
    let base = load_base_keyspace(host)?;
    let min_levels = keyspace::read_u32_le_default(
        &base,
        &DELAYED_INBOX_MIN_LEVELS_KEY,
        default_min_levels,
    )?;
    if min_levels == default_min_levels {
        log!(
            Debug,
            "Using default delayed inbox minimum levels: {}",
            default_min_levels
        );
    } else {
        log!(Debug, "Using delayed inbox minimum levels: {}", min_levels);
    }
    Ok(min_levels)
}

pub fn read_tracer_input<Host>(host: &mut Host) -> anyhow::Result<Option<TracerInput>>
where
    Host: StorageV1,
{
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
        log!(Debug, "Tracer input found: {:?}", tracer);

        Ok(Some(tracer))
    } else {
        Ok(None)
    }
}

pub fn is_enable_fa_bridge(base: &impl KeySpace) -> bool {
    base.contains(&ENABLE_FA_BRIDGE_KEY)
}

pub fn max_blueprint_lookahead_in_seconds(base: &impl KeySpace) -> anyhow::Result<i64> {
    Ok(keyspace::read_i64_le(
        base,
        &MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS_KEY,
    )?)
}

/// Smart Contract of the delayed bridge
///
/// This smart contract is used to submit transactions to the rollup
/// when in sequencer mode
pub fn read_delayed_transaction_bridge(base: &impl KeySpace) -> Option<ContractKt1Hash> {
    keyspace::read_b58_kt1(base, &DELAYED_BRIDGE_KEY)
}

#[cfg(test)]
mod tests {
    use tezos_data_encoding::enc::BinWriter;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_encoding::public_key_hash::PublicKeyHash;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_keyspace::KeySpace;
    use tezos_smart_rollup_keyspace::KeySpaceLoader;
    use tezosx_journal::{CracId, TezosXJournal};

    use crate::storage::DAL_SLOTS_KEY;
    use crate::storage::ENABLE_FA_BRIDGE;
    use crate::storage::{load_base_keyspace, ENABLE_DAL_KEY};

    fn tweak_dal_activation(
        host: &mut (impl StorageV1 + KeySpaceLoader),
        activate_dal: bool,
    ) -> anyhow::Result<()> {
        // The reader (`enable_dal`) tests key presence, so an empty value enables
        // the flag and deleting it disables it.
        let mut base = load_base_keyspace(host)?;
        if activate_dal {
            base.set(&ENABLE_DAL_KEY, b"")?;
        } else {
            base.delete(&ENABLE_DAL_KEY);
        }
        Ok(())
    }

    fn store_dal_slots(
        host: &mut (impl StorageV1 + KeySpaceLoader),
        slots: &[u8],
    ) -> anyhow::Result<()> {
        let mut base = load_base_keyspace(host)?;
        base.set(&DAL_SLOTS_KEY, slots)?;
        Ok(())
    }

    // RLP-encode a list of public key hashes the way the DAL publishers
    // whitelist is stored: an RLP list whose items are the binary-encoded PKHs.
    fn encode_dal_publishers_whitelist(pkhs: &[PublicKeyHash]) -> Vec<u8> {
        let mut stream = rlp::RlpStream::new_list(pkhs.len());
        for pkh in pkhs {
            let mut pkh_bytes = Vec::new();
            pkh.bin_write(&mut pkh_bytes).unwrap();
            stream.append(&pkh_bytes);
        }
        stream.out().to_vec()
    }

    // Byte-compat check for the values migrated to the `/base` keyspace:
    // each value written through the raw host at its historical absolute
    // path must be read back through the keyspace reader, proving the
    // resolved durable paths never moved.
    #[test]
    fn base_keyspace_readers_resolve_to_absolute_paths() {
        let mut host = MockKernelHost::default();
        let kt1 = tezos_crypto_rs::hash::ContractKt1Hash::from_base58_check(
            "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT",
        )
        .unwrap();
        let kt1_bytes = kt1.to_base58_check().into_bytes();

        for path in [
            b"/base/admin".as_slice(),
            b"/base/kernel_governance",
            b"/base/kernel_security_governance",
            b"/base/delayed_bridge",
        ] {
            host.store_write_all(&RefPath::assert_from(path), &kt1_bytes)
                .unwrap();
        }
        host.store_write_all(
            &RefPath::assert_from(b"/base/maximum_allowed_ticks"),
            &42u64.to_le_bytes(),
        )
        .unwrap();
        host.store_write_all(
            &RefPath::assert_from(b"/base/max_blueprint_lookahead_in_seconds"),
            &300i64.to_le_bytes(),
        )
        .unwrap();
        host.store_write_all(&ENABLE_FA_BRIDGE, &[1u8]).unwrap();

        // DAL readers go through `/base`; seed their durable paths raw here
        // so the reader side is exercised against a known on-chain layout.
        host.store_write_all(
            &RefPath::assert_from(b"/base/feature_flags/enable_dal"),
            &[1u8],
        )
        .unwrap();
        host.store_write_all(
            &RefPath::assert_from(b"/base/feature_flags/disable_legacy_dal_signals"),
            &[1u8],
        )
        .unwrap();
        host.store_write_all(&RefPath::assert_from(b"/base/dal_slots"), &[3u8, 7u8])
            .unwrap();
        let pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        host.store_write_all(
            &RefPath::assert_from(b"/base/dal_publishers_whitelist"),
            &encode_dal_publishers_whitelist(&[pkh.clone()]),
        )
        .unwrap();

        let base = super::load_base_keyspace(&mut host).unwrap();
        assert_eq!(super::read_admin(&base), Some(kt1.clone()));
        assert_eq!(super::read_kernel_governance(&base), Some(kt1.clone()));
        assert_eq!(
            super::read_kernel_security_governance(&base),
            Some(kt1.clone())
        );
        assert_eq!(super::read_delayed_transaction_bridge(&base), Some(kt1));
        assert_eq!(super::read_maximum_allowed_ticks(&base), Some(42));
        assert_eq!(
            super::max_blueprint_lookahead_in_seconds(&base).unwrap(),
            300
        );
        assert!(super::is_enable_fa_bridge(&base));
        assert!(super::enable_dal(&base, false));
        // EVM node always sees the DAL feature as disabled.
        assert!(!super::enable_dal(&base, true));
        assert!(super::is_legacy_dal_signals_disabled(&base));
        assert_eq!(super::dal_slots(&base), Some(vec![3u8, 7u8]));
        assert_eq!(
            super::read_dal_publishers_whitelist(&base).unwrap(),
            vec![pkh]
        );
    }

    // Sibling of `base_keyspace_readers_resolve_to_absolute_paths`: on a fresh
    // `/base` keyspace every migrated reader must report its absent value
    // (`None` / error / `false` / empty whitelist). A reader resolving to a
    // wrong-but-present key, or defaulting to a "present" value when the key is
    // missing, would slip past the positive test but is caught here.
    #[test]
    fn base_keyspace_readers_on_empty_base_return_absent() {
        let mut host = MockKernelHost::default();
        let base = super::load_base_keyspace(&mut host).unwrap();

        assert_eq!(super::read_admin(&base), None);
        assert_eq!(super::read_kernel_governance(&base), None);
        assert_eq!(super::read_kernel_security_governance(&base), None);
        assert_eq!(super::read_delayed_transaction_bridge(&base), None);
        assert_eq!(super::read_maximum_allowed_ticks(&base), None);
        assert!(super::max_blueprint_lookahead_in_seconds(&base).is_err());
        assert!(!super::is_enable_fa_bridge(&base));
        assert!(!super::enable_dal(&base, false));
        assert!(!super::is_legacy_dal_signals_disabled(&base));
        assert_eq!(super::dal_slots(&base), None);
        // An absent whitelist decodes to the empty list, which the kernel
        // treats as "reject all publishers".
        assert!(super::read_dal_publishers_whitelist(&base)
            .unwrap()
            .is_empty());
    }

    // The steady-state scalars now go through the `/base` keyspace on both
    // sides. This round-trip proves keyspace writer and reader agree and that
    // the value still lands at the historical absolute path.
    #[test]
    fn base_keyspace_scalar_writers_resolve_to_absolute_paths() {
        use tezos_smart_rollup_encoding::timestamp::Timestamp;

        let mut host = MockKernelHost::default();

        super::store_l1_level(&mut host, 99).unwrap();
        assert_eq!(super::read_l1_level(&mut host).unwrap(), 99);
        // The keyspace writer must land at the historical absolute path.
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/base/l1_level"))
                .unwrap(),
            99u32.to_le_bytes()
        );

        super::store_last_info_per_level_timestamp(&mut host, Timestamp::from(123))
            .unwrap();
        assert_eq!(
            super::read_last_info_per_level_timestamp(&mut host).unwrap(),
            Timestamp::from(123)
        );
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/base/info_per_level/timestamp"))
                .unwrap(),
            123i64.to_le_bytes()
        );
    }

    // The DAL writers (migration-only) now go through the `/base` keyspace,
    // matching their already-keyspace readers. This proves the keyspace
    // writer lands at the historical absolute path and round-trips through
    // the reader, and that disabling deletes the flag.
    #[test]
    fn base_keyspace_dal_writers_resolve_to_absolute_paths() {
        let mut host = MockKernelHost::default();

        let enable_dal_path = RefPath::assert_from(b"/base/feature_flags/enable_dal");
        let dal_slots_path = RefPath::assert_from(b"/base/dal_slots");

        tweak_dal_activation(&mut host, true).unwrap();
        assert!(host.store_read_all(&enable_dal_path).is_ok());
        {
            let base = super::load_base_keyspace(&mut host).unwrap();
            assert!(super::enable_dal(&base, false));
        }

        tweak_dal_activation(&mut host, false).unwrap();
        assert!(host.store_read_all(&enable_dal_path).is_err());
        {
            let base = super::load_base_keyspace(&mut host).unwrap();
            assert!(!super::enable_dal(&base, false));
        }

        store_dal_slots(&mut host, &[0, 1, 2]).unwrap();
        assert_eq!(host.store_read_all(&dal_slots_path).unwrap(), vec![0, 1, 2]);
        let base = super::load_base_keyspace(&mut host).unwrap();
        assert_eq!(super::dal_slots(&base), Some(vec![0, 1, 2]));
    }

    // The read-only config readers (set by the installer/node, read by the
    // kernel) now go through the `/base` keyspace. Each value seeded at its
    // historical absolute path must read back through the migrated reader,
    // proving the resolved durable paths never moved.
    #[test]
    fn base_keyspace_config_readers_resolve_to_absolute_paths() {
        let mut host = MockKernelHost::default();

        host.store_write_all(
            &RefPath::assert_from(b"/base/delayed_inbox_timeout"),
            &3600u64.to_le_bytes(),
        )
        .unwrap();
        host.store_write_all(
            &RefPath::assert_from(b"/base/delayed_inbox_min_levels"),
            &120u32.to_le_bytes(),
        )
        .unwrap();
        host.store_write_all(
            &RefPath::assert_from(b"/base/feature_flags/enable_tezos_runtime"),
            &[1u8],
        )
        .unwrap();
        host.store_write_all(
            &RefPath::assert_from(b"/base/feature_flags/enable_michelson_gas_refund"),
            &[1u8],
        )
        .unwrap();

        assert_eq!(super::delayed_inbox_timeout(&mut host).unwrap(), 3600);
        assert_eq!(super::delayed_inbox_min_levels(&mut host).unwrap(), 120);
        {
            let base = super::load_base_keyspace(&mut host).unwrap();
            assert!(super::enable_tezos_runtime(&base));
            assert!(super::enable_michelson_gas_refund(&base));
        }
    }

    // On a fresh `/base`, the delayed-inbox scalars fall back to their
    // historical defaults and the flags read `false` — a reader resolving to a
    // wrong key, or defaulting to a "present" value, would be caught here.
    #[test]
    fn base_keyspace_config_readers_on_empty_base_use_defaults() {
        let mut host = MockKernelHost::default();

        assert_eq!(super::delayed_inbox_timeout(&mut host).unwrap(), 43200);
        assert_eq!(super::delayed_inbox_min_levels(&mut host).unwrap(), 720);
        let base = super::load_base_keyspace(&mut host).unwrap();
        assert!(!super::enable_tezos_runtime(&base));
        assert!(!super::enable_michelson_gas_refund(&base));
    }

    // `clear_events` reads the one-shot `keep_rollup_events` flag through the
    // `/base` keyspace and, when set, consumes it (keeping this run's events).
    #[test]
    fn clear_events_consumes_keep_flag_through_keyspace() {
        let mut host = MockKernelHost::default();
        let keep_path = RefPath::assert_from(b"/base/keep_rollup_events");

        // Flag set at its historical absolute path: clear_events keeps the
        // events and consumes the flag.
        host.store_write_all(&keep_path, &[]).unwrap();
        super::clear_events(&mut host).unwrap();
        assert!(host.store_read_all(&keep_path).is_err());

        // Flag absent: clear_events takes the index-clearing branch and
        // succeeds (no events to clear on a fresh base).
        super::clear_events(&mut host).unwrap();
    }

    // Storage and kernel version now go through the `/base` keyspace on both
    // sides. This round-trip proves keyspace writer and reader agree and that
    // each value still lands at its historical absolute path.
    #[test]
    fn base_keyspace_version_writers_resolve_to_absolute_paths() {
        let mut host = MockKernelHost::default();

        // On a fresh base, versioning is not yet initialised.
        {
            let base = super::load_base_keyspace(&mut host).unwrap();
            assert!(!super::is_storage_version_initialised(&base));
        }

        super::store_storage_version(&mut host, super::STORAGE_VERSION).unwrap();
        assert_eq!(
            super::read_storage_version(&mut host).unwrap(),
            super::STORAGE_VERSION
        );
        // The keyspace writer must land at the historical absolute path.
        assert_eq!(
            host.store_read_all(&super::STORAGE_VERSION_PATH).unwrap(),
            u64::from(super::STORAGE_VERSION).to_le_bytes()
        );
        {
            let base = super::load_base_keyspace(&mut host).unwrap();
            assert!(super::is_storage_version_initialised(&base));
        }

        super::store_kernel_version(&mut host, "kernel-test").unwrap();
        assert_eq!(
            super::read_kernel_version(&mut host).unwrap(),
            "kernel-test"
        );
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/base/kernel_version"))
                .unwrap(),
            b"kernel-test"
        );
    }

    // The simulation-output writers now go through the `/base` keyspace.
    // Each writer must land its bytes at the historical absolute path with
    // the exact same encoding as before, so the EVM-node-side decoders that
    // read these values across the kernel↔node ABI are unaffected.
    #[test]
    fn base_keyspace_simulation_writers_resolve_to_absolute_paths() {
        use crate::simulation::SimulationResult;
        use tezos_ethereum::rlp_helpers::VersionedEncoding;

        let mut host = MockKernelHost::default();

        // The simulation result is RLP-encoded with a leading version byte
        // (`VersionedEncoding`). The keyspace writer must store exactly those
        // bytes at `/base/evm_simulation_result`.
        let result: SimulationResult<u64, String> = SimulationResult::Ok(42);
        let expected = result.to_bytes();
        super::store_simulation_result(&mut host, result).unwrap();
        assert_eq!(
            host.store_read_all(&super::SIMULATION_RESULT).unwrap(),
            expected
        );

        // The HTTP traces are stored as an RLP list. An empty capture is the
        // common case (no cross-runtime HTTP call); it must resolve to the
        // empty-list encoding at `/base/simulation_http_traces`.
        super::store_simulation_http_traces(&mut host, &[]).unwrap();
        assert_eq!(
            host.store_read_all(&super::SIMULATION_HTTP_TRACES).unwrap(),
            rlp::RlpStream::new_list(0).out().to_vec()
        );
    }

    // `read_storage_version` keeps reading the legacy /evm/ path (outside
    // `/base`) when the `/base` key is absent, and `store_storage_version`
    // cleans the legacy path up so only the `/base` value remains.
    #[test]
    fn storage_version_legacy_fallback_and_cleanup() {
        let mut host = MockKernelHost::default();
        let version_bytes = u64::from(super::STORAGE_VERSION).to_le_bytes();

        // Only the legacy /evm/ path is set: the reader falls back to it.
        host.store_write_all(&super::LEGACY_STORAGE_VERSION_PATH, &version_bytes)
            .unwrap();
        assert_eq!(
            super::read_storage_version(&mut host).unwrap(),
            super::STORAGE_VERSION
        );

        // Writing through the keyspace removes the legacy path.
        super::store_storage_version(&mut host, super::STORAGE_VERSION).unwrap();
        assert!(host
            .store_read_all(&super::LEGACY_STORAGE_VERSION_PATH)
            .is_err());
        assert_eq!(
            host.store_read_all(&super::STORAGE_VERSION_PATH).unwrap(),
            version_bytes
        );
    }

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

    #[test]
    fn http_trace_flag_default_off() {
        let host = MockKernelHost::default();
        assert!(!super::is_http_trace_enabled(&host));
    }

    #[test]
    fn http_trace_flag_on_once_written() {
        let mut host = MockKernelHost::default();
        host.store_write_all(&super::HTTP_TRACE_ENABLED, &[1u8])
            .unwrap();
        assert!(super::is_http_trace_enabled(&host));
    }

    #[test]
    fn maybe_store_http_traces_is_noop_when_disabled() {
        let mut host = MockKernelHost::default();
        let tx_hash = [7u8; 32];
        let journal = TezosXJournal::new(
            CracId::new(0, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        super::maybe_store_http_traces_for_tx(&mut host, false, &tx_hash, &journal);
        let path = super::http_traces_path(&tx_hash).unwrap();
        assert!(matches!(host.store_has(&path), Ok(None)));
    }

    #[test]
    fn maybe_store_http_traces_is_noop_when_journal_empty() {
        // When the flag is on but the journal captured no HTTP exchange,
        // we write nothing — a missing key is equivalent to the empty list
        // on the reader side, and skipping avoids writing one RLP-encoded
        // empty list per non-CRAC transaction on every replayed block.
        let mut host = MockKernelHost::default();
        let tx_hash = [42u8; 32];
        let journal = TezosXJournal::new(
            CracId::new(0, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        super::maybe_store_http_traces_for_tx(&mut host, true, &tx_hash, &journal);
        let path = super::http_traces_path(&tx_hash).unwrap();
        assert!(matches!(host.store_has(&path), Ok(None)));
    }

    #[test]
    fn maybe_store_http_traces_persists_rlp_when_journal_has_trace() {
        // Happy path: enabled, journal captured one HTTP exchange.
        // [maybe_store_http_traces_for_tx] writes an RLP-encoded
        // one-element list under [http_traces_path(tx_hash)], which is what
        // the EVM node expects to decode through [Simulation.decode_http_traces].
        let mut host = MockKernelHost::default();

        let mut journal = TezosXJournal::new(
            CracId::new(0, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        journal.set_http_trace_enabled(true);
        let request = http::Request::builder()
            .method("GET")
            .uri("http://tezos/KT1abc/default")
            .header("X-Tezos-Sender", "KT1sender")
            .body(b"hello".to_vec())
            .unwrap();
        journal.record_request(&request);
        let response = http::Response::builder()
            .status(200)
            .header("X-Tezos-Gas-Consumed", "42")
            .body(b"world".to_vec())
            .unwrap();
        journal.record_response(&response);
        assert_eq!(journal.http_traces().len(), 1);

        let tx_hash = [0xABu8; 32];
        super::maybe_store_http_traces_for_tx(&mut host, true, &tx_hash, &journal);

        let path = super::http_traces_path(&tx_hash).unwrap();
        let bytes = host.store_read_all(&path).unwrap();
        // Non-empty: the empty-list RLP encoding is a single 0xc0 byte.
        assert!(
            bytes.len() > 1,
            "expected a non-trivial RLP payload, got {bytes:?}"
        );

        // Decoding the payload as an RLP list of [HttpTrace] yields the one
        // exchange we recorded.
        let rlp = rlp::Rlp::new(&bytes);
        assert!(rlp.is_list());
        let decoded: Vec<tezosx_journal::HttpTrace> = rlp.as_list().unwrap();
        assert_eq!(decoded.len(), 1);
        assert_eq!(decoded[0].method, "GET");
        assert_eq!(decoded[0].url, "http://tezos/KT1abc/default");
        assert_eq!(decoded[0].response_status, 200);
        assert_eq!(decoded[0].response_body, b"world".to_vec());
    }

    #[test]
    fn maybe_store_http_traces_preserves_order_on_multi_trace_journal() {
        // A realistic CRAC-heavy transaction performs several sequential
        // HTTP calls in the same journal. Exercise the
        // [RlpStream::new_list(traces.len())] path with two top-level
        // exchanges and confirm the persisted list round-trips through RLP
        // while preserving the order in which the journal recorded them
        // (first call first).
        let mut host = MockKernelHost::default();

        let mut journal = TezosXJournal::new(
            CracId::new(0, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        journal.set_http_trace_enabled(true);

        // First exchange: GET /first.
        let req1 = http::Request::builder()
            .method("GET")
            .uri("http://tezos/KT1abc/first")
            .body(b"".to_vec())
            .unwrap();
        journal.record_request(&req1);
        let resp1 = http::Response::builder()
            .status(200)
            .body(b"r1".to_vec())
            .unwrap();
        journal.record_response(&resp1);

        // Second exchange: POST /second.
        let req2 = http::Request::builder()
            .method("POST")
            .uri("http://tezos/KT1abc/second")
            .body(b"payload".to_vec())
            .unwrap();
        journal.record_request(&req2);
        let resp2 = http::Response::builder()
            .status(204)
            .body(b"r2".to_vec())
            .unwrap();
        journal.record_response(&resp2);

        assert_eq!(journal.http_traces().len(), 2);

        let tx_hash = [0xCDu8; 32];
        super::maybe_store_http_traces_for_tx(&mut host, true, &tx_hash, &journal);

        let path = super::http_traces_path(&tx_hash).unwrap();
        let bytes = host.store_read_all(&path).unwrap();
        let rlp = rlp::Rlp::new(&bytes);
        let decoded: Vec<tezosx_journal::HttpTrace> = rlp.as_list().unwrap();

        assert_eq!(decoded.len(), 2);
        assert_eq!(decoded[0].method, "GET");
        assert_eq!(decoded[0].url, "http://tezos/KT1abc/first");
        assert_eq!(decoded[0].response_status, 200);
        assert_eq!(decoded[0].response_body, b"r1".to_vec());
        assert_eq!(decoded[1].method, "POST");
        assert_eq!(decoded[1].url, "http://tezos/KT1abc/second");
        assert_eq!(decoded[1].response_status, 204);
        assert_eq!(decoded[1].request_body, b"payload".to_vec());
        assert_eq!(decoded[1].response_body, b"r2".to_vec());
    }
}
