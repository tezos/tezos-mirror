// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::blueprint::Blueprint;
use crate::chains::{ExperimentalFeatures, TezosXChainConfig, TezosXTransaction};
use crate::configuration::{Configuration, ConfigurationMode};
use crate::error::{Error, StorageError};
use crate::l2block::L2Block;
use crate::sequencer_blueprint::{
    BlueprintWithDelayedHashes, UnsignedSequencerBlueprint,
};
use crate::storage::read_last_info_per_level_timestamp;
use crate::transaction::TransactionContent;
use crate::{delayed_inbox, DelayedInbox};
use primitive_types::{H256, U256};
use rlp::{Decodable, DecoderError, Encodable};
use std::collections::BTreeSet;
use std::fmt::Debug;
use tezos_ethereum::block::EthBlock;
use tezos_ethereum::eth_gen::OwnedHash;
use tezos_ethereum::rlp_helpers::{
    self, append_timestamp, append_u256_le, decode_field, decode_field_u256_le,
    decode_timestamp,
};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup::types::Timestamp;
use tezos_smart_rollup_core::MAX_INPUT_MESSAGE_SIZE;
use tezos_smart_rollup_host::path::*;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{Key, KeySpace, KeySpaceLoader};
use tezos_storage::{keyspace, read_rlp, store_rlp};
use tezos_tezlink::block::TezBlock;
use tezos_tezlink::protocol::{Protocol, INITIAL_PROTOCOL};

// Absolute form kept for the `migration.rs` blueprint moves (`store_move`,
// a path-subtree op the flat keyspace API cannot express) and for tests; the
// steady-state read/write helpers go through the `/base` keyspace at the
// relative keys built below, resolving to these same paths.
pub const EVM_BLUEPRINTS: RefPath = RefPath::assert_from(b"/base/blueprints");

// Relative key (under the `/base` keyspace) of the current blueprint
// generation. This generation number is used to decide if a blueprint is
// outdated or not. Instead of deleting all blueprints when we want to
// invalidate them, we just increment the current generation number stored
// so that when reading a blueprint we can check if its generation matches
// the current one.
const BLUEPRINT_CURRENT_GENERATION_KEY: Key = Key::from_static(b"/blueprints/generation");

// The current block header is read and written through the `/base` keyspace at
// this relative key. The absolute form is kept only for tests asserting the key
// resolves to the historical durable path.
#[cfg(test)]
const EVM_CURRENT_BLOCK_HEADER: RefPath =
    RefPath::assert_from(b"/base/current_block_header");
const EVM_CURRENT_BLOCK_HEADER_KEY: Key = Key::from_static(b"/current_block_header");

const TEZ_CURRENT_BLOCK_HEADER: RefPath =
    RefPath::assert_from(b"/tez/world_state/current_chain_header");

/// The store representation of a blueprint.
/// It's designed to support storing sequencer blueprints,
/// which can be chunked, and blueprints constructed from
/// inbox messages. Note that the latter are only to be
/// used when the kernel isn't running with a sequencer.
#[derive(PartialEq, Debug)]
enum StoreBlueprint {
    SequencerChunk(Vec<u8>),
    InboxBlueprint(Blueprint),
}

const SEQUENCER_CHUNK_TAG: u8 = 0;
const INBOX_BLUEPRINT_TAG: u8 = 1;

impl Encodable for StoreBlueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        match &self {
            StoreBlueprint::SequencerChunk(chunk) => {
                stream.append(&SEQUENCER_CHUNK_TAG);
                stream.append(chunk);
            }
            StoreBlueprint::InboxBlueprint(blueprint) => {
                stream.append(&INBOX_BLUEPRINT_TAG);
                stream.append(blueprint);
            }
        }
    }
}

impl Decodable for StoreBlueprint {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let tag: u8 = decoder.at(0)?.as_val()?;
        let rest = decoder.at(1)?;
        match tag {
            SEQUENCER_CHUNK_TAG => {
                let chunk: Vec<u8> = rest.as_val()?;
                Ok(Self::SequencerChunk(chunk))
            }
            INBOX_BLUEPRINT_TAG => {
                let blueprint = rlp_helpers::decode_field(&rest, "blueprint")?;
                Ok(Self::InboxBlueprint(blueprint))
            }
            _ => Err(DecoderError::Custom("Unknown store blueprint tag.")),
        }
    }
}

// Part of the block header which is generic information because it is
// about the blueprint from which the block was build. This part is
// useful to validate the next blueprint.
#[derive(PartialEq, Debug, Clone)]
pub struct BlueprintHeader {
    pub number: U256,
    pub timestamp: Timestamp,
}

// Part of the block header which is specific of the EVM chain. All
// fields are needed to build the next block. The hash is also needed
// to validate the next blueprint (which commits on this hash).
#[derive(PartialEq, Debug, Clone)]
pub struct EVMBlockHeader {
    pub hash: H256,
    pub receipts_root: OwnedHash,
    pub transactions_root: OwnedHash,
}

#[derive(PartialEq, Debug, Clone)]
pub struct BlockHeader<H> {
    pub blueprint_header: BlueprintHeader,
    pub chain_header: H,
}
// Part of the block header which is specific of the Michelson chain. All
// fields are needed to build the next block
#[derive(PartialEq, Debug, Clone)]
pub struct TezBlockHeader {
    pub hash: H256,
    pub next_protocol: Protocol,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ChainHeader {
    Tez(TezBlockHeader),
    Eth(EVMBlockHeader),
}

impl From<EthBlock> for BlockHeader<ChainHeader> {
    fn from(block: EthBlock) -> Self {
        Self {
            blueprint_header: BlueprintHeader {
                number: block.number,
                timestamp: block.timestamp,
            },
            chain_header: ChainHeader::Eth(EVMBlockHeader {
                hash: block.hash,
                receipts_root: block.receipts_root,
                transactions_root: block.transactions_root,
            }),
        }
    }
}

impl From<TezBlock> for BlockHeader<ChainHeader> {
    fn from(block: TezBlock) -> Self {
        Self {
            blueprint_header: BlueprintHeader {
                number: block.number.into(),
                timestamp: block.timestamp,
            },
            chain_header: ChainHeader::Tez(TezBlockHeader {
                hash: H256(*block.hash),
                next_protocol: block.next_protocol,
            }),
        }
    }
}

impl From<L2Block> for BlockHeader<ChainHeader> {
    fn from(value: L2Block) -> Self {
        match value {
            L2Block::Etherlink(block) => (*block).into(),
            L2Block::Tezlink(block) => block.into(),
        }
    }
}

// Absolute path of a blueprint, kept for `migration.rs` (`store_move`) and
// tests. Steady-state access uses the relative `Key` builders below.
pub fn blueprint_path(number: U256) -> Result<OwnedPath, StorageError> {
    let number_as_path: Vec<u8> = format!("/{number}").into();
    // The key being an integer value, it will always be valid as a path,
    // `assert_from` cannot fail.
    let number_subkey = RefPath::assert_from(&number_as_path);
    concat(&EVM_BLUEPRINTS, &number_subkey).map_err(StorageError::from)
}

// Keyspace-relative keys for the blueprint sub-paths. They emit keys relative
// to the `/base` prefix (`/blueprints/<n>/...`) that resolve, once
// concatenated with the prefix, to the exact same durable paths as the
// absolute `concat`-built ones above.
fn blueprint_chunk_key(number: U256, chunk_index: u16) -> Result<Key, StorageError> {
    Key::try_from(format!("/blueprints/{number}/{chunk_index}"))
        .map_err(StorageError::from)
}

fn blueprint_nb_chunks_key(number: U256) -> Result<Key, StorageError> {
    Key::try_from(format!("/blueprints/{number}/nb_chunks")).map_err(StorageError::from)
}

fn blueprint_generation_key(number: U256) -> Result<Key, StorageError> {
    Key::try_from(format!("/blueprints/{number}/generation")).map_err(StorageError::from)
}

// 32-byte little-endian `U256` read/write through a keyspace handle, mirroring
// the byte layout of the absolute-path helpers. Kept local to the kernel crate
// since the storage crate has no `primitive_types` dependency.
fn write_u256_le(base: &mut impl KeySpace, key: &Key, value: U256) -> Result<(), Error> {
    let mut buffer = [0u8; 32];
    value.to_little_endian(&mut buffer);
    base.set(key, buffer).map_err(Error::from)
}

// Reads a 32-byte little-endian `U256`, falling back to `default` when the key
// is absent or not a value — matching the `PathNotFound | StoreNotAValue` arms
// of the previous absolute-path readers.
fn read_u256_le_or_default(base: &impl KeySpace, key: &Key, default: U256) -> U256 {
    match base.get_prefix_exact::<32>(key) {
        Some(buffer) => U256::from_little_endian(&buffer),
        None => default,
    }
}

// The blueprint accessors below route `/base/blueprints` through the `/base`
// keyspace, each loading the handle transiently.

fn read_current_generation_or_default<Host>(
    host: &mut Host,
    default: U256,
) -> Result<U256, Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let base = crate::storage::load_base_keyspace(host)?;
    Ok(read_u256_le_or_default(
        &base,
        &BLUEPRINT_CURRENT_GENERATION_KEY,
        default,
    ))
}

fn store_current_generation<Host>(host: &mut Host, generation: U256) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let mut base = crate::storage::load_base_keyspace(host)?;
    write_u256_le(&mut base, &BLUEPRINT_CURRENT_GENERATION_KEY, generation)
}

fn increment_current_generation<Host>(host: &mut Host) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let current_generation = read_current_generation_or_default(host, U256::zero())?;
    let new_generation =
        current_generation
            .checked_add(U256::one())
            .ok_or(Error::Overflow(String::from(
                "blueprint current generation",
            )))?;
    store_current_generation(host, new_generation)?;
    Ok(())
}

fn read_blueprint_generation_or_default<Host>(
    host: &mut Host,
    number: U256,
    default: U256,
) -> Result<U256, Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let key = blueprint_generation_key(number)?;
    let base = crate::storage::load_base_keyspace(host)?;
    Ok(read_u256_le_or_default(&base, &key, default))
}

fn store_blueprint_generation<Host>(
    host: &mut Host,
    number: U256,
    generation: U256,
) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let key = blueprint_generation_key(number)?;
    let mut base = crate::storage::load_base_keyspace(host)?;
    write_u256_le(&mut base, &key, generation)
}

fn read_blueprint_nb_chunks<Host>(host: &mut Host, number: U256) -> Result<u16, Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let key = blueprint_nb_chunks_key(number)?;
    let base = crate::storage::load_base_keyspace(host)?;
    keyspace::read_u16_le(&base, &key).map_err(Error::from)
}

fn store_blueprint_nb_chunks<Host>(
    host: &mut Host,
    number: U256,
    nb_chunks: u16,
) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let key = blueprint_nb_chunks_key(number)?;
    let mut base = crate::storage::load_base_keyspace(host)?;
    keyspace::write_u16_le(&mut base, &key, nb_chunks).map_err(Error::from)
}

fn store_blueprint_chunk<Host>(
    host: &mut Host,
    number: U256,
    chunk_index: u16,
    chunk: &StoreBlueprint,
) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let key = blueprint_chunk_key(number, chunk_index)?;
    let mut base = crate::storage::load_base_keyspace(host)?;
    keyspace::store_rlp(chunk, &mut base, &key).map_err(Error::from)
}

pub fn store_sequencer_blueprint<Host>(
    host: &mut Host,
    blueprint: UnsignedSequencerBlueprint,
) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    store_blueprint_nb_chunks(host, blueprint.number, blueprint.nb_chunks)?;
    let current_generation = read_current_generation_or_default(host, U256::zero())?;
    store_blueprint_generation(host, blueprint.number, current_generation)?;
    let store_blueprint = StoreBlueprint::SequencerChunk(blueprint.chunk);
    store_blueprint_chunk(
        host,
        blueprint.number,
        blueprint.chunk_index,
        &store_blueprint,
    )
}

pub fn store_inbox_blueprint_by_number<Host>(
    host: &mut Host,
    blueprint: Blueprint,
    number: U256,
) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    store_blueprint_nb_chunks(host, number, 1)?;
    let current_generation = read_current_generation_or_default(host, U256::zero())?;
    store_blueprint_generation(host, number, current_generation)?;
    let store_blueprint = StoreBlueprint::InboxBlueprint(blueprint);
    store_blueprint_chunk(host, number, 0, &store_blueprint)
}

pub fn store_inbox_blueprint<Host>(
    host: &mut Host,
    blueprint: Blueprint,
) -> anyhow::Result<()>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let number = read_next_blueprint_number(host)?;
    Ok(store_inbox_blueprint_by_number(host, blueprint, number)?)
}

#[inline(always)]
pub fn read_next_blueprint_number<Host>(host: &mut Host) -> Result<U256, Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    match read_current_blueprint_header(host) {
        Ok(blueprint_header) => Ok(blueprint_header.number + 1),
        Err(Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))) => {
            Ok(U256::zero())
        }
        Err(err) => Err(err),
    }
}

// Used to store a blueprint made out of forced delayed transactions.
pub fn store_forced_blueprint<Host>(
    host: &mut Host,
    blueprint: Blueprint,
    number: U256,
) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    store_blueprint_nb_chunks(host, number, 1)?;
    let current_generation = read_current_generation_or_default(host, U256::zero())?;
    store_blueprint_generation(host, number, current_generation)?;
    let store_blueprint = StoreBlueprint::InboxBlueprint(blueprint);
    store_blueprint_chunk(host, number, 0, &store_blueprint)
}

impl Encodable for EVMBlockHeader {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let Self {
            hash,
            receipts_root,
            transactions_root,
        } = self;
        stream.begin_list(3);
        stream.append(hash);
        stream.append(receipts_root);
        stream.append(transactions_root);
    }
}

impl Decodable for EVMBlockHeader {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_list(decoder, 3)?;
        let mut it = decoder.iter();
        let hash = rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "hash")?;
        let receipts_root =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "receipts_root")?;
        let transactions_root =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "transactions_root")?;
        Ok(Self {
            hash,
            receipts_root,
            transactions_root,
        })
    }
}

impl Encodable for TezBlockHeader {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let Self {
            hash,
            next_protocol,
        } = self;
        stream.begin_list(2);
        stream.append(hash);
        stream.append(next_protocol);
    }
}

impl Decodable for TezBlockHeader {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        let item_count = decoder.item_count()?;
        let mut it = decoder.iter();
        let hash = decode_field(&rlp_helpers::next(&mut it)?, "hash")?;
        let next_protocol: Protocol = match item_count {
            // V0 format: only hash, default next_protocol to INITIAL_PROTOCOL
            1 => INITIAL_PROTOCOL,
            // V1 format: hash + next_protocol
            2 => decode_field(&rlp_helpers::next(&mut it)?, "protocol")?,
            _ => return Err(DecoderError::RlpIncorrectListLen),
        };
        Ok(Self {
            hash,
            next_protocol,
        })
    }
}

impl Encodable for ChainHeader {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        match self {
            Self::Eth(evm_block_header) => {
                stream.append(evm_block_header);
            }
            Self::Tez(tez_block_header) => {
                stream.append(tez_block_header);
            }
        }
    }
}

impl Encodable for BlockHeader<ChainHeader> {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let Self {
            blueprint_header: BlueprintHeader { number, timestamp },
            chain_header,
        } = self;
        stream.begin_list(3);
        append_u256_le(stream, number);
        append_timestamp(stream, *timestamp);
        stream.begin_list(1); // Nesting added for forward-compatibility with multichain
        stream.append(chain_header);
    }
}

impl<H: Decodable> Decodable for BlockHeader<H> {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_list(decoder, 3)?;
        let mut it = decoder.iter();
        let number = decode_field_u256_le(&rlp_helpers::next(&mut it)?, "number")?;
        let timestamp = decode_timestamp(&rlp_helpers::next(&mut it)?)?;
        let decoder = rlp_helpers::next(&mut it)?;
        rlp_helpers::check_list(&decoder, 1)?; // Nesting added for forward-compatibility with multichain
        let mut it = decoder.iter();
        let chain_header =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "chain_header")?;
        Ok(Self {
            blueprint_header: BlueprintHeader { number, timestamp },
            chain_header,
        })
    }
}

pub fn store_current_block_header<Host>(
    host: &mut Host,
    current_block_header: &BlockHeader<ChainHeader>,
) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let mut base = crate::storage::load_base_keyspace(host)?;
    keyspace::store_rlp(
        current_block_header,
        &mut base,
        &EVM_CURRENT_BLOCK_HEADER_KEY,
    )
    .map_err(Error::from)
}

pub fn read_current_block_header<Host, H: Decodable>(
    host: &mut Host,
) -> Result<BlockHeader<H>, Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let base = crate::storage::load_base_keyspace(host)?;
    Ok(keyspace::read_rlp(&base, &EVM_CURRENT_BLOCK_HEADER_KEY)?)
}

pub fn read_current_blueprint_header<Host>(
    host: &mut Host,
) -> Result<BlueprintHeader, Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let block_header = read_current_block_header::<_, rlp_helpers::IgnoredField>(host)?;
    Ok(block_header.blueprint_header)
}

pub fn store_current_tez_block_header<Host>(
    host: &mut Host,
    header: &TezBlockHeader,
) -> Result<(), Error>
where
    Host: StorageV1,
{
    store_rlp(header, host, &TEZ_CURRENT_BLOCK_HEADER).map_err(Error::from)
}

pub fn read_current_tez_block_header<Host>(host: &Host) -> Result<TezBlockHeader, Error>
where
    Host: StorageV1,
{
    Ok(read_rlp(host, &TEZ_CURRENT_BLOCK_HEADER)?)
}

/// For the tick model we only accept blueprints where cumulative size of chunks
/// less or equal than 512kB. A chunk weights 4kB, then (512 * 1024) / 4096 =
/// 128.
pub const MAXIMUM_NUMBER_OF_CHUNKS: u16 = 128;

const MAXIMUM_SIZE_OF_BLUEPRINT: usize =
    MAXIMUM_NUMBER_OF_CHUNKS as usize * MAX_INPUT_MESSAGE_SIZE;

const MAXIMUM_SIZE_OF_DELAYED_TRANSACTION: usize = MAX_INPUT_MESSAGE_SIZE;

/// Possible errors when validating a blueprint
/// Only used for test, as all errors are handled in the same way
#[cfg_attr(feature = "benchmark", allow(dead_code))]
#[derive(Debug, PartialEq)]
pub enum BlueprintValidity {
    Valid(Blueprint),
    InvalidParentHash,
    TimestampFromPast,
    TimestampFromFuture,
    DecoderError(DecoderError),
    DelayedHashMissing(delayed_inbox::Hash),
    DuplicateDelayedHash(delayed_inbox::Hash),
    BlueprintTooLarge,
    // The blueprint becomes stale when we expect it
    // to not be used anymore. For now it's when :
    // - Flush when taking transactions from delayed inbox
    // - Change sequencer key
    StaleBlueprint,
}

pub enum DelayedTransactionFetchingResult<Tx> {
    Ok(Vec<Tx>),
    BlueprintTooLarge,
    DelayedHashMissing(delayed_inbox::Hash),
}

pub fn fetch_hashes_from_delayed_inbox<Host>(
    host: &mut Host,
    delayed_hashes: Vec<delayed_inbox::Hash>,
    delayed_inbox: &DelayedInbox,
    current_blueprint_size: usize,
    block_number: U256,
) -> anyhow::Result<(DelayedTransactionFetchingResult<TezosXTransaction>, usize)>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let mut delayed_txs = vec![];
    let mut total_size = current_blueprint_size;
    let experimental_features = ExperimentalFeatures::read_from_storage(host);
    for tx_hash in delayed_hashes {
        let tx = delayed_inbox.find_transaction(host, tx_hash)?;
        match tx {
            Some(tx) => {
                if let TransactionContent::TezosDelayed(_) = &tx.0.content {
                    if !experimental_features.is_tezos_runtime_enabled(block_number) {
                        log!(Error,
                            "TezosDelayed transaction found in delayed inbox while Tezos runtime is disabled. Skipping."
                        );
                        continue;
                    }
                }
                // This is overestimated, as the transactions cannot be chunked in the
                // delayed bridge.
                total_size += MAXIMUM_SIZE_OF_DELAYED_TRANSACTION;
                // If the size would overflow the 512KB, reject the blueprint
                if MAXIMUM_SIZE_OF_BLUEPRINT < total_size {
                    return Ok((
                        DelayedTransactionFetchingResult::BlueprintTooLarge,
                        total_size,
                    ));
                }
                delayed_txs.push(tx.0)
            }
            None => {
                return Ok((
                    DelayedTransactionFetchingResult::DelayedHashMissing(tx_hash),
                    total_size,
                ))
            }
        }
    }
    Ok((
        DelayedTransactionFetchingResult::Ok(
            delayed_txs
                .into_iter()
                .map(|tx| TezosXTransaction::Ethereum(Box::new(tx)))
                .collect(),
        ),
        total_size,
    ))
}

fn transactions_from_bytes(
    transactions: Vec<Vec<u8>>,
    blueprint_version: u8,
) -> anyhow::Result<Vec<TezosXTransaction>> {
    let mut result = vec![];
    for tx_common in transactions.iter() {
        let transaction =
            TezosXChainConfig::transaction_from_bytes(tx_common, blueprint_version)?;
        result.push(transaction)
    }
    Ok(result)
}

pub fn fetch_delayed_txs<Host>(
    host: &mut Host,
    blueprint_with_hashes: BlueprintWithDelayedHashes,
    delayed_inbox: &DelayedInbox,
    current_blueprint_size: usize,
    block_number: U256,
) -> anyhow::Result<(BlueprintValidity, usize)>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let (mut delayed_txs, total_size) =
        match TezosXChainConfig::fetch_hashes_from_delayed_inbox(
            host,
            blueprint_with_hashes.delayed_hashes,
            delayed_inbox,
            current_blueprint_size,
            block_number,
        )? {
            (DelayedTransactionFetchingResult::Ok(delayed_txs), total_size) => {
                (delayed_txs, total_size)
            }
            (DelayedTransactionFetchingResult::BlueprintTooLarge, total_size) => {
                return Ok((BlueprintValidity::BlueprintTooLarge, total_size));
            }
            (DelayedTransactionFetchingResult::DelayedHashMissing(hash), total_size) => {
                return Ok((BlueprintValidity::DelayedHashMissing(hash), total_size));
            }
        };

    let transactions_with_hashes = transactions_from_bytes(
        blueprint_with_hashes.transactions,
        blueprint_with_hashes.version,
    )?;

    delayed_txs.extend(transactions_with_hashes);
    Ok((
        BlueprintValidity::Valid(Blueprint {
            transactions: delayed_txs,
            timestamp: blueprint_with_hashes.timestamp,
        }),
        total_size,
    ))
}

// Default value is 5 minutes. The rationale for 5 minutes is that we have
// only the timestamp from the predecessor block, and we want the rollup to
// accept blocks even if the chain is impacted by high rounds (e.g. > 10).
// The predecessor block timestamp can be completely off and we do not
// wish to refuse such blueprints.
pub const DEFAULT_MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS: i64 = 300i64;

#[allow(clippy::too_many_arguments)]
fn parse_and_validate_blueprint<Host>(
    host: &mut Host,
    bytes: &[u8],
    delayed_inbox: &DelayedInbox,
    current_blueprint_size: usize,
    evm_node_flag: bool,
    max_blueprint_lookahead_in_seconds: i64,
    parent_chain_header: &EVMBlockHeader,
    head_timestamp: Timestamp,
    block_number: U256,
) -> anyhow::Result<(BlueprintValidity, usize)>
where
    Host: StorageV1 + KeySpaceLoader,
{
    // Decode
    match rlp::decode::<BlueprintWithDelayedHashes>(bytes) {
        Err(e) => Ok((BlueprintValidity::DecoderError(e), bytes.len())),
        Ok(blueprint_with_hashes) => {
            // Reject blueprints that reference the same delayed transaction
            // more than once. `find_transaction` is read-only, so two copies
            // of the same hash both resolve to the same transaction and both
            // apply. Deposits and FA deposits have no nonce-based replay
            // guard, so a duplicate would credit the receiver twice from a
            // single L1 inbox event. `BTreeSet` over `HashSet` to avoid any
            // dependence on the per-process hasher seed in the rollup PVM.
            let mut seen = BTreeSet::new();
            for hash in &blueprint_with_hashes.delayed_hashes {
                if !seen.insert(*hash) {
                    return Ok((
                        BlueprintValidity::DuplicateDelayedHash(*hash),
                        bytes.len(),
                    ));
                }
            }

            // Validate parent hash
            #[cfg(not(feature = "benchmark"))]
            if parent_chain_header.hash() != blueprint_with_hashes.parent_hash {
                return Ok((BlueprintValidity::InvalidParentHash, bytes.len()));
            }

            // Validate parent timestamp
            #[cfg(not(feature = "benchmark"))]
            if blueprint_with_hashes.timestamp < head_timestamp {
                return Ok((BlueprintValidity::TimestampFromPast, bytes.len()));
            }

            // The timestamp must be within  max_blueprint_lookahead_in_seconds
            // of the current view of the L1 timestamp.
            //
            // That means that the sequencer cannot produce blueprints too much
            // in the future. If the L1 timestamp is not progressing i.e.
            // the network is stuck, the sequencer will have to reinject the
            // blueprint when the L1 timestamp is finally greater than
            // blueprint timestamp.
            //
            // All this prevents the sequencer to manipulate too much the
            // timestamps.
            #[cfg(not(feature = "benchmark"))]
            {
                let last_seen_l1_timestamp = read_last_info_per_level_timestamp(host)?;
                let accepted_bound = Timestamp::from(
                    last_seen_l1_timestamp
                        .i64()
                        .saturating_add(max_blueprint_lookahead_in_seconds),
                );

                // In the sequencer we don't have a valid `last_seen_l1_timesteamp`
                // so it must not fails on this.
                if !evm_node_flag && blueprint_with_hashes.timestamp > accepted_bound {
                    log!(
                        Debug,
                        "Accepted bound is {}, Blueprint.timestamp is {}",
                        accepted_bound,
                        blueprint_with_hashes.timestamp
                    );
                    return Ok((BlueprintValidity::TimestampFromFuture, bytes.len()));
                }
            }

            // Fetch delayed transactions
            fetch_delayed_txs(
                host,
                blueprint_with_hashes,
                delayed_inbox,
                current_blueprint_size,
                block_number,
            )
        }
    }
}

fn invalidate_blueprint<Host>(
    host: &mut Host,
    number: U256,
    error: &BlueprintValidity,
) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    log!(
        Info,
        "Deleting invalid blueprint at path {}, error: {:?}",
        blueprint_path(number)?,
        error
    );
    // Remove invalid blueprint from storage
    delete_blueprint(host, number)
}

// Reads a single blueprint chunk through the `/base` keyspace.
fn read_blueprint_chunk<Host>(
    host: &mut Host,
    number: U256,
    chunk_index: u16,
) -> Result<StoreBlueprint, Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let key = blueprint_chunk_key(number, chunk_index)?;
    let base = crate::storage::load_base_keyspace(host)?;
    keyspace::read_rlp(&base, &key).map_err(Error::from)
}

fn read_all_chunks_and_validate<Host>(
    host: &mut Host,
    number: U256,
    nb_chunks: u16,
    config: &Configuration,
    previous_chain_header: &EVMBlockHeader,
    previous_timestamp: Timestamp,
    block_number: U256,
) -> anyhow::Result<(Option<Blueprint>, usize)>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let mut chunks = vec![];
    let mut size = 0;
    if nb_chunks > MAXIMUM_NUMBER_OF_CHUNKS {
        invalidate_blueprint(host, number, &BlueprintValidity::BlueprintTooLarge)?;
        return Ok((None, 0));
    };
    for i in 0..nb_chunks {
        let stored_chunk = match read_blueprint_chunk(host, number, i) {
            Ok(chunk) => chunk,
            Err(Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))) => {
                delete_blueprint(host, number)?;
                return Ok((None, 0));
            }
            Err(err) => return Err(err.into()),
        };
        // The tick model is based on the size of the chunk, we overapproximate it.
        size += MAX_INPUT_MESSAGE_SIZE;
        match stored_chunk {
            StoreBlueprint::InboxBlueprint(blueprint) => {
                // Special case when there's an inbox blueprint stored.
                // There must be only one chunk in this case.
                return Ok((Some(blueprint), size));
            }
            StoreBlueprint::SequencerChunk(chunk) => chunks.push(chunk),
        }
    }
    match &config.mode {
        ConfigurationMode::Proxy => Ok((None, size)),
        ConfigurationMode::Sequencer {
            delayed_inbox,
            evm_node_flag,
            max_blueprint_lookahead_in_seconds,
            ..
        } => {
            let validity: (BlueprintValidity, usize) = parse_and_validate_blueprint(
                host,
                chunks.concat().as_slice(),
                delayed_inbox,
                size,
                *evm_node_flag,
                *max_blueprint_lookahead_in_seconds,
                previous_chain_header,
                previous_timestamp,
                block_number,
            )?;
            if let (BlueprintValidity::Valid(blueprint), size_with_delayed_transactions) =
                validity
            {
                log!(
                    Benchmarking,
                    "Number of transactions in blueprint: {}",
                    blueprint.transactions.len()
                );
                Ok((Some(blueprint), size_with_delayed_transactions))
            } else {
                invalidate_blueprint(host, number, &validity.0)?;
                Ok((None, size))
            }
        }
    }
}

pub fn read_blueprint<Host>(
    host: &mut Host,
    config: &Configuration,
    number: U256,
    previous_timestamp: Timestamp,
    previous_chain_header: &EVMBlockHeader,
) -> anyhow::Result<(Option<Blueprint>, usize)>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let exists = blueprint_exists(host, number)?;
    if exists {
        let nb_chunks = read_blueprint_nb_chunks(host, number)?;
        let current_generation = read_current_generation_or_default(host, U256::zero())?;
        let blueprint_generation =
            read_blueprint_generation_or_default(host, number, U256::zero())?;
        // If the generation is not the current one, the blueprint is stale
        if blueprint_generation < current_generation {
            invalidate_blueprint(host, number, &BlueprintValidity::StaleBlueprint)?;
            return Ok((None, 0));
        }
        log!(Benchmarking, "Number of chunks in blueprint: {}", nb_chunks);
        // All chunks are available
        let (blueprint, size) = read_all_chunks_and_validate(
            host,
            number,
            nb_chunks,
            config,
            previous_chain_header,
            previous_timestamp,
            number,
        )?;
        Ok((blueprint, size))
    } else {
        log!(Benchmarking, "Number of chunks in blueprint: {}", 0);
        log!(Benchmarking, "Number of transactions in blueprint: {}", 0);
        Ok((None, 0))
    }
}

#[cfg(test)]
pub fn read_next_blueprint<Host>(
    host: &mut Host,
    config: &mut Configuration,
) -> anyhow::Result<(Option<Blueprint>, usize)>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let (number, previous_timestamp, block_header) =
        match read_current_block_header::<_, EVMBlockHeader>(host) {
            Ok(BlockHeader {
                blueprint_header,
                chain_header,
            }) => (
                blueprint_header.number + 1,
                blueprint_header.timestamp,
                chain_header,
            ),
            Err(_) => (
                U256::zero(),
                Timestamp::from(0),
                EVMBlockHeader::genesis_header(),
            ),
        };
    read_blueprint(host, config, number, previous_timestamp, &block_header)
}

pub fn drop_blueprint<Host>(host: &mut Host, number: U256) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    delete_blueprint(host, number)
}

pub fn delete_blueprint<Host>(host: &mut Host, number: U256) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let exists = blueprint_exists(host, number)?;
    if !exists {
        return Ok(());
    }
    let nb_chunks = read_blueprint_nb_chunks(host, number)?;
    // Delete the whole `/base/blueprints/<n>` subtree through the keyspace.
    // `KeySpace::delete` is infallible (it returns whether the key existed),
    // so absent keys need no special handling — unlike the raw
    // `store_delete` + `allow_path_not_found` it replaces.
    let mut base = crate::storage::load_base_keyspace(host)?;
    for i in 0..nb_chunks {
        base.delete(&blueprint_chunk_key(number, i)?);
    }
    base.delete(&blueprint_generation_key(number)?);
    base.delete(&blueprint_nb_chunks_key(number)?);
    Ok(())
}

pub fn blueprint_exists<Host>(host: &mut Host, number: U256) -> Result<bool, Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let key = blueprint_nb_chunks_key(number)?;
    let base = crate::storage::load_base_keyspace(host)?;
    Ok(base.contains(&key))
}

pub fn clear_all_blueprints<Host>(host: &mut Host) -> Result<(), Error>
where
    Host: StorageV1 + KeySpaceLoader,
{
    increment_current_generation(host)?;
    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::block::GENESIS_PARENT_HASH;
    use crate::configuration::{DalConfiguration, TezosContracts};
    use crate::delayed_inbox::Hash;
    use crate::sequencer_blueprint::{
        rlp_roundtrip, rlp_roundtrip_f, LATEST_BLUEPRINT_VERSION,
    };
    use crate::storage::store_last_info_per_level_timestamp;
    use crate::tick_model::constants::MAX_ALLOWED_TICKS;
    use primitive_types::H256;
    use tezos_crypto_rs::hash::ContractKt1Hash;
    use tezos_ethereum::transaction::TRANSACTION_HASH_SIZE;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_encoding::public_key::PublicKey;
    use tezos_tezlink::protocol::TARGET_TEZOS_PROTOCOL;

    fn test_invalid_sequencer_blueprint_is_removed(enable_dal: bool) {
        let mut host = MockKernelHost::default();
        let delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should be created");
        let delayed_bridge: ContractKt1Hash =
            ContractKt1Hash::from_base58_check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
                .unwrap();
        let sequencer: PublicKey = PublicKey::from_b58check(
            "edpkuDMUm7Y53wp4gxeLBXuiAhXZrLn8XB1R83ksvvesH8Lp8bmCfK",
        )
        .unwrap();
        let dal = if enable_dal {
            Some(DalConfiguration {
                slot_indices: vec![5],
            })
        } else {
            None
        };
        let mut config = Configuration {
            tezos_contracts: TezosContracts::default(),
            mode: ConfigurationMode::Sequencer {
                delayed_bridge,
                delayed_inbox: Box::new(delayed_inbox),
                sequencer,
                dal,
                evm_node_flag: false,
                max_blueprint_lookahead_in_seconds: 100_000i64,
            },
            maximum_allowed_ticks: MAX_ALLOWED_TICKS,
            enable_fa_bridge: false,
        };

        let dummy_tx_hash = Hash([0u8; TRANSACTION_HASH_SIZE]);
        let dummy_parent_hash = H256::from_slice(
            &hex::decode(
                "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
            )
            .unwrap(),
        );

        let blueprint_with_invalid_hash: BlueprintWithDelayedHashes =
            BlueprintWithDelayedHashes {
                version: LATEST_BLUEPRINT_VERSION,
                delayed_hashes: vec![dummy_tx_hash],
                parent_hash: dummy_parent_hash,
                timestamp: Timestamp::from(42),
                transactions: vec![],
            };
        let blueprint_with_hashes_bytes =
            rlp::Encodable::rlp_bytes(&blueprint_with_invalid_hash);

        let seq_blueprint = UnsignedSequencerBlueprint {
            chunk: blueprint_with_hashes_bytes.clone().into(),
            number: U256::from(0),
            nb_chunks: 1u16,
            chunk_index: 0u16,
            chain_id: None,
        };

        store_last_info_per_level_timestamp(&mut host, Timestamp::from(40)).unwrap();

        let delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should be created");
        // Blueprint should have invalid parent hash
        let validity = parse_and_validate_blueprint(
            &mut host,
            blueprint_with_hashes_bytes.as_ref(),
            &delayed_inbox,
            0,
            false,
            500,
            &EVMBlockHeader {
                hash: GENESIS_PARENT_HASH,
                receipts_root: vec![0; 32],
                transactions_root: vec![0; 32],
            },
            Timestamp::from(0),
            U256::zero(),
        )
        .expect("Should be able to parse blueprint");
        assert_eq!(
            validity.0,
            BlueprintValidity::DelayedHashMissing(dummy_tx_hash)
        );

        // Store blueprint
        store_sequencer_blueprint(&mut host, seq_blueprint)
            .expect("Should be able to store sequencer blueprint");

        // Blueprint 0 should be stored
        let exists = blueprint_exists(&mut host, U256::zero()).unwrap();
        assert!(exists);

        // Reading the next blueprint should be None, as the delayed hash
        // isn't in the delayed inbox
        let blueprint = read_next_blueprint(&mut host, &mut config)
            .expect("Reading next blueprint should work");
        assert!(blueprint.0.is_none());

        // Next number should be 0, as we didn't read one
        let number = read_next_blueprint_number(&mut host)
            .expect("Should be able to read next blueprint number");
        assert!(number.is_zero());

        // The blueprint 0 should have been removed
        let exists = blueprint_exists(&mut host, U256::zero()).unwrap();
        assert!(!exists);

        // Test with invalid parent hash
        let blueprint_with_invalid_parent_hash: BlueprintWithDelayedHashes =
            BlueprintWithDelayedHashes {
                version: LATEST_BLUEPRINT_VERSION,
                delayed_hashes: vec![],
                parent_hash: H256::zero(),
                timestamp: Timestamp::from(42),
                transactions: vec![],
            };
        let blueprint_with_hashes_bytes =
            rlp::Encodable::rlp_bytes(&blueprint_with_invalid_parent_hash);

        let seq_blueprint = UnsignedSequencerBlueprint {
            chunk: blueprint_with_hashes_bytes.clone().into(),
            number: U256::from(0),
            nb_chunks: 1u16,
            chunk_index: 0u16,
            chain_id: None,
        };

        let delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should be created");
        // Blueprint should have invalid parent hash
        let validity = parse_and_validate_blueprint(
            &mut host,
            blueprint_with_hashes_bytes.as_ref(),
            &delayed_inbox,
            0,
            false,
            500,
            &EVMBlockHeader {
                hash: GENESIS_PARENT_HASH,
                receipts_root: vec![0; 32],
                transactions_root: vec![0; 32],
            },
            Timestamp::from(0),
            U256::zero(),
        )
        .expect("Should be able to parse blueprint");
        assert_eq!(validity.0, BlueprintValidity::InvalidParentHash);

        // Store blueprint
        store_sequencer_blueprint(&mut host, seq_blueprint)
            .expect("Should be able to store sequencer blueprint");
        // Blueprint 0 should be stored
        let exists = blueprint_exists(&mut host, U256::zero()).unwrap();
        assert!(exists);

        // Reading the next blueprint should be None, as the parent hash
        // is invalid
        let blueprint = read_next_blueprint(&mut host, &mut config)
            .expect("Reading next blueprint should work");
        assert!(blueprint.0.is_none());

        // The blueprint 0 should have been removed
        let exists = blueprint_exists(&mut host, U256::zero()).unwrap();
        assert!(!exists)
    }

    #[test]
    fn test_invalid_sequencer_blueprint_is_removed_without_dal() {
        test_invalid_sequencer_blueprint_is_removed(false)
    }

    #[test]
    fn test_invalid_sequencer_blueprint_is_removed_with_dal() {
        test_invalid_sequencer_blueprint_is_removed(true)
    }

    // The current-block-header writer now goes through the `/base` keyspace
    // while its reader still reads the raw absolute path: writing then reading
    // back proves both halves resolve to the same durable location.
    #[test]
    fn store_current_block_header_resolves_to_absolute_path() {
        let mut host = MockKernelHost::default();
        let block_header = BlockHeader {
            blueprint_header: BlueprintHeader {
                number: 7.into(),
                timestamp: Timestamp::from(10),
            },
            chain_header: ChainHeader::Eth(EVMBlockHeader {
                hash: H256::from([42u8; 32]),
                receipts_root: vec![23; 5],
                transactions_root: vec![18; 5],
            }),
        };

        store_current_block_header(&mut host, &block_header).unwrap();

        // The keyspace write must land at the historical absolute path...
        assert!(host.store_read_all(&EVM_CURRENT_BLOCK_HEADER).is_ok());
        // ...and the keyspace reader must decode the same blueprint header back.
        let read = read_current_block_header::<_, EVMBlockHeader>(&mut host).unwrap();
        assert_eq!(read.blueprint_header.number, 7.into());
        assert_eq!(read.blueprint_header.timestamp, Timestamp::from(10));
    }

    // The sequencer-blueprint writer and its readers now go through the `/base`
    // keyspace at relative keys. Checking the raw absolute paths and reading
    // back through the migrated readers proves the relative keys resolve to the
    // historical durable locations.
    #[test]
    fn store_sequencer_blueprint_resolves_to_absolute_paths() {
        let mut host = MockKernelHost::default();
        let number = U256::from(3);
        let chunk_index = 2u16;
        let nb_chunks = 5u16;
        let chunk = vec![7u8; 16];

        let blueprint = UnsignedSequencerBlueprint {
            chunk: chunk.clone(),
            number,
            nb_chunks,
            chunk_index,
            chain_id: None,
        };
        store_sequencer_blueprint(&mut host, blueprint).unwrap();

        // The keyspace writes must land at the historical absolute paths,
        // identical to the `concat`-built ones.
        let path = blueprint_path(number).unwrap();
        let nb_chunks_path = concat(&path, &RefPath::assert_from(b"/nb_chunks")).unwrap();
        let generation_path =
            concat(&path, &RefPath::assert_from(b"/generation")).unwrap();
        let chunk_path = concat(&path, &RefPath::assert_from(b"/2")).unwrap();
        assert_eq!(
            host.store_read_all(&nb_chunks_path).unwrap(),
            nb_chunks.to_le_bytes()
        );
        // The current generation defaults to zero, so the blueprint inherits it.
        assert_eq!(host.store_read_all(&generation_path).unwrap(), [0u8; 32]);
        assert!(host.store_read_all(&chunk_path).is_ok());

        // ...and the migrated readers read the same values back.
        assert_eq!(
            read_blueprint_nb_chunks(&mut host, number).unwrap(),
            nb_chunks
        );
        assert_eq!(
            read_blueprint_generation_or_default(&mut host, number, U256::one()).unwrap(),
            U256::zero()
        );
        assert_eq!(
            read_blueprint_chunk(&mut host, number, chunk_index).unwrap(),
            StoreBlueprint::SequencerChunk(chunk)
        );

        // The blueprint exists and is removed cleanly through the keyspace.
        assert!(blueprint_exists(&mut host, number).unwrap());
        delete_blueprint(&mut host, number).unwrap();
        assert!(!blueprint_exists(&mut host, number).unwrap());
        assert!(host.store_read_all(&chunk_path).is_err());
    }

    #[test]
    fn test_evm_block_header_roundtrip() {
        let blueprint_header = BlueprintHeader {
            number: 42.into(),
            timestamp: Timestamp::from(10),
        };
        let evm_block_header = EVMBlockHeader {
            hash: H256::from([42u8; 32]),
            receipts_root: vec![23; 5],
            transactions_root: vec![18; 5],
        };

        rlp_roundtrip(evm_block_header.clone());

        let block_header = BlockHeader {
            blueprint_header,
            chain_header: ChainHeader::Eth(evm_block_header),
        };

        rlp_roundtrip_f(
            block_header,
            |BlockHeader {
                 blueprint_header,
                 chain_header,
             }| BlockHeader {
                blueprint_header,
                chain_header: ChainHeader::Eth(chain_header),
            },
        );
    }

    #[test]
    fn test_tez_block_header_roundtrip() {
        let blueprint_header = BlueprintHeader {
            number: 42.into(),
            timestamp: Timestamp::from(10),
        };
        let tez_block_header = TezBlockHeader {
            hash: H256(*TezBlock::genesis_block_hash()),
            next_protocol: TARGET_TEZOS_PROTOCOL,
        };
        let block_header = BlockHeader {
            blueprint_header,
            chain_header: ChainHeader::Tez(tez_block_header),
        };

        rlp_roundtrip_f(
            block_header,
            |BlockHeader {
                 blueprint_header,
                 chain_header,
             }| BlockHeader {
                blueprint_header,
                chain_header: ChainHeader::Tez(chain_header),
            },
        );
    }
}
