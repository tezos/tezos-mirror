// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::blueprint::Blueprint;
use crate::chains::{ChainConfigTrait, ChainHeaderTrait, ExperimentalFeatures};
use crate::configuration::{Configuration, ConfigurationMode};
use crate::error::{Error, StorageError};
use crate::l2block::L2Block;
use crate::migration::allow_path_not_found;
use crate::sequencer_blueprint::{
    BlueprintWithDelayedHashes, UnsignedSequencerBlueprint,
};
use crate::storage::read_last_info_per_level_timestamp;
use crate::transaction::{Transaction, TransactionContent};
use crate::{delayed_inbox, DelayedInbox};
use primitive_types::{H256, U256};
use rlp::{Decodable, DecoderError, Encodable};
use std::fmt::Debug;
use tezos_ethereum::block::EthBlock;
use tezos_ethereum::eth_gen::OwnedHash;
use tezos_ethereum::rlp_helpers::{
    self, append_timestamp, append_u256_le, decode_field, decode_field_u256_le,
    decode_timestamp,
};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::Timestamp;
use tezos_smart_rollup_core::MAX_INPUT_MESSAGE_SIZE;
use tezos_smart_rollup_host::path::*;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_storage::{read_rlp, store_read_slice, store_rlp};
use tezos_tezlink::block::TezBlock;

pub const EVM_BLUEPRINTS: RefPath = RefPath::assert_from(b"/evm/blueprints");

const EVM_BLUEPRINT_NB_CHUNKS: RefPath = RefPath::assert_from(b"/nb_chunks");

// This generation number is used to decide if the blueprint containing this sub-key
// is outdated or not. Instead of deleting all blueprints when we want to
// invalidate them, we just increment the current generation number stored
// so that when reading a blueprint we can check if its generation matches
// the current one.
const EVM_BLUEPRINT_GENERATION: RefPath = RefPath::assert_from(b"/generation");

const EVM_CURRENT_BLOCK_HEADER: RefPath =
    RefPath::assert_from(b"/evm/current_block_header");

/// The store representation of a blueprint.
/// It's designed to support storing sequencer blueprints,
/// which can be chunked, and blueprints constructed from
/// inbox messages. Note that the latter are only to be
/// used when the kernel isn't running with a sequencer.
#[derive(PartialEq, Debug, Clone)]
enum StoreBlueprint<Tx> {
    SequencerChunk(Vec<u8>),
    InboxBlueprint(Blueprint<Tx>),
}

const SEQUENCER_CHUNK_TAG: u8 = 0;
const INBOX_BLUEPRINT_TAG: u8 = 1;

impl<Tx: Encodable> Encodable for StoreBlueprint<Tx> {
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

impl<Tx: Decodable> Decodable for StoreBlueprint<Tx> {
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
            chain_header: ChainHeader::Tez(TezBlockHeader { hash: block.hash.0 }),
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

pub fn blueprint_path(number: U256) -> Result<OwnedPath, StorageError> {
    let number_as_path: Vec<u8> = format!("/{number}").into();
    // The key being an integer value, it will always be valid as a path,
    // `assert_from` cannot fail.
    let number_subkey = RefPath::assert_from(&number_as_path);
    concat(&EVM_BLUEPRINTS, &number_subkey).map_err(StorageError::from)
}

fn blueprint_chunk_path(
    blueprint_path: &OwnedPath,
    chunk_index: u16,
) -> Result<OwnedPath, StorageError> {
    let chunk_index_as_path: Vec<u8> = format!("/{chunk_index}").into();
    let chunk_index_subkey = RefPath::assert_from(&chunk_index_as_path);
    concat(blueprint_path, &chunk_index_subkey).map_err(StorageError::from)
}

fn blueprint_nb_chunks_path(
    blueprint_path: &OwnedPath,
) -> Result<OwnedPath, StorageError> {
    concat(blueprint_path, &EVM_BLUEPRINT_NB_CHUNKS).map_err(StorageError::from)
}

fn blueprint_current_generation_path() -> Result<OwnedPath, StorageError> {
    concat(&EVM_BLUEPRINTS, &EVM_BLUEPRINT_GENERATION).map_err(StorageError::from)
}

fn blueprint_generation_path(
    blueprint_path: &OwnedPath,
) -> Result<OwnedPath, StorageError> {
    concat(blueprint_path, &EVM_BLUEPRINT_GENERATION).map_err(StorageError::from)
}

fn read_current_generation_or_default<Host: Runtime>(
    host: &Host,
    default: U256,
) -> Result<U256, Error> {
    let path = blueprint_current_generation_path()?;
    let mut buffer = [0u8; 32];
    match store_read_slice(host, &path, &mut buffer, 32) {
        Ok(()) => Ok(U256::from_little_endian(&buffer)),
        Err(tezos_storage::error::Error::Runtime(RuntimeError::PathNotFound))
        | Err(tezos_storage::error::Error::Runtime(RuntimeError::HostErr(
            tezos_smart_rollup_host::Error::StoreNotAValue,
        ))) => Ok(default),
        Err(err) => Err(err.into()),
    }
}

fn store_current_generation<Host: Runtime>(
    host: &mut Host,
    generation: U256,
) -> Result<(), Error> {
    let path = blueprint_current_generation_path()?;
    let mut buffer = [0u8; 32];
    generation.to_little_endian(&mut buffer);
    host.store_write(&path, &buffer, 0).map_err(Error::from)
}

fn increment_current_generation<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
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

fn read_blueprint_generation_or_default<Host: Runtime>(
    host: &Host,
    blueprint_path: &OwnedPath,
    default: U256,
) -> Result<U256, Error> {
    let path = blueprint_generation_path(blueprint_path)?;
    let mut buffer = [0u8; 32];
    match store_read_slice(host, &path, &mut buffer, 32) {
        Ok(()) => Ok(U256::from_little_endian(&buffer)),
        Err(tezos_storage::error::Error::Runtime(RuntimeError::PathNotFound))
        | Err(tezos_storage::error::Error::Runtime(RuntimeError::HostErr(
            tezos_smart_rollup_host::Error::StoreNotAValue,
        ))) => Ok(default),
        Err(err) => Err(err.into()),
    }
}

fn store_blueprint_generation<Host: Runtime>(
    host: &mut Host,
    blueprint_path: &OwnedPath,
    generation: U256,
) -> Result<(), Error> {
    let path = blueprint_generation_path(blueprint_path)?;
    let mut buffer = [0u8; 32];
    generation.to_little_endian(&mut buffer);
    host.store_write(&path, &buffer, 0).map_err(Error::from)
}

fn read_blueprint_nb_chunks<Host: Runtime>(
    host: &Host,
    blueprint_path: &OwnedPath,
) -> Result<u16, Error> {
    let path = blueprint_nb_chunks_path(blueprint_path)?;
    let mut buffer = [0u8; 2];
    store_read_slice(host, &path, &mut buffer, 2)?;
    Ok(u16::from_le_bytes(buffer))
}

fn store_blueprint_nb_chunks<Host: Runtime>(
    host: &mut Host,
    blueprint_path: &OwnedPath,
    nb_chunks: u16,
) -> Result<(), Error> {
    let path = blueprint_nb_chunks_path(blueprint_path)?;
    let bytes = nb_chunks.to_le_bytes();
    host.store_write(&path, &bytes, 0).map_err(Error::from)
}

pub fn store_sequencer_blueprint<Host: Runtime>(
    host: &mut Host,
    blueprint: UnsignedSequencerBlueprint,
) -> Result<(), Error> {
    let blueprint_path = blueprint_path(blueprint.number)?;
    store_blueprint_nb_chunks(host, &blueprint_path, blueprint.nb_chunks)?;
    let current_generation = read_current_generation_or_default(host, U256::zero())?;
    store_blueprint_generation(host, &blueprint_path, current_generation)?;
    let blueprint_chunk_path =
        blueprint_chunk_path(&blueprint_path, blueprint.chunk_index)?;
    // The `Transactions` type parameter of `StoreBlueprint` is not
    // used in the `SequencerChunk` case so it is irrelevant here, we could pass
    // any type implementing the `Encodable` trait.
    let store_blueprint = StoreBlueprint::<Vec<u8>>::SequencerChunk(blueprint.chunk);
    store_rlp(&store_blueprint, host, &blueprint_chunk_path).map_err(Error::from)
}

pub fn store_inbox_blueprint_by_number<Host: Runtime, Tx: Encodable>(
    host: &mut Host,
    blueprint: Blueprint<Tx>,
    number: U256,
) -> Result<(), Error> {
    let blueprint_path = blueprint_path(number)?;
    store_blueprint_nb_chunks(host, &blueprint_path, 1)?;
    let current_generation = read_current_generation_or_default(host, U256::zero())?;
    store_blueprint_generation(host, &blueprint_path, current_generation)?;
    let chunk_path = blueprint_chunk_path(&blueprint_path, 0)?;
    let store_blueprint = StoreBlueprint::InboxBlueprint(blueprint);
    store_rlp(&store_blueprint, host, &chunk_path).map_err(Error::from)
}

pub fn store_inbox_blueprint<Host: Runtime, Tx: Encodable>(
    host: &mut Host,
    blueprint: Blueprint<Tx>,
) -> anyhow::Result<()> {
    let number = read_next_blueprint_number(host)?;
    Ok(store_inbox_blueprint_by_number(host, blueprint, number)?)
}

#[inline(always)]
pub fn read_next_blueprint_number<Host: Runtime>(host: &Host) -> Result<U256, Error> {
    match read_current_blueprint_header(host) {
        Ok(blueprint_header) => Ok(blueprint_header.number + 1),
        Err(Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))) => {
            Ok(U256::zero())
        }
        Err(err) => Err(err),
    }
}

// Used to store a blueprint made out of forced delayed transactions.
pub fn store_forced_blueprint<Host: Runtime, Tx: Encodable>(
    host: &mut Host,
    blueprint: Blueprint<Tx>,
    number: U256,
) -> Result<(), Error> {
    let blueprint_path = blueprint_path(number)?;
    store_blueprint_nb_chunks(host, &blueprint_path, 1)?;
    let current_generation = read_current_generation_or_default(host, U256::zero())?;
    store_blueprint_generation(host, &blueprint_path, current_generation)?;
    let chunk_path = blueprint_chunk_path(&blueprint_path, 0)?;
    let store_blueprint = StoreBlueprint::InboxBlueprint(blueprint);
    store_rlp(&store_blueprint, host, &chunk_path).map_err(Error::from)
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
        let Self { hash } = self;
        stream.begin_list(1);
        stream.append(hash);
    }
}

impl Decodable for TezBlockHeader {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_list(decoder, 1)?;
        let mut it = decoder.iter();
        let hash = decode_field(&rlp_helpers::next(&mut it)?, "hash")?;
        Ok(Self { hash })
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
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "block_header")?;
        Ok(Self {
            blueprint_header: BlueprintHeader { number, timestamp },
            chain_header,
        })
    }
}

pub fn store_current_block_header<Host: Runtime>(
    host: &mut Host,
    current_block_header: &BlockHeader<ChainHeader>,
) -> Result<(), Error> {
    store_rlp(current_block_header, host, &EVM_CURRENT_BLOCK_HEADER).map_err(Error::from)
}

pub fn read_current_block_header<Host: Runtime, H: Decodable>(
    host: &Host,
) -> Result<BlockHeader<H>, Error> {
    Ok(read_rlp(host, &EVM_CURRENT_BLOCK_HEADER)?)
}

pub fn read_current_blueprint_header<Host: Runtime>(
    host: &Host,
) -> Result<BlueprintHeader, Error> {
    let block_header = read_current_block_header::<_, rlp_helpers::IgnoredField>(host)?;
    Ok(block_header.blueprint_header)
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
pub enum BlueprintValidity<Tx> {
    Valid(Blueprint<Tx>),
    InvalidParentHash,
    TimestampFromPast,
    TimestampFromFuture,
    DecoderError(DecoderError),
    DelayedHashMissing(delayed_inbox::Hash),
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

pub fn fetch_hashes_from_delayed_inbox<Host: Runtime>(
    host: &mut Host,
    delayed_hashes: Vec<delayed_inbox::Hash>,
    delayed_inbox: &mut DelayedInbox,
    current_blueprint_size: usize,
) -> anyhow::Result<(DelayedTransactionFetchingResult<Transaction>, usize)> {
    let mut delayed_txs = vec![];
    let mut total_size = current_blueprint_size;
    let experimental_features = ExperimentalFeatures::read_from_storage(host);
    for tx_hash in delayed_hashes {
        let tx = delayed_inbox.find_transaction(host, tx_hash)?;
        match tx {
            Some(tx) => {
                if let TransactionContent::TezosDelayed(_) = &tx.0.content {
                    if !experimental_features.is_tezos_runtime_enabled() {
                        log!(
                            host,
                            Error,
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
        DelayedTransactionFetchingResult::Ok(delayed_txs),
        total_size,
    ))
}

fn transactions_from_bytes<ChainConfig: ChainConfigTrait>(
    transactions: Vec<Vec<u8>>,
) -> anyhow::Result<Vec<ChainConfig::Transaction>> {
    transactions
        .iter()
        .map(|tx_common| ChainConfig::transaction_from_bytes(tx_common))
        .collect::<anyhow::Result<Vec<ChainConfig::Transaction>>>()
}

pub fn fetch_delayed_txs<Host: Runtime, ChainConfig: ChainConfigTrait>(
    host: &mut Host,
    blueprint_with_hashes: BlueprintWithDelayedHashes,
    delayed_inbox: &mut DelayedInbox,
    current_blueprint_size: usize,
) -> anyhow::Result<(BlueprintValidity<ChainConfig::Transaction>, usize)> {
    let (mut delayed_txs, total_size) =
        match ChainConfig::fetch_hashes_from_delayed_inbox(
            host,
            blueprint_with_hashes.delayed_hashes,
            delayed_inbox,
            current_blueprint_size,
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

    let transactions_with_hashes =
        transactions_from_bytes::<ChainConfig>(blueprint_with_hashes.transactions)?;

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
fn parse_and_validate_blueprint<Host: Runtime, ChainConfig: ChainConfigTrait>(
    host: &mut Host,
    bytes: &[u8],
    delayed_inbox: &mut DelayedInbox,
    current_blueprint_size: usize,
    evm_node_flag: bool,
    max_blueprint_lookahead_in_seconds: i64,
    parent_chain_header: &ChainConfig::ChainHeader,
    head_timestamp: Timestamp,
) -> anyhow::Result<(BlueprintValidity<ChainConfig::Transaction>, usize)> {
    // Decode
    match rlp::decode::<BlueprintWithDelayedHashes>(bytes) {
        Err(e) => Ok((BlueprintValidity::DecoderError(e), bytes.len())),
        Ok(blueprint_with_hashes) => {
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
                        host,
                        Debug,
                        "Accepted bound is {}, Blueprint.timestamp is {}",
                        accepted_bound,
                        blueprint_with_hashes.timestamp
                    );
                    return Ok((BlueprintValidity::TimestampFromFuture, bytes.len()));
                }
            }

            // Fetch delayed transactions
            fetch_delayed_txs::<_, ChainConfig>(
                host,
                blueprint_with_hashes,
                delayed_inbox,
                current_blueprint_size,
            )
        }
    }
}

fn invalidate_blueprint<Host: Runtime, Tx: Debug>(
    host: &mut Host,
    blueprint_path: &OwnedPath,
    error: &BlueprintValidity<Tx>,
) -> Result<(), Error> {
    log!(
        host,
        Info,
        "Deleting invalid blueprint at path {}, error: {:?}",
        blueprint_path,
        error
    );
    // Remove invalid blueprint from storage
    delete_blueprint(host, blueprint_path)
}

fn read_all_chunks_and_validate<Host: Runtime, ChainConfig: ChainConfigTrait>(
    host: &mut Host,
    blueprint_path: &OwnedPath,
    nb_chunks: u16,
    config: &mut Configuration,
    previous_chain_header: &ChainConfig::ChainHeader,
    previous_timestamp: Timestamp,
) -> anyhow::Result<(Option<Blueprint<ChainConfig::Transaction>>, usize)> {
    let mut chunks = vec![];
    let mut size = 0;
    if nb_chunks > MAXIMUM_NUMBER_OF_CHUNKS {
        invalidate_blueprint::<_, ChainConfig::Transaction>(
            host,
            blueprint_path,
            &BlueprintValidity::BlueprintTooLarge,
        )?;
        return Ok((None, 0));
    };
    for i in 0..nb_chunks {
        let path = blueprint_chunk_path(blueprint_path, i)?;
        let stored_chunk = match read_rlp(host, &path) {
            Ok(chunk) => chunk,
            Err(tezos_storage::error::Error::Runtime(RuntimeError::PathNotFound)) => {
                delete_blueprint(host, blueprint_path)?;
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
    match &mut config.mode {
        ConfigurationMode::Proxy => Ok((None, size)),
        ConfigurationMode::Sequencer {
            delayed_inbox,
            evm_node_flag,
            max_blueprint_lookahead_in_seconds,
            ..
        } => {
            let validity: (BlueprintValidity<ChainConfig::Transaction>, usize) =
                parse_and_validate_blueprint::<_, ChainConfig>(
                    host,
                    chunks.concat().as_slice(),
                    delayed_inbox,
                    size,
                    *evm_node_flag,
                    *max_blueprint_lookahead_in_seconds,
                    previous_chain_header,
                    previous_timestamp,
                )?;
            if let (BlueprintValidity::Valid(blueprint), size_with_delayed_transactions) =
                validity
            {
                log!(
                    host,
                    Benchmarking,
                    "Number of transactions in blueprint: {}",
                    blueprint.transactions.len()
                );
                Ok((Some(blueprint), size_with_delayed_transactions))
            } else {
                invalidate_blueprint(host, blueprint_path, &validity.0)?;
                Ok((None, size))
            }
        }
    }
}

pub fn read_blueprint<Host: Runtime, ChainConfig: ChainConfigTrait>(
    host: &mut Host,
    config: &mut Configuration,
    number: U256,
    previous_timestamp: Timestamp,
    previous_chain_header: &ChainConfig::ChainHeader,
) -> anyhow::Result<(Option<Blueprint<ChainConfig::Transaction>>, usize)> {
    let blueprint_path = blueprint_path(number)?;
    let exists = blueprint_exists(host, &blueprint_path)?;
    if exists {
        let nb_chunks = read_blueprint_nb_chunks(host, &blueprint_path)?;
        let current_generation = read_current_generation_or_default(host, U256::zero())?;
        let blueprint_generation =
            read_blueprint_generation_or_default(host, &blueprint_path, U256::zero())?;
        // If the generation is not the current one, the blueprint is stale
        if blueprint_generation < current_generation {
            invalidate_blueprint(
                host,
                &blueprint_path,
                &BlueprintValidity::<Vec<ChainConfig::Transaction>>::StaleBlueprint,
            )?;
            return Ok((None, 0));
        }
        log!(
            host,
            Benchmarking,
            "Number of chunks in blueprint: {}",
            nb_chunks
        );
        // All chunks are available
        let (blueprint, size) = read_all_chunks_and_validate::<_, ChainConfig>(
            host,
            &blueprint_path,
            nb_chunks,
            config,
            previous_chain_header,
            previous_timestamp,
        )?;
        Ok((blueprint, size))
    } else {
        log!(host, Benchmarking, "Number of chunks in blueprint: {}", 0);
        log!(
            host,
            Benchmarking,
            "Number of transactions in blueprint: {}",
            0
        );
        Ok((None, 0))
    }
}

#[cfg(test)]
pub fn read_next_blueprint<Host: Runtime>(
    host: &mut Host,
    config: &mut Configuration,
) -> anyhow::Result<(Option<Blueprint<crate::transaction::Transaction>>, usize)> {
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
    read_blueprint::<_, crate::chains::EvmChainConfig>(
        host,
        config,
        number,
        previous_timestamp,
        &block_header,
    )
}

pub fn drop_blueprint<Host: Runtime>(host: &mut Host, number: U256) -> Result<(), Error> {
    let path = blueprint_path(number)?;
    delete_blueprint(host, &path)
}

pub fn delete_blueprint<Host: Runtime>(
    host: &mut Host,
    blueprint_path: &OwnedPath,
) -> Result<(), Error> {
    let exists = blueprint_exists(host, blueprint_path)?;
    if !exists {
        return Ok(());
    }
    let nb_chunks = read_blueprint_nb_chunks(host, blueprint_path)?;
    for i in 0..nb_chunks {
        let chunk_path = blueprint_chunk_path(blueprint_path, i)?;
        allow_path_not_found(host.store_delete(&chunk_path))?;
    }
    allow_path_not_found(host.store_delete(&blueprint_generation_path(blueprint_path)?))?;
    allow_path_not_found(host.store_delete(&blueprint_nb_chunks_path(blueprint_path)?))?;
    Ok(())
}

pub fn blueprint_exists<Host: Runtime>(
    host: &Host,
    blueprint_path: &OwnedPath,
) -> Result<bool, Error> {
    host.store_has(&blueprint_nb_chunks_path(blueprint_path)?)
        .map_err(Error::from)
        .map(|exists| exists.is_some())
}

pub fn clear_all_blueprints<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    increment_current_generation(host)?;
    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::block::GENESIS_PARENT_HASH;
    use crate::chains::EvmChainConfig;
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

        let mut delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should be created");
        // Blueprint should have invalid parent hash
        let validity = parse_and_validate_blueprint::<_, EvmChainConfig>(
            &mut host,
            blueprint_with_hashes_bytes.as_ref(),
            &mut delayed_inbox,
            0,
            false,
            500,
            &EVMBlockHeader {
                hash: GENESIS_PARENT_HASH,
                receipts_root: vec![0; 32],
                transactions_root: vec![0; 32],
            },
            Timestamp::from(0),
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
        let blueprint_path = blueprint_path(U256::zero()).unwrap();
        let exists = blueprint_exists(&host, &blueprint_path).unwrap();
        assert!(exists);

        // Reading the next blueprint should be None, as the delayed hash
        // isn't in the delayed inbox
        let blueprint = read_next_blueprint(&mut host, &mut config)
            .expect("Reading next blueprint should work");
        assert!(blueprint.0.is_none());

        // Next number should be 0, as we didn't read one
        let number = read_next_blueprint_number(&host)
            .expect("Should be able to read next blueprint number");
        assert!(number.is_zero());

        // The blueprint 0 should have been removed
        let exists = blueprint_exists(&host, &blueprint_path).unwrap();
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

        let mut delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should be created");
        // Blueprint should have invalid parent hash
        let validity = parse_and_validate_blueprint::<_, EvmChainConfig>(
            &mut host,
            blueprint_with_hashes_bytes.as_ref(),
            &mut delayed_inbox,
            0,
            false,
            500,
            &EVMBlockHeader {
                hash: GENESIS_PARENT_HASH,
                receipts_root: vec![0; 32],
                transactions_root: vec![0; 32],
            },
            Timestamp::from(0),
        )
        .expect("Should be able to parse blueprint");
        assert_eq!(validity.0, BlueprintValidity::InvalidParentHash);

        // Store blueprint
        store_sequencer_blueprint(&mut host, seq_blueprint)
            .expect("Should be able to store sequencer blueprint");
        // Blueprint 0 should be stored
        let exists = blueprint_exists(&host, &blueprint_path).unwrap();
        assert!(exists);

        // Reading the next blueprint should be None, as the parent hash
        // is invalid
        let blueprint = read_next_blueprint(&mut host, &mut config)
            .expect("Reading next blueprint should work");
        assert!(blueprint.0.is_none());

        // The blueprint 0 should have been removed
        let exists = blueprint_exists(&host, &blueprint_path).unwrap();
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
            hash: TezBlock::genesis_block_hash(),
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
