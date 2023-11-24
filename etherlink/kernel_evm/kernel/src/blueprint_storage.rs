// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::blueprint::Blueprint;
use crate::error::{Error, StorageError};
use crate::sequencer_blueprint::SequencerBlueprint;
use crate::storage::{
    read_current_block_number, read_rlp, store_read_slice, store_rlp, write_u256,
};
use primitive_types::U256;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::rlp_helpers;
use tezos_smart_rollup_host::path::*;
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};

const EVM_BLUEPRINTS: RefPath = RefPath::assert_from(b"/blueprints");

/// Number of the last (as in, with larger number) blueprint stored.
/// This is only used for proxy mode, where the kernel guarantees that
/// blueprints are added incrementally.
const EVM_LAST_BLUEPRINT: RefPath = RefPath::assert_from(b"/blueprints/last");

/// The store representation of a blueprint.
/// It's designed to support storing sequencer blueprints,
/// which can be chunked, and blueprints constructed from
/// inbox messages. Note that the latter are only to be
/// used when the kernel isn't running with a sequencer.
#[derive(PartialEq, Debug, Clone)]
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

fn read_u256_default<Host: Runtime>(
    host: &Host,
    path: OwnedPath,
    default: U256,
) -> Result<U256, Error> {
    let mut buffer = [0_u8; 8];
    match store_read_slice(host, &path, &mut buffer, 8) {
        Ok(()) => Ok(U256::from_little_endian(&buffer)),
        Err(
            Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))
            | Error::Storage(StorageError::Runtime(RuntimeError::HostErr(
                tezos_smart_rollup_host::Error::StoreNotAValue,
            )))
            | Error::Storage(StorageError::Runtime(RuntimeError::HostErr(
                tezos_smart_rollup_host::Error::StoreInvalidAccess,
            ))),
            // An InvalidAccess implies that the path does not exist at all
            // in the storage: store_read fails because reading is out of
            // bounds since the value has never been allocated before
        ) => Ok(default),
        Err(e) => Err(e),
    }
}

fn read_last_blueprint_number<Host: Runtime>(host: &Host) -> Result<U256, Error> {
    read_u256_default(host, EVM_LAST_BLUEPRINT.into(), U256::max_value())
}

fn store_last_blueprint_number<Host: Runtime>(
    host: &mut Host,
    number: U256,
) -> Result<(), Error> {
    write_u256(host, &EVM_LAST_BLUEPRINT.into(), number)
}

fn blueprint_path(number: U256) -> Result<OwnedPath, StorageError> {
    let number_as_path: Vec<u8> = format!("/{}", number).into();
    // The key being an integer value, it will always be valid as a path,
    // `assert_from` cannot fail.
    let number_subkey = RefPath::assert_from(&number_as_path);
    concat(&EVM_BLUEPRINTS, &number_subkey).map_err(StorageError::from)
}

pub fn store_sequencer_blueprint<Host: Runtime>(
    host: &mut Host,
    blueprint: SequencerBlueprint,
    number: U256,
) -> Result<(), Error> {
    // Note: when the chunking mechanism is implemented, the sequencer
    // blueprint will have additional information (nb of chunks, chunk
    // index) that will be used to enrich the blueprint path.
    let blueprint_path = blueprint_path(number)?;
    let store_blueprint = StoreBlueprint::SequencerChunk(blueprint.chunk);
    store_rlp(&store_blueprint, host, &blueprint_path)
}

pub fn store_inbox_blueprint<Host: Runtime>(
    host: &mut Host,
    blueprint: Blueprint,
) -> Result<(), Error> {
    let number = read_last_blueprint_number(host)?;
    // We overflow, as the default is the max U256
    let (number, _) = number.overflowing_add(U256::one());
    store_last_blueprint_number(host, number)?;
    let blueprint_path = blueprint_path(number)?;
    let store_blueprint = StoreBlueprint::InboxBlueprint(blueprint);
    store_rlp(&store_blueprint, host, &blueprint_path)
}

fn read_next_blueprint_number<Host: Runtime>(host: &Host) -> Result<U256, Error> {
    match read_current_block_number(host) {
        Ok(number) => Ok(number.saturating_add(U256::one())),
        Err(_) => Ok(U256::zero()),
    }
}

#[allow(dead_code)]
pub fn read_next_blueprint<Host: Runtime>(
    host: &Host,
) -> Result<Option<Blueprint>, Error> {
    let number = read_next_blueprint_number(host)?;
    let path = blueprint_path(number)?;
    let exists = host.store_has(&path)?.is_some();
    if exists {
        // Note: this is where the de-chunking/parsing of sequencer
        // blueprints will be implemented.
        // For now, we assume the bytes correspond to the only chunk.
        let stored_blueprint: StoreBlueprint = read_rlp(host, &path)?;
        match stored_blueprint {
            StoreBlueprint::InboxBlueprint(blueprint) => Ok(Some(blueprint)),
            StoreBlueprint::SequencerChunk(chunk) => {
                let blueprint = rlp::decode(&chunk)?;
                Ok(Some(blueprint))
            }
        }
    } else {
        Ok(None)
    }
}

// Removes from the storage the blueprint at index CURRENT_BLOCK_NUMBER
#[allow(dead_code)]
pub fn drop_head_blueprint<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let number = read_current_block_number(host)?;
    let path = blueprint_path(number)?;
    host.store_delete(&path).map_err(Error::from)
}
