// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use rlp::{Decodable, DecoderError, Encodable};
use tezos_crypto_rs::public_key::PublicKey;
use tezos_ethereum::rlp_helpers::{
    append_public_key, append_timestamp, decode_public_key, decode_timestamp, next,
};
use tezos_evm_logging::{log, Level::Info};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::OwnedPath;

use revm::primitives::U256;

use crate::{
    helpers::storage::{read_u256_le_default, write_u256_le},
    storage::world_state_handler::{
        SEQUENCER_KEY_CHANGE_COUNTER_PATH, SEQUENCER_KEY_CHANGE_PATH,
    },
    Error,
};

#[derive(Debug, Eq, PartialEq)]
pub struct SequencerKeyChange {
    sequencer_key: PublicKey,
    activation_timestamp: Timestamp,
}

impl Encodable for SequencerKeyChange {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.begin_list(2);
        append_public_key(s, &self.sequencer_key);
        append_timestamp(s, self.activation_timestamp);
    }
}

impl Decodable for SequencerKeyChange {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !rlp.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }

        let mut iter = rlp.iter();
        let sequencer_key = decode_public_key(&next(&mut iter)?)?;
        let activation_timestamp = decode_timestamp(&next(&mut iter)?)?;
        Ok(SequencerKeyChange {
            sequencer_key,
            activation_timestamp,
        })
    }
}

impl SequencerKeyChange {
    pub fn new(sequencer_key: PublicKey, activation_timestamp: Timestamp) -> Self {
        SequencerKeyChange {
            sequencer_key,
            activation_timestamp,
        }
    }

    pub fn activation_timestamp(&self) -> Timestamp {
        self.activation_timestamp
    }

    pub fn sequencer_key(&self) -> &PublicKey {
        &self.sequencer_key
    }
}

impl std::fmt::Display for SequencerKeyChange {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "sequencer: {}, activation_timestamp: {}",
            self.sequencer_key, self.activation_timestamp
        )
    }
}

pub fn store_sequencer_key_change<Host: Runtime>(
    host: &mut Host,
    sequencer_key_change: SequencerKeyChange,
) -> Result<(), Error> {
    log!(
        host,
        Info,
        "An L2 based sequencer key change is planned: {}",
        sequencer_key_change
    );
    let bytes = &sequencer_key_change.rlp_bytes();
    let path = OwnedPath::from(SEQUENCER_KEY_CHANGE_PATH);
    host.store_write_all(&path, bytes)?;
    Ok(())
}

/// Reads the sequencer key change counter, defaulting to zero when no
/// change has happened yet. This is the counter value the *next* change
/// must be signed against.
pub fn read_sequencer_change_counter<Host: Runtime>(host: &Host) -> Result<U256, Error> {
    Ok(read_u256_le_default(
        host,
        &SEQUENCER_KEY_CHANGE_COUNTER_PATH,
        U256::ZERO,
    )?)
}

/// Writes the sequencer key change counter to durable storage.
pub fn write_sequencer_change_counter<Host: Runtime>(
    host: &mut Host,
    value: U256,
) -> Result<(), Error> {
    write_u256_le(host, &SEQUENCER_KEY_CHANGE_COUNTER_PATH, value)?;
    Ok(())
}

/// Increments the sequencer key change counter, so that the calldata of past
/// changes cannot be replayed. Signed (precompile) changes bump at store-time
/// — as soon as the change is stored, invalidating the captured signature —
/// through the layered state (see `LayeredState::store_sequencer_key_change`).
/// This helper is used by unsigned governance changes, which carry no
/// replayable signature and bump at apply-time. Either way a single change
/// advances the counter by exactly one.
pub fn increment_sequencer_change_counter<Host: Runtime>(
    host: &mut Host,
) -> Result<(), Error> {
    let next = read_sequencer_change_counter(host)?.saturating_add(U256::ONE);
    write_sequencer_change_counter(host, next)
}
