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

use crate::{storage::world_state_handler::SEQUENCER_KEY_CHANGE_PATH, Error};

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
