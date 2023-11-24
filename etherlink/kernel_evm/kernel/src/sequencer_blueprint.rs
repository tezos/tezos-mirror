// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::inbox::Transaction;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::rlp_helpers::FromRlpBytes;
use tezos_ethereum::rlp_helpers::{self, append_timestamp, decode_timestamp};
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::runtime::Runtime;

#[derive(PartialEq, Debug, Clone)]
pub struct SequencerBlueprint {
    pub timestamp: Timestamp,
    pub transactions: Vec<Transaction>,
}

impl Encodable for SequencerBlueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append_list(&self.transactions);
        append_timestamp(stream, self.timestamp);
    }
}

impl Decodable for SequencerBlueprint {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let transactions =
            rlp_helpers::decode_list(&rlp_helpers::next(&mut it)?, "transactions")?;
        let timestamp = decode_timestamp(&rlp_helpers::next(&mut it)?)?;
        Ok(Self {
            transactions,
            timestamp,
        })
    }
}

// This is a work in progress, the logic is bound to change
pub fn fetch<Host: Runtime>(host: &mut Host) -> anyhow::Result<Vec<SequencerBlueprint>> {
    let _sol = host.read_input()?;
    let _ipl = host.read_input()?;
    let message = host.read_input()?;
    let eol = host.read_input()?;
    match (message, eol) {
        (Some(message), Some(_)) => {
            let bytes = message.as_ref().get(1..).unwrap();
            let seq_blueprint = FromRlpBytes::from_rlp_bytes(bytes)?;
            Ok(vec![seq_blueprint])
        }
        _ => Ok(vec![]),
    }
}
