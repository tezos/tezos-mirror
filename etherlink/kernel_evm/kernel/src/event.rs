// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{storage, upgrade};
use primitive_types::{H256, U256};
use rlp::{Encodable, RlpStream};
use tezos_ethereum::rlp_helpers::append_u256_le;
use tezos_smart_rollup_host::runtime::Runtime;

pub const UPGRADE_TAG: u8 = 0x01;
pub const SEQUENCER_UPGRADE_TAG: u8 = 0x02;
pub const BLUEPRINT_APPLIED_TAG: u8 = 0x03;

#[derive(Debug, PartialEq, Clone)]
pub enum Event {
    Upgrade(upgrade::KernelUpgrade),
    SequencerUpgrade(upgrade::SequencerUpgrade),
    BlueprintApplied { number: U256, hash: H256 },
}

impl Encodable for Event {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(2);
        match self {
            Event::Upgrade(upgrade) => {
                stream.append(&UPGRADE_TAG);
                stream.append(upgrade);
            }
            Event::SequencerUpgrade(sequencer_upgrade) => {
                stream.append(&SEQUENCER_UPGRADE_TAG);
                stream.append(sequencer_upgrade);
            }
            Event::BlueprintApplied { number, hash } => {
                stream.append(&BLUEPRINT_APPLIED_TAG);
                stream.begin_list(2);
                append_u256_le(stream, number);
                stream.append(hash);
            }
        }
    }
}

impl Event {
    pub fn store<Host: Runtime>(&self, host: &mut Host) -> anyhow::Result<()> {
        storage::store_event(host, self)
    }
}
