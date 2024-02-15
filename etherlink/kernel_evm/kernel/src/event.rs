// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{storage, upgrade};
use rlp::{Encodable, RlpStream};
use tezos_smart_rollup_host::runtime::Runtime;

pub const UPGRADE_TAG: u8 = 0x01;
pub const SEQUENCER_UPGRADE_TAG: u8 = 0x02;

#[derive(Debug, PartialEq, Clone)]
pub enum Event {
    Upgrade(upgrade::KernelUpgrade),
    SequencerUpgrade(upgrade::SequencerUpgrade),
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
        }
    }
}

impl Event {
    pub fn store<Host: Runtime>(&self, host: &mut Host) -> anyhow::Result<()> {
        storage::store_event(host, self)
    }
}
