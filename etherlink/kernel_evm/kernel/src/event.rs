// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{storage, upgrade};
use rlp::{Encodable, RlpStream};
use tezos_smart_rollup_host::runtime::Runtime;

pub const UPGRADE_TAG: u8 = 0x01;

#[derive(Debug, PartialEq, Clone)]
pub enum Event {
    Upgrade(upgrade::KernelUpgrade),
}

impl Encodable for Event {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(2);
        match self {
            Event::Upgrade(upgrade) => {
                stream.append(&UPGRADE_TAG);
                stream.append(upgrade);
            }
        }
    }
}

impl Event {
    pub fn store<Host: Runtime>(&self, host: &mut Host) -> anyhow::Result<()> {
        storage::store_event(host, self)
    }
}
