// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    blueprint_storage::{BlockHeader, BlueprintHeader, ChainHeader},
    storage,
    transaction::Transaction,
    upgrade,
};
use primitive_types::{H256, U256};
use rlp::{Encodable, RlpStream};
use tezos_ethereum::rlp_helpers::{append_timestamp, append_u256_le};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::timestamp::Timestamp;

pub const UPGRADE_TAG: u8 = 0x01;
pub const SEQUENCER_UPGRADE_TAG: u8 = 0x02;
pub const BLUEPRINT_APPLIED_TAG: u8 = 0x03;
pub const NEW_DELAYED_TRANSACTION_TAG: u8 = 0x04;
pub const FLUSH_DELAYED_INBOX: u8 = 0x05;

#[derive(Debug, PartialEq, Clone)]
pub enum Event<'a> {
    Upgrade(upgrade::KernelUpgrade),
    SequencerUpgrade(upgrade::SequencerUpgrade),
    BlueprintApplied {
        number: U256,
        hash: H256,
    },
    NewDelayedTransaction(Box<Transaction>),
    FlushDelayedInbox {
        transactions: &'a Vec<Transaction>,
        timestamp: Timestamp,
        level: U256,
    },
}

impl Encodable for Event<'_> {
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
            Event::NewDelayedTransaction(txn) => {
                stream.append(&NEW_DELAYED_TRANSACTION_TAG);
                stream.append(txn);
            }
            Event::FlushDelayedInbox {
                transactions,
                timestamp,
                level,
            } => {
                stream.append(&FLUSH_DELAYED_INBOX);
                stream.begin_list(3);
                stream.append_list(transactions);
                append_timestamp(stream, *timestamp);
                stream.append(level);
            }
        }
    }
}

impl Event<'_> {
    pub fn store<Host: Runtime>(&self, host: &mut Host) -> anyhow::Result<()> {
        storage::store_event(host, self)
    }

    pub fn blueprint_applied(block: BlockHeader<ChainHeader>) -> Self {
        let BlockHeader {
            blueprint_header:
                BlueprintHeader {
                    number,
                    timestamp: _,
                },
            chain_header,
        } = block;
        match chain_header {
            ChainHeader::Eth(evm_block_header) => Self::BlueprintApplied {
                number,
                hash: evm_block_header.hash,
            },
            ChainHeader::Tez(tez_block_header) => Self::BlueprintApplied {
                number,
                hash: tez_block_header.hash,
            },
        }
    }
}
