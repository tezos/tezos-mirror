// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};

use crate::{delayed_inbox::UserMessage, queue::Queue, storage::write_state};

/// Represent the state of the delayed inbox
///
/// The delayed inbox has 2 states:
///  - Sequenced(SmartRollupAddress):
///     the delayed inbox accepts messages from the registered sequencer
///  - Fallback:
///     the kernel will process by itself the messages from the delayed inbox,
///     it's also the default mode
#[derive(Debug, PartialEq, Eq, NomReader, BinWriter, Clone)]
pub enum State {
    Sequenced(PublicKey),
    Fallback,
}

impl Default for State {
    fn default() -> Self {
        State::Fallback
    }
}

