// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Utilities for constructing inboxes & dealing with the `inbox.json` file,
//! used as inputs for both the *wasm debugger* and *RISC-V sandbox*.

pub mod file;

use self::file::{InboxFile, Message};
use std::{collections::LinkedList, error::Error, path::Path};
use tezos_crypto_rs::hash::{BlockHash, ContractKt1Hash};
use tezos_smart_rollup_encoding::{
    inbox::{InboxMessage, InfoPerLevel, InternalInboxMessage, Transfer},
    michelson,
    public_key_hash::PublicKeyHash,
    smart_rollup::SmartRollupAddress,
    timestamp::Timestamp,
};

/// Inbox builder
pub struct InboxBuilder {
    levels: LinkedList<LinkedList<Vec<u8>>>,
}

impl InboxBuilder {
    /// Construct a new inbox builder.
    pub fn new() -> Self {
        let mut builder = Self {
            levels: LinkedList::from([LinkedList::new()]),
        };
        builder.start_level();
        builder
    }

    /// Load inbox messages from a JSON file.
    pub fn load_from_file(
        &mut self,
        path: impl AsRef<Path>,
    ) -> Result<&mut Self, Box<dyn Error>> {
        let inbox_messages = InboxFile::load(path.as_ref()).map_err(|err| {
            format!(
                "Failed to read inbox from {}: {err}",
                path.as_ref().display()
            )
        })?;
        self.add_inbox_messages(inbox_messages);
        Ok(self)
    }

    /// Load inbox messages from a pre-loaded [`InboxFile`].
    pub fn add_inbox_messages(&mut self, inbox_messages: InboxFile) -> &mut Self {
        for (idx, level) in inbox_messages.0.into_iter().enumerate() {
            if idx > 0 {
                self.next_level();
            }

            for message in level {
                match message {
                    Message::Raw(raw) => self.insert_raw(raw),
                    Message::External { external } => self.insert_external(external),
                };
            }
        }
        self
    }

    /// Inject the `start of level` and `info per level` messages to indicate
    /// the beginning of a level.
    fn start_level(&mut self) -> &mut Self {
        let sol: InboxMessage<michelson::MichelsonUnit> =
            InboxMessage::Internal(InternalInboxMessage::StartOfLevel);

        let mut data = Vec::new();
        sol.serialize(&mut data)
            .expect("Failed to serialise StartOfLevel");
        self.insert_raw(data);

        let iol: InboxMessage<michelson::MichelsonUnit> =
            InboxMessage::Internal(InternalInboxMessage::InfoPerLevel(InfoPerLevel {
                predecessor_timestamp: Timestamp::from(0i64),
                predecessor: BlockHash::from_base58_check(
                    "BLkJzts53kqEzvDmomAEAmnPuiZjYeLpNZemPbnGJpndkJePE7L",
                )
                .expect("Invalid predecessor block hash"),
            }));

        let mut data = Vec::new();
        iol.serialize(&mut data)
            .expect("Failed to serialise InfoPerLevel");

        self.insert_raw(data);
        self
    }

    /// Add the `end of level` message to finalise a level.
    fn end_level(&mut self) -> &mut Self {
        let eol: InboxMessage<michelson::MichelsonUnit> =
            InboxMessage::Internal(InternalInboxMessage::EndOfLevel);

        let mut data = Vec::new();
        eol.serialize(&mut data)
            .expect("Failed to serialise EndOfLevel data");

        self.insert_raw(data);
        self
    }

    /// Finalise the current level and start a new one.
    pub fn next_level(&mut self) -> &mut Self {
        self.end_level();
        self.levels.push_back(LinkedList::new());
        self.start_level();
        self
    }

    /// Inject an external message.
    pub fn insert_external(&mut self, data: impl AsRef<[u8]>) -> &mut Self {
        let msg: InboxMessage<michelson::MichelsonUnit> =
            InboxMessage::External(data.as_ref());
        let mut data = Vec::new();
        msg.serialize(&mut data)
            .expect("Failed to serialise external message");

        self.insert_raw(data);
        self
    }

    /// Inject a transfer notification.
    pub fn insert_transfer<E: michelson::Michelson>(
        &mut self,
        sender: ContractKt1Hash,
        source: PublicKeyHash,
        destination: SmartRollupAddress,
        payload: E,
    ) -> &mut Self {
        let msg = InboxMessage::Internal(InternalInboxMessage::Transfer(Transfer {
            payload,
            sender,
            source,
            destination,
        }));

        let mut data = Vec::new();
        msg.serialize(&mut data)
            .expect("Failed to serialise transfer");

        self.insert_raw(data)
    }

    /// Inject a raw message.
    fn insert_raw(&mut self, data: Vec<u8>) -> &mut Self {
        self.levels
            .back_mut()
            .expect("Inbox list should be non-empty upon construction of InboxBuilder")
            .push_back(data);
        self
    }

    /// Build the inbox.
    pub fn build(mut self) -> Inbox {
        // Only when we have started a level do we want to finalise it.
        if !self.levels.is_empty() {
            self.end_level();
        }

        Inbox {
            current_level: 0,
            current_id: 0,
            levels: self.levels,
        }
    }
}

impl Default for InboxBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Inbox
#[derive(Clone)]
pub struct Inbox {
    current_level: u32,
    current_id: u32,
    levels: LinkedList<LinkedList<Vec<u8>>>,
}

impl Iterator for Inbox {
    type Item = (u32, u32, Vec<u8>);

    fn next(&mut self) -> Option<Self::Item> {
        let level = self.levels.front_mut()?;

        match level.pop_front() {
            // An inbox message is available for this level
            Some(data) => {
                let info = (self.current_level, self.current_id, data);
                self.current_id += 1;
                Some(info)
            }

            // No more messages at this level
            None => {
                self.levels.pop_front();
                self.current_level += 1;
                self.current_id = 0;
                self.next()
            }
        }
    }
}
