// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![doc = include_str!("../README.md")]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

mod host;
mod state;

extern crate tezos_crypto_rs as crypto;

use crypto::hash::ContractKt1Hash;
use crypto::hash::HashType;
use crypto::hash::SmartRollupHash;
use tezos_data_encoding::enc::BinWriter;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_encoding::inbox;
use tezos_smart_rollup_encoding::michelson::Michelson;
use tezos_smart_rollup_encoding::michelson::MichelsonUnit;
use tezos_smart_rollup_encoding::public_key_hash::PublicKeyHash;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::metadata::RollupMetadata;

use state::HostState;
use std::cell::RefCell;

const MAXIMUM_REBOOTS_PER_INPUT: i32 = 1000;

/// Flag used in the durable storage by the kernel to ask a reboot from the PVM
/// without increasing level.
const REBOOT_FLAG_KEY: &str = "/kernel/env/reboot";

const TOO_MANY_REBOOT_FLAG_KEY: &str = "/readonly/kernel/env/too_many_reboot";

/// The path where the WASM PVM exposes the remaining reboots a kernel can do
/// with a given inbox. Written as i32 (little-endian).
const REBOOT_COUNTER_KEY: &str = "/readonly/kernel/env/reboot_counter";

const NAIROBI_ACTIVATION_LEVEL: u32 = 3_760_129;
const NAIROBI_BLOCK_TIME: i64 = 15;
// Nairobi activated approximately at 0:07AM UTC on June 24th 2023.
const NAIROBI_ACTIVATION_TIMESTAMP: i64 = 1_687_561_630;

pub use state::InMemoryStore;

/// The runtime host when _not_ running in **wasm**.
#[derive(Debug)]
pub struct MockHost {
    state: RefCell<HostState>,
    info: inbox::InfoPerLevel,
}

impl Default for MockHost {
    fn default() -> Self {
        let address = SmartRollupAddress::new(SmartRollupHash(vec![
                0;
                HashType::SmartRollupHash
                    .size()
            ]));

        Self::with_address(&address)
    }
}

/// Specifies the sender & source of a message from an L1-contract.
///
/// Additionally, the target smart rollup is specified - by default
/// referring to the rollup address of the running kernel.
pub struct TransferMetadata {
    sender: ContractKt1Hash,
    source: PublicKeyHash,
    destination: Option<SmartRollupAddress>,
}

impl TransferMetadata {
    /// Create transfer metadata from a smart contract hash & public key hash.
    pub fn new<E1: std::fmt::Debug, E2: std::fmt::Debug>(
        sender: impl TryInto<ContractKt1Hash, Error = E1>,
        source: impl TryInto<PublicKeyHash, Error = E2>,
    ) -> Self {
        Self {
            sender: sender.try_into().unwrap(),
            source: source.try_into().unwrap(),
            destination: None,
        }
    }

    /// Where desired, manually set the target rollup address to be different
    /// to that of the current rollup.
    pub fn override_destination(&mut self, dest: SmartRollupAddress) {
        self.destination = Some(dest);
    }
}

impl MockHost {
    /// Create a new instance of the `MockHost`, specifying the rollup address.
    pub fn with_address(address: &SmartRollupAddress) -> Self {
        let raw_rollup_address = address
            .hash()
            .0
            .as_slice()
            .try_into()
            .expect("Incorrect length for SmartRollupHash");

        let mut state = HostState::default_with_metadata(RollupMetadata {
            raw_rollup_address,
            origination_level: NAIROBI_ACTIVATION_LEVEL,
        });

        // Ensure reboots correctly initialised for testing without using
        // `run_level` API.
        let reboots = MAXIMUM_REBOOTS_PER_INPUT;
        let bytes = reboots.to_le_bytes().to_vec();
        state.store.0.set_value(REBOOT_COUNTER_KEY, bytes);

        state.curr_level = NAIROBI_ACTIVATION_LEVEL;

        let info = info_for_level(state.curr_level as i32);

        let mut host = Self {
            state: state.into(),
            info,
        };

        // Ensure inbox setup correctly
        host.bump_level();

        host
    }

    /// Append an internal message to the current inbox.
    pub fn add_transfer(&mut self, payload: impl Michelson, metadata: &TransferMetadata) {
        let destination = metadata.destination.clone().unwrap_or_else(|| {
            SmartRollupAddress::new(self.state.borrow().get_metadata().address())
        });

        let transfer = inbox::Transfer {
            payload,
            sender: metadata.sender.clone(),
            source: metadata.source.clone(),
            destination,
        };

        let message = inbox::InboxMessage::Internal(
            inbox::InternalInboxMessage::Transfer(transfer),
        );

        let mut inbox_message = Vec::new();
        message
            .serialize(&mut inbox_message)
            .expect("serialization of transfer failed");

        self.as_mut().add_input(inbox_message);
    }

    /// Append an external message to the current inbox.
    pub fn add_external(&mut self, message: impl BinWriter) {
        let mut payload = Vec::new();
        message
            .bin_write(&mut payload)
            .expect("serialization of external payload failed");

        let external_message = inbox::InboxMessage::External::<MichelsonUnit>(&payload);

        self.add_inbox_message(external_message);
    }

    /// Make a preimage available to the _reveal_data_ channel.
    pub fn set_preimage(&mut self, preimage: Vec<u8>) -> [u8; PREIMAGE_HASH_SIZE] {
        self.as_mut().set_preimage(preimage)
    }

    /// Runs `kernel_run` against the current level's inbox.
    ///
    /// - Includes the `StartOfLevel`, `InfoPerLevel` & `EndOfLevel` messages.
    /// - Returns the level the kernel was run at.
    pub fn run_level(&mut self, kernel_run: fn(&mut Self)) -> u32 {
        self.finalise_inputs();

        let mut reboots = MAXIMUM_REBOOTS_PER_INPUT;

        loop {
            let bytes = reboots.to_le_bytes().to_vec();
            self.as_mut().store.0.set_value(REBOOT_COUNTER_KEY, bytes);

            kernel_run(self);
            self.as_mut().store.0.node_delete(TOO_MANY_REBOOT_FLAG_KEY);

            reboots -= 1;

            let reboot_requested = self
                .as_mut()
                .store
                .0
                .maybe_get_value(REBOOT_FLAG_KEY)
                .is_some();

            if reboot_requested {
                self.as_mut().store.0.node_delete(REBOOT_FLAG_KEY);

                if reboots > 0 {
                    continue;
                }

                self.as_mut()
                    .store
                    .0
                    .set_value(TOO_MANY_REBOOT_FLAG_KEY, vec![]);
            }
            break;
        }

        let level_ran_at = self.state.borrow().curr_level;
        self.bump_level();

        level_ran_at
    }

    /// Returns the level of the next `kernel_run`.
    pub fn level(&self) -> u32 {
        self.state.borrow().curr_level
    }

    /// Returns the `InfoPerLevel` of the next `kernel_run`.
    pub fn info_per_level(&self) -> &inbox::InfoPerLevel {
        &self.info
    }

    /// Show the outbox at the given level
    pub fn outbox_at(&self, level: u32) -> Vec<Vec<u8>> {
        self.state.borrow().store.0.outbox_at(level).to_vec()
    }

    fn bump_level(&mut self) {
        let state = self.as_mut();
        state.curr_level += 1;
        state.curr_input_id = 0;
        state.input.truncate(0);
        let curr_info = info_for_level(state.curr_level as i32);

        let sol = inbox::InboxMessage::<MichelsonUnit>::Internal(
            inbox::InternalInboxMessage::StartOfLevel,
        );

        self.add_inbox_message(sol);

        let info = inbox::InboxMessage::<MichelsonUnit>::Internal(
            inbox::InternalInboxMessage::InfoPerLevel(curr_info.clone()),
        );

        self.add_inbox_message(info);
        self.info = curr_info;
    }

    fn finalise_inputs(&mut self) {
        let eol = inbox::InboxMessage::<MichelsonUnit>::Internal(
            inbox::InternalInboxMessage::EndOfLevel,
        );

        self.add_inbox_message(eol);
    }

    fn add_inbox_message<Expr: Michelson>(&mut self, message: inbox::InboxMessage<Expr>) {
        let mut inbox_message = Vec::new();
        message
            .serialize(&mut inbox_message)
            .expect("serialization of message failed");

        self.as_mut().add_input(inbox_message);
    }
}

fn info_for_level(level: i32) -> inbox::InfoPerLevel {
    let timestamp = (level as i64 - 1 - (NAIROBI_ACTIVATION_LEVEL as i64))
        * NAIROBI_BLOCK_TIME
        + NAIROBI_ACTIVATION_TIMESTAMP;

    let hash = crypto::blake2b::digest_256(&timestamp.to_le_bytes()).unwrap();

    inbox::InfoPerLevel {
        predecessor: crypto::hash::BlockHash(hash),
        predecessor_timestamp: Timestamp::from(timestamp),
    }
}
