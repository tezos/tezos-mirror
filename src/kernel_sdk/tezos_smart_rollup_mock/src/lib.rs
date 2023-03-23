// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Mock runtime provides a host that can used in integration & unit tests.
//!
//! To do so, [`MockHost`] implements the [`SmartRollupCore`] trait - allowing
//! it to be passed as an argument to any function, where the argument is
//! required to implement either `SmartRollupCore` or `Runtime`.
//!
//! # Example
//!
//! Take a simple kernel that will count the number of times it is called, rebooting
//! at most 500 times per level.
//!
//! ```
//! use tezos_smart_rollup_mock::MockHost;
//! use tezos_smart_rollup_host::runtime::Runtime;
//! use tezos_smart_rollup_host::path::RefPath;
//!
//! const COUNTER_PATH: RefPath = RefPath::assert_from(b"/counter");
//! const COUNTER_SIZE: usize = core::mem::size_of::<u32>();
//!
//! // Kernel entrypoint, to count the number of times called.
//! // After 1000 calls at a given level, will restart at the
//! // next level.
//! fn count_calls<Host: Runtime>(host: &mut Host) {
//!   let mut counter = read_counter(host);
//!
//!   counter += 1;
//!
//!   // A kernel can reboot up to 1000 times per level.
//!   if counter % 1000 != 0 {
//!     host.mark_for_reboot().expect("Marking for reboot failed.");
//!   }
//!
//!   host.store_write(&COUNTER_PATH, &counter.to_le_bytes(), 0)
//!       .expect("Failed to write counter to storage.")
//! }
//!
//! // Reads `COUNTER_PATH` as `u32` in little-endian.
//! // - returns 0 if counter does not exist.
//! fn read_counter(host: &impl Runtime) -> u32 {
//!   host.store_read(&COUNTER_PATH, 0, COUNTER_SIZE)
//!       .map(|bytes| bytes.try_into().unwrap_or_default())
//!       .map(u32::from_le_bytes)
//!       .unwrap_or_default()
//! }
//!
//! // Run using mock host - every `run_level` should increment
//! // counter by 500.
//! let mut host = MockHost::default();
//! assert_eq!(0, read_counter(&host));
//!
//! host.run_level(count_calls);
//! assert_eq!(1000, read_counter(&host));
//!
//! host.run_level(count_calls);
//! assert_eq!(2000, read_counter(&host));
//! ```
//!
//! [`SmartRollupCore`]: tezos_smart_rollup_core::smart_rollup_core::SmartRollupCore

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

const MUMBAI_ACTIVATION_LEVEL: u32 = 3_268_609;
const MUMBAI_BLOCK_TIME: i64 = 15;
// Mumbai activates approximately at 2:24PM UTC on March 29th 2023.
const MUMBAI_ACTIVATION_TIMESTAMP: i64 = 1_680_096_240;

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
            origination_level: MUMBAI_ACTIVATION_LEVEL,
        });

        state.curr_level = MUMBAI_ACTIVATION_LEVEL;

        let info = info_for_level(state.curr_level as i32);

        Self {
            state: state.into(),
            info,
        }
    }

    /// Append an internal message to the current inbox.
    pub fn add_transfer(&mut self, payload: impl Michelson, metadata: &TransferMetadata) {
        let destination = metadata.destination.clone().unwrap_or_else(|| {
            SmartRollupAddress::new(self.state.borrow().get_metadata().address().unwrap())
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
            self.as_mut().store.set_value(REBOOT_COUNTER_KEY, bytes);

            kernel_run(self);
            self.as_mut().store.node_delete(TOO_MANY_REBOOT_FLAG_KEY);

            reboots -= 1;

            let reboot_requested = self
                .as_mut()
                .store
                .maybe_get_value(REBOOT_FLAG_KEY)
                .is_some();

            if reboot_requested {
                self.as_mut().store.node_delete(REBOOT_FLAG_KEY);

                if reboots > 0 {
                    continue;
                }

                self.as_mut()
                    .store
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
        self.state.borrow().store.outbox_at(level).to_vec()
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
    let timestamp = (level as i64 - 1 - (MUMBAI_ACTIVATION_LEVEL as i64))
        * MUMBAI_BLOCK_TIME
        + MUMBAI_ACTIVATION_TIMESTAMP;

    let hash = crypto::blake2b::digest_256(&timestamp.to_le_bytes()).unwrap();

    inbox::InfoPerLevel {
        predecessor: crypto::hash::BlockHash(hash),
        predecessor_timestamp: Timestamp::from(timestamp),
    }
}
