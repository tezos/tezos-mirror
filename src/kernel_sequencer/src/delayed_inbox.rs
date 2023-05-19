// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::nom::NomReader;
use tezos_data_encoding_derive::BinWriter;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
use tezos_smart_rollup_host::{
    input::Message,
    metadata::{RollupMetadata, RAW_ROLLUP_ADDRESS_SIZE},
    runtime::{Runtime, RuntimeError},
};

use crate::{
    message::{Framed, KernelMessage, Sequence, SequencerMsg, SetSequencer},
    queue::Queue,
    routing::FilterBehavior,
};

/// Message added to the delayed inbox
#[derive(BinWriter)]
pub struct UserMessage {
    timeout_level: u32,
    payload: Vec<u8>,
}

/// Return a message from the inbox
///
/// This function drives the delayed inbox:
///  - add messages to the delayed inbox
///  - process messages from the sequencer
///  - returns message as "normal" message to the user kernel
pub fn read_input<Host: Runtime>(
    host: &mut Host,
    filter_behavior: FilterBehavior,
    timeout_window: u32,
    delayed_inbox_queue: &mut Queue,
) -> Result<Option<Message>, RuntimeError> {
    let RollupMetadata {
        raw_rollup_address, ..
    } = host.reveal_metadata();
    loop {
        let msg = host.read_input()?;
        match msg {
            None => return Ok(None), // No more messages to be processed
            Some(msg) => {
                let payload = msg.as_ref();
                let message = KernelMessage::nom_read(payload);
                match message {
                    Err(_) => {}
                    Ok((_, message)) => match message {
                        KernelMessage::Sequencer(Framed {
                            destination,
                            payload: SequencerMsg::Sequence(sequence),
                        }) => handle_sequence_message(sequence, destination, &raw_rollup_address),
                        KernelMessage::Sequencer(Framed {
                            destination,
                            payload: SequencerMsg::SetSequencer(set_sequence),
                        }) => handle_set_sequencer_message(
                            set_sequence,
                            destination,
                            &raw_rollup_address,
                        ),
                        KernelMessage::DelayedMessage(user_message) => {
                            let _ = handle_message(
                                host,
                                delayed_inbox_queue,
                                timeout_window,
                                user_message,
                                msg.level,
                                filter_behavior,
                                &raw_rollup_address,
                            );
                        }
                    },
                }
            }
        }
    }
}

/// Handle Sequence message
fn handle_sequence_message(
    _sequence: Sequence,
    destination: SmartRollupAddress,
    rollup_address: &[u8; RAW_ROLLUP_ADDRESS_SIZE],
) {
    if destination.hash().as_ref() == rollup_address {
        // process the sequence
    }
}

fn handle_set_sequencer_message(
    _set_sequencer: SetSequencer,
    destination: SmartRollupAddress,
    rollup_address: &[u8; RAW_ROLLUP_ADDRESS_SIZE],
) {
    if destination.hash().as_ref() == rollup_address {
        // process the set sequencer message
    }
}

/// Handle messages
fn handle_message<H: Runtime>(
    host: &mut H,
    queue: &mut Queue,
    timeout_window: u32,
    user_message: Vec<u8>,
    level: u32,
    filter_behavior: FilterBehavior,
    rollup_address: &[u8; RAW_ROLLUP_ADDRESS_SIZE],
) -> Result<(), RuntimeError> {
    // Check if the message should be included in the delayed inbox
    if filter_behavior.predicate(user_message.as_ref(), rollup_address) {
        // add the message to the delayed inbox
        let user_message = UserMessage {
            timeout_level: level + timeout_window,
            payload: user_message,
        };

        queue.add(host, &user_message)?;
    }

    Ok(())
}
