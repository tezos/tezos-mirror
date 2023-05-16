// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::nom::NomReader;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
use tezos_smart_rollup_host::{
    input::Message,
    metadata::{RollupMetadata, RAW_ROLLUP_ADDRESS_SIZE},
    runtime::{Runtime, RuntimeError},
};

use crate::{
    message::{Framed, KernelMessage, Sequence, SequencerMsg, SetSequencer},
    routing::FilterBehavior,
};

/// Return a message from the inbox
///
/// This function drives the delayed inbox:
///  - add messages to the delayed inbox
///  - process messages from the sequencer
///  - returns message as "normal" message to the user kernel
pub fn read_input<Host: Runtime>(
    host: &mut Host,
    filter_behavior: FilterBehavior,
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
                            handle_message(user_message, filter_behavior, &raw_rollup_address)
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
fn handle_message(
    user_message: Vec<u8>,
    filter_behavior: FilterBehavior,
    rollup_address: &[u8; RAW_ROLLUP_ADDRESS_SIZE],
) {
    // Check if the message should be included in the delayed inbox
    if filter_behavior.predicate(user_message.as_ref(), rollup_address) {
        // add the message to the delayed inbox
    }
}
