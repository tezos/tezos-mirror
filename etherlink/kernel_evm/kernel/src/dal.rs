// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::parsing::parse_unsigned_blueprint_chunk;
use crate::sequencer_blueprint::UnsignedSequencerBlueprint;
use rlp::{DecoderError, PayloadInfo};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_host::dal_parameters::RollupDalParameters;

use tezos_smart_rollup_host::runtime::Runtime;

const TAG_SIZE: usize = 1;

const DAL_BLUEPRINT_INPUT_TAG: u8 = 1;

const DAL_PADDING_TAG: u8 = 0;

enum ParsedInput {
    UnsignedSequencerBlueprint(UnsignedSequencerBlueprint),
    Padding,
}

// Import all the pages of a DAL slot and concatenate them.
fn import_dal_slot<Host: Runtime>(
    host: &mut Host,
    params: &RollupDalParameters,
    published_level: u32,
    slot_index: u8,
) -> Option<Vec<u8>> {
    // From the protocol perspective the levels are encoded in [0; 2^31[, as
    // such any levels above would be invalid and the rollup node will return
    // the empty preimage.
    if published_level > i32::MAX as u32 {
        return None;
    }
    let page_size = params.page_size as usize;
    let slot_size = params.slot_size as usize;
    let mut slot: Vec<u8> = vec![0u8; slot_size];
    let number_of_pages = (params.slot_size / params.page_size) as i16;
    let mut page_start = 0usize;
    for page_index in 0..number_of_pages {
        let imported_page_len = host
            .reveal_dal_page(
                published_level as i32,
                slot_index,
                page_index,
                &mut slot[page_start..page_start + page_size],
            )
            .unwrap_or(0);
        if imported_page_len == page_size {
            page_start += imported_page_len
        } else {
            return None;
        }
    }
    Some(slot)
}

// data is assumed to be one RLP object followed by some padding.
// this function returns the length of the RLP object, including its
// length prefix
fn rlp_length(data: &[u8]) -> Result<usize, DecoderError> {
    let PayloadInfo {
        header_len,
        value_len,
    } = PayloadInfo::from(data)?;
    Result::Ok(header_len + value_len)
}

fn parse_unsigned_sequencer_blueprint(bytes: &[u8]) -> (Option<ParsedInput>, usize) {
    if let Result::Ok(chunk_length) = rlp_length(bytes) {
        let unsigned_chunk = parse_unsigned_blueprint_chunk(&bytes[..chunk_length]);
        (
            unsigned_chunk.map(ParsedInput::UnsignedSequencerBlueprint),
            chunk_length + TAG_SIZE,
        )
    } else {
        (None, TAG_SIZE)
    }
}

fn parse_input(bytes: &[u8]) -> (Option<ParsedInput>, usize) {
    // The expected format is:

    // blueprint tag (1 byte) / blueprint chunk (variable)

    match bytes[0] {
        DAL_PADDING_TAG => (Some(ParsedInput::Padding), TAG_SIZE),
        DAL_BLUEPRINT_INPUT_TAG => {
            let bytes = &bytes[TAG_SIZE..];
            parse_unsigned_sequencer_blueprint(bytes)
        }
        _ => (None, TAG_SIZE), // Tag is invalid, let's yield and give the responsibility to the
                               // caller to continue reading.
    }
}

fn parse_slot(slot: &[u8]) -> Vec<UnsignedSequencerBlueprint> {
    // The format of a dal slot is
    // tagged chunk | tagged chunk | .. | padding
    //
    // DAL slots are padded with zeros to have a constant size. We read chunks
    // until reading `\x00` which is considered as `end of list`.

    let mut buffer = Vec::new();
    let mut offset = 0;

    // Invariant: at each loop, at least one byte has been read. Once
    // `end_of_list` has been reached or the slot has been fully read, the
    // buffer is returned.
    loop {
        // Checked at the beginning of the loop: if the slot is empty, it
        // returns directly and avoids reading the tag outside of the bounds of the slot.
        if offset >= slot.len() {
            return buffer;
        };
        let (next_input, length) = parse_input(&slot[offset..]);

        match next_input {
            None => return buffer, // Once an unparsable input has been read,
            // stop reading and return the list of chunks read.
            Some(ParsedInput::UnsignedSequencerBlueprint(b)) => buffer.push(b),
            Some(ParsedInput::Padding) => return buffer,
        }

        // The offset is incremented by the number of bytes read, and the
        // function returns if all the bytes have been read already.
        offset += length;
    }
}

pub fn fetch_and_parse_sequencer_blueprint_from_dal<Host: Runtime>(
    host: &mut Host,
    params: &RollupDalParameters,
    slot_index: u8,
    published_level: u32,
) -> Option<Vec<UnsignedSequencerBlueprint>> {
    if let Some(slot) = import_dal_slot(host, params, published_level, slot_index) {
        log!(
            host,
            Debug,
            "DAL slot at level {} and index {} successfully imported",
            published_level,
            slot_index
        );

        // DAL slots are padded with zeros to have a constant
        // size, we need to remove this padding before parsing the
        // slot as a blueprint chunk.

        let chunks = parse_slot(&slot);
        log!(
            host,
            Debug,
            "DAL slot successfully parsed as {} unsigned blueprint chunks",
            chunks.len()
        );
        Some(chunks)
    } else {
        log!(
            host,
            Debug,
            "Slot {} at level {} is invalid",
            slot_index,
            published_level
        );
        None
    }
}
