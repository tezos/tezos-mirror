// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::parsing::{
    Input::ModeSpecific, InputResult::Input, SequencerInput, SequencerParsingContext,
};
use rlp::{DecoderError, PayloadInfo};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_host::dal_parameters::RollupDalParameters;
use tezos_smart_rollup_host::metadata::RAW_ROLLUP_ADDRESS_SIZE;

use tezos_smart_rollup_host::runtime::Runtime;

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

pub fn fetch_and_parse_sequencer_blueprint_from_dal<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; RAW_ROLLUP_ADDRESS_SIZE],
    parsing_context: &mut SequencerParsingContext,
    params: &RollupDalParameters,
    slot_index: u8,
    published_level: u32,
) -> Option<SequencerInput> {
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

        // The expected format is:

        // 0 (1B) / rollup_address (RAW_ROLLUP_ADDRESS_SIZE B) / blueprint tag (1B) / blueprint chunk (variable) / padding

        // To remove the padding we need to measure the length of
        // the RLP-encoded blueprint chunk which starts at
        // position 2 + RAW_ROLLUP_ADDRESS_SIZE
        if let Result::Ok(chunk_length) = rlp_length(&slot[2 + RAW_ROLLUP_ADDRESS_SIZE..])
        {
            // Padding removal
            let slot = &slot[0..2 + RAW_ROLLUP_ADDRESS_SIZE + chunk_length];
            let res = crate::parsing::InputResult::parse_external(
                slot,
                &smart_rollup_address,
                parsing_context,
            );
            if let Input(ModeSpecific(chunk)) = res {
                log!(
                    host,
                    Info,
                    "DAL slot successfully parsed as a blueprint chunk"
                );
                Some(chunk)
            } else {
                log!(
                    host,
                    Debug,
                    "Slot {} at level {} contains an invalid chunk",
                    slot_index,
                    published_level
                );
                None
            }
        } else {
            log!(
                host,
                Debug,
                "Slot {} at level {} is illformed",
                slot_index,
                published_level
            );
            None
        }
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
