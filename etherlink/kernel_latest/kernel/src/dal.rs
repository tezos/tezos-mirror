// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::parsing::{parse_unsigned_blueprint_chunk, SequencerBlueprintRes};
use crate::sequencer_blueprint::UnsignedSequencerBlueprint;
use primitive_types::U256;
use rlp::{DecoderError, PayloadInfo};
use tezos_evm_logging::{log, Level::*};

use tezos_evm_runtime::runtime::Runtime;

const TAG_SIZE: usize = 1;

const DAL_BLUEPRINT_INPUT_TAG: u8 = 1;

const DAL_PADDING_TAG: u8 = 0;

enum ParsedInput {
    UnsignedSequencerBlueprint(UnsignedSequencerBlueprint),
    InvalidInput,
    Padding,
}

// Import all the pages of a DAL slot and concatenate them.
fn import_dal_slot<Host: Runtime>(
    host: &mut Host,
    slot_size: u64,
    page_size: u64,
    published_level: u32,
    slot_index: u8,
) -> Option<Vec<u8>> {
    // From the protocol perspective the levels are encoded in [0; 2^31[, as
    // such any levels above would be invalid and the rollup node will return
    // the empty preimage.
    if published_level > i32::MAX as u32 {
        return None;
    }
    let page_size_usize = page_size as usize;
    let slot_size_usize = slot_size as usize;
    let mut slot: Vec<u8> = vec![0u8; slot_size_usize];
    let number_of_pages = (slot_size / page_size) as i16;
    let mut page_start = 0usize;
    for page_index in 0..number_of_pages {
        let imported_page_len = host
            .reveal_dal_page(
                published_level as i32,
                slot_index,
                page_index,
                &mut slot[page_start..page_start + page_size_usize],
            )
            .unwrap_or(0);
        if imported_page_len == page_size_usize {
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

fn parse_unsigned_sequencer_blueprint<Host: Runtime>(
    host: &mut Host,
    bytes: &[u8],
    next_blueprint_number: &U256,
) -> (Option<ParsedInput>, usize) {
    if let Result::Ok(chunk_length) = rlp_length(bytes) {
        match parse_unsigned_blueprint_chunk(
            &bytes[..chunk_length],
            next_blueprint_number,
        ) {
            SequencerBlueprintRes::SequencerBlueprint(unsigned_chunk) => (
                Some(ParsedInput::UnsignedSequencerBlueprint(unsigned_chunk)),
                chunk_length + TAG_SIZE,
            ),
            SequencerBlueprintRes::InvalidNumberOfChunks
            | SequencerBlueprintRes::InvalidSignature
            | SequencerBlueprintRes::InvalidNumber => {
                (Some(ParsedInput::InvalidInput), chunk_length + TAG_SIZE)
            }
            SequencerBlueprintRes::Unparsable => (None, chunk_length + TAG_SIZE),
        }
    } else {
        log!(host, Debug, "Read an invalid chunk from slot.");
        (None, TAG_SIZE)
    }
}

fn parse_input<Host: Runtime>(
    host: &mut Host,
    bytes: &[u8],
    next_blueprint_number: &U256,
) -> (Option<ParsedInput>, usize) {
    // The expected format is:

    // blueprint tag (1 byte) / blueprint chunk (variable)

    match bytes[0] {
        DAL_PADDING_TAG => (Some(ParsedInput::Padding), TAG_SIZE),
        DAL_BLUEPRINT_INPUT_TAG => {
            let bytes = &bytes[TAG_SIZE..];
            parse_unsigned_sequencer_blueprint(host, bytes, next_blueprint_number)
        }
        invalid_tag => {
            log!(
                host,
                Debug,
                "DAL slot contains an invalid message tag: '{}'.",
                invalid_tag
            );
            (None, TAG_SIZE)
        } // Tag is invalid, let's yield and give the responsibility to the
          // caller to continue reading.
    }
}

fn parse_slot<Host: Runtime>(
    host: &mut Host,
    slot: &[u8],
    next_blueprint_number: &U256,
) -> Vec<UnsignedSequencerBlueprint> {
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
        let (next_input, length) =
            parse_input(host, &slot[offset..], next_blueprint_number);

        match next_input {
            None => return buffer, // Once an unparsable input has been read,
            // stop reading and return the list of chunks read.
            Some(ParsedInput::UnsignedSequencerBlueprint(b)) => buffer.push(b),
            // Invalid inputs are ignored.
            Some(ParsedInput::InvalidInput) => {}
            Some(ParsedInput::Padding) => return buffer,
        }

        // The offset is incremented by the number of bytes read, and the
        // function returns if all the bytes have been read already.
        offset += length;
    }
}

pub fn fetch_and_parse_sequencer_blueprint_from_dal<Host: Runtime>(
    host: &mut Host,
    slot_size: u64,
    page_size: u64,
    next_blueprint_number: &U256,
    slot_index: u8,
    published_level: u32,
) -> Option<Vec<UnsignedSequencerBlueprint>> {
    if let Some(slot) =
        import_dal_slot(host, slot_size, page_size, published_level, slot_index)
    {
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

        let chunks = parse_slot(host, &slot, next_blueprint_number);
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

#[cfg(test)]
pub mod tests {
    use std::iter::repeat;

    use primitive_types::{H160, U256};
    use rlp::Encodable;
    use tezos_ethereum::tx_common::EthereumTransactionCommon;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;
    use tezos_smart_rollup_host::reveal::HostReveal;

    use crate::{
        block::GENESIS_PARENT_HASH,
        sequencer_blueprint::{
            BlueprintWithDelayedHashes, UnsignedSequencerBlueprint,
            LATEST_BLUEPRINT_VERSION,
        },
    };

    use super::{fetch_and_parse_sequencer_blueprint_from_dal, DAL_BLUEPRINT_INPUT_TAG};

    fn dummy_transaction(i: u8) -> EthereumTransactionCommon {
        let to = &hex::decode("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let to = Some(H160::from_slice(to));

        let unsigned_transaction = EthereumTransactionCommon::new(
            tezos_ethereum::transaction::TransactionType::Legacy,
            Some(U256::one()),
            i as u64,
            U256::from(40000000u64),
            U256::from(40000000u64),
            21000u64,
            to,
            U256::from(500000000u64),
            vec![],
            vec![],
            None,
            None,
        );

        unsigned_transaction
            .sign_transaction(
                "cb9db6b5878db2fa20586e23b7f7b51c22a7c6ed0530daafc2615b116f170cd3"
                    .to_string(),
            )
            .expect("Transaction signature shouldn't fail")
    }

    // See the size of chunks in `sequencer_blueprint.ml` in the sequencer. Note
    // that the kernel doesn't enforce a size for the chunks, this constant is
    // purely for consistency.
    const MAX_CHUNK_SIZE: usize = 3957;

    pub fn dummy_big_blueprint(nb_transactions: usize) -> BlueprintWithDelayedHashes {
        let transactions = repeat(())
            .enumerate()
            .map(|(i, ())| dummy_transaction(i as u8).to_bytes())
            .take(nb_transactions)
            .collect();
        let timestamp = Timestamp::from(123456);
        BlueprintWithDelayedHashes {
            version: LATEST_BLUEPRINT_VERSION,
            timestamp,
            transactions,
            parent_hash: GENESIS_PARENT_HASH,
            delayed_hashes: vec![],
        }
    }

    pub fn chunk_blueprint(
        blueprint: BlueprintWithDelayedHashes,
        number: U256,
    ) -> Vec<UnsignedSequencerBlueprint> {
        let bytes = blueprint.rlp_bytes();
        let chunks = bytes.chunks(MAX_CHUNK_SIZE);
        let nb_chunks = chunks.len();
        chunks
            .enumerate()
            .map(|(chunk_index, chunk)| UnsignedSequencerBlueprint {
                chunk: chunk.to_vec(),
                nb_chunks: nb_chunks as u16,
                number,
                chunk_index: chunk_index as u16,
                chain_id: None,
            })
            .collect()
    }

    pub fn prepare_dal_slot(
        host: &mut MockKernelHost,
        chunks: &[UnsignedSequencerBlueprint],
        published_level: i32,
        slot_index: u8,
    ) {
        let mut slot = Vec::with_capacity((MAX_CHUNK_SIZE + 1) * chunks.len());
        for chunk in chunks {
            let bytes = chunk.rlp_bytes();
            slot.push(DAL_BLUEPRINT_INPUT_TAG);
            slot.extend_from_slice(&bytes);
        }

        host.host.set_dal_slot(published_level, slot_index, &slot)
    }

    #[derive(PartialEq)]
    enum InsertAt {
        Start,
        End,
        AfterChunk(u16),
    }

    fn prepare_invalid_dal_slot(
        host: &mut MockKernelHost,
        chunks: &[UnsignedSequencerBlueprint],
        published_level: i32,
        slot_index: u8,
        invalid_data: &[u8],
        insert_at: InsertAt,
    ) {
        let mut slot =
            Vec::with_capacity((MAX_CHUNK_SIZE + 1) * chunks.len() + invalid_data.len());

        if insert_at == InsertAt::Start {
            slot.extend_from_slice(invalid_data);
        }

        for chunk in chunks {
            let bytes = chunk.rlp_bytes();
            slot.push(DAL_BLUEPRINT_INPUT_TAG);
            slot.extend_from_slice(&bytes);

            if insert_at == InsertAt::AfterChunk(chunk.chunk_index) {
                slot.extend_from_slice(invalid_data);
            }
        }

        if insert_at == InsertAt::End {
            slot.extend_from_slice(invalid_data);
        }

        host.host.set_dal_slot(published_level, slot_index, &slot)
    }

    #[test]
    fn test_parse_regular_slot() {
        let mut host = MockKernelHost::default();

        let blueprint = dummy_big_blueprint(100);
        let chunks = chunk_blueprint(blueprint, 0.into());
        assert!(
            chunks.len() >= 2,
            "Blueprint is composed of {} chunks, but at least two are required for the test to make sense",
            chunks.len()
        );

        let dal_parameters = host.reveal_dal_parameters();
        let published_level = host.host.level() - (dal_parameters.attestation_lag as u32);
        prepare_dal_slot(&mut host, &chunks, published_level as i32, 0);

        let chunks_from_slot = fetch_and_parse_sequencer_blueprint_from_dal(
            &mut host,
            dal_parameters.slot_size,
            dal_parameters.page_size,
            &0.into(),
            0,
            published_level,
        );

        assert_eq!(Some(chunks), chunks_from_slot)
    }

    fn make_invalid_slot(
        invalid_data: &[u8],
        insert_at: InsertAt,
    ) -> (
        Vec<UnsignedSequencerBlueprint>,
        Option<Vec<UnsignedSequencerBlueprint>>,
    ) {
        let mut host = MockKernelHost::default();

        let blueprint = dummy_big_blueprint(100);
        let chunks = chunk_blueprint(blueprint, 0.into());

        assert!(
            chunks.len() >= 2,
            "Blueprint is composed of {} chunks, but at least two are required for the test to make sense",
            chunks.len()
        );

        let dal_parameters = host.reveal_dal_parameters();
        let published_level = host.host.level() - (dal_parameters.attestation_lag as u32);
        prepare_invalid_dal_slot(
            &mut host,
            &chunks,
            published_level as i32,
            0,
            invalid_data,
            insert_at,
        );

        let chunks_from_slot = fetch_and_parse_sequencer_blueprint_from_dal(
            &mut host,
            dal_parameters.slot_size,
            dal_parameters.page_size,
            &0.into(),
            0,
            published_level,
        );

        (chunks, chunks_from_slot)
    }

    #[test]
    fn test_parse_slot_with_invalid_last_data() {
        let invalid_data = vec![2; 10];
        let (chunks, parsed_chunks) = make_invalid_slot(&invalid_data, InsertAt::End);

        assert_eq!(Some(chunks), parsed_chunks)
    }

    #[test]
    fn test_parse_slot_with_invalid_last_chunk() {
        // The tag announces a chunk, the data is not an RLP encoded chunk
        let mut invalid_data = vec![DAL_BLUEPRINT_INPUT_TAG];
        invalid_data.extend_from_slice(dummy_transaction(0).rlp_bytes().as_ref());
        let (chunks, parsed_chunks) = make_invalid_slot(&invalid_data, InsertAt::End);

        assert_eq!(Some(chunks), parsed_chunks)
    }

    #[test]
    fn test_parse_slot_with_invalid_first_data() {
        let invalid_data = vec![2; 10];
        let (_chunks, parsed_chunks) = make_invalid_slot(&invalid_data, InsertAt::Start);

        assert_eq!(Some(vec![]), parsed_chunks)
    }

    #[test]
    fn test_parse_slot_resume_after_invalid_chunk() {
        let mut host = MockKernelHost::default();

        let valid_blueprint_chunks_1 = chunk_blueprint(dummy_big_blueprint(1), 0.into());

        let invalid_blueprint_chunks = {
            let mut chunks = chunk_blueprint(dummy_big_blueprint(1), 0.into());
            for chunk in chunks.iter_mut() {
                chunk.nb_chunks = crate::blueprint_storage::MAXIMUM_NUMBER_OF_CHUNKS + 1
            }
            chunks
        };

        let valid_blueprint_chunks_2 = chunk_blueprint(dummy_big_blueprint(1), 0.into());

        let mut chunks = vec![];
        chunks.extend(valid_blueprint_chunks_1.clone());
        chunks.extend(invalid_blueprint_chunks);
        chunks.extend(valid_blueprint_chunks_2.clone());

        let mut expected_chunks = vec![];
        expected_chunks.extend(valid_blueprint_chunks_1);
        expected_chunks.extend(valid_blueprint_chunks_2);

        let dal_parameters = host.reveal_dal_parameters();
        let published_level = host.host.level() - (dal_parameters.attestation_lag as u32);
        prepare_dal_slot(&mut host, &chunks, published_level as i32, 0);

        let chunks_from_slot = fetch_and_parse_sequencer_blueprint_from_dal(
            &mut host,
            dal_parameters.slot_size,
            dal_parameters.page_size,
            &0.into(),
            0,
            published_level,
        );

        assert_eq!(Some(expected_chunks), chunks_from_slot)
    }

    #[test]
    fn test_parse_slot_with_invalid_first_chunk() {
        // The tag announces a chunk, the data is not an RLP encoded chunk
        let mut invalid_data = vec![DAL_BLUEPRINT_INPUT_TAG];
        invalid_data.extend_from_slice(dummy_transaction(0).rlp_bytes().as_ref());
        let (_chunks, parsed_chunks) = make_invalid_slot(&invalid_data, InsertAt::Start);

        assert_eq!(Some(vec![]), parsed_chunks)
    }

    #[test]
    fn test_parse_slot_with_padding_at_start() {
        // The first byte being 0, the slot is parsed as padding
        let mut invalid_data = vec![0; 1];
        invalid_data.extend_from_slice(dummy_transaction(0).rlp_bytes().as_ref());
        let (_chunks, parsed_chunks) = make_invalid_slot(&invalid_data, InsertAt::Start);

        assert_eq!(Some(vec![]), parsed_chunks)
    }

    #[test]
    fn test_parse_slot_with_invalid_data_after_first_chunk() {
        let invalid_data = vec![2; 10];
        let (chunks, parsed_chunks) =
            make_invalid_slot(&invalid_data, InsertAt::AfterChunk(0));

        let expected_chunks = vec![chunks[0].clone()];
        assert_eq!(Some(expected_chunks), parsed_chunks)
    }

    #[test]
    fn test_parse_slot_with_invalid_chunk_after_first_chunk() {
        // The tag announces a chunk, the data is not an RLP encoded chunk
        let mut invalid_data = vec![DAL_BLUEPRINT_INPUT_TAG];
        invalid_data.extend_from_slice(dummy_transaction(0).rlp_bytes().as_ref());
        let (chunks, parsed_chunks) =
            make_invalid_slot(&invalid_data, InsertAt::AfterChunk(0));

        let expected_chunks = vec![chunks[0].clone()];
        assert_eq!(Some(expected_chunks), parsed_chunks)
    }

    #[test]
    fn test_parse_slot_with_padding_after_first_chunk() {
        // The first byte being 0, the slot is parsed as padding
        let mut invalid_data = vec![0; 1];
        invalid_data.extend_from_slice(dummy_transaction(0).rlp_bytes().as_ref());
        let (chunks, parsed_chunks) =
            make_invalid_slot(&invalid_data, InsertAt::AfterChunk(0));

        let expected_chunks = vec![chunks[0].clone()];
        assert_eq!(Some(expected_chunks), parsed_chunks)
    }

    #[test]
    fn test_parse_invalid_slot() {
        let mut host = MockKernelHost::default();

        let blueprint = dummy_big_blueprint(1);
        let chunks = chunk_blueprint(blueprint, 0.into());

        let dal_parameters = host.reveal_dal_parameters();
        let published_level = host.host.level();

        // Slot will be invalid as it hasn't been attested yet.
        prepare_dal_slot(&mut host, &chunks, published_level as i32, 0);

        let chunks_from_slot = fetch_and_parse_sequencer_blueprint_from_dal(
            &mut host,
            dal_parameters.slot_size,
            dal_parameters.page_size,
            &0.into(),
            0,
            published_level,
        );

        assert_eq!(None, chunks_from_slot);

        // Slot will be invalid as the level of its publication is negative
        // (hence higher than i32::MAX when viewed as unsigned)
        prepare_dal_slot(&mut host, &chunks, -1, 0);

        let chunks_from_slot = fetch_and_parse_sequencer_blueprint_from_dal(
            &mut host,
            dal_parameters.slot_size,
            dal_parameters.page_size,
            &0.into(),
            0,
            published_level,
        );

        assert_eq!(None, chunks_from_slot)
    }

    fn chunk_blueprint_range(min: usize, max: usize) -> Vec<UnsignedSequencerBlueprint> {
        let mut chunks = vec![];
        for n in min..max {
            chunks.extend(chunk_blueprint(dummy_big_blueprint(1), n.into()));
        }
        chunks
    }

    #[test]
    fn test_parse_slot_with_blueprints_from_the_past() {
        let mut host = MockKernelHost::default();

        let next_blueprint_number = 3.into();
        let chunks = chunk_blueprint_range(0, 5);
        let expected_chunks = chunk_blueprint_range(3, 5);

        assert_eq!(5, chunks.len());
        assert_eq!(2, expected_chunks.len());

        let dal_parameters = host.reveal_dal_parameters();
        let published_level = host.host.level() - (dal_parameters.attestation_lag as u32);
        prepare_dal_slot(&mut host, &chunks, published_level as i32, 0);

        let chunks_from_slot = fetch_and_parse_sequencer_blueprint_from_dal(
            &mut host,
            dal_parameters.slot_size,
            dal_parameters.page_size,
            &next_blueprint_number,
            0,
            published_level,
        );

        assert_eq!(chunks_from_slot, Some(expected_chunks));
    }
}
