// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_host::dal_parameters::RollupDalParameters;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::Runtime;

fn process_slot(
    host: &mut impl Runtime,
    published_level: i32,
    num_pages: usize,
    page_size: usize,
    slot_index: u8,
) {
    let mut buffer = vec![0u8; page_size * num_pages];

    let mut read_bytes = 0;

    for page_index in 0..num_pages {
        let result = host.reveal_dal_page(
            published_level,
            slot_index,
            page_index.try_into().unwrap(),
            &mut buffer[page_index * page_size..(page_index + 1) * page_size],
        );

        match result {
            Ok(0) => {
                // Currently, we send empty pages to kernels for non-attested slots.
                debug_msg!(
                    host,
                    "Slot {} not attested for level {}\n",
                    slot_index,
                    published_level
                );
            }
            Ok(num) => {
                debug_msg!(
                    host,
                    "Retrieved page {} for level {}, slot index {} successfully. {} bytes read\n",
                    page_index,
                    published_level,
                    slot_index,
                    num
                );
                read_bytes += num;
            }
            Err(err) => {
                debug_msg!(
                    host,
                    "Failed to retrieve one of the pages. Slot {} not processed. Error: {}\n",
                    slot_index,
                    &err.to_string()
                );
            }
        }
    }

    let slot_path = format!("/output/slot-{slot_index}");
    let path: OwnedPath = slot_path.as_bytes().to_vec().try_into().unwrap();
    host.store_write(&path, &read_bytes.to_le_bytes(), 0)
        .map_err(|_| "Error writing to storage".to_string())
        .unwrap_or_default();
}

fn get_slots_from_storage(host: &impl Runtime, number_of_slots: u64) -> Vec<u8> {
    let path = RefPath::assert_from("/slots".as_bytes());
    let bitvec_bytes = host.store_read_all(&path).unwrap();

    let mut slots = Vec::new();
    for (i, byte) in bitvec_bytes.iter().enumerate() {
        let slots_start = i * 8;
        // Stop early if the slots are out of bounds, which can happen with
        // padding in the bitvector.
        if slots_start >= number_of_slots as usize {
            continue;
        }
        for bit in 0..8 {
            let slot = slots_start + bit;
            // Stop early if the slot is out of bound, due to padding again.
            if slot >= number_of_slots as usize {
                continue;
            } else if byte >> bit & 1 > 0 {
                slots.push(slot as u8)
            }
        }
    }
    slots
}

#[entrypoint::main]
pub fn entry(host: &mut impl Runtime) {
    let parameters = host.reveal_dal_parameters();
    debug_msg!(host, "Running kernel with parameters: {:?}\n", parameters);
    let RollupDalParameters {
        number_of_slots,
        attestation_lag,
        slot_size,
        page_size,
    } = parameters;

    let slot_indexes: Vec<u8> = get_slots_from_storage(host, number_of_slots);

    match host.read_input() {
        Ok(Some(message)) => {
            let level = message.level;
            let published_level = (level as i32) - (attestation_lag as i32);
            let num_pages = (slot_size / page_size) as usize;
            for slot_index in slot_indexes {
                process_slot(
                    host,
                    published_level,
                    num_pages,
                    page_size as usize,
                    slot_index,
                );
            }
        }
        Ok(None) => {
            debug_msg!(host, "Input message was empty");
        }
        Err(_) => {
            debug_msg!(host, "Failed to read input message");
        }
    }
}
