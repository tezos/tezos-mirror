// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_entrypoint::kernel_entry;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::runtime::Runtime;

fn process_slot(
    host: &mut impl Runtime,
    published_level: i32,
    num_pages: usize,
    page_size: usize,
    slot_index: u8,
) {
    let mut buffer = vec![0u8; page_size * num_pages];

    for page_index in 0..num_pages {
        let result = host.reveal_dal_page(
            published_level,
            slot_index,
            page_index.try_into().unwrap(),
            &mut buffer[page_index * page_size..(page_index + 1) * page_size],
        );

        match result {
            Ok(_) => {}
            Err(err) => {
                debug_msg!(
                    host,
                    "Failed to retrieve one of the pages. Slot {} not processed. Error: {}",
                    slot_index,
                    &err.to_string()
                );
                // Stop fetching pages on error
                return;
            }
        }
    }

    debug_msg!(host, "Writing buffer with size {}\n", buffer.len());
    let slot_path = format!("/output/slot-{}", slot_index);
    let path: OwnedPath = slot_path.as_bytes().to_vec().try_into().unwrap();
    host.store_write(&path, &buffer, 0)
        .map_err(|_| "Error writing to storage".to_string())
        .unwrap_or_default();
}

pub fn entry(host: &mut impl Runtime) {
    // we use the current sandbox parameters
    let attestation_lag = 4;
    let slot_size = 32768;
    let page_size = 128;
    let num_pages = slot_size / page_size;

    match host.read_input() {
        Ok(Some(message)) => {
            let level = message.level;
            let published_level = (level - attestation_lag) as i32;
            let slot_indexes = vec![0u8];
            for slot_index in slot_indexes {
                process_slot(host, published_level, num_pages, page_size, slot_index);
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

kernel_entry!(entry);
