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

#[derive(Debug)]
struct Parameters {
    attestation_lag: u32,
    slot_size: usize,
    page_size: usize,
    slot_indexes: Vec<u8>,
}

fn get_parameters() -> Parameters {
    // By default use the current sandbox parameters.
    let default_attestation_lag = 4;
    let default_slot_size = 32768;
    let default_page_size = 128;
    // By default track slot index 0.
    let default_slot_indexes = vec![0];

    let attestation_lag = option_env!("ATTESTATION_LAG")
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(default_attestation_lag);
    let slot_size = option_env!("SLOT_SIZE")
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(default_slot_size);
    let page_size = option_env!("PAGE_SIZE")
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(default_page_size);
    let slot_indexes = match option_env!("SLOT_INDEXES") {
        None => default_slot_indexes,
        Some(s) => s
            .split(',')
            .map(|s| s.parse::<u8>())
            .collect::<Result<Vec<u8>, _>>()
            .unwrap_or(default_slot_indexes),
    };

    Parameters {
        attestation_lag,
        slot_size,
        page_size,
        slot_indexes,
    }
}

pub fn entry(host: &mut impl Runtime) {
    let parameters = get_parameters();
    debug_msg!(host, "Running kernel with parameters: {:?}\n", parameters);
    let Parameters {
        attestation_lag,
        slot_size,
        page_size,
        slot_indexes,
    } = parameters;
    match host.read_input() {
        Ok(Some(message)) => {
            let level = message.level;
            let published_level = (level - attestation_lag) as i32;
            let num_pages = slot_size / page_size;
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
