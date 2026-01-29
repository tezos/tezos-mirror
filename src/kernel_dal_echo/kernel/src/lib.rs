// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_host::dal_parameters::RollupDalParameters;
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
            Ok(0) => {
                // Currently, we send empty pages to kernels for non-attested slots.
                debug_msg!(
                    host,
                    "Slot {} not attested for level {}\n",
                    slot_index,
                    published_level
                );
                return;
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
                let slot_path = format!("/output/slot-{slot_index}");
                let path: OwnedPath = slot_path.as_bytes().to_vec().try_into().unwrap();

                host.store_write(&path, &buffer, 0)
                    .map_err(|_| "Error writing to storage".to_string())
                    .unwrap_or_default();
            }
            Err(err) => {
                debug_msg!(
                    host,
                    "Failed to retrieve one of the pages. Slot {} not processed. Error: {}\n",
                    slot_index,
                    &err.to_string()
                );
                // Stop fetching pages on error
                return;
            }
        }
    }
}

fn get_slot_indexes_from_env() -> Vec<u8> {
    // By default track slot index 0.
    let default_slot_indexes = vec![0];

    let slot_indexes = match option_env!("SLOT_INDEXES") {
        None => default_slot_indexes,
        Some(s) => s
            .split(',')
            .map(|s| s.parse::<u8>())
            .collect::<Result<Vec<u8>, _>>()
            .unwrap_or(default_slot_indexes),
    };

    slot_indexes
}

#[entrypoint::main]
pub fn entry(host: &mut impl Runtime) {
    let parameters = host.reveal_dal_parameters();
    debug_msg!(host, "Running kernel with parameters: {:?}\n", parameters);
    let RollupDalParameters {
        number_of_slots: _number_of_slots,
        attestation_lag,
        slot_size,
        page_size,
    } = parameters;

    let slot_indexes = get_slot_indexes_from_env();

    match host.read_input() {
        Ok(Some(message)) => {
            let level = message.level;
            // Before importing a slot, we wait 2 blocks for finality + 1 block for DAL node processing
            let import_extra_delay = 3;
            let published_level = (level as i32) - (attestation_lag as i32) - import_extra_delay;
            if published_level > 0 {
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
        }
        Ok(None) => {
            debug_msg!(host, "Input message was empty");
        }
        Err(_) => {
            debug_msg!(host, "Failed to read input message");
        }
    }
}
