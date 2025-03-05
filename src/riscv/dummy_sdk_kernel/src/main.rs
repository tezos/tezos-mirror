// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::{entrypoint, prelude::Runtime};
use tezos_smart_rollup_constants::core::PREIMAGE_HASH_SIZE;

#[entrypoint::main]
pub fn entry(host: &mut impl Runtime) {
    host.write_debug("Starting Dummy SDK Kernel\n");

    // TODO: The below reveals are being implemented / updated
    // track with the following linear project:
    // https://linear.app/tezos/project/reveal-support-4855d60e4e4f

    // Reveal metadata: implemented (not by pointing at reveal raw yet, so will be updated, but doesn't currently panic.)
    host.write_debug("Reveal metadata...\n");
    let result = host.reveal_metadata();
    host.write_debug("Reveal metadata succeeded, result: \n");
    host.write_debug(&format!("Rollup address: {:?}\n", result));

    host.write_debug("Reveal preimage...\n");

    let hash: [u8; PREIMAGE_HASH_SIZE] = [
        1, 10, 20, 30, 40, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        100, 120, 140, 160,
    ];
    let mut buffer = [0u8; 4096];
    let result_size = host.reveal_preimage(&hash, &mut buffer[..]).unwrap();
    host.write_debug(&format!("Preimage: {:?}\n", &buffer[..result_size]));

    // Drain the inbox, making the sandbox stop.
    while host.read_input().map(|msg| msg.is_some()).unwrap_or(true) {}
}
