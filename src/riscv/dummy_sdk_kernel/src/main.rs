// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::{entrypoint, prelude::Runtime};

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

    // Reveal Preimage: unimplemented.
    host.write_debug("Reveal preimage...\n");

    // Preimage reveal is disabled for !15536, and will be reenabled in !15563
    host.write_debug("Reveal pre-image disabled, not supported\n");
    // let hash = [0u8; PREIMAGE_HASH_SIZE];
    // let mut buffer = [0u8];
    // let result = host.reveal_preimage(&hash, &mut buffer[..]);
    // host.write_debug("Reveal preimage succeeded (unexpected!)\n");
    // host.write_debug(&format!("Preimage: {:?}\n", result));

    // Drain the inbox, making the sandbox stop.
    while host.read_input().map(|msg| msg.is_some()).unwrap_or(true) {}
}
