// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::blake2b::digest_256;
use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup::prelude::Runtime;
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
        // tag byte of preimage hash
        vec![0u8],
        digest_256(hex::decode("cafebabe").unwrap().as_slice()),
    ]
    .concat()
    .try_into()
    .unwrap();
    let mut buffer = [0u8; 4096];
    let result_size = host.reveal_preimage(&hash, &mut buffer[..]).unwrap();
    host.write_debug(&format!(
        "Preimage: {:?}\n",
        hex::encode(&buffer[..result_size])
    ));

    // Drain the inbox, making the sandbox stop.
    while host.read_input().map(|msg| msg.is_some()).unwrap_or(true) {}
}
