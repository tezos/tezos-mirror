// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::{entrypoint, prelude::Runtime};
use tezos_smart_rollup_constants::riscv::{SBI_FIRMWARE_TEZOS, SBI_TEZOS_REVEAL};

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

    host.write_debug("Reveal with inline asm ...\n");

    let payload = [1u8; 100];
    let mut buffer = [0u8; 4096];

    let result_size = {
        let result: isize;

        // SBI call
        //   extension = SBI_FIRMWARE_TEZOS
        //   function = SBI_TEZOS_REVEAL
        unsafe {
            core::arch::asm!(
                "ecall",
                in("a0") &payload,
                in("a1") std::mem::size_of_val(&payload),
                in("a2") &mut buffer,
                in("a3") std::mem::size_of_val(&buffer),
                in("a6") SBI_TEZOS_REVEAL,
                in("a7") SBI_FIRMWARE_TEZOS,
                lateout("a0") result,
            );
        }

        assert!(result >= 0);
        result as usize
    };
    host.write_debug(&format!(
        "Reveal with inline asm result: {:?}\n",
        &buffer[..(result_size)]
    ));

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
