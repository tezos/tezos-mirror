// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use kernel_sequencer::sequencer_kernel_entry;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_host::runtime::Runtime;

pub fn kernel_loop<Host: Runtime>(host: &mut Host) {
    while let Ok(Some(message)) = host.read_input() {
        debug_msg!(
            host,
            "Processing MessageData {} at level {}",
            message.id,
            message.level
        );

        if let Err(e) = host.mark_for_reboot() {
            debug_msg!(host, "Could not mark host for reboot: {}", e);
        }
    }
}

sequencer_kernel_entry!(
    kernel_loop,
    kernel_sequencer::FilterBehavior::OnlyThisRollup
);
