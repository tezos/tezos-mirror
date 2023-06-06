// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use kernel_sequencer::sequencer_kernel_entry;
use tezos_smart_rollup_host::runtime::Runtime;

pub fn kernel_loop<Host: Runtime>(_host: &mut Host) {}

sequencer_kernel_entry!(
    kernel_loop,
    kernel_sequencer::FilterBehavior::OnlyThisRollup
);
