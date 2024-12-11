// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Options when running a kernel on the native architecture,
//! for example, controlling whether debug logs are printed with
//! timestamps.

#![cfg(pvm_kind = "none")]

use crate::console::Console;
use clap::Parser;
use tezos_smart_rollup_mock::MockHost;

#[derive(Debug, Clone, Parser)]
struct NativeCli {
    /// Whether the debug log is printed as is, or with timestamp information.
    #[arg(long, default_value_t = false)]
    pub timings: bool,

    /// Keep going after the inbox has been drained.
    #[arg(long, default_value_t = false)]
    pub keep_going: bool,
}

/// Apply cli options to the mock host.
pub fn apply_cli_opts(host: &mut MockHost) {
    let NativeCli {
        timings,
        keep_going,
    } = NativeCli::parse();

    let console = if timings {
        Console::with_timings()
    } else {
        Console::new()
    };

    host.set_debug_handler(console);
    host.keep_going(keep_going);
}
