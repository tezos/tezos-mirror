// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Options when running a kernel on the native architecture,
//! for example, controlling whether debug logs are printed with
//! timestamps.

#![cfg(not(any(
    target_arch = "wasm32",
    all(target_arch = "riscv64", target_os = "hermit")
)))]

use crate::console::Console;
use clap::Parser;
use tezos_smart_rollup_mock::MockHost;

#[derive(Debug, Clone, Parser)]
struct NativeCli {
    /// Whether the debug log is printed as is, or with timestamp information.
    #[arg(long, default_value_t = false)]
    pub timings: bool,
}

/// Apply cli options to the mock host.
pub fn apply_cli_opts(host: &mut MockHost) {
    let NativeCli { timings } = NativeCli::parse();

    let console = if timings {
        Console::with_timings()
    } else {
        Console::new()
    };

    host.set_debug_handler(console);
}
