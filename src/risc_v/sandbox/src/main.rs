// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use cli::ExitMode;
use risc_v_interpreter::{machine_state::mode::Mode, traps::EnvironException, InterpreterResult};
use std::error::Error;

mod cli;
mod commands;
mod inbox;
mod table;

fn format_status(result: &InterpreterResult) -> String {
    use InterpreterResult::*;
    match result {
        Exit { code: 0, .. } => "Ok (exit code = 0)".to_string(),
        Exit { code, .. } => format!("Failed with exit code = {}", code),
        Running(_) => "Timeout".to_string(),
        Exception(exc, _) => format!("{}", exception_to_error(exc)),
    }
}

/// Convert a RISC-V exception into an error.
fn exception_to_error(exc: &EnvironException) -> Box<dyn Error> {
    format!("{:?}", exc).into()
}

fn posix_exit_mode(exit_mode: &ExitMode) -> Mode {
    match exit_mode {
        ExitMode::User => Mode::User,
        ExitMode::Supervisor => Mode::Supervisor,
        ExitMode::Machine => Mode::Machine,
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    use commands::{bench, debug, run, rvemu};
    let cli = cli::parse();
    match cli.command {
        cli::Mode::Rvemu(opts) => rvemu::rvemu(opts),
        cli::Mode::Run(opts) => run::run(opts),
        cli::Mode::Debug(opts) => debug::debug(opts),
        cli::Mode::Bench(opts) => bench::bench(opts),
    }
}
