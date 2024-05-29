// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use cli::ExitMode;
use octez_riscv::{
    machine_state::mode::Mode, stepper::test::InterpreterResult, traps::EnvironException,
};
use std::error::Error;

mod cli;
mod commands;
mod console;
mod inbox;
mod table;

fn format_status(result: &InterpreterResult) -> String {
    use InterpreterResult::*;
    match result {
        Exit { code: 0, .. } => "Ok (exit code = 0)".to_string(),
        Exit { code, .. } => format!("Failed with exit code = {}", code),
        Running(_) => "Timeout".to_string(),
        Exception { cause, .. } => format!("{}", exception_to_error(cause)),
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
        cli::Mode::Rvemu(opts) => rvemu(opts),
        cli::Mode::Run(opts) => run(opts),
        cli::Mode::Debug(opts) => debug(opts),
        cli::Mode::Bench(opts) => bench(opts),
    }
}
