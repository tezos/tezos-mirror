// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use cli::ExitMode;
use octez_riscv::{machine_state::mode::Mode, stepper::StepperStatus};
use std::error::Error;

mod cli;
mod commands;
mod table;

fn format_status(result: &StepperStatus) -> String {
    use StepperStatus::*;
    match result {
        Exited {
            success: true,
            status,
            ..
        } => format!("Ok (status = {status})"),
        Exited { status, .. } => format!("Exit with exit code = {}", status),
        Running { .. } => "Timeout".to_string(),
        Errored { cause, message, .. } => format!("{message}\nCaused by: {cause}"),
    }
}

fn posix_exit_mode(exit_mode: &ExitMode) -> Mode {
    match exit_mode {
        ExitMode::User => Mode::User,
        ExitMode::Supervisor => Mode::Supervisor,
        ExitMode::Machine => Mode::Machine,
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    use commands::{bench, debug, run};
    let cli = cli::parse();
    match cli.command {
        cli::Mode::Run(opts) => run(opts),
        cli::Mode::Debug(opts) => debug(opts),
        cli::Mode::Bench(opts) => bench(opts),
    }
}
