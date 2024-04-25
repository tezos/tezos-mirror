// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use cli::{DebugOptions, ExitMode, RunOptions};
use risc_v_interpreter::{
    machine_state::mode::Mode, traps::EnvironException, Interpreter, InterpreterResult,
};
use std::{error::Error, path::Path};

mod cli;
mod commands;
mod debugger;
mod inbox;

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
pub fn exception_to_error(exc: &EnvironException) -> Box<dyn Error> {
    format!("{:?}", exc).into()
}

fn run(opts: RunOptions) -> Result<(), Box<dyn Error>> {
    let contents = std::fs::read(&opts.common.input)?;
    let mut backend = Interpreter::create_backend();
    let mut interpreter = Interpreter::new(
        &mut backend,
        &contents,
        None,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?;

    match interpreter.run(opts.common.max_steps) {
        InterpreterResult::Exit { code: 0, .. } => Ok(()),
        result => Err(format_status(&result).into()),
    }
}

fn debug(opts: DebugOptions) -> Result<(), Box<dyn Error>> {
    let path = Path::new(&opts.common.input);
    let fname = path
        .file_name()
        .ok_or("Invalid program path")?
        .to_str()
        .ok_or("File name cannot be converted to string")?;
    let contents = std::fs::read(path)?;
    Ok(debugger::DebuggerApp::launch(
        fname,
        &contents,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?)
}

fn posix_exit_mode(exit_mode: &ExitMode) -> Mode {
    match exit_mode {
        ExitMode::User => Mode::User,
        ExitMode::Supervisor => Mode::Supervisor,
        ExitMode::Machine => Mode::Machine,
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = cli::parse();
    match cli.command {
        cli::Mode::Rvemu(opts) => commands::rvemu::rvemu(opts),
        cli::Mode::Run(opts) => run(opts),
        cli::Mode::Debug(opts) => debug(opts),
        cli::Mode::Bench(opts) => commands::bench::bench(opts),
    }
}
