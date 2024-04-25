// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{cli::RunOptions, format_status, posix_exit_mode};
use risc_v_interpreter::{Interpreter, InterpreterResult};
use std::error::Error;

pub fn run(opts: RunOptions) -> Result<(), Box<dyn Error>> {
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
