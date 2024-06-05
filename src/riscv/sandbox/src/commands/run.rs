// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{cli::RunOptions, format_status, posix_exit_mode};
use octez_riscv::{
    machine_state::bus::main_memory::M1G,
    stepper::test::{TestStepper, TestStepperResult},
};
use std::error::Error;

pub fn run(opts: RunOptions) -> Result<(), Box<dyn Error>> {
    let path = opts.input;
    let contents = std::fs::read(path)?;
    let mut backend = TestStepper::<'_, M1G>::create_backend();
    let mut interpreter = TestStepper::new(
        &mut backend,
        &contents,
        None,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?;

    match interpreter.run(opts.common.max_steps) {
        TestStepperResult::Exit { code: 0, .. } => Ok(()),
        result => Err(format_status(&result).into()),
    }
}
