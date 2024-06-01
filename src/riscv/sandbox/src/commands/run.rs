// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{cli::RunOptions, posix_exit_mode};
use octez_riscv::{
    exec_env::pvm::PvmSbiConfig,
    machine_state::bus::main_memory::M1G,
    stepper::{pvm::PvmStepper, test::TestStepper, StepResult, Stepper, StepperStatus},
};
use std::{error::Error, fs};
use tezos_smart_rollup::utils::inbox::InboxBuilder;

pub fn run(opts: RunOptions) -> Result<(), Box<dyn Error>> {
    let program = fs::read(&opts.input)?;
    let initrd = opts.initrd.as_ref().map(fs::read).transpose()?;

    if opts.common.pvm {
        run_pvm(program.as_slice(), initrd.as_deref(), &opts)
    } else {
        run_test(program.as_slice(), initrd.as_deref(), &opts)
    }
}

fn run_test(
    program: &[u8],
    initrd: Option<&[u8]>,
    opts: &RunOptions,
) -> Result<(), Box<dyn Error>> {
    let mut backend = TestStepper::<'_, M1G>::create_backend();
    let stepper = TestStepper::new(
        &mut backend,
        program,
        initrd,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?;

    run_stepper(stepper, opts.common.max_steps)
}

fn run_pvm(program: &[u8], initrd: Option<&[u8]>, opts: &RunOptions) -> Result<(), Box<dyn Error>> {
    let mut inbox = InboxBuilder::new();
    if let Some(inbox_file) = &opts.common.inbox.file {
        inbox.load_from_file(inbox_file)?;
    }

    let config = PvmSbiConfig::default();
    let mut backend = PvmStepper::<'_, '_, M1G>::create_backend();
    let stepper = PvmStepper::new(&mut backend, program, initrd, inbox.build(), config)?;

    run_stepper(stepper, opts.common.max_steps)
}

fn run_stepper(mut stepper: impl Stepper, max_steps: usize) -> Result<(), Box<dyn Error>> {
    match stepper.step_max(max_steps).to_stepper_status() {
        StepperStatus::Exited { success: true, .. } => Ok(()),
        result => Err(format!("{result:?}").into()),
    }
}
