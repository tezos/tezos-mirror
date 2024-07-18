// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{cli::RunOptions, posix_exit_mode};
use octez_riscv::{
    machine_state::bus::main_memory::M1G,
    pvm::PvmHooks,
    stepper::{pvm::PvmStepper, test::TestStepper, StepResult, Stepper, StepperStatus},
};
use std::{error::Error, fs, io::Write};
use tezos_smart_rollup::utils::{console::Console, inbox::InboxBuilder};
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;

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

    let rollup_address = SmartRollupAddress::from_b58check(opts.common.inbox.address.as_str())?;

    let mut console = if opts.common.timings {
        Console::with_timings()
    } else {
        Console::new()
    };

    let hooks = PvmHooks::new(|c| {
        let _written = console.write(&[c]).unwrap();
    });

    let mut backend = PvmStepper::<'_, '_, M1G>::create_backend();
    let stepper = PvmStepper::new(
        &mut backend,
        program,
        initrd,
        inbox.build(),
        hooks,
        rollup_address.into_hash().0.try_into().unwrap(),
        opts.common.inbox.origination_level,
    )?;

    run_stepper(stepper, opts.common.max_steps)
}

fn run_stepper(mut stepper: impl Stepper, max_steps: Option<usize>) -> Result<(), Box<dyn Error>> {
    let result = match max_steps {
        Some(max_steps) => stepper.step_range_while(..=max_steps, |_| true),
        None => stepper.step_range_while(.., |_| true),
    };

    match result.to_stepper_status() {
        StepperStatus::Exited { success: true, .. } => Ok(()),
        result => Err(format!("{result:?}").into()),
    }
}
