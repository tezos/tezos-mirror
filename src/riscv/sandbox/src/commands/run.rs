// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    cli::{CommonOptions, RunOptions},
    posix_exit_mode,
};
use octez_riscv::{
    machine_state::bus::main_memory::M1G,
    pvm::PvmHooks,
    stepper::{pvm::PvmStepper, test::TestStepper, StepResult, Stepper, StepperStatus},
};
use std::{error::Error, fs, io::Write, ops::Bound};
use tezos_smart_rollup::utils::{console::Console, inbox::InboxBuilder};
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;

pub fn run(opts: RunOptions) -> Result<(), Box<dyn Error>> {
    let program = fs::read(&opts.input)?;
    let initrd = opts.initrd.as_ref().map(fs::read).transpose()?;

    struct Runner<'a>(&'a RunOptions);

    impl UseStepper<Result<(), Box<dyn Error>>> for Runner<'_> {
        fn advance<S: Stepper>(self, stepper: S) -> Result<(), Box<dyn Error>> {
            run_stepper(stepper, self.0.common.max_steps)
        }
    }

    general_run(&opts.common, program, initrd, Runner(&opts))?
}

/// XXX: Trait used to pass a function for using the generic stepper.
/// (Couldn't use a trait object + impl FnOnce(...) since [`Stepper`] is not object safe)
pub trait UseStepper<R> {
    fn advance<S: Stepper>(self, stepper: S) -> R;
}

pub fn general_run<F: UseStepper<R>, R>(
    common: &CommonOptions,
    program: Vec<u8>,
    initrd: Option<Vec<u8>>,
    f: F,
) -> Result<R, Box<dyn Error>> {
    if common.pvm {
        run_pvm(program.as_slice(), initrd.as_deref(), common, |stepper| {
            f.advance(stepper)
        })
    } else {
        run_test(program.as_slice(), initrd.as_deref(), common, |stepper| {
            f.advance(stepper)
        })
    }
}

fn run_test<R>(
    program: &[u8],
    initrd: Option<&[u8]>,
    common: &CommonOptions,
    f_stepper: impl FnOnce(TestStepper) -> R,
) -> Result<R, Box<dyn Error>> {
    let mut backend = TestStepper::<'_, M1G>::create_backend();
    let stepper = TestStepper::new(
        &mut backend,
        program,
        initrd,
        posix_exit_mode(&common.posix_exit_mode),
    )?;

    Ok(f_stepper(stepper))
}

fn run_pvm<R>(
    program: &[u8],
    initrd: Option<&[u8]>,
    common: &CommonOptions,
    f_stepper: impl FnOnce(PvmStepper<M1G>) -> R,
) -> Result<R, Box<dyn Error>> {
    let mut inbox = InboxBuilder::new();
    if let Some(inbox_file) = &common.inbox.file {
        inbox.load_from_file(inbox_file)?;
    }

    let rollup_address = SmartRollupAddress::from_b58check(common.inbox.address.as_str())?;

    let mut console = if common.timings {
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
        rollup_address.into_hash().as_ref().try_into().unwrap(),
        common.inbox.origination_level,
    )?;

    Ok(f_stepper(stepper))
}

fn run_stepper(mut stepper: impl Stepper, max_steps: Option<usize>) -> Result<(), Box<dyn Error>> {
    let max_steps = match max_steps {
        Some(max_steps) => Bound::Included(max_steps),
        None => Bound::Unbounded,
    };

    let result = stepper.step_max(max_steps);

    match result.to_stepper_status() {
        StepperStatus::Exited { success: true, .. } => Ok(()),
        result => Err(format!("{result:?}").into()),
    }
}
