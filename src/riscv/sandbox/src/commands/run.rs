// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::error::Error;
use std::fs;
use std::io::Write;
use std::ops::Bound;

use octez_riscv::machine_state::DefaultCacheLayouts;
use octez_riscv::machine_state::TestCacheLayouts;
use octez_riscv::machine_state::block_cache::bcall;
use octez_riscv::machine_state::block_cache::bcall::Block;
use octez_riscv::machine_state::memory::M1G;
use octez_riscv::pvm::PvmHooks;
use octez_riscv::state_backend::owned_backend::Owned;
use octez_riscv::stepper::StepResult;
use octez_riscv::stepper::Stepper;
use octez_riscv::stepper::StepperStatus;
use octez_riscv::stepper::pvm::PvmStepper;
use octez_riscv::stepper::test::TestStepper;
use tezos_smart_rollup::utils::console::Console;
use tezos_smart_rollup::utils::inbox::InboxBuilder;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;

use crate::cli::CommonOptions;
use crate::cli::RunOptions;
use crate::posix_exit_mode;

/// Inner execution strategy for blocks.
#[cfg(not(feature = "inline-jit"))]
type BlockImplInner = bcall::Interpreted<M1G, Owned>;

/// Inner execution strategy for blocks.
#[cfg(feature = "inline-jit")]
type BlockImplInner = bcall::InlineJit<M1G, Owned>;

/// Executor of blocks
#[cfg(not(feature = "metrics"))]
pub type BlockImpl = BlockImplInner;

/// Executor of blocks
#[cfg(feature = "metrics")]
pub type BlockImpl = octez_riscv::machine_state::block_cache::metrics::BlockMetrics<BlockImplInner>;

pub fn run(opts: RunOptions) -> Result<(), Box<dyn Error>> {
    let program = fs::read(&opts.input)?;
    let initrd = opts.initrd.as_ref().map(fs::read).transpose()?;

    struct Runner<'a>(&'a RunOptions);

    impl UseStepper<Result<usize, Box<dyn Error>>> for Runner<'_> {
        fn advance<S: Stepper>(self, stepper: S) -> Result<usize, Box<dyn Error>> {
            run_stepper(stepper, self.0.common.max_steps)
        }
    }

    let steps = general_run::<_, _, BlockImpl>(&opts.common, program, initrd, Runner(&opts))??;

    if opts.print_steps {
        println!("Run consumed {steps} steps.");
    }

    #[cfg(feature = "metrics")]
    octez_riscv::dump_block_metrics!(
        file = &opts.metrics.block_metrics_file,
        exclude_supported_instructions = opts.metrics.exclude_supported_instructions
    )?;

    Ok(())
}

/// XXX: Trait used to pass a function for using the generic stepper.
/// (Couldn't use a trait object + impl FnOnce(...) since [`Stepper`] is not object safe)
pub trait UseStepper<R> {
    fn advance<S: Stepper>(self, stepper: S) -> R;
}

pub fn general_run<F: UseStepper<R>, R, B: Block<M1G, Owned>>(
    common: &CommonOptions,
    program: Vec<u8>,
    initrd: Option<Vec<u8>>,
    f: F,
) -> Result<R, Box<dyn Error>> {
    let block_builder = B::BlockBuilder::default();

    if common.pvm {
        run_pvm::<_, B>(
            program.as_slice(),
            initrd.as_deref(),
            common,
            |stepper| f.advance(stepper),
            block_builder,
        )
    } else {
        run_test::<_, B>(
            program.as_slice(),
            initrd.as_deref(),
            common,
            |stepper| f.advance(stepper),
            block_builder,
        )
    }
}

fn run_test<R, B: Block<M1G, Owned>>(
    program: &[u8],
    initrd: Option<&[u8]>,
    common: &CommonOptions,
    f_stepper: impl FnOnce(TestStepper<M1G, TestCacheLayouts, B>) -> R,
    block_builder: B::BlockBuilder,
) -> Result<R, Box<dyn Error>> {
    let stepper = TestStepper::<M1G, _, B>::new(
        program,
        initrd,
        posix_exit_mode(&common.posix_exit_mode),
        block_builder,
    )?;
    Ok(f_stepper(stepper))
}

fn run_pvm<R, B: Block<M1G, Owned>>(
    program: &[u8],
    initrd: Option<&[u8]>,
    common: &CommonOptions,
    f_stepper: impl FnOnce(PvmStepper<M1G, DefaultCacheLayouts, Owned, B>) -> R,
    block_builder: B::BlockBuilder,
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

    let stepper = PvmStepper::<'_, M1G, DefaultCacheLayouts, Owned, B>::new(
        program,
        initrd,
        inbox.build(),
        hooks,
        rollup_address.into_hash().as_ref().try_into().unwrap(),
        common.inbox.origination_level,
        common.preimage.preimages_dir.clone(),
        block_builder,
    )?;

    Ok(f_stepper(stepper))
}

fn run_stepper(
    mut stepper: impl Stepper,
    max_steps: Option<usize>,
) -> Result<usize, Box<dyn Error>> {
    let max_steps = match max_steps {
        Some(max_steps) => Bound::Included(max_steps),
        None => Bound::Unbounded,
    };

    let result = stepper.step_max(max_steps);

    match result.to_stepper_status() {
        StepperStatus::Exited {
            success: true,
            steps,
            ..
        } => Ok(steps),
        result => Err(format!("{result:?}").into()),
    }
}
