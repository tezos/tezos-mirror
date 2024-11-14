// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    cli::{BenchMode, BenchRunOptions},
    commands::{
        bench::{
            data::{BenchData, FineBenchData, InstrGetError, InstrType, SimpleBenchData},
            save_to_file, show_results, BenchStats,
        },
        run::{general_run, UseStepper},
    },
    format_status,
};
use enum_tag::EnumTag;
use octez_riscv::{
    machine_state::{
        bus::{Address, AddressableRead},
        AccessType,
    },
    parser::{instruction::Instr, parse},
    state_backend::ManagerRead,
    stepper::{StepResult, Stepper, StepperStatus},
};
use std::{
    collections::HashSet,
    error::Error,
    fs,
    ops::Bound,
    path::{Path, PathBuf},
};

/// Helper function to look in the [`Interpreter`] to peek for the current [`Instr`]
/// Assumes the program counter will be a multiple of 2.
fn get_current_instr<S: Stepper>(stepper: &S) -> Result<Instr, InstrGetError>
where
    S::Manager: ManagerRead,
{
    let machine_state = stepper.machine_state();
    let get_half_instr = |raw_pc: Address| -> Result<u16, InstrGetError> {
        let pc = machine_state
            .translate_without_cache(raw_pc, AccessType::Instruction)
            .or(Err(InstrGetError::Translation))?;
        machine_state.bus.read(pc).or(Err(InstrGetError::Parse))
    };
    let pc = machine_state.hart.pc.read();
    let first = get_half_instr(pc)?;
    let second = || get_half_instr(pc + 2);
    parse(first, second)
}

/// Composes "in time" two [`InterpreterResult`] one after another,
/// to obtain the equivalent final [`InterpreterResult`]
fn compose(current_state: StepperStatus, following_result: StepperStatus) -> StepperStatus {
    use StepperStatus::*;
    match current_state {
        Exited { .. } => current_state,
        Errored { .. } => current_state,
        Running { steps: prev_steps } => match following_result {
            Exited {
                steps,
                status,
                success,
            } => Exited {
                success,
                status,
                steps: prev_steps + steps,
            },
            Errored {
                cause,
                steps,
                message,
            } => Errored {
                cause,
                message,
                steps: prev_steps + steps,
            },
            Running { steps } => Running {
                steps: prev_steps + steps,
            },
        },
    }
}

fn bench_fine<S: Stepper>(interpreter: &mut S, opts: &BenchRunOptions) -> BenchData {
    let mut run_res = StepperStatus::default();
    let mut bench_data = FineBenchData::new();
    let bench_start = quanta::Instant::now();

    for _step in 0..opts.common.max_steps.unwrap_or(usize::MAX) {
        let instr = match get_current_instr(interpreter) {
            Ok(instr) => InstrType::Instr(instr.tag()),
            Err(err) => InstrType::FetchErr(err),
        };

        let start = quanta::Instant::now();
        let step_res = interpreter.step_max(Bound::Included(1));
        let step_duration = start.elapsed();

        bench_data.add_instr(instr, step_duration);

        run_res = compose(run_res.to_stepper_status(), step_res.to_stepper_status());
        match run_res {
            StepperStatus::Exited { .. } => break,
            StepperStatus::Errored { .. } => break,
            StepperStatus::Running { .. } => (),
        }
    }
    let bench_duration = bench_start.elapsed();

    BenchData::from_fine(bench_data, bench_duration, run_res)
}

/// A single run of the given `interpreter`.
/// Provides basic benchmark data and interpreter result.
fn bench_simple<S: Stepper>(interpreter: &mut S, opts: &BenchRunOptions) -> BenchData {
    let start = quanta::Instant::now();
    let step_bound = opts
        .common
        .max_steps
        .map(Bound::Included)
        .unwrap_or(Bound::Unbounded);
    let res = interpreter.step_max(step_bound);
    let duration = start.elapsed();

    use StepperStatus::*;
    let steps = match res.to_stepper_status() {
        Exited { steps, .. } => steps,
        Running { steps } => steps,
        Errored { steps, .. } => steps,
    };
    let data = SimpleBenchData::new(duration, steps);

    BenchData::from_simple(data, res.to_stepper_status())
}

fn bench_iteration(path: &Path, opts: &BenchRunOptions) -> Result<BenchData, Box<dyn Error>> {
    let program = std::fs::read(path)?;
    let initrd = opts.initrd.as_ref().map(fs::read).transpose()?;

    struct Runner<'a>(&'a BenchRunOptions);

    impl UseStepper<BenchData> for Runner<'_> {
        fn advance<S: Stepper>(self, mut stepper: S) -> BenchData {
            let opts = &self.0;
            match opts.mode {
                BenchMode::Simple => bench_simple(&mut stepper, opts),
                BenchMode::Fine => bench_fine(&mut stepper, opts),
            }
        }
    }

    general_run(&opts.common, program, initrd, Runner(opts))
}

fn transform_folders(inputs: &[Box<Path>]) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let mut binary_paths = vec![];
    for path in inputs {
        if fs::metadata(path)?.is_dir() {
            for sub_path in fs::read_dir(path)? {
                let sub_path = sub_path?.path();
                if fs::metadata(&sub_path)?.is_file() {
                    binary_paths.push(sub_path)
                }
            }
        } else {
            binary_paths.push(path.to_path_buf())
        }
    }
    Ok(binary_paths)
}

pub fn run(opts: BenchRunOptions) -> Result<(), Box<dyn Error>> {
    let paths = transform_folders(&opts.inputs)?;
    let mut stats = paths
        .into_iter()
        .filter_map(|path| run_binary(&path, &opts).ok())
        .reduce(|acc, e| e.combine(acc))
        .ok_or("Could not combine benchmark results".to_string())?;
    stats.normalize_instr_data();
    save_to_file(&stats, &opts)?;
    show_results(&stats, &opts);
    Ok(())
}

fn run_binary(path: &Path, opts: &BenchRunOptions) -> Result<BenchStats, Box<dyn Error>> {
    let get_warning_from_iteration = |iteration: &BenchData| match &iteration.run_result {
        StepperStatus::Exited { success: true, .. } => None,
        result => Some(format_status(result)),
    };

    let mut warnings = HashSet::<String>::new();
    let mut consider_iteration = |iteration: &BenchData| {
        if let Some(w) = get_warning_from_iteration(iteration) {
            warnings.insert(w);
        }
    };

    let stats = match opts.repeat {
        0 | 1 => {
            let iteration = bench_iteration(path, opts)?;
            consider_iteration(&iteration);
            BenchStats::from_data(iteration)?
        }
        iterations => {
            let mut data_list = vec![];
            for _ in 0..iterations {
                let iteration = bench_iteration(path, opts)?;
                consider_iteration(&iteration);
                data_list.push(iteration);
            }
            BenchStats::from_data_list(data_list)?
        }
    };
    if !warnings.is_empty() {
        eprintln!("Warning: Binary {:?} exited with:", path);
        for w in warnings {
            eprintln!(" - {}", w);
        }
    }
    Ok(stats)
}
