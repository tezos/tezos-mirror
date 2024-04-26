// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    cli::{BenchMode, BenchOptions},
    commands::bench::data::{BenchData, FineBenchData, InstrGetError, InstrType, SimpleBenchData},
    posix_exit_mode, table,
};
use enum_tag::EnumTag;
use risc_v_interpreter::{
    parser::{instruction::Instr, parse},
    Interpreter, InterpreterResult,
};
use std::{error::Error, fs::File};

pub use crate::commands::bench::stats::{BenchStats, NamedStats};

mod data;
mod stats;

fn save_to_file(stats: &BenchStats, opts: &BenchOptions) -> Result<(), Box<dyn Error>> {
    let mut file = File::create(&opts.output)?;
    serde_json::to_writer(&mut file, &stats)?;
    Ok(())
}

/// Helper function to look in the [`Interpreter`] to peek for the current [`Instr`]
fn get_current_instr(interpreter: &Interpreter) -> Result<Instr, InstrGetError> {
    let pc = interpreter.read_pc();
    let pc = interpreter
        .translate_instruction_address(pc)
        .or(Err(InstrGetError::Translation))?;
    let first = interpreter.read_bus(pc).or(Err(InstrGetError::Parse))?;
    let second = || interpreter.read_bus(pc);
    parse(first, second).or(Err(InstrGetError::Parse))
}

/// Composes "in time" two [`InterpreterResult`] one after another,
/// to obtain the equivalent final [`InterpreterResult`]
fn compose(
    current_state: InterpreterResult,
    following_result: InterpreterResult,
) -> InterpreterResult {
    use InterpreterResult::*;
    match current_state {
        Exit { .. } => current_state,
        Exception(_, _) => current_state,
        Running(prev_steps) => match following_result {
            Exit { code, steps } => Exit {
                code,
                steps: prev_steps + steps,
            },
            Exception(exc, steps) => Exception(exc, prev_steps + steps),
            Running(steps) => Running(prev_steps + steps),
        },
    }
}

fn bench_fine(interpreter: &mut Interpreter, opts: &BenchOptions) -> BenchData {
    let mut run_res = InterpreterResult::Running(0);
    let mut bench_data = FineBenchData::new();
    let bench_start = quanta::Instant::now();

    for _step in 0..opts.common.max_steps {
        let instr = match get_current_instr(interpreter) {
            Ok(instr) => InstrType::Instr(instr.tag()),
            Err(err) => InstrType::FetchErr(err),
        };

        let start = quanta::Instant::now();
        let step_res = interpreter.run(1);
        let step_duration = start.elapsed();

        bench_data.add_instr(instr, step_duration);

        run_res = compose(run_res, step_res);
        match run_res {
            InterpreterResult::Exit { .. } => break,
            InterpreterResult::Exception(_, _) => break,
            InterpreterResult::Running(_) => (),
        }
    }
    let bench_duration = bench_start.elapsed();

    BenchData::from_fine(bench_data, bench_duration, run_res)
}

/// A single run of the given `interpreter`.
/// Provides basic benchmark data and interpreter result.
fn bench_simple(interpreter: &mut Interpreter, opts: &BenchOptions) -> BenchData {
    let start = quanta::Instant::now();
    let res = interpreter.run(opts.common.max_steps);
    let duration = start.elapsed();

    use InterpreterResult::*;
    let steps = match res {
        Exit { steps, .. } => steps,
        Running(steps) => steps,
        Exception(_exc, steps) => steps,
    };
    let data = SimpleBenchData::new(duration, steps);

    BenchData::from_simple(data, res)
}

fn bench_iteration(opts: &BenchOptions) -> Result<BenchData, Box<dyn Error>> {
    let contents = std::fs::read(&opts.common.input)?;
    let mut backend = Interpreter::create_backend();
    let mut interpreter = Interpreter::new(
        &mut backend,
        &contents,
        None,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?;

    let data = match &opts.mode {
        BenchMode::Simple => bench_simple(&mut interpreter, opts),
        BenchMode::Fine => bench_fine(&mut interpreter, opts),
    };

    Ok(data)
}

fn show_results(stats: &BenchStats, opts: &BenchOptions) {
    match opts.pretty {
        false => println!("{stats}"),
        true => {
            let table = table::table_from_stats(stats, opts.output.clone());
            println!("{table}")
        }
    }
}

pub fn bench(opts: BenchOptions) -> Result<(), Box<dyn Error>> {
    let stats = match opts.repeat {
        0 | 1 => BenchStats::from_data(bench_iteration(&opts)?)?,
        iterations => {
            let mut data_list = vec![];
            for _ in 0..iterations {
                data_list.push(bench_iteration(&opts)?)
            }
            BenchStats::from_data_list(data_list)?
        }
    };

    save_to_file(&stats, &opts)?;

    show_results(&stats, &opts);
    Ok(())
}
