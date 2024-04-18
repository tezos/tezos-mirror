// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{bench::data::SimpleBenchData, cli::BenchOptions, format_status, posix_exit_mode};
use risc_v_interpreter::{Interpreter, InterpreterResult};
use std::error::Error;

mod data;

/// A single run of the given `interpreter`.
/// Provides basic benchmark data and interpreter result.
fn bench_simple(
    interpreter: &mut Interpreter,
    opts: BenchOptions,
) -> (SimpleBenchData, InterpreterResult) {
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

    (data, res)
}

pub fn bench(opts: BenchOptions) -> Result<(), Box<dyn Error>> {
    let contents = std::fs::read(&opts.common.input)?;
    let mut backend = Interpreter::create_backend();
    let mut interpreter = Interpreter::new(
        &mut backend,
        &contents,
        None,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?;

    let (bench_data, result) = bench_simple(&mut interpreter, opts);

    println!("Outcome of the run: {}", format_status(&result));
    println!("{}", bench_data);
    Ok(())
}
