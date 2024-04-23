// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{cli::Options, format_status, posix_exit_mode};
use risc_v_interpreter::{Interpreter, InterpreterResult};
use std::{error::Error, fmt::Display, time::Duration};

struct SimpleBenchData {
    duration: Duration,
    steps: usize,
}

impl Display for SimpleBenchData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let f_duration = format!("Duration: {:?}", self.duration);
        let f_steps = format!("Steps:    {}", self.steps);
        write!(f, "Simple bench data:\n {}\n {}", f_duration, f_steps)
    }
}

/// A single run of the given `interpreter`.
/// Provides basic benchmark data and interpreter result.
fn bench_simple(
    interpreter: &mut Interpreter,
    opts: Options,
) -> (SimpleBenchData, InterpreterResult) {
    let start = quanta::Instant::now();
    let res = interpreter.run(opts.max_steps);
    let duration = start.elapsed();

    use InterpreterResult::*;
    let steps = match res {
        Exit { steps, .. } => steps,
        Running(steps) => steps,
        Exception(_exc, steps) => steps,
    };
    let data = SimpleBenchData { duration, steps };

    (data, res)
}

pub fn bench(opts: Options) -> Result<(), Box<dyn Error>> {
    let contents = std::fs::read(&opts.input)?;
    let mut backend = Interpreter::create_backend();
    let mut interpreter = Interpreter::new(&mut backend, &contents, None, posix_exit_mode(&opts))?;

    let (bench_data, result) = bench_simple(&mut interpreter, opts);

    println!("Outcome of the run: {}", format_status(&result));
    println!("{}", bench_data);
    Ok(())
}
