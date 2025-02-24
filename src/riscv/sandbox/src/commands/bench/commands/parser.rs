// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::error::Error;
use std::ops::Div;

use octez_riscv::parser::parse_block;

use crate::cli::BenchParserRunOptions;

pub fn bench(opts: BenchParserRunOptions) -> Result<(), Box<dyn Error>> {
    let runs = opts.repeat;
    let files = opts.inputs;

    let mut formatter = numfmt::Formatter::new()
        .precision(numfmt::Precision::Decimals(2))
        .separator(',')
        .unwrap();

    for f in files.iter() {
        let kernel = std::fs::read(f)?;

        let average = (0..runs)
            .map(|_| do_run(&kernel))
            .sum::<std::time::Duration>()
            .div(runs as u32);

        let num_instr = parse_block(&kernel).len();

        let speed = num_instr as f64 / average.as_secs_f64();

        let speed = formatter.fmt2(speed);

        println!(
            r#"{}
- Bytes parsed: {}
- Average:      {average:.2?}
- Throughput:   {speed} Instructions/s
- Runs:         {runs}"#,
            f.display(),
            kernel.len()
        );
    }

    Ok(())
}

fn do_run(bytes: &[u8]) -> std::time::Duration {
    let now = quanta::Instant::now();
    let _ = parse_block(bytes);
    now.elapsed()
}
