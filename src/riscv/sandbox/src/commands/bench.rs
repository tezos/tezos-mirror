// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::error::Error;
use std::fs;
use std::fs::File;

use crate::cli;
use crate::cli::BenchOptions;
use crate::cli::BenchRunOptions;
pub(crate) use crate::commands::bench::stats::BenchStats;
pub(crate) use crate::commands::bench::stats::NamedStats;
use crate::table;

pub mod commands;
mod data;
mod stats;

fn load_from_file(file: &str) -> Result<BenchStats, Box<dyn Error>> {
    let reader = fs::File::open(file)?;
    let stats: BenchStats = serde_json::from_reader(reader)?;
    Ok(stats)
}

fn save_to_file(stats: &BenchStats, opts: &BenchRunOptions) -> Result<(), Box<dyn Error>> {
    let mut file = File::create(&opts.output)?;
    serde_json::to_writer(&mut file, &stats)?;
    Ok(())
}

fn show_results(stats: &BenchStats, opts: &BenchRunOptions) {
    match opts.pretty {
        false => println!("{stats}"),
        true => {
            let table =
                table::table_from_stats(&opts.sort_args, [(stats, &opts.output)].as_slice());
            println!("{table}")
        }
    }
}

pub fn bench(opts: BenchOptions) -> Result<(), Box<dyn Error>> {
    use commands::compare;
    use commands::parser;
    use commands::run;
    match opts.bench_command {
        cli::BenchSubcommand::Run(bench_run_opts) => run::run(bench_run_opts),
        cli::BenchSubcommand::Compare(bench_run_opts) => compare::compare(bench_run_opts),
        cli::BenchSubcommand::Parser(parser_run_opts) => parser::bench(parser_run_opts),
    }
}
