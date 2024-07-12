// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use clap::{Parser, Subcommand, ValueEnum};
use std::{error::Error, path::Path};

#[derive(Debug, Clone, Subcommand)]
pub enum Mode {
    /// Run a program using the RISC-V interpreter
    Run(RunOptions),
    /// Launch a program in the debugger
    Debug(DebugOptions),
    /// Benchmark a program
    Bench(BenchOptions),
}

#[derive(Clone, ValueEnum, Debug)]
pub enum ExitMode {
    User,
    Supervisor,
    Machine,
}

#[derive(Debug, Clone, Parser)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Mode,
}

#[derive(Debug, Clone, Parser)]
pub struct RunOptions {
    #[command(flatten)]
    pub common: CommonOptions,

    /// Path to the input ELF executable
    #[arg(long, short)]
    pub input: Box<Path>,

    /// Path to the initrd
    #[arg(long)]
    pub initrd: Option<Box<Path>>,
}

#[derive(Debug, Clone, Parser)]
pub struct DebugOptions {
    #[command(flatten)]
    pub common: CommonOptions,

    /// Path to the input ELF executable
    #[arg(long, short)]
    pub input: Box<Path>,

    /// Path to the initrd
    #[arg(long)]
    pub initrd: Option<Box<Path>>,
}

#[derive(Clone, ValueEnum, Debug)]
pub enum BenchMode {
    Simple,
    Fine,
}

#[derive(Debug, Clone, Parser)]
pub struct BenchOptions {
    #[command(subcommand)]
    pub bench_command: BenchSubcommand,
}

#[derive(Debug, Clone, Subcommand)]
pub enum BenchSubcommand {
    /// Runs a benchmark (alias for default bench command)
    Run(BenchRunOptions),
    /// Loads and compares runs from given json files
    Compare(BenchCompareOptions),
}

#[derive(Debug, Clone, Parser)]
pub struct BenchRunOptions {
    /// Type of benchmark
    #[arg(long)]
    pub mode: BenchMode,

    /// How often to repeat the benchmark run
    #[arg(long = "iter", default_value_t = 1)]
    pub repeat: usize,

    /// Name of the file to save the benchmark stats
    #[arg(
        long,
        default_value = "benchmark.json",
        value_parser = validate_output,
    )]
    pub output: String,

    /// Pretty print the benchmark results in a table
    #[arg(long)]
    pub pretty: bool,

    #[command(flatten)]
    pub common: CommonOptions,

    /// Paths to the input ELF executable
    #[arg(long, short, num_args=1..)]
    pub inputs: Vec<Box<Path>>,

    #[command(flatten)]
    pub sort_args: TableSortArgs,
}

/// Validator for `--output <filename>` option
pub fn validate_output(output: &str) -> Result<String, &'static str> {
    if output.is_empty() {
        Err("Output filename can not be empty")
    } else {
        Ok(output.to_string())
    }
}

#[derive(Debug, Clone, ValueEnum)]
pub enum SortRuns {
    /// Keep the input order for runs
    Input,
    /// Order runs alphabetically
    Alphabetic,
    /// Order instructions / s speed
    Speed,
    /// Order by total count of the instruction
    Total,
}

#[derive(Debug, Clone, ValueEnum)]
pub enum SortInstr {
    /// Order instruction groups by the maximum average
    MaxAvg,
    /// Order sum of total count of instructions in the whole group
    TotalCount,
    /// Order instruction groups alphabetically by instruction name
    Alphabetic,
}

#[derive(Debug, Clone, Parser)]
pub struct BenchCompareOptions {
    /// List of .json files for comparison
    #[arg(long, num_args = 1..)]
    comp_file: Vec<Box<Path>>,

    #[command(flatten)]
    pub sort_args: TableSortArgs,
}

impl BenchCompareOptions {
    pub fn comparison_files(&self) -> Result<Vec<String>, Box<dyn Error>> {
        let x = self
            .comp_file
            .iter()
            .map(|p| {
                if let Some(p) = p.to_str() {
                    Ok(p.to_string())
                } else {
                    Err(Box::new(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        "Invalid path to benchmark file",
                    )))
                }
            })
            // If any of the items is `Err`, it will fail with `Err`
            .collect::<Result<Vec<_>, _>>()?;

        Ok(x)
    }
}

#[derive(Debug, Clone, Parser)]
pub struct TableSortArgs {
    /// Order of benchmarks in the context of a single instruction
    #[arg(long, value_enum, default_value_t = SortRuns::Input)]
    pub sort_runs: SortRuns,

    /// Order of instruction data groups
    #[arg(long, value_enum, default_value_t = SortInstr::MaxAvg)]
    pub sort_instr: SortInstr,
}

#[derive(Debug, Clone, Parser)]
pub struct CommonOptions {
    #[arg(short = 'm', long, value_enum, default_value_t = ExitMode::User)]
    pub posix_exit_mode: ExitMode,

    #[arg(long)]
    pub max_steps: Option<usize>,

    #[command(flatten)]
    pub inbox: InboxOptions,

    #[arg(long, default_value_t = false)]
    pub timings: bool,

    /// Run as PVM.
    #[arg(long)]
    pub pvm: bool,
}

#[derive(Debug, Clone, Parser)]
pub struct InboxOptions {
    /// Keep going after the inbox has been drained.
    #[arg(short, long)]
    pub keep_going: bool,

    /// Rollup address
    #[arg(short, long, default_value = "sr1UNDWPUYVeomgG15wn5jSw689EJ4RNnVQa")]
    pub address: String,

    /// Rollup origination level
    #[arg(short = 'l', long, default_value_t = 0)]
    pub origination_level: u32,

    /// Path to the file containing inbox messages
    #[arg(long = "inbox-file")]
    pub file: Option<Box<Path>>,
}

/// Parse the command-line arguments.
pub fn parse() -> Cli {
    Cli::parse()
}
