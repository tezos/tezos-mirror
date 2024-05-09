// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use clap::{Parser, Subcommand, ValueEnum};
use std::path::Path;

#[derive(Debug, Clone, Subcommand)]
pub enum Mode {
    /// Run a program using the RISC-V interpreter
    Run(RunOptions),
    /// Launch a program in the debugger
    Debug(DebugOptions),
    /// Run a program using rvemu
    Rvemu(RvemuOptions),
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
}

#[derive(Debug, Clone, Parser)]
pub struct DebugOptions {
    #[command(flatten)]
    pub common: CommonOptions,
}

#[derive(Debug, Clone, Parser)]
pub struct RvemuOptions {
    #[command(flatten)]
    pub common: CommonOptions,

    /// Path to the initrd
    #[arg(long)]
    pub initrd: Option<String>,

    /// Support some POSIX-style system calls
    #[arg(long)]
    pub posix: bool,
}

#[derive(Clone, ValueEnum, Debug)]
pub enum BenchMode {
    Simple,
    Fine,
}

#[derive(Debug, Clone, Parser)]
pub struct BenchOptions {
    /// Type of benchmark
    #[arg(long)]
    pub mode: BenchMode,

    #[arg(
        help = "How often to repeat the benchmark run",
        long = "iter",
        default_value_t = 1
    )]
    pub repeat: usize,

    #[arg(
        help = "Name of the file to save the benchmark stats",
        long,
        default_value = "benchmark.json",
        value_parser = validate_output,
    )]
    pub output: String,

    #[command(flatten)]
    pub common: CommonOptions,
}

/// Validator for `--output <filename>` option
pub fn validate_output(output: &str) -> Result<String, &'static str> {
    if output.is_empty() {
        Err("Output filename can not be empty")
    } else {
        Ok(output.to_string())
    }
}

#[derive(Debug, Clone, Parser)]
pub struct CommonOptions {
    /// Path to the input ELF executable
    #[arg(short, long)]
    pub input: String,

    #[arg(short = 'm', long, value_enum, default_value_t = ExitMode::User)]
    pub posix_exit_mode: ExitMode,

    #[arg(long, default_value_t = 1_000_000)]
    pub max_steps: usize,

    #[command(flatten)]
    pub inbox: InboxOptions,
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
    pub origination_level: u64,

    /// Path to the file containing inbox messages
    #[arg(long = "inbox-file")]
    pub file: Option<Box<Path>>,
}

/// Parse the command-line arguments.
pub fn parse() -> Cli {
    Cli::parse()
}
