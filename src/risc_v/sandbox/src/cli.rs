// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use clap::{Parser, Subcommand};

#[derive(Debug, Clone, Subcommand)]
pub enum Mode {
    /// Run a program using the RISC-V interpreter
    Run(Options),
    /// Run a program using rvemu
    Rvemu(Options),
}

#[derive(Debug, Clone, Parser)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Mode,
}

/// Structure that encodes the CLI options, flags and commands
#[derive(Debug, Clone, Parser)]
pub struct Options {
    /// Path to the input ELF executable
    #[arg(short, long)]
    pub input: String,

    /// Path to the initrd
    #[arg(long)]
    pub initrd: Option<String>,

    /// Keep going after the inbox has been drained.
    #[arg(short, long)]
    pub keep_going: bool,

    /// Support some POSIX-style system calls
    #[arg(long)]
    pub posix: bool,

    /// Rollup address
    #[arg(short, long, default_value = "sr1UNDWPUYVeomgG15wn5jSw689EJ4RNnVQa")]
    pub address: String,

    /// Rollup origination level
    #[arg(short = 'l', long, default_value_t = 0)]
    pub origination_level: u64,
}

/// Parse the command-line arguments.
pub fn parse() -> Cli {
    Cli::parse()
}
