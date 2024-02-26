// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(long_about = "CLI-tool for stress-testing etherlink")]
struct Cli {
    #[arg(short, long, value_name = "CONFIG_FILE")]
    config_file: Option<String>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Configure {
        // etherlink URL
        // etherlink account
        // max connections
    },
    Status {
        // current gas price
        // current balances
    },
    Cleanup {
        // move balances back to main
    },
    Run {
        // Type (erc-20/transfers/etc)
        // Target TPS
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    println!("Hello, world!");

    Ok(())
}
