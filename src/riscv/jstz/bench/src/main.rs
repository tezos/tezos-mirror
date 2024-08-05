// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use clap::{Parser, Subcommand};
use generate::handle_generate;
use results::handle_results;
use std::error::Error;
use std::path::Path;

mod generate;
mod results;

const DEFAULT_ROLLUP_ADDRESS: &str = "sr163Lv22CdE8QagCwf48PWDTquk6isQwv57";

type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Debug, Parser)]
#[command(long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    #[command(about = "Generate inbox.json file")]
    Generate {
        #[arg(long, default_value = DEFAULT_ROLLUP_ADDRESS)]
        address: String,
        #[arg(long)]
        transfers: usize,
        #[arg(long, default_value = "inbox.json")]
        inbox_file: Box<Path>,
    },
    #[command(about = "Extract results from inbox.json & log file")]
    Results {
        #[arg(long)]
        inbox_file: Box<Path>,
        #[arg(long)]
        log_file: Box<Path>,
    },
}

fn main() -> Result<()> {
    match Cli::parse().command {
        Commands::Generate {
            address,
            inbox_file,
            transfers,
        } => handle_generate(&address, &inbox_file, transfers)?,
        Commands::Results {
            inbox_file,
            log_file,
        } => handle_results(inbox_file, log_file)?,
    }

    Ok(())
}
