// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::error::Error;
use std::path::Path;

use clap::Parser;
use clap::Subcommand;
use generate::handle_generate;
use generate::handle_generate_script;
use results::handle_results;

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
    #[command(about = "Generate inbox.sh script")]
    GenerateScript {
        #[arg(long, default_value = DEFAULT_ROLLUP_ADDRESS)]
        address: String,
        #[arg(long)]
        transfers: usize,
        #[arg(long, default_value = "inbox.sh")]
        script_file: Box<Path>,
    },
    #[command(about = "Extract results from inbox.json & log file")]
    Results {
        #[arg(long)]
        inbox_file: Box<Path>,
        #[arg(long)]
        log_file: Vec<Box<Path>>,
        #[arg(long)]
        expected_transfers: usize,
    },
}

fn main() -> Result<()> {
    match Cli::parse().command {
        Commands::Generate {
            address,
            inbox_file,
            transfers,
        } => handle_generate(&address, &inbox_file, transfers)?,
        Commands::GenerateScript {
            address,
            script_file,
            transfers,
        } => handle_generate_script(&address, &script_file, transfers)?,
        Commands::Results {
            inbox_file,
            log_file,
            expected_transfers,
        } => handle_results(inbox_file, log_file, expected_transfers)?,
    }

    Ok(())
}
