// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use clap::{Parser, Subcommand};
use generate::handle_generate;
use std::error::Error;

mod generate;

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
        #[arg(long)]
        transfers: usize,
    },
}

fn main() -> Result<()> {
    match Cli::parse().command {
        Commands::Generate { transfers } => handle_generate(transfers)?,
    }

    Ok(())
}
