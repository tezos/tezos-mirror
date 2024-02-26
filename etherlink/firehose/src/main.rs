// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use anyhow::Result;
use clap::{Parser, Subcommand};
use tokio::task::spawn_blocking;

mod config;

use config::Config;

#[derive(Debug, Parser)]
#[command(long_about = "CLI-tool for stress-testing etherlink")]
struct Cli {
    #[arg(short, long)]
    config_file: Option<String>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    #[command(long_about = "Set the RPC endpoint to be used, and optionally the controller private key.")]
    Configure {
        #[arg(long)]
        endpoint: String,
        #[arg(long, value_name = "COORDINATOR_SK")]
        coordinator: Option<String>,
    },
    Status {
        // current gas price
        // current balances
    },
    Start {
        // Type (erc-20/transfers/etc)
        // Target TPS
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = spawn_blocking(Cli::parse).await?;
    let config_path = if let Some(p) = cli.config_file {
        p.into()
    } else {
        Config::config_path().await?
    };

    match cli.command {
        Commands::Configure {
            endpoint,
            coordinator,
        } => Config::configure(&config_path, endpoint, coordinator).await?,
        command => todo!("handle command {command:?}"),
    };

    Ok(())
}
