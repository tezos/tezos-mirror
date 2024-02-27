// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use anyhow::Result;
use clap::{Parser, Subcommand};
use ethers::utils::format_units;
use tokio::task::spawn_blocking;
use tokio::try_join;

mod client;
mod config;

use client::Client;
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
        #[arg(long, value_name = "CONTROLLER_SK")]
        controller: Option<String>,
    },
    Status,
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
            controller,
        } => Config::configure(&config_path, endpoint, controller).await?,
        Commands::Status => {
            let config = Config::load(&config_path).await?;
            status_check(&config).await?;
        }
        command => todo!("handle command {command:?}"),
    };

    Ok(())
}

// retrieve controller balance & current gas price
async fn status_check(config: &Config) -> Result<()> {
    let client = Client::new(config)?;
    let (chain_id, gas_price, controller_balance) = try_join!(
        client.chain_id(),
        client.gas_price(),
        client.controller_balance()
    )?;
    println!("chain_id\t\t{chain_id}");
    println!("gas_price\t\t{}\tGwei", format_units(gas_price, "gwei")?);
    println!(
        "\ncontroller_address\t{:?}", client.controller_address()
    );
    println!(
        "controller_balance\t{}\tXTZ",
        format_units(controller_balance, "ether")?
    );
    Ok(())
}
