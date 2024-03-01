// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use anyhow::Result;
use clap::{Parser, Subcommand, ValueEnum};
use ethers::types::H160;
use ethers::utils::{format_units, parse_units, ConversionError, Units};
use tokio::task::spawn_blocking;
use tokio::try_join;

use std::str::FromStr;

mod client;
mod config;
mod scenario;

use client::Client;
use config::Config;
use scenario::Setup;

const ONE_XTZ_IN_WEI: u64 = 10_u64.pow(18);

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
    #[command(
        long_about = "Set the RPC endpoint to be used, and optionally the controller private key."
    )]
    Configure {
        #[arg(long)]
        endpoint: String,
        #[arg(long, value_name = "CONTROLLER_SK")]
        controller: Option<String>,
    },
    Status,
    #[command(long_about = "Perform a transfer from the controller account to another address")]
    Transfer {
        #[arg(long)]
        to: String,
        #[arg(long)]
        amount: String,
        #[arg(default_value = "xtz")]
        kind: TransferKind,
    },
    Flood {
        #[arg(default_value = "wei")]
        kind: TransferKind,
        #[arg(long, default_value = "1")]
        fund_amount: String,
        #[arg(long)]
        tps: usize,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum TransferKind {
    Xtz,
    Gwei,
    Wei,
    Erc20,
}

impl TryInto<Units> for TransferKind {
    type Error = ConversionError;

    fn try_into(self) -> std::result::Result<Units, ConversionError> {
        let u = match self {
            TransferKind::Xtz => Units::Ether,
            TransferKind::Gwei => Units::Gwei,
            TransferKind::Wei => Units::Wei,
            k => return Err(ConversionError::UnrecognizedUnits(format!("{k:?}"))),
        };

        Ok(u)
    }
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
        Commands::Transfer { kind, amount, to } => {
            let amount = parse_units(amount, kind)?.into();
            let to = H160::from_str(&to)?;

            let config = Config::load(&config_path).await?;
            let client = Client::new(&config).await?;
            client.controller_xtz_transfer(to, amount).await?;
        }
        Commands::Flood {
            kind,
            fund_amount,
            tps,
        } => {
            let fund_amount = parse_units(fund_amount, kind)?.into();
            let required_workers = 2 * tps;

            let mut config = Config::load(&config_path).await?;
            config.generate_workers(required_workers);
            config.save(&config_path).await?;

            let client = Client::new(&config).await?;

            let setup = Setup::new(&config, &client, required_workers)?;

            setup.fund_workers_xtz(fund_amount).await?;
            //    scenario::fund_workers(&config, &client, required_workers).await?;
        }
    };

    Ok(())
}

// retrieve controller balance & current gas price
async fn status_check(config: &Config) -> Result<()> {
    let client = Client::new(config).await?;
    let (gas_price, controller_balance) =
        try_join!(client.gas_price(), client.controller_balance())?;
    println!("chain_id\t\t{}", client.chain_id());
    println!("gas_price\t\t{}\tGwei", format_units(gas_price, "gwei")?);
    println!("\ncontroller_address\t{:?}", client.controller_address());
    println!(
        "controller_balance\t{}\tXTZ",
        format_units(controller_balance, "ether")?
    );
    Ok(())
}
