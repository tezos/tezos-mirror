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
mod contracts;
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
        #[arg(long)]
        workers: usize,
        #[arg(long)]
        kind: FloodType,
    },
    #[command(long_about = "Move XTZ funds from any workers back to the controller")]
    Cleanup,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum TransferKind {
    Xtz,
    Gwei,
    Wei,
    Erc20,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum FloodType {
    Xtz,
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
        Commands::Transfer {
            kind: TransferKind::Erc20,
            amount,
            to,
        } => {
            let amount = amount.parse::<u64>()?.into();
            let to = H160::from_str(&to)?;

            // deploy erc20 if not already done
            let config = Config::load(&config_path).await?;
            let config = ensure_erc20_contract(config).await?;
            config.save(&config_path).await?;

            // transfer
            let client = Client::new(&config).await?;
            let setup = Setup::new(&config, &client, config.workers().len())?;

            setup.mint_and_transfer_erc20(amount, to).await?;
        }
        Commands::Transfer { kind, amount, to } => {
            let amount = parse_units(amount, kind)?.into();
            let to = H160::from_str(&to)?;

            let config = Config::load(&config_path).await?;
            let client = Client::new(&config).await?;
            client.controller_xtz_transfer(to, amount).await?;
        }
        Commands::Flood {
            workers,
            kind: FloodType::Xtz,
        } => {
            let mut config = Config::load(&config_path).await?;
            config.generate_workers(workers);
            config.save(&config_path).await?;

            let client = Client::new(&config).await?;
            let setup = Setup::new(&config, &client, workers)?;

            setup.fund_workers_xtz(ONE_XTZ_IN_WEI.into()).await?;
            setup.xtz_transfers().await?;
        }
        Commands::Flood {
            workers,
            kind: FloodType::Erc20,
        } => {
            let config = Config::load(&config_path).await?;
            // deploy erc20 if not already done
            let mut config = ensure_erc20_contract(config).await?;
            config.save(&config_path).await?;

            // workers
            config.generate_workers(workers);
            config.save(&config_path).await?;

            let client = Client::new(&config).await?;
            let setup = Setup::new(&config, &client, workers)?;

            // Have to fund workers with xtz
            setup.fund_workers_xtz(ONE_XTZ_IN_WEI.into()).await?;

            // Now need to mint ERC20 for all workers
            setup.fund_workers_erc20(ONE_XTZ_IN_WEI.into()).await?;

            setup.erc20_transfers().await?;
        }
        Commands::Cleanup => {
            let mut config = Config::load(&config_path).await?;
            let client = Client::new(&config).await?;
            let setup = Setup::new(&config, &client, config.workers().len())?;

            setup.defund_workers_xtz().await?;

            config.cleanup_workers(&client).await?;
            config.save(&config_path).await?;
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

async fn ensure_erc20_contract(mut config: Config) -> Result<Config> {
    if config.erc20.is_none() {
        let client = Client::new(&config).await?;
        let setup = Setup::new(&config, &client, 0)?;

        let contract = setup.deploy_erc20().await?;

        config.erc20 = Some(contract);
    }
    Ok(config)
}