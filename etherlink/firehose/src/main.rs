// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use anyhow::Result;
use clap::{Parser, Subcommand, ValueEnum};
use ethers::core::rand::{self, Rng};
use ethers::types::{H160, U256};
use ethers::utils::{format_units, parse_units, ConversionError, Units};
use tokio::task::spawn_blocking;
use tokio::try_join;

use std::path::PathBuf;
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
        #[arg(short, long, default_value_t = false)]
        random_nonce: bool,
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
    LargePayload,
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

            let config = create_config(config_path, None, true).await?;

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
            random_nonce,
        } => {
            let config = create_config(config_path, Some(workers), false).await?;

            let client = Client::new(&config).await?;

            let setup = create_setup(&config, &client, workers, true, false, false).await?;

            if random_nonce {
                setup.xtz_transfers(get_random_nonce, 0).await?;
            } else {
                setup
                    .xtz_transfers(|address| client.nonce(address), 0)
                    .await?;
            }
        }
        Commands::Flood {
            workers,
            kind: FloodType::LargePayload,
            random_nonce,
        } => {
            let config = create_config(config_path, Some(workers), false).await?;

            let client = Client::new(&config).await?;

            let setup = create_setup(&config, &client, workers, true, false, false).await?;

            if random_nonce {
                setup.xtz_transfers(get_random_nonce, 100_000_000).await?;
            } else {
                setup
                    .xtz_transfers(|address| client.nonce(address), 100_000_000)
                    .await?;
            }
        }
        Commands::Flood {
            workers,
            kind: FloodType::Erc20,
            random_nonce: _,
        } => {
            let config = create_config(config_path, Some(workers), true).await?;

            let client = Client::new(&config).await?;

            let setup = create_setup(&config, &client, workers, true, true, false).await?;

            setup.erc20_transfers().await?;
        }
        Commands::Cleanup => {
            let mut config = Config::load(&config_path).await?;
            let client = Client::new(&config).await?;

            let _setup =
                create_setup(&config, &client, config.workers().len(), false, false, true).await?;

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

pub async fn create_config(
    config_path: PathBuf,
    workers: Option<usize>,
    use_erc20: bool,
) -> Result<Config> {
    let mut config = Config::load(&config_path).await?;
    if use_erc20 {
        config = ensure_erc20_contract(config).await?
    }
    match workers {
        Some(workers) => config.generate_workers(workers),
        _ => {}
    }
    config.save(&config_path).await?;
    Ok(config)
}

pub async fn create_setup<'a>(
    config: &'a Config,
    client: &'a Client,
    workers: usize,
    fund_xtz: bool,
    fund_erc20: bool,
    defund: bool,
) -> Result<Setup<'a>> {
    let setup = Setup::new(config, client, workers)?;
    if defund {
        setup.defund_workers_xtz().await?;
    } else {
        if fund_xtz {
            setup.fund_workers_xtz(ONE_XTZ_IN_WEI.into()).await?;
        }
        if fund_erc20 {
            setup.fund_workers_erc20(ONE_XTZ_IN_WEI.into()).await?;
        }
    }
    Ok(setup)
}

pub async fn get_random_nonce(_address: H160) -> Result<U256> {
    Ok(U256::from(rand::thread_rng().gen::<u64>()))
}
