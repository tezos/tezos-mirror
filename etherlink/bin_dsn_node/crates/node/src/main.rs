// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![doc = include_str!("../README.md")]

mod bundler;
mod keyholder;
mod sequencer;
mod shutdown;

use clap::{command, Parser, Subcommand};
use tracing::{info, subscriber::set_global_default};
use tracing_subscriber::filter::EnvFilter;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
    /// Logging level (off, error, warn, info, debug, trace)
    #[arg(short, long, default_value = "debug")]
    log_level: String,
}

#[derive(Subcommand, Clone, Debug)]
enum Commands {
    /// Run the dsn node in bundler mode
    Bundler(bundler::Args),
    /// Run the dsn node in sequencer sidecar mode
    Sequencer(sequencer::Args),
    /// Run the dsn node in keyholder mode
    Keyholder(keyholder::Args),
}

fn init_tracing(log_level: &str) {
    let env_filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(log_level));

    let subscriber_builder =
        tracing_subscriber::fmt::Subscriber::builder().with_env_filter(env_filter);

    let subscriber = subscriber_builder.with_writer(std::io::stderr).finish();
    set_global_default(subscriber).expect("Failed to set subscriber");
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();
    init_tracing(&cli.log_level);

    info!("DSN node is launching...");

    match cli.command {
        Commands::Bundler(args) => bundler::run(args).await,
        Commands::Sequencer(args) => sequencer::run(args).await,
        Commands::Keyholder(args) => keyholder::run(args).await,
    }
}
