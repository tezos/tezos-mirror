// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Decentralised Sequencer Network (DSN) node
//! The DSN node is a binary written in rust that implements the components needed
//! to run a decentralised sequencer network with threshold encryption support.
//! The node provides three modes of operation:
//! * Bundler: a sidecar for the EVM observer node, responsible for encrypting
//! incoming transactions and forwarding them to the EVM sequencer node
//! * Sequencer sidecar: a sidecar for the EVM sequencer node, responsible for
//! aggregating transactions into proposals, collect attestations and decryption
//! shares for encrypted transactions from keyholders, and constructing blueprint
//! that will be applied by the EVM sequencer node
//! * Keyholder: a standalone node responsible for monitoring proposals from the
//! sequencer sidecar, attesting the order of transactions in the proposal and
//! providing decryption shares for encrypted transactions.

use clap::{command, Parser, Subcommand};
use futures::future::try_join_all;
use std::net::SocketAddr;
use tracing::info;
use url::Url;

mod bundler;
mod errors;
mod json_http_rpc;
mod rpc_encoding;
mod server;
mod shutdown;

mod rpc_helpers;
mod types;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Clone, Debug)]
enum Commands {
    /// Run the dsn node in bundler mode
    Bundler {
        #[arg(short, long, default_value_t = ([127,0,0,1], 3000).into())]
        listening_addr: SocketAddr,
        sequencer_endpoint: Url,
    },
    /// Run the dsn node in sequencer sidecar mode
    Sequencer,
    /// Run the dsn node in keyholder mode
    Keyholder,
}

#[tokio::main]
async fn main() {
    // construct a subscriber that prints formatted traces to stdout
    let subscriber = tracing_subscriber::FmtSubscriber::new();
    // use that subscriber to process traces emitted after this point
    tracing::subscriber::set_global_default(subscriber).expect("Failed to set subscriber");

    info!("DSN node is launching...");

    let mut shutdown = shutdown::Shutdown::default();
    let rx = shutdown.subscribe();
    let tx = shutdown.subscribe_to_shutdown();

    let node_handle = tokio::spawn(async move {
        let cli = Cli::parse();
        match cli.command {
            Commands::Bundler {
                listening_addr,
                sequencer_endpoint,
            } => {
                info!("Starting DSN node in Bundler mode");
                bundler::run(listening_addr, sequencer_endpoint, rx, tx).await
            }
            Commands::Sequencer => {
                info!("Starting DSN node in Sequencer sidecar mode");
                Ok::<(), ()>(())
            }
            Commands::Keyholder => {
                info!("Starting DSN node in Keyholder mode");
                Ok::<(), ()>(())
            }
        }
    });

    let shutdown_handle = tokio::spawn(async move { shutdown.run().await });

    try_join_all([node_handle, shutdown_handle]).await.unwrap();
}
