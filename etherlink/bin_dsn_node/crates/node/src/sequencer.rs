// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::{net::SocketAddr, time::Duration};

use dsn_sequencer::{config::SequencerConfig, rpc::SequencerRpc, sequencer};
use futures::future::try_join_all;
use tracing::info;

use crate::shutdown::Shutdown;

const DEFAULT_RPC_ADDRESS: ([u8; 4], u16) = ([127, 0, 0, 1], 5303);

const DEFAULT_PREBLOCK_TIME: u16 = 500;

/// Command line arguments for the `bin_dsn_node` when running in sequencer mode.`
#[derive(Clone, Debug, clap::Args)]
pub struct Args {
    #[arg(short, long, default_value_t=DEFAULT_RPC_ADDRESS.into())]
    /// RPC server exposed on this address
    pub rpc_address: SocketAddr,
    #[arg(short, long)]
    /// Minimum time, in milliseconds, to produce a preblock.
    #[arg(short, long, default_value_t = DEFAULT_PREBLOCK_TIME)]
    pub preblock_time: u16,
}

pub async fn run(
    Args {
        rpc_address,
        preblock_time,
    }: Args,
) {
    info!("Starting DSN node in Sequencer sidecar mode");

    let shutdown = Shutdown::default();
    let config = SequencerConfig {
        // TODO: either merge args and config or rely on config only
        // and use args for generic purposes (e.g. specify config file path or logging level)
        min_block_time: Duration::from_millis(preblock_time as u64),
        ..Default::default()
    };

    let (client, mut runner) = sequencer(config, shutdown.subscribe());
    let mut rpc = SequencerRpc::new(rpc_address, shutdown.subscribe(), client);

    let seq_handle = tokio::spawn(async move { runner.run().await });
    let rpc_handle = tokio::spawn(async move { rpc.run().await });
    let shutdown_handle = tokio::spawn(async move { shutdown.run().await });

    try_join_all([seq_handle, rpc_handle, shutdown_handle])
        .await
        .unwrap();
}
