use std::net::SocketAddr;

use dsn_bundler::{client::BundlerClient, rpc::BundlerRpc};
use futures::future::try_join_all;
use tracing::info;
use url::Url;

use crate::shutdown::Shutdown;

/// Command line arguments for the `bin_dsn_node` when running in bundler mode.`
#[derive(Clone, Debug, clap::Args)]
pub struct Args {
    /// RPC server exposed at this address
    #[arg(short, long, default_value_t = ([127,0,0,1], 3000).into())]
    rpc_address: SocketAddr,
    /// Upstream EVM node url
    #[arg(short, long)]
    sequencer_url: Url,
}

pub async fn run(
    Args {
        rpc_address,
        sequencer_url,
    }: Args,
) {
    info!("Starting DSN node in Bundler mode");

    let shutdown = Shutdown::default();

    let client = BundlerClient::new(sequencer_url);
    let mut rpc = BundlerRpc::new(rpc_address, shutdown.subscribe(), client);

    let rpc_handle = tokio::spawn(async move { rpc.run().await });
    let shutdown_handle = tokio::spawn(async move { shutdown.run().await });

    try_join_all([rpc_handle, shutdown_handle]).await.unwrap();
}
