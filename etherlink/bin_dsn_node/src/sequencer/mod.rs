// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::sync::Arc;

pub mod protocol;

pub mod rpc_server;

pub mod cli;

const BUFFER_CAPACITY: usize = 10;

/// Starts the sequencer sidecar. In particular, this functions starts the
/// sequencer
pub async fn start(args: cli::Args) -> Result<(), Box<dyn std::error::Error>> {
    let cli::Args {
        rpc_address,
        preblock_time,
    } = args;
    let (tx_proposals, rx_proposals) = tokio::sync::mpsc::channel(BUFFER_CAPACITY);
    let (tx_preblocks, rx_preblocks) = tokio::sync::broadcast::channel(BUFFER_CAPACITY);
    let protocol_runner =
        protocol::ProtocolRunner::spawn(rx_proposals, tx_preblocks, preblock_time.into());
    // It is fine to leak the protocol client, as this reference must be valid for the whole program execution
    // TODO: Handle graceful shutdown
    // TODO: Find a way not to leak the protocol client
    let protocol_client: Arc<protocol::ProtocolClient> =
        Arc::new(protocol::ProtocolClient::new(tx_proposals, rx_preblocks));
    // TODO: Move to async handler, handle graceful shutdown.
    let server = tokio::spawn(rpc_server::start_server(protocol_client, rpc_address));
    //TODO: Handle shutdowns
    let _ = tokio::join!(server, protocol_runner);
    Ok(())
}
