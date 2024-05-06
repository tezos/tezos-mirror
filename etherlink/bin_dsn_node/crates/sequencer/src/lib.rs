// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Sequencer sidecar components.

use client::SequencerClient;
use config::SequencerConfig;
use runner::SequencerRunner;
use tokio::sync::{broadcast, mpsc};

pub mod client;
pub mod config;
pub mod error;
pub mod rpc;
pub mod runner;

/// Create a pair of sequencer local client (clonable) and runner, wired together
pub fn sequencer(
    config: SequencerConfig,
    rx_shutdown: broadcast::Receiver<()>,
) -> (SequencerClient, SequencerRunner) {
    let (tx_proposals, rx_proposals) = mpsc::channel(config.channel_capacity);
    let (tx_preblocks, rx_preblocks) = broadcast::channel(config.channel_capacity);
    let runner = SequencerRunner::new(rx_proposals, tx_preblocks, rx_shutdown, config);
    let client = SequencerClient::new(tx_proposals, rx_preblocks);
    (client, runner)
}
