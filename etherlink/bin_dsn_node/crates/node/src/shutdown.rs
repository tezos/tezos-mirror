// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-Contributor: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Graceful shutdown helper.

use tokio::{
    signal::unix::{signal, SignalKind},
    sync::broadcast,
};
use tracing::info;

pub struct Shutdown {
    tx_shutdown: broadcast::Sender<()>,
}

impl Default for Shutdown {
    fn default() -> Self {
        let (tx_shutdown, _) = broadcast::channel(1);
        Self { tx_shutdown }
    }
}

impl Shutdown {
    pub fn subscribe(&self) -> broadcast::Receiver<()> {
        self.tx_shutdown.subscribe()
    }

    pub async fn run(&self) -> Result<(), ()> {
        let mut sigterm = signal(SignalKind::terminate()).unwrap();
        let mut sigint = signal(SignalKind::interrupt()).unwrap();

        tokio::select! {
            _ = sigterm.recv() => info!("Received SIGTERM, initiating shutdown..."),
            _ = sigint.recv() => info!("Received SIGINT, initiating shutdown..."),
        };

        self.tx_shutdown.send(()).map(|_| ()).map_err(|_| ())
    }
}
