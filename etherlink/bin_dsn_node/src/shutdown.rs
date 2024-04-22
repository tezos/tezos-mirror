// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-Contributor: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Graceful shutdown helper.

use std::error::Error;
use std::sync::Arc;

use tokio::{
    signal::unix::{signal, SignalKind},
    sync::broadcast,
};
use tracing::info;

pub struct Shutdown {
    tx_shutdown: broadcast::Sender<Arc<dyn Error + Send + Sync>>,
    rx_shutdown: broadcast::Receiver<Arc<dyn Error + Send + Sync>>,
}

impl Default for Shutdown {
    fn default() -> Self {
        let (tx_shutdown, rx_shutdown) = broadcast::channel(1);
        Self {
            tx_shutdown,
            rx_shutdown,
        }
    }
}

impl Shutdown {
    pub fn subscribe(&self) -> broadcast::Receiver<Arc<dyn Error + Send + Sync>> {
        self.tx_shutdown.subscribe()
    }

    pub fn subscribe_to_shutdown(&self) -> broadcast::Sender<Arc<dyn Error + Send + Sync>> {
        self.tx_shutdown.clone()
    }

    pub async fn run(&mut self) -> Result<(), ()> {
        let mut sigterm = signal(SignalKind::terminate()).unwrap();
        let mut sigint = signal(SignalKind::interrupt()).unwrap();

        let err = tokio::select! {
            _ = sigterm.recv() => {info!("Received SIGTERM, initiating shutdown..."); Arc::new(crate::errors::Error::SigTerm)},
            _ = sigint.recv() => {info!("Received SIGINT, initiating shutdown..."); Arc::new(crate::errors::Error::SigInt)},
            err = self.rx_shutdown.recv() => {info!("Received error {:?}, initiating shutdown...", err.clone().unwrap()); err.unwrap()},
        };

        self.tx_shutdown.send(err).map(|_| ()).map_err(|_| ())
    }
}
