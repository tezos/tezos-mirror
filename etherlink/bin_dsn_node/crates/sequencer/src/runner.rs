// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MITxw

use dsn_core::types::{Preblock, Proposal};
use tokio::sync::{broadcast, mpsc};
use tracing::{debug, error, info, warn};

use crate::{config::SequencerConfig, error::SequencerError};

pub struct SequencerRunner {
    rx_proposal: mpsc::Receiver<Proposal>,
    tx_preblocks: broadcast::Sender<Preblock>,
    rx_shutdown: broadcast::Receiver<()>,
    config: SequencerConfig,
}

impl SequencerRunner {
    pub fn new(
        rx_proposal: mpsc::Receiver<Proposal>,
        tx_preblocks: broadcast::Sender<Preblock>,
        rx_shutdown: broadcast::Receiver<()>,
        config: SequencerConfig,
    ) -> Self {
        Self {
            rx_proposal,
            tx_preblocks,
            rx_shutdown,
            config,
        }
    }

    async fn run_inner(&mut self) -> Result<(), SequencerError> {
        info!("Starting threshold encryption protocol");
        loop {
            tokio::select! {
                Some(proposal) = self.rx_proposal.recv() => {
                    debug!("Received proposal {proposal:?}");
                    //TODO: Persist preblock once processed. Needed in the case of disconnections and catchup from EVM node is needed.
                    if let Err(e) = self.tx_preblocks.send(Preblock(proposal)) {
                        warn!("Cannot broadcast preblock: is the sequencer shutting down? {e:?}")
                    };
                    tokio::time::sleep(self.config.min_block_time).await;
                },
                _ = self.rx_shutdown.recv() => {
                    return Ok(())
                }
            }
        }
    }

    pub async fn run(&mut self) -> Result<(), ()> {
        match self.run_inner().await {
            Err(err) => {
                error!("Protocol runner failed with {}", err);
                Err(())
            }
            Ok(()) => {
                info!("Protocol runner terminated");
                Ok(())
            }
        }
    }
}
