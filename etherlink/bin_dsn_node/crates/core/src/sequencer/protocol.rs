// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::time::Duration;

use thiserror::Error;
use tokio::{
    sync::{
        broadcast::{self, error::RecvError},
        mpsc,
    },
    task::JoinHandle,
};
use tracing::{info, warn};

use crate::types::{Preblock, Proposal};

pub struct ProtocolRunner {
    rx_proposal: mpsc::Receiver<Proposal>,
    tx_preblocks: broadcast::Sender<Preblock>,
    min_block_time: Duration,
}

impl ProtocolRunner {
    pub fn spawn(
        rx_proposal: mpsc::Receiver<Proposal>,
        tx_preblocks: broadcast::Sender<Preblock>,
        min_block_time: Duration,
    ) -> JoinHandle<()> {
        tokio::spawn(async move {
            Self {
                rx_proposal,
                tx_preblocks,
                min_block_time,
            }
            .run()
            .await
        })
    }

    async fn run(&mut self) {
        let min_block_time = self.min_block_time;

        loop {
            tokio::select! {
            Some(proposal) = self.rx_proposal.recv() => {
                info!("Received proposal {proposal:?}");
                //TODO: Persist preblock once processed. Needed in the case of disconnections and catchup from EVM node is needed.

                            if let Err(e) = self.tx_preblocks.send(Preblock(proposal)) {
                                warn!("Cannot broadcast preblock: is the sequencer shutting down? {e:?}")
                            };
                    tokio::time::sleep(min_block_time).await;
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct ProtocolClient {
    tx_proposal: mpsc::Sender<Proposal>,
    rx_preblock: broadcast::Receiver<Preblock>,
}

impl Clone for ProtocolClient {
    fn clone(&self) -> Self {
        Self {
            tx_proposal: self.tx_proposal.clone(),
            rx_preblock: self.rx_preblock.resubscribe(),
        }
    }
}

#[derive(Debug, Error)]
pub enum ProtocolClientError {
    // Could not send the transaction to the Protocol Runner for processing
    #[error("Could not send proposal to protocol runner for processing")]
    CannotForwardProposal(Proposal),
    #[error("Could not receive next preblock from protocol runner")]
    ProtocolRunnerCannotBroadcast,
    #[error("Broadcast receiver is lagging behind")]
    OutOfSync,
}

impl From<broadcast::error::RecvError> for ProtocolClientError {
    fn from(e: tokio::sync::broadcast::error::RecvError) -> Self {
        match e {
            RecvError::Closed => ProtocolClientError::ProtocolRunnerCannotBroadcast,
            RecvError::Lagged(_) => ProtocolClientError::OutOfSync,
        }
    }
}

impl ProtocolClient {
    pub fn new(
        tx_proposal: mpsc::Sender<Proposal>,
        rx_preblock: broadcast::Receiver<Preblock>,
    ) -> Self {
        Self {
            tx_proposal,
            rx_preblock,
        }
    }

    pub async fn submit_proposal(&self, proposal: Proposal) -> Result<(), ProtocolClientError> {
        match self.tx_proposal.send(proposal.clone()).await {
            Ok(()) => Ok(()),
            Err(e) => {
                tracing::error!("Could not forward transaction from protocol client: {e:?}");
                Err(ProtocolClientError::CannotForwardProposal(proposal))
            }
        }
    }

    pub async fn preblock_streams(&self) -> broadcast::Receiver<Preblock> {
        self.rx_preblock.resubscribe()
    }
}
