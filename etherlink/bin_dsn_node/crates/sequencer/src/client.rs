// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use dsn_core::types::{Preblock, Proposal};
use tokio::sync::{broadcast, mpsc};

use crate::error::SequencerError;

#[derive(Debug)]
pub struct SequencerClient {
    tx_proposals: mpsc::Sender<Proposal>,
    rx_preblocks: broadcast::Receiver<Preblock>,
}

impl SequencerClient {
    pub fn new(
        tx_proposals: mpsc::Sender<Proposal>,
        rx_preblocks: broadcast::Receiver<Preblock>,
    ) -> Self {
        Self {
            tx_proposals,
            rx_preblocks,
        }
    }

    pub async fn submit_proposal(&self, proposal: Proposal) -> Result<(), SequencerError> {
        self.tx_proposals
            .send(proposal.clone())
            .await
            .map_err(Into::into)
    }

    pub async fn preblock_streams(&self) -> broadcast::Receiver<Preblock> {
        self.rx_preblocks.resubscribe()
    }
}

impl Clone for SequencerClient {
    fn clone(&self) -> Self {
        Self {
            tx_proposals: self.tx_proposals.clone(),
            rx_preblocks: self.rx_preblocks.resubscribe(),
        }
    }
}
