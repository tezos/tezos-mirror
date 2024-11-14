// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use dsn_core::types::Proposal;
use tokio::sync::{broadcast, mpsc};

#[derive(Debug, thiserror::Error)]
pub enum SequencerError {
    #[error("Could not send proposal to sequencer runner for processing")]
    CannotForwardProposal(#[from] mpsc::error::SendError<Proposal>),
    #[error("Could not receive next preblock from sequencer runner")]
    ProtocolRunnerCannotBroadcast(#[from] broadcast::error::RecvError),
    #[error("Broadcast receiver is lagging behind")]
    OutOfSync,
}
