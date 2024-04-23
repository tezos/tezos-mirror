// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;

/// Transaction type: a list of bytes serialized in hex format.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transaction(#[serde(with = "hex")] pub Vec<u8>);

/// Delayed transaction hash: A list of bytes serialized in hex format.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DelayedTransactionHash(#[serde(with = "hex")] pub Vec<u8>);

/// Etherlink block hash: A list of bytes serialized in hex format.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct BlockHash(#[serde(with = "hex")] pub Vec<u8>);

/// Proposals received by the sequencer. It contains the
/// transactions and delayed tranaction hashes to be included in the
/// next [Preblock], together with other metadata such as the
/// blueprint number, the timestamp, and the [BlockHash] of the latest
/// Etherlink block.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Proposal {
    pub transactions: Vec<Transaction>,

    pub delayed_transaction_hashes: Vec<DelayedTransactionHash>,

    pub previous_block_hash: BlockHash,

    pub timestamp: chrono::DateTime<Utc>,

    pub current_blueprint_number: u64,
}

/// Custom [Debug] implementation for [Proposal].
/// It differs from the implementation that would be provided by
/// the `[derive(Debug)]` macro, in that transactions and delayed
/// transaction hashes vectores are replaced with the corresponding length.
impl Debug for Proposal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Proposal")
            .field("transactions", &self.transactions.len())
            .field(
                "delayed_transaction_hashes",
                &self.delayed_transaction_hashes.len(),
            )
            .field("previous_block_hash", &self.previous_block_hash)
            .field("timestamp", &self.timestamp)
            .field("current_blueprint_number", &self.current_blueprint_number)
            .finish()
    }
}

/// [Preblock]s are produced by the sequencer as the result of
/// validating and acknowledging a proposal.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Preblock(pub Proposal);
