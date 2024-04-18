use chrono::Utc;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transaction(#[serde(with = "hex")] pub Vec<u8>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct DelayedTransactionHash(#[serde(with = "hex")] pub Vec<u8>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct BlockHash(#[serde(with = "hex")] pub Vec<u8>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Proposal {
    pub transactions: Vec<Transaction>,

    pub delayed_transaction_hashes: Vec<DelayedTransactionHash>,

    pub previous_block_hash: BlockHash,

    pub timestamp: chrono::DateTime<Utc>,

    pub current_blueprint_number: u64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Preblock(pub Option<Proposal>);
