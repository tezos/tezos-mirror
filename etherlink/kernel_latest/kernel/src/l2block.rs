use primitive_types::{H256, U256};
use tezos_ethereum::{block::EthBlock, rlp_helpers::VersionedEncoding};
use tezos_smart_rollup::types::Timestamp;
use tezos_tezlink::block::TezBlock;

use crate::blueprint_storage::{BlockHeader, ChainHeader};

#[derive(PartialEq, Debug)]
pub enum L2Block {
    // The difference in size between the two variant is too big
    // So we need to Box the EthBlock struct to prevent a warning
    // When TezBlock will be complete we could remove the Box or
    // box the whole L2Block structure enum instead
    Etherlink(Box<EthBlock>),
    Tezlink(TezBlock),
}

impl L2Block {
    pub fn number(&self) -> U256 {
        match self {
            Self::Etherlink(block) => block.number,
            Self::Tezlink(block) => block.number.into(),
        }
    }

    pub fn timestamp(&self) -> Timestamp {
        match self {
            Self::Etherlink(block) => block.timestamp,
            Self::Tezlink(block) => block.timestamp,
        }
    }

    pub fn number_of_transactions(&self) -> usize {
        match &self {
            Self::Etherlink(block) => block.transactions.len(),
            Self::Tezlink(block) => block.operations.list.len(),
        }
    }

    pub fn gas_used(&self) -> U256 {
        match self {
            Self::Etherlink(block) => block.gas_used,
            Self::Tezlink(_) => U256::zero(),
        }
    }

    pub fn header(self) -> BlockHeader<ChainHeader> {
        match self {
            Self::Etherlink(block) => (*block).into(),
            Self::Tezlink(block) => block.into(),
        }
    }

    pub fn hash(&self) -> H256 {
        match self {
            Self::Etherlink(block) => block.hash,
            Self::Tezlink(block) => H256(*block.hash),
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            Self::Etherlink(block) => block.to_bytes(),
            Self::Tezlink(block) => block.to_bytes(),
        }
    }
}
