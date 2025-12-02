use primitive_types::{H256, U256};
use rlp::DecoderError;
use tezos_data_encoding::enc::{BinError, BinWriter};
use tezos_data_encoding::nom::NomReader;
use tezos_ethereum::{block::EthBlock, rlp_helpers::VersionedEncoding};
use tezos_smart_rollup::types::Timestamp;
use tezos_tezlink::block::TezBlock;

use crate::{
    blueprint_storage::{BlockHeader, ChainHeader},
    chains::ChainFamily,
};

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
            Self::Tezlink(block) => block.operations.len(),
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
            Self::Tezlink(block) => block.hash.0,
        }
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, BinError> {
        match self {
            Self::Etherlink(block) => Ok(block.to_bytes()),
            Self::Tezlink(block) => Ok(block.to_bytes()?),
        }
    }

    pub fn try_from_bytes(
        chain_family: &ChainFamily,
        bytes: &[u8],
    ) -> Result<L2Block, DecoderError> {
        match chain_family {
            ChainFamily::Evm => {
                Ok(L2Block::Etherlink(Box::new(EthBlock::from_bytes(bytes)?)))
            }
            ChainFamily::Michelson => {
                let block = TezBlock::nom_read_exact(bytes)
                    .map_err(|_| DecoderError::Custom("Binary decoding error"))?;
                Ok(L2Block::Tezlink(block))
            }
        }
    }
}
