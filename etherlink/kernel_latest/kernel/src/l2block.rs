use tezos_ethereum::block::EthBlock;
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
    pub fn header(self) -> BlockHeader<ChainHeader> {
        match self {
            Self::Etherlink(block) => (*block).into(),
            Self::Tezlink(block) => block.into(),
        }
    }
}
