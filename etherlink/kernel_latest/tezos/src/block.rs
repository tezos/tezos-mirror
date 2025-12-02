// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::enc_wrappers::{BlockHash, BlockNumber, OperationHash};
use crate::operation_result::OperationDataAndMetadata;
use primitive_types::H256;
use tezos_crypto_rs::blake2b::digest_256;
use tezos_data_encoding::enc as tezos_enc;
use tezos_enc::{BinError, BinWriter};
use tezos_smart_rollup::types::Timestamp;

#[derive(PartialEq, Debug, BinWriter)]
pub struct AppliedOperation {
    // OperationHash are 32 bytes long
    pub hash: OperationHash,
    pub branch: BlockHash,
    #[encoding(dynamic)]
    pub op_and_receipt: OperationDataAndMetadata,
}

// WIP: This structure will evolve to look like Tezos block
#[derive(PartialEq, Debug, BinWriter)]
pub struct TezBlock {
    pub hash: BlockHash,
    pub number: BlockNumber,
    pub previous_hash: BlockHash,
    pub timestamp: Timestamp,
    #[encoding(dynamic, list)]
    pub operations: Vec<AppliedOperation>,
}

impl TezBlock {
    pub fn genesis_block_hash() -> H256 {
        // This H256 comes from this b58 hash 'BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9'
        // That is the ghostnet genesis hash according to 'devtools/get_contracts/config.ml'
        H256::from_slice(
            &hex::decode(
                "8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed8423e7c02934",
            )
            .unwrap(),
        )
    }

    // This function must be used on a TezBlock whose hash field is H256::zero()
    fn hash(&self) -> Result<BlockHash, BinError> {
        let mut encoded_data = vec![];
        self.bin_write(&mut encoded_data)?;
        let hashed_data = digest_256(&encoded_data);
        Ok(BlockHash(H256::from_slice(&hashed_data)))
    }

    pub fn new(
        number: BlockNumber,
        timestamp: Timestamp,
        previous_hash: H256,
        operations: Vec<AppliedOperation>,
    ) -> Result<Self, BinError> {
        let block = Self {
            hash: BlockHash(H256::zero()), // Placeholder, will be computed
            number,
            timestamp,
            previous_hash: BlockHash(previous_hash),
            operations,
        };
        Ok(Self {
            hash: block.hash()?,
            ..block
        })
    }
}
