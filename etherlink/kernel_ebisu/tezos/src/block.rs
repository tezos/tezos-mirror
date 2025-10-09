// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::enc_wrappers::{BlockHash, BlockNumber, OperationHash};
use crate::operation_result::OperationDataAndMetadata;
use primitive_types::H256;
use tezos_crypto_rs::blake2b::digest_256;
use tezos_data_encoding::enc as tezos_enc;
use tezos_data_encoding::nom::{self as tezos_nom};
use tezos_enc::{BinError, BinWriter};
use tezos_nom::NomReader;
use tezos_smart_rollup::types::Timestamp;

#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub struct AppliedOperation {
    // OperationHash are 32 bytes long
    pub hash: OperationHash,
    pub branch: BlockHash,
    #[encoding(dynamic)]
    pub op_and_receipt: OperationDataAndMetadata,
}

// WIP: This structure will evolve to look like Tezos block
#[derive(PartialEq, Debug, BinWriter, NomReader)]
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

#[cfg(test)]
mod tests {
    use crate::operation::zip_operations;
    use primitive_types::H256;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::nom::NomReader;
    use tezos_smart_rollup::types::Timestamp;

    use crate::operation_result::{
        OperationBatchWithMetadata, OperationDataAndMetadata, OperationResult,
        OperationResultSum, RevealSuccess,
    };

    use super::{AppliedOperation, TezBlock};

    pub fn block_roundtrip(block: TezBlock) {
        let bytes = block
            .to_bytes()
            .expect("Block encoding should have succeeded");
        let decoded_block =
            TezBlock::nom_read_exact(&bytes).expect("Block should be decodable");
        assert_eq!(block, decoded_block, "Roundtrip failed on {block:?}")
    }

    fn dummy_applied_operation() -> AppliedOperation {
        let hash = H256::random().into();
        let data = crate::operation::make_dummy_reveal_operation();
        let receipt = vec![OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: crate::operation_result::ContentResult::Applied(RevealSuccess {
                consumed_gas: 0u64.into(),
            }),
            internal_operation_results: vec![],
        })];
        AppliedOperation {
            hash,
            branch: data.branch.clone(),
            op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                OperationBatchWithMetadata {
                    operations: zip_operations(data.clone(), receipt),
                    signature: data.signature,
                },
            ),
        }
    }

    fn dummy_tezblock(operations: Vec<AppliedOperation>) -> TezBlock {
        let number = 1u32.into();
        let timestamp = Timestamp::from(0);
        let previous_hash = TezBlock::genesis_block_hash();
        TezBlock::new(number, timestamp, previous_hash, operations)
            .expect("Block creation should have succeeded")
    }

    #[test]
    fn test_empty_block_rlp_roundtrip() {
        block_roundtrip(dummy_tezblock(vec![]));
    }

    #[test]
    fn test_block_rlp_roundtrip() {
        block_roundtrip(dummy_tezblock(vec![
            dummy_applied_operation(),
            dummy_applied_operation(),
        ]));
    }
}
