// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::enc_wrappers::{BlockHash, BlockNumber, OperationHash};
use crate::operation_result::OperationDataAndMetadata;
use primitive_types::H256;
use rlp::{Decodable, Encodable};
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

#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub struct OperationsWithReceipts {
    pub list: Vec<AppliedOperation>,
}

impl Encodable for OperationsWithReceipts {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        let bytes = self.to_bytes().unwrap_or_else(|_| {
            let operations = OperationsWithReceipts { list: vec![] };
            // This is a "safe" unwrap as we know exactly what we're trying to
            // convert to bytes.
            operations.to_bytes().unwrap()
        });
        s.append_internal(&bytes);
    }
}

impl Decodable for OperationsWithReceipts {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let bytes = rlp.data()?;
        Self::nom_read_exact(bytes).map_err(|_| {
            rlp::DecoderError::Custom("Failed to decode OperationsWithReceipts")
        })
    }
}

#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub struct TezBlock {
    pub hash: BlockHash,
    pub number: BlockNumber,
    pub previous_hash: BlockHash,
    pub timestamp: Timestamp,
    pub operations: OperationsWithReceipts,
}

const VERSION: u8 = 0;

impl Encodable for TezBlock {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.begin_list(6);
        s.append(&VERSION);
        s.append(&self.hash);
        s.append(&self.number);
        s.append(&self.previous_hash);
        s.append(&self.timestamp.i64().to_le_bytes().to_vec());
        s.append(&self.operations);
    }
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
        let encoded_data = self.to_bytes();
        let hashed_data = digest_256(&encoded_data);
        Ok(BlockHash(H256::from_slice(&hashed_data)))
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.rlp_bytes().to_vec()
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
            operations: OperationsWithReceipts { list: operations },
        };
        Ok(Self {
            hash: block.hash()?,
            ..block
        })
    }
}

#[cfg(test)]
mod tests {
    use primitive_types::H256;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::nom::NomReader;
    use tezos_smart_rollup::types::Timestamp;

    use crate::operation_result::{
        OperationBatchWithMetadata, OperationDataAndMetadata, OperationResult,
        OperationResultSum, OperationWithMetadata, RevealSuccess,
    };

    use super::{AppliedOperation, TezBlock};

    pub fn block_roundtrip(block: TezBlock) {
        let bytes = BinWriter::to_bytes(&block).expect("Encoding should succeed");
        let decoded_block =
            TezBlock::nom_read_exact(&bytes).expect("Block should be decodable");
        assert_eq!(block, decoded_block, "Roundtrip failed on {block:?}")
    }

    fn dummy_applied_operation() -> AppliedOperation {
        let hash = H256::random().into();
        let data = crate::operation::make_dummy_reveal_operation();
        let operations = vec![OperationWithMetadata {
            content: data.content[0].clone(),
            receipt: OperationResultSum::Reveal(OperationResult {
                balance_updates: vec![],
                result: crate::operation_result::ContentResult::Applied(RevealSuccess {
                    consumed_milligas: 0u64.into(),
                }),
                internal_operation_results: vec![],
            }),
        }];
        AppliedOperation {
            hash,
            branch: data.branch.clone(),
            op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                OperationBatchWithMetadata {
                    operations,
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
