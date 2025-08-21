// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H256, U256};
use std::{array::TryFromSliceError, str::FromStr};
use tezos_crypto_rs::blake2b::digest_256;
use tezos_smart_rollup::types::Timestamp;

// WIP: This structure will evolve to look like Tezos block
#[derive(PartialEq, Debug)]
pub struct TezBlock {
    pub number: U256,
    pub hash: H256,
    pub timestamp: Timestamp,
    pub previous_hash: H256,
}

impl TezBlock {
    pub fn genesis_block_hash() -> H256 {
        // This H256 comes from this b58 hash 'BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9'
        // That is the ghostnet genesis hash according to 'devtools/get_contracts/config.ml'
        H256::from_str("8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed8423e7c02934")
            .unwrap()
    }

    fn hash(&self) -> H256 {
        let encoded_data = self.to_bytes();
        let hashed_data = digest_256(&encoded_data);
        H256::from_slice(&hashed_data)
    }

    pub fn new(number: U256, timestamp: Timestamp, previous_hash: H256) -> Self {
        let block = Self {
            hash: H256::zero(),
            number,
            timestamp,
            previous_hash,
        };
        Self {
            hash: block.hash(),
            ..block
        }
    }

    // Encoded size for parameter were taken from this command:
    // `octez-codec describe block_header binary schema`
    pub fn to_bytes(&self) -> Vec<u8> {
        let Self {
            hash: _,
            number,
            timestamp,
            previous_hash,
        } = self;
        let mut data = vec![];

        // Encode all block fields
        let num_enc: [u8; 4] = number.as_u32().to_be_bytes();
        let predecessor: [u8; 32] = previous_hash.to_fixed_bytes();
        let time_enc: [u8; 8] = timestamp.i64().to_le_bytes();

        // Append encoded fields to data
        data.extend_from_slice(&num_enc);
        data.extend_from_slice(&predecessor);
        data.extend_from_slice(&time_enc);

        data
    }

    pub fn try_from_bytes(bytes: &[u8]) -> Result<Self, TryFromSliceError> {
        let number = U256::from_big_endian(&bytes[0..4]);

        let previous_hash = H256::from_slice(&bytes[4..36]);

        // Decode the timestamp
        let timestamp_array: [u8; 8] = bytes[36..44].try_into()?;
        let timestamp = Timestamp::from(i64::from_le_bytes(timestamp_array));

        Ok(TezBlock::new(number, timestamp, previous_hash))
    }
}

#[cfg(test)]
mod tests {
    use primitive_types::U256;
    use tezos_smart_rollup::types::Timestamp;

    use super::TezBlock;

    pub fn block_roundtrip(block: TezBlock) {
        let bytes = block.to_bytes();
        let decoded_block =
            TezBlock::try_from_bytes(&bytes).expect("Block should be decodable");
        assert_eq!(block, decoded_block, "Roundtrip failed on {:?}", block)
    }

    fn dummy_tezblock() -> TezBlock {
        let number = U256::one();
        let timestamp = Timestamp::from(0);
        let previous_hash = TezBlock::genesis_block_hash();
        TezBlock::new(number, timestamp, previous_hash)
    }

    #[test]
    fn test_block_rlp_roundtrip() {
        block_roundtrip(dummy_tezblock());
    }
}
