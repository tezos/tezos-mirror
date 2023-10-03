// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::eth_gen::OwnedHash;
use crate::rlp_helpers::{
    append_u256_le, decode_field, decode_field_h256, decode_field_u256_le,
    decode_transaction_hash_list, next,
};
use crate::transaction::TransactionHash;
use primitive_types::{H160, H256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
use tezos_smart_rollup_encoding::timestamp::Timestamp;

/// All data for an Ethereum block.
///
/// This data does not change for the duration of the block. All balues are
/// updated when the block is finalized and may change for the next block.
pub struct BlockConstants {
    /// Price of one unit of gas in Wei
    pub gas_price: U256,
    /// The number of the current block
    pub number: U256,
    /// Who is the beneficiary of the current block
    pub coinbase: H160,
    /// Unix date/time of the current block - when was the previous block finished
    pub timestamp: U256,
    /// Mining difficulty of the current block. This relates to PoW, and we can set
    /// the value to an arbitrary value.
    pub difficulty: U256,
    /// Gas limit for the current block.
    pub gas_limit: u64,
    /// The base fee per gas for doing a transaction.
    pub base_fee_per_gas: U256,
    /// Identifier for the chain. Normally this would identify the chain (Ethereum
    /// main net, or some other net). We can use it to identify rollup EVM kernel.
    pub chain_id: U256,
}

impl BlockConstants {
    /// Return the first block of the chain (genisis).
    /// TODO find suitable values for gas_limit et.c.
    /// To be done in <https://gitlab.com/tezos/tezos/-/milestones/114>.
    pub fn first_block(timestamp: U256, chain_id: U256, base_fee_per_gas: U256) -> Self {
        Self {
            gas_price: U256::one(),
            number: U256::zero(),
            coinbase: H160::zero(),
            timestamp,
            difficulty: U256::zero(),
            gas_limit: 1u64,
            base_fee_per_gas,
            chain_id,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct L2Block {
    // This choice of a L2 block representation is totally
    // arbitrarily based on what is an Ethereum block and is
    // subject to change.
    pub number: U256,
    pub hash: H256,
    pub parent_hash: H256,
    pub nonce: U256,
    pub sha3_uncles: OwnedHash,
    pub logs_bloom: Option<OwnedHash>,
    pub transactions_root: OwnedHash,
    pub state_root: OwnedHash,
    pub receipts_root: OwnedHash,
    pub miner: OwnedHash,
    pub difficulty: U256,
    pub total_difficulty: U256,
    pub extra_data: OwnedHash,
    pub gas_limit: u64,
    pub gas_used: U256,
    pub timestamp: Timestamp,
    pub transactions: Vec<TransactionHash>,
    pub uncles: Vec<OwnedHash>,
}

impl L2Block {
    const DUMMY_HASH: &str = "0000000000000000000000000000000000000000";
    const BLOCK_HASH_SIZE: usize = 32;

    fn dummy_hash() -> OwnedHash {
        L2Block::DUMMY_HASH.into()
    }

    fn dummy_block_hash() -> H256 {
        H256([0; L2Block::BLOCK_HASH_SIZE])
    }

    pub fn new(
        number: U256,
        transactions: Vec<TransactionHash>,
        timestamp: Timestamp,
        parent_hash: H256,
    ) -> Self {
        L2Block {
            number,
            hash: H256(number.into()),
            parent_hash,
            timestamp,
            transactions,
            ..Self::default()
        }
    }

    pub fn constants(&self, chain_id: U256, base_fee_per_gas: U256) -> BlockConstants {
        let timestamp = U256::from(self.timestamp.as_u64());
        BlockConstants {
            gas_price: U256::one(),
            number: self.number,
            coinbase: H160::zero(),
            timestamp,
            difficulty: self.difficulty,
            gas_limit: self.gas_limit,
            base_fee_per_gas,
            chain_id,
        }
    }
}

impl Default for L2Block {
    fn default() -> Self {
        Self {
            number: U256::default(),
            hash: H256::default(),
            parent_hash: L2Block::dummy_block_hash(),
            nonce: U256::zero(),
            sha3_uncles: L2Block::dummy_hash(),
            logs_bloom: None,
            transactions_root: L2Block::dummy_hash(),
            state_root: L2Block::dummy_hash(),
            receipts_root: L2Block::dummy_hash(),
            miner: L2Block::dummy_hash(),
            difficulty: U256::zero(),
            total_difficulty: U256::zero(),
            extra_data: L2Block::dummy_hash(),
            gas_limit: 1u64,
            gas_used: U256::zero(),
            timestamp: Timestamp::from(0),
            transactions: Vec::new(),
            uncles: Vec::new(),
        }
    }
}

impl Encodable for L2Block {
    fn rlp_append(&self, s: &mut RlpStream) {
        s.begin_list(6);
        append_u256_le(s, self.number);
        s.append(&self.hash);
        s.append(&self.parent_hash);
        let transactions_bytes: Vec<Vec<u8>> =
            self.transactions.iter().map(|x| x.to_vec()).collect();
        s.append_list::<Vec<u8>, _>(&transactions_bytes);
        s.append(&self.gas_used);
        s.append(&self.timestamp.i64().to_le_bytes().to_vec());
    }
}

impl Decodable for L2Block {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        if decoder.is_list() {
            if Ok(6) == decoder.item_count() {
                let mut it = decoder.iter();
                let number: U256 = decode_field_u256_le(&next(&mut it)?, "number")?;
                let hash: H256 = decode_field_h256(&next(&mut it)?, "hash")?;
                let parent_hash: H256 =
                    decode_field_h256(&next(&mut it)?, "parent_hash")?;
                let transactions: Vec<TransactionHash> =
                    decode_transaction_hash_list(&next(&mut it)?, "transactions")?;
                let gas_used: U256 = decode_field_u256_le(&next(&mut it)?, "gas_used")?;
                let timestamp_bytes =
                    decode_field::<Vec<u8>>(&next(&mut it)?, "timestamp")?;
                let timestamp: Timestamp =
                    i64::from_le_bytes(timestamp_bytes.try_into().map_err(|_| {
                        DecoderError::Custom(
                            "Invalid conversion from timestamp vector of bytes to bytes.",
                        )
                    })?)
                    .into();
                Ok(L2Block {
                    number,
                    hash,
                    parent_hash,
                    gas_used,
                    timestamp,
                    transactions,
                    ..Default::default()
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }
}

#[cfg(test)]
mod tests {

    use super::L2Block;
    use crate::rlp_helpers::FromRlpBytes;
    use crate::transaction::TRANSACTION_HASH_SIZE;
    use primitive_types::{H256, U256};
    use rlp::Encodable;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    fn block_encoding_roundtrip(v: L2Block) {
        let bytes = v.rlp_bytes();
        let v2 = L2Block::from_rlp_bytes(&bytes).expect("L2Block should be decodable");
        assert_eq!(v, v2, "Roundtrip failed on {:?}", v)
    }

    fn dummy_block(tx_length: usize) -> L2Block {
        L2Block {
            number: U256::from(42),
            hash: H256::from([3u8; 32]),
            parent_hash: H256::from([2u8; 32]),
            timestamp: Timestamp::from(10i64),
            transactions: vec![[0u8; TRANSACTION_HASH_SIZE]; tx_length],
            ..Default::default()
        }
    }

    #[test]
    fn roundtrip_rlp() {
        for tx_length in 0..3 {
            let v: L2Block = dummy_block(tx_length);
            block_encoding_roundtrip(v);
        }
    }
}
