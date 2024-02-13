// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::eth_gen::OwnedHash;
use crate::helpers::{bytes_of_u256, hex_of_option};
use crate::rlp_helpers::{
    append_option_explicit, append_timestamp, append_u256_le, append_u64_le,
    decode_field, decode_field_h256, decode_field_u256_le, decode_field_u64_le,
    decode_option_explicit, decode_timestamp, decode_transaction_hash_list, next,
};
use crate::transaction::TransactionHash;
use ethbloom::Bloom;
use primitive_types::{H160, H256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
use sha3::{Digest, Keccak256};
use tezos_smart_rollup_encoding::timestamp::Timestamp;

/// Container for fee calculation.
#[derive(Debug, Clone, Copy)]
pub struct BlockFees {
    base_fee_per_gas: U256,
    da_fee_per_byte: U256,
}

impl BlockFees {
    /// Setup fee information for the current block
    pub const fn new(
        base_fee_per_gas: U256,
        da_fee_per_byte: U256,
    ) -> Self {
        Self {
            base_fee_per_gas,
            da_fee_per_byte,
        }
    }

    /// The base fee per gas for doing a transaction within the current block.
    #[inline(always)]
    pub const fn base_fee_per_gas(&self) -> U256 {
        self.base_fee_per_gas
    }

    /// The da fee per byte charged per transaction.
    #[inline(always)]
    pub const fn da_fee_per_byte(&self) -> U256 {
        self.da_fee_per_byte
    }
}

/// All data for an Ethereum block.
///
/// This data does not change for the duration of the block. All values are
/// updated when the block is finalized and may change for the next block.
pub struct BlockConstants {
    /// The number of the current block
    pub number: U256,
    /// Who is the beneficiary of the current block
    pub coinbase: H160,
    /// Unix date/time of the current block - when was the previous block finished
    pub timestamp: U256,
    /// Mining difficulty of the current block. This relates to PoW, and we can set
    /// Gas limit for the current block.
    pub gas_limit: u64,
    /// Basis of fee calculation when performing transactions in the current block.
    pub block_fees: BlockFees,
    /// Identifier for the chain. Normally this would identify the chain (Ethereum
    /// main net, or some other net). We can use it to identify rollup EVM kernel.
    pub chain_id: U256,
    /// A random number depending on previous block
    /// NB: this field is not relevant for Etherlink but is required to enable other
    /// relevant test from the Ethereum test suit
    pub prevrandao: Option<H256>,
}

impl BlockConstants {
    /// Return the first block of the chain (genisis).
    /// TODO find suitable values for gas_limit et.c.
    /// To be done in <https://gitlab.com/tezos/tezos/-/milestones/114>.
    pub fn first_block(timestamp: U256, chain_id: U256, block_fees: BlockFees) -> Self {
        Self {
            number: U256::zero(),
            coinbase: H160::zero(),
            timestamp,
            gas_limit: 1u64,
            block_fees,
            chain_id,
            prevrandao: None,
        }
    }

    #[inline(always)]
    pub const fn base_fee_per_gas(&self) -> U256 {
        self.block_fees.base_fee_per_gas
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct L2Block {
    // This choice of a L2 block representation is totally
    // arbitrarily based on what is an Ethereum block and is
    // subject to change.
    // Optional types are used for currently unused fields,
    // which will be populated in the future. This makes the
    // data representation future proof, as it won't need
    // to be migrated once the fields are used.
    pub number: U256,
    pub hash: H256,
    pub parent_hash: H256,
    pub logs_bloom: Bloom,
    pub transactions_root: OwnedHash,
    pub state_root: OwnedHash,
    pub receipts_root: OwnedHash,
    pub miner: Option<OwnedHash>,
    pub extra_data: Option<OwnedHash>,
    pub gas_limit: Option<u64>,
    pub gas_used: U256,
    pub timestamp: Timestamp,
    pub transactions: Vec<TransactionHash>,
}

impl L2Block {
    const DUMMY_HASH: &str = "00000000000000000000000000000000";
    const BLOCK_HASH_SIZE: usize = 32;

    fn dummy_block_hash() -> H256 {
        H256([0; L2Block::BLOCK_HASH_SIZE])
    }

    pub fn dummy_hash() -> OwnedHash {
        L2Block::DUMMY_HASH.into()
    }

    #[allow(clippy::too_many_arguments)]
    pub fn new(
        number: U256,
        transactions: Vec<TransactionHash>,
        timestamp: Timestamp,
        parent_hash: H256,
        logs_bloom: Bloom,
        transactions_root: OwnedHash,
        state_root: OwnedHash,
        receipts_root: OwnedHash,
        gas_used: U256,
    ) -> Self {
        let hash = Self::hash(
            parent_hash,
            &state_root,
            &transactions_root,
            &receipts_root,
            &logs_bloom,
            &None,
            number,
            None,
            gas_used,
            timestamp,
            &None,
        );
        L2Block {
            number,
            hash,
            parent_hash,
            timestamp,
            transactions,
            logs_bloom,
            transactions_root,
            state_root,
            receipts_root,
            gas_used,
            ..Self::default()
        }
    }

    pub fn constants(&self, chain_id: U256, block_fees: BlockFees) -> BlockConstants {
        let timestamp = U256::from(self.timestamp.as_u64());
        BlockConstants {
            number: self.number,
            coinbase: H160::zero(),
            timestamp,
            gas_limit: self.gas_limit.unwrap_or(1u64),
            block_fees,
            chain_id,
            prevrandao: None,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn hash(
        parent_hash: H256,
        state_root: &OwnedHash,
        transactions_root: &OwnedHash,
        receipts_root: &OwnedHash,
        logs_bloom: &Bloom,
        miner: &Option<OwnedHash>,
        number: U256,
        gas_limit: Option<u64>,
        gas_used: U256,
        timestamp: Timestamp,
        extra_data: &Option<OwnedHash>,
    ) -> H256 {
        let header = [
            hex::encode(parent_hash),
            hex::encode(state_root),
            hex::encode(transactions_root),
            hex::encode(receipts_root),
            hex::encode(logs_bloom),
            hex_of_option(miner),
            hex::encode(bytes_of_u256(number)),
            hex_of_option(&gas_limit.map(|u| u.to_le_bytes())),
            hex::encode(gas_used.to_string()),
            hex::encode(timestamp.to_string()),
            hex_of_option(extra_data),
        ];
        let bytes: Vec<u8> = rlp::encode_list::<String, String>(&header).into();
        H256(Keccak256::digest(bytes).into())
    }
}

impl Default for L2Block {
    fn default() -> Self {
        Self {
            number: U256::default(),
            hash: H256::default(),
            parent_hash: L2Block::dummy_block_hash(),
            logs_bloom: Bloom::default(),
            transactions_root: L2Block::dummy_hash(),
            state_root: L2Block::dummy_hash(),
            receipts_root: L2Block::dummy_hash(),
            miner: None,
            extra_data: None,
            gas_limit: None,
            gas_used: U256::zero(),
            timestamp: Timestamp::from(0),
            transactions: Vec::new(),
        }
    }
}

impl Encodable for L2Block {
    fn rlp_append(&self, s: &mut RlpStream) {
        s.begin_list(13);
        append_u256_le(s, &self.number);
        s.append(&self.hash);
        s.append(&self.parent_hash);
        s.append(&self.logs_bloom);
        s.append(&self.transactions_root);
        s.append(&self.state_root);
        s.append(&self.receipts_root);
        append_option_explicit(s, &self.miner, RlpStream::append);
        append_option_explicit(s, &self.extra_data, RlpStream::append);
        append_option_explicit(s, &self.gas_limit, append_u64_le);
        let transactions_bytes: Vec<Vec<u8>> =
            self.transactions.iter().map(|x| x.to_vec()).collect();
        s.append_list::<Vec<u8>, _>(&transactions_bytes);
        append_u256_le(s, &self.gas_used);
        append_timestamp(s, self.timestamp);
    }
}

impl Decodable for L2Block {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        if decoder.is_list() {
            if Ok(13) == decoder.item_count() {
                let mut it = decoder.iter();
                let number: U256 = decode_field_u256_le(&next(&mut it)?, "number")?;
                let hash: H256 = decode_field_h256(&next(&mut it)?, "hash")?;
                let parent_hash: H256 =
                    decode_field_h256(&next(&mut it)?, "parent_hash")?;
                let logs_bloom: Bloom = decode_field(&next(&mut it)?, "logs_bloom")?;
                let transactions_root: OwnedHash =
                    decode_field(&next(&mut it)?, "transactions_root")?;
                let state_root: OwnedHash = decode_field(&next(&mut it)?, "state_root")?;
                let receipts_root: OwnedHash =
                    decode_field(&next(&mut it)?, "receipts_root")?;
                let miner: Option<OwnedHash> =
                    decode_option_explicit(&next(&mut it)?, "miner", decode_field)?;
                let extra_data: Option<OwnedHash> =
                    decode_option_explicit(&next(&mut it)?, "extra_data", decode_field)?;
                let gas_limit: Option<u64> = decode_option_explicit(
                    &next(&mut it)?,
                    "gas_limit",
                    decode_field_u64_le,
                )?;
                let transactions: Vec<TransactionHash> =
                    decode_transaction_hash_list(&next(&mut it)?, "transactions")?;
                let gas_used: U256 = decode_field_u256_le(&next(&mut it)?, "gas_used")?;
                let timestamp = decode_timestamp(&next(&mut it)?)?;
                Ok(L2Block {
                    number,
                    hash,
                    parent_hash,
                    logs_bloom,
                    transactions_root,
                    state_root,
                    receipts_root,
                    miner,
                    extra_data,
                    gas_limit,
                    gas_used,
                    timestamp,
                    transactions,
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
