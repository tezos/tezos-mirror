// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::eth_gen::OwnedHash;
use crate::helpers::{bytes_of_u256, hex_of_option};
use crate::rlp_helpers::{
    append_option_explicit, append_timestamp, append_u256_le, append_u64_le,
    decode_field, decode_field_u256_le, decode_field_u64_le, decode_option_explicit,
    decode_timestamp, decode_transaction_hash_list, next, VersionedEncoding,
};
use crate::transaction::TransactionHash;
use ethbloom::Bloom;
use primitive_types::{H160, H256, U256};
use rlp::{DecoderError, Rlp, RlpStream};
use sha3::{Digest, Keccak256};
use tezos_smart_rollup_encoding::timestamp::Timestamp;

/// Container for fee calculation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockFees {
    minimum_base_fee_per_gas: U256,
    base_fee_per_gas: U256,
    da_fee_per_byte: U256,
    blob_base_fee: U256,
}

impl BlockFees {
    /// Setup fee information for the current block
    pub const fn new(
        minimum_base_fee_per_gas: U256,
        base_fee_per_gas: U256,
        da_fee_per_byte: U256,
    ) -> Self {
        Self {
            minimum_base_fee_per_gas,
            base_fee_per_gas,
            da_fee_per_byte,
            // Etherlink doesn't support blob as defined by
            // EIP-7516.
            // As such, the following value will always return
            // zero.
            blob_base_fee: U256::zero(),
        }
    }

    /// The base fee per gas for doing a transaction within the current block.
    #[inline(always)]
    pub const fn base_fee_per_gas(&self) -> U256 {
        self.base_fee_per_gas
    }

    /// The minimum base fee per gas
    #[inline(always)]
    pub const fn minimum_base_fee_per_gas(&self) -> U256 {
        self.minimum_base_fee_per_gas
    }

    /// The da fee per byte charged per transaction.
    #[inline(always)]
    pub const fn da_fee_per_byte(&self) -> U256 {
        self.da_fee_per_byte
    }

    #[inline(always)]
    pub const fn blob_base_fee(&self) -> U256 {
        self.blob_base_fee
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
    pub fn first_block(
        timestamp: U256,
        chain_id: U256,
        block_fees: BlockFees,
        gas_limit: u64,
        coinbase: H160,
    ) -> Self {
        Self {
            number: U256::zero(),
            coinbase,
            timestamp,
            gas_limit,
            block_fees,
            chain_id,
            prevrandao: None,
        }
    }

    #[inline(always)]
    pub const fn base_fee_per_gas(&self) -> U256 {
        self.block_fees.base_fee_per_gas
    }

    #[inline(always)]
    pub const fn blob_base_fee(&self) -> U256 {
        self.block_fees.blob_base_fee
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EthBlock {
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
    pub miner: Option<H160>,
    pub extra_data: Option<OwnedHash>,
    pub gas_limit: u64,
    pub gas_used: U256,
    pub timestamp: Timestamp,
    pub transactions: Vec<TransactionHash>,
    pub base_fee_per_gas: U256,
    pub mix_hash: H256,
}

impl EthBlock {
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
        block_constants: &BlockConstants,
        base_fee_per_gas: U256,
    ) -> Self {
        let hash = Self::hash(
            parent_hash,
            &state_root,
            &transactions_root,
            &receipts_root,
            &logs_bloom,
            &Some(block_constants.coinbase),
            number,
            block_constants.gas_limit,
            gas_used,
            timestamp,
            &None,
        );
        EthBlock {
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
            gas_limit: block_constants.gas_limit,
            extra_data: None,
            miner: Some(block_constants.coinbase),
            base_fee_per_gas,
            mix_hash: H256::zero(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn hash(
        parent_hash: H256,
        state_root: &OwnedHash,
        transactions_root: &OwnedHash,
        receipts_root: &OwnedHash,
        logs_bloom: &Bloom,
        miner: &Option<H160>,
        number: U256,
        gas_limit: u64,
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
            hex::encode(gas_limit.to_le_bytes()),
            hex::encode(gas_used.to_string()),
            hex::encode(timestamp.to_string()),
            hex_of_option(extra_data),
        ];
        let bytes: Vec<u8> = rlp::encode_list::<String, String>(&header).into();
        H256(Keccak256::digest(bytes).into())
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<EthBlock, DecoderError> {
        let first = *bytes.first().ok_or(DecoderError::Custom("Empty bytes"))?;
        match first {
            0x01 | 0x02 => {
                let decoder = Rlp::new(&bytes[1..]);
                Self::rlp_decode_v1(&decoder)
            }
            _ => {
                let decoder = Rlp::new(bytes);
                Self::rlp_decode_v0(&decoder)
            }
        }
    }

    fn rlp_decode_v0(decoder: &Rlp) -> Result<EthBlock, DecoderError> {
        if decoder.is_list() {
            if Ok(13) == decoder.item_count() {
                let mut it = decoder.iter();
                let number: U256 = decode_field_u256_le(&next(&mut it)?, "number")?;
                let hash: H256 = decode_field(&next(&mut it)?, "hash")?;
                let parent_hash: H256 = decode_field(&next(&mut it)?, "parent_hash")?;
                let logs_bloom: Bloom = decode_field(&next(&mut it)?, "logs_bloom")?;
                let transactions_root: OwnedHash =
                    decode_field(&next(&mut it)?, "transactions_root")?;
                let state_root: OwnedHash = decode_field(&next(&mut it)?, "state_root")?;
                let receipts_root: OwnedHash =
                    decode_field(&next(&mut it)?, "receipts_root")?;
                let miner: Option<H160> =
                    decode_option_explicit(&next(&mut it)?, "miner", decode_field)?;
                let extra_data: Option<OwnedHash> =
                    decode_option_explicit(&next(&mut it)?, "extra_data", decode_field)?;
                let gas_limit = decode_field_u64_le(&next(&mut it)?, "gas_limit")?;
                let transactions: Vec<TransactionHash> =
                    decode_transaction_hash_list(&next(&mut it)?, "transactions")?;
                let gas_used: U256 = decode_field_u256_le(&next(&mut it)?, "gas_used")?;
                let timestamp = decode_timestamp(&next(&mut it)?)?;
                Ok(EthBlock {
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
                    base_fee_per_gas: U256::from(1000000000),
                    mix_hash: H256::zero(),
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }

    fn rlp_decode_v1(decoder: &Rlp) -> Result<EthBlock, DecoderError> {
        if decoder.is_list() {
            if Ok(15) == decoder.item_count() {
                let mut it = decoder.iter();
                let number: U256 = decode_field_u256_le(&next(&mut it)?, "number")?;
                let hash: H256 = decode_field(&next(&mut it)?, "hash")?;
                let parent_hash: H256 = decode_field(&next(&mut it)?, "parent_hash")?;
                let logs_bloom: Bloom = decode_field(&next(&mut it)?, "logs_bloom")?;
                let transactions_root: OwnedHash =
                    decode_field(&next(&mut it)?, "transactions_root")?;
                let state_root: OwnedHash = decode_field(&next(&mut it)?, "state_root")?;
                let receipts_root: OwnedHash =
                    decode_field(&next(&mut it)?, "receipts_root")?;
                let miner: Option<H160> =
                    decode_option_explicit(&next(&mut it)?, "miner", decode_field)?;
                let extra_data: Option<OwnedHash> =
                    decode_option_explicit(&next(&mut it)?, "extra_data", decode_field)?;
                let gas_limit = decode_field_u64_le(&next(&mut it)?, "gas_limit")?;
                let transactions: Vec<TransactionHash> =
                    decode_transaction_hash_list(&next(&mut it)?, "transactions")?;
                let gas_used: U256 = decode_field_u256_le(&next(&mut it)?, "gas_used")?;
                let timestamp = decode_timestamp(&next(&mut it)?)?;
                let base_fee_per_gas: U256 =
                    decode_field_u256_le(&next(&mut it)?, "base_fee_per_gas")?;
                let mix_hash: H256 = decode_field(&next(&mut it)?, "mix_hash")?;
                Ok(EthBlock {
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
                    base_fee_per_gas,
                    mix_hash,
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }

    fn rlp_encode(self: &EthBlock, s: &mut RlpStream) {
        s.begin_list(15);
        append_u256_le(s, &self.number);
        s.append(&self.hash);
        s.append(&self.parent_hash);
        s.append(&self.logs_bloom);
        s.append(&self.transactions_root);
        s.append(&self.state_root);
        s.append(&self.receipts_root);
        append_option_explicit(s, &self.miner, RlpStream::append);
        append_option_explicit(s, &self.extra_data, RlpStream::append);
        append_u64_le(s, &self.gas_limit);
        let transactions_bytes: Vec<Vec<u8>> =
            self.transactions.iter().map(|x| x.to_vec()).collect();
        s.append_list::<Vec<u8>, _>(&transactions_bytes);
        append_u256_le(s, &self.gas_used);
        append_timestamp(s, self.timestamp);
        append_u256_le(s, &self.base_fee_per_gas);
        s.append(&self.mix_hash);
    }
}

impl VersionedEncoding for EthBlock {
    // Versions history
    //   - 0x02: Interpreted by the node as blocks with
    //      - `withdrawals` and `withdrawalsRoot` (EIP-4895)
    //      - `blobGasUsed` and `excessBlobGas` (EIP-4844)
    //      - `parentBeaconBlockRoot` (EIP-4788)
    //   - 0x01: EIP-1559 compliant blocks
    //   - No tag: legacy blocks
    const VERSION: u8 = 2;

    fn unversionned_encode(&self) -> bytes::BytesMut {
        let mut s = RlpStream::new();
        self.rlp_encode(&mut s);
        s.out()
    }

    fn unversionned_decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        Self::rlp_decode_v1(decoder)
    }
}

#[cfg(test)]
mod tests {

    use super::EthBlock;
    use crate::eth_gen::OwnedHash;
    use crate::rlp_helpers::VersionedEncoding;
    use crate::transaction::TRANSACTION_HASH_SIZE;
    use crate::Bloom;
    use primitive_types::{H256, U256};
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    fn block_encoding_roundtrip(v: EthBlock) {
        let bytes = v.to_bytes();
        let v2 = EthBlock::from_bytes(&bytes).expect("EthBlock should be decodable");
        assert_eq!(v, v2, "Roundtrip failed on {v:?}")
    }

    const DUMMY_HASH: &str = "00000000000000000000000000000000";

    pub fn dummy_hash() -> OwnedHash {
        DUMMY_HASH.into()
    }
    fn dummy_block(tx_length: usize) -> EthBlock {
        EthBlock {
            number: U256::from(42),
            hash: H256::from([3u8; 32]),
            parent_hash: H256::from([2u8; 32]),
            timestamp: Timestamp::from(10i64),
            transactions: vec![[0u8; TRANSACTION_HASH_SIZE]; tx_length],
            logs_bloom: Bloom::default(),
            transactions_root: dummy_hash(),
            state_root: dummy_hash(),
            receipts_root: dummy_hash(),
            miner: None,
            extra_data: None,
            gas_limit: 1 << 50,
            gas_used: U256::zero(),
            base_fee_per_gas: U256::zero(),
            mix_hash: H256::default(),
        }
    }

    #[test]
    fn roundtrip_rlp() {
        for tx_length in 0..3 {
            let v: EthBlock = dummy_block(tx_length);
            block_encoding_roundtrip(v);
        }
    }
}
