// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    rlp_helpers::*,
    tx_signature::{rlp_append_opt, rlp_decode_opt, TxSignature},
};
use ethbloom::{Bloom, Input};
use ethereum::Log;
use primitive_types::{H160, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};

pub const TRANSACTION_HASH_SIZE: usize = 32;
pub type TransactionHash = [u8; TRANSACTION_HASH_SIZE];

#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub enum TransactionType {
    Legacy,
    Eip2930,
    Eip1559,
    Eip7702,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TransactionStatus {
    Success,
    Failure,
}

pub enum TransactionDecodingError {
    InvalidEncoding,
    InvalidVectorLength,
}

impl TryFrom<&u8> for TransactionStatus {
    type Error = TransactionDecodingError;

    fn try_from(v: &u8) -> Result<Self, Self::Error> {
        if *v == 0 {
            Ok(Self::Failure)
        } else if *v == 1 {
            Ok(Self::Success)
        } else {
            Err(TransactionDecodingError::InvalidEncoding)
        }
    }
}

impl From<TransactionStatus> for u8 {
    fn from(v: TransactionStatus) -> Self {
        match v {
            TransactionStatus::Failure => 0u8,
            TransactionStatus::Success => 1u8,
        }
    }
}

impl TryFrom<&u8> for TransactionType {
    type Error = TransactionDecodingError;

    fn try_from(v: &u8) -> Result<Self, Self::Error> {
        if *v == 0 {
            Ok(Self::Legacy)
        } else if *v == 1 {
            Ok(Self::Eip2930)
        } else if *v == 2 {
            Ok(Self::Eip1559)
        } else if *v == 4 {
            Ok(Self::Eip7702)
        } else {
            Err(TransactionDecodingError::InvalidEncoding)
        }
    }
}

impl From<TransactionType> for u8 {
    fn from(v: TransactionType) -> Self {
        match v {
            TransactionType::Legacy => 0u8,
            TransactionType::Eip2930 => 1u8,
            TransactionType::Eip1559 => 2u8,
            TransactionType::Eip7702 => 4u8,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexedLog {
    pub log: Log,
    /// Position of the log within a block.
    pub index: u64,
}

impl Encodable for IndexedLog {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(2);
        stream.append(&self.log);
        append_u64_le(stream, &self.index);
    }
}

impl Decodable for IndexedLog {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if Ok(2) != decoder.item_count() {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let log: Log = decode_field(&next(&mut it)?, "log")?;
        let index: u64 = decode_field_u64_le(&next(&mut it)?, "index")?;
        Ok(Self { log, index })
    }
}

/// Transaction receipt, see https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_gettransactionreceipt
#[derive(Debug, PartialEq, Clone)]
pub struct TransactionReceipt {
    /// Hash of the transaction.
    pub hash: TransactionHash,
    /// Integer of the transactions index position in the block.
    pub index: u32,
    /// Block number where this transaction was in.
    pub block_number: U256,
    /// Address of the sender.
    pub from: H160,
    /// Address of the receiver. null when its a contract creation transaction.
    pub to: Option<H160>,
    /// The total amount of gas used when this transaction was executed in the block
    pub cumulative_gas_used: U256,
    /// The sum of the base fee and tip paid per unit of gas.
    pub effective_gas_price: U256,
    /// The amount of gas used by this specific transaction alone.
    pub gas_used: U256,
    /// The contract address created, if the transaction was a contract creation, otherwise null.
    pub contract_address: Option<H160>,
    /// The logs emitted during contract execution
    pub logs: Vec<IndexedLog>,
    /// The bloom filter corresponding to the logs.
    /// It basically contains all addresses and topics from log objects.
    pub logs_bloom: Bloom,
    pub type_: TransactionType,
    /// Transaction status
    pub status: TransactionStatus,
}

impl TransactionReceipt {
    // DO NOT RENAME: function name is used during benchmark
    // Never inlined when the kernel is compiled for benchmarks, to ensure the
    // function is visible in the profiling results.
    #[cfg_attr(feature = "benchmark", inline(never))]
    pub fn logs_to_bloom(logs: &[IndexedLog]) -> Bloom {
        let mut bloom = Bloom::default();
        // According to
        // https://github.com/ethereum/go-ethereum/blob/41ee96fdfee5924004e8fbf9bbc8aef783893917/core/types/bloom9.go#L119
        for log in logs {
            bloom.accrue(Input::Raw(log.log.address.as_bytes()));
            for topic in log.log.topics.iter() {
                bloom.accrue(Input::Raw(topic.as_bytes()));
            }
        }
        bloom
    }
}

impl Decodable for TransactionReceipt {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if Ok(13) != decoder.item_count() {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let hash: TransactionHash = decode_transaction_hash(&next(&mut it)?)?;
        let index: u32 = decode_field(&next(&mut it)?, "index")?;
        let block_number: U256 = decode_field_u256_le(&next(&mut it)?, "block_number")?;
        let from: H160 = decode_field(&next(&mut it)?, "from")?;
        let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
        let cumulative_gas_used: U256 =
            decode_field_u256_le(&next(&mut it)?, "cumulative_gas_used")?;
        let effective_gas_price: U256 =
            decode_field_u256_le(&next(&mut it)?, "effective_gas_price")?;
        let gas_used: U256 = decode_field_u256_le(&next(&mut it)?, "gas_used")?;
        let contract_address: Option<H160> =
            decode_option(&next(&mut it)?, "contract_address")?;
        let logs = decode_list(&next(&mut it)?, "logs")?;
        let logs_bloom = decode_field(&next(&mut it)?, "logs_bloom")?;
        let type_: TransactionType = decode_transaction_type(&next(&mut it)?)?;
        let status: TransactionStatus = decode_transaction_status(&next(&mut it)?)?;
        Ok(Self {
            hash,
            index,
            block_number,
            from,
            to,
            cumulative_gas_used,
            effective_gas_price,
            gas_used,
            contract_address,
            logs,
            logs_bloom,
            type_,
            status,
        })
    }
}

impl Encodable for TransactionReceipt {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(13);
        stream.append(&self.hash.to_vec());
        stream.append(&self.index);
        append_u256_le(stream, &self.block_number);
        stream.append(&self.from);
        match &self.to {
            Some(to) => stream.append(to),
            None => stream.append_empty_data(),
        };
        append_u256_le(stream, &self.cumulative_gas_used);
        append_u256_le(stream, &self.effective_gas_price);
        append_u256_le(stream, &self.gas_used);
        match &self.contract_address {
            Some(address) => stream.append(address),
            None => stream.append_empty_data(),
        };
        stream.append_list(&self.logs);
        stream.append(&self.logs_bloom);
        stream.append::<u8>(&self.type_.into());
        stream.append::<u8>(&self.status.into());
    }
}

// For now all the receipts we support encode the same payload
// Can't use `Encodable2718` from alloy because one parameter is
// `bytes::BufMut` which use floats
// Standard for eips : Legacy, 2930, 1559, 4844, 7702
impl TransactionReceipt {
    pub fn encode_2718(&self, out: &mut Vec<u8>) {
        let mut rlp_stream = RlpStream::new();

        if self.type_ != TransactionType::Legacy {
            rlp_stream.begin_list(4);
        } else {
            rlp_stream.begin_list(5);
            rlp_stream.append::<u8>(&self.type_.into());
        }
        match &self.status {
            TransactionStatus::Success => rlp_stream.append::<bool>(&true),
            TransactionStatus::Failure => rlp_stream.append_empty_data(),
        };
        rlp_stream
            .append(&self.cumulative_gas_used)
            .append(&self.logs_bloom)
            .append_list(&self.logs);
        out.extend_from_slice(&rlp_stream.out());
    }
}

impl TryFrom<&[u8]> for TransactionReceipt {
    type Error = DecoderError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        Self::from_rlp_bytes(bytes)
    }
}

/// Transaction object, https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_gettransactionbyhash
/// There a lot of redundancy between a transaction object and a transaction
/// receipt. In fact, transaction objects should not be stored in the kernel
/// but rather in the EVM node. Duplicating the code instead of sharing fields
/// is intentional to facilitate the associated code to the EVM node.
/// TODO: https://gitlab.com/tezos/tezos/-/issues/5695
#[derive(Debug, PartialEq, Clone)]
pub struct TransactionObject {
    /// Block number where this transaction was in. null when its pending.
    pub block_number: U256,
    /// Address of the sender.
    pub from: H160,
    /// The amount of gas used by this specific transaction alone.
    pub gas_used: U256,
    /// The amount of gas price provided by the sender in Wei.
    pub gas_price: U256,
    /// Hash of the transaction.
    pub hash: TransactionHash,
    /// The data send along with the transaction.
    pub input: Vec<u8>,
    /// The number of transactions made by the sender prior to this one.
    pub nonce: u64,
    /// Address of the receiver. null when its a contract creation transaction.
    pub to: Option<H160>,
    /// Integer of the transactions index position in the block.
    pub index: u32,
    /// Value transferred in Wei.
    pub value: U256,
    /// ECDSA signature
    pub signature: Option<TxSignature>,
}

impl Decodable for TransactionObject {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if decoder.is_list() {
            if Ok(13) == decoder.item_count() {
                let mut it = decoder.iter();
                let block_number: U256 =
                    decode_field_u256_le(&next(&mut it)?, "block_number")?;
                let from: H160 = decode_field(&next(&mut it)?, "from")?;
                let gas_used: U256 = decode_field_u256_le(&next(&mut it)?, "gas_used")?;
                let gas_price: U256 = decode_field_u256_le(&next(&mut it)?, "gas_price")?;
                let hash: TransactionHash = decode_transaction_hash(&next(&mut it)?)?;
                let input: Vec<u8> = decode_field(&next(&mut it)?, "input")?;
                let nonce: u64 = decode_field_u64_le(&next(&mut it)?, "nonce")?;
                let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
                let index: u32 = decode_field(&next(&mut it)?, "index")?;
                let value: U256 = decode_field_u256_le(&next(&mut it)?, "value")?;
                let signature = rlp_decode_opt(&mut it)?;
                Ok(Self {
                    block_number,
                    from,
                    gas_used,
                    gas_price,
                    hash,
                    input,
                    nonce,
                    to,
                    index,
                    value,
                    signature,
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }
}

impl Encodable for TransactionObject {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(13);
        append_u256_le(stream, &self.block_number);
        stream.append(&self.from);
        append_u256_le(stream, &self.gas_used);
        append_u256_le(stream, &self.gas_price);
        stream.append(&self.hash.to_vec());
        stream.append(&self.input);
        append_u64_le(stream, &self.nonce);
        match &self.to {
            Some(to) => stream.append(to),
            None => stream.append_empty_data(),
        };
        stream.append(&self.index);
        append_u256_le(stream, &self.value);
        rlp_append_opt(&self.signature, stream);
    }
}

// Not ethereum compatible because we don't have the type field here
// For now all the receipts we support encode the same payload
// Can't use `Encodable2718` from alloy because one parameter is
// `bytes::BufMut` which use floats
impl TransactionObject {
    pub fn encode_2718(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.rlp_bytes());
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use primitive_types::H256;

    fn address_of_str(s: &str) -> H160 {
        let data = &hex::decode(s).unwrap();
        H160::from_slice(data)
    }

    fn receipt_encoding_roundtrip(v: TransactionReceipt) {
        let bytes = v.rlp_bytes();
        let v2 = TransactionReceipt::from_rlp_bytes(&bytes)
            .expect("Transaction receipt should be decodable");
        assert_eq!(v, v2, "Roundtrip failed on {v:?}")
    }

    fn tx_receipt(logs: Vec<IndexedLog>) -> TransactionReceipt {
        TransactionReceipt {
            hash: [0; TRANSACTION_HASH_SIZE],
            index: 15u32,
            block_number: U256::from(42),
            from: address_of_str("3535353535353535353535353535353535353535"),
            to: Some(address_of_str("3635353535353535353535353535353535353536")),
            cumulative_gas_used: U256::from(1252345235),
            effective_gas_price: U256::from(47457345),
            gas_used: U256::from(474573452),
            contract_address: Some(address_of_str(
                "4335353535353535353535353535353535353543",
            )),
            type_: TransactionType::Legacy,
            logs_bloom: TransactionReceipt::logs_to_bloom(&logs),
            logs,
            status: TransactionStatus::Success,
        }
    }

    #[test]
    fn test_receipt_encoding_rountrip() {
        let address = address_of_str("ef2d6d194084c2de36e0dabfce45d046b37d1106");
        let topic = H256::from_slice(
            &hex::decode(
                "02c69be41d0b7e40352fc85be1cd65eb03d40ef8427a0ca4596b1ead9a00e9fc",
            )
            .expect("Valid hex"),
        );
        let logs = vec![IndexedLog {
            log: Log {
                address,
                topics: vec![topic],
                data: vec![0, 1, 2, 3],
            },
            index: 0,
        }];

        let v = tx_receipt(logs);
        receipt_encoding_roundtrip(v.clone());

        let v1 = TransactionReceipt {
            to: None,
            ..v.clone()
        };
        receipt_encoding_roundtrip(v1);

        let v2 = TransactionReceipt {
            to: None,
            contract_address: None,
            ..v
        };
        receipt_encoding_roundtrip(v2);
    }

    fn object_encoding_roundtrip(v: TransactionObject) {
        let bytes = v.rlp_bytes();
        let v2 = TransactionObject::from_rlp_bytes(&bytes)
            .expect("Transaction object should be decodable");
        assert_eq!(v, v2, "Roundtrip failed on {v:?}")
    }

    #[test]
    fn test_object_encoding_rountrip() {
        let v = TransactionObject {
            block_number: U256::from(532532),
            from: address_of_str("3535353535353535353535353535353535353535"),
            gas_used: U256::from(32523),
            gas_price: U256::from(100432432),
            hash: [5; TRANSACTION_HASH_SIZE],
            input: vec![],
            nonce: 8888,
            to: Some(address_of_str("3635353535353535353535353535353535353536")),
            index: 15u32,
            value: U256::from(0),
            signature: Some(
                TxSignature::new(
                    U256::from(1337),
                    H256::from_low_u64_be(1),
                    H256::from_low_u64_be(2),
                )
                .unwrap(),
            ),
        };
        object_encoding_roundtrip(v.clone());

        let v1 = TransactionObject {
            to: None,
            ..v.clone()
        };
        object_encoding_roundtrip(v1);

        let v2 = TransactionObject {
            to: None,
            input: [15; 564].to_vec(),
            ..v
        };
        object_encoding_roundtrip(v2);
    }
}
