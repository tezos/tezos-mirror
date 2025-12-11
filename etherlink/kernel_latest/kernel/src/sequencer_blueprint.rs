// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H256, U256};
use rlp::{Decodable, DecoderError, Encodable};
use tezos_crypto_rs::hash::UnknownSignature;
use tezos_ethereum::rlp_helpers::{
    self, append_timestamp, append_u16_le, append_u256_le, decode_field_u16_le,
    decode_field_u256_le, decode_timestamp,
};
use tezos_smart_rollup::types::Timestamp;

use crate::delayed_inbox::Hash;

#[cfg(test)]
pub const LATEST_BLUEPRINT_VERSION: u8 = 0;

#[derive(Debug, Clone)]
pub struct BlueprintWithDelayedHashes {
    // The `version` field tells how to decode the `transactions`
    // field.
    pub version: u8,
    pub parent_hash: H256,
    pub delayed_hashes: Vec<Hash>,
    // We are using `Vec<u8>` for the transaction instead of `EthereumTransactionCommon`
    // to avoid decoding then re-encoding to compute the hash.
    pub transactions: Vec<Vec<u8>>,
    pub timestamp: Timestamp,
}

impl Encodable for BlueprintWithDelayedHashes {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let BlueprintWithDelayedHashes {
            version,
            parent_hash,
            delayed_hashes,
            transactions,
            timestamp,
        } = self;
        stream.begin_list(5);
        stream.append(version);
        stream.append(parent_hash);
        stream.append_list(delayed_hashes);
        stream.append_list::<Vec<u8>, _>(transactions);
        append_timestamp(stream, *timestamp);
    }
}

impl Decodable for BlueprintWithDelayedHashes {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        match decoder.item_count()? {
            4 => {
                // Only 4 fields means that this is a legacy blueprint
                // missing the version field.
                let version = 0;

                let mut it = decoder.iter();
                let parent_hash = rlp_helpers::decode_field(
                    &rlp_helpers::next(&mut it)?,
                    "parent_hash",
                )?;
                let delayed_hashes = rlp_helpers::decode_list(
                    &rlp_helpers::next(&mut it)?,
                    "delayed_hashes",
                )?;
                let transactions = rlp_helpers::decode_list(
                    &rlp_helpers::next(&mut it)?,
                    "transactions",
                )?;
                let timestamp = decode_timestamp(&rlp_helpers::next(&mut it)?)?;

                Ok(Self {
                    version,
                    delayed_hashes,
                    parent_hash,
                    transactions,
                    timestamp,
                })
            }
            5 => {
                let mut it = decoder.iter();
                let version =
                    rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "version")?;
                let parent_hash = rlp_helpers::decode_field(
                    &rlp_helpers::next(&mut it)?,
                    "parent_hash",
                )?;
                let delayed_hashes = rlp_helpers::decode_list(
                    &rlp_helpers::next(&mut it)?,
                    "delayed_hashes",
                )?;
                let transactions = rlp_helpers::decode_list(
                    &rlp_helpers::next(&mut it)?,
                    "transactions",
                )?;
                let timestamp = decode_timestamp(&rlp_helpers::next(&mut it)?)?;

                Ok(Self {
                    version,
                    delayed_hashes,
                    parent_hash,
                    transactions,
                    timestamp,
                })
            }
            _ => Err(DecoderError::RlpInvalidLength),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnsignedSequencerBlueprint {
    pub chunk: Vec<u8>,
    pub number: U256,
    pub nb_chunks: u16,
    pub chunk_index: u16,
    pub chain_id: Option<U256>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct SequencerBlueprint {
    pub blueprint: UnsignedSequencerBlueprint,
    pub signature: UnknownSignature,
}

impl From<&SequencerBlueprint> for UnsignedSequencerBlueprint {
    fn from(val: &SequencerBlueprint) -> UnsignedSequencerBlueprint {
        val.blueprint.clone()
    }
}

impl Encodable for UnsignedSequencerBlueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let Self {
            chunk,
            number,
            nb_chunks,
            chunk_index,
            chain_id,
        } = self;
        stream.begin_list(4 + if chain_id.is_some() { 1 } else { 0 });
        stream.append(chunk);
        append_u256_le(stream, number);
        append_u16_le(stream, nb_chunks);
        append_u16_le(stream, chunk_index);
        if let Some(chain_id) = chain_id {
            append_u256_le(stream, chain_id);
        }
    }
}

impl Encodable for SequencerBlueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let UnsignedSequencerBlueprint {
            chunk,
            number,
            nb_chunks,
            chunk_index,
            chain_id,
        } = &self.blueprint;
        stream.begin_list(5 + if chain_id.is_some() { 1 } else { 0 });
        stream.append(chunk);
        append_u256_le(stream, number);
        append_u16_le(stream, nb_chunks);
        append_u16_le(stream, chunk_index);
        if let Some(chain_id) = chain_id {
            append_u256_le(stream, chain_id);
        }
        stream.append(&self.signature.as_ref());
    }
}

impl Decodable for UnsignedSequencerBlueprint {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        match decoder.item_count()? {
            4 => {
                // Optional chain_id field is absent
                let mut it = decoder.iter();
                let chunk =
                    rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "chunk")?;
                let number =
                    decode_field_u256_le(&rlp_helpers::next(&mut it)?, "number")?;
                let nb_chunks =
                    decode_field_u16_le(&rlp_helpers::next(&mut it)?, "nb_chunks")?;
                let chunk_index =
                    decode_field_u16_le(&rlp_helpers::next(&mut it)?, "chunk_index")?;
                Ok(Self {
                    chunk,
                    number,
                    nb_chunks,
                    chunk_index,
                    chain_id: None,
                })
            }
            5 => {
                // Optional chain_id field is provided
                let mut it = decoder.iter();
                let chunk =
                    rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "chunk")?;
                let number =
                    decode_field_u256_le(&rlp_helpers::next(&mut it)?, "number")?;
                let nb_chunks =
                    decode_field_u16_le(&rlp_helpers::next(&mut it)?, "nb_chunks")?;
                let chunk_index =
                    decode_field_u16_le(&rlp_helpers::next(&mut it)?, "chunk_index")?;
                let chain_id =
                    decode_field_u256_le(&rlp_helpers::next(&mut it)?, "chain_id")?;
                Ok(Self {
                    chunk,
                    number,
                    nb_chunks,
                    chunk_index,
                    chain_id: Some(chain_id),
                })
            }
            _ => Err(DecoderError::RlpInvalidLength),
        }
    }
}

impl Decodable for SequencerBlueprint {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        match decoder.item_count()? {
            5 => {
                // Optional chain_id field is absent
                let mut it = decoder.iter();
                let chunk =
                    rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "chunk")?;
                let number =
                    decode_field_u256_le(&rlp_helpers::next(&mut it)?, "number")?;
                let nb_chunks =
                    decode_field_u16_le(&rlp_helpers::next(&mut it)?, "nb_chunks")?;
                let chunk_index =
                    decode_field_u16_le(&rlp_helpers::next(&mut it)?, "chunk_index")?;
                let bytes: Vec<u8> =
                    rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "signature")?;
                let signature = UnknownSignature::try_from(bytes.as_slice())
                    .map_err(|_| DecoderError::Custom("Invalid signature encoding"))?;
                let blueprint = UnsignedSequencerBlueprint {
                    chunk,
                    number,
                    nb_chunks,
                    chunk_index,
                    chain_id: None,
                };
                Ok(Self {
                    blueprint,
                    signature,
                })
            }
            6 => {
                // Optional chain_id field is provided
                let mut it = decoder.iter();
                let chunk =
                    rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "chunk")?;
                let number =
                    decode_field_u256_le(&rlp_helpers::next(&mut it)?, "number")?;
                let nb_chunks =
                    decode_field_u16_le(&rlp_helpers::next(&mut it)?, "nb_chunks")?;
                let chunk_index =
                    decode_field_u16_le(&rlp_helpers::next(&mut it)?, "chunk_index")?;
                let chain_id =
                    decode_field_u256_le(&rlp_helpers::next(&mut it)?, "chain_id")?;
                let bytes: Vec<u8> =
                    rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "signature")?;
                let signature = UnknownSignature::try_from(bytes.as_slice())
                    .map_err(|_| DecoderError::Custom("Invalid signature encoding"))?;
                let blueprint = UnsignedSequencerBlueprint {
                    chunk,
                    number,
                    nb_chunks,
                    chunk_index,
                    chain_id: Some(chain_id),
                };
                Ok(Self {
                    blueprint,
                    signature,
                })
            }
            _ => Err(DecoderError::RlpInvalidLength),
        }
    }
}

#[cfg(test)]
pub fn rlp_roundtrip_f<S: Encodable + PartialEq + std::fmt::Debug, T: Decodable>(
    v: S,
    f: impl FnOnce(T) -> S,
) {
    let bytes = v.rlp_bytes();
    let v2: T =
        rlp_helpers::FromRlpBytes::from_rlp_bytes(&bytes).expect("Should be decodable");
    assert_eq!(v, f(v2), "Roundtrip failed on {v:?}")
}

#[cfg(test)]
pub fn rlp_roundtrip<S: Encodable + Decodable + PartialEq + std::fmt::Debug>(v: S) {
    rlp_roundtrip_f::<S, S>(v, |s| s)
}

#[cfg(test)]
mod tests {
    use super::{rlp_roundtrip, SequencerBlueprint, UnsignedSequencerBlueprint};
    use crate::blueprint::Blueprint;
    use crate::transaction::Transaction;
    use crate::transaction::TransactionContent::Ethereum;
    use primitive_types::{H160, U256};
    use tezos_crypto_rs::hash::UnknownSignature;
    use tezos_ethereum::{
        transaction::TRANSACTION_HASH_SIZE, tx_common::EthereumTransactionCommon,
    };
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    fn tx_(i: u64) -> EthereumTransactionCommon {
        EthereumTransactionCommon::new(
            tezos_ethereum::transaction::TransactionType::Legacy,
            Some(U256::one()),
            i,
            U256::from(40000000u64),
            U256::from(40000000u64),
            21000u64,
            address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea"),
            U256::from(500000000u64),
            vec![],
            vec![],
            None,
            None,
        )
    }

    fn dummy_transaction(i: u8) -> Transaction {
        Transaction {
            tx_hash: [i; TRANSACTION_HASH_SIZE],
            content: Ethereum(tx_(i.into())),
        }
    }

    fn dummy_blueprint_unsigned(chain_id: Option<U256>) -> UnsignedSequencerBlueprint {
        let transactions = vec![dummy_transaction(0), dummy_transaction(1)];
        let timestamp = Timestamp::from(42);
        let blueprint = Blueprint {
            timestamp,
            transactions,
        };
        let chunk = rlp::Encodable::rlp_bytes(&blueprint);
        UnsignedSequencerBlueprint {
            chunk: chunk.into(),
            number: U256::from(42),
            nb_chunks: 1u16,
            chunk_index: 0u16,
            chain_id,
        }
    }

    fn dummy_blueprint(chain_id: Option<U256>) -> SequencerBlueprint {
        let signature = UnknownSignature::from_base58_check(
            "sigdGBG68q2vskMuac4AzyNb1xCJTfuU8MiMbQtmZLUCYydYrtTd5Lessn1EFLTDJzjXoYxRasZxXbx6tHnirbEJtikcMHt3"
        ).expect("signature decoding should work");

        SequencerBlueprint {
            blueprint: dummy_blueprint_unsigned(chain_id),
            signature,
        }
    }

    #[test]
    fn roundtrip_rlp_no_chain_id() {
        let chunk = dummy_blueprint(None);
        rlp_roundtrip(chunk);
    }

    #[test]
    fn roundtrip_rlp() {
        let chain_id = U256::one();
        let chunk = dummy_blueprint(Some(chain_id));
        rlp_roundtrip(chunk);
    }

    #[test]
    fn roundtrip_rlp_no_chain_id_unsigned() {
        let chunk = dummy_blueprint_unsigned(None);
        rlp_roundtrip(chunk);
    }

    #[test]
    fn roundtrip_rlp_unsigned() {
        let chain_id = U256::one();
        let chunk = dummy_blueprint_unsigned(Some(chain_id));
        rlp_roundtrip(chunk);
    }
}
