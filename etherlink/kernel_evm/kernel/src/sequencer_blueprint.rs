// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H256, U256};
use rlp::{Decodable, DecoderError, Encodable};
use tezos_crypto_rs::hash::Signature;
use tezos_ethereum::rlp_helpers::{
    self, append_timestamp, append_u16_le, append_u256_le, decode_field_u16_le,
    decode_field_u256_le, decode_timestamp,
};

use crate::{blueprint::Blueprint, delayed_inbox::Hash};

#[derive(Debug, Clone)]
pub struct BlueprintWithDelayedHashes {
    pub parent_hash: H256,
    pub delayed_hashes: Vec<Hash>,
    pub blueprint: Blueprint,
}

impl Encodable for BlueprintWithDelayedHashes {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let BlueprintWithDelayedHashes {
            parent_hash,
            delayed_hashes,
            blueprint:
                Blueprint {
                    transactions,
                    timestamp,
                },
        } = self;
        stream.begin_list(4);
        rlp_helpers::append_h256(stream, *parent_hash);
        stream.append_list(delayed_hashes);
        stream.append_list(transactions);
        append_timestamp(stream, *timestamp);
    }
}

impl Decodable for BlueprintWithDelayedHashes {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 4 {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let parent_hash =
            rlp_helpers::decode_field_h256(&rlp_helpers::next(&mut it)?, "parent_hash")?;
        let delayed_hashes =
            rlp_helpers::decode_list(&rlp_helpers::next(&mut it)?, "delayed_hashes")?;
        let transactions =
            rlp_helpers::decode_list(&rlp_helpers::next(&mut it)?, "transactions")?;
        let timestamp = decode_timestamp(&rlp_helpers::next(&mut it)?)?;

        Ok(Self {
            delayed_hashes,
            parent_hash,
            blueprint: Blueprint {
                transactions,
                timestamp,
            },
        })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnsignedSequencerBlueprint {
    pub chunk: Vec<u8>,
    pub number: U256,
    pub nb_chunks: u16,
    pub chunk_index: u16,
}

#[derive(PartialEq, Debug, Clone)]
pub struct SequencerBlueprint {
    pub blueprint: UnsignedSequencerBlueprint,
    pub signature: Signature,
}

impl From<&SequencerBlueprint> for UnsignedSequencerBlueprint {
    fn from(val: &SequencerBlueprint) -> UnsignedSequencerBlueprint {
        val.blueprint.clone()
    }
}

impl Encodable for UnsignedSequencerBlueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(4);
        stream.append(&self.chunk);
        append_u256_le(stream, &self.number);
        append_u16_le(stream, &self.nb_chunks);
        append_u16_le(stream, &self.chunk_index);
    }
}

impl Encodable for SequencerBlueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(5);
        stream.append(&self.blueprint.chunk);
        append_u256_le(stream, &self.blueprint.number);
        append_u16_le(stream, &self.blueprint.nb_chunks);
        append_u16_le(stream, &self.blueprint.chunk_index);
        stream.append(&self.signature.0);
    }
}

impl Decodable for SequencerBlueprint {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 5 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let chunk = rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "chunk")?;
        let number = decode_field_u256_le(&rlp_helpers::next(&mut it)?, "number")?;
        let nb_chunks = decode_field_u16_le(&rlp_helpers::next(&mut it)?, "nb_chunks")?;
        let chunk_index =
            decode_field_u16_le(&rlp_helpers::next(&mut it)?, "chunk_index")?;
        let bytes: Vec<u8> =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "signature")?;
        let signature = Signature::try_from(bytes.as_slice())
            .map_err(|_| DecoderError::Custom("Invalid signature encoding"))?;
        let blueprint = UnsignedSequencerBlueprint {
            chunk,
            number,
            nb_chunks,
            chunk_index,
        };
        Ok(Self {
            blueprint,
            signature,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{SequencerBlueprint, UnsignedSequencerBlueprint};
    use crate::blueprint::Blueprint;
    use crate::inbox::Transaction;
    use crate::inbox::TransactionContent::Ethereum;
    use primitive_types::{H160, U256};
    use rlp::Encodable;
    use tezos_crypto_rs::hash::Signature;
    use tezos_ethereum::rlp_helpers::FromRlpBytes;
    use tezos_ethereum::{
        transaction::TRANSACTION_HASH_SIZE, tx_common::EthereumTransactionCommon,
    };
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    fn sequencer_blueprint_roundtrip(v: SequencerBlueprint) {
        let bytes = v.rlp_bytes();
        let v2: SequencerBlueprint = FromRlpBytes::from_rlp_bytes(&bytes)
            .expect("Sequencer blueprint should be decodable");
        assert_eq!(v, v2, "Roundtrip failed on {:?}", v)
    }

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    fn tx_(i: u64) -> EthereumTransactionCommon {
        EthereumTransactionCommon::new(
            tezos_ethereum::transaction::TransactionType::Legacy,
            Some(U256::one()),
            U256::from(i),
            U256::from(40000000u64),
            U256::from(40000000u64),
            21000u64,
            address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea"),
            U256::from(500000000u64),
            vec![],
            vec![],
            None,
        )
    }

    fn dummy_transaction(i: u8) -> Transaction {
        Transaction {
            tx_hash: [i; TRANSACTION_HASH_SIZE],
            content: Ethereum(tx_(i.into())),
        }
    }

    fn dummy_blueprint() -> SequencerBlueprint {
        let transactions = vec![dummy_transaction(0), dummy_transaction(1)];
        let timestamp = Timestamp::from(42);
        let blueprint = Blueprint {
            timestamp,
            transactions,
        };
        let chunk = rlp::Encodable::rlp_bytes(&blueprint);
        let signature = Signature::from_base58_check(
            "sigdGBG68q2vskMuac4AzyNb1xCJTfuU8MiMbQtmZLUCYydYrtTd5Lessn1EFLTDJzjXoYxRasZxXbx6tHnirbEJtikcMHt3"
        ).expect("signature decoding should work");

        SequencerBlueprint {
            blueprint: UnsignedSequencerBlueprint {
                chunk: chunk.into(),
                number: U256::from(42),
                nb_chunks: 1u16,
                chunk_index: 0u16,
            },
            signature,
        }
    }

    #[test]
    fn roundtrip_rlp() {
        let v = dummy_blueprint();
        sequencer_blueprint_roundtrip(v);
    }
}
