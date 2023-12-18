// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::rlp_helpers::{
    self, append_u16_le, append_u256_le, decode_field_u16_le, decode_field_u256_le,
};

#[derive(PartialEq, Debug, Clone)]
pub struct SequencerBlueprint {
    pub chunk: Vec<u8>,
    pub number: U256,
    pub nb_chunks: u16,
    pub chunk_index: u16,
}

impl Encodable for SequencerBlueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(4);
        stream.append(&self.chunk);
        append_u256_le(stream, &self.number);
        append_u16_le(stream, &self.nb_chunks);
        append_u16_le(stream, &self.chunk_index);
    }
}

impl Decodable for SequencerBlueprint {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 4 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let chunk = rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "chunk")?;
        let number = decode_field_u256_le(&rlp_helpers::next(&mut it)?, "number")?;
        let nb_chunks = decode_field_u16_le(&rlp_helpers::next(&mut it)?, "nb_chunks")?;
        let chunk_index =
            decode_field_u16_le(&rlp_helpers::next(&mut it)?, "chunk_index")?;
        Ok(Self {
            chunk,
            number,
            nb_chunks,
            chunk_index,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::SequencerBlueprint;
    use crate::blueprint::Blueprint;
    use crate::inbox::Transaction;
    use crate::inbox::TransactionContent::Ethereum;
    use primitive_types::{H160, U256};
    use rlp::Encodable;
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
        EthereumTransactionCommon {
            type_: tezos_ethereum::transaction::TransactionType::Legacy,
            chain_id: U256::one(),
            nonce: U256::from(i),
            max_priority_fee_per_gas: U256::from(40000000u64),
            max_fee_per_gas: U256::from(40000000u64),
            gas_limit: 21000u64,
            to: address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea"),
            value: U256::from(500000000u64),
            data: vec![],
            access_list: vec![],
            signature: None,
        }
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
        SequencerBlueprint {
            chunk: chunk.into(),
            number: U256::from(42),
            nb_chunks: 1u16,
            chunk_index: 0u16,
        }
    }

    #[test]
    fn roundtrip_rlp() {
        let v = dummy_blueprint();
        sequencer_blueprint_roundtrip(v);
    }
}
