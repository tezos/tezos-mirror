// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::rlp_helpers::{
    self, append_timestamp, append_u256_le, decode_field_u256_le, decode_timestamp,
};
use tezos_smart_rollup_encoding::timestamp::Timestamp;

#[derive(PartialEq, Debug, Clone)]
pub struct SequencerBlueprint {
    pub timestamp: Timestamp,
    pub transactions: Vec<u8>,
    pub number: U256,
}

impl Encodable for SequencerBlueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(3);
        stream.append_list(&self.transactions);
        append_timestamp(stream, self.timestamp);
        append_u256_le(stream, &self.number);
    }
}

impl Decodable for SequencerBlueprint {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 3 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let transactions =
            rlp_helpers::decode_list(&rlp_helpers::next(&mut it)?, "transactions")?;
        let timestamp = decode_timestamp(&rlp_helpers::next(&mut it)?)?;
        let number = decode_field_u256_le(&rlp_helpers::next(&mut it)?, "number")?;
        Ok(Self {
            transactions,
            timestamp,
            number,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::SequencerBlueprint;
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
        let transaction_bytes = rlp::encode_list(&transactions).into();
        SequencerBlueprint {
            timestamp: Timestamp::from(42),
            transactions: transaction_bytes,
            number: U256::from(42),
        }
    }

    #[test]
    fn roundtrip_rlp() {
        let v = dummy_blueprint();
        sequencer_blueprint_roundtrip(v);
    }
}
