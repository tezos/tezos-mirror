// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::inbox::Transaction;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::rlp_helpers::{self, append_timestamp, decode_timestamp};

use tezos_smart_rollup_encoding::timestamp::Timestamp;

/// The blueprint of a block is a list of transactions.
#[derive(PartialEq, Debug, Clone)]
pub struct Blueprint {
    pub transactions: Vec<Transaction>,
    pub timestamp: Timestamp,
}

impl Encodable for Blueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append_list(&self.transactions);
        append_timestamp(stream, self.timestamp);
    }
}

impl Decodable for Blueprint {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let transactions =
            rlp_helpers::decode_list(&rlp_helpers::next(&mut it)?, "transactions")?;
        let timestamp = decode_timestamp(&rlp_helpers::next(&mut it)?)?;

        Ok(Blueprint {
            transactions,
            timestamp,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::inbox::TransactionContent::Ethereum;
    use primitive_types::{H160, U256};
    use rlp::Rlp;
    use tezos_ethereum::{
        transaction::TRANSACTION_HASH_SIZE, tx_common::EthereumTransactionCommon,
    };

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

    #[test]
    fn test_encode_blueprint() {
        let proposal = Blueprint {
            transactions: vec![dummy_transaction(0), dummy_transaction(1)],
            timestamp: Timestamp::from(0i64),
        };
        let encoded = proposal.rlp_bytes();
        let decoder = Rlp::new(&encoded);
        let decoded = Blueprint::decode(&decoder).expect("Should be decodable");
        assert_eq!(decoded, proposal);
    }
}
