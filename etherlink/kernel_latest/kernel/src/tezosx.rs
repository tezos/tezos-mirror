// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use rlp::{Decodable, Encodable, Rlp};
use tezos_ethereum::rlp_helpers::{
    append_u256_le, append_u64_le, decode_field_u256_le, decode_field_u64_le,
};
use tezos_smart_rollup::types::PublicKey;

// Used as a value for the durable storage.
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct TezosAccountInfo {
    pub balance: U256,
    pub nonce: u64,
    pub pub_key: Option<PublicKey>,
}

impl Encodable for TezosAccountInfo {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.begin_list(3);
        append_u256_le(s, &self.balance);
        append_u64_le(s, &self.nonce);
        match &self.pub_key {
            Some(pub_key) => s.append(&pub_key.to_b58check().as_bytes()),
            None => s.append_empty_data(),
        };
    }
}

impl Decodable for TezosAccountInfo {
    fn decode(rlp: &Rlp) -> Result<Self, rlp::DecoderError> {
        if !rlp.is_list() {
            return Err(rlp::DecoderError::RlpExpectedToBeList);
        }
        if rlp.item_count()? != 3 {
            return Err(rlp::DecoderError::RlpIncorrectListLen);
        }
        let mut it = rlp.iter();
        let balance_decoder = it.next().ok_or(rlp::DecoderError::RlpExpectedToBeList)?;
        let balance = decode_field_u256_le(&balance_decoder, "balance")?;
        let nonce_decoder = it.next().ok_or(rlp::DecoderError::RlpExpectedToBeList)?;
        let nonce = decode_field_u64_le(&nonce_decoder, "nonce")?;
        let pub_key_decoder = it.next().ok_or(rlp::DecoderError::RlpExpectedToBeList)?;
        let pub_key: Option<PublicKey> = if pub_key_decoder.is_empty() {
            None
        } else {
            let vec: Vec<u8> = pub_key_decoder.as_val()?;
            let s: String = String::from_utf8(vec).map_err(|_| {
                rlp::DecoderError::Custom("Invalid public key (not a string)")
            })?;
            let pub_key = PublicKey::from_b58check(&s).map_err(|_| {
                rlp::DecoderError::Custom("Invalid public key (b58check)")
            })?;
            Some(pub_key)
        };

        Ok(TezosAccountInfo {
            balance,
            nonce,
            pub_key,
        })
    }
}

#[test]
fn tezos_account_info_encoding() {
    use crate::tezosx::TezosAccountInfo;
    use primitive_types::U256;
    use rlp::Rlp;
    use tezos_smart_rollup::types::PublicKey;

    let pub_key: PublicKey = PublicKey::from_b58check(
        "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
    )
    .expect("Public key should be a b58 string");
    let account = TezosAccountInfo {
        balance: U256::from(1234u64),
        nonce: 18,
        pub_key: Some(pub_key),
    };
    let bytes = &account.rlp_bytes();
    let decoded_account =
        TezosAccountInfo::decode(&Rlp::new(bytes)).expect("Account should be decodable");
    assert!(decoded_account == account);
}
