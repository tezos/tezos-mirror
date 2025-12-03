// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::PublicKeyHash;
use primitive_types::U256;
use revm_etherlink::tezosx::{
    ethereum_address_from_tezos, set_ethereum_address_mapping, ForeignAddress,
};
use revm_etherlink::Error;
use rlp::{Decodable, Encodable, Rlp};
use tezos_ethereum::rlp_helpers::{
    append_u256_le, append_u64_le, decode_field_u256_le, decode_field_u64_le,
};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::PublicKey;
use tezos_smart_rollup_host::path::PathError;
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, RefPath},
    runtime::RuntimeError,
};

// Path where Tezos accounts are stored.
const TEZOS_ACCOUNTS_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/eth_accounts/tezos");

// Path where all the infos of a Tezos contract are stored under the same key.
// This path must contains balance, nonce and optionally a revealed public key.
const INFO_PATH: RefPath = RefPath::assert_from(b"/info");

// Used as a value for the durable storage.
#[derive(Debug, Eq, PartialEq, Clone, Default)]
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

fn path_to_tezos_account(pub_key_hash: &PublicKeyHash) -> Result<OwnedPath, PathError> {
    let address_path: Vec<u8> = format!("/{pub_key_hash}").into();
    let address_path = OwnedPath::try_from(address_path)?;
    let prefix = concat(&TEZOS_ACCOUNTS_PATH, &address_path)?;
    concat(&prefix, &INFO_PATH)
}

pub fn add_balance(
    host: &mut impl Runtime,
    pub_key_hash: &PublicKeyHash,
    amount: U256,
) -> Result<(), Error> {
    let mut info = get_tezos_account_info_or_init(host, pub_key_hash)?;
    info.balance = info
        .balance
        .checked_add(amount)
        .ok_or(Error::Custom("Balance overflow".to_string()))?;
    set_tezos_account_info(host, pub_key_hash, info)
}

pub fn get_tezos_account_info(
    host: &impl Runtime,
    pub_key_hash: &PublicKeyHash,
) -> Result<Option<TezosAccountInfo>, Error> {
    let path =
        path_to_tezos_account(pub_key_hash).map_err(|_| RuntimeError::PathNotFound)?;
    match host.store_read_all(&path) {
        Ok(bytes) => {
            let account_info = TezosAccountInfo::decode(&Rlp::new(&bytes))
                .map_err(|_| RuntimeError::DecodingError)?;
            Ok(Some(account_info))
        }
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(Error::Runtime(err)),
    }
}

pub fn get_tezos_account_info_or_init(
    host: &mut impl Runtime,
    pub_key_hash: &PublicKeyHash,
) -> Result<TezosAccountInfo, Error> {
    match get_tezos_account_info(host, pub_key_hash)? {
        Some(info) => Ok(info),
        None => {
            let evm_addr = ethereum_address_from_tezos(pub_key_hash);
            let foreign_addr = ForeignAddress::Tezos(pub_key_hash.clone());
            set_ethereum_address_mapping(host, &evm_addr, foreign_addr)?;
            Ok(TezosAccountInfo::default())
        }
    }
}

pub fn set_tezos_account_info(
    host: &mut impl Runtime,
    pub_key_hash: &PublicKeyHash,
    info: TezosAccountInfo,
) -> Result<(), Error> {
    let path =
        path_to_tezos_account(pub_key_hash).map_err(|_| RuntimeError::PathNotFound)?;
    let value = &info.rlp_bytes();
    Ok(host.store_write(&path, value, 0)?)
}

#[cfg(test)]
mod tests {
    use crate::tezosx::*;
    use alloy_primitives::Address;
    use revm_etherlink::tezosx::*;
    use std::str::FromStr;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;

    #[test]
    fn tezos_account_info_size_constant() {
        let pub_key: PublicKey = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key should be a b58 string");
        let account = TezosAccountInfo {
            balance: U256::zero(),
            nonce: 0,
            pub_key: Some(pub_key),
        };
        let rlp_size = account.rlp_bytes().len();
        // Reading an account info in one go is safe
        assert!(rlp_size < MAX_FILE_CHUNK_SIZE);
    }

    #[test]
    fn tezos_account_info_encoding() {
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
        let decoded_account = TezosAccountInfo::decode(&Rlp::new(bytes))
            .expect("Account should be decodable");
        assert!(decoded_account == account);
    }

    #[test]
    fn tezos_account_storage() {
        let mut host = MockKernelHost::default();

        let pub_key_hash: PublicKeyHash =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                .expect("Public key hash should be a b58 string");
        let pub_key: PublicKey = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key should be a b58 string");
        let account = TezosAccountInfo {
            balance: U256::from(1234u64),
            nonce: 18,
            pub_key: Some(pub_key.clone()),
        };

        set_tezos_account_info(&mut host, &pub_key_hash, account.clone())
            .expect("Writing to the storage should have worked");

        let read_account = get_tezos_account_info(&host, &pub_key_hash)
            .expect("Reading the storage should have worked")
            .expect("The path to the account should exist");
        assert_eq!(account, read_account);
    }

    #[test]
    fn foreign_address_encoding() {
        let pub_key_hash: PublicKeyHash =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                .expect("Public key hash should be a b58 string");
        let source_address = ForeignAddress::Tezos(pub_key_hash);
        let bytes = &source_address.rlp_bytes();
        let decoded_address = ForeignAddress::decode(&Rlp::new(bytes))
            .expect("Address should be decodable");
        assert!(decoded_address == source_address);
    }

    #[test]
    fn ethereum_address_mapping_storage() {
        let mut host = MockKernelHost::default();

        let address: Address =
            Address::from_str("0x2E2Ac8699AD02e710951ea0F56b892Ed36916Cd5")
                .expect("Hex should be an EVM address");
        let pub_key_hash: PublicKeyHash =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                .expect("Public key hash should be a b58 string");
        let address_mapping = ForeignAddress::Tezos(pub_key_hash);

        set_ethereum_address_mapping(&mut host, &address, address_mapping.clone())
            .expect("Writing to the storage should have worked");

        let read_address_mapping = get_ethereum_address_mapping(&host, &address)
            .expect("Reading the storage should have worked")
            .expect("The path to the account should exist");
        assert_eq!(address_mapping, read_address_mapping);
    }
}
