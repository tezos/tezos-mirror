// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use rlp::{Decodable, Encodable, Rlp};
use tezos_crypto_rs::{public_key::PublicKey, public_key_hash::PublicKeyHash};
use tezos_data_encoding::types::Narith;
use tezos_ethereum::rlp_helpers::{
    append_u256_le, append_u64_le, decode_field_u256_le, decode_field_u64_le,
};
use tezos_evm_runtime::runtime::Runtime;
use tezos_execution::account_storage::{
    Manager, TezlinkAccount, TezosImplicitAccount as TezosImplicitAccountTrait,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, RefPath},
    runtime::RuntimeError,
};

use crate::TezosRuntimeError;

// Path where Tezos accounts are stored.
pub(crate) const TEZOS_ACCOUNTS_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/eth_accounts/tezos");

// Path where all the infos of a Tezos contract are stored under the same key.
// This path must contains balance, nonce and optionally a revealed public key.
const INFO_PATH: RefPath = RefPath::assert_from(b"/info");

pub fn narith_to_u256(
    narith: &Narith,
) -> Result<primitive_types::U256, TezosRuntimeError> {
    let bytes = narith.0.to_bytes_be();
    if bytes.len() <= 32 {
        Ok(primitive_types::U256::from_big_endian(&bytes))
    } else {
        Err(TezosRuntimeError::ConversionError(
            "Narith value too large to fit in U256".to_string(),
        ))
    }
}

pub fn u256_to_narith(value: &primitive_types::U256) -> Narith {
    let mut bytes = [0u8; 32];
    value.to_big_endian(&mut bytes);
    Narith(num_bigint::BigUint::from_bytes_be(&bytes))
}

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

pub fn path_to_tezos_account(
    pub_key_hash: &PublicKeyHash,
) -> Result<OwnedPath, TezosRuntimeError> {
    let address_path: Vec<u8> = format!("/{pub_key_hash}").into();
    let address_path = OwnedPath::try_from(address_path)
        .map_err(|e| TezosRuntimeError::Custom(e.to_string()))?;
    let prefix = concat(&TEZOS_ACCOUNTS_PATH, &address_path)?;
    Ok(concat(&prefix, &INFO_PATH)?)
}

pub fn get_tezos_account_info(
    host: &impl Runtime,
    pub_key_hash: &PublicKeyHash,
) -> Result<Option<TezosAccountInfo>, TezosRuntimeError> {
    let path =
        path_to_tezos_account(pub_key_hash).map_err(|_| RuntimeError::PathNotFound)?;
    match host.store_read_all(&path) {
        Ok(bytes) => {
            let account_info = TezosAccountInfo::decode(&Rlp::new(&bytes))
                .map_err(|_| RuntimeError::DecodingError)?;
            Ok(Some(account_info))
        }
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(TezosRuntimeError::Runtime(err)),
    }
}

pub fn get_tezos_account_info_or_init(
    host: &mut impl Runtime,
    pub_key_hash: &PublicKeyHash,
) -> Result<TezosAccountInfo, TezosRuntimeError> {
    match get_tezos_account_info(host, pub_key_hash)? {
        Some(info) => Ok(info),
        None => Ok(TezosAccountInfo::default()),
    }
}

pub fn set_tezos_account_info(
    host: &mut impl Runtime,
    pub_key_hash: &PublicKeyHash,
    info: TezosAccountInfo,
) -> Result<(), TezosRuntimeError> {
    let path =
        path_to_tezos_account(pub_key_hash).map_err(|_| RuntimeError::PathNotFound)?;
    let value = &info.rlp_bytes();
    Ok(host.store_write(&path, value, 0)?)
}

pub struct TezosImplicitAccount {
    pub(crate) pkh: PublicKeyHash,
    pub(crate) path: OwnedPath,
}

impl TezlinkAccount for TezosImplicitAccount {
    fn path(&self) -> &OwnedPath {
        &self.path
    }

    fn contract(&self) -> Contract {
        Contract::Implicit(self.pkh.clone())
    }

    fn balance(
        &self,
        host: &impl Runtime,
    ) -> Result<Narith, tezos_storage::error::Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(info)) => Ok(u256_to_narith(&info.balance)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(tezos_storage::error::Error::TcError(format!("{e}"))),
        }
    }

    fn set_balance(
        &self,
        host: &mut impl Runtime,
        balance: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))?;
        info.balance = narith_to_u256(balance)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))?;
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))
    }
}

impl TezosImplicitAccountTrait for TezosImplicitAccount {
    fn pkh(&self) -> &PublicKeyHash {
        &self.pkh
    }

    fn counter(
        &self,
        host: &impl Runtime,
    ) -> Result<Narith, tezos_storage::error::Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(info)) => Ok(Narith::from(info.nonce)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(tezos_storage::error::Error::NomReadError(format!("{e}"))),
        }
    }

    fn set_counter(
        &self,
        host: &mut impl Runtime,
        counter: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        info.nonce = counter
            .0
            .clone()
            .try_into()
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))
    }

    fn manager(
        &self,
        host: &impl Runtime,
    ) -> Result<Manager, tezos_storage::error::Error> {
        let info = get_tezos_account_info(host, &self.pkh)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        match info {
            Some(info) => match info.pub_key {
                Some(pk) => Ok(Manager::Revealed(pk)),
                None => Ok(Manager::NotRevealed(self.pkh.clone())),
            },
            None => Ok(Manager::NotRevealed(self.pkh.clone())),
        }
    }

    fn set_manager_pk_hash_internal(
        &self,
        _host: &mut impl Runtime,
        _public_key_hash: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        // In TezosX, we do not need this function which is used in Tezlink
        // only for backward compatibility.
        Ok(())
    }

    fn set_manager_public_key(
        &self,
        host: &mut impl Runtime,
        public_key: &tezos_smart_rollup::types::PublicKey,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        info.pub_key = Some(public_key.clone());
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))
    }

    fn allocated(
        &self,
        host: &impl Runtime,
    ) -> Result<bool, tezos_storage::error::Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(_)) => Ok(true),
            Ok(None) => Ok(false),
            Err(e) => Err(tezos_storage::error::Error::NomReadError(format!("{e}"))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::account::narith_to_u256;
    use primitive_types::U256;
    use tezos_data_encoding::types::Narith;

    #[test]
    fn convert_narith_u256() {
        let narith = Narith::from(123456789u64);
        let u256_value = narith_to_u256(&narith).unwrap();
        assert_eq!(u256_value.low_u64(), 123456789u64);
    }

    #[test]
    fn convert_u256_narith() {
        let big_value = U256::from_dec_str("123456789012345678901234567890").unwrap();
        let narith_value = super::u256_to_narith(&big_value);
        let expected_narith = Narith(
            num_bigint::BigUint::from_str("123456789012345678901234567890").unwrap(),
        );
        assert_eq!(narith_value, expected_narith);
    }

    #[test]
    fn narith_u256_overflow() {
        let u256_value = U256::MAX;
        let narith_value = super::u256_to_narith(&u256_value);
        let narith_value = Narith(narith_value.0 + num_bigint::BigUint::from(1u64));
        let _ = narith_to_u256(&narith_value).expect_err("Should error on overflow");
    }
}
