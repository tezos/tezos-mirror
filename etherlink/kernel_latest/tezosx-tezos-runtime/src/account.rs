// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use rlp::{Decodable, Encodable, Rlp};
use tezos_crypto_rs::{public_key::PublicKey, public_key_hash::PublicKeyHash};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader, types::Narith};
use tezos_ethereum::rlp_helpers::{
    append_u256_le, append_u64_le, decode_field_u256_le, decode_field_u64_le,
};
use tezos_execution::account_storage::{
    Manager, TezlinkAccount, TezosImplicitAccountTrait,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, RefPath},
    runtime::RuntimeError,
};
use tezosx_interfaces::{Origin, TezosXRuntimeError};

// Path where Tezos accounts are stored.
pub(crate) const TEZOS_ACCOUNTS_PATH: RefPath =
    RefPath::assert_from(b"/tez/tez_accounts/tezosx");

// Path where all the infos of a Tezos contract are stored under the same key.
// This path must contains balance, nonce and optionally a revealed public key.
const INFO_PATH: RefPath = RefPath::assert_from(b"/info");

/// Sibling path that holds the classification record of an originated KT1
/// account: the segment is appended to the account prefix and the resulting
/// path lives next to the rest of the account state. Implicit accounts carry
/// no `/origin` record — a tz1/2/3 is Tezos-native by construction (see
/// [`crate::context`]), so only KT1 accounts (which may be `Native` or an
/// `Alias` forwarder) store a classification.
///
/// Re-exported from `tezos_execution` rather than redeclared, so the runtime
/// (writer) and the execution layer (reader) share a single source of truth.
pub(crate) use tezos_execution::context::code::ORIGIN_PATH;

pub fn narith_to_u256(
    narith: &Narith,
) -> Result<primitive_types::U256, TezosXRuntimeError> {
    let bytes = narith.0.to_bytes_be();
    if bytes.len() <= 32 {
        Ok(primitive_types::U256::from_big_endian(&bytes))
    } else {
        Err(TezosXRuntimeError::ConversionError(
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

/// Path of the durable storage subtree for an implicit account. The
/// info RLP record sits under this prefix at the info segment; the
/// classification record sits at the origin segment.
pub fn path_to_implicit_account_prefix(
    pub_key_hash: &PublicKeyHash,
) -> Result<OwnedPath, TezosXRuntimeError> {
    let address_path: Vec<u8> = format!("/{pub_key_hash}").into();
    let address_path = OwnedPath::try_from(address_path)
        .map_err(|e| TezosXRuntimeError::Custom(e.to_string()))?;
    Ok(concat(&TEZOS_ACCOUNTS_PATH, &address_path)?)
}

pub fn path_to_tezos_account(
    pub_key_hash: &PublicKeyHash,
) -> Result<OwnedPath, TezosXRuntimeError> {
    let prefix = path_to_implicit_account_prefix(pub_key_hash)?;
    Ok(concat(&prefix, &INFO_PATH)?)
}

pub fn get_tezos_account_info(
    host: &impl StorageV1,
    pub_key_hash: &PublicKeyHash,
) -> Result<Option<TezosAccountInfo>, TezosXRuntimeError> {
    let path =
        path_to_tezos_account(pub_key_hash).map_err(|_| RuntimeError::PathNotFound)?;
    match host.store_read_all(&path) {
        Ok(bytes) => {
            let account_info = TezosAccountInfo::decode(&Rlp::new(&bytes))
                .map_err(|_| RuntimeError::DecodingError)?;
            Ok(Some(account_info))
        }
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(TezosXRuntimeError::Runtime(err)),
    }
}

pub fn get_tezos_account_info_or_init(
    host: &mut impl StorageV1,
    pub_key_hash: &PublicKeyHash,
) -> Result<TezosAccountInfo, TezosXRuntimeError> {
    match get_tezos_account_info(host, pub_key_hash)? {
        Some(info) => Ok(info),
        None => Ok(TezosAccountInfo::default()),
    }
}

pub fn set_tezos_account_info(
    host: &mut impl StorageV1,
    pub_key_hash: &PublicKeyHash,
    info: TezosAccountInfo,
) -> Result<(), TezosXRuntimeError> {
    let path =
        path_to_tezos_account(pub_key_hash).map_err(|_| RuntimeError::PathNotFound)?;
    let value = &info.rlp_bytes();
    Ok(host.store_write(&path, value, 0)?)
}

/// Read the classification record stored at the origin path under the
/// given KT1 account path. Only originated accounts store a classification:
/// the record sits next to the rest of the contract's state. Implicit
/// accounts are `Native` by construction and keep no `/origin` record.
pub fn get_origin_at(
    host: &impl StorageV1,
    account_path: &OwnedPath,
) -> Result<Option<Origin>, TezosXRuntimeError> {
    let path = concat(account_path, &ORIGIN_PATH)?;
    match host.store_read_all(&path) {
        Ok(bytes) => {
            let (rest, origin) = Origin::nom_read(&bytes).map_err(|_| {
                TezosXRuntimeError::ConversionError("Invalid origin encoding".to_string())
            })?;
            if !rest.is_empty() {
                return Err(TezosXRuntimeError::ConversionError(
                    "Trailing bytes after origin encoding".to_string(),
                ));
            }
            Ok(Some(origin))
        }
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(TezosXRuntimeError::Runtime(err)),
    }
}

/// Write the classification record at the origin path under the given account path.
pub fn set_origin_at(
    host: &mut impl StorageV1,
    account_path: &OwnedPath,
    origin: &Origin,
) -> Result<(), TezosXRuntimeError> {
    let path = concat(account_path, &ORIGIN_PATH)?;
    let mut buf = Vec::new();
    origin.bin_write(&mut buf).map_err(|e| {
        TezosXRuntimeError::ConversionError(format!("encoding Origin failed: {e}"))
    })?;
    Ok(host.store_write(&path, &buf, 0)?)
}

/// Seed the shared alias implementation with the canonical forwarder code if
/// the slot is empty, leaving an already-populated slot untouched.
///
/// Idempotent, so it is safe to call from both seeding points: the Michelson
/// runtime activation (fresh networks) and the storage migration
/// (already-deployed networks).
///
/// The slot itself lives in `tezos_execution` (see
/// [`tezos_execution::account_storage::read_alias_implementation`]); this
/// wrapper supplies the canonical forwarder code, which is Michelson-runtime
/// specific and so belongs in this crate rather than in the generic execution
/// layer.
pub fn init_alias_implementation(
    host: &mut impl StorageV1,
) -> Result<(), TezosXRuntimeError> {
    use tezos_execution::account_storage::{
        read_alias_implementation, write_alias_implementation,
    };
    // Storage failures keep their structured `Storage` classification through
    // the existing `From<tezos_storage::error::Error>` impl; only the hex decode
    // of the compile-time-constant forwarder string needs a custom message.
    let seeded = read_alias_implementation(host)?;
    if seeded.is_none() {
        let code = crate::alias_forwarder::forwarder_code().map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "decoding forwarder code from hex failed: {e}"
            ))
        })?;
        write_alias_implementation(host, &code)?;
    }
    Ok(())
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
        host: &impl StorageV1,
    ) -> Result<Narith, tezos_storage::error::Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(info)) => Ok(u256_to_narith(&info.balance)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(tezos_storage::error::Error::TcError(format!("{e}"))),
        }
    }

    fn set_balance(
        &self,
        host: &mut impl StorageV1,
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
        host: &impl StorageV1,
    ) -> Result<Narith, tezos_storage::error::Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(info)) => Ok(Narith::from(info.nonce)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(tezos_storage::error::Error::NomReadError(format!("{e}"))),
        }
    }

    fn set_counter(
        &self,
        host: &mut impl StorageV1,
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
        host: &impl StorageV1,
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
        _host: &mut impl StorageV1,
        _public_key_hash: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        // In TezosX, we do not need this function which is used in Tezlink
        // only for backward compatibility.
        Ok(())
    }

    fn set_manager_public_key(
        &self,
        host: &mut impl StorageV1,
        public_key: &tezos_smart_rollup::types::PublicKey,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        info.pub_key = Some(public_key.clone());
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        Ok(())
    }

    fn allocated(
        &self,
        host: &impl StorageV1,
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

    #[test]
    fn origin_at_path_roundtrip() {
        use crate::account::{get_origin_at, set_origin_at};
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
        use tezosx_interfaces::{AliasInfo, Origin, RuntimeId};

        let mut host = MockKernelHost::default();
        let path: OwnedPath =
            RefPath::assert_from(b"/test/some/originated/account").into();

        // An absent path returns no value.
        assert!(get_origin_at(&host, &path).unwrap().is_none());

        // The native variant round-trips.
        set_origin_at(&mut host, &path, &Origin::Native).unwrap();
        assert_eq!(get_origin_at(&host, &path).unwrap(), Some(Origin::Native));

        // The alias variant round-trips.
        let alias = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Tezos,
            native_address: b"tz1...".to_vec(),
        });
        set_origin_at(&mut host, &path, &alias).unwrap();
        assert_eq!(get_origin_at(&host, &path).unwrap(), Some(alias));
    }

    #[test]
    fn alias_implementation_seed_and_roundtrip() {
        use crate::account::init_alias_implementation;
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezos_execution::account_storage::{
            read_alias_implementation, write_alias_implementation,
        };

        let mut host = MockKernelHost::default();

        // Absent before seeding.
        assert!(read_alias_implementation(&host).unwrap().is_none());

        // Seeding installs the canonical forwarder code.
        init_alias_implementation(&mut host).unwrap();
        let forwarder = crate::alias_forwarder::forwarder_code().unwrap();
        assert_eq!(
            read_alias_implementation(&host).unwrap(),
            Some(forwarder.clone())
        );

        // Seeding again is a no-op: an already-populated slot is untouched.
        write_alias_implementation(&mut host, b"already here").unwrap();
        init_alias_implementation(&mut host).unwrap();
        assert_eq!(
            read_alias_implementation(&host).unwrap(),
            Some(b"already here".to_vec())
        );

        // An explicit write overwrites — the O(1) upgrade primitive.
        write_alias_implementation(&mut host, &forwarder).unwrap();
        assert_eq!(read_alias_implementation(&host).unwrap(), Some(forwarder));
    }

    #[test]
    fn set_manager_public_key_reveals_manager() {
        use crate::account::TezosImplicitAccount;
        use tezos_crypto_rs::{public_key::PublicKey, public_key_hash::PublicKeyHash};
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezos_execution::account_storage::{Manager, TezosImplicitAccountTrait};

        let mut host = MockKernelHost::default();
        let pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let public_key = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .unwrap();
        let path = crate::account::path_to_tezos_account(&pkh).unwrap();
        let account = TezosImplicitAccount {
            pkh: pkh.clone(),
            path,
        };

        // Reveal records the public key as the account's manager. It writes no
        // `/origin` record — an implicit account is Native by construction.
        account
            .set_manager_public_key(&mut host, &public_key)
            .unwrap();
        assert_eq!(
            account.manager(&host).unwrap(),
            Manager::Revealed(public_key)
        );
    }
}
