// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Account addressing, construction, and origin classification helpers.

use crate::account_storage::{TezosImplicitAccount, TezosOriginatedAccount};
use mir::ast::{big_map::BigMapId, AddressHash};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_runtime::runtime_keyspaces::TEZ_ACCOUNTS_ROOT_PATH;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_smart_rollup_host::path::{concat, OwnedPath, Path, PathError, RefPath};
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{Key, KeyError};
use tezosx_interfaces::Origin;

// Account resolution helpers.

/// Resolve the implicit (`tz1`/`tz2`/`tz3`) account for a public key hash.
pub fn implicit_from_public_key_hash(
    pkh: &PublicKeyHash,
) -> Result<TezosImplicitAccount, tezos_storage::error::Error> {
    Ok(TezosImplicitAccount { pkh: pkh.clone() })
}

/// Resolve the implicit account backing a [`Contract`]. Errors on an
/// originated (`KT1`) contract.
pub fn implicit_from_contract(
    contract: &Contract,
) -> Result<TezosImplicitAccount, tezos_storage::error::Error> {
    match contract {
        Contract::Implicit(pkh) => implicit_from_public_key_hash(pkh),
        _ => Err(tezos_storage::error::Error::OriginatedToImplicit),
    }
}

/// Resolve the originated (`KT1`) account under the Tezos accounts root.
pub fn originated_from_kt1(
    kt1: &ContractKt1Hash,
) -> Result<TezosOriginatedAccount, tezos_storage::error::Error> {
    let key = contracts::account_key(&Contract::Originated(kt1.clone()))?;
    Ok(TezosOriginatedAccount {
        kt1: kt1.clone(),
        key,
    })
}

/// Resolve the originated account backing a [`Contract`] under the Tezos
/// accounts root. Errors on an implicit contract.
pub fn originated_from_contract(
    contract: &Contract,
) -> Result<TezosOriginatedAccount, tezos_storage::error::Error> {
    match contract {
        Contract::Originated(kt1) => originated_from_kt1(kt1),
        _ => Err(tezos_storage::error::Error::ImplicitToOriginated),
    }
}

/// Read the origin classification (native / alias) for the given address.
pub fn read_origin_for_address(
    host: &impl StorageV1,
    address: &AddressHash,
) -> Result<Option<Origin>, tezos_storage::error::Error> {
    match address {
        // A tz1/2/3 is a public-key hash: it is intrinsically Tezos-native
        // and can never be an alias (aliases are materialized as KT1
        // forwarders). Its classification is therefore [Origin::Native] by
        // construction, with no durable read and no stored `/origin` record.
        AddressHash::Implicit(_) => Ok(Some(Origin::Native)),
        AddressHash::Kt1(kt1) => {
            let originated = originated_from_kt1(kt1)?;
            originated.origin(host)
        }
        AddressHash::Sr1(_) => Ok(None),
    }
}

/// Returns the durable path of `suffix` in the storage of `account`, under
/// the accounts root.
///
/// Fails when the result is not a valid storage path, for example when it is
/// too long.
fn originated_path(
    account: &TezosOriginatedAccount,
    suffix: &RefPath,
) -> Result<OwnedPath, PathError> {
    OwnedPath::try_from(
        [
            TEZ_ACCOUNTS_ROOT_PATH.as_bytes(),
            account.key.as_bytes(),
            suffix.as_bytes(),
        ]
        .concat(),
    )
}

pub mod contracts {
    use mir::ast::BinWriter;

    use super::*;

    const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

    /// The contract index, as a key of the accounts keyspace.
    const INDEX_KEY: Key = Key::from_static(b"/contracts/index");

    /// Returns the path segment `/<hex>` that identifies `contract` under the
    /// contract index. `<hex>` is the hex form of the binary encoding that the
    /// context of the octez node uses (see
    /// `octez-codec describe alpha.contract binary schema`).
    ///
    /// Fails with a decoding error if `contract` cannot be binary-encoded, or
    /// with a path error if the result is not a valid storage path.
    pub fn account_path(
        contract: &Contract,
    ) -> Result<OwnedPath, tezos_storage::error::Error> {
        let mut contract_encoded = Vec::new();
        contract
            .bin_write(&mut contract_encoded)
            .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;

        let path_string = alloc::format!("/{}", hex::encode(&contract_encoded));
        Ok(OwnedPath::try_from(path_string)?)
    }

    /// Returns the key of `contract` under the contract index of the accounts
    /// keyspace. Every key of an originated account starts with this key.
    ///
    /// Fails when `contract` cannot be binary-encoded, or when the resulting
    /// path or key is invalid (for example, too long).
    pub fn account_key(contract: &Contract) -> Result<Key, tezos_storage::error::Error> {
        Ok(INDEX_KEY.concat(account_path(contract)?.as_bytes())?)
    }

    /// Path of an originated account's mutez balance. (Implicit accounts keep
    /// their balance in the RLP `/info` record, so only originated accounts
    /// use this path.)
    pub fn balance_path(
        account: &TezosOriginatedAccount,
    ) -> Result<OwnedPath, PathError> {
        originated_path(account, &BALANCE_PATH)
    }
}

pub mod big_maps {
    use tezos_crypto_rs::hash::ScriptExprHash;

    use super::*;

    const BIG_MAP_PATH: RefPath = RefPath::assert_from(b"/big_map");

    const KEY_TYPE_PATH: RefPath = RefPath::assert_from(b"/key_type");

    const VALUE_TYPE_PATH: RefPath = RefPath::assert_from(b"/value_type");

    const TOTAL_BYTES_PATH: RefPath = RefPath::assert_from(b"/total_bytes");

    fn root() -> Result<OwnedPath, PathError> {
        concat(&TEZ_ACCOUNTS_ROOT_PATH, &BIG_MAP_PATH)
    }

    /// The key, in the accounts keyspace, of the counter that holds the next
    /// permanent big-map ID.
    pub const NEXT_ID_KEY: Key = Key::from_static(b"/big_map/next_id");

    pub fn big_map_path(id: &BigMapId) -> Result<OwnedPath, PathError> {
        concat(&root()?, &OwnedPath::try_from(format!("/{id}"))?)
    }

    pub fn key_type_path(id: &BigMapId) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(id)?, &KEY_TYPE_PATH)
    }

    pub fn value_type_path(id: &BigMapId) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(id)?, &VALUE_TYPE_PATH)
    }

    pub fn total_bytes_path(id: &BigMapId) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(id)?, &TOTAL_BYTES_PATH)
    }

    pub fn value_path(
        id: &BigMapId,
        key_hashed: &ScriptExprHash,
    ) -> Result<OwnedPath, PathError> {
        let key_hex = hex::encode(key_hashed);
        concat(
            &big_map_path(id)?,
            &OwnedPath::try_from(format!("/{key_hex}"))?,
        )
    }
}

pub mod address_registry {
    use mir::ast::{AddressHash, ByteReprTrait};

    use super::*;

    /// The registry, as a key of the accounts keyspace.
    const ROOT: Key = Key::from_static(b"/address_registry");

    /// Returns the key of the registry entry of `address`, relative to the
    /// accounts keyspace.
    ///
    /// Returns a [`KeyError`] if the key breaks the size or path limits of
    /// [`Key`]. The hex encoding of an address hash stays within them.
    pub fn entry_key(address: &AddressHash) -> Result<Key, KeyError> {
        let addr_hex = hex::encode(address.to_bytes_vec());
        ROOT.concat(format!("/{addr_hex}"))
    }

    /// Key of the registry counter (the next free index), relative to the
    /// accounts keyspace.
    pub const COUNTER_KEY: Key = Key::from_static(b"/address_registry/counter");
}

pub mod code {
    use super::*;

    const CODE_PATH: RefPath = RefPath::assert_from(b"/data/code");

    const STORAGE_PATH: RefPath = RefPath::assert_from(b"/data/storage");

    /// Classification record (`Origin`) of an originated account, read here to
    /// resolve a code-less alias to the shared implementation. This is the
    /// canonical `/origin` segment: the `TezosOriginatedAccount::origin` /
    /// `set_origin` methods build on it, so the reader and the writer of the
    /// record share a single source of truth.
    pub const ORIGIN_PATH: RefPath = RefPath::assert_from(b"/origin");

    /// Aggregated storage-accounting record: holds [code_size],
    /// [storage_size], [used_bytes] and [paid_bytes] in a single value, so
    /// they can be read and written with one host call. The code and storage
    /// *blobs* (`/data/code`, `/data/storage`) stay separate.
    const INFO_PATH: RefPath = RefPath::assert_from(b"/info");

    pub fn info_path(account: &TezosOriginatedAccount) -> Result<OwnedPath, PathError> {
        originated_path(account, &INFO_PATH)
    }

    pub fn code_path(account: &TezosOriginatedAccount) -> Result<OwnedPath, PathError> {
        originated_path(account, &CODE_PATH)
    }

    pub fn storage_path(
        account: &TezosOriginatedAccount,
    ) -> Result<OwnedPath, PathError> {
        originated_path(account, &STORAGE_PATH)
    }

    pub fn origin_path(account: &TezosOriginatedAccount) -> Result<OwnedPath, PathError> {
        originated_path(account, &ORIGIN_PATH)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mir::ast::{BinWriter, ByteReprTrait};
    use tezos_crypto_rs::blake2b;
    use tezos_evm_runtime::runtime::MockKernelHost;

    /// Returns the durable path that `key` resolves to in the accounts
    /// keyspace, which prepends its name to every key. This function writes
    /// the name in full, and it does not read the name from the keyspace.
    fn durable(key: &Key) -> Vec<u8> {
        [b"/tez/tez_accounts", key.as_bytes()].concat()
    }

    /// Makes sure that every key of the address registry resolves to its
    /// durable path under `/tez/tez_accounts/address_registry`. The expected
    /// paths are literals, not rebuilt from the key builders, so a builder
    /// that moves a key fails the test.
    #[test]
    fn address_registry_keys_keep_their_durable_paths() {
        assert_eq!(
            durable(&address_registry::COUNTER_KEY),
            b"/tez/tez_accounts/address_registry/counter"
        );
        let zero = AddressHash::default();
        assert_eq!(
            durable(&address_registry::entry_key(&zero).unwrap()),
            format!(
                "/tez/tez_accounts/address_registry/{}",
                hex::encode(zero.to_bytes_vec())
            )
            .into_bytes()
        );
    }

    /// Makes sure that the key of the next-ID counter resolves to its durable
    /// path. The test writes the path in full, so it fails if the key changes.
    #[test]
    fn big_map_next_id_key_keeps_its_durable_path() {
        assert_eq!(
            durable(&big_maps::NEXT_ID_KEY),
            b"/tez/tez_accounts/big_map/next_id"
        );
    }

    /// Returns an originated contract, its account and the durable path of the
    /// account.
    fn sample_originated() -> (Contract, TezosOriginatedAccount, String) {
        let contract =
            Contract::from_b58check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT").unwrap();
        let account = originated_from_kt1(match &contract {
            Contract::Originated(kt1) => kt1,
            _ => unreachable!("KT1 parses as originated"),
        })
        .unwrap();
        let index = format!(
            "/tez/tez_accounts/contracts/index/{}",
            hex::encode({
                let mut bytes = Vec::new();
                contract.bin_write(&mut bytes).unwrap();
                bytes
            })
        );
        (contract, account, index)
    }

    /// Makes sure that the key of a contract resolves to
    /// `/tez/tez_accounts/contracts/index/<hex>`, for an originated contract
    /// and for an implicit one.
    #[test]
    fn account_key_keeps_its_durable_path() {
        let (contract, account, index) = sample_originated();
        assert_eq!(
            durable(&contracts::account_key(&contract).unwrap()),
            index.clone().into_bytes()
        );
        assert_eq!(durable(&account.key), index.into_bytes());

        let implicit =
            Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let mut bytes = Vec::new();
        implicit.bin_write(&mut bytes).unwrap();
        assert_eq!(
            durable(&contracts::account_key(&implicit).unwrap()),
            format!("/tez/tez_accounts/contracts/index/{}", hex::encode(bytes))
                .into_bytes()
        );
    }

    #[test]
    fn read_origin_for_address_implicit_is_native_by_construction() {
        let host = MockKernelHost::default();
        let pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let address = AddressHash::Implicit(pkh);

        // An implicit account is Native by construction: classification needs
        // no durable read and stores no `/origin` record.
        assert_eq!(
            read_origin_for_address(&host, &address).unwrap(),
            Some(Origin::Native),
        );
    }

    #[test]
    fn read_origin_for_address_kt1_round_trips_classification() {
        let mut host = MockKernelHost::default();
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(b"kt1-test-seed"));
        let address = AddressHash::Kt1(kt1.clone());

        // Unrecorded: returns None.
        assert!(read_origin_for_address(&host, &address).unwrap().is_none());

        // Native: returns Native.
        originated_from_kt1(&kt1)
            .unwrap()
            .set_origin(&mut host, &Origin::Native)
            .unwrap();
        assert_eq!(
            read_origin_for_address(&host, &address).unwrap(),
            Some(Origin::Native),
        );
    }
}
