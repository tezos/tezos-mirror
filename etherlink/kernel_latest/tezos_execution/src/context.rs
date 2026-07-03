// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Account addressing, construction, and origin classification helpers.

use crate::account_storage::{
    path_to_tezos_account, TezosAccount, TezosImplicitAccount, TezosOriginatedAccount,
};
use mir::ast::{big_map::BigMapId, AddressHash};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_smart_rollup_host::path::{concat, OwnedPath, PathError, RefPath};
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Origin;

/// SafeStorage root for the Michelson account state. Re-exported as
/// `chains::TEZOS_ACCOUNTS_ROOT`.
pub const TEZOS_ACCOUNTS_ROOT: RefPath = RefPath::assert_from(b"/tez/tez_accounts");

// Account resolution helpers.

/// Resolve the implicit (`tz1`/`tz2`/`tz3`) account for a public key hash.
pub fn implicit_from_public_key_hash(
    pkh: &PublicKeyHash,
) -> Result<TezosImplicitAccount, tezos_storage::error::Error> {
    let path = path_to_tezos_account(pkh)
        .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
    Ok(TezosImplicitAccount {
        path,
        pkh: pkh.clone(),
    })
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
    let index = contracts::index()?;
    let contract = Contract::Originated(kt1.clone());
    let path = concat(&index, &contracts::account_path(&contract)?)?;
    Ok(TezosOriginatedAccount {
        path,
        kt1: kt1.clone(),
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

pub mod contracts {
    use mir::ast::BinWriter;

    use super::*;

    const ROOT: RefPath = RefPath::assert_from(b"/contracts");

    const INDEX: RefPath = RefPath::assert_from(b"/index");

    const GLOBAL_COUNTER: RefPath = RefPath::assert_from(b"/global_counter");

    const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

    pub fn root() -> Result<OwnedPath, PathError> {
        concat(&TEZOS_ACCOUNTS_ROOT, &ROOT)
    }

    pub fn index() -> Result<OwnedPath, PathError> {
        concat(&root()?, &INDEX)
    }

    pub fn global_counter() -> Result<OwnedPath, PathError> {
        concat(&root()?, &GLOBAL_COUNTER)
    }

    /// Path segment identifying a contract under [`index`], using the same
    /// encoding as the octez node's context (see
    /// `octez-codec describe alpha.contract binary schema`).
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

    /// Path of an originated account's mutez balance. (Implicit accounts keep
    /// their balance in the RLP `/info` record, so only originated accounts
    /// use this path.)
    pub fn balance_path(
        account: &TezosOriginatedAccount,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &BALANCE_PATH)
    }
}

pub mod big_maps {
    use tezos_crypto_rs::hash::ScriptExprHash;

    use super::*;

    const BIG_MAP_PATH: RefPath = RefPath::assert_from(b"/big_map");

    const KEY_TYPE_PATH: RefPath = RefPath::assert_from(b"/key_type");

    const VALUE_TYPE_PATH: RefPath = RefPath::assert_from(b"/value_type");

    const NEXT_ID_PATH: RefPath = RefPath::assert_from(b"/next_id");

    const KEYS: RefPath = RefPath::assert_from(b"/keys");

    const TOTAL_BYTES_PATH: RefPath = RefPath::assert_from(b"/total_bytes");

    fn root() -> Result<OwnedPath, PathError> {
        concat(&TEZOS_ACCOUNTS_ROOT, &BIG_MAP_PATH)
    }

    pub fn next_id_path() -> Result<OwnedPath, PathError> {
        concat(&root()?, &NEXT_ID_PATH)
    }

    pub fn big_map_path(id: &BigMapId) -> Result<OwnedPath, PathError> {
        concat(&root()?, &OwnedPath::try_from(format!("/{id}"))?)
    }

    pub fn key_type_path(id: &BigMapId) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(id)?, &KEY_TYPE_PATH)
    }

    pub fn keys_of_big_map(id: &BigMapId) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(id)?, &KEYS)
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
        concat(account.path(), &INFO_PATH)
    }

    pub fn code_path(account: &TezosOriginatedAccount) -> Result<OwnedPath, PathError> {
        concat(account.path(), &CODE_PATH)
    }

    pub fn storage_path(
        account: &TezosOriginatedAccount,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &STORAGE_PATH)
    }

    pub fn origin_path(account: &TezosOriginatedAccount) -> Result<OwnedPath, PathError> {
        concat(account.path(), &ORIGIN_PATH)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_crypto_rs::blake2b;
    use tezos_evm_runtime::runtime::MockKernelHost;

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
