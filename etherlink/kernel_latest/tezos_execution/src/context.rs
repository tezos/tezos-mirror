// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::{big_map::BigMapId, AddressHash, PublicKeyHash};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::path::{concat, OwnedPath, Path, PathError, RefPath};
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Origin;

use crate::account_storage::{
    TezlinkImplicitAccount, TezlinkOriginatedAccount, TezosImplicitAccountTrait,
    TezosOriginatedAccount,
};

// TODO: https://gitlab.com/tezos/tezos/-/issues/7867: add the missing paths

// Instead of using directly the paths, we construct a Context object that holds the
// path to the context and does the concatenations.
// This prevents the root path (e.g. '/tez/tez_accounts') from
// appearing at multiple places in the codebase.
pub struct TezlinkContext {
    path: OwnedPath,
}

pub trait Context {
    type ImplicitAccountType: TezosImplicitAccountTrait;

    type OriginatedAccountType: TezosOriginatedAccount;

    fn implicit_from_contract(
        &self,
        contract: &Contract,
    ) -> Result<Self::ImplicitAccountType, tezos_storage::error::Error> {
        match contract {
            Contract::Implicit(pkh) => self.implicit_from_public_key_hash(pkh),
            _ => Err(tezos_storage::error::Error::OriginatedToImplicit),
        }
    }

    fn implicit_from_public_key_hash(
        &self,
        pkh: &PublicKeyHash,
    ) -> Result<Self::ImplicitAccountType, tezos_storage::error::Error>;

    fn originated_from_contract(
        &self,
        contract: &Contract,
    ) -> Result<Self::OriginatedAccountType, tezos_storage::error::Error> {
        match contract {
            Contract::Originated(kt1) => self.originated_from_kt1(kt1),
            _ => Err(tezos_storage::error::Error::ImplicitToOriginated),
        }
    }

    fn originated_from_kt1(
        &self,
        kt1: &ContractKt1Hash,
    ) -> Result<Self::OriginatedAccountType, tezos_storage::error::Error>;

    /// Record the origin classification of an originated account.
    /// Default is a no-op for runtimes without classification storage.
    /// TezosX overrides this to write the given origin at the origin
    /// storage path.
    fn record_origin(
        &self,
        _host: &mut impl StorageV1,
        _kt1: &ContractKt1Hash,
        _origin: &Origin,
    ) -> Result<(), tezos_storage::error::Error> {
        Ok(())
    }

    /// Read the origin classification (native / alias) for the given address.
    /// Default returns None for runtimes without classification storage.
    /// TezosX overrides this to read the origin path.
    fn read_origin_for_address(
        &self,
        _host: &impl StorageV1,
        _address: &AddressHash,
    ) -> Result<Option<Origin>, tezos_storage::error::Error> {
        Ok(None)
    }

    fn path(&self) -> OwnedPath;

    fn from_root(root: &impl Path) -> Result<Self, PathError>
    where
        Self: Sized;
}

impl Context for TezlinkContext {
    type ImplicitAccountType = TezlinkImplicitAccount;

    fn implicit_from_public_key_hash(
        &self,
        pkh: &PublicKeyHash,
    ) -> Result<Self::ImplicitAccountType, tezos_storage::error::Error> {
        let index = contracts::index(self)?;
        let contract = Contract::Implicit(pkh.clone());
        let path = concat(&index, &account::account_path(&contract)?)?;
        Ok(TezlinkImplicitAccount {
            path,
            pkh: pkh.clone(),
        })
    }

    type OriginatedAccountType = TezlinkOriginatedAccount;

    fn originated_from_kt1(
        &self,
        kt1: &ContractKt1Hash,
    ) -> Result<Self::OriginatedAccountType, tezos_storage::error::Error> {
        let index = contracts::index(self)?;
        let contract = Contract::Originated(kt1.clone());
        let path = concat(&index, &account::account_path(&contract)?)?;
        Ok(TezlinkOriginatedAccount {
            path,
            kt1: kt1.clone(),
        })
    }

    fn from_root(root: &impl Path) -> Result<Self, PathError> {
        Ok(Self {
            path: OwnedPath::from(root),
        })
    }

    fn path(&self) -> OwnedPath {
        self.path.clone()
    }
}

impl TezlinkContext {
    #[cfg(test)]
    pub fn init_context() -> Self {
        let path = RefPath::assert_from(b"/tez/tez_accounts");
        Self {
            path: OwnedPath::from(path),
        }
    }
}

pub mod contracts {
    use super::*;

    const ROOT: RefPath = RefPath::assert_from(b"/contracts");

    const INDEX: RefPath = RefPath::assert_from(b"/index");

    const GLOBAL_COUNTER: RefPath = RefPath::assert_from(b"/global_counter");

    pub fn root<C: Context>(context: &C) -> Result<OwnedPath, PathError> {
        concat(&context.path(), &ROOT)
    }

    pub fn index<C: Context>(context: &C) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &INDEX)
    }

    pub fn global_counter<C: Context>(context: &C) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &GLOBAL_COUNTER)
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

    fn root<C: Context>(context: &C) -> Result<OwnedPath, PathError> {
        concat(&context.path(), &BIG_MAP_PATH)
    }

    pub fn next_id_path<C: Context>(context: &C) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &NEXT_ID_PATH)
    }

    pub fn big_map_path<C: Context>(
        context: &C,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &OwnedPath::try_from(format!("/{id}"))?)
    }

    pub fn key_type_path<C: Context>(
        context: &C,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(context, id)?, &KEY_TYPE_PATH)
    }

    pub fn keys_of_big_map<C: Context>(
        context: &C,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(context, id)?, &KEYS)
    }

    pub fn value_type_path<C: Context>(
        context: &C,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(context, id)?, &VALUE_TYPE_PATH)
    }

    pub fn total_bytes_path<C: Context>(
        context: &C,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(context, id)?, &TOTAL_BYTES_PATH)
    }

    pub fn value_path<C: Context>(
        context: &C,
        id: &BigMapId,
        key_hashed: &ScriptExprHash,
    ) -> Result<OwnedPath, PathError> {
        let key_hex = hex::encode(key_hashed);
        concat(
            &big_map_path(context, id)?,
            &OwnedPath::try_from(format!("/{key_hex}"))?,
        )
    }
}

pub mod code {
    use crate::account_storage::TezosOriginatedAccount;

    use super::*;

    const CODE_PATH: RefPath = RefPath::assert_from(b"/data/code");

    const STORAGE_PATH: RefPath = RefPath::assert_from(b"/data/storage");

    /// Classification record (`Origin`) of an originated account, read here to
    /// resolve a code-less alias to the shared implementation. This is the
    /// canonical copy of the `/origin` segment: the Michelson runtime imports it
    /// (`tezosx-tezos-runtime/src/account.rs`) instead of redeclaring it, so the
    /// reader and the writer of the record can never drift apart.
    pub const ORIGIN_PATH: RefPath = RefPath::assert_from(b"/origin");

    /// Aggregated storage-accounting record: holds [code_size],
    /// [storage_size], [used_bytes] and [paid_bytes] in a single value, so
    /// they can be read and written with one host call. The code and storage
    /// *blobs* (`/data/code`, `/data/storage`) stay separate.
    const INFO_PATH: RefPath = RefPath::assert_from(b"/info");

    pub fn info_path<A: TezosOriginatedAccount>(
        account: &A,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &INFO_PATH)
    }

    pub fn code_path<A: TezosOriginatedAccount>(
        account: &A,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &CODE_PATH)
    }

    pub fn storage_path<A: TezosOriginatedAccount>(
        account: &A,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &STORAGE_PATH)
    }

    pub fn origin_path<A: TezosOriginatedAccount>(
        account: &A,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &ORIGIN_PATH)
    }
}

pub mod account {
    use mir::ast::BinWriter;

    use crate::account_storage::TezlinkAccount;

    use super::*;

    const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

    const COUNTER_PATH: RefPath = RefPath::assert_from(b"/counter");

    const MANAGER_PATH: RefPath = RefPath::assert_from(b"/manager");

    pub fn account_path(
        contract: &Contract,
    ) -> Result<OwnedPath, tezos_storage::error::Error> {
        // uses the same encoding as in the octez node's representation of the context
        // see `octez-codec describe alpha.contract binary schema`
        let mut contract_encoded = Vec::new();
        contract
            .bin_write(&mut contract_encoded)
            .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;

        let path_string = alloc::format!("/{}", hex::encode(&contract_encoded));
        Ok(OwnedPath::try_from(path_string)?)
    }

    pub fn balance_path<A: TezlinkAccount + ?Sized>(
        account: &A,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &BALANCE_PATH)
    }

    pub fn counter_path<A: TezlinkAccount + ?Sized>(
        account: &A,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &COUNTER_PATH)
    }

    pub fn manager_path<A: TezlinkAccount + ?Sized>(
        account: &A,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &MANAGER_PATH)
    }
}
