// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::big_map::BigMapId;
use tezos_smart_rollup_host::path::{concat, OwnedPath, Path, PathError, RefPath};

// TODO: https://gitlab.com/tezos/tezos/-/issues/7867: add the missing paths

// Instead of using directly the paths, we construct a Context object that holds the
// path to the context and does the concatenations.
// This will prevent '/tezlink/context' to appear at multiple place like '/evm/world_state'
pub struct Context {
    path: OwnedPath,
}

impl Context {
    pub fn from(root: &impl Path) -> Result<Self, PathError> {
        let context = RefPath::assert_from(b"/context");
        let path = concat(root, &context)?;
        Ok(Self { path })
    }

    pub fn path(&self) -> OwnedPath {
        self.path.clone()
    }

    #[cfg(test)]
    pub fn init_context() -> Self {
        let path = RefPath::assert_from(b"/tezlink/context");
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

    pub fn root(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&context.path, &ROOT)
    }

    pub fn index(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &INDEX)
    }

    pub fn global_counter(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &GLOBAL_COUNTER)
    }
}

pub mod big_maps {
    use super::*;

    const BIG_MAP_PATH: RefPath = RefPath::assert_from(b"/big_map");

    const KEY_TYPE_PATH: RefPath = RefPath::assert_from(b"/key_type");

    const VALUE_TYPE_PATH: RefPath = RefPath::assert_from(b"/value_type");

    const NEXT_ID_PATH: RefPath = RefPath::assert_from(b"/next_id");

    const KEYS: RefPath = RefPath::assert_from(b"/keys");

    fn root(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&context.path, &BIG_MAP_PATH)
    }

    pub fn next_id_path(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &NEXT_ID_PATH)
    }

    pub fn big_map_path(
        context: &Context,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &OwnedPath::try_from(format!("/{id}"))?)
    }

    pub fn key_type_path(
        context: &Context,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(context, id)?, &KEY_TYPE_PATH)
    }

    pub fn keys_of_big_map(
        context: &Context,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(context, id)?, &KEYS)
    }

    pub fn value_type_path(
        context: &Context,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(context, id)?, &VALUE_TYPE_PATH)
    }

    pub fn value_path(
        context: &Context,
        id: &BigMapId,
        key_hashed: &[u8],
    ) -> Result<OwnedPath, PathError> {
        let key_hex = hex::encode(key_hashed);
        concat(
            &big_map_path(context, id)?,
            &OwnedPath::try_from(format!("/{key_hex}"))?,
        )
    }
}

pub mod code {
    use crate::account_storage::{TezlinkAccount, TezlinkOriginatedAccount};

    use super::*;

    const CODE_PATH: RefPath = RefPath::assert_from(b"/data/code");

    const STORAGE_PATH: RefPath = RefPath::assert_from(b"/data/storage");

    const CODE_SIZE_PATH: RefPath = RefPath::assert_from(b"/len/code");

    const STORAGE_SIZE_PATH: RefPath = RefPath::assert_from(b"/len/storage");

    const PAID_BYTES_PATH: RefPath = RefPath::assert_from(b"/paid_bytes");

    const USED_BYTES_PATH: RefPath = RefPath::assert_from(b"/used_bytes");

    pub fn code_path(account: &TezlinkOriginatedAccount) -> Result<OwnedPath, PathError> {
        concat(account.path(), &CODE_PATH)
    }

    pub fn storage_path(
        account: &TezlinkOriginatedAccount,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &STORAGE_PATH)
    }

    pub fn code_size_path(
        account: &TezlinkOriginatedAccount,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &CODE_SIZE_PATH)
    }

    pub fn storage_size_path(
        account: &TezlinkOriginatedAccount,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &STORAGE_SIZE_PATH)
    }

    pub fn paid_bytes_path(
        account: &TezlinkOriginatedAccount,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &PAID_BYTES_PATH)
    }

    pub fn used_bytes_path(
        account: &TezlinkOriginatedAccount,
    ) -> Result<OwnedPath, PathError> {
        concat(account.path(), &USED_BYTES_PATH)
    }
}

pub mod account {
    use crate::account_storage::TezlinkAccount;

    use super::*;

    const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

    const COUNTER_PATH: RefPath = RefPath::assert_from(b"/counter");

    const MANAGER_PATH: RefPath = RefPath::assert_from(b"/manager");

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
