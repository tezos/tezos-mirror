// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use mir::{
    ast::{big_map::BigMapId, IntoMicheline, TypedValue},
    parser::Parser,
};
use tezos_crypto_rs::blake2b::digest_256;
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

    pub fn value_type_path(
        context: &Context,
        id: &BigMapId,
    ) -> Result<OwnedPath, PathError> {
        concat(&big_map_path(context, id)?, &VALUE_TYPE_PATH)
    }

    pub fn value_path(
        context: &Context,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<OwnedPath, PathError> {
        let parser = Parser::new();
        let key_encoded = key
            .clone()
            .into_micheline_optimized_legacy(&parser.arena)
            .encode();
        let key_hashed = digest_256(&key_encoded);
        let key_hex = hex::encode(key_hashed);
        concat(
            &big_map_path(context, id)?,
            &OwnedPath::try_from(format!("/{key_hex}"))?,
        )
    }
}
