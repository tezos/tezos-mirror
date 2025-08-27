// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>

//! Collections of functions exposed from OCaml and allowing Rust functions to interact with the
//! state manipulated by the kernel.

use log::trace;

use crate::types::{ContextHash, EvmTree, OCamlBytes, OCamlString, OpenTelemetryScope};
use std::{
    error::Error,
    fmt::{Display, Formatter},
};

pub trait Key {
    fn as_str(&self) -> &str;
}

impl<T> Key for T
where
    T: AsRef<[u8]>,
{
    fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.as_ref()) }
    }
}

#[derive(Debug)]
pub enum BindingsError {
    HostFuncError(i32),
    OCamlError(ocaml::Error),
}

impl Display for BindingsError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::HostFuncError(err) => write!(f, "host func error: {}", err),
            Self::OCamlError(err) => write!(f, "ocaml error: {:?}", err),
        }
    }
}

impl Error for BindingsError {}

#[allow(non_snake_case)]
mod ocaml_imports {
    use crate::types::{ContextHash, EvmTree, OCamlBytes, OCamlString, OpenTelemetryScope};

    ocaml::import! {
        pub fn fetch_preimage_from_remote(preimages_endpoint: &str, hash_hex: &str) -> OCamlString;

        pub fn layer2_store__read_durable_value(evm_tree: EvmTree, key: &str) -> Result<OCamlBytes, isize>;
        pub fn layer2_store__mem_tree(evm_tree: EvmTree, key: &str) -> Result<bool, isize>;
        pub fn layer2_store__check_reboot_flag(evm_tree: EvmTree) -> (bool, EvmTree);

        pub fn layer2_store__store_get_hash(evm_tree: EvmTree, key: &str) -> Result<ContextHash, isize>;
        pub fn layer2_store__store_delete(otel_scope: &OpenTelemetryScope, evm_tree: EvmTree, key: &str, is_value: bool) -> Result<EvmTree, isize>;
        pub fn layer2_store__store_has(otel_scope: &OpenTelemetryScope, evm_tree: EvmTree, key: &str) -> Result<isize, isize>;
        pub fn layer2_store__store_copy(otel_scope: &OpenTelemetryScope, evm_tree: EvmTree, from: &str, to: &str) -> Result<EvmTree, isize>;
        pub fn layer2_store__store_move(otel_scope: &OpenTelemetryScope, evm_tree: EvmTree, from: &str, to: &str) -> Result<EvmTree, isize>;
        pub fn layer2_store__store_list_size(otel_scope: &OpenTelemetryScope, evm_tree: EvmTree, key: &str) -> Result<isize, isize>;
        pub fn layer2_store__store_value_size(otel_scope: &OpenTelemetryScope, evm_tree: EvmTree, key: &str) -> Result<isize, isize>;
        pub fn layer2_store__store_read(otel_scope: &OpenTelemetryScope, evm_tree: EvmTree, key: &str, offset: usize, num_bytes: usize) -> Result<OCamlBytes, isize>;
        pub fn layer2_store__store_write(otel_scope: &OpenTelemetryScope, evm_tree: EvmTree, key: &str, offset: usize, bytes: &[u8]) -> Result<(EvmTree, isize), isize>;
        pub fn layer2_store__store_write_all(otel_scope: &OpenTelemetryScope, evm_tree: EvmTree, key: &str, bytes: &[u8]) -> Result<EvmTree, isize>;

        pub fn open_span(parent_scope: &OpenTelemetryScope, span_name: &str) -> OpenTelemetryScope;
        pub fn close_span(scope: &OpenTelemetryScope) -> ();
    }
}

fn gc() -> ocaml::Runtime {
    ocaml::Runtime::init()
}

pub fn fetch_preimage_from_remote(
    preimages_endpoint: &str,
    hash_hex: &str,
) -> Result<OCamlString, BindingsError> {
    trace!(
        "fetch_preimage_from_remote({}, {})",
        preimages_endpoint,
        hash_hex
    );
    unsafe {
        ocaml_imports::fetch_preimage_from_remote(&gc(), preimages_endpoint, hash_hex)
            .map_err(BindingsError::OCamlError)
    }
}

pub fn store_get_hash<K>(evm_tree: &EvmTree, key: K) -> Result<ContextHash, BindingsError>
where
    K: Key,
{
    trace!("store_get_hash({})", key.as_str());
    let res = unsafe {
        ocaml_imports::layer2_store__store_get_hash(&gc(), evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn read_value<K>(evm_tree: &EvmTree, key: K) -> Result<OCamlBytes, BindingsError>
where
    K: Key,
{
    trace!("read_value({})", key.as_str());
    let code = unsafe {
        ocaml_imports::layer2_store__read_durable_value(&gc(), evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    code.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn mem_tree<K>(evm_tree: &EvmTree, key: K) -> Result<bool, BindingsError>
where
    K: Key,
{
    trace!("mem_tree({})", key.as_str());
    let mem = unsafe {
        ocaml_imports::layer2_store__mem_tree(&gc(), evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    mem.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_has<K>(
    scope: &OpenTelemetryScope,
    evm_tree: &EvmTree,
    key: K,
) -> Result<isize, BindingsError>
where
    K: Key,
{
    trace!("store_has({})", key.as_str());
    let mem = unsafe {
        ocaml_imports::layer2_store__store_has(&gc(), scope, evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    mem.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_delete<K>(
    scope: &OpenTelemetryScope,
    evm_tree: &EvmTree,
    key: K,
    is_value: bool,
) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    trace!("store_delete({}, is_value:{})", key.as_str(), is_value);
    let mem = unsafe {
        ocaml_imports::layer2_store__store_delete(
            &gc(),
            scope,
            evm_tree.clone(),
            key.as_str(),
            is_value,
        )
        .map_err(BindingsError::OCamlError)?
    };

    mem.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_copy<K>(
    scope: &OpenTelemetryScope,
    evm_tree: &EvmTree,
    from: K,
    to: K,
) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    trace!("store_copy({}, {})", from.as_str(), to.as_str());
    let res = unsafe {
        ocaml_imports::layer2_store__store_copy(
            &gc(),
            scope,
            evm_tree.clone(),
            from.as_str(),
            to.as_str(),
        )
        .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_move<K>(
    scope: &OpenTelemetryScope,
    evm_tree: &EvmTree,
    from: K,
    to: K,
) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    trace!("store_move({}, {})", from.as_str(), to.as_str());
    let res = unsafe {
        ocaml_imports::layer2_store__store_move(
            &gc(),
            scope,
            evm_tree.clone(),
            from.as_str(),
            to.as_str(),
        )
        .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_list_size<K>(
    scope: &OpenTelemetryScope,
    evm_tree: &EvmTree,
    key: K,
) -> Result<isize, BindingsError>
where
    K: Key,
{
    trace!("store_list_size({})", key.as_str());
    let res = unsafe {
        ocaml_imports::layer2_store__store_list_size(&gc(), scope, evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_value_size<K>(
    scope: &OpenTelemetryScope,
    evm_tree: &EvmTree,
    key: K,
) -> Result<isize, BindingsError>
where
    K: Key,
{
    trace!("store_value_size({})", key.as_str());
    let res = unsafe {
        ocaml_imports::layer2_store__store_value_size(&gc(), scope, evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_read<K>(
    scope: &OpenTelemetryScope,
    evm_tree: &EvmTree,
    key: K,
    offset: usize,
    num_bytes: usize,
) -> Result<OCamlBytes, BindingsError>
where
    K: Key,
{
    trace!(
        "store_read({}, offset:{}, num_bytes:{})",
        key.as_str(),
        offset,
        num_bytes
    );
    let res = unsafe {
        ocaml_imports::layer2_store__store_read(
            &gc(),
            scope,
            evm_tree.clone(),
            key.as_str(),
            offset,
            num_bytes,
        )
        .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_write<K>(
    scope: &OpenTelemetryScope,
    evm_tree: &EvmTree,
    key: K,
    offset: usize,
    bytes: &[u8],
) -> Result<(EvmTree, isize), BindingsError>
where
    K: Key,
{
    trace!("store_write({}, offset:{})", key.as_str(), offset);
    let res = unsafe {
        ocaml_imports::layer2_store__store_write(
            &gc(),
            scope,
            evm_tree.clone(),
            key.as_str(),
            offset,
            bytes,
        )
        .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn check_reboot_flag(evm_tree: &EvmTree) -> Result<(bool, EvmTree), BindingsError> {
    trace!("check_reboot_flag()");
    unsafe {
        ocaml_imports::layer2_store__check_reboot_flag(&gc(), evm_tree.clone())
            .map_err(BindingsError::OCamlError)
    }
}

pub fn store_write_all<K>(
    scope: &OpenTelemetryScope,
    evm_tree: &EvmTree,
    key: K,
    bytes: &[u8],
) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    trace!("store_write_all({})", key.as_str());
    let res = unsafe {
        ocaml_imports::layer2_store__store_write_all(
            &gc(),
            scope,
            evm_tree.clone(),
            key.as_str(),
            bytes,
        )
        .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn open_span(
    s: &OpenTelemetryScope,
    top_span_name: &str,
) -> Result<OpenTelemetryScope, BindingsError> {
    let s = unsafe {
        ocaml_imports::open_span(&gc(), s, top_span_name).map_err(BindingsError::OCamlError)?
    };

    Ok(s)
}

pub fn close_span(s: &OpenTelemetryScope) -> Result<(), BindingsError> {
    unsafe {
        ocaml_imports::close_span(&gc(), s).map_err(BindingsError::OCamlError)?;
    }

    Ok(())
}
