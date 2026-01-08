// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>

//! Collections of functions exposed from OCaml and allowing Rust functions to interact with the
//! state manipulated by the kernel.

use log::trace;

use crate::types::{
    ContextHash, EvmTree, OCamlBytes, OCamlPairList, OCamlString, OTelAttrValue, OpenTelemetryScope,
};
use std::{
    error::Error,
    fmt::{Display, Formatter},
};

#[macro_export]
macro_rules! otel_trace {
    ($name:expr, $attrs:expr, $expr:expr) => {{
        if crate::api::trace_host_funs_enabled() {
            crate::bindings::start_span($name);
            crate::bindings::span_add_attrs($attrs);
            let __otel_result = { $expr };
            crate::bindings::end_span();
            __otel_result
        } else {
            $expr
        }
    }};
    ($name:expr, $expr:expr) => {{
        if crate::api::trace_host_funs_enabled() {
            crate::bindings::start_span($name);
            let __otel_result = { $expr };
            crate::bindings::end_span();
            __otel_result
        } else {
            $expr
        }
    }};
}

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
        pub fn layer2_store__store_delete(evm_tree: EvmTree, key: &str, is_value: bool) -> Result<EvmTree, isize>;
        pub fn layer2_store__store_has(evm_tree: EvmTree, key: &str) -> Result<isize, isize>;
        pub fn layer2_store__store_copy(evm_tree: EvmTree, from: &str, to: &str) -> Result<EvmTree, isize>;
        pub fn layer2_store__store_move(evm_tree: EvmTree, from: &str, to: &str) -> Result<EvmTree, isize>;
        pub fn layer2_store__store_list_size(evm_tree: EvmTree, key: &str) -> Result<isize, isize>;
        pub fn layer2_store__store_value_size(evm_tree: EvmTree, key: &str) -> Result<isize, isize>;
        pub fn layer2_store__store_read(evm_tree: EvmTree, key: &str, offset: usize, num_bytes: usize) -> Result<OCamlBytes, isize>;
        pub fn layer2_store__store_write(evm_tree: EvmTree, key: &str, offset: usize, bytes: &[u8]) -> Result<(EvmTree, isize), isize>;
        pub fn layer2_store__store_write_all( evm_tree: EvmTree, key: &str, bytes: &[u8]) -> Result<EvmTree, isize>;

        pub fn init_spans(scope: &OpenTelemetryScope, span_name: &str);
        pub fn start_span(span_name: &str);
        pub fn span_add_attrs(attrs: ocaml::List<ocaml::Value>);
        pub fn end_span();
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
        otel_trace!(
            "reveal_preimage",
            ocaml_imports::fetch_preimage_from_remote(&gc(), preimages_endpoint, hash_hex)
                .map_err(BindingsError::OCamlError)
        )
    }
}

pub fn store_get_hash<K>(evm_tree: &EvmTree, key: K) -> Result<ContextHash, BindingsError>
where
    K: Key,
{
    trace!("store_get_hash({})", key.as_str());
    let res = unsafe {
        otel_trace!(
            "__internal_store_get_hash",
            ocaml_imports::layer2_store__store_get_hash(&gc(), evm_tree.clone(), key.as_str())
                .map_err(BindingsError::OCamlError)?
        )
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn read_value<K>(evm_tree: &EvmTree, key: K) -> Result<OCamlBytes, BindingsError>
where
    K: Key,
{
    trace!("read_value({})", key.as_str());
    let code = unsafe {
        otel_trace!(
            "store_read_all",
            ocaml_imports::layer2_store__read_durable_value(&gc(), evm_tree.clone(), key.as_str())
                .map_err(BindingsError::OCamlError)?
        )
    };

    code.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn mem_tree<K>(evm_tree: &EvmTree, key: K) -> Result<bool, BindingsError>
where
    K: Key,
{
    trace!("mem_tree({})", key.as_str());
    let mem = unsafe {
        otel_trace!(
            "store_exists",
            ocaml_imports::layer2_store__mem_tree(&gc(), evm_tree.clone(), key.as_str())
                .map_err(BindingsError::OCamlError)?
        )
    };

    mem.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_has<K>(evm_tree: &EvmTree, key: K) -> Result<isize, BindingsError>
where
    K: Key,
{
    trace!("store_has({})", key.as_str());
    let mem = unsafe {
        otel_trace!(
            "store_has",
            ocaml_imports::layer2_store__store_has(&gc(), evm_tree.clone(), key.as_str())
                .map_err(BindingsError::OCamlError)?
        )
    };

    mem.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_delete<K>(evm_tree: &EvmTree, key: K, is_value: bool) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    trace!("store_delete({}, is_value:{})", key.as_str(), is_value);

    let mem = unsafe {
        otel_trace!(
            if is_value {
                "store_delete_value"
            } else {
                "store_delete"
            },
            ocaml_imports::layer2_store__store_delete(
                &gc(),
                evm_tree.clone(),
                key.as_str(),
                is_value
            )
            .map_err(BindingsError::OCamlError)?
        )
    };

    mem.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_copy<K>(evm_tree: &EvmTree, from: K, to: K) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    trace!("store_copy({}, {})", from.as_str(), to.as_str());
    let res = unsafe {
        otel_trace!(
            "store_copy",
            ocaml_imports::layer2_store__store_copy(
                &gc(),
                evm_tree.clone(),
                from.as_str(),
                to.as_str()
            )
            .map_err(BindingsError::OCamlError)?
        )
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_move<K>(evm_tree: &EvmTree, from: K, to: K) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    trace!("store_move({}, {})", from.as_str(), to.as_str());
    let res = unsafe {
        otel_trace!(
            "store_move",
            ocaml_imports::layer2_store__store_move(
                &gc(),
                evm_tree.clone(),
                from.as_str(),
                to.as_str()
            )
            .map_err(BindingsError::OCamlError)?
        )
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_list_size<K>(evm_tree: &EvmTree, key: K) -> Result<isize, BindingsError>
where
    K: Key,
{
    trace!("store_list_size({})", key.as_str());
    let res = unsafe {
        otel_trace!(
            "store_count_subkeys",
            ocaml_imports::layer2_store__store_list_size(&gc(), evm_tree.clone(), key.as_str())
                .map_err(BindingsError::OCamlError)?
        )
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_value_size<K>(evm_tree: &EvmTree, key: K) -> Result<isize, BindingsError>
where
    K: Key,
{
    trace!("store_value_size({})", key.as_str());
    let res = unsafe {
        otel_trace!(
            "store_value_size",
            ocaml_imports::layer2_store__store_value_size(&gc(), evm_tree.clone(), key.as_str())
                .map_err(BindingsError::OCamlError)?
        )
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_read<K>(
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
        otel_trace!(
            "store_read",
            ocaml_imports::layer2_store__store_read(
                &gc(),
                evm_tree.clone(),
                key.as_str(),
                offset,
                num_bytes,
            )
            .map_err(BindingsError::OCamlError)?
        )
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_write<K>(
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
        otel_trace!(
            "store_write",
            ocaml_imports::layer2_store__store_write(
                &gc(),
                evm_tree.clone(),
                key.as_str(),
                offset,
                bytes,
            )
            .map_err(BindingsError::OCamlError)?
        )
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
    evm_tree: &EvmTree,
    key: K,
    bytes: &[u8],
) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    trace!("store_write_all({})", key.as_str());
    let res = unsafe {
        otel_trace!(
            "store_write_all",
            ocaml_imports::layer2_store__store_write_all(
                &gc(),
                evm_tree.clone(),
                key.as_str(),
                bytes
            )
            .map_err(BindingsError::OCamlError)?
        )
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn init_spans(s: &OpenTelemetryScope, span_name: &str) -> Result<(), BindingsError> {
    unsafe { ocaml_imports::init_spans(&gc(), s, span_name).map_err(BindingsError::OCamlError)? };

    Ok(())
}

pub fn start_span(span_name: &str) -> Result<(), BindingsError> {
    unsafe { ocaml_imports::start_span(&gc(), span_name).map_err(BindingsError::OCamlError)? };

    Ok(())
}

pub fn span_add_attrs(attrs: Vec<(String, OTelAttrValue)>) -> Result<(), BindingsError> {
    unsafe {
        let ocaml_value_attrs = OCamlPairList::new(attrs).to_value(&gc());
        ocaml_imports::span_add_attrs(&gc(), ocaml_value_attrs)
            .map_err(BindingsError::OCamlError)?
    };

    Ok(())
}

pub fn end_span() -> Result<(), BindingsError> {
    unsafe { ocaml_imports::end_span(&gc()).map_err(BindingsError::OCamlError)? };

    Ok(())
}
