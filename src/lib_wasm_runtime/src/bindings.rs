// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

//! Collections of functions exposed from OCaml and allowing Rust functions to interact with the
//! state manipulated by the kernel.

use crate::types::{ContextHash, EvmTree, OCamlBytes};
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
    use crate::types::{ContextHash, EvmTree, OCamlBytes};

    ocaml::import! {
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
    }
}

fn gc() -> ocaml::Runtime {
    ocaml::Runtime::init()
}

pub fn store_get_hash<K>(evm_tree: &EvmTree, key: K) -> Result<ContextHash, BindingsError>
where
    K: Key,
{
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
    let mem = unsafe {
        ocaml_imports::layer2_store__mem_tree(&gc(), evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    mem.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_has<K>(evm_tree: &EvmTree, key: K) -> Result<isize, BindingsError>
where
    K: Key,
{
    let mem = unsafe {
        ocaml_imports::layer2_store__store_has(&gc(), evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    mem.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_delete<K>(evm_tree: &EvmTree, key: K, is_value: bool) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    let mem = unsafe {
        ocaml_imports::layer2_store__store_delete(&gc(), evm_tree.clone(), key.as_str(), is_value)
            .map_err(BindingsError::OCamlError)?
    };

    mem.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_copy<K>(evm_tree: &EvmTree, from: K, to: K) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    let res = unsafe {
        ocaml_imports::layer2_store__store_copy(&gc(), evm_tree.clone(), from.as_str(), to.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_move<K>(evm_tree: &EvmTree, from: K, to: K) -> Result<EvmTree, BindingsError>
where
    K: Key,
{
    let res = unsafe {
        ocaml_imports::layer2_store__store_copy(&gc(), evm_tree.clone(), from.as_str(), to.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_list_size<K>(evm_tree: &EvmTree, key: K) -> Result<isize, BindingsError>
where
    K: Key,
{
    let res = unsafe {
        ocaml_imports::layer2_store__store_list_size(&gc(), evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
    };

    res.map_err(|i| BindingsError::HostFuncError(i as i32))
}

pub fn store_value_size<K>(evm_tree: &EvmTree, key: K) -> Result<isize, BindingsError>
where
    K: Key,
{
    let res = unsafe {
        ocaml_imports::layer2_store__store_value_size(&gc(), evm_tree.clone(), key.as_str())
            .map_err(BindingsError::OCamlError)?
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
    let res = unsafe {
        ocaml_imports::layer2_store__store_read(
            &gc(),
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
    evm_tree: &EvmTree,
    key: K,
    offset: usize,
    bytes: &[u8],
) -> Result<(EvmTree, isize), BindingsError>
where
    K: Key,
{
    let res = unsafe {
        ocaml_imports::layer2_store__store_write(
            &gc(),
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
    unsafe {
        ocaml_imports::layer2_store__check_reboot_flag(&gc(), evm_tree.clone())
            .map_err(BindingsError::OCamlError)
    }
}
