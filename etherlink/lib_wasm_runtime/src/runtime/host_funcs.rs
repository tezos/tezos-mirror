// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>

//! Implementations of the host functions defined by the PVM, and used by the kernel when executed
//! with Wasmer.

use std::{fs, path::PathBuf};

use crate::{
    bindings::{self, end_span, start_span, BindingsError, Key},
    otel_trace,
    reveal::{self, RevealPreimageError},
};

use super::env::Env;
use log::trace;
use tezos_crypto_rs::blake2b;
use wasmer::{
    imports, AsStoreRef, Function, FunctionEnv, FunctionEnvMut, Imports, MemoryView, RuntimeError,
    Store,
};

#[allow(dead_code)]
mod error_code {
    pub const STORE_KEY_TOO_LARGE: i32 = -1;
    pub const STORE_INVALID_KEY: i32 = -2;
    pub const STORE_NOT_A_VALUE: i32 = -3;
    pub const STORE_INVALID_ACCESS: i32 = -4;
    pub const STORE_VALUE_SIZE_EXCEEDED: i32 = -5;
    pub const MEMORY_INVALID_ACCESS: i32 = -6;
    pub const INPUT_OUTPUT_TOO_LARGE: i32 = -7;
    pub const GENERIC_INVALID_ACCESS: i32 = -8;
    pub const STORE_READONLY_VALUE: i32 = -9;
    pub const STORE_NOT_A_NODE: i32 = -10;
    pub const FULL_OUTBOX: i32 = -11;
    pub const STORE_INVALID_SUBKEY_INDEX: i32 = -12;
    pub const STORE_VALUE_ALREADY_EXISTS: i32 = -13;
}

const INPUT_OUTPUT_MAX_SIZE: u32 = 4096;

fn result_from_binding_error(err: BindingsError) -> Result<i32, RuntimeError> {
    match err {
        BindingsError::HostFuncError(i) => Ok(i),
        BindingsError::OCamlError(ocaml::Error::Caml(ocaml::CamlError::Exception(exn))) => {
            let exn = unsafe { exn.exception_to_string().unwrap() };
            Err(RuntimeError::new(format!("exception: {}", exn)))
        }
        BindingsError::OCamlError(err) => Err(RuntimeError::new(format!(
            "unexpected internal error: {:?}",
            err
        ))),
    }
}

fn i64_result_from_binding_error(err: BindingsError) -> Result<i64, RuntimeError> {
    match err {
        BindingsError::HostFuncError(i) => Ok(i as i64),
        BindingsError::OCamlError(_) => Err(RuntimeError::new("unexpected error")),
    }
}

fn read_from_memory(memory_view: &MemoryView, ptr: u32, len: u32) -> Result<Vec<u8>, RuntimeError> {
    // TODO: Can we get a slice from `read_from_memory`?
    let mut buffer = vec![0u8; len as usize];
    memory_view.read(ptr.into(), &mut buffer)?;

    Ok(buffer)
}

fn write_debug(env: FunctionEnvMut<Env>, ptr: u32, length: u32) -> Result<(), RuntimeError> {
    let store = env.as_store_ref();
    let runtime_env = env.data();
    let memory = runtime_env.memory();

    let buffer = read_from_memory(&memory.view(&store), ptr, length)?;

    runtime_env.host().write_debug(&buffer);

    Ok(())
}

fn write_output(_env: FunctionEnvMut<Env>, _ptr: u32, length: u32) -> i32 {
    // The EVM node has little reason to emulate the `write_output` host function, because it is
    // only useful as part of the native bridge (i.e., as part of a published state). We do have to
    // perform the sanity checks to deal with errors, though.
    //
    // We assume the kernel is implemented in safe Rust, and that memory safety is enforced by the
    // compiler. This only leaves us the limit on the size of the input.
    if length > INPUT_OUTPUT_MAX_SIZE {
        return error_code::INPUT_OUTPUT_TOO_LARGE;
    }

    return 0;
}

fn read_input(mut env: FunctionEnvMut<Env>, info_addr: u32, dst: u32, max_bytes: u32) -> i32 {
    trace!("read_input");
    let (runtime_env, store) = env.data_and_store_mut();

    match runtime_env.mut_host().next_input() {
        Some(input) => {
            let memory = runtime_env.memory();
            let to_write = std::cmp::min(max_bytes as usize, input.payload.len());
            let memory_view = memory.view(&store);

            // No need to do sanity check if we assume Rust is a memory-safe language.
            memory_view
                .write(dst as u64, &input.payload[..to_write])
                .expect("memory bounds were checked");
            memory_view
                .write(info_addr as u64, &input.level.to_le_bytes())
                .expect("memory bounds were checked");
            memory_view
                .write(info_addr as u64 + 4, &input.index.to_le_bytes())
                .expect("memory bounds were checked");

            to_write as i32
        }
        None => 0,
    }
}

fn store_exists(env: FunctionEnvMut<Env>, key_ptr: u32, key_len: u32) -> Result<i32, RuntimeError> {
    let store = env.as_store_ref();
    let runtime_env = env.data();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let key = read_from_memory(&memory_view, key_ptr, key_len)?;
    trace!("store_exists({})", key.as_str());

    match bindings::mem_tree(&runtime_env.host().tree(), key) {
        Ok(true) => Ok(1),
        Ok(false) => Ok(0),
        Err(err) => result_from_binding_error(err),
    }
}

fn store_has(env: FunctionEnvMut<Env>, key_ptr: u32, key_len: u32) -> Result<i32, RuntimeError> {
    let store = env.as_store_ref();
    let runtime_env = env.data();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let key = read_from_memory(&memory_view, key_ptr, key_len)?;
    trace!("store_has({})", key.as_str());

    let host = runtime_env.host();
    match bindings::store_has(&host.tree(), key) {
        Ok(i) => Ok(i as i32),
        Err(err) => result_from_binding_error(err),
    }
}

fn store_delete(
    mut env: FunctionEnvMut<Env>,
    key_ptr: u32,
    key_len: u32,
) -> Result<i32, RuntimeError> {
    let (runtime_env, store) = env.data_and_store_mut();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let key = read_from_memory(&memory_view, key_ptr, key_len)?;
    trace!("store_delete({})", key.as_str());

    let tree = &runtime_env.host().tree().clone();
    let host = runtime_env.host();
    match bindings::store_delete(&tree, key, false) {
        Ok(evm_tree) => {
            runtime_env.mut_host().set_tree(evm_tree);
            Ok(0)
        }
        Err(err) => result_from_binding_error(err),
    }
}

fn store_delete_value(
    mut env: FunctionEnvMut<Env>,
    key_ptr: u32,
    key_len: u32,
) -> Result<i32, RuntimeError> {
    let (runtime_env, store) = env.data_and_store_mut();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let key = read_from_memory(&memory_view, key_ptr, key_len)?;
    trace!("store_delete_value({})", key.as_str());

    let tree = &runtime_env.host().tree().clone();
    let host = runtime_env.host();
    match bindings::store_delete(&tree, key, true) {
        Ok(evm_tree) => {
            runtime_env.mut_host().set_tree(evm_tree);
            Ok(0)
        }
        Err(err) => result_from_binding_error(err),
    }
}

fn store_copy(
    mut env: FunctionEnvMut<Env>,
    from_ptr: u32,
    from_len: u32,
    to_ptr: u32,
    to_len: u32,
) -> Result<i32, RuntimeError> {
    let (runtime_env, store) = env.data_and_store_mut();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let from = read_from_memory(&memory_view, from_ptr, from_len)?;
    let to = read_from_memory(&memory_view, to_ptr, to_len)?;
    trace!("store_copy(from: {}, to: {})", from.as_str(), to.as_str());

    let tree = &runtime_env.host().tree().clone();
    let host = runtime_env.host();
    match bindings::store_copy(&tree, from, to) {
        Ok(evm_tree) => {
            runtime_env.mut_host().set_tree(evm_tree);
            Ok(0)
        }
        Err(err) => result_from_binding_error(err),
    }
}

fn store_move(
    mut env: FunctionEnvMut<Env>,
    from_ptr: u32,
    from_len: u32,
    to_ptr: u32,
    to_len: u32,
) -> Result<i32, RuntimeError> {
    let (runtime_env, store) = env.data_and_store_mut();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let from = read_from_memory(&memory_view, from_ptr, from_len)?;
    let to = read_from_memory(&memory_view, to_ptr, to_len)?;
    trace!("store_move(from: {}, to: {})", from.as_str(), to.as_str());

    let tree = &runtime_env.host().tree().clone();
    let host = runtime_env.host();
    match bindings::store_move(&tree, from, to) {
        Ok(evm_tree) => {
            runtime_env.mut_host().set_tree(evm_tree);
            Ok(0)
        }
        Err(err) => result_from_binding_error(err),
    }
}

fn store_get_hash(
    env: FunctionEnvMut<Env>,
    key_ptr: u32,
    key_len: u32,
    dst_ptr: u32,
    max_size: u32,
) -> Result<i32, RuntimeError> {
    let store = env.as_store_ref();
    let runtime_env = env.data();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let key = read_from_memory(&memory_view, key_ptr, key_len)?;
    trace!("store_get_hash({})", key.as_str());

    let host = runtime_env.host();
    match bindings::store_get_hash(&host.tree(), &key) {
        Ok(hash) => {
            let hash_bytes = hash.as_bytes();
            let to_write = std::cmp::min(hash_bytes.len(), max_size as usize);
            memory_view.write(dst_ptr as u64, &hash_bytes[..to_write])?;
            Ok(to_write as i32)
        }
        Err(err) => result_from_binding_error(err),
    }
}

fn store_list_size(
    env: FunctionEnvMut<Env>,
    key_ptr: u32,
    key_len: u32,
) -> Result<i64, RuntimeError> {
    let store = env.as_store_ref();
    let runtime_env = env.data();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let key = read_from_memory(&memory_view, key_ptr, key_len)?;
    trace!("store_list_size({})", key.as_str());

    let host = runtime_env.host();
    match bindings::store_list_size(&host.tree(), key) {
        Ok(res) => Ok(res as i64),
        Err(err) => i64_result_from_binding_error(err),
    }
}

fn store_value_size(
    env: FunctionEnvMut<Env>,
    key_ptr: u32,
    key_len: u32,
) -> Result<i32, RuntimeError> {
    let store = env.as_store_ref();
    let runtime_env = env.data();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let key = read_from_memory(&memory_view, key_ptr, key_len)?;
    trace!("store_value_size({})", key.as_str());

    let host = runtime_env.host();
    match bindings::store_value_size(&host.tree(), key) {
        Ok(res) => Ok(res as i32),
        Err(err) => result_from_binding_error(err),
    }
}

fn store_read(
    mut env: FunctionEnvMut<Env>,
    key_ptr: u32,
    key_len: u32,
    offset: u32,
    dst_ptr: u32,
    max_bytes: u32,
) -> Result<i32, RuntimeError> {
    let (runtime_env, store) = env.data_and_store_mut();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let key = read_from_memory(&memory_view, key_ptr, key_len)?;
    trace!("store_read({})", key.as_str());

    let tree = &runtime_env.host().tree().clone();
    let host = runtime_env.host();
    match bindings::store_read(&host.tree(), key, offset as usize, max_bytes as usize) {
        Ok(buffer) => {
            memory_view.write(dst_ptr as u64, buffer.as_bytes())?;
            Ok(max_bytes as i32)
        }
        Err(err) => result_from_binding_error(err),
    }
}

fn store_write(
    mut env: FunctionEnvMut<Env>,
    key_ptr: u32,
    key_len: u32,
    offset: u32,
    src_ptr: u32,
    src_len: u32,
) -> Result<i32, RuntimeError> {
    let (runtime_env, store) = env.data_and_store_mut();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let key = read_from_memory(&memory_view, key_ptr, key_len)?;
    trace!("store_write({})", key.as_str());
    let buffer = read_from_memory(&memory_view, src_ptr, src_len)?;

    let tree = &runtime_env.host().tree().clone();
    let host = runtime_env.host();
    match bindings::store_write(&tree, key, offset as usize, &buffer) {
        Ok((evm_tree, res)) => {
            runtime_env.mut_host().set_tree(evm_tree);
            Ok(res as i32)
        }
        Err(err) => result_from_binding_error(err),
    }
}

fn reveal_metadata_internal(
    mut env: FunctionEnvMut<Env>,
    dst_ptr: u32,
    max_bytes: u32,
) -> Result<i32, RuntimeError> {
    trace!("reveal_metadata");
    let (runtime_env, store) = env.data_and_store_mut();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    // The metadata result consists in (1) the rollup address, and (2) the origination level.
    // Etherlink kernel has no use for the origination level, so we default to the same behavior as
    // the WASM Debugger (that is, `origination_level = 0l`).

    let address = runtime_env.host().rollup_address();
    let metadata_size = address.as_bytes().len() + core::mem::size_of::<i32>();
    let to_write = std::cmp::min(metadata_size, max_bytes as usize);
    let address_to_write = std::cmp::min(address.as_bytes().len(), max_bytes as usize);
    let level_to_write = to_write - address_to_write;

    memory_view.write(dst_ptr as u64, &address.as_bytes()[..address_to_write])?;

    if level_to_write > 0 {
        memory_view.write(
            dst_ptr as u64 + address.as_bytes().len() as u64,
            &vec![0u8; level_to_write],
        )?;
    }

    Ok(to_write as i32)
}

fn reveal_metadata(
    mut env: FunctionEnvMut<Env>,
    dst_ptr: u32,
    max_bytes: u32,
) -> Result<i32, RuntimeError> {
    otel_trace!(
        "reveal_metadata",
        reveal_metadata_internal(env, dst_ptr, max_bytes)
    )
}

fn reveal_preimage(
    mut env: FunctionEnvMut<Env>,
    hash_ptr: u32,
    hash_len: u32,
    dst_ptr: u32,
    max_bytes: u32,
) -> Result<i32, RuntimeError> {
    let (runtime_env, store) = env.data_and_store_mut();
    let memory = runtime_env.memory();
    let memory_view = memory.view(&store);

    let hash_bytes = read_from_memory(&memory_view, hash_ptr, hash_len)?;

    trace!("reveal_preimage({})", hex::encode(&hash_bytes));

    let res = reveal::preimage(
        &hash_bytes,
        runtime_env.host().preimages_dir(),
        runtime_env.host().preimages_endpoint(),
    )
    .map_err(|err| {
        RuntimeError::new(format!(
            "Cannot reveal preimage of {}: {}",
            hex::encode(&hash_bytes),
            err
        ))
    })?;

    let to_write = std::cmp::min(res.len(), max_bytes as usize);
    memory_view.write(dst_ptr as u64, &res[..to_write])?;

    Ok(res.len() as i32)
}

fn reveal(
    _env: FunctionEnvMut<Env>,
    _payload_addr: u32,
    _payload_size: u32,
    _dst_ptr: u32,
    _max_bytes: u32,
) -> Result<i32, RuntimeError> {
    // TODO: https://gitlab.com/tezos/tezos/-/issues/7532
    // We don’t support the `reveal` host function because it is not an immediate requirement of
    // Etherlink, but it is required to enable the WASM Runtime with DAL+Etherlink.
    todo!("reveal")
}

pub fn imports(store: &mut Store, env: &FunctionEnv<Env>) -> Imports {
    imports! {
        "smart_rollup_core" => {
            "write_debug" =>  Function::new_typed_with_env(store, env, write_debug),
            "write_output" =>  Function::new_typed_with_env(store, env, write_output),
            "read_input" =>  Function::new_typed_with_env(store, env, read_input),
            "store_exists" =>  Function::new_typed_with_env(store, env, store_exists),
            "store_has" =>  Function::new_typed_with_env(store, env, store_has),
            "store_delete" =>  Function::new_typed_with_env(store, env, store_delete),
            "store_delete_value" =>  Function::new_typed_with_env(store, env, store_delete_value),
            "store_copy" =>  Function::new_typed_with_env(store, env, store_copy),
            "store_move" =>  Function::new_typed_with_env(store, env, store_move),
            "store_list_size" => Function::new_typed_with_env(store, env, store_list_size),
            "store_value_size" => Function::new_typed_with_env(store, env, store_value_size),
            "store_read" => Function::new_typed_with_env(store, env, store_read),
            "store_write" => Function::new_typed_with_env(store, env, store_write),
            "reveal_metadata" => Function::new_typed_with_env(store, env, reveal_metadata),
            "reveal_preimage" => Function::new_typed_with_env(store, env, reveal_preimage),
            "reveal" => Function::new_typed_with_env(store, env, reveal),
            "__internal_store_get_hash" =>  Function::new_typed_with_env(store, env, store_get_hash),
        }
    }
}
