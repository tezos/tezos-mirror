// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;
use revm::primitives::{alloy_primitives::Keccak256, B256, U256};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::{
    path::{concat as host_concat, OwnedPath, Path},
    runtime::RuntimeError,
};

pub fn concat(prefix: &impl Path, suffix: &impl Path) -> Result<OwnedPath, Error> {
    host_concat(prefix, suffix).map_err(|err| Error::Custom(err.to_string()))
}

pub fn read_u64_le_default(
    host: &impl Runtime,
    path: &impl Path,
    default: u64,
) -> Result<u64, Error> {
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == std::mem::size_of::<u64>() => {
            let bytes_array: [u8; std::mem::size_of::<u64>()] = match bytes.try_into() {
                Ok(bytes) => bytes,
                Err(err) => {
                    return Err(Error::Custom(format!(
                        "Bytes array conversion failed with {err:?}",
                    )))
                }
            };
            Ok(u64::from_le_bytes(bytes_array))
        }
        Ok(_) | Err(RuntimeError::PathNotFound) => Ok(default),
        Err(err) => Err(Error::Runtime(err)),
    }
}

pub fn read_u256_le_default(
    host: &impl Runtime,
    path: &impl Path,
    default: U256,
) -> Result<U256, RuntimeError> {
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == 32 => Ok(U256::from_le_slice(&bytes)),
        Ok(_) | Err(RuntimeError::PathNotFound) => Ok(default),
        Err(runtime_error) => Err(runtime_error),
    }
}

pub fn read_b256_be_opt(
    host: &impl Runtime,
    path: &impl Path,
) -> Result<Option<B256>, RuntimeError> {
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == 32 => Ok(Some(B256::from_slice(&bytes))),
        Ok(_) | Err(RuntimeError::PathNotFound) => Ok(None),
        Err(runtime_error) => Err(runtime_error),
    }
}

pub fn read_b256_be_default(
    host: &impl Runtime,
    path: &impl Path,
    default: B256,
) -> Result<B256, RuntimeError> {
    match read_b256_be_opt(host, path)? {
        Some(v) => Ok(v),
        None => Ok(default),
    }
}

pub fn write_u64_le(
    host: &mut impl Runtime,
    path: &impl Path,
    value: u64,
) -> Result<(), RuntimeError> {
    host.store_write_all(path, value.to_le_bytes().as_slice())
}

pub fn bytes_hash(bytes: &[u8]) -> B256 {
    let mut keccak = Keccak256::new();
    keccak.update(bytes);
    keccak.finalize()
}

pub fn u256_to_le_bytes(value: primitive_types::U256) -> Vec<u8> {
    let mut bytes = vec![0u8; 32];
    value.to_little_endian(&mut bytes);
    bytes
}
