// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::{B256, U256};
use tezos_smart_rollup_host::{
    path::Path,
    runtime::{Runtime, RuntimeError},
};

pub fn read_u64_le_default(host: &impl Runtime, path: &impl Path, default: u64) -> u64 {
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == std::mem::size_of::<u64>() => {
            let bytes_array: [u8; std::mem::size_of::<u64>()] = bytes.try_into().unwrap();
            u64::from_le_bytes(bytes_array)
        }
        Ok(_) | Err(RuntimeError::PathNotFound) => default,
        Err(err) => panic!("{err:?}"),
    }
}

pub fn read_u256_le_default(
    host: &impl Runtime,
    path: &impl Path,
    default: U256,
) -> U256 {
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == 32 => U256::from_le_slice(&bytes),
        Ok(_) | Err(RuntimeError::PathNotFound) => default,
        Err(err) => panic!("{err:?}"),
    }
}

pub fn read_b256_be_opt(host: &impl Runtime, path: &impl Path) -> Option<B256> {
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == 32 => Some(B256::from_slice(&bytes)),
        Ok(_) | Err(RuntimeError::PathNotFound) => None,
        Err(err) => panic!("{err:?}"),
    }
}

pub fn read_b256_be_default(
    host: &impl Runtime,
    path: &impl Path,
    default: B256,
) -> B256 {
    match read_b256_be_opt(host, path) {
        Some(v) => v,
        None => default,
    }
}
