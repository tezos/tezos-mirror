// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use num_bigint::{BigInt, Sign};
use revm::primitives::{alloy_primitives::Keccak256, B256, U256};
use tezos_smart_rollup_host::{path::Path, runtime::RuntimeError, storage::StorageV1};

pub fn read_u256_le_default(
    host: &impl StorageV1,
    path: &impl Path,
    default: U256,
) -> Result<U256, RuntimeError> {
    match host.store_read(path, 0, 32) {
        Ok(bytes) if bytes.len() == 32 => Ok(U256::from_le_slice(&bytes)),
        Ok(_) | Err(RuntimeError::PathNotFound) => Ok(default),
        Err(runtime_error) => Err(runtime_error),
    }
}

pub fn write_u256_le(
    host: &mut impl StorageV1,
    path: &impl Path,
    value: U256,
) -> Result<(), RuntimeError> {
    host.store_write(path, &value.to_le_bytes::<{ U256::BYTES }>(), 0)
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

pub fn u256_to_bigint(value: U256) -> BigInt {
    BigInt::from_bytes_be(Sign::Plus, &value.to_be_bytes::<{ U256::BYTES }>())
}
