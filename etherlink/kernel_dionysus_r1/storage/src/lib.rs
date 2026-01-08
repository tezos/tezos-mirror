// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod error;
pub mod helpers;

use crate::error::Error;

use primitive_types::{H256, U256};
use rlp::{Decodable, Encodable};

use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_ethereum::rlp_helpers::FromRlpBytes;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::*;
use tezos_smart_rollup_host::runtime::{RuntimeError, ValueType};

/// The size of one 256 bit word. Size in bytes.
pub const WORD_SIZE: usize = 32usize;

/// Return up to buffer.len() from the given path in storage and
/// store the read slice in `buffer`.
///
/// NB: Value is read starting 0.
pub fn store_read_slice(
    host: &impl Runtime,
    path: &impl Path,
    buffer: &mut [u8],
    expected_size: usize,
) -> Result<(), Error> {
    let size = host.store_read_slice(path, 0, buffer)?;
    if size == expected_size {
        Ok(())
    } else {
        Err(Error::InvalidLoadValue {
            expected: expected_size,
            actual: size,
        })
    }
}

/// Get the path corresponding to an index of H256.
pub fn path_from_h256(index: &H256) -> Result<OwnedPath, Error> {
    let path_string = format!("/{}", hex::encode(index.to_fixed_bytes()));
    OwnedPath::try_from(path_string).map_err(Error::from)
}

/// Return a 32 bytes hash from storage at the given `path`.
///
/// NB: The given bytes are interpreted in big endian order.
pub fn read_h256_be(host: &impl Runtime, path: &impl Path) -> anyhow::Result<H256> {
    let mut buffer = [0_u8; WORD_SIZE];
    store_read_slice(host, path, &mut buffer, WORD_SIZE)?;
    Ok(H256::from_slice(&buffer))
}

/// Return a 32 bytes hash from storage at the given `path`.
///
/// NB: The given bytes are interpreted in big endian order.
pub fn read_h256_be_opt(
    host: &impl Runtime,
    path: &impl Path,
) -> Result<Option<H256>, Error> {
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == WORD_SIZE => Ok(Some(H256::from_slice(&bytes))),
        Ok(_) | Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(err.into()),
    }
}

/// Return a 32 bytes hash from storage at the given `path`.
/// If the path is not found, `default` is returned.
///
/// NB: The given bytes are interpreted in big endian order.
pub fn read_h256_be_default(
    host: &impl Runtime,
    path: &impl Path,
    default: H256,
) -> Result<H256, Error> {
    match read_h256_be_opt(host, path)? {
        Some(v) => Ok(v),
        None => Ok(default),
    }
}

/// Write a 32 bytes hash into storage at the given `path`.
///
/// NB: The hash is stored in big endian order.
pub fn write_h256_be(
    host: &mut impl Runtime,
    path: &impl Path,
    hash: H256,
) -> anyhow::Result<()> {
    Ok(host.store_write_all(path, hash.as_bytes())?)
}

/// Return an unsigned 32 bytes value from storage at the given `path`.
///
/// NB: The given bytes are interpreted in little endian order.
pub fn read_u256_le(host: &impl Runtime, path: &impl Path) -> Result<U256, Error> {
    let bytes = host.store_read_all(path)?;
    Ok(U256::from_little_endian(&bytes))
}

/// Return an unsigned 32 bytes value from storage at the given `path`.
/// If the path is not found, `default` is returned.
///
/// NB: The given bytes are interpreted in little endian order.
pub fn read_u256_le_default(
    host: &impl Runtime,
    path: &impl Path,
    default: U256,
) -> Result<U256, Error> {
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == WORD_SIZE => Ok(U256::from_little_endian(&bytes)),
        Ok(_) | Err(RuntimeError::PathNotFound) => Ok(default),
        Err(err) => Err(err.into()),
    }
}

/// Write an unsigned 32 bytes value into storage at the given `path`.
///
/// NB: The value is stored in little endian order.
pub fn write_u256_le(
    host: &mut impl Runtime,
    path: &impl Path,
    value: U256,
) -> Result<(), Error> {
    let mut bytes: [u8; WORD_SIZE] = value.into();
    value.to_little_endian(&mut bytes);
    host.store_write_all(path, &bytes).map_err(Error::from)
}

/// Return an unsigned 8 bytes value from storage at the given `path`.
///
/// NB: The given bytes are interpreted in little endian order.
pub fn read_u64_le(host: &impl Runtime, path: &impl Path) -> Result<u64, Error> {
    let mut bytes = [0; std::mem::size_of::<u64>()];
    host.store_read_slice(path, 0, bytes.as_mut_slice())?;
    Ok(u64::from_le_bytes(bytes))
}

/// Return an unsigned 8 bytes value from storage at the given `path`.
/// If the path is not found, `default` is returned.
///
/// NB: The given bytes are interpreted in little endian order.
pub fn read_u64_le_default(
    host: &impl Runtime,
    path: &impl Path,
    default: u64,
) -> Result<u64, Error> {
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == std::mem::size_of::<u64>() => {
            let bytes_array: [u8; std::mem::size_of::<u64>()] = bytes
                .try_into()
                .map_err(|_| Error::Runtime(RuntimeError::DecodingError))?;
            Ok(u64::from_le_bytes(bytes_array))
        }
        Ok(_) | Err(RuntimeError::PathNotFound) => Ok(default),
        Err(err) => Err(err.into()),
    }
}

/// Return an unsigned 2 bytes value from storage at the given `path`.
/// If the path is not found, `default` is returned.
///
/// NB: The given bytes are interpreted in little endian order.
pub fn read_u16_le_default(
    host: &impl Runtime,
    path: &impl Path,
    default: u16,
) -> Result<u16, Error> {
    // This is exactly the same function as `read_u64_le_default`, but you know
    // the rule, you start sharing code on the 3rd duplication ;-).
    match host.store_read_all(path) {
        Ok(bytes) if bytes.len() == std::mem::size_of::<u16>() => {
            let bytes_array: [u8; std::mem::size_of::<u16>()] = bytes
                .try_into()
                .map_err(|_| Error::Runtime(RuntimeError::DecodingError))?;
            Ok(u16::from_le_bytes(bytes_array))
        }
        Ok(_) | Err(RuntimeError::PathNotFound) => Ok(default),
        Err(err) => Err(err.into()),
    }
}

/// Write an unsigned 8 bytes value into storage at the given `path`.
///
/// NB: The value is stored in little endian order.
pub fn write_u64_le(
    host: &mut impl Runtime,
    path: &impl Path,
    value: u64,
) -> Result<(), Error> {
    host.store_write_all(path, value.to_le_bytes().as_slice())
        .map_err(Error::from)
}

/// Store `src` (which must be encodable) as rlp bytes into storage
/// at the given `path`.
pub fn store_rlp<T: Encodable>(
    src: &T,
    host: &mut impl Runtime,
    path: &impl Path,
) -> Result<(), Error> {
    host.store_write_all(path, &src.rlp_bytes())
        .map_err(Error::from)
}

/// Return a decodable value from storage as rlp bytes
/// at the given `path`.
pub fn read_rlp<T: Decodable>(host: &impl Runtime, path: &impl Path) -> Result<T, Error> {
    let bytes = host.store_read_all(path)?;
    FromRlpBytes::from_rlp_bytes(&bytes).map_err(Error::from)
}

/// Return a potential decodable value from storage as rlp bytes
/// at the given `path`.
///
/// If there is no data, `None` is returned.
pub fn read_optional_rlp<T: Decodable>(
    host: &impl Runtime,
    path: &impl Path,
) -> Result<Option<T>, anyhow::Error> {
    if let Some(ValueType::Value) = host.store_has(path)? {
        let elt = read_rlp(host, path)?;
        Ok(Some(elt))
    } else {
        Ok(None)
    }
}

/// Return a base58 contract address from storage at the given `path`.
pub fn read_b58_kt1(host: &impl Runtime, path: &impl Path) -> Option<ContractKt1Hash> {
    let bytes = host.store_read_all(path).ok()?;
    let kt1_b58 = String::from_utf8(bytes).ok()?;
    ContractKt1Hash::from_b58check(&kt1_b58).ok()
}
