// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod error;
pub mod helpers;

use crate::error::Error;

use helpers::is_incomplete;
use nom::error::ParseError;
use nom::Finish;
use primitive_types::{H256, U256};
use rlp::{Decodable, Encodable};

use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::nom as tezos_nom;
use tezos_data_encoding::nom::NomReader;
use tezos_ethereum::rlp_helpers::FromRlpBytes;
use tezos_nom::error::DecodeError;
use tezos_smart_rollup_host::path::*;
use tezos_smart_rollup_host::runtime::{RuntimeError, ValueType};
use tezos_smart_rollup_host::storage::StorageV1;

/// The size of one 256 bit word. Size in bytes.
pub const WORD_SIZE: usize = 32usize;

/// The size of contract hash encoded in base58.
pub const KT1_B58_SIZE: usize = 36usize;

/// Return up to buffer.len() from the given path in storage and
/// store the read slice in `buffer`.
///
/// NB: Value is read starting 0.
pub fn store_read_slice(
    host: &impl StorageV1,
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
pub fn read_h256_be(host: &impl StorageV1, path: &impl Path) -> anyhow::Result<H256> {
    let mut buffer = [0_u8; WORD_SIZE];
    store_read_slice(host, path, &mut buffer, WORD_SIZE)?;
    Ok(H256::from_slice(&buffer))
}

/// Return a 32 bytes hash from storage at the given `path`.
///
/// NB: The given bytes are interpreted in big endian order.
pub fn read_h256_be_opt(
    host: &impl StorageV1,
    path: &impl Path,
) -> Result<Option<H256>, Error> {
    match host.store_read(path, 0, WORD_SIZE) {
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
    host: &impl StorageV1,
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
    host: &mut impl StorageV1,
    path: &impl Path,
    hash: H256,
) -> Result<(), Error> {
    Ok(host.store_write(path, hash.as_bytes(), 0)?)
}

/// Return an unsigned 32 bytes value from storage at the given `path`.
///
/// NB: The given bytes are interpreted in little endian order.
pub fn read_u256_le(host: &impl StorageV1, path: &impl Path) -> Result<U256, Error> {
    let bytes = host.store_read(path, 0, 32)?;
    Ok(U256::from_little_endian(&bytes))
}

/// Return an unsigned 32 bytes value from storage at the given `path`.
/// If the path is not found, `default` is returned.
///
/// NB: The given bytes are interpreted in little endian order.
pub fn read_u256_le_default(
    host: &impl StorageV1,
    path: &impl Path,
    default: U256,
) -> Result<U256, Error> {
    match host.store_read(path, 0, WORD_SIZE) {
        Ok(bytes) if bytes.len() == WORD_SIZE => Ok(U256::from_little_endian(&bytes)),
        Ok(_) | Err(RuntimeError::PathNotFound) => Ok(default),
        Err(err) => Err(err.into()),
    }
}

/// Write an unsigned 32 bytes value into storage at the given `path`.
///
/// NB: The value is stored in little endian order.
pub fn write_u256_le(
    host: &mut impl StorageV1,
    path: &impl Path,
    value: U256,
) -> Result<(), Error> {
    let mut bytes: [u8; WORD_SIZE] = value.into();
    value.to_little_endian(&mut bytes);
    host.store_write(path, &bytes, 0).map_err(Error::from)
}

/// Return an unsigned 8 bytes value from storage at the given `path`.
///
/// NB: The given bytes are interpreted in little endian order.
pub fn read_u64_le(host: &impl StorageV1, path: &impl Path) -> Result<u64, Error> {
    let mut bytes = [0; std::mem::size_of::<u64>()];
    host.store_read_slice(path, 0, bytes.as_mut_slice())?;
    Ok(u64::from_le_bytes(bytes))
}

/// Return an unsigned 8 bytes value from storage at the given `path`.
/// If the path is not found, `default` is returned.
///
/// NB: The given bytes are interpreted in little endian order.
pub fn read_u64_le_default(
    host: &impl StorageV1,
    path: &impl Path,
    default: u64,
) -> Result<u64, Error> {
    match host.store_read(path, 0, std::mem::size_of::<u64>()) {
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
    host: &impl StorageV1,
    path: &impl Path,
    default: u16,
) -> Result<u16, Error> {
    // This is exactly the same function as `read_u64_le_default`, but you know
    // the rule, you start sharing code on the 3rd duplication ;-).
    match host.store_read(path, 0, std::mem::size_of::<u16>()) {
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
    host: &mut impl StorageV1,
    path: &impl Path,
    value: u64,
) -> Result<(), Error> {
    host.store_write(path, value.to_le_bytes().as_slice(), 0)
        .map_err(Error::from)
}

/// Store `src` (which must be encodable) as rlp bytes into storage
/// at the given `path`.
pub fn store_rlp<T: Encodable>(
    src: &T,
    host: &mut impl StorageV1,
    path: &impl Path,
) -> Result<(), Error> {
    host.store_write_all(path, &src.rlp_bytes())
        .map_err(Error::from)
}

/// Return a decodable value from storage as rlp bytes
/// at the given `path`.
pub fn read_rlp<T: Decodable>(
    host: &impl StorageV1,
    path: &impl Path,
) -> Result<T, Error> {
    let bytes = host.store_read_all(path)?;
    FromRlpBytes::from_rlp_bytes(&bytes).map_err(Error::from)
}

/// Return a potential decodable value from storage as rlp bytes
/// at the given `path`.
///
/// If there is no data, `None` is returned.
pub fn read_optional_rlp<T: Decodable>(
    host: &impl StorageV1,
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
pub fn read_b58_kt1(host: &impl StorageV1, path: &impl Path) -> Option<ContractKt1Hash> {
    let bytes = host.store_read(path, 0, KT1_B58_SIZE).ok()?;
    let kt1_b58 = String::from_utf8(bytes).ok()?;
    ContractKt1Hash::from_b58check(&kt1_b58).ok()
}

/// Store the `value` into the storage at the given `path`
///
/// The stored value must implement BinWriter
pub fn store_bin(
    value: &impl BinWriter,
    host: &mut impl StorageV1,
    path: &impl Path,
) -> Result<(), Error> {
    let mut buffer = vec![];
    value.bin_write(&mut buffer)?;
    host.store_write_all(path, &buffer)?;
    Ok(())
}

/// Decode a complete `NomReader` value out of `bytes`, rejecting both
/// incomplete parses and trailing data.
fn decode_nom_value<T: for<'a> NomReader<'a>>(bytes: &[u8]) -> Result<T, Error> {
    let result = T::nom_read(bytes);
    // The finish function may panic if the parser is a *streaming parser* that has not enough data.
    // To be sure we don't panic, verify if the result is complete
    if is_incomplete(&result) {
        let incomplete_error =
            DecodeError::from_error_kind(bytes, nom::error::ErrorKind::Eof);
        return Err(incomplete_error.into());
    }
    // Finish the parsing because we can't panic now
    let (remaining, value) = result.finish()?;
    // Verify that all data were consumed
    if !remaining.is_empty() {
        return Err(Error::NomReadError(format!(
            "decoding didn't consume all data, remaining data: {remaining:?}"
        )));
    }
    Ok(value)
}

/// Return a potential decoded value using NomReader
/// at the given `path`
pub fn read_nom_value<T: for<'a> NomReader<'a>>(
    host: &impl StorageV1,
    path: &impl Path,
) -> Result<T, Error> {
    let bytes = host.store_read_all(path)?;
    decode_nom_value(&bytes)
}

/// Like [`read_nom_value`], but reads at most `max_bytes` in a single
/// `store_read` host call rather than first querying the value size and
/// then reading the whole value. Saves one `store_value_size` host call
/// per read.
///
/// `max_bytes` MUST be a proven upper bound on the encoded size of `T`
/// at this path. The host truncates the read to `max_bytes`, so an
/// undersized bound silently feeds a partial value to the decoder, which
/// then errors as incomplete or as having trailing data — it never
/// returns a wrong value, but it does turn a readable value into an
/// error.
pub fn read_nom_value_bounded<T: for<'a> NomReader<'a>>(
    host: &impl StorageV1,
    path: &impl Path,
    max_bytes: usize,
) -> Result<T, Error> {
    let bytes = host.store_read(path, 0, max_bytes)?;
    decode_nom_value(&bytes)
}

/// Return an optional decoded value using NomReader
/// at the given `path`
pub fn read_optional_nom_value<T: for<'a> NomReader<'a>>(
    host: &impl StorageV1,
    path: &impl Path,
) -> Result<Option<T>, Error> {
    // Read directly and map a missing path to `None`, avoiding the extra
    // `store_has` probe (same pattern as [`read_optional_nom_value_bounded`]).
    match host.store_read_all(path) {
        Ok(bytes) => decode_nom_value(&bytes).map(Some),
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(err.into()),
    }
}

/// Like [`read_optional_nom_value`], but reads at most `max_bytes` in a
/// single `store_read` host call. A missing path yields `Ok(None)`,
/// avoiding the extra `store_has` probe — so this is one host call where
/// [`read_optional_nom_value`] needs two (or three, counting the
/// `store_value_size` inside `store_read_all`).
///
/// See [`read_nom_value_bounded`] for the `max_bytes` contract.
pub fn read_optional_nom_value_bounded<T: for<'a> NomReader<'a>>(
    host: &impl StorageV1,
    path: &impl Path,
    max_bytes: usize,
) -> Result<Option<T>, Error> {
    match host.store_read(path, 0, max_bytes) {
        Ok(bytes) => decode_nom_value(&bytes).map(Some),
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(err.into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_data_encoding::types::Narith;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::RefPath;

    const PATH: RefPath = RefPath::assert_from(b"/some/value");

    /// A value read back through a generous bound matches what was
    /// written — the bound being larger than the value is the normal
    /// case and must not corrupt the read.
    #[test]
    fn bounded_read_matches_written_value() {
        let mut host = MockKernelHost::default();
        let value: Narith = 123_456_u64.into();
        store_bin(&value, &mut host, &PATH).unwrap();

        let read: Narith = read_nom_value_bounded(&host, &PATH, 32).unwrap();
        assert_eq!(value, read);

        let read_opt: Option<Narith> =
            read_optional_nom_value_bounded(&host, &PATH, 32).unwrap();
        assert_eq!(Some(value), read_opt);
    }

    /// A missing path yields `None` from the optional variant without a
    /// separate `store_has` probe, and an error from the non-optional
    /// variant.
    #[test]
    fn bounded_read_missing_path() {
        let host = MockKernelHost::default();

        let read_opt: Option<Narith> =
            read_optional_nom_value_bounded(&host, &PATH, 32).unwrap();
        assert_eq!(None, read_opt);

        let read: Result<Narith, _> = read_nom_value_bounded(&host, &PATH, 32);
        assert!(read.is_err());
    }

    /// A bound smaller than the encoded value truncates the read, so the
    /// decoder sees a partial value: it must fail loudly rather than
    /// return a wrong value. `128` encodes to two `Narith` bytes
    /// (`0x80 0x01`); reading a single byte leaves the continuation bit
    /// set with no follow-up byte.
    #[test]
    fn bounded_read_too_small_bound_fails() {
        let mut host = MockKernelHost::default();
        let value: Narith = 128_u64.into();
        store_bin(&value, &mut host, &PATH).unwrap();

        let read: Result<Narith, _> = read_nom_value_bounded(&host, &PATH, 1);
        assert!(read.is_err());

        let read_opt: Result<Option<Narith>, _> =
            read_optional_nom_value_bounded(&host, &PATH, 1);
        assert!(read_opt.is_err());
    }
}
