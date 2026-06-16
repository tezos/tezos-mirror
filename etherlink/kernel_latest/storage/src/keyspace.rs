// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! [`KeySpace`]-based counterparts of the [`StorageV1`]-based read/write
//! helpers from the crate root, taking a keyspace handle and a key relative
//! to it instead of a host and an absolute path.
//!
//! Each helper mirrors the byte layout *and* the error behaviour of its
//! absolute-path counterpart, so consumers can migrate call sites one by one
//! without any observable change in durable storage.
//!
//! [`StorageV1`]: tezos_smart_rollup_host::storage::StorageV1

use crate::error::Error;
use crate::KT1_B58_SIZE;
use rlp::{Decodable, Encodable};
use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_ethereum::rlp_helpers::FromRlpBytes;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_keyspace::{Key, KeySpace};

/// Return an unsigned 8 bytes value at the given `key`.
///
/// NB: The given bytes are interpreted in little endian order. Mirroring
/// the root [`read_u64_le`](crate::read_u64_le), a value shorter than 8
/// bytes is zero-extended rather than rejected.
pub fn read_u64_le(ks: &impl KeySpace, key: &Key) -> Result<u64, Error> {
    let mut bytes = [0; std::mem::size_of::<u64>()];
    ks.read(key, 0, bytes.as_mut_slice())
        .ok_or(Error::Runtime(RuntimeError::PathNotFound))?;
    Ok(u64::from_le_bytes(bytes))
}

/// Return an unsigned 4 bytes value at the given `key`.
///
/// NB: The given bytes are interpreted in little endian order. The value
/// must hold at least 4 bytes.
pub fn read_u32_le(ks: &impl KeySpace, key: &Key) -> Result<u32, Error> {
    let bytes = ks
        .get_prefix_exact::<{ std::mem::size_of::<u32>() }>(key)
        .ok_or(Error::Runtime(RuntimeError::PathNotFound))?;
    Ok(u32::from_le_bytes(bytes))
}

/// Write an unsigned 4 bytes value at the given `key`.
///
/// NB: The value is stored in little endian order, byte-compatible with the
/// root [`store_read_slice`](crate::store_read_slice)/`store_write` pair.
pub fn write_u32_le(ks: &mut impl KeySpace, key: &Key, value: u32) -> Result<(), Error> {
    ks.set(key, value.to_le_bytes()).map_err(Error::from)
}

/// Return an unsigned 2 bytes value at the given `key`.
///
/// NB: The given bytes are interpreted in little endian order. The value
/// must hold at least 2 bytes.
pub fn read_u16_le(ks: &impl KeySpace, key: &Key) -> Result<u16, Error> {
    let bytes = ks
        .get_prefix_exact::<{ std::mem::size_of::<u16>() }>(key)
        .ok_or(Error::Runtime(RuntimeError::PathNotFound))?;
    Ok(u16::from_le_bytes(bytes))
}

/// Write an unsigned 2 bytes value at the given `key`.
///
/// NB: The value is stored in little endian order, byte-compatible with the
/// root [`store_read_slice`](crate::store_read_slice)/`store_write` pair.
pub fn write_u16_le(ks: &mut impl KeySpace, key: &Key, value: u16) -> Result<(), Error> {
    ks.set(key, value.to_le_bytes()).map_err(Error::from)
}

/// Write a signed 8 bytes value at the given `key`.
///
/// NB: The value is stored in little endian order.
pub fn write_i64_le(ks: &mut impl KeySpace, key: &Key, value: i64) -> Result<(), Error> {
    ks.set(key, value.to_le_bytes()).map_err(Error::from)
}

/// Return a signed 8 bytes value at the given `key`.
///
/// NB: The given bytes are interpreted in little endian order. The value
/// must hold at least 8 bytes.
pub fn read_i64_le(ks: &impl KeySpace, key: &Key) -> Result<i64, Error> {
    let bytes = ks
        .get_prefix_exact::<{ std::mem::size_of::<i64>() }>(key)
        .ok_or(Error::Runtime(RuntimeError::PathNotFound))?;
    Ok(i64::from_le_bytes(bytes))
}

/// Return a base58 contract address at the given `key`.
pub fn read_b58_kt1(ks: &impl KeySpace, key: &Key) -> Option<ContractKt1Hash> {
    let mut buffer = [0; KT1_B58_SIZE];
    let read = ks.read(key, 0, buffer.as_mut_slice())?;
    let kt1_b58 = std::str::from_utf8(&buffer[..read]).ok()?;
    ContractKt1Hash::from_b58check(kt1_b58).ok()
}

/// Store `src` (which must be encodable) as rlp bytes at the given `key`.
///
/// Mirrors the root [`store_rlp`](crate::store_rlp): the same encoded bytes
/// are written, so a value migrated to a keyspace key resolving to the same
/// durable path round-trips unchanged.
pub fn store_rlp<T: Encodable>(
    src: &T,
    ks: &mut impl KeySpace,
    key: &Key,
) -> Result<(), Error> {
    ks.set(key, src.rlp_bytes()).map_err(Error::from)
}

/// Return a decodable value stored as rlp bytes at the given `key`.
///
/// Mirrors the root [`read_rlp`](crate::read_rlp), returning
/// [`RuntimeError::PathNotFound`] when the key is absent.
pub fn read_rlp<T: Decodable>(ks: &impl KeySpace, key: &Key) -> Result<T, Error> {
    let bytes = ks
        .get(key)
        .ok_or(Error::Runtime(RuntimeError::PathNotFound))?;
    FromRlpBytes::from_rlp_bytes(&bytes).map_err(Error::from)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_keyspace::KeySpaceLoader;

    const KT1: &str = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT";

    fn key(bytes: &[u8]) -> Key {
        Key::from_bytes(bytes).unwrap()
    }

    // Each value written through the raw host at the absolute path must read
    // back identically through the keyspace helper at the relative key: the
    // helpers must not change the byte layout.

    #[test]
    fn u64_le_byte_compatible_with_absolute_helper() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/ks/number");
        host.store_write_all(&path, &42u64.to_le_bytes()).unwrap();

        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(read_u64_le(&ks, &key(b"/number")), Ok(42));
    }

    #[test]
    fn u64_le_missing_key_is_path_not_found() {
        let mut host = MockKernelHost::default();
        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(
            read_u64_le(&ks, &key(b"/missing")),
            Err(Error::Runtime(RuntimeError::PathNotFound))
        );
    }

    #[test]
    fn i64_le_round_trips() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/ks/number");
        host.store_write_all(&path, &(-42i64).to_le_bytes())
            .unwrap();

        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(read_i64_le(&ks, &key(b"/number")), Ok(-42));
    }

    #[test]
    fn b58_kt1_byte_compatible_with_absolute_helper() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/ks/contract");
        host.store_write_all(&path, KT1.as_bytes()).unwrap();

        assert_eq!(
            crate::read_b58_kt1(&host, &path),
            ContractKt1Hash::from_b58check(KT1).ok()
        );
        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(
            read_b58_kt1(&ks, &key(b"/contract")),
            ContractKt1Hash::from_b58check(KT1).ok()
        );
    }

    #[test]
    fn b58_kt1_missing_or_garbage_is_none() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/ks/garbage");
        host.store_write_all(&path, b"not a contract").unwrap();

        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(read_b58_kt1(&ks, &key(b"/garbage")), None);
        assert_eq!(read_b58_kt1(&ks, &key(b"/missing")), None);
    }

    // The write helpers must land their bytes at the absolute path the
    // relative key resolves to, so a value migrated to a keyspace writer
    // stays readable at its historical durable location.

    #[test]
    fn write_u32_le_lands_at_absolute_path_and_round_trips() {
        let mut host = MockKernelHost::default();
        {
            let mut ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
            write_u32_le(&mut ks, &key(b"/level"), 7).unwrap();
        }
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/ks/level"))
                .unwrap(),
            7u32.to_le_bytes()
        );
        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(read_u32_le(&ks, &key(b"/level")), Ok(7));
    }

    #[test]
    fn write_u16_le_lands_at_absolute_path_and_round_trips() {
        let mut host = MockKernelHost::default();
        {
            let mut ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
            write_u16_le(&mut ks, &key(b"/nb_chunks"), 7).unwrap();
        }
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/ks/nb_chunks"))
                .unwrap(),
            7u16.to_le_bytes()
        );
        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(read_u16_le(&ks, &key(b"/nb_chunks")), Ok(7));
    }

    #[test]
    fn write_i64_le_lands_at_absolute_path_and_round_trips() {
        let mut host = MockKernelHost::default();
        {
            let mut ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
            write_i64_le(&mut ks, &key(b"/ts"), -42).unwrap();
        }
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/ks/ts"))
                .unwrap(),
            (-42i64).to_le_bytes()
        );
        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(read_i64_le(&ks, &key(b"/ts")), Ok(-42));
    }

    #[test]
    fn store_rlp_byte_compatible_with_absolute_helper() {
        // Same value, same resolved path, written through the keyspace helper
        // vs the root `store_rlp`: the durable bytes must be identical.
        let mut ks_host = MockKernelHost::default();
        {
            let mut ks = ks_host.load_or_create("/ks".parse().unwrap()).unwrap();
            store_rlp(&1234u64, &mut ks, &key(b"/blob")).unwrap();
        }
        let mut raw_host = MockKernelHost::default();
        crate::store_rlp(&1234u64, &mut raw_host, &RefPath::assert_from(b"/ks/blob"))
            .unwrap();

        assert_eq!(
            ks_host
                .store_read_all(&RefPath::assert_from(b"/ks/blob"))
                .unwrap(),
            raw_host
                .store_read_all(&RefPath::assert_from(b"/ks/blob"))
                .unwrap()
        );
        let ks = ks_host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(read_rlp::<u64>(&ks, &key(b"/blob")), Ok(1234));
    }
}
