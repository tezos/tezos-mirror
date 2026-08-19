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

/// Return a base58 contract address at the given `key`.
pub fn read_b58_kt1(ks: &impl KeySpace, key: &Key) -> Option<ContractKt1Hash> {
    let buffer: [u8; KT1_B58_SIZE] = ks.get_prefix_exact(key)?;
    let kt1_b58 = std::str::from_utf8(&buffer).ok()?;
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

/// Return a decodable value stored as rlp bytes at the given `key`, or
/// `None` if the key is absent.
///
/// Mirrors the root [`read_optional_rlp`](crate::read_optional_rlp): a
/// missing key yields `None` rather than an error.
pub fn read_optional_rlp<T: Decodable>(
    ks: &impl KeySpace,
    key: &Key,
) -> Result<Option<T>, Error> {
    match ks.get(key) {
        Some(bytes) => Ok(Some(FromRlpBytes::from_rlp_bytes(&bytes)?)),
        None => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_keyspace::extensions::KeySpaceExtNum;
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
        assert_eq!(ks.get_le(&key(b"/number")), Some(42u64));
    }

    #[test]
    fn u64_le_missing_key_is_path_not_found() {
        let mut host = MockKernelHost::default();
        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(ks.get_le::<8, u64>(&key(b"/missing")), None);
    }

    #[test]
    fn le_default_helpers_return_default_when_absent_and_value_when_present() {
        let mut host = MockKernelHost::default();
        {
            let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
            // Absent keys fall back to the supplied default.
            assert_eq!(ks.get_le_or(&key(b"/u64"), 7u64), 7);
            assert_eq!(ks.get_le_or(&key(b"/u32"), 7u32), 7);
            assert_eq!(ks.get_le_or(&key(b"/u16"), 7u16), 7);
        }
        host.store_write_all(&RefPath::assert_from(b"/ks/u64"), &9u64.to_le_bytes())
            .unwrap();
        host.store_write_all(&RefPath::assert_from(b"/ks/u32"), &9u32.to_le_bytes())
            .unwrap();
        host.store_write_all(&RefPath::assert_from(b"/ks/u16"), &9u16.to_le_bytes())
            .unwrap();
        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(ks.get_le_or(&key(b"/u64"), 7u64), 9);
        assert_eq!(ks.get_le_or(&key(b"/u32"), 7u32), 9);
        assert_eq!(ks.get_le_or(&key(b"/u16"), 7u16), 9);
    }

    #[test]
    fn i64_le_round_trips() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/ks/number");
        host.store_write_all(&path, &(-42i64).to_le_bytes())
            .unwrap();

        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(ks.get_le(&key(b"/number")), Some(-42i64));
    }

    #[test]
    fn be_reads_the_same_bytes_the_other_way_round() {
        let mut host = MockKernelHost::default();
        host.store_write_all(
            &RefPath::assert_from(b"/ks/number"),
            &0x0102_0304u32.to_be_bytes(),
        )
        .unwrap();

        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(ks.get_be(&key(b"/number")), Some(0x0102_0304u32));
        // The same bytes read little-endian give the byte-swapped value, so
        // the two readers cannot be confused for one another.
        assert_eq!(ks.get_le(&key(b"/number")), Some(0x0403_0201u32));
        assert_eq!(ks.get_be_or(&key(b"/missing"), 7u32), 7);
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

    // An integer written through the keyspace must land at the absolute path
    // the relative key resolves to, so a migrated value stays readable at its
    // historical durable location.

    #[test]
    fn written_integers_land_at_their_absolute_path_and_round_trip() {
        let mut host = MockKernelHost::default();
        {
            let mut ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
            ks.store_le(&key(b"/level"), 7u32).unwrap();
            ks.store_le(&key(b"/nb_chunks"), 7u16).unwrap();
            ks.store_le(&key(b"/ts"), -42i64).unwrap();
        }
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/ks/level"))
                .unwrap(),
            7u32.to_le_bytes()
        );
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/ks/nb_chunks"))
                .unwrap(),
            7u16.to_le_bytes()
        );
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/ks/ts"))
                .unwrap(),
            (-42i64).to_le_bytes()
        );

        let ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        assert_eq!(ks.get_le(&key(b"/level")), Some(7u32));
        assert_eq!(ks.get_le(&key(b"/nb_chunks")), Some(7u16));
        assert_eq!(ks.get_le(&key(b"/ts")), Some(-42i64));
    }
}
