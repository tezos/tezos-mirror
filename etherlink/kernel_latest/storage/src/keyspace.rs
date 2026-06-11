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
use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
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
}
