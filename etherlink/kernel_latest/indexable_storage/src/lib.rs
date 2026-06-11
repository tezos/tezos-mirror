// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use num_bigint::{BigUint, TryFromBigIntError};
use rlp::DecoderError;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Error;
use tezos_smart_rollup_host::path::{concat, OwnedPath, PathError, RefPath};
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{Key, KeyError, KeySpace, KeySpaceWriteError};
use tezos_smart_rollup_storage::StorageError;
use tezos_storage::{error::Error as GenStorageError, read_u64_le, write_u64_le};
use thiserror::Error;

const LENGTH: RefPath = RefPath::assert_from(b"/length");

/// An indexable storage is a push-only mapping between increasing integers to
/// bytes. It can serve as a replacement for the combination of the host
/// functions `store_get_nth` and `store_list_size` that are unsafe.
pub struct IndexableStorage {
    /// An indexable storage is stored at a given path and consists of:
    /// - `/length`: the number of keys
    /// - `/<key>` where keys are from `0` to `length - 1`
    pub path: OwnedPath,
}

#[derive(Error, Debug, Eq, PartialEq, Clone)]
pub enum IndexableStorageError {
    #[error(transparent)]
    Path(#[from] PathError),
    #[error(transparent)]
    Runtime(#[from] RuntimeError),
    #[error(transparent)]
    Storage(#[from] StorageError),
    #[error("Failed to decode: {0}")]
    RlpDecoderError(DecoderError),
    #[error("Storage error: error while reading a value (incorrect size). Expected {expected} but got {actual}")]
    InvalidLoadValue { expected: usize, actual: usize },
    #[error("Storage error: index out of bound")]
    IndexOutOfBounds,
    #[error("Storage error: Failed to encode a value with BinWriter: {0}")]
    BinWriteError(String),
    #[error("Storage error: Failed to decode a value with NomReader: {0}")]
    NomReadError(String),
    #[error("Tried casting an Implicit account into an Originated account")]
    ImplicitToOriginated,
    #[error("Tried casting an Originated account into an Implicit account")]
    OriginatedToImplicit,
    #[error("Typechecking error: {0}")]
    TcError(String),
    #[error("BigInt conversion error: {0}")]
    TryFromBigIntError(TryFromBigIntError<BigUint>),
    #[error("Internal invariant violation: {0}")]
    Internal(String),
    #[error(transparent)]
    KeySpaceWrite(#[from] KeySpaceWriteError),
    #[error("Invalid keyspace key: {0}")]
    KeySpaceKey(#[from] KeyError),
}

impl From<GenStorageError> for IndexableStorageError {
    fn from(e: GenStorageError) -> Self {
        match e {
            GenStorageError::Path(e) => IndexableStorageError::Path(e),
            GenStorageError::Runtime(e) => IndexableStorageError::Runtime(e),
            GenStorageError::Storage(e) => IndexableStorageError::Storage(e),
            GenStorageError::RlpDecoderError(e) => {
                IndexableStorageError::RlpDecoderError(e)
            }
            GenStorageError::InvalidLoadValue { expected, actual } => {
                IndexableStorageError::InvalidLoadValue { expected, actual }
            }
            GenStorageError::NomReadError(msg) => {
                IndexableStorageError::NomReadError(msg)
            }
            GenStorageError::BinWriteError(msg) => {
                IndexableStorageError::BinWriteError(msg)
            }
            GenStorageError::ImplicitToOriginated => {
                IndexableStorageError::ImplicitToOriginated
            }
            GenStorageError::OriginatedToImplicit => {
                IndexableStorageError::OriginatedToImplicit
            }
            GenStorageError::TcError(msg) => IndexableStorageError::TcError(msg),
            GenStorageError::TryFromBigIntError(msg) => {
                IndexableStorageError::TryFromBigIntError(msg)
            }
            GenStorageError::Internal(msg) => IndexableStorageError::Internal(msg),
        }
    }
}

impl IndexableStorage {
    pub fn new(path: &RefPath<'_>) -> Result<Self, StorageError> {
        Ok(Self { path: path.into() })
    }

    pub fn new_owned_path(path: OwnedPath) -> Self {
        Self { path }
    }

    fn value_path(&self, index: u64) -> Result<OwnedPath, PathError> {
        let index_as_path: Vec<u8> = format!("/{index}").into();
        // The key being an integer value, it will always be valid as a path,
        // `assert_from` cannot fail.
        let index_subkey = RefPath::assert_from(&index_as_path);
        concat(&self.path, &index_subkey)
    }

    fn store_index(
        &self,
        host: &mut impl StorageV1,
        index: u64,
        value_repr: &[u8],
    ) -> Result<(), IndexableStorageError> {
        let key_path = self.value_path(index)?;
        host.store_write_all(&key_path, value_repr)
            .map_err(IndexableStorageError::from)
    }

    fn get_length_and_increment(
        &self,
        host: &mut impl StorageV1,
    ) -> Result<u64, IndexableStorageError> {
        let path = concat(&self.path, &LENGTH)?;
        let length = read_u64_le(host, &path).unwrap_or(0);
        write_u64_le(host, &path, length + 1)?;
        Ok(length)
    }

    #[allow(dead_code)]
    /// `length` returns the number of keys in the storage. If `/length` does
    /// not exists, the storage is considered as empty and returns '0'.
    pub fn length<Host>(&self, host: &Host) -> Result<u64, IndexableStorageError>
    where
        Host: StorageV1,
    {
        let path = concat(&self.path, &LENGTH)?;
        match read_u64_le(host, &path) {
            Ok(l) => Ok(l),
            Err(
                GenStorageError::Runtime(
                    RuntimeError::PathNotFound
                    | RuntimeError::HostErr(tezos_smart_rollup_host::Error::StoreNotAValue)
                    | RuntimeError::HostErr(
                        tezos_smart_rollup_host::Error::StoreInvalidAccess,
                    ),
                ),
                // An InvalidAccess implies that the path does not exist at all
                // in the storage: store_read fails because reading is out of
                // bounds since the value has never been allocated before
            ) => Ok(0_u64),
            Err(e) => {
                log!(Error, "Error in indexable storage: {}", e);
                Err(e.into())
            }
        }
    }

    #[allow(dead_code)]
    /// Same as `get_value`, but doesn't check for bounds.
    pub fn unsafe_get_value(
        &self,
        host: &impl StorageV1,
        index: u64,
    ) -> Result<Vec<u8>, StorageError> {
        let key_path = self.value_path(index)?;
        host.store_read_all(&key_path).map_err(StorageError::from)
    }

    /// Returns the value a the given index. Fails if the index is greater or
    /// equal to the length.
    pub fn get_value<Host>(
        &self,
        host: &Host,
        index: u64,
    ) -> Result<Vec<u8>, IndexableStorageError>
    where
        Host: StorageV1,
    {
        let length = self.length(host)?;
        if index >= length {
            return Err(IndexableStorageError::IndexOutOfBounds);
        };
        self.unsafe_get_value(host, index)
            .map_err(IndexableStorageError::from)
    }

    /// Push a value at index `length`, and increments the length.
    pub fn push_value(
        &self,
        host: &mut impl StorageV1,
        value: &[u8],
    ) -> Result<(), IndexableStorageError> {
        let new_index = self.get_length_and_increment(host)?;
        self.store_index(host, new_index, value)
    }

    /// Push multiple values in batch, reading and writing the length only once.
    /// This reduces storage operations from 3*N to N+2 compared to calling
    /// `push_value` N times.
    pub fn push_values(
        &self,
        host: &mut impl StorageV1,
        values: &[Vec<u8>],
    ) -> Result<(), IndexableStorageError> {
        if values.is_empty() {
            return Ok(());
        }
        let length_path = concat(&self.path, &LENGTH)?;
        let base_index = read_u64_le(host, &length_path).unwrap_or(0);
        for (i, value) in values.iter().enumerate() {
            self.store_index(host, base_index + i as u64, value)?;
        }
        write_u64_le(host, &length_path, base_index + values.len() as u64)?;
        Ok(())
    }

    pub fn clear<Host>(&self, host: &mut Host) -> Result<(), IndexableStorageError>
    where
        Host: StorageV1,
    {
        let length = self.length(host)?;
        for index in 0..length {
            let key_path = self.value_path(index)?;
            host.store_delete(&key_path)?;
        }
        let length_path = concat(&self.path, &LENGTH)?;
        match host.store_delete(&length_path) {
            Ok(()) => Ok(()),
            Err(RuntimeError::PathNotFound) if length == 0 => Ok(()),
            Err(err) => Err(IndexableStorageError::Runtime(err)),
        }
    }
}

/// KeySpace-native variant of [`IndexableStorage`]: the same push-only
/// mapping from increasing integers to bytes, addressed by a relative key
/// `prefix` inside a [`KeySpace`] instead of an absolute durable path.
///
/// Byte-compatible with [`IndexableStorage`]: for a keyspace loaded under
/// the name `/name` and a prefix `/p`, the counter lives at
/// `/name/p/length` and the values at `/name/p/<index>` — exactly where
/// the [`StorageV1`] variant rooted at the absolute path `/name/p` puts
/// them.
pub struct KeyspaceIndexableStorage {
    prefix: Key,
}

impl KeyspaceIndexableStorage {
    pub fn new(prefix: Key) -> Self {
        Self { prefix }
    }

    fn value_key(&self, index: u64) -> Result<Key, IndexableStorageError> {
        // The index being an integer, it is always a valid key segment; the
        // construction can only fail if `prefix` plus the segment exceeds
        // the maximum key size.
        Key::try_from(format!("{}/{}", self.prefix, index)).map_err(Into::into)
    }

    fn length_key(&self) -> Result<Key, IndexableStorageError> {
        Key::try_from(format!("{}/length", self.prefix)).map_err(Into::into)
    }

    /// `length` returns the number of keys in the storage. If the length
    /// key does not exist, the storage is considered as empty and returns
    /// '0'.
    pub fn length(&self, ks: &impl KeySpace) -> Result<u64, IndexableStorageError> {
        // Mirrors the `StorageV1` variant: the buffer is zero-initialised
        // and the read size deliberately unchecked, so a missing key is an
        // empty storage and a short value is zero-extended.
        let mut bytes = [0u8; std::mem::size_of::<u64>()];
        match ks.read(&self.length_key()?, 0, &mut bytes) {
            Some(_) => Ok(u64::from_le_bytes(bytes)),
            None => Ok(0),
        }
    }

    fn store_length(
        &self,
        ks: &mut impl KeySpace,
        length: u64,
    ) -> Result<(), IndexableStorageError> {
        ks.set(&self.length_key()?, length.to_le_bytes())
            .map_err(Into::into)
    }

    /// Returns the value at the given index. Fails if the index is greater
    /// or equal to the length.
    pub fn get_value(
        &self,
        ks: &impl KeySpace,
        index: u64,
    ) -> Result<Vec<u8>, IndexableStorageError> {
        let length = self.length(ks)?;
        if index >= length {
            return Err(IndexableStorageError::IndexOutOfBounds);
        };
        ks.get(&self.value_key(index)?)
            .ok_or(IndexableStorageError::Runtime(RuntimeError::PathNotFound))
    }

    /// Push a value at index `length`, and increments the length.
    pub fn push_value(
        &self,
        ks: &mut impl KeySpace,
        value: &[u8],
    ) -> Result<(), IndexableStorageError> {
        let new_index = self.length(ks)?;
        self.store_length(ks, new_index + 1)?;
        ks.set(&self.value_key(new_index)?, value)
            .map_err(Into::into)
    }

    /// Push multiple values in batch, reading and writing the length only
    /// once.
    pub fn push_values(
        &self,
        ks: &mut impl KeySpace,
        values: &[Vec<u8>],
    ) -> Result<(), IndexableStorageError> {
        if values.is_empty() {
            return Ok(());
        }
        let base_index = self.length(ks)?;
        for (i, value) in values.iter().enumerate() {
            ks.set(&self.value_key(base_index + i as u64)?, value)?;
        }
        self.store_length(ks, base_index + values.len() as u64)
    }

    pub fn clear(&self, ks: &mut impl KeySpace) -> Result<(), IndexableStorageError> {
        let length = self.length(ks)?;
        for index in 0..length {
            ks.delete(&self.value_key(index)?);
        }
        ks.delete(&self.length_key()?);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_keyspace::KeySpaceLoader;

    #[test]
    fn test_indexable_empty() {
        let host = MockKernelHost::default();
        let values = RefPath::assert_from(b"/values");
        let storage = IndexableStorage::new(&values).expect("Path to index is invalid");

        assert_eq!(storage.length(&host), Ok(0));
    }

    #[test]
    fn test_indexing_new_value() {
        let mut host = MockKernelHost::default();
        let values = RefPath::assert_from(b"/values");
        let storage = IndexableStorage::new(&values).expect("Path to index is invalid");

        let value = b"value";

        storage
            .push_value(&mut host, value)
            .expect("Value could not be indexed");

        assert_eq!(storage.length(&host), Ok(1));

        assert_eq!(storage.get_value(&host, 0), Ok(value.to_vec()))
    }

    #[test]
    fn test_get_out_of_bounds() {
        let mut host = MockKernelHost::default();
        let values = RefPath::assert_from(b"/values");
        let storage = IndexableStorage::new(&values).expect("Path to index is invalid");

        let value = b"value";

        storage
            .push_value(&mut host, value)
            .expect("Value could not be indexed");

        assert_eq!(storage.length(&host), Ok(1));

        assert_eq!(
            storage.get_value(&host, 1),
            Err(IndexableStorageError::IndexOutOfBounds)
        )
    }

    #[test]
    fn test_push_values_batch() {
        let mut host = MockKernelHost::default();
        let values = RefPath::assert_from(b"/values");
        let storage = IndexableStorage::new(&values).expect("Path to index is invalid");

        let batch = vec![b"first".to_vec(), b"second".to_vec(), b"third".to_vec()];
        storage
            .push_values(&mut host, &batch)
            .expect("Batch push failed");

        assert_eq!(storage.length(&host), Ok(3));
        assert_eq!(storage.get_value(&host, 0), Ok(b"first".to_vec()));
        assert_eq!(storage.get_value(&host, 1), Ok(b"second".to_vec()));
        assert_eq!(storage.get_value(&host, 2), Ok(b"third".to_vec()));
    }

    #[test]
    fn test_push_values_appends_to_existing() {
        let mut host = MockKernelHost::default();
        let values = RefPath::assert_from(b"/values");
        let storage = IndexableStorage::new(&values).expect("Path to index is invalid");

        storage
            .push_value(&mut host, b"existing")
            .expect("Push failed");

        let batch = vec![b"new1".to_vec(), b"new2".to_vec()];
        storage
            .push_values(&mut host, &batch)
            .expect("Batch push failed");

        assert_eq!(storage.length(&host), Ok(3));
        assert_eq!(storage.get_value(&host, 0), Ok(b"existing".to_vec()));
        assert_eq!(storage.get_value(&host, 1), Ok(b"new1".to_vec()));
        assert_eq!(storage.get_value(&host, 2), Ok(b"new2".to_vec()));
    }

    #[test]
    fn test_push_values_empty_is_noop() {
        let mut host = MockKernelHost::default();
        let values = RefPath::assert_from(b"/values");
        let storage = IndexableStorage::new(&values).expect("Path to index is invalid");

        storage
            .push_values(&mut host, &[])
            .expect("Empty batch push failed");

        assert_eq!(storage.length(&host), Ok(0));
    }

    fn keyspace_storage() -> KeyspaceIndexableStorage {
        KeyspaceIndexableStorage::new(Key::from_bytes(b"/values").unwrap())
    }

    #[test]
    fn test_keyspace_empty() {
        let mut host = MockKernelHost::default();
        let ks = host.load_or_create("/indexed".parse().unwrap()).unwrap();
        assert_eq!(keyspace_storage().length(&ks).unwrap(), 0);
    }

    #[test]
    fn test_keyspace_push_get_and_bounds() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/indexed".parse().unwrap()).unwrap();
        let storage = keyspace_storage();

        storage.push_value(&mut ks, b"value").unwrap();

        assert_eq!(storage.length(&ks).unwrap(), 1);
        assert_eq!(storage.get_value(&ks, 0).unwrap(), b"value".to_vec());
        assert_eq!(
            storage.get_value(&ks, 1),
            Err(IndexableStorageError::IndexOutOfBounds)
        );
    }

    #[test]
    fn test_keyspace_push_values_batch_and_clear() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/indexed".parse().unwrap()).unwrap();
        let storage = keyspace_storage();

        storage.push_value(&mut ks, b"existing").unwrap();
        storage
            .push_values(&mut ks, &[b"new1".to_vec(), b"new2".to_vec()])
            .unwrap();

        assert_eq!(storage.length(&ks).unwrap(), 3);
        assert_eq!(storage.get_value(&ks, 0).unwrap(), b"existing".to_vec());
        assert_eq!(storage.get_value(&ks, 1).unwrap(), b"new1".to_vec());
        assert_eq!(storage.get_value(&ks, 2).unwrap(), b"new2".to_vec());

        storage.clear(&mut ks).unwrap();
        assert_eq!(storage.length(&ks).unwrap(), 0);
        assert_eq!(
            storage.get_value(&ks, 0),
            Err(IndexableStorageError::IndexOutOfBounds)
        );
    }

    // The byte-compat guarantee the consumer migration relies on: values
    // written through the `StorageV1` variant at the absolute path
    // `/indexed/values` are read back through the KeySpace variant
    // (keyspace `/indexed`, prefix `/values`) and vice versa, proving both
    // resolve to identical durable paths.
    #[test]
    fn test_keyspace_variant_is_byte_compatible_with_storage_v1_variant() {
        let mut host = MockKernelHost::default();
        let raw = IndexableStorage::new(&RefPath::assert_from(b"/indexed/values"))
            .expect("Path to index is invalid");
        let storage = keyspace_storage();

        raw.push_value(&mut host, b"first").unwrap();
        raw.push_value(&mut host, b"second").unwrap();

        {
            let mut ks = host.load_or_create("/indexed".parse().unwrap()).unwrap();
            assert_eq!(storage.length(&ks).unwrap(), 2);
            assert_eq!(storage.get_value(&ks, 0).unwrap(), b"first".to_vec());
            assert_eq!(storage.get_value(&ks, 1).unwrap(), b"second".to_vec());

            storage.push_value(&mut ks, b"third").unwrap();
            storage.push_values(&mut ks, &[b"fourth".to_vec()]).unwrap();
        }

        assert_eq!(raw.length(&host), Ok(4));
        assert_eq!(raw.get_value(&host, 2), Ok(b"third".to_vec()));
        assert_eq!(raw.get_value(&host, 3), Ok(b"fourth".to_vec()));
    }
}
