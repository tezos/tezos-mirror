// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_keyspace::{Key, KeyError, KeySpace, KeySpaceError, ReadKind};
use tezos_storage::error::StorageReadErrorKind;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum IndexableStorageError {
    #[error("Storage error: index out of bound")]
    IndexOutOfBounds,
    #[error(transparent)]
    KeySpace(#[from] KeySpaceError<StorageReadErrorKind>),
    #[error(transparent)]
    Key(#[from] KeyError),
}

impl From<KeySpaceError> for IndexableStorageError {
    fn from(e: KeySpaceError) -> Self {
        Self::KeySpace(e.widen())
    }
}

/// A push-only mapping from increasing integers to bytes, addressed by a
/// relative key `prefix` inside a [`KeySpace`]: the counter lives at
/// `<prefix>/length` and the values at `<prefix>/<index>`.
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
        // The buffer is zero-initialised and the read size deliberately
        // unchecked, so a missing key is an empty storage and a short value
        // is zero-extended.
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
        let key = self.value_key(index)?;
        ks.get(&key).ok_or_else(|| {
            KeySpaceError::read(
                ks.name(),
                &key,
                ReadKind::Ext(StorageReadErrorKind::NotFound),
            )
            .into()
        })
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
    use tezos_smart_rollup_host::{path::RefPath, storage::StorageV1};
    use tezos_smart_rollup_keyspace::KeySpaceLoader;

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

    /// The keyspace `/indexed` with the prefix `/values` resolves to the
    /// durable paths `/indexed/values/length` and `/indexed/values/<index>`.
    #[test]
    fn test_keyspace_layout_resolves_to_durable_paths() {
        let mut host = MockKernelHost::default();
        {
            let mut ks = host.load_or_create("/indexed".parse().unwrap()).unwrap();
            keyspace_storage()
                .push_values(&mut ks, &[b"first".to_vec(), b"second".to_vec()])
                .unwrap();
        }
        let read = |path: &str| {
            host.store_read_all(&RefPath::assert_from(path.as_bytes()))
                .unwrap()
        };
        assert_eq!(read("/indexed/values/length"), 2u64.to_le_bytes());
        assert_eq!(read("/indexed/values/0"), b"first");
        assert_eq!(read("/indexed/values/1"), b"second");
    }

    /// A counter ahead of its values reports the missing key, not an index
    /// out of bounds.
    #[test]
    fn test_get_value_reports_a_missing_value_as_not_found() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/indexed".parse().unwrap()).unwrap();
        ks.set(
            &Key::from_bytes(b"/values/length").unwrap(),
            1u64.to_le_bytes(),
        )
        .unwrap();
        assert_eq!(
            keyspace_storage().get_value(&ks, 0),
            Err(KeySpaceError::read(
                ks.name(),
                &Key::from_bytes(b"/values/0").unwrap(),
                ReadKind::Ext(StorageReadErrorKind::NotFound)
            )
            .into())
        );
    }
}
