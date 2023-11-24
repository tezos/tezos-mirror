// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::StorageError;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Error;
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};

const LENGTH: RefPath = RefPath::assert_from(b"/length");

/// Utility function to read a little_endian encoded u64 in the storage
fn read_u64<Host: Runtime>(host: &Host, path: &OwnedPath) -> Result<u64, RuntimeError> {
    let mut buffer = [0_u8; std::mem::size_of::<u64>()];
    let value = host.store_read_slice(path, 0, &mut buffer)?;
    if value != 8 {
        Err(RuntimeError::DecodingError)
    } else {
        Ok(u64::from_le_bytes(buffer))
    }
}

/// Utility function to write u64 in little endian in the storage
fn store_u64<Host: Runtime>(
    host: &mut Host,
    path: &OwnedPath,
    value: u64,
) -> Result<(), RuntimeError> {
    host.store_write_all(path, &value.to_le_bytes())
}

/// An indexable storage is a push-only mapping between increasing integers to
/// bytes. It can serve as a replacement for the combination of the host
/// functions `store_get_nth` and `store_list_size` that are unsafe.
pub struct IndexableStorage {
    /// An indexable storage is stored at a given path and consists of:
    /// - `/length`: the number of keys
    /// - `/<key>` where keys are from `0` to `length - 1`
    pub path: OwnedPath,
}

impl IndexableStorage {
    pub fn new(path: &RefPath<'_>) -> Result<Self, StorageError> {
        Ok(Self { path: path.into() })
    }

    fn value_path(&self, index: u64) -> Result<OwnedPath, StorageError> {
        let index_as_path: Vec<u8> = format!("/{}", index).into();
        // The key being an integer value, it will always be valid as a path,
        // `assert_from` cannot fail.
        let index_subkey = RefPath::assert_from(&index_as_path);
        concat(&self.path, &index_subkey).map_err(StorageError::from)
    }

    fn store_index<Host: Runtime>(
        &self,
        host: &mut Host,
        index: u64,
        value_repr: &[u8],
    ) -> Result<(), StorageError> {
        let key_path = self.value_path(index)?;
        host.store_write_all(&key_path, value_repr)
            .map_err(StorageError::from)
    }

    fn get_length_and_increment<Host: Runtime>(
        &mut self,
        host: &mut Host,
    ) -> Result<u64, StorageError> {
        let path = concat(&self.path, &LENGTH)?;
        let length = read_u64(host, &path).unwrap_or(0);
        store_u64(host, &path, length + 1)?;
        Ok(length)
    }

    #[allow(dead_code)]
    /// `length` returns the number of keys in the storage. If `/length` does
    /// not exists, the storage is considered as empty and returns '0'.
    pub fn length<Host: Runtime>(&self, host: &Host) -> Result<u64, StorageError> {
        let path = concat(&self.path, &LENGTH)?;
        match read_u64(host, &path) {
            Ok(l) => Ok(l),
            Err(
                RuntimeError::PathNotFound
                | RuntimeError::HostErr(tezos_smart_rollup_host::Error::StoreNotAValue)
                | RuntimeError::HostErr(tezos_smart_rollup_host::Error::StoreInvalidAccess),
                // An InvalidAccess implies that the path does not exist at all
                // in the storage: store_read fails because reading is out of
                // bounds since the value has never been allocated before
            ) => Ok(0_u64),
            Err(e) => {
                log!(host, Error, "Error in indexable storage: {}", e);
                Err(e.into())
            }
        }
    }

    #[allow(dead_code)]
    /// Same as `get_value`, but doesn't check for bounds.
    pub fn unsafe_get_value<Host: Runtime>(
        &self,
        host: &Host,
        index: u64,
    ) -> Result<Vec<u8>, StorageError> {
        let key_path = self.value_path(index)?;
        host.store_read_all(&key_path).map_err(StorageError::from)
    }

    /// Returns the value a the given index. Fails if the index is greater or
    /// equal to the length.
    #[cfg(test)]
    pub fn get_value<Host: Runtime>(
        &self,
        host: &Host,
        index: u64,
    ) -> Result<Vec<u8>, StorageError> {
        let length = self.length(host)?;
        if index >= length {
            return Err(StorageError::IndexOutOfBounds);
        };
        self.unsafe_get_value(host, index)
    }

    /// Push a value at index `length`, and increments the length.
    pub fn push_value<Host: Runtime>(
        &mut self,
        host: &mut Host,
        value: &[u8],
    ) -> Result<(), StorageError> {
        let new_index = self.get_length_and_increment(host)?;
        self.store_index(host, new_index, value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_mock::MockHost;

    #[test]
    fn test_indexable_empty() {
        let host = MockHost::default();
        let values = RefPath::assert_from(b"/values");
        let storage = IndexableStorage::new(&values).expect("Path to index is invalid");

        assert_eq!(storage.length(&host), Ok(0));
    }

    #[test]
    fn test_indexing_new_value() {
        let mut host = MockHost::default();
        let values = RefPath::assert_from(b"/values");
        let mut storage =
            IndexableStorage::new(&values).expect("Path to index is invalid");

        let value = b"value";

        storage
            .push_value(&mut host, value)
            .expect("Value could not be indexed");

        assert_eq!(storage.length(&host), Ok(1));

        assert_eq!(storage.get_value(&host, 0), Ok(value.to_vec()))
    }

    #[test]
    fn test_get_out_of_bounds() {
        let mut host = MockHost::default();
        let values = RefPath::assert_from(b"/values");
        let mut storage =
            IndexableStorage::new(&values).expect("Path to index is invalid");

        let value = b"value";

        storage
            .push_value(&mut host, value)
            .expect("Value could not be indexed");

        assert_eq!(storage.length(&host), Ok(1));

        assert_eq!(
            storage.get_value(&host, 1),
            Err(StorageError::IndexOutOfBounds)
        )
    }
}
