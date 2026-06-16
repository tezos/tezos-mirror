// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use rlp::DecoderError;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Error;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::{concat, OwnedPath, PathError, RefPath};
use tezos_smart_rollup_host::runtime::RuntimeError;
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

#[derive(Error, Debug, Eq, PartialEq)]
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

    fn store_index<Host: Runtime>(
        &self,
        host: &mut Host,
        index: u64,
        value_repr: &[u8],
    ) -> Result<(), IndexableStorageError> {
        let key_path = self.value_path(index)?;
        host.store_write_all(&key_path, value_repr)
            .map_err(IndexableStorageError::from)
    }

    fn get_length_and_increment<Host: Runtime>(
        &self,
        host: &mut Host,
    ) -> Result<u64, IndexableStorageError> {
        let path = concat(&self.path, &LENGTH)?;
        let length = read_u64_le(host, &path).unwrap_or(0);
        write_u64_le(host, &path, length + 1)?;
        Ok(length)
    }

    #[allow(dead_code)]
    /// `length` returns the number of keys in the storage. If `/length` does
    /// not exists, the storage is considered as empty and returns '0'.
    pub fn length<Host: Runtime>(
        &self,
        host: &Host,
    ) -> Result<u64, IndexableStorageError> {
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
    #[cfg(debug_assertions)]
    pub fn get_value<Host: Runtime>(
        &self,
        host: &Host,
        index: u64,
    ) -> Result<Vec<u8>, IndexableStorageError> {
        let length = self.length(host)?;
        if index >= length {
            return Err(IndexableStorageError::IndexOutOfBounds);
        };
        self.unsafe_get_value(host, index)
            .map_err(IndexableStorageError::from)
    }

    /// Push a value at index `length`, and increments the length.
    pub fn push_value<Host: Runtime>(
        &self,
        host: &mut Host,
        value: &[u8],
    ) -> Result<(), IndexableStorageError> {
        let new_index = self.get_length_and_increment(host)?;
        self.store_index(host, new_index, value)
    }

    /// Push multiple values in batch, reading and writing the length only once.
    /// This reduces storage operations from 3*N to N+2 compared to calling
    /// `push_value` N times.
    pub fn push_values<Host: Runtime>(
        &self,
        host: &mut Host,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::RefPath;

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
}
