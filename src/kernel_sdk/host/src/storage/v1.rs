// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Irmin-backed durable storage functionality, currently integrated in the WASM pvm.

use tezos_smart_rollup_core::SmartRollupCore;
use tezos_smart_rollup_core::STORE_HASH_SIZE;

use crate::path::Path;
use crate::runtime::RuntimeError;
use crate::runtime::ValueType;
use crate::Error;

#[cfg(feature = "alloc")]
use alloc::vec::Vec;

/// Durable storage capability, backed by irmin when running in the WASM pvm.
pub trait StorageV1 {
    /// Returns whether a given path exists in storage.
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError>;

    /// Read up to `max_bytes` from the given path in storage, starting `from_offset`.
    #[cfg(feature = "alloc")]
    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError>;

    /// Read up to `buffer.len()` from the given path in storage.
    ///
    /// Value is read starting `from_offset`.
    ///
    /// The total bytes read is returned.
    /// If the returned value `n` is `n < buffer.len()`, then only the first `n`
    /// bytes of the buffer will have been written too.
    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError>;

    /// Read an entire value from the given path in storage.
    #[cfg(feature = "alloc")]
    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError>;

    /// Write the bytes given by `src` to storage at `path`, starting `at_offset`.
    ///
    /// Contrary to `store_write_all`, this does not replace the value (if any)
    /// previously stored under `path`. This allows for splicing/patching values
    /// directly in storage, without having to read the entire value from disk.
    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError>;

    /// Write the bytes given by `src` to storage at `path`.
    ///
    /// Contrary to `store_write`, this replaces the value (if any) that
    /// was previously stored at `path`.
    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError>;

    /// Delete `path` from storage.
    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError>;

    /// Delete value under `path` from storage.
    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError>;

    /// Count the number of subkeys under `prefix`.
    ///
    /// See [SmartRollupCore::store_list_size].
    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError>;

    /// Move one part of durable storage to a different location
    ///
    /// See [SmartRollupCore::store_move].
    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError>;

    /// Copy one part of durable storage to a different location
    ///
    /// See [SmartRollupCore::store_copy].
    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError>;

    /// Return the size of value stored at `path`
    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError>;

    /// Retrieve the root hash of the tree in durable storage at `path`
    fn store_get_hash(
        &self,
        path: &impl Path,
    ) -> Result<[u8; STORE_HASH_SIZE], RuntimeError>;
}

impl<Host: SmartRollupCore> StorageV1 for Host {
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        let result =
            unsafe { SmartRollupCore::store_has(self, path.as_ptr(), path.size()) };

        let value_type = Error::wrap(result).map_err(RuntimeError::HostErr)? as i32;

        match value_type {
            tezos_smart_rollup_core::VALUE_TYPE_NONE => Ok(None),
            tezos_smart_rollup_core::VALUE_TYPE_VALUE => Ok(Some(ValueType::Value)),
            tezos_smart_rollup_core::VALUE_TYPE_SUBTREE => Ok(Some(ValueType::Subtree)),
            tezos_smart_rollup_core::VALUE_TYPE_VALUE_WITH_SUBTREE => {
                Ok(Some(ValueType::ValueWithSubtree))
            }
            _ => Err(RuntimeError::HostErr(Error::GenericInvalidAccess)),
        }
    }

    #[cfg(feature = "alloc")]
    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;

        let mut buffer = Vec::with_capacity(max_bytes);

        unsafe {
            #![allow(clippy::uninit_vec)]
            // SAFETY:
            // Setting length here gives access, from safe rust, to
            // uninitialised bytes.
            //
            // This is safe as these bytes will not be read by `store_read_slice`.
            // Rather, store_read_slice writes to the (part) of the slice, and
            // returns the total bytes written.
            buffer.set_len(usize::min(MAX_FILE_CHUNK_SIZE, max_bytes));

            let size = self
                .store_read_slice(path, from_offset, &mut buffer)
                .map_err(check_path_has_value(self, path))?;

            // SAFETY:
            // We ensure that we set the length of the vector to the
            // total bytes written - ie so that only the bytes that are now
            // initialised, are accessible.
            buffer.set_len(size);
        }

        Ok(buffer)
    }

    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        let result = unsafe {
            self.store_read(
                path.as_ptr(),
                path.size(),
                from_offset,
                buffer.as_mut_ptr(),
                buffer.len(),
            )
        };

        match Error::wrap(result) {
            Ok(i) => Ok(i),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    #[cfg(feature = "alloc")]
    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;

        let length = StorageV1::store_value_size(self, path)?;
        let mut buffer: Vec<u8> = Vec::with_capacity(length);

        // SAFETY: the algorithm goes as follows:
        //
        // A vector of capacity [length] has been previously allocated, of the exact
        // size of the value in the storage. Only `MAX_FILE_CHUNK_SIZE` bytes
        // can be read at once, as such the values must be read as chunks.
        while buffer.len() < length {
            let offset = buffer.len();
            let max_length = usize::min(MAX_FILE_CHUNK_SIZE, length - offset);
            // At each loop, `store_read` takes a slice starting at the next
            // offset in the value, which is the current length of the buffer.
            // The slicing is always valid since it starts at the buffer's
            // length, and always below the vector capacity by construction of
            // the looping condition.
            let slice = &mut buffer[offset..];
            unsafe {
                // SAFETY: `store_read` expects a pointer to write the value,
                // which is given by the slicing in the buffer.
                let chunk_size = self.store_read(
                    path.as_ptr(),
                    path.size(),
                    offset,
                    slice.as_mut_ptr(),
                    max_length,
                );
                let chunk_size =
                    Error::wrap(chunk_size).map_err(RuntimeError::HostErr)?;
                // SAFETY: at the end of the loop, the buffer's length is
                // incremented with the size of the value read, since
                // `store_read` won't update it (as it only deals with
                // pointers). This makes the slicing at the next loop valid,
                // since it starts from the buffer length, and is less than the capacity.
                buffer.set_len(offset + chunk_size)
            };
        }
        Ok(buffer)
    }

    fn store_write<T: Path>(
        &mut self,
        path: &T,
        mut src: &[u8],
        mut at_offset: usize,
    ) -> Result<(), RuntimeError> {
        use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;

        let write = |bytes: &[u8], offset| {
            let result_code = unsafe {
                SmartRollupCore::store_write(
                    self,
                    path.as_ptr(),
                    path.size(),
                    offset,
                    bytes.as_ptr(),
                    bytes.len(),
                )
            };
            match Error::wrap(result_code) {
                Ok(_) => Ok(()),
                Err(e) => Err(RuntimeError::HostErr(e)),
            }
        };

        if src.len() <= MAX_FILE_CHUNK_SIZE {
            return write(src, at_offset);
        }

        while src.len() > MAX_FILE_CHUNK_SIZE {
            write(&src[..MAX_FILE_CHUNK_SIZE], at_offset)?;
            at_offset += MAX_FILE_CHUNK_SIZE;
            src = &src[MAX_FILE_CHUNK_SIZE..];
        }

        // Don't do final extra write of zero bytes
        if !src.is_empty() {
            write(src, at_offset)
        } else {
            Ok(())
        }
    }

    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        value: &[u8],
    ) -> Result<(), RuntimeError> {
        // Removing the value first prevents some cases where a value already
        // exists in the storage at the given path and is bigger than the one
        // being written. Keeping the old value would lead to a situation where
        // ```
        // let () = host.store_write_all(path, value)?;
        // let value_read = host.store_read_all(path)?;
        // value != value_read
        // ```
        // due to remaining bytes from the previous value.
        //
        // `store_delete_value` always succeeds even if the path does not
        // exists, hence there is no need to check the value exists beforehand.
        StorageV1::store_delete_value(self, path)?;

        StorageV1::store_write(self, path, value, 0)
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        if let Ok(None) = StorageV1::store_has(self, path) {
            return Err(RuntimeError::PathNotFound);
        }

        let res =
            unsafe { SmartRollupCore::store_delete(self, path.as_ptr(), path.size()) };
        match Error::wrap(res) {
            Ok(_) => Ok(()),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let res = unsafe {
            SmartRollupCore::store_delete_value(self, path.as_ptr(), path.size())
        };
        match Error::wrap(res) {
            Ok(_) => Ok(()),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn store_count_subkeys<T: Path>(&self, path: &T) -> Result<u64, RuntimeError> {
        let count =
            unsafe { SmartRollupCore::store_list_size(self, path.as_ptr(), path.size()) };

        if count >= 0 {
            Ok(count as u64)
        } else {
            Err(RuntimeError::HostErr(count.into()))
        }
    }

    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let res = unsafe {
            SmartRollupCore::store_move(
                self,
                from_path.as_ptr(),
                from_path.size(),
                to_path.as_ptr(),
                to_path.size(),
            )
        };
        match Error::wrap(res) {
            Ok(_) => Ok(()),
            Err(e) => {
                Err(RuntimeError::HostErr(e)).map_err(check_path_exists(self, from_path))
            }
        }
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let res = unsafe {
            SmartRollupCore::store_copy(
                self,
                from_path.as_ptr(),
                from_path.size(),
                to_path.as_ptr(),
                to_path.size(),
            )
        };
        match Error::wrap(res) {
            Ok(_) => Ok(()),
            Err(e) => {
                Err(RuntimeError::HostErr(e)).map_err(check_path_exists(self, from_path))
            }
        }
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        let res = unsafe {
            SmartRollupCore::store_value_size(self, path.as_ptr(), path.size())
        };
        match Error::wrap(res) {
            Ok(size) => Ok(size),
            Err(e) => {
                Err(RuntimeError::HostErr(e)).map_err(check_path_has_value(self, path))
            }
        }
    }

    fn store_get_hash(
        &self,
        path: &impl Path,
    ) -> Result<[u8; STORE_HASH_SIZE], RuntimeError> {
        let mut buffer = [0u8; STORE_HASH_SIZE];
        let result = unsafe {
            self.__internal_store_get_hash(
                path.as_ptr(),
                path.size(),
                buffer.as_mut_ptr(),
                STORE_HASH_SIZE,
            )
        };
        match Error::wrap(result) {
            Ok(i) if i == STORE_HASH_SIZE => Ok(buffer),
            Ok(_) => Err(RuntimeError::DecodingError),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }
}

fn check_path_has_value<'a>(
    runtime: &'a impl StorageV1,
    path: &'a impl Path,
) -> impl FnOnce(RuntimeError) -> RuntimeError + 'a {
    |err| {
        if let Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) =
            runtime.store_has(path)
        {
            err
        } else {
            RuntimeError::PathNotFound
        }
    }
}

fn check_path_exists<'a, T: Path>(
    runtime: &'a impl StorageV1,
    path: &'a T,
) -> impl FnOnce(RuntimeError) -> RuntimeError + 'a {
    |err| {
        if let Ok(Some(_)) = runtime.store_has(path) {
            err
        } else {
            RuntimeError::PathNotFound
        }
    }
}
