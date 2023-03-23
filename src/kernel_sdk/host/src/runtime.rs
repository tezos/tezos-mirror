// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Definition of **Runtime** api that is callable from *safe* rust.
//!
//! Includes blanket implementation for all types implementing [SmartRollupCore].

#[cfg(feature = "alloc")]
use alloc::vec::Vec;
use tezos_smart_rollup_core::{SmartRollupCore, PREIMAGE_HASH_SIZE};

#[cfg(feature = "alloc")]
use crate::input::Message;
use crate::metadata::RollupMetadata;
#[cfg(feature = "alloc")]
use crate::path::{Path, RefPath};
#[cfg(not(feature = "alloc"))]
use crate::path::{Path, RefPath};
use crate::{Error, METADATA_SIZE};
#[cfg(feature = "alloc")]
use tezos_smart_rollup_core::smart_rollup_core::ReadInputMessageInfo;

#[cfg(feature = "alloc")]
use alloc::string::String;

#[derive(Copy, Eq, PartialEq, Clone, Debug)]
/// Errors that may be returned when called [Runtime] methods.
pub enum RuntimeError {
    /// Attempted to read from/delete a key that does not exist.
    PathNotFound,
    /// Attempted to get a subkey at an out-of-bounds index.
    StoreListIndexOutOfBounds,
    /// Errors returned by the host functions
    HostErr(Error),
    /// Failed parsing
    DecodingError,
}

// TODO: use `core:error::Error` once `error_in_core` stabilised.
//       <https://github.com/rust-lang/rust/issues/103765>
#[cfg(feature = "std")]
impl std::error::Error for RuntimeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl core::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::PathNotFound => write!(f, "RuntimeError::PathNotFound"),
            Self::HostErr(e) => e.fmt(f),
            Self::DecodingError => write!(f, "RuntimeError::DecodingError"),
            Self::StoreListIndexOutOfBounds => {
                write!(f, "RuntimeError::StoreListIndexOutOfBounds")
            }
        }
    }
}

/// Returned by [`Runtime::store_has`] - specifies whether a path has a value or is a prefix.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ValueType {
    /// The path has a value, but is not a prefix to further values.
    Value,
    /// The path is a prefix to further values, but has no value.
    Subtree,
    /// The path has a value, and is a prefix to further values.
    ValueWithSubtree,
}

/// Safe wrappers for host capabilities.
///
/// **NB**:
/// - methods that take `&self` will not cause changes to the runtime state.
/// - methods taking `&mut self` are expected to cause changes - either to *input*,
///   *output* or *durable storage*.
pub trait Runtime {
    /// Write contents of the given slice to output.
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError>;

    /// Write message to debug log.
    fn write_debug(&self, msg: &str);

    /// Read the next input from the global inbox.
    ///
    /// Returns `None` if no message was available. This happens when the kernel has
    /// finished reading the inbox at the current level.
    ///
    /// The kernel will need to yield to the next level to recieve more input.
    #[cfg(feature = "alloc")]
    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError>;

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
    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError>;

    /// Delete `path` from storage.
    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError>;

    /// Delete value under `path` from storage.
    #[cfg(feature = "proto-nairobi")]
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

    /// Reveal pre-image from a hash of size `PREIMAGE_HASH_SIZE` in bytes.
    ///
    /// N.B. in future, multiple hashing schemes will be supported, but for
    /// now the kernels only support hashes of type `Reveal_hash`, which is
    /// a 32-byte Blake2b hash with a prefix-byte of `0`.
    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError>;

    /// Return the size of value stored at `path`
    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError>;

    /// Mark the kernel for reboot.
    ///
    /// If the kernel is marked for reboot, it will continue
    /// reading inbox messages for the current level next time `kernel_run` runs.
    /// If the inbox contains no more messages, the kernel will still continue at
    /// the current inbox level until it is no longer marked for reboot.
    ///
    /// If the kernel is _not_ marked for reboot, it will skip the rest of the inbox
    /// for the current level and _yield_. It will then continue at the next inbox
    /// level.
    ///
    /// The kernel is given a maximum number of reboots per level. The number of reboots remaining
    /// is written to `/readonly/kernel/env/reboot_counter` (little endian i32).
    ///
    /// If the kernel exceeds this, it is forced to yield to the next level (and a flag is set at
    /// `/readonly/kernel/env/too_many_reboot` to indicate this happened.
    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError>;

    /// Returns [RollupMetadata]
    fn reveal_metadata(&self) -> RollupMetadata;

    /// True if the last kernel run was aborted.
    fn last_run_aborted(&self) -> Result<bool, RuntimeError>;

    /// True if the kernel failed to upgrade.
    fn upgrade_failed(&self) -> Result<bool, RuntimeError>;

    /// True if the kernel rebooted too many times.
    fn restart_forced(&self) -> Result<bool, RuntimeError>;

    /// The number of reboot left for the kernel.
    fn reboot_left(&self) -> Result<u32, RuntimeError>;

    /// The runtime_version the kernel is using.
    #[cfg(feature = "alloc")]
    fn runtime_version(&self) -> Result<String, RuntimeError>;
}

const REBOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/env/reboot");

impl<Host> Runtime for Host
where
    Host: SmartRollupCore,
{
    fn write_output(&mut self, output: &[u8]) -> Result<(), RuntimeError> {
        let result_code =
            unsafe { SmartRollupCore::write_output(self, output.as_ptr(), output.len()) };

        match Error::wrap(result_code) {
            Ok(_) => Ok(()),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn write_debug(&self, msg: &str) {
        unsafe { SmartRollupCore::write_debug(self, msg.as_ptr(), msg.len()) };
    }

    #[cfg(feature = "alloc")]
    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        use core::mem::MaybeUninit;
        use tezos_smart_rollup_core::MAX_INPUT_MESSAGE_SIZE;

        let mut buffer = Vec::with_capacity(MAX_INPUT_MESSAGE_SIZE);

        let mut message_info = MaybeUninit::<ReadInputMessageInfo>::uninit();

        let bytes_read = unsafe {
            SmartRollupCore::read_input(
                self,
                message_info.as_mut_ptr(),
                buffer.as_mut_ptr(),
                MAX_INPUT_MESSAGE_SIZE,
            )
        };

        let bytes_read = match Error::wrap(bytes_read) {
            Ok(0) => return Ok(None),
            Ok(size) => size,
            Err(e) => return Err(RuntimeError::HostErr(e)),
        };

        let ReadInputMessageInfo { level, id } = unsafe {
            buffer.set_len(bytes_read);
            message_info.assume_init()
        };

        // level & id are guaranteed to be positive
        let input = Message::new(level as u32, id as u32, buffer);

        Ok(Some(input))
    }

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

        check_path_has_value(self, path)?;

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

            let size = self.store_read_slice(path, from_offset, &mut buffer)?;

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

        let length = Runtime::store_value_size(self, path)?;
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
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        let result_code = unsafe {
            SmartRollupCore::store_write(
                self,
                path.as_ptr(),
                path.size(),
                at_offset,
                src.as_ptr(),
                src.len(),
            )
        };
        match Error::wrap(result_code) {
            Ok(_) => Ok(()),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        check_path_exists(self, path)?;

        let res =
            unsafe { SmartRollupCore::store_delete(self, path.as_ptr(), path.size()) };
        match Error::wrap(res) {
            Ok(_) => Ok(()),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    #[cfg(feature = "proto-nairobi")]
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
        check_path_exists(self, from_path)?;

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
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        check_path_exists(self, from_path)?;

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
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        let res = unsafe {
            SmartRollupCore::reveal_preimage(
                self,
                hash.as_ptr(),
                PREIMAGE_HASH_SIZE,
                buffer.as_mut_ptr(),
                buffer.len(),
            )
        };
        match Error::wrap(res) {
            Ok(size) => Ok(size),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn reveal_metadata(&self) -> RollupMetadata {
        let mut destination = [0u8; METADATA_SIZE];
        let res = unsafe {
            SmartRollupCore::reveal_metadata(
                self,
                destination.as_mut_ptr(),
                destination.len(),
            )
        };

        // Revealing metadata should always succeed
        debug_assert!(res == METADATA_SIZE as i32, "SDK_ERROR: Revealing metadata always succeeds. \
                                             If you see this message, please report it to the \
                                             SDK developers at https://gitlab.com/tezos/tezos");

        RollupMetadata::from(destination)
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        check_path_exists(self, path)?;
        let res = unsafe {
            SmartRollupCore::store_value_size(self, path.as_ptr(), path.size())
        };
        match Error::wrap(res) {
            Ok(size) => Ok(size),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.store_write(&REBOOT_PATH, &[0_u8], 0)
    }

    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        const PATH_STUCK_FLAG: RefPath =
            RefPath::assert_from_readonly(b"/readonly/kernel/env/stuck");
        let last_run_aborted = Runtime::store_has(self, &PATH_STUCK_FLAG)?.is_some();
        Ok(last_run_aborted)
    }

    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        const PATH_UPGRADE_ERROR_FLAG: RefPath =
            RefPath::assert_from_readonly(b"/readonly/kernel/env/upgrade_error");
        let upgrade_failed =
            Runtime::store_has(self, &PATH_UPGRADE_ERROR_FLAG)?.is_some();
        Ok(upgrade_failed)
    }

    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        const PATH_TOO_MANY_REBOOT_FLAG: RefPath =
            RefPath::assert_from_readonly(b"/readonly/kernel/env/too_many_reboot");
        let restart_forced =
            Runtime::store_has(self, &PATH_TOO_MANY_REBOOT_FLAG)?.is_some();
        Ok(restart_forced)
    }

    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        const PATH_REBOOT_COUNTER: RefPath =
            RefPath::assert_from_readonly(b"/readonly/kernel/env/reboot_counter");
        const SIZE: usize = core::mem::size_of::<i32>();

        let mut bytes: [u8; SIZE] = [0; SIZE];
        self.store_read_slice(&PATH_REBOOT_COUNTER, 0, &mut bytes)?;

        let counter = u32::from_le_bytes(bytes);
        Ok(counter)
    }

    #[cfg(feature = "alloc")]
    fn runtime_version(&self) -> Result<String, RuntimeError> {
        const PATH_VERSION: RefPath =
            RefPath::assert_from_readonly(b"/readonly/wasm_version");
        let bytes = Runtime::store_read(self, &PATH_VERSION, 0, 9)?;
        // SAFETY: This storage can only contains valid version string which are utf8 safe.
        let version = unsafe { alloc::string::String::from_utf8_unchecked(bytes) };
        Ok(version)
    }
}

#[cfg(feature = "alloc")]
fn check_path_has_value<T: Path>(
    runtime: &impl Runtime,
    path: &T,
) -> Result<(), RuntimeError> {
    if let Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) =
        runtime.store_has(path)
    {
        Ok(())
    } else {
        Err(RuntimeError::PathNotFound)
    }
}

fn check_path_exists<T: Path>(
    runtime: &impl Runtime,
    path: &T,
) -> Result<(), RuntimeError> {
    if let Ok(Some(_)) = runtime.store_has(path) {
        Ok(())
    } else {
        Err(RuntimeError::PathNotFound)
    }
}

#[cfg(test)]
mod tests {
    use super::{Runtime, RuntimeError, PREIMAGE_HASH_SIZE};
    use crate::{
        input::Message,
        metadata::RollupMetadata,
        path::{OwnedPath, Path, RefPath},
        Error, METADATA_SIZE,
    };
    use std::slice::{from_raw_parts, from_raw_parts_mut};
    use test_helpers::*;
    use tezos_smart_rollup_core::{
        smart_rollup_core::MockSmartRollupCore, MAX_FILE_CHUNK_SIZE,
        MAX_INPUT_MESSAGE_SIZE, MAX_OUTPUT_SIZE,
    };

    const READ_SIZE: usize = 80;

    #[test]
    fn given_output_written_then_ok() {
        // Arrange
        let mut mock = MockSmartRollupCore::new();
        let output = "just a bit of output we want to write";

        mock.expect_write_output()
            .withf(|ptr, len| {
                let slice = unsafe { from_raw_parts(*ptr, *len) };

                output.as_bytes() == slice
            })
            .return_const(0);

        // Act
        let result = mock.write_output(output.as_bytes());

        // Assert
        assert_eq!(Ok(()), result);
    }

    #[test]
    fn given_output_too_large_then_err() {
        // Arrange
        let mut mock = MockSmartRollupCore::new();

        let output = [b'a'; MAX_OUTPUT_SIZE + 1];

        mock.expect_write_output().return_once(|ptr, len| {
            let slice = unsafe { from_raw_parts(ptr, len) };

            assert!(slice.iter().all(|b| b == &b'a'));
            assert_eq!(MAX_OUTPUT_SIZE + 1, slice.len());

            Error::InputOutputTooLarge.code()
        });

        // Act
        let result = mock.write_output(output.as_slice());

        // Assert
        assert_eq!(
            Err(RuntimeError::HostErr(Error::InputOutputTooLarge)),
            result
        );
    }

    #[test]
    fn read_input_returns_none_when_nothing_read() {
        // Arrange
        let mut mock = MockSmartRollupCore::new();
        mock.expect_read_input().return_const(0_i32);

        // Act
        let outcome = mock.read_input();

        // Assert
        assert_eq!(Ok(None), outcome);
    }

    #[test]
    fn read_message_input_with_size_max_bytes() {
        // Arrange
        let level = 5;
        let id = 12908;
        let byte = b'?';
        const FRACTION: usize = 1;

        let mut mock = read_input_with(level, id, byte, FRACTION);

        // Act
        let outcome = mock.read_input();

        // Assert
        let expected = Message::new(
            level,
            id,
            Box::new([byte; MAX_INPUT_MESSAGE_SIZE / FRACTION]).to_vec(),
        );

        assert_eq!(Ok(Some(expected)), outcome);
    }

    #[test]
    fn store_has_existing_return_true() {
        // Arrange
        let mut mock = MockSmartRollupCore::new();
        let existing_path = RefPath::assert_from("/an/Existing/path".as_bytes());

        mock.expect_store_has()
            .withf(move |ptr, size| {
                let bytes = unsafe { from_raw_parts(*ptr, *size) };
                existing_path.as_bytes() == bytes
            })
            .return_const(tezos_smart_rollup_core::VALUE_TYPE_VALUE);

        // Act
        let result = mock.store_has(&existing_path);

        assert!(matches!(result, Ok(Some(_))));
    }

    fn mock_path_not_existing(path_bytes: Vec<u8>) -> MockSmartRollupCore {
        let mut mock = MockSmartRollupCore::new();

        mock.expect_store_has()
            .withf(move |ptr, size| {
                let bytes = unsafe { from_raw_parts(*ptr, *size) };
                path_bytes == bytes
            })
            .return_const(tezos_smart_rollup_core::VALUE_TYPE_NONE);

        mock
    }

    #[test]
    fn store_has_not_existing_returns_false() {
        // Arrange
        let path_bytes = String::from("/does/not.exist").into_bytes();
        let non_existent_path: OwnedPath = RefPath::assert_from(&path_bytes).into();

        let mock = mock_path_not_existing(path_bytes);

        // Act
        let result = mock.store_has(&non_existent_path);

        // Assert
        assert!(matches!(result, Ok(None)));
    }

    #[test]
    fn store_read_max_bytes() {
        // Arrange
        const FRACTION: usize = 1;
        const PATH: RefPath<'static> = RefPath::assert_from("/a/simple/path".as_bytes());
        const OFFSET: usize = 5;

        let mut mock = mock_path_exists(PATH.as_bytes());
        mock.expect_store_read()
            .withf(|path_ptr, path_size, from_offset, _, max_bytes| {
                let slice = unsafe { from_raw_parts(*path_ptr, *path_size) };

                READ_SIZE == *max_bytes
                    && PATH.as_bytes() == slice
                    && OFFSET == *from_offset
            })
            .return_once(|_, _, _, buf_ptr, _| {
                let stored_bytes = [b'2'; READ_SIZE / FRACTION];
                let buffer = unsafe { from_raw_parts_mut(buf_ptr, READ_SIZE / FRACTION) };
                buffer.copy_from_slice(&stored_bytes);
                (READ_SIZE / FRACTION).try_into().unwrap()
            });

        // Act
        let result = mock.store_read(&PATH, OFFSET, READ_SIZE);

        // Assert
        let expected = std::iter::repeat(b'2').take(READ_SIZE / FRACTION).collect();

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn store_read_lt_max_bytes() {
        // Arrange
        const FRACTION: usize = 5;
        const PATH: RefPath<'static> = RefPath::assert_from("/a/simple/path".as_bytes());
        const OFFSET: usize = 10;

        let mut mock = mock_path_exists(PATH.as_bytes());
        mock.expect_store_read()
            .withf(|path_ptr, path_size, from_offset, _, max_bytes| {
                let slice = unsafe { from_raw_parts(*path_ptr, *path_size) };

                READ_SIZE == *max_bytes
                    && PATH.as_bytes() == slice
                    && OFFSET == *from_offset
            })
            .return_once(|_, _, _, buf_ptr, _| {
                let stored_bytes = [b'Z'; READ_SIZE / FRACTION];
                let buffer = unsafe { from_raw_parts_mut(buf_ptr, READ_SIZE / FRACTION) };
                buffer.copy_from_slice(&stored_bytes);
                (READ_SIZE / FRACTION).try_into().unwrap()
            });

        // Act
        let result = mock.store_read(&PATH, OFFSET, READ_SIZE);

        // Assert
        let expected = std::iter::repeat(b'Z').take(READ_SIZE / FRACTION).collect();

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn store_read_path_not_found() {
        // Arrange
        let bytes = "/a/2nd/PATH.which/doesnt/exist".as_bytes().to_vec();
        let path: OwnedPath = RefPath::assert_from(&bytes).into();
        let offset = 25;

        let mock = mock_path_not_existing(bytes);

        // Act
        let result = mock.store_read(&path, offset, READ_SIZE);

        // Assert
        assert_eq!(Err(RuntimeError::PathNotFound), result);
    }

    #[test]
    fn store_read_all_above_max_file_chunk_size() {
        // Arrange

        // The value read is formed of 3 chunks, two of the max chunk value and
        // the last one being less than the max size.
        const PATH: RefPath<'static> = RefPath::assert_from("/a/simple/path".as_bytes());
        const VALUE_FIRST_CHUNK: [u8; MAX_FILE_CHUNK_SIZE] = [b'a'; MAX_FILE_CHUNK_SIZE];
        const VALUE_SECOND_CHUNK: [u8; MAX_FILE_CHUNK_SIZE] = [b'b'; MAX_FILE_CHUNK_SIZE];
        const VALUE_LAST_CHUNK: [u8; MAX_FILE_CHUNK_SIZE / 2] =
            [b'c'; MAX_FILE_CHUNK_SIZE / 2];
        const VALUE_SIZE: usize =
            VALUE_FIRST_CHUNK.len() + VALUE_SECOND_CHUNK.len() + VALUE_LAST_CHUNK.len();

        let mut mock = mock_path_exists(PATH.as_bytes());
        // Check that the path is the one given to `store_read_all` and the
        // offset is always a multiple of `MAX_FILE_CHUNK_SIZE`.
        mock.expect_store_read()
            .withf(|path_ptr, path_size, offset, _, max_bytes| {
                let slice = unsafe { from_raw_parts(*path_ptr, *path_size) };
                let last_offset = MAX_FILE_CHUNK_SIZE * 2;
                let expected_max_bytes = if offset == &last_offset {
                    VALUE_LAST_CHUNK.len()
                } else {
                    MAX_FILE_CHUNK_SIZE
                };

                (offset % MAX_FILE_CHUNK_SIZE) == 0
                    && &expected_max_bytes == max_bytes
                    && PATH.as_bytes() == slice
            })
            // Returns the expected chunks, as we know the offsets are consistent.
            .returning(|_, _, offset, buf_ptr, _| {
                let chunk = if offset == 0 {
                    VALUE_FIRST_CHUNK.to_vec()
                } else if offset == MAX_FILE_CHUNK_SIZE {
                    VALUE_SECOND_CHUNK.to_vec()
                } else {
                    VALUE_LAST_CHUNK.to_vec()
                };
                let buffer = unsafe { from_raw_parts_mut(buf_ptr, chunk.len()) };
                buffer.copy_from_slice(&chunk);
                (chunk.len()).try_into().unwrap()
            });

        mock.expect_store_value_size()
            .return_const(i32::try_from(VALUE_SIZE).unwrap());

        // Act
        let result = Runtime::store_read_all(&mock, &PATH);

        // Assert
        let mut expected: Vec<u8> = Vec::new();
        expected.extend_from_slice(&VALUE_FIRST_CHUNK);
        expected.extend_from_slice(&VALUE_SECOND_CHUNK);
        expected.extend_from_slice(&VALUE_LAST_CHUNK);

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn store_write_ok() {
        // Arrange
        const PATH: RefPath<'static> = RefPath::assert_from("/a/simple/path".as_bytes());
        const OUTPUT: &[u8] = "One two three four five".as_bytes();
        const OFFSET: usize = 12398;

        let mut mock = MockSmartRollupCore::new();
        mock.expect_store_write()
            .withf(|path_ptr, path_size, at_offset, src_ptr, src_size| {
                let path_slice = unsafe { from_raw_parts(*path_ptr, *path_size) };
                let output_slice = unsafe { from_raw_parts(*src_ptr, *src_size) };

                OUTPUT == output_slice
                    && PATH.as_bytes() == path_slice
                    && OFFSET == *at_offset
            })
            .return_const(0);

        // Act
        let result = mock.store_write(&PATH, OUTPUT, OFFSET);

        // Assert
        assert_eq!(Ok(()), result);
    }

    #[test]
    fn store_write_too_large() {
        // Arrange
        const PATH: RefPath<'static> = RefPath::assert_from("/a/simple/path".as_bytes());
        const OUTPUT: &[u8] = "once I saw a fish alive".as_bytes();
        const OFFSET: usize = 0;

        let mut mock = MockSmartRollupCore::new();
        mock.expect_store_write()
            .withf(|path_ptr, path_size, at_offset, src_ptr, src_size| {
                let path_slice = unsafe { from_raw_parts(*path_ptr, *path_size) };
                let output_slice = unsafe { from_raw_parts(*src_ptr, *src_size) };

                OUTPUT == output_slice
                    && PATH.as_bytes() == path_slice
                    && OFFSET == *at_offset
            })
            .return_const(Error::InputOutputTooLarge.code());

        // Act
        let result = mock.store_write(&PATH, OUTPUT, OFFSET);

        // Assert
        assert_eq!(
            Err(RuntimeError::HostErr(Error::InputOutputTooLarge)),
            result
        );
    }

    #[test]
    fn store_delete() {
        // Arrange
        const PATH: RefPath<'static> =
            RefPath::assert_from("/a/2nd/PATH.which/does/exist".as_bytes());

        let mut mock = mock_path_exists(PATH.as_bytes());
        mock.expect_store_delete()
            .withf(|ptr, size| {
                let slice = unsafe { from_raw_parts(*ptr, *size) };

                PATH.as_bytes() == slice
            })
            .return_const(0);

        // Act
        let result = mock.store_delete(&PATH);

        // Assert
        assert_eq!(Ok(()), result);
    }

    #[test]
    fn store_delete_path_not_found() {
        // Arrange
        let bytes = String::from("/a/2nd/PATH.which/doesnt/exist").into_bytes();
        let path: OwnedPath = RefPath::assert_from(&bytes).into();

        let mut mock = mock_path_not_existing(bytes);

        // Act
        let result = mock.store_delete(&path);

        // Assert
        assert_eq!(Err(RuntimeError::PathNotFound), result);
    }

    #[test]
    #[cfg(feature = "proto-nairobi")]
    fn store_delete_value() {
        // Arrange
        const PATH: RefPath<'static> =
            RefPath::assert_from("/a/PATH.which/does/exist".as_bytes());

        const REMAINING_PATH: RefPath<'static> =
            RefPath::assert_from("/a/PATH.which/does/exist/and/survived".as_bytes());

        let mut mock = mock_path_exists(PATH.as_bytes());
        mock.expect_store_delete()
            .withf(|ptr, size| {
                let slice = unsafe { from_raw_parts(*ptr, *size) };

                PATH.as_bytes() == slice
            })
            .return_const(0);
        mock.expect_store_has()
            .withf(move |ptr, size| {
                let bytes = unsafe { from_raw_parts(*ptr, *size) };
                REMAINING_PATH.as_bytes() == bytes
            })
            .return_const(tezos_smart_rollup_core::VALUE_TYPE_VALUE);

        // Act
        let result = mock.store_delete(&PATH);
        let result_remaining = mock.store_has(&REMAINING_PATH);

        // Assert
        assert_eq!(Ok(()), result);
        assert!(matches!(result_remaining, Ok(Some(_))));
    }

    #[test]
    fn store_count_subkeys() {
        // Arrange
        const PATH: RefPath<'static> =
            RefPath::assert_from("/prefix/of/other/keys".as_bytes());

        let subkey_count = 14;

        let mut mock = MockSmartRollupCore::new();

        mock.expect_store_list_size()
            .withf(|ptr, size| {
                let slice = unsafe { from_raw_parts(*ptr, *size) };

                PATH.as_bytes() == slice
            })
            .return_const(subkey_count);

        // Act
        let result = mock.store_count_subkeys(&PATH);

        // Assert
        assert_eq!(Ok(subkey_count.try_into().unwrap()), result);
    }

    #[test]
    fn reveal_preimage_ok() {
        let mut mock = MockSmartRollupCore::new();

        mock.expect_reveal_preimage()
            .withf(|hash_addr, hash_len, _dest_addr, max_bytes| {
                let hash = unsafe { from_raw_parts(*hash_addr, *hash_len) };
                hash_len == &PREIMAGE_HASH_SIZE
                    && hash == [5; PREIMAGE_HASH_SIZE]
                    && *max_bytes == 55
            })
            .return_once(|_, _, destination_address, _| {
                let revealed_bytes = [b'!'; 50];
                let buffer = unsafe { from_raw_parts_mut(destination_address, 50) };
                buffer.copy_from_slice(&revealed_bytes);
                50
            });
        let mut buffer = [0; 55];
        // Act
        let result =
            mock.reveal_preimage(&[5; PREIMAGE_HASH_SIZE], buffer.as_mut_slice());

        // Assert
        assert_eq!(Ok(50), result);
    }

    #[test]
    fn store_value_size() {
        let mut mock = MockSmartRollupCore::new();
        const PATH: RefPath<'static> = RefPath::assert_from(b"/prefix/of/other/paths");
        let size = 256_usize;
        mock.expect_store_has()
            .return_const(tezos_smart_rollup_core::VALUE_TYPE_VALUE);
        mock.expect_store_value_size()
            .return_const(i32::try_from(size).unwrap());
        let value_size = mock.store_value_size(&PATH);
        assert_eq!(size, value_size.unwrap());
    }

    #[test]
    fn store_value_size_path_not_found() {
        let mut mock = MockSmartRollupCore::new();
        const PATH: RefPath<'static> = RefPath::assert_from(b"/prefix/of/other/paths");
        mock.expect_store_has()
            .return_const(tezos_smart_rollup_core::VALUE_TYPE_NONE);

        assert_eq!(
            Err(RuntimeError::PathNotFound),
            mock.store_value_size(&PATH)
        );
    }

    #[test]
    fn reveal_metadata_ok() {
        let mut mock = MockSmartRollupCore::new();
        let metadata_bytes = [
            // sr1 as 20 bytes
            b'M', 165, 28, b']', 231, 161, 205, 212, 148, 193, b'[', b'S', 129, b'^', 31,
            170, b'L', 26, 150, 202, // origination level as 4 bytes
            0, 0, 0, 42,
        ];
        let expected_metadata = RollupMetadata::from(metadata_bytes);

        mock.expect_reveal_metadata()
            .return_once(move |destination_address, _| {
                let buffer =
                    unsafe { from_raw_parts_mut(destination_address, METADATA_SIZE) };
                buffer.copy_from_slice(&metadata_bytes.clone());
                METADATA_SIZE as i32
            });

        // Act
        let result = mock.reveal_metadata();

        // Assert
        assert_eq!(expected_metadata, result);
    }

    mod test_helpers {
        use tezos_smart_rollup_core::smart_rollup_core::ReadInputMessageInfo;
        use tezos_smart_rollup_core::MAX_INPUT_MESSAGE_SIZE;

        use super::MockSmartRollupCore;
        use std::slice::{from_raw_parts, from_raw_parts_mut};

        pub fn mock_path_exists(path_bytes: &'static [u8]) -> MockSmartRollupCore {
            let mut mock = MockSmartRollupCore::new();

            mock.expect_store_has()
                .withf(move |ptr, size| {
                    let bytes = unsafe { from_raw_parts(*ptr, *size) };
                    path_bytes == bytes
                })
                .return_const(tezos_smart_rollup_core::VALUE_TYPE_VALUE);

            mock
        }

        pub fn read_input_with(
            level: u32,
            id: u32,
            fill_with: u8,
            fill_fraction: usize,
        ) -> MockSmartRollupCore {
            let mut mock = MockSmartRollupCore::new();

            let write_bytes = MAX_INPUT_MESSAGE_SIZE / fill_fraction;

            let input_bytes = std::iter::repeat(fill_with)
                .take(write_bytes)
                .collect::<Box<_>>();

            mock.expect_read_input().return_once(
                move |message_info_arg, buffer_arg, max_bytes_arg| {
                    assert_eq!(max_bytes_arg, MAX_INPUT_MESSAGE_SIZE);

                    unsafe {
                        std::ptr::write(
                            message_info_arg,
                            ReadInputMessageInfo {
                                level: level as i32,
                                id: id as i32,
                            },
                        );
                        let buffer = from_raw_parts_mut(buffer_arg, write_bytes);
                        buffer.copy_from_slice(input_bytes.as_ref());
                    }
                    write_bytes.try_into().unwrap()
                },
            );

            mock
        }
    }
}
