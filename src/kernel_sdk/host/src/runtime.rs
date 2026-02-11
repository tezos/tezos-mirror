// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Definition of **Runtime** api that is callable from *safe* rust.
//!
//! Includes blanket implementation for all types implementing [SmartRollupCore].

pub mod unwindable;

use tezos_smart_rollup_core::SmartRollupCore;

use crate::debug::HostDebug;
use crate::reveal::HostReveal;
use crate::storage::StorageV1;
use crate::wasm::WasmHost;
use crate::Error;

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

/// Returned by [`StorageV1::store_has`] - specifies whether a path has a value or is a prefix.
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
pub trait Runtime: HostDebug + HostReveal + StorageV1 + WasmHost {}

impl<Host> Runtime for Host where Host: SmartRollupCore {}

#[cfg(test)]
mod tests {
    use super::RuntimeError;
    use crate::{dal_parameters::RollupDalParameters, DAL_PARAMETERS_SIZE};
    use crate::{
        input::Message,
        metadata::RollupMetadata,
        path::{OwnedPath, Path, RefPath},
        reveal::HostReveal,
        storage::StorageV1,
        wasm::WasmHost,
        Error, METADATA_SIZE,
    };
    use std::slice::{from_raw_parts, from_raw_parts_mut};
    use test_helpers::*;
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
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
            [byte; MAX_INPUT_MESSAGE_SIZE / FRACTION].to_vec(),
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

        let path_bytes_delete = path_bytes.clone();
        let path_bytes_read = path_bytes.clone();
        let path_bytes_has = path_bytes.clone();

        mock.expect_store_delete()
            .withf(move |ptr, size| {
                let bytes = unsafe { from_raw_parts(*ptr, *size) };
                path_bytes_delete == bytes
            })
            .return_const(tezos_smart_rollup_core::STORE_NOT_A_NODE);

        mock.expect_store_read()
            .withf(move |ptr, size, _, _, _| {
                let bytes = unsafe { from_raw_parts(*ptr, *size) };
                path_bytes_read == bytes
            })
            .return_const(tezos_smart_rollup_core::STORE_NOT_A_NODE);

        mock.expect_store_has()
            .withf(move |ptr, size| {
                let bytes = unsafe { from_raw_parts(*ptr, *size) };
                path_bytes_has == bytes
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
        let expected = std::iter::repeat_n(b'2', READ_SIZE / FRACTION).collect();

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
        let expected = std::iter::repeat_n(b'Z', READ_SIZE / FRACTION).collect();

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
        let result = StorageV1::store_read_all(&mock, &PATH);

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
    fn store_write_all() {
        const PATH: RefPath<'static> = RefPath::assert_from("/a/simple/path".as_bytes());
        const VALUE_FIRST_CHUNK: [u8; MAX_FILE_CHUNK_SIZE] = [b'a'; MAX_FILE_CHUNK_SIZE];
        const VALUE_SECOND_CHUNK: [u8; MAX_FILE_CHUNK_SIZE] = [b'b'; MAX_FILE_CHUNK_SIZE];
        const VALUE_LAST_CHUNK: [u8; MAX_FILE_CHUNK_SIZE / 2] =
            [b'c'; MAX_FILE_CHUNK_SIZE / 2];

        let mut mock = MockSmartRollupCore::new();
        mock.expect_store_delete_value()
            .withf(|path_ptr, path_size| {
                let path = unsafe { from_raw_parts(*path_ptr, *path_size) };
                path == PATH.as_bytes()
            })
            .return_const(0);

        mock.expect_store_write()
            .withf(|path_ptr, path_size, at_offset, src_ptr, src_size| {
                let path_slice = unsafe { from_raw_parts(*path_ptr, *path_size) };
                let output_slice = unsafe { from_raw_parts(*src_ptr, *src_size) };
                // Store_write_all should always write maximum size of chunk per
                // maximum size of chunk
                let correct_value = if *at_offset == 0 {
                    output_slice == VALUE_FIRST_CHUNK
                } else if *at_offset == MAX_FILE_CHUNK_SIZE {
                    output_slice == VALUE_SECOND_CHUNK
                } else if *at_offset == MAX_FILE_CHUNK_SIZE * 2 {
                    output_slice == VALUE_LAST_CHUNK
                } else {
                    false
                };

                correct_value && PATH.as_bytes() == path_slice
            })
            .return_const(0);

        mock.expect_store_read()
            .withf(|path_ptr, path_size, _, _, _| {
                let slice = unsafe { from_raw_parts(*path_ptr, *path_size) };
                PATH.as_bytes() == slice
            })
            .returning(|_, _, offset, buf_ptr, _| {
                let chunk = if offset < MAX_FILE_CHUNK_SIZE {
                    VALUE_FIRST_CHUNK.to_vec()
                } else if offset < MAX_FILE_CHUNK_SIZE * 2 {
                    VALUE_SECOND_CHUNK.to_vec()
                } else {
                    VALUE_LAST_CHUNK.to_vec()
                };
                let buffer = unsafe { from_raw_parts_mut(buf_ptr, chunk.len()) };
                buffer.copy_from_slice(&chunk);
                (chunk.len()).try_into().unwrap()
            });

        mock.expect_store_value_size().return_const(
            i32::try_from(MAX_FILE_CHUNK_SIZE * 2 + MAX_FILE_CHUNK_SIZE / 2).unwrap(),
        );

        mock.expect_store_has().return_const(1_i32);

        // Act

        let mut value: Vec<u8> = Vec::new();
        value.extend_from_slice(&VALUE_FIRST_CHUNK);
        value.extend_from_slice(&VALUE_SECOND_CHUNK);
        value.extend_from_slice(&VALUE_LAST_CHUNK);
        let result = mock.store_write_all(&PATH, &value);
        let result_read = StorageV1::store_read_all(&mock, &PATH).unwrap();

        // Assert
        assert_eq!(Ok(()), result);
        assert_eq!(value.len(), result_read.len());
        assert_eq!(value, result_read);
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
        mock.expect_store_value_size()
            .return_const(tezos_smart_rollup_core::STORE_NOT_A_VALUE);
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

    #[test]
    fn reveal_dal_parameters_ok() {
        // Arrange
        let expected_dal_parameters = RollupDalParameters {
            number_of_slots: 1122,
            attestation_lag: 3344,
            slot_size: 5566,
            page_size: 7788,
        };
        let mut mock = MockSmartRollupCore::new();
        let dal_parameters_bytes = [
            0, 0, 0, 0, 0, 0, 4, 98, 0, 0, 0, 0, 0, 0, 13, 16, 0, 0, 0, 0, 0, 0, 21, 190,
            0, 0, 0, 0, 0, 0, 30, 108,
        ];
        mock.expect_reveal()
            .return_once(move |_, _, destination_address, _| {
                let buffer = unsafe {
                    from_raw_parts_mut(destination_address, DAL_PARAMETERS_SIZE)
                };
                buffer.copy_from_slice(&dal_parameters_bytes.clone());
                DAL_PARAMETERS_SIZE as i32
            });

        // Act
        let result = mock.reveal_dal_parameters();

        // Assert
        assert_eq!(expected_dal_parameters, result);
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

            let input_bytes =
                std::iter::repeat_n(fill_with, write_bytes).collect::<Box<_>>();

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
