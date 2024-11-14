// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Definition of the `smart_rollup_core` host functions.
//!
//! Links directly to the module provided by the smart rollup system, when
//! targetting `wasm32-unknown-unknown`.

#[cfg(not(target_arch = "riscv64"))]
#[link(wasm_import_module = "smart_rollup_core")]
extern "C" {
    /// If `/input/consumed >= /input/bytes`, return `0`.
    ///
    /// Otherwise:
    /// - Fills the given buffer with up to `max_bytes` and returns actual
    ///   number written.
    /// - Sets the `level` and `id` of the message to `message_info`.
    pub fn read_input(
        message_info: *mut ReadInputMessageInfo,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32;

    /// Write the given number of bytes to output.
    ///
    /// Returns [`INPUT_OUTPUT_TOO_LARGE`] if output size is greater than
    /// [`MAX_OUTPUT_SIZE`].
    ///
    /// [`INPUT_OUTPUT_TOO_LARGE`]: crate::INPUT_OUTPUT_TOO_LARGE
    /// [`MAX_OUTPUT_SIZE`]: crate::MAX_OUTPUT_SIZE
    pub fn write_output(src: *const u8, num_bytes: usize) -> i32;

    /// Write the given number of bytes to the debug log.
    pub fn write_debug(src: *const u8, num_bytes: usize);

    /// Return whether the given path exists in durable storage.
    pub fn store_has(path: *const u8, path_len: usize) -> i32;

    /// Read up to `num_bytes` bytes from the value at `path` into memory.
    ///
    /// Returns the number of bytes copied to memory. The bytes read from storage begin
    /// at `offset`.
    pub fn store_read(
        path: *const u8,
        path_len: usize,
        offset: usize,
        dst: *mut u8,
        num_bytes: usize,
    ) -> i32;

    /// Write the given number of bytes from memory to the given key, starting at `offset`.
    ///
    /// Returns [`INPUT_OUTPUT_TOO_LARGE`] if output size is greater than
    /// [`MAX_FILE_CHUNK_SIZE`].
    ///
    /// [`INPUT_OUTPUT_TOO_LARGE`]: crate::INPUT_OUTPUT_TOO_LARGE
    /// [`MAX_FILE_CHUNK_SIZE`]: crate::MAX_FILE_CHUNK_SIZE
    pub fn store_write(
        path: *const u8,
        path_len: usize,
        offset: usize,
        src: *const u8,
        num_bytes: usize,
    ) -> i32;

    /// Delete the value and/or subkeys of `path` from storage.
    pub fn store_delete(path: *const u8, path_len: usize) -> i32;

    /// Delete the value of `path` from storage.
    pub fn store_delete_value(path: *const u8, path_len: usize) -> i32;

    /// Get the number of subkeys of the prefix given by `path`.
    pub fn store_list_size(path: *const u8, path_len: usize) -> i64;

    /// Moves the value and/or subkeys of `from_path` to `to_path`.
    ///
    /// Overwrites the destination, if it already exists.
    ///
    /// e.g. if we have a store with `/x/y/z` containing a value
    /// `store_move /x/y /a/b`
    /// results in a store with `/a/b/z` as a location with a value
    /// (location `/x/y/z` will hold no value after the call to
    /// `store_move`).
    pub fn store_move(
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32;

    /// Copies the value and/or subkeys of `from_path` to `to_path`.
    ///
    /// Overwrites the destination, if it already exists.
    ///
    /// e.g. if we have a store with `/x/y/z` containing a value
    /// `store_copy /x/y /a/b`
    /// results in a store with `/x/y/z; /a/b/z` as locations with values.
    pub fn store_copy(
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32;

    /// Loads the preimage of a preimage-hash to memory.
    ///
    /// If the preimage is larger than `max_bytes`, its contents is trimmed.
    pub fn reveal_preimage(
        hash_addr: *const u8,
        hash_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32;

    /// Return the size (in bytes) of the value stored at `path`.
    pub fn store_value_size(path: *const u8, path_len: usize) -> i32;

    /// Loads the rollup metadata into memory.
    ///
    /// This is made of the:
    /// - 20-byte address of the rollup.
    /// - 4-byte origination level.
    ///
    /// Returns the size of data read (will always be 24).
    pub fn reveal_metadata(destination_addr: *mut u8, max_bytes: usize) -> i32;

    /// Loads the result of a raw reveal request to memory.
    ///
    /// If the result is larger than `max_bytes`, its contents is trimmed.
    ///
    /// The encoding of the request as stored in the buffer described by `payload_addr` and
    /// `payload_len` is the same as the one used by the Tezos protocol.
    ///
    /// Returns the size of the data loaded in memory.
    pub fn reveal(
        payload_addr: *const u8,
        payload_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32;
}

/// Wrapper trait for `smart_rollup_core` host functions.
///
/// Parameterised by `&self` - note that while
/// these function may cause side effects, they are unsafe to call.
///
/// While [`RollupHost`] is the implementor used when running in a smart-rollup,
/// alternative hosts can be used - for example in unit tests.
///
/// # Safety
/// The caller should take care to give correct buffer sizes, pointers, and
/// path-encodings.  See safety notes on each method for more details.
///
/// # Error handling
/// Most functions in *SmartRollupCore* return `i32`. When used correctly, this
/// result will tend to be positive (for example to represent the number of
/// bytes read/written to/from memory.)
///
/// There may be situtations where a host function does return an error (for
/// example `store_read` on a non-existent value would return an error). As
/// a result, you should be careful to check for possible errors after calling
/// host functions.
///
/// A full list of error codes is given in the [top-level](./lib.rs).
///
/// [`RollupHost`]: crate::rollup_host::RollupHost
#[cfg_attr(feature = "testing", mockall::automock)]
pub unsafe trait SmartRollupCore {
    /// See [read_input].
    ///
    /// # Safety
    /// - `message_info` must all be valid pointer to a `ReadInputMessageInfo`.
    /// - `dst` must point to a mutable slice of bytes with `capacity >= max_bytes`.
    ///
    /// If `return > 0`, the `message_info` & `dst` are guaranteed to be initialised.
    unsafe fn read_input(
        &self,
        message_info: *mut ReadInputMessageInfo,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32;

    /// See [write_output].
    ///
    /// # Safety
    /// - `src` must be a ptr to an initialised slice of bytes.
    /// - `num_bytes` must be the length of that slice.
    unsafe fn write_output(&self, src: *const u8, num_bytes: usize) -> i32;

    /// See [write_debug].
    ///
    /// # Safety
    /// - `src` must be a ptr to an initialised slice of bytes.
    /// - `num_bytes` must be the length of that slice.
    unsafe fn write_debug(&self, src: *const u8, num_bytes: usize);

    /// See [store_has].
    ///
    /// # Safety
    /// - `path` must be a ptr to a correctly path-encoded slice of bytes.
    /// - `path_len` must be the length of that slice.
    unsafe fn store_has(&self, path: *const u8, path_len: usize) -> i32;

    /// See [store_read].
    ///
    /// # Safety
    /// - `path` must be a ptr to a correctly path-encoded slice of bytes.
    /// - `path_len` must be the length of that slice.
    /// - `dst` must point to a mutable slice of bytes with `capacity >= max_bytes`.
    unsafe fn store_read(
        &self,
        path: *const u8,
        path_len: usize,
        offset: usize,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32;

    /// See [store_write].
    ///
    /// # Safety
    /// - `path` must be a ptr to a correctly path-encoded slice of bytes.
    /// - `path_len` must be the length of that slice.
    /// - `dst` must point to a slice of bytes with `length >= num_bytes`.
    unsafe fn store_write(
        &self,
        path: *const u8,
        path_len: usize,
        offset: usize,
        src: *const u8,
        num_bytes: usize,
    ) -> i32;

    /// See [store_delete].
    ///
    /// # Safety
    /// - `path` must be a ptr to a correctly path-encoded slice of bytes.
    /// - `len` must be the length of that slice.
    unsafe fn store_delete(&self, path: *const u8, len: usize) -> i32;

    /// See [store_delete_value].
    ///
    /// # Safety
    /// - `path` must be a ptr to a correctly path-encoded slice of bytes.
    /// - `len` must be the length of that slice.
    unsafe fn store_delete_value(&self, path: *const u8, len: usize) -> i32;

    /// See [store_list_size].
    ///
    /// # Safety
    /// - `path` must be a ptr to a correctly path-encoded slice of bytes.
    /// - `path_len` must be the length of that slice.
    unsafe fn store_list_size(&self, path: *const u8, path_len: usize) -> i64;

    /// See [store_move] above.
    ///
    /// # Safety
    /// - `from_path` and `to_path` must be pointers to correctly path encoded slices
    ///   bytes.
    /// - `from_path_len` and `to_path_len` must be length of those slices respectively.
    unsafe fn store_move(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32;

    /// See [store_copy] above.
    ///
    /// # Safety
    /// - `from_path` and `to_path` must be pointers to correctly path encoded slices
    ///   bytes.
    /// - `from_path_len` and `to_path_len` must be length of those slices respectively.
    unsafe fn store_copy(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32;

    /// Loads the preimage of a given hash of size `PREIMAGE_HASH_BYTES` in memory.
    /// If the preimage is larger than `max_bytes`, its contents is trimmed.
    ///
    /// # Safety
    /// - `hash_addr` must be a ptr to a slice containing a hash.
    /// - `hash_len` must be the length of the slice.
    /// - `destination_addr `must point to a mutable slice of bytes with
    ///   `capacity >= max_size`.
    unsafe fn reveal_preimage(
        &self,
        hash_addr: *const u8,
        hash_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32;

    /// See [store_value_size] above.
    ///
    /// # Safety
    /// - `path` must be a ptr to a correctly path-encoded slice of bytes.
    /// - `path_len` must be the length of that slice.
    unsafe fn store_value_size(&self, path: *const u8, path_len: usize) -> i32;

    /// Loads the 24-byte metedata (20-byte address of the rollup followed by the 4-byte
    /// origination level) into memory.
    ///
    /// # Safety
    /// - `destination_addr` must point to a mutable slice of bytes with
    ///   `capacity >= max_bytes`
    unsafe fn reveal_metadata(&self, destination_addr: *mut u8, max_bytes: usize) -> i32;

    /// Loads the result of a raw reveal request to memory.
    /// If the preimage is larger than `max_bytes`, its contents is trimmed.
    ///
    /// # Safety
    /// - `payload_addr` must be a ptr to a slice containing a hash.
    /// - `payload_len` must be the length of the slice.
    /// - `destination_addr `must point to a mutable slice of bytes with
    ///   `capacity >= max_bytes`.
    unsafe fn reveal(
        &self,
        payload_addr: *const u8,
        payload_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32;
}

/// Information about message level & id.
#[repr(C)]
pub struct ReadInputMessageInfo {
    /// The current inbox level.
    pub level: i32,
    /// The message id at the current level.
    pub id: i32,
}

#[cfg(all(target_arch = "riscv64", target_os = "hermit", feature = "proto-alpha"))]
mod riscv64_hermit {
    extern crate std;

    use crate::smart_rollup_core::ReadInputMessageInfo;
    use std::{
        io::{self, Write},
        slice::from_raw_parts,
    };
    use tezos_smart_rollup_constants::{
        core::{
            GENERIC_INVALID_ACCESS, MEMORY_INVALID_ACCESS, METADATA_LENGTH,
            ORIGINATION_LEVEL_LENGTH, ROLLUP_ADDRESS_LENGTH,
        },
        riscv::{
            SbiError, SBI_FIRMWARE_TEZOS, SBI_TEZOS_INBOX_NEXT, SBI_TEZOS_METADATA_REVEAL,
        },
    };

    /// Check the SBI return value for errors.
    fn check_sbi_result(result: isize) -> Result<usize, i32> {
        match SbiError::from_result(result) {
            None => Ok(result as usize),

            // The SBI call was not supported. This is a fatal error.
            Some(SbiError::NotSupported) => panic!("SBI call not supported"),

            // Indicates a bad address or memory access.
            Some(SbiError::InvalidAddress) => Err(MEMORY_INVALID_ACCESS),

            // Uncategorised error.
            Some(_) => Err(GENERIC_INVALID_ACCESS),
        }
    }

    pub unsafe fn read_input(
        message_info: *mut ReadInputMessageInfo,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        let message_info = &mut *message_info;
        let result: isize;

        // SBI call
        //   extension = SBI_FIRMWARE_TEZOS
        //   function = SBI_TEZOS_INBOX_NEXT
        core::arch::asm!(
            "ecall",
            in("a0") dst,
            in("a1") max_bytes,
            in("a2") &mut message_info.level,
            in("a3") &mut message_info.id,
            in("a6") SBI_TEZOS_INBOX_NEXT,
            in("a7") SBI_FIRMWARE_TEZOS,
            lateout("a0") result,
        );

        match check_sbi_result(result) {
            Ok(result) => result as i32,
            Err(err) => err,
        }
    }

    pub unsafe fn write_output(_src: *const u8, _num_bytes: usize) -> i32 {
        unimplemented!()
    }

    pub unsafe fn write_debug(src: *const u8, num_bytes: usize) {
        let buffer = unsafe { from_raw_parts(src, num_bytes) };
        io::stdout()
            .write_all(buffer)
            .expect("Writing to stdout failed");
    }

    pub unsafe fn store_has(_path: *const u8, _path_len: usize) -> i32 {
        unimplemented!()
    }

    pub unsafe fn store_read(
        _path: *const u8,
        _path_len: usize,
        _offset: usize,
        _dst: *mut u8,
        _max_bytes: usize,
    ) -> i32 {
        unimplemented!()
    }

    pub unsafe fn store_write(
        _path: *const u8,
        _path_len: usize,
        _offset: usize,
        _src: *const u8,
        _num_bytes: usize,
    ) -> i32 {
        unimplemented!()
    }

    pub unsafe fn store_delete(_path: *const u8, _len: usize) -> i32 {
        unimplemented!()
    }

    pub unsafe fn store_delete_value(_path: *const u8, _len: usize) -> i32 {
        unimplemented!()
    }

    pub unsafe fn store_list_size(_path: *const u8, _path_len: usize) -> i64 {
        unimplemented!()
    }

    pub unsafe fn store_move(
        _from_path: *const u8,
        _from_path_len: usize,
        _to_path: *const u8,
        _to_path_len: usize,
    ) -> i32 {
        unimplemented!()
    }

    pub unsafe fn store_copy(
        _from_path: *const u8,
        _from_path_len: usize,
        _to_path: *const u8,
        _to_path_len: usize,
    ) -> i32 {
        unimplemented!()
    }

    pub unsafe fn reveal_preimage(
        _hash_addr: *const u8,
        _hash_len: usize,
        _destination_addr: *mut u8,
        _max_bytes: usize,
    ) -> i32 {
        unimplemented!()
    }

    pub unsafe fn reveal(
        _payload_addr: *const u8,
        _payload_len: usize,
        _destination_addr: *mut u8,
        _max_bytes: usize,
    ) -> i32 {
        unimplemented!()
    }

    pub unsafe fn store_value_size(_path: *const u8, _path_len: usize) -> i32 {
        unimplemented!()
    }

    pub unsafe fn reveal_metadata(buffer: *mut u8, max_bytes: usize) -> i32 {
        let mut sbi_buffer = [0u8; METADATA_LENGTH];
        let origin_level = {
            let result: isize;

            // SBI call
            //   extension = SBI_FIRMWARE_TEZOS
            //   function = SBI_TEZOS_METADATA_REVEAL
            core::arch::asm!(
                "ecall",
                in("a0") sbi_buffer.as_mut_ptr(),
                in("a6") SBI_TEZOS_METADATA_REVEAL,
                in("a7") SBI_FIRMWARE_TEZOS,
                lateout("a0") result,
            );

            match check_sbi_result(result) {
                Err(err) => return err,
                Ok(result) => result,
            }
        };

        match ORIGINATION_LEVEL_LENGTH {
            4 => {
                sbi_buffer[ROLLUP_ADDRESS_LENGTH
                    ..(ROLLUP_ADDRESS_LENGTH + ORIGINATION_LEVEL_LENGTH)]
                    .copy_from_slice(&(origin_level as i32).to_be_bytes());
            }

            8 => {
                sbi_buffer[ROLLUP_ADDRESS_LENGTH
                    ..(ROLLUP_ADDRESS_LENGTH + ORIGINATION_LEVEL_LENGTH)]
                    .copy_from_slice(&(origin_level as i64).to_be_bytes());
            }

            _ => {
                panic!("Unknown origination level type length {ORIGINATION_LEVEL_LENGTH}")
            }
        }

        let written = max_bytes.min(sbi_buffer.len());
        buffer.copy_from(sbi_buffer.as_mut_ptr(), written);

        written as i32
    }
}

#[cfg(all(target_arch = "riscv64", target_os = "hermit", feature = "proto-alpha"))]
pub use riscv64_hermit::*;
