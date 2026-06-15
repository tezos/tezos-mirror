// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::smart_rollup_core::ReadInputMessageInfo;

#[link(wasm_import_module = "smart_rollup_core")]
unsafe extern "C" {
    pub(super) fn read_input(
        message_info: *mut ReadInputMessageInfo,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32;

    pub(super) fn write_output(src: *const u8, num_bytes: usize) -> i32;

    pub fn write_debug(src: *const u8, num_bytes: usize);

    pub(super) fn store_has(path: *const u8, path_len: usize) -> i32;

    pub(super) fn store_read(
        path: *const u8,
        path_len: usize,
        offset: usize,
        dst: *mut u8,
        num_bytes: usize,
    ) -> i32;

    pub(super) fn store_write(
        path: *const u8,
        path_len: usize,
        offset: usize,
        src: *const u8,
        num_bytes: usize,
    ) -> i32;

    pub(super) fn store_delete(path: *const u8, path_len: usize) -> i32;

    pub(super) fn store_delete_value(path: *const u8, path_len: usize) -> i32;

    pub(super) fn store_list_size(path: *const u8, path_len: usize) -> i64;

    pub(super) fn store_move(
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32;

    pub(super) fn store_copy(
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32;

    pub(super) fn reveal_preimage(
        hash_addr: *const u8,
        hash_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32;

    pub(super) fn store_value_size(path: *const u8, path_len: usize) -> i32;

    pub(super) fn reveal_metadata(destination_addr: *mut u8, max_bytes: usize) -> i32;

    pub(super) fn reveal(
        payload_addr: *const u8,
        payload_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32;

    pub(super) fn __internal_store_get_hash(
        path: *const u8,
        path_len: usize,
        destination_addr: *mut u8,
        max_size: usize,
    ) -> i32;

    /// Resize the registry up/down by `n` databases. Growing appends fresh empty
    /// databases; shrinking drops the trailing ones (and their contents).
    /// Returns the new size of the registry on success.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`NDS_RESIZE_INVALID`]: either `|n| > 1`, or `current_size + n > i32::MAX`.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`NDS_RESIZE_INVALID`]: super::NDS_RESIZE_INVALID
    pub fn nds_registry_resize(diff: i64) -> isize;

    /// Replace the contents of database `dst` with a copy of database
    /// `src`. `src` is unchanged. `dst == src` is a no-op. Returns 0 on
    /// success.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `src >= registry_size || dst >= registry_size`.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    pub fn nds_registry_copy(src: u64, dst: u64) -> isize;

    /// Replace the contents of database `dst` with the contents of `src`,
    /// then reset `src` to empty. `dst == src` is a no-op. Returns 0 on
    /// success.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `src >= registry_size || dst >= registry_size`.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    pub fn nds_registry_move(src: u64, dst: u64) -> isize;

    /// Empty the database at `db_index` (drop every key). Returns 0 on
    /// success.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `db_index >= registry_size`.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    pub fn nds_registry_clear(db_index: u64) -> isize;

    /// Return `1` if `key` exists in database `db_index`, `0` otherwise.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`INPUT_OUTPUT_TOO_LARGE`]: `key_len > 4096`.
    /// - [`STORE_INVALID_KEY`]: the bytes pointed to by `key` and `key_len` are not a valid nds key.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `db_index >= registry_size`.
    ///
    /// # Safety
    ///
    /// `key` must point to a valid readable region of memory of at least `key_len` bytes in length.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`INPUT_OUTPUT_TOO_LARGE`]: super::INPUT_OUTPUT_TOO_LARGE
    /// [`STORE_INVALID_KEY`]: super::STORE_INVALID_KEY
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    pub unsafe fn nds_store_exists(
        db_index: u64,
        key: *const u8,
        key_len: usize,
    ) -> isize;

    /// Read up to `max_bytes` of the value at `(db_index, key)` starting
    /// at byte `offset` into `dst`. Returns the number of bytes written to `dst`.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`INPUT_OUTPUT_TOO_LARGE`]: `key_len > 4096 || max_bytes > 2048`.
    /// - [`STORE_INVALID_KEY`]: the bytes pointed to by `key` and `key_len` are not a valid nds key.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `db_index >= registry_size`.
    ///
    /// # Safety
    ///
    /// `key` must point to a valid readable region of memory of at least `key_len` bytes in length.
    ///
    /// `dst` must point to a valid writeable region of memory of at
    /// least `max_bytes` bytes in length.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`INPUT_OUTPUT_TOO_LARGE`]: super::INPUT_OUTPUT_TOO_LARGE
    /// [`STORE_INVALID_KEY`]: super::STORE_INVALID_KEY
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    pub unsafe fn nds_store_read(
        db_index: u64,
        key: *const u8,
        key_len: usize,
        offset: u64,
        dst: *mut u8,
        max_bytes: usize,
    ) -> isize;

    /// Write `num_bytes` from `src` into the value at `(db_index, key)`
    /// starting at byte `offset`. Creates the key if absent and
    /// `offset == 0`. The value is extended if the write runs past the end,
    /// but is never truncated. Returns number of bytes written on success.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`INPUT_OUTPUT_TOO_LARGE`]: `key_len > 4096 || num_bytes > 2048`.
    /// - [`STORE_INVALID_KEY`]: the bytes pointed to by `key` and `key_len` are not a valid nds key.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `db_index >= registry_size`.
    /// - [`STORE_INVALID_ACCESS`]: the offset is larger than the current length (`0` if no value) of the value
    ///   pointed to by `key`.
    /// - [`STORE_VALUE_SIZE_EXCEEDED`]: after the write, the value would be larger than `64MiB` in size.
    ///
    /// # Safety
    ///
    /// `key` must point to a valid readable region of memory of at least `key_len` bytes in length.
    ///
    /// `src` must point to a valid readable region of memory of at
    /// least `num_bytes` bytes in length.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`INPUT_OUTPUT_TOO_LARGE`]: super::INPUT_OUTPUT_TOO_LARGE
    /// [`STORE_INVALID_KEY`]: super::STORE_INVALID_KEY
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    /// [`STORE_INVALID_ACCESS`]: super::STORE_INVALID_ACCESS
    /// [`STORE_VALUE_SIZE_EXCEEDED`]: super::STORE_VALUE_SIZE_EXCEEDED
    pub fn nds_store_write(
        db_index: u64,
        key: *const u8,
        key_len: usize,
        offset: u64,
        src: *const u8,
        num_bytes: usize,
    ) -> isize;

    /// Set the value at `(db_index, key)` to the `num_bytes` at `src`,
    /// fully replacing any previous value. Creates the key if absent.
    /// Returns 0 on success.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`INPUT_OUTPUT_TOO_LARGE`]: `key_len > 4096 || num_bytes > 2048`.
    /// - [`STORE_INVALID_KEY`]: the bytes pointed to by `key` and `key_len` are not a valid nds key.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `db_index >= registry_size`.
    ///
    /// # Safety
    ///
    /// `key` must point to a valid readable region of memory of at least `key_len` bytes in length.
    ///
    /// `src` must point to a valid readable region of memory of at
    /// least `num_bytes` bytes in length.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`INPUT_OUTPUT_TOO_LARGE`]: super::INPUT_OUTPUT_TOO_LARGE
    /// [`STORE_INVALID_KEY`]: super::STORE_INVALID_KEY
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    pub fn nds_store_set(
        db_index: u64,
        key: *const u8,
        key_len: usize,
        src: *const u8,
        num_bytes: usize,
    ) -> isize;

    /// Delete `(db_index, key)`. Deleting a non-existent key is a silent
    /// no-op. Returns 0 on success.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`INPUT_OUTPUT_TOO_LARGE`]: `key_len > 4096`.
    /// - [`STORE_INVALID_KEY`]: the bytes pointed to by `key` and `key_len` are not a valid nds key.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `db_index >= registry_size`.
    ///
    /// # Safety
    ///
    /// `key` must point to a valid readable region of memory of at least `key_len` bytes in length.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`INPUT_OUTPUT_TOO_LARGE`]: super::INPUT_OUTPUT_TOO_LARGE
    /// [`STORE_INVALID_KEY`]: super::STORE_INVALID_KEY
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    pub fn nds_store_delete(db_index: u64, key: *const u8, key_len: usize) -> isize;

    /// Return the length in bytes of the value at `(db_index, key)`.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`INPUT_OUTPUT_TOO_LARGE`]: `key_len > 4096`.
    /// - [`STORE_INVALID_KEY`]: the bytes pointed to by `key` and `key_len` are not a valid nds key.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `db_index >= registry_size`.
    ///
    /// # Safety
    ///
    /// `key` must point to a valid readable region of memory of at least `key_len` bytes in length.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`INPUT_OUTPUT_TOO_LARGE`]: super::INPUT_OUTPUT_TOO_LARGE
    /// [`STORE_INVALID_KEY`]: super::STORE_INVALID_KEY
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    pub fn nds_store_value_size(db_index: u64, key: *const u8, key_len: usize) -> isize;

    /// Write up to `max_bytes` of the Blake3 root-hash of the database at
    /// `db_index` to `dst`. The full hash is 32 bytes
    /// (`STORE_HASH_SIZE`); smaller `max_bytes` yields a truncated
    /// prefix. Returns the number of bytes written.
    ///
    /// Errors:
    /// - [`NDS_NOT_ENABLED`]: nds has not yet been enabled.
    /// - [`NDS_DATABASE_OUT_OF_BOUNDS`]: `db_index >= registry_size`.
    ///
    /// # Safety
    ///
    /// `dst` must point to a valid writeable region of memory of at
    /// least `max_bytes` bytes in length.
    ///
    /// [`NDS_NOT_ENABLED`]: super::NDS_NOT_ENABLED
    /// [`NDS_DATABASE_OUT_OF_BOUNDS`]: super::NDS_DATABASE_OUT_OF_BOUNDS
    pub fn nds_database_get_hash(db_index: u64, dst: *mut u8, max_bytes: usize) -> i32;
}
