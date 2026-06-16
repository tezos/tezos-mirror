// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::smart_rollup_core::ReadInputMessageInfo;

#[link(wasm_import_module = "smart_rollup_core")]
extern "C" {
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
}
