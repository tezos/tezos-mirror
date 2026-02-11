// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// TODO: RV-289: Etherlink uses `SmartRollupCore` on native target
// Etherlink uses Clippy which runs on the native target. When running natively, the fallback
// target will be used.

use crate::smart_rollup_core::ReadInputMessageInfo;

pub(super) unsafe fn read_input(
    _message_info: *mut ReadInputMessageInfo,
    _dst: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn write_output(_src: *const u8, _num_bytes: usize) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn write_debug(_src: *const u8, _num_bytes: usize) {
    unimplemented!()
}

pub(super) unsafe fn store_has(_path: *const u8, _path_len: usize) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn store_read(
    _path: *const u8,
    _path_len: usize,
    _offset: usize,
    _dst: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn store_write(
    _path: *const u8,
    _path_len: usize,
    _offset: usize,
    _src: *const u8,
    _num_bytes: usize,
) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn store_delete(_path: *const u8, _len: usize) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn store_delete_value(_path: *const u8, _len: usize) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn store_list_size(_path: *const u8, _path_len: usize) -> i64 {
    unimplemented!()
}

pub(super) unsafe fn store_move(
    _from_path: *const u8,
    _from_path_len: usize,
    _to_path: *const u8,
    _to_path_len: usize,
) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn store_copy(
    _from_path: *const u8,
    _from_path_len: usize,
    _to_path: *const u8,
    _to_path_len: usize,
) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn reveal_preimage(
    _hash_addr: *const u8,
    _hash_len: usize,
    _destination_addr: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn store_value_size(_path: *const u8, _path_len: usize) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn reveal_metadata(
    _destination_addr: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn reveal(
    _payload_addr: *const u8,
    _payload_len: usize,
    _destination_addr: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

pub(super) unsafe fn __internal_store_get_hash(
    __path: *const u8,
    __path_len: usize,
    __destination_addr: *mut u8,
    __max_bytes: usize,
) -> i32 {
    unimplemented!()
}
