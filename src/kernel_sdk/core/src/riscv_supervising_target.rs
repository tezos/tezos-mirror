// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

extern crate std;

use crate::smart_rollup_core::ReadInputMessageInfo;
use std::{
    io::{self, Write},
    slice::from_raw_parts,
};

#[inline]
pub(super) unsafe fn read_input(
    _message_info: *mut ReadInputMessageInfo,
    _dst: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn write_output(_src: *const u8, _num_bytes: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn write_debug(src: *const u8, num_bytes: usize) {
    let buffer = unsafe { from_raw_parts(src, num_bytes) };
    io::stdout()
        .write_all(buffer)
        .expect("Writing to stdout failed");
}

#[inline]
pub(super) unsafe fn store_has(_path: *const u8, _path_len: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_read(
    _path: *const u8,
    _path_len: usize,
    _offset: usize,
    _dst: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_write(
    _path: *const u8,
    _path_len: usize,
    _offset: usize,
    _src: *const u8,
    _num_bytes: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_delete(_path: *const u8, _len: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_delete_value(_path: *const u8, _len: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_list_size(_path: *const u8, _path_len: usize) -> i64 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_move(
    _from_path: *const u8,
    _from_path_len: usize,
    _to_path: *const u8,
    _to_path_len: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_copy(
    _from_path: *const u8,
    _from_path_len: usize,
    _to_path: *const u8,
    _to_path_len: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn reveal_preimage(
    _hash_addr: *const u8,
    _hash_len: usize,
    _destination_addr: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn reveal(
    _payload_addr: *const u8,
    _payload_len: usize,
    _destination_addr: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_value_size(_path: *const u8, _path_len: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn reveal_metadata(_buffer: *mut u8, _max_bytes: usize) -> i32 {
    unimplemented!()
}
