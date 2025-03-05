// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

extern crate std;

#[path = "riscv_hermit_target.rs"]
mod hermit_target;

use crate::smart_rollup_core::ReadInputMessageInfo;
use std::{
    io::{self, Write},
    slice::from_raw_parts,
};

// Re-use SBI definitions from HermitOS target
pub(crate) use hermit_target::{
    read_input, reveal, reveal_metadata, reveal_preimage, write_debug, write_output,
};

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
pub(super) unsafe fn store_value_size(_path: *const u8, _path_len: usize) -> i32 {
    unimplemented!()
}
