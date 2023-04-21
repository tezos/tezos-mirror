// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Implementation of [`SmartRollupCore`] used when compiling to **wasm**.

use crate::smart_rollup_core as core;
use crate::smart_rollup_core::ReadInputMessageInfo;
use crate::SmartRollupCore;

/// The runtime host when running in `wasm` rollup.
///
/// # Safety
/// The only way to create an instance of `RollupHost` is to call [`RollupHost::new`], which
/// itself is *unsafe* to call. This is done to enforce the invariant that a kernel only
/// ever holds *one* reference of its *runtime*.
///
/// Therefore, `WasmHost` **does not** implement `Debug`, `Copy` or `Clone`. This prevents
/// the `kernel` from creating duplicate Hosts, which may break invariants in other SDK
/// crates.
pub struct RollupHost {}

impl RollupHost {
    /// Create a new reference to the wasm runtime.
    ///
    /// # Safety
    /// **Must** only ever be called once per *kernel entry*. Multiple
    /// instances of `WasmHost` may conflict with each other - breaking invariants
    /// elsewhere which make assumptions about the behaviour of the runtime.
    pub unsafe fn new() -> Self {
        Self {}
    }
}

unsafe impl SmartRollupCore for RollupHost {
    unsafe fn read_input(
        &self,
        message_info: *mut ReadInputMessageInfo,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        core::read_input(message_info, dst, max_bytes)
    }

    unsafe fn write_output(&self, src: *const u8, num_bytes: usize) -> i32 {
        core::write_output(src, num_bytes)
    }

    unsafe fn write_debug(&self, src: *const u8, num_bytes: usize) {
        core::write_debug(src, num_bytes)
    }

    unsafe fn store_has(&self, path: *const u8, path_len: usize) -> i32 {
        core::store_has(path, path_len)
    }

    unsafe fn store_read(
        &self,
        path: *const u8,
        path_len: usize,
        offset: usize,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        core::store_read(path, path_len, offset, dst, max_bytes)
    }

    unsafe fn store_write(
        &self,
        path: *const u8,
        path_len: usize,
        offset: usize,
        src: *const u8,
        num_bytes: usize,
    ) -> i32 {
        core::store_write(path, path_len, offset, src, num_bytes)
    }

    unsafe fn store_delete(&self, path: *const u8, len: usize) -> i32 {
        core::store_delete(path, len)
    }

    unsafe fn store_list_size(&self, path: *const u8, path_len: usize) -> i64 {
        core::store_list_size(path, path_len)
    }

    unsafe fn store_move(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        core::store_move(from_path, from_path_len, to_path, to_path_len)
    }

    unsafe fn store_copy(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        core::store_copy(from_path, from_path_len, to_path, to_path_len)
    }

    unsafe fn reveal_preimage(
        &self,
        hash_addr: *const u8,
        hash_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        core::reveal_preimage(hash_addr, hash_len, destination_addr, max_bytes)
    }

    unsafe fn store_value_size(&self, path: *const u8, path_len: usize) -> i32 {
        core::store_value_size(path, path_len)
    }

    unsafe fn reveal_metadata(&self, destination_addr: *mut u8, max_bytes: usize) -> i32 {
        core::reveal_metadata(destination_addr, max_bytes)
    }
}
