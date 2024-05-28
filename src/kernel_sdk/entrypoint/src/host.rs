// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![cfg(feature = "experimental-host-in-memory-store")]

use std::cell::RefCell;
use tezos_smart_rollup_core::rollup_host::RollupHost;
use tezos_smart_rollup_core::smart_rollup_core::{ReadInputMessageInfo, SmartRollupCore};
use tezos_smart_rollup_mock::InMemoryStore;

/// Runtime with in-memory storage
pub struct RollupHostWithInMemoryStorage {
    host: RollupHost,
    store: RefCell<InMemoryStore>,
}

impl RollupHostWithInMemoryStorage {
    /// Create a host with a new in-memory storage
    ///
    /// # Safety
    /// **Must** only ever be called once per *kernel entry*.
    pub unsafe fn new() -> Self {
        Self {
            host: RollupHost::new(),
            store: InMemoryStore::default().into(),
        }
    }
}

unsafe impl SmartRollupCore for RollupHostWithInMemoryStorage {
    unsafe fn read_input(
        &self,
        message_info: *mut ReadInputMessageInfo,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        self.host.read_input(message_info, dst, max_bytes)
    }

    unsafe fn write_output(&self, src: *const u8, num_bytes: usize) -> i32 {
        self.host.write_output(src, num_bytes)
    }

    unsafe fn write_debug(&self, src: *const u8, num_bytes: usize) {
        self.host.write_debug(src, num_bytes)
    }

    unsafe fn store_has(&self, path: *const u8, path_len: usize) -> i32 {
        self.store.borrow().store_has(path, path_len)
    }

    unsafe fn store_read(
        &self,
        path: *const u8,
        path_len: usize,
        offset: usize,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        self.store
            .borrow()
            .store_read(path, path_len, offset, dst, max_bytes)
    }

    unsafe fn store_write(
        &self,
        path: *const u8,
        path_len: usize,
        offset: usize,
        src: *const u8,
        num_bytes: usize,
    ) -> i32 {
        self.store
            .borrow_mut()
            .store_write(path, path_len, offset, src, num_bytes)
    }

    unsafe fn store_delete(&self, path: *const u8, len: usize) -> i32 {
        self.store.borrow_mut().store_delete(path, len)
    }

    unsafe fn store_delete_value(&self, path: *const u8, len: usize) -> i32 {
        self.store.borrow_mut().store_delete_value(path, len)
    }

    unsafe fn store_list_size(&self, path: *const u8, path_len: usize) -> i64 {
        self.store.borrow().store_list_size(path, path_len)
    }

    unsafe fn store_move(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        self.store
            .borrow_mut()
            .store_move(from_path, from_path_len, to_path, to_path_len)
    }

    unsafe fn store_copy(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        self.store
            .borrow_mut()
            .store_copy(from_path, from_path_len, to_path, to_path_len)
    }

    unsafe fn reveal_preimage(
        &self,
        hash_addr: *const u8,
        hash_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        self.host
            .reveal_preimage(hash_addr, hash_len, destination_addr, max_bytes)
    }

    #[cfg(feature = "proto-alpha")]
    unsafe fn reveal(
        &self,
        payload_addr: *const u8,
        payload_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        self.host
            .reveal(payload_addr, payload_len, destination_addr, max_bytes)
    }

    unsafe fn store_value_size(&self, path: *const u8, path_len: usize) -> i32 {
        self.store.borrow().store_value_size(path, path_len)
    }

    unsafe fn reveal_metadata(&self, destination_addr: *mut u8, max_bytes: usize) -> i32 {
        self.host.reveal_metadata(destination_addr, max_bytes)
    }
}
