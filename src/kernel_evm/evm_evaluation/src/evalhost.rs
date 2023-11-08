// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::fs::OpenOptions;
use std::io::Write;

use tezos_smart_rollup_mock::MockHost;

use core::slice::from_raw_parts;
use tezos_smart_rollup_core::smart_rollup_core::{ReadInputMessageInfo, SmartRollupCore};

pub struct EvalHost {
    pub host: MockHost,
    pub output_file: Option<String>,
}

impl EvalHost {
    /// Create a new instance of the `MockHost`, additionally provide the file's
    /// name where the logs will be outputed.
    pub fn default_with_output_file(output_file: String) -> Self {
        let host = MockHost::default();
        Self {
            host,
            output_file: Some(output_file),
        }
    }
}

unsafe impl SmartRollupCore for EvalHost {
    unsafe fn read_input(
        &self,
        message_info: *mut ReadInputMessageInfo,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        self.host.read_input(message_info, dst, max_bytes)
    }

    unsafe fn write_debug(&self, src: *const u8, num_bytes: usize) {
        let debug_out = from_raw_parts(src, num_bytes).to_vec();

        let debug = String::from_utf8(debug_out).expect("unexpected non-utf8 debug log");

        match &self.output_file {
            Some(output_file) => {
                let output_file = OpenOptions::new()
                    .append(true)
                    .truncate(false)
                    .create(true)
                    .open(output_file);
                match output_file {
                    Ok(mut output_file) => {
                        if let Err(e) = write!(output_file, "{}", &debug) {
                            eprint!("Error due to: {}", e)
                        }
                    }
                    Err(e) => eprint!("Error due to: {}", e),
                }
            }
            None => eprint!("{}", &debug),
        }
    }

    unsafe fn write_output(&self, src: *const u8, num_bytes: usize) -> i32 {
        self.host.write_output(src, num_bytes)
    }

    unsafe fn store_has(&self, path: *const u8, len: usize) -> i32 {
        self.host.store_has(path, len)
    }

    unsafe fn store_read(
        &self,
        path: *const u8,
        len: usize,
        offset: usize,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        self.host.store_read(path, len, offset, dst, max_bytes)
    }

    unsafe fn store_write(
        &self,
        path: *const u8,
        len: usize,
        offset: usize,
        src: *const u8,
        num_bytes: usize,
    ) -> i32 {
        self.host.store_write(path, len, offset, src, num_bytes)
    }

    unsafe fn store_delete(&self, path: *const u8, len: usize) -> i32 {
        self.host.store_delete(path, len)
    }

    unsafe fn store_delete_value(&self, path: *const u8, len: usize) -> i32 {
        self.host.store_delete_value(path, len)
    }

    unsafe fn store_list_size(&self, path: *const u8, len: usize) -> i64 {
        self.host.store_list_size(path, len)
    }

    unsafe fn store_move(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        self.host
            .store_move(from_path, from_path_len, to_path, to_path_len)
    }

    unsafe fn store_copy(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        self.host
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

    unsafe fn store_value_size(&self, path: *const u8, path_len: usize) -> i32 {
        self.host.store_value_size(path, path_len)
    }

    unsafe fn reveal_metadata(&self, destination_addr: *mut u8, max_bytes: usize) -> i32 {
        self.host.reveal_metadata(destination_addr, max_bytes)
    }

    #[cfg(feature = "proto-alpha")]
    unsafe fn reveal(
        &self,
        _payload_addr: *const u8,
        _payload_len: usize,
        _destination_addr: *mut u8,
        _max_bytes: usize,
    ) -> i32 {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6171
        unimplemented!("The `reveal` host function is not yet mocked.")
    }
}
