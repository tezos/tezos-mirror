// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// The [__internal_store_get_hash] host function is not made available by the
// SDK. We expose it through an [InternalRuntime] trait.

use tezos_smart_rollup_host::{path::Path, runtime::RuntimeError, Error};

const STORE_HASH_SIZE: usize = 32;

#[cfg(not(target_arch = "riscv64"))]
#[link(wasm_import_module = "smart_rollup_core")]
extern "C" {
    pub fn __internal_store_get_hash(
        path: *const u8,
        path_len: usize,
        dst: *mut u8,
        max_size: usize,
    ) -> i32;
}

// The RISC-V PVM does not have a `storage_get_hash` host function.
// To enable compilation to RISC-V, this stand in implementation
// deterministically returns dummy hash values.
#[cfg(target_arch = "riscv64")]
pub unsafe extern "C" fn __internal_store_get_hash(
    _path: *const u8,
    _path_len: usize,
    _dst: *mut u8,
    _max_size: usize,
) -> i32 {
    thread_local! {
        static COUNTER: std::cell::Cell<i32> = std::cell::Cell::new(0);
    }

    COUNTER.with(|counter| {
        let count = counter.get();
        counter.set(count.wrapping_add(1));
        count
    })
}

pub trait InternalRuntime {
    fn __internal_store_get_hash<T: Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError>;
}

// Wrapper for InternalRuntime, this will be added
// to the Runtime for the Kernel to use.
// The path is optional to be able to get the hash
// of the root directory.
pub trait ExtendedRuntime {
    fn store_get_hash<T: Path>(&mut self, path: &T) -> Result<Vec<u8>, RuntimeError>;
}

pub struct InternalHost();

impl InternalRuntime for InternalHost {
    fn __internal_store_get_hash<T: Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError> {
        let mut buffer = [0u8; STORE_HASH_SIZE];
        let result = unsafe {
            __internal_store_get_hash(
                path.as_ptr(),
                path.size(),
                buffer.as_mut_ptr(),
                STORE_HASH_SIZE,
            )
        };
        match Error::wrap(result) {
            Ok(_i) => Ok(buffer.to_vec()),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }
}
