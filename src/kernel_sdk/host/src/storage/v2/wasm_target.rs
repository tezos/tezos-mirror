// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of [`WasmNds`] used when targetting wasm.

#![cfg(pvm_kind = "wasm")]

/// Zero-sized handle implementing [`WasmNds`] on top of the NDS host
/// functions of the WASM PVM.
///
/// Only available when targeting the WASM PVM.
pub struct WasmNdsHandle;

use super::Key;
use super::NdsError;
use super::RegistryResizeRequest;
use super::WasmNds;
use core::mem::MaybeUninit;
use tezos_smart_rollup_core::target_impl;
use tezos_smart_rollup_core::STORE_HASH_SIZE;

/// Interpret an NDS host-function return code: a non-negative value is the
/// successful result, a negative value is mapped to the matching
/// [`NdsError`].
fn wrap(code: isize) -> Result<usize, NdsError> {
    if code >= 0 {
        // Non-negative codes are valid results; casting cannot wrap.
        return Ok(code as usize);
    }

    let err = match code as i32 {
        tezos_smart_rollup_core::NDS_NOT_ENABLED => NdsError::NdsDisabled,
        tezos_smart_rollup_core::NDS_RESIZE_INVALID => NdsError::ResizeInvalid,
        tezos_smart_rollup_core::NDS_DATABASE_OUT_OF_BOUNDS => {
            NdsError::DatabaseOutOfBounds
        }
        tezos_smart_rollup_core::INPUT_OUTPUT_TOO_LARGE => NdsError::InputOutputTooLarge,
        tezos_smart_rollup_core::STORE_NOT_A_VALUE => NdsError::KeyNotFound,
        tezos_smart_rollup_core::STORE_INVALID_ACCESS => NdsError::OffsetTooLarge,
        tezos_smart_rollup_core::STORE_VALUE_SIZE_EXCEEDED => {
            NdsError::StoreValueSizeExceeded
        }
        // `Key` guarantees a well-formed key, and the
        // signed-integer arguments are always in range, so the host can
        // only return the error codes enumerated above. Any other code is
        // a host-contract violation.
        _ => {
            cfg_if::cfg_if! {
                if #[cfg(feature = "alloc")] {
                    unreachable!("unexpected NDS host function error code: {code}")
                } else {
                    unreachable!("unexpected NDS host function error code")
                }
            }
        }
    };

    Err(err)
}

impl WasmNds for WasmNdsHandle {
    fn len(&self) -> Result<usize, NdsError> {
        // A resize by zero leaves the registry untouched and returns its
        // current size.
        wrap(unsafe { target_impl::nds_registry_resize(0) })
    }

    fn resize(&self, request: RegistryResizeRequest) -> Result<usize, NdsError> {
        wrap(unsafe { target_impl::nds_registry_resize(request as i64) })
    }

    fn copy_db(&self, src: usize, dst: usize) -> Result<(), NdsError> {
        wrap(unsafe { target_impl::nds_registry_copy(src as u64, dst as u64) })
            .map(|_| ())
    }

    fn move_db(&self, src: usize, dst: usize) -> Result<(), NdsError> {
        wrap(unsafe { target_impl::nds_registry_move(src as u64, dst as u64) })
            .map(|_| ())
    }

    fn clear_db(&self, db_index: usize) -> Result<(), NdsError> {
        wrap(unsafe { target_impl::nds_registry_clear(db_index as u64) }).map(|_| ())
    }

    fn hash_db(&self, db_index: usize) -> Result<[u8; STORE_HASH_SIZE], NdsError> {
        let mut buffer = [0u8; STORE_HASH_SIZE];
        // SAFETY: `buffer` is a valid writeable region of `STORE_HASH_SIZE`
        // bytes.
        let code = unsafe {
            target_impl::nds_database_get_hash(
                db_index as u64,
                buffer.as_mut_ptr(),
                STORE_HASH_SIZE,
            )
        };
        wrap(code as isize).map(|_| buffer)
    }

    fn store_exists(&self, db_index: usize, key: &Key) -> Result<bool, NdsError> {
        // SAFETY: `key.0` is a valid readable region of `key.0.len()` bytes.
        let code = unsafe {
            target_impl::nds_store_exists(db_index as u64, key.0.as_ptr(), key.0.len())
        };
        wrap(code).map(|exists| exists != 0)
    }

    fn store_value_size(&self, db_index: usize, key: &Key) -> Result<usize, NdsError> {
        // SAFETY: `key.0` is a valid readable region of `key.0.len()` bytes.
        wrap(unsafe {
            target_impl::nds_store_value_size(
                db_index as u64,
                key.0.as_ptr(),
                key.0.len(),
            )
        })
    }

    fn store_read(
        &self,
        db_index: usize,
        key: &Key,
        offset: usize,
        out: &mut [MaybeUninit<u8>],
    ) -> Result<usize, NdsError> {
        // SAFETY: `key.0` is a valid readable region of `key.0.len()` bytes,
        // and `out` is a valid writeable region of `out.len()` bytes.
        let code = unsafe {
            target_impl::nds_store_read(
                db_index as u64,
                key.0.as_ptr(),
                key.0.len(),
                offset as u64,
                out.as_mut_ptr() as *mut u8,
                out.len(),
            )
        };
        wrap(code)
    }

    fn store_set(
        &self,
        db_index: usize,
        key: &Key,
        value: &[u8],
    ) -> Result<(), NdsError> {
        // SAFETY: `key.0` and `value` are valid readable regions of
        // `key.0.len()` and `value.len()` bytes respectively.
        wrap(unsafe {
            target_impl::nds_store_set(
                db_index as u64,
                key.0.as_ptr(),
                key.0.len(),
                value.as_ptr(),
                value.len(),
            )
        })
        .map(|_| ())
    }

    fn store_write(
        &self,
        db_index: usize,
        key: &Key,
        offset: usize,
        value: &[u8],
    ) -> Result<(), NdsError> {
        // SAFETY: `key.0` and `value` are valid readable regions of
        // `key.0.len()` and `value.len()` bytes respectively.
        wrap(unsafe {
            target_impl::nds_store_write(
                db_index as u64,
                key.0.as_ptr(),
                key.0.len(),
                offset as u64,
                value.as_ptr(),
                value.len(),
            )
        })
        .map(|_| ())
    }

    fn store_delete(&self, db_index: usize, key: &Key) -> Result<(), NdsError> {
        // SAFETY: `key.0` is a valid readable region of `key.0.len()` bytes.
        wrap(unsafe {
            target_impl::nds_store_delete(db_index as u64, key.0.as_ptr(), key.0.len())
        })
        .map(|_| ())
    }
}
