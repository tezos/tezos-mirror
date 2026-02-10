// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Wrapper utilities to make a `Runtime` unwindable.

#![cfg(pvm_kind = "riscv")]

use super::{Runtime, RuntimeError, ValueType};
use crate::debug::HostDebug;
#[cfg(feature = "alloc")]
use crate::input::Message;
use crate::reveal::HostReveal;
use crate::storage::StorageV1;
use crate::{dal_parameters::RollupDalParameters, metadata::RollupMetadata, path::Path};
use alloc::rc::Rc;
use core::panic::{RefUnwindSafe, UnwindSafe};
use std::sync::RwLock;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;

extern crate alloc;
extern crate std;

/// Wrapper for a [`Runtime`] that makes it unwindable.
pub struct UnwindableRuntime<R> {
    // We need `UnwindableRuntime` to be `RefUnwindSafe` regardless of `R` so that we can obtain an
    // immutable reference to it and use it in conjunction with `catch_unwind` and `resume_unwind`.
    // `RwLock<R>` is `RefUnwindSafe` independently of `R`.
    runtime: Rc<RwLock<R>>,
}

impl<R> UnwindableRuntime<R> {
    /// Create a new [`UnwindableRuntime`] from a [`Runtime`].
    pub fn new(runtime: R) -> Self {
        Self {
            runtime: Rc::new(RwLock::new(runtime)),
        }
    }

    /// Is the underlying [`RwLock`] poisoned?
    #[inline]
    pub fn is_poisoned(&self) -> bool {
        self.runtime.is_poisoned()
    }

    /// Wraps a function that wants to use the [`Runtime`].
    #[inline]
    pub fn wrap<'a, F, X>(&'a self, func: F) -> impl 'a + UnwindSafe + FnOnce() -> X
    where
        F: 'a + UnwindSafe + FnOnce(&mut UnwindableRuntime<R>) -> X,
    {
        let mut runtime = Self {
            runtime: self.runtime.clone(),
        };
        move || func(&mut runtime)
    }

    /// Write a debug message. This won't panic if the runtime wrapper is poisoned.
    #[inline]
    pub fn unreliably_write_debug(&self, msg: &str)
    where
        R: Runtime,
    {
        if let Ok(runtime) = self.runtime.read() {
            runtime.write_debug(msg);
        }
    }
}

impl<R> UnwindSafe for UnwindableRuntime<R> {}

impl<R> RefUnwindSafe for UnwindableRuntime<R> {}

impl<R: HostDebug> HostDebug for UnwindableRuntime<R> {
    fn write_debug(&self, msg: &str) {
        self.runtime.read().unwrap().write_debug(msg)
    }
}

impl<R: HostReveal> HostReveal for UnwindableRuntime<R> {
    fn reveal_metadata(&self) -> RollupMetadata {
        self.runtime.read().unwrap().reveal_metadata()
    }

    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.runtime
            .read()
            .unwrap()
            .reveal_preimage(hash, destination)
    }

    #[cfg(feature = "alloc")]
    fn reveal_dal_page(
        &self,
        published_level: i32,
        slot_index: u8,
        page_index: i16,
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.runtime.read().unwrap().reveal_dal_page(
            published_level,
            slot_index,
            page_index,
            destination,
        )
    }

    fn reveal_dal_parameters(&self) -> RollupDalParameters {
        self.runtime.read().unwrap().reveal_dal_parameters()
    }
}

impl<R: StorageV1> StorageV1 for UnwindableRuntime<R> {
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        self.runtime.read().unwrap().store_has(path)
    }

    #[cfg(feature = "alloc")]
    fn store_read<T: crate::path::Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.runtime
            .read()
            .unwrap()
            .store_read(path, from_offset, max_bytes)
    }

    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.runtime
            .read()
            .unwrap()
            .store_read_slice(path, from_offset, buffer)
    }

    #[cfg(feature = "alloc")]
    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        self.runtime.read().unwrap().store_read_all(path)
    }

    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        self.runtime
            .write()
            .unwrap()
            .store_write(path, src, at_offset)
    }

    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        self.runtime.write().unwrap().store_write_all(path, src)
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.runtime.write().unwrap().store_delete(path)
    }

    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.runtime.write().unwrap().store_delete_value(path)
    }

    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        self.runtime.read().unwrap().store_count_subkeys(prefix)
    }

    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.runtime.write().unwrap().store_move(from_path, to_path)
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.runtime.write().unwrap().store_copy(from_path, to_path)
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        self.runtime.read().unwrap().store_value_size(path)
    }
}

impl<R: Runtime> Runtime for UnwindableRuntime<R> {
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.runtime.write().unwrap().write_output(from)
    }

    #[cfg(feature = "alloc")]
    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        self.runtime.write().unwrap().read_input()
    }

    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.runtime.write().unwrap().mark_for_reboot()
    }

    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        self.runtime.read().unwrap().last_run_aborted()
    }

    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        self.runtime.read().unwrap().upgrade_failed()
    }

    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        self.runtime.read().unwrap().restart_forced()
    }

    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        self.runtime.read().unwrap().reboot_left()
    }

    #[cfg(feature = "alloc")]
    fn runtime_version(&self) -> Result<String, RuntimeError> {
        self.runtime.read().unwrap().runtime_version()
    }
}
