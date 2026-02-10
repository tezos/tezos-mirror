// SPDX-FileCopyrightText: 2023, 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::{cell::RefCell, io::Write};

use tezos_evm_logging::Verbosity;
use tezos_evm_runtime::{
    extensions::WithGas,
    internal_runtime::{ExtendedRuntime, InternalRuntime},
    runtime::{IsEvmNode, MockKernelHost},
};
use tezos_smart_rollup_host::{
    dal_parameters::RollupDalParameters,
    debug::HostDebug,
    input::Message,
    metadata::RollupMetadata,
    path::Path,
    reveal::HostReveal,
    runtime::{Runtime as SdkRuntime, RuntimeError, ValueType},
    storage::StorageV1,
};

use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;

pub struct EvalHost {
    pub host: MockKernelHost,
    pub buffer: RefCell<Vec<u8>>,
}

impl EvalHost {
    /// Create a new instance of the `MockHost`, additionally provide the buffer
    /// where the logs will be outputed.
    pub fn default_with_buffer(buffer: RefCell<Vec<u8>>) -> Self {
        let host = MockKernelHost::default();
        Self { host, buffer }
    }
}

impl HostDebug for EvalHost {
    #[inline(always)]
    fn write_debug(&self, data: &str) {
        let mut unboxed_buffer = self.buffer.borrow_mut();
        if let Err(e) = write!(*unboxed_buffer, "{data}") {
            eprint!("Error due to: {e}")
        }
    }
}

impl HostReveal for EvalHost {
    #[inline(always)]
    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.reveal_preimage(hash, destination)
    }

    #[inline(always)]
    fn reveal_metadata(&self) -> RollupMetadata {
        self.host.reveal_metadata()
    }

    #[inline(always)]
    fn reveal_dal_page(
        &self,
        published_level: i32,
        slot_index: u8,
        page_index: i16,
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host
            .reveal_dal_page(published_level, slot_index, page_index, destination)
    }

    #[inline(always)]
    fn reveal_dal_parameters(&self) -> RollupDalParameters {
        self.host.reveal_dal_parameters()
    }
}

impl StorageV1 for EvalHost {
    #[inline(always)]
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        self.host.store_has(path)
    }

    #[inline(always)]
    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.host.store_read(path, from_offset, max_bytes)
    }

    #[inline(always)]
    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.store_read_slice(path, from_offset, buffer)
    }

    #[inline(always)]
    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        self.host.store_read_all(path)
    }

    #[inline(always)]
    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        self.host.store_write(path, src, at_offset)
    }

    #[inline(always)]
    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        self.host.store_write_all(path, src)
    }

    #[inline(always)]
    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.host.store_delete(path)
    }

    #[inline(always)]
    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.host.store_delete_value(path)
    }

    #[inline(always)]
    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        self.host.store_count_subkeys(prefix)
    }

    #[inline(always)]
    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.host.store_move(from_path, to_path)
    }

    #[inline(always)]
    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.host.store_copy(from_path, to_path)
    }

    #[inline(always)]
    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        self.host.store_value_size(path)
    }
}

impl SdkRuntime for EvalHost {
    #[inline(always)]
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.host.write_output(from)
    }

    #[inline(always)]
    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        self.host.read_input()
    }

    #[inline(always)]
    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.host.mark_for_reboot()
    }

    #[inline(always)]
    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        self.host.last_run_aborted()
    }

    #[inline(always)]
    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        self.host.upgrade_failed()
    }

    #[inline(always)]
    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        self.host.restart_forced()
    }

    #[inline(always)]
    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        self.host.reboot_left()
    }

    #[inline(always)]
    fn runtime_version(&self) -> Result<String, RuntimeError> {
        self.host.runtime_version()
    }
}

impl InternalRuntime for EvalHost {
    fn __internal_store_get_hash<T: tezos_smart_rollup_host::path::Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, tezos_smart_rollup_host::runtime::RuntimeError> {
        self.host.__internal_store_get_hash(path)
    }
}

impl ExtendedRuntime for EvalHost {
    fn store_get_hash<T: tezos_smart_rollup_host::path::Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, tezos_smart_rollup_host::runtime::RuntimeError> {
        self.host.store_get_hash(path)
    }

    fn internal_store_read_all<T: Path>(
        &self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.host.store_read_all(path)
    }
}

impl Verbosity for EvalHost {
    fn verbosity(&self) -> tezos_evm_logging::Level {
        self.host.verbosity()
    }
}

// This is a blank implementation on purpose, as this is not useful for the
// evaluation
impl WithGas for EvalHost {
    fn add_execution_gas(&mut self, _gas: u64) {}

    fn executed_gas(&self) -> u64 {
        self.host.executed_gas()
    }
}

impl IsEvmNode for EvalHost {
    fn is_evm_node(&self) -> bool {
        panic!("`is_evm_node` should not be used by REVM");
    }
}
