// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::extensions::WithGas;
use crate::internal_runtime::{ExtendedRuntime, InternalRuntime};
use crate::runtime::{IsEvmNode, Runtime};
use tezos_evm_logging::Verbosity;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::dal_parameters::RollupDalParameters;
use tezos_smart_rollup_host::debug::HostDebug;
use tezos_smart_rollup_host::reveal::HostReveal;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::wasm::WasmHost;
use tezos_smart_rollup_host::{
    input::Message,
    metadata::RollupMetadata,
    path::{concat, OwnedPath, Path, RefPath},
    runtime::{Runtime as SdkRuntime, RuntimeError, ValueType},
};

pub const TMP_PATH: RefPath = RefPath::assert_from(b"/tmp");
pub const TRACE_PATH: RefPath = RefPath::assert_from(b"/evm/trace");

pub fn safe_path<T: Path>(path: &T) -> Result<OwnedPath, RuntimeError> {
    concat(&TMP_PATH, path).map_err(|_| RuntimeError::PathNotFound)
}

pub struct SafeStorage<Runtime> {
    pub host: Runtime,
    /// Invariant: paths must not overlap (no path is a prefix of another)
    pub world_states: Vec<OwnedPath>,
}

impl<Host: Runtime> InternalRuntime for SafeStorage<&mut Host> {
    fn __internal_store_get_hash<T: Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.host.__internal_store_get_hash(path)
    }
}

impl<Host: Runtime> ExtendedRuntime for SafeStorage<&mut Host> {
    #[inline(always)]
    fn store_get_hash<P: Path>(&mut self, path: &P) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.__internal_store_get_hash(&path)
    }

    #[inline(always)]
    fn internal_store_read_all<T: Path>(
        &self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.host.store_read_all(path)
    }
}

impl<Host: HostDebug> HostDebug for SafeStorage<&mut Host> {
    #[inline(always)]
    fn write_debug(&self, msg: &str) {
        self.host.write_debug(msg)
    }
}

impl<Host: HostReveal> HostReveal for SafeStorage<&mut Host> {
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

impl<Host: StorageV1> StorageV1 for SafeStorage<&mut Host> {
    #[inline(always)]
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_has(&path)
    }

    #[inline(always)]
    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read(&path, from_offset, max_bytes)
    }

    #[inline(always)]
    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read_slice(&path, from_offset, buffer)
    }

    #[inline(always)]
    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read_all(&path)
    }

    #[inline(always)]
    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_write(&path, src, at_offset)
    }

    #[inline(always)]
    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_write_all(&path, src)
    }

    #[inline(always)]
    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_delete(&path)
    }

    #[inline(always)]
    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_delete_value(&path)
    }

    #[inline(always)]
    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        let prefix = safe_path(prefix)?;
        self.host.store_count_subkeys(&prefix)
    }

    #[inline(always)]
    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.host.store_move(&from_path, &to_path)
    }

    #[inline(always)]
    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.host.store_copy(&from_path, &to_path)
    }

    #[inline(always)]
    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_value_size(&path)
    }
}

impl<Host: WasmHost> WasmHost for SafeStorage<&mut Host> {
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

impl<Host: Runtime> SdkRuntime for SafeStorage<&mut Host> {}

impl<Host: Runtime> Verbosity for SafeStorage<&mut Host> {
    fn verbosity(&self) -> tezos_evm_logging::Level {
        self.host.verbosity()
    }
}

impl<Host: Runtime> SafeStorage<&mut Host> {
    pub fn start(&mut self) -> Result<(), RuntimeError> {
        for world_state in self.world_states.iter() {
            let tmp_path = safe_path(world_state)?;
            self.host.store_copy(world_state, &tmp_path)?;
        }
        Ok(())
    }

    pub fn promote(&mut self) -> Result<(), RuntimeError> {
        for world_state in self.world_states.iter() {
            let tmp_path = safe_path(world_state)?;
            self.host.store_move(&tmp_path, world_state)?;
        }
        Ok(())
    }

    // Only used in tracing mode, so that the trace doesn't polute the world
    // state but is still promoted at the end and accessible from the node.
    pub fn promote_trace(&mut self) -> Result<(), RuntimeError> {
        let tmp_path = safe_path(&TRACE_PATH)?;
        if let Ok(Some(_)) = self.host.store_has(&tmp_path) {
            self.host.store_move(&tmp_path, &TRACE_PATH)?
        }
        Ok(())
    }

    pub fn revert(&mut self) -> Result<(), RuntimeError> {
        self.host.store_delete(&TMP_PATH)
    }
}

impl<Host: Runtime> WithGas for SafeStorage<&mut Host> {
    fn add_execution_gas(&mut self, gas: u64) {
        self.host.add_execution_gas(gas)
    }

    fn executed_gas(&self) -> u64 {
        self.host.executed_gas()
    }
}

impl<Host: Runtime> IsEvmNode for SafeStorage<&mut Host> {
    fn is_evm_node(&self) -> bool {
        self.host.is_evm_node()
    }
}
