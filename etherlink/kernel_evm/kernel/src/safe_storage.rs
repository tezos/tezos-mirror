// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::internal_storage::{ExtendedRuntime, InternalRuntime};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
#[cfg(feature = "proto-alpha")]
use tezos_smart_rollup_host::dal_parameters::RollupDalParameters;
use tezos_smart_rollup_host::{
    input::Message,
    metadata::RollupMetadata,
    path::{concat, OwnedPath, Path, RefPath},
    runtime::{Runtime, RuntimeError, ValueType},
};

pub const TMP_PATH: RefPath = RefPath::assert_from(b"/tmp");
pub const WORLD_STATE_PATH: RefPath = RefPath::assert_from(b"/evm/world_state");
pub const TMP_WORLD_STATE_PATH: RefPath = RefPath::assert_from(b"/tmp/evm/world_state");

// The kernel runtime requires both the standard Runtime and the
// new Extended one.
pub trait KernelRuntime: Runtime + ExtendedRuntime {}

// Every type implementing an InternalRuntime will implement
// the ExtendedRuntime.
impl<T: InternalRuntime> ExtendedRuntime for T {
    fn store_get_hash<P: Path>(
        &mut self,
        path: Option<P>,
    ) -> Result<Vec<u8>, RuntimeError> {
        match path {
            Some(p) => {
                let path = safe_path(&p)?;
                self.__internal_store_get_hash(&path)
            }
            None => self.__internal_store_get_hash(&TMP_PATH),
        }
    }
}

// If a type implements the Runtime and InternalRuntime traits,
// it also implements the KernelRuntime.
impl<T: Runtime + InternalRuntime> KernelRuntime for T {}

pub struct SafeStorage<Host, Internal> {
    pub host: Host,
    pub internal: Internal,
}

pub fn safe_path<T: Path>(path: &T) -> Result<OwnedPath, RuntimeError> {
    concat(&TMP_PATH, path).map_err(|_| RuntimeError::PathNotFound)
}

impl<Host, InternalHost: InternalRuntime> InternalRuntime
    for SafeStorage<&mut Host, &mut InternalHost>
{
    fn __internal_store_get_hash<T: Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.internal.__internal_store_get_hash(path)
    }
}

impl<Host: Runtime, InternalHost> Runtime for SafeStorage<&mut Host, &mut InternalHost> {
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.host.write_output(from)
    }

    fn write_debug(&self, msg: &str) {
        self.host.write_debug(msg)
    }

    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        self.host.read_input()
    }

    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_has(&path)
    }

    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read(&path, from_offset, max_bytes)
    }

    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read_slice(&path, from_offset, buffer)
    }

    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read_all(&path)
    }

    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_write(&path, src, at_offset)
    }

    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_write_all(&path, src)
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_delete(&path)
    }

    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_delete_value(&path)
    }

    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        let prefix = safe_path(prefix)?;
        self.host.store_count_subkeys(&prefix)
    }

    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.host.store_move(&from_path, &to_path)
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.host.store_copy(&from_path, &to_path)
    }

    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.reveal_preimage(hash, destination)
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_value_size(&path)
    }

    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.host.mark_for_reboot()
    }

    fn reveal_metadata(&self) -> RollupMetadata {
        self.host.reveal_metadata()
    }

    #[cfg(all(feature = "alloc", feature = "proto-alpha"))]
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

    #[cfg(feature = "proto-alpha")]
    fn reveal_dal_parameters(&self) -> RollupDalParameters {
        self.host.reveal_dal_parameters()
    }

    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        self.host.last_run_aborted()
    }

    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        self.host.upgrade_failed()
    }

    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        self.host.restart_forced()
    }

    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        self.host.reboot_left()
    }

    fn runtime_version(&self) -> Result<String, RuntimeError> {
        self.host.runtime_version()
    }
}

impl<Host: Runtime, Internal> SafeStorage<&mut Host, &mut Internal> {
    pub fn start(&mut self) -> Result<(), RuntimeError> {
        self.host
            .store_copy(&WORLD_STATE_PATH, &TMP_WORLD_STATE_PATH)
    }

    pub fn promote(&mut self) -> Result<(), RuntimeError> {
        self.host
            .store_move(&TMP_WORLD_STATE_PATH, &WORLD_STATE_PATH)
    }

    pub fn revert(&mut self) -> Result<(), RuntimeError> {
        self.host.store_delete(&TMP_PATH)
    }
}
