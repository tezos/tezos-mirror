// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::{
    input::Message,
    metadata::RollupMetadata,
    path::{concat, OwnedPath, Path, RefPath},
    runtime::{Runtime, RuntimeError, ValueType},
};

pub const TMP_PATH: RefPath = RefPath::assert_from(b"/tmp");

pub struct SafeStorage<Host>(pub Host);

fn safe_path<T: Path>(path: &T) -> Result<OwnedPath, RuntimeError> {
    concat(&TMP_PATH, path).map_err(|_| RuntimeError::PathNotFound)
}

impl<Host: Runtime> Runtime for SafeStorage<&mut Host> {
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.0.write_output(from)
    }

    fn write_debug(&self, msg: &str) {
        self.0.write_debug(msg)
    }

    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        self.0.read_input()
    }

    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        let path = safe_path(path)?;
        self.0.store_has(&path)
    }

    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.0.store_read(&path, from_offset, max_bytes)
    }

    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.0.store_read_slice(&path, from_offset, buffer)
    }

    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.0.store_write(&path, src, at_offset)
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.0.store_delete(&path)
    }

    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        let prefix = safe_path(prefix)?;
        self.0.store_count_subkeys(&prefix)
    }

    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.0.store_move(&from_path, &to_path)
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.0.store_copy(&from_path, &to_path)
    }

    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.0.reveal_preimage(hash, destination)
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.0.store_value_size(&path)
    }

    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.0.mark_for_reboot()
    }

    fn reveal_metadata(&self) -> RollupMetadata {
        self.0.reveal_metadata()
    }

    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        self.0.last_run_aborted()
    }

    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        self.0.upgrade_failed()
    }

    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        self.0.restart_forced()
    }

    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        self.0.reboot_left()
    }

    fn runtime_version(&self) -> Result<String, RuntimeError> {
        self.0.runtime_version()
    }
}

impl<Host: Runtime> SafeStorage<&mut Host> {
    pub fn promote(&mut self, path: &impl Path) -> Result<(), RuntimeError> {
        self.0.store_move(&TMP_PATH, path)
    }

    pub fn revert(&mut self) -> Result<(), RuntimeError> {
        self.0.store_delete(&TMP_PATH)
    }
}
