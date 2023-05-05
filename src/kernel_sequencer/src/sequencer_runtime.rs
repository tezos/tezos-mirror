// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::{
    input::Message,
    metadata::RollupMetadata,
    path::Path,
    runtime::{Runtime, RuntimeError, ValueType},
};

use crate::delayed_inbox::read_input;

pub struct SequencerRuntime<R>
where
    R: Runtime,
{
    host: R,
}

/// Runtime that handles the delayed inbox and the sequencer protocol.
///
/// The sequencer protocol is driven by the call of the read_input function.
/// That's why all the other functions of this runtime are calling the runtime passed in parameter.
impl<R> SequencerRuntime<R>
where
    R: Runtime,
{
    pub fn new(host: R) -> Self {
        Self { host }
    }
}

impl<R> Runtime for SequencerRuntime<R>
where
    R: Runtime,
{
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.host.write_output(from)
    }

    fn write_debug(&self, msg: &str) {
        self.host.write_debug(msg)
    }

    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        read_input(&mut self.host)
    }

    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        self.host.store_has(path)
    }

    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.host.store_read(path, from_offset, max_bytes)
    }

    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.store_read_slice(path, from_offset, buffer)
    }

    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        self.host.store_write(path, src, at_offset)
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.host.store_delete(path)
    }

    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        self.host.store_count_subkeys(prefix)
    }

    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.host.store_move(from_path, to_path)
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.host.store_copy(from_path, to_path)
    }

    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.reveal_preimage(hash, destination)
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        self.host.store_value_size(path)
    }

    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.host.mark_for_reboot()
    }

    fn reveal_metadata(&self) -> Result<RollupMetadata, RuntimeError> {
        self.host.reveal_metadata()
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
