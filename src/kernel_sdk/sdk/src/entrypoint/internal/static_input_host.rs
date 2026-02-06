// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module enables creating a [`Runtime`] which can use an inbox file
//! (read at compile time) as source for creating an [`Inbox`] for the runtime.
//!
//! To create the new [`Runtime`], an existing [`Runtime`] will need to be provided along
//! the contents of an [`Inbox`] in a serialized format with [`serde_json`]

use tezos_smart_rollup_host::debug::HostDebug;
use tezos_smart_rollup_host::storage::StorageV1;

use crate::core_unsafe::PREIMAGE_HASH_SIZE;
use crate::host::{Runtime, RuntimeError, ValueType};
use crate::storage::path::Path;
use crate::types::RollupDalParameters;
use crate::types::{Message, RollupMetadata};
use crate::utils::inbox::{file::InboxFile, Inbox, InboxBuilder};

/// Wrapper struct for creating a [`Runtime`] with a static inbox.
pub struct StaticInbox {
    inbox: Inbox,
}

impl StaticInbox {
    /// Create a new [`StaticInbox`] where `inbox` is the content of an [`InboxFile`].
    pub fn new_from_json(inbox: &str) -> Self {
        let messages: InboxFile = serde_json::from_str(inbox).unwrap();
        let mut builder = InboxBuilder::new();
        builder.add_inbox_messages(messages);
        let inbox = builder.build();

        Self { inbox }
    }

    /// Create the static input [`Runtime`] associated with the current inbox file.
    pub fn wrap_runtime<'runtime, R: Runtime>(
        &'runtime mut self,
        host: &'runtime mut R,
    ) -> impl Runtime + 'runtime {
        StaticInputHost {
            host,
            inbox: &mut self.inbox,
        }
    }
}

/// A smart rollup [`Runtime`] layer which uses a static inbox on top of another [`Runtime`].
struct StaticInputHost<'runtime, R> {
    host: &'runtime mut R,
    inbox: &'runtime mut Inbox,
}

impl<Host: HostDebug> HostDebug for StaticInputHost<'_, Host> {
    #[inline(always)]
    fn write_debug(&self, msg: &str) {
        self.host.write_debug(msg)
    }
}

impl<Host: StorageV1> StorageV1 for StaticInputHost<'_, Host> {
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

impl<R: Runtime> Runtime for StaticInputHost<'_, R> {
    #[inline(always)]
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.host.write_output(from)
    }

    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        let message = if let Some((level, id, bytes)) = self.inbox.next() {
            Some(Message::new(level, id, bytes))
        } else {
            // We want to consume the rollup's inbox, but not polute the kernel's view of the inbox
            // with that.
            self.host.read_input()?;
            None
        };

        Ok(message)
    }

    #[inline(always)]
    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.reveal_preimage(hash, destination)
    }

    #[inline(always)]
    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.host.mark_for_reboot()
    }

    #[inline(always)]
    fn reveal_metadata(&self) -> RollupMetadata {
        self.host.reveal_metadata()
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
