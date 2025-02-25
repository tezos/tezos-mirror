// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementations of system calls related to the file system

use super::{SupervisorState, error::Error};
use crate::{
    machine_state::{MachineCoreState, memory::MemoryConfig},
    state_backend::{ManagerBase, ManagerReadWrite},
};

impl<M: ManagerBase> SupervisorState<M> {
    /// Handle the `openat` system call. All access to the file system is denied.
    ///
    /// See: <https://www.man7.org/linux/man-pages/man3/openat.3p.html>
    pub(super) fn handle_openat(
        &mut self,
        core: &mut MachineCoreState<impl MemoryConfig, M>,
    ) -> bool
    where
        M: ManagerReadWrite,
    {
        core.hart.xregisters.write_system_call_error(Error::Access);
        true
    }

    /// Handle the `readlinkat` system call. All access to the file system is denied.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/readlink.2.html>
    pub(super) fn handle_readlinkat(
        &mut self,
        core: &mut MachineCoreState<impl MemoryConfig, M>,
    ) -> bool
    where
        M: ManagerReadWrite,
    {
        core.hart.xregisters.write_system_call_error(Error::Access);
        true
    }
}
