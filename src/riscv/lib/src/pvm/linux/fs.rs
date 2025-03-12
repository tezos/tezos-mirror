// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementations of system calls related to the file system

use super::SupervisorState;
use super::error::Error;
use crate::machine_state::MachineCoreState;
use crate::machine_state::memory::Memory;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::registers;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerReadWrite;

impl<M: ManagerBase> SupervisorState<M> {
    /// Handle the `openat` system call. All access to the file system is denied.
    ///
    /// See: <https://www.man7.org/linux/man-pages/man3/openat.3p.html>
    pub(super) fn handle_openat(&mut self) -> Result<bool, Error>
    where
        M: ManagerReadWrite,
    {
        Err(Error::Access)
    }

    /// Handle the `readlinkat` system call. All access to the file system is denied.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/readlink.2.html>
    pub(super) fn handle_readlinkat(&mut self) -> Result<bool, Error>
    where
        M: ManagerReadWrite,
    {
        Err(Error::Access)
    }

    // Handle the `getcwd` system call. This is a simple implementation that returns the root
    // directory `/`.
    //
    // See: <https://man7.org/linux/man-pages/man3/getcwd.3.html>
    pub(super) fn handle_getcwd(
        &mut self,
        core: &mut MachineCoreState<impl MemoryConfig, M>,
    ) -> Result<bool, Error>
    where
        M: ManagerReadWrite,
    {
        const CWD: &[u8] = c"/".to_bytes_with_nul();

        let buffer = core.hart.xregisters.read(registers::a0);
        let length = core.hart.xregisters.read(registers::a1);

        if length == 0 && buffer != 0 {
            return Err(Error::InvalidArgument);
        }

        if (length as usize) < CWD.len() {
            return Err(Error::Range);
        }

        // TODO: RV-487: Memory mappings are not yet protected. We assume the kernel knows what
        // it's doing for now.
        core.main_memory.write_all(buffer, CWD)?;

        // Return the buffer address as an indicator of success
        core.hart.xregisters.write(registers::a0, buffer);

        Ok(true)
    }
}
