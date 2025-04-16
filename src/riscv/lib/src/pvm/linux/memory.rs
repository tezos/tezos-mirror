// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Linux-style memory management
//!
//! # Layout
//!
//! For a memory config `MC`, the layout consists of the following areas:
//!
//! - `0..program_start` is inaccessible
//! - `program_start..program_end` is the program code and data area
//! - `program_end..heap_start` is the area available for the program break
//! - `heap_start..stack_guard_start` is the heap area
//! - `stack_guard_start..stack_guard_start+PAGE_SIZE` is the stack guard page
//! - `stack_guard_start+PAGE_SIZE..MC::TOTAL_BYTES` is the stack area

use std::num::NonZeroU64;

use super::SupervisorState;
use super::addr::PageAligned;
use super::addr::VirtAddr;
use super::error::Error;
use super::parameters::AddressHint;
use super::parameters::Backend;
use super::parameters::Flags;
use super::parameters::NoFileDescriptor;
use super::parameters::Visibility;
use super::parameters::Zero;
use crate::machine_state::MachineCoreState;
use crate::machine_state::memory::Memory;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::memory::PAGE_SIZE;
use crate::machine_state::memory::Permissions;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerReadWrite;

/// Number of pages that make up the stack
const STACK_PAGES: u64 = 0x2000;

/// Maximum stack size in bytes
pub const STACK_SIZE: u64 = PAGE_SIZE.get() * STACK_PAGES;

impl<M: ManagerBase> SupervisorState<M> {
    /// Handle `brk` system call.
    ///
    /// We do not allow moving the program break. This system call can only be used to query the
    /// position of the program break.
    ///
    /// What does this mean for the user kernel? Musl's mallocng doesn't strictly need `brk` to
    /// work. If it detects that the program break can't be moved it will default to `mmap` to
    /// allocate smaller areas and allocator metadata.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/brk.2.html>
    pub(super) fn handle_brk(&self) -> Result<u64, Error>
    where
        M: ManagerReadWrite,
    {
        // The program break may not be moved
        Ok(self.program.end.to_machine_address())
    }

    /// Handle `madvise` system call.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/madvise.2.html>
    pub(super) fn handle_madvise(&mut self) -> Result<u64, Error> {
        // We don't make use of advice yet. We just return 0 to indicate success.
        Ok(0)
    }

    /// Handle `mprotect` system call.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/mprotect.2.html>
    pub(super) fn handle_mprotect<MC>(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        addr: PageAligned<VirtAddr>,
        length: u64,
        perms: Permissions,
    ) -> Result<u64, Error>
    where
        MC: MemoryConfig,
        M: ManagerReadWrite,
    {
        core.main_memory
            .protect_pages(addr.to_machine_address(), length as usize, perms)?;

        // Return 0 to indicate success.
        Ok(0)
    }

    /// Handle `mmap` system call.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/mmap.2.html>
    #[expect(
        clippy::too_many_arguments,
        reason = "The system call dispatch mechanism needs these arguments to exist, they can't be on a nested structure"
    )]
    pub(super) fn handle_mmap<MC>(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        addr: VirtAddr,
        length: NonZeroU64,
        perms: Permissions,
        flags: Flags,
        _fd: NoFileDescriptor,
        _offset: Zero,
    ) -> Result<u64, Error>
    where
        MC: MemoryConfig,
        M: ManagerReadWrite,
    {
        // We don't allow shared mappings
        match flags.visibility {
            Visibility::Private => {}
            Visibility::Shared => return Err(Error::NoSystemCall),
        }

        // We don't support file descriptors yet
        match flags.backend {
            Backend::None => {}
            Backend::File => return Err(Error::NoSystemCall),
        }

        let res_addr: VirtAddr = match flags.addr_hint {
            AddressHint::Hint => core.main_memory.allocate_and_protect_pages(
                None,
                length.get() as usize,
                perms,
                false,
            )?,

            AddressHint::Fixed { allow_replace } => {
                if !addr.is_aligned(PAGE_SIZE) {
                    return Err(Error::InvalidArgument);
                }

                core.main_memory.allocate_and_protect_pages(
                    Some(addr.to_machine_address()),
                    length.get() as usize,
                    perms,
                    allow_replace,
                )?
            }
        }
        .into();

        Ok(res_addr.to_machine_address())
    }

    /// Handle `munmap` system call.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/mmap.2.html>
    pub(super) fn handle_munmap<MC>(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        addr: u64,
        length: u64,
    ) -> Result<u64, Error>
    where
        MC: MemoryConfig,
        M: ManagerReadWrite,
    {
        core.main_memory
            .deallocate_and_protect_pages(addr, length as usize)
            .map_err(|_| Error::InvalidArgument)?;

        // TODO: RV-534: Mapped memory is never freed up to be re-allocated

        Ok(0)
    }
}
