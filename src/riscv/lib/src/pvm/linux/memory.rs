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

use std::cmp::Ordering;
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
use crate::machine_state::memory::OutOfBounds;
use crate::machine_state::memory::PAGE_SIZE;
use crate::machine_state::memory::Permissions;
use crate::machine_state::registers;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerWrite;

/// Number of pages that make up the break area
const BREAK_PAGES: u64 = 0x1000;

/// Maximum break size in bytes
pub const BREAK_SIZE: u64 = PAGE_SIZE.get() * BREAK_PAGES;

/// Number of pages that make up the stack
const STACK_PAGES: u64 = 0x2000;

/// Maximum stack size in bytes
pub const STACK_SIZE: u64 = PAGE_SIZE.get() * STACK_PAGES;

/// Update the underlying memory mappings with the new permissions.
fn update_mappings<MC, M>(
    core: &mut MachineCoreState<MC, M>,
    addr: VirtAddr,
    length: NonZeroU64,
    perms: Permissions,
) -> Result<(), Error>
where
    MC: MemoryConfig,
    M: ManagerWrite,
{
    let machine_address = addr.to_machine_address();
    let length = length.get() as usize;
    core.main_memory
        .protect_pages(machine_address, length, perms)
        .map_err(|_: OutOfBounds| Error::NoMemory)
}

impl<M: ManagerBase> SupervisorState<M> {
    /// Handle `brk` system call.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/brk.2.html>
    pub(super) fn handle_brk<MC>(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        new_brk: VirtAddr,
    ) -> Result<u64, Error>
    where
        MC: MemoryConfig,
        M: ManagerReadWrite,
    {
        let brk = self.program_break.read();

        // When the new break address is NULL, we need to return the current program break
        if new_brk == 0 {
            return Ok(brk.to_machine_address());
        }

        // Ensure the program break is in the break area. This means at or above the end of the
        // program, and below or at the lowest heap address.
        let new_brk = if new_brk > self.heap.start || new_brk < self.program.end {
            brk
        } else {
            new_brk
        };

        match new_brk.cmp(&brk) {
            // Program break has been increased, we need to ensure the new area is accessible
            Ordering::Greater => {
                core.main_memory.protect_pages(
                    brk.to_machine_address(),
                    (new_brk - brk) as usize,
                    Permissions::ReadWrite,
                )?;
            }

            // Program break has been decreased, we need to unmap the unused area
            Ordering::Less => {
                core.main_memory.protect_pages(
                    new_brk.to_machine_address(),
                    (brk - new_brk) as usize,
                    Permissions::None,
                )?;
            }

            Ordering::Equal => {}
        }

        // Only update the program break after the mappings have been changed
        self.program_break.write(new_brk);

        Ok(new_brk.to_machine_address())
    }

    /// Handle `madvise` system call.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/madvise.2.html>
    pub(super) fn handle_madvise<MC>(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
    ) -> Result<bool, Error>
    where
        MC: MemoryConfig,
        M: ManagerWrite,
    {
        // We don't make use of advice yet. We just return 0 to indicate success.
        core.hart.xregisters.write(registers::a0, 0);

        Ok(true)
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
            .protect_pages(addr.to_machine_address(), length as usize, perms)
            .map_err(|_| Error::NoMemory)?;

        // Return 0 to indicate success.
        Ok(0)
    }

    /// Allocate a number of bytes on the heap.
    fn alloc_on_heap(&mut self, length: NonZeroU64) -> Result<VirtAddr, Error>
    where
        M: ManagerReadWrite,
    {
        let addr = self.heap_next_free.read();
        let Some(new_next_free) = (addr + length.get()).align_up(PAGE_SIZE) else {
            return Err(Error::NoMemory);
        };

        // Cannot allocate beyond the heap which ends at the stack guard
        if new_next_free > self.stack_guard.start {
            return Err(Error::NoMemory);
        }

        self.heap_next_free.write(new_next_free);

        Ok(addr)
    }

    /// Validate whether a fixed mapping would be valid.
    fn check_fixed_map_request(&self, addr: VirtAddr, length: NonZeroU64) -> Result<VirtAddr, Error>
    where
        M: ManagerRead,
    {
        // The address hint must be page aligned
        if !addr.is_aligned(PAGE_SIZE) {
            return Err(Error::InvalidArgument);
        };

        let Some(addr_after) = (addr + length.get()).align_up(PAGE_SIZE) else {
            return Err(Error::NoMemory);
        };

        let program_end = self.program.end;
        let program_break = self.program_break.read();

        // We only allow fixed mappings in the break area
        if addr_after > program_break || addr < program_end {
            return Err(Error::NoMemory);
        }

        Ok(addr)
    }

    /// Handle `mmap` system call.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/mmap.2.html>
    pub(super) fn handle_mmap<MC>(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        addr: VirtAddr,
        length: NonZeroU64,
        perms: Permissions,
        flags: Flags,
    ) -> Result<u64, Error>
    where
        MC: MemoryConfig,
        M: ManagerReadWrite,
    {
        // We don't support file descriptors yet
        let NoFileDescriptor = core.hart.xregisters.try_read(registers::a4)?;

        // Offset must be 0 as we don't support file descriptors
        let Zero = core.hart.xregisters.try_read(registers::a5)?;

        // We don't allow shared mappings
        match flags.visibility {
            Visibility::Private => {}
            Visibility::Shared { .. } => return Err(Error::NoSystemCall),
        }

        // We don't support file descriptors yet
        match flags.backend {
            Backend::None => {}
            Backend::File => return Err(Error::NoSystemCall),
        }

        let addr = match flags.addr_hint {
            AddressHint::Hint => self.alloc_on_heap(length)?,
            AddressHint::Fixed {
                allow_replace: false,
            } => {
                // We don't support checking whether a mapping would clobber an existing one.
                // Doing so poses a proof-size risk as we would possibly check all mappings.
                return Err(Error::InvalidArgument);
            }
            AddressHint::Fixed {
                allow_replace: true,
            } => self.check_fixed_map_request(addr, length)?,
        };

        // Without updating the underlying memory mappings, the mapped memory would not be accessible
        update_mappings(core, addr, length, perms)?;

        Ok(addr.to_machine_address())
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
            .protect_pages(addr, length as usize, Permissions::None)
            .map_err(|_| Error::InvalidArgument)?;

        // TODO: RV-534: Mapped memory is never freed up to be re-allocated

        Ok(0)
    }
}
