// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::mem;

use super::Address;
use super::BadMemoryAccess;
use super::Memory;
use super::PAGE_SIZE;
use super::Permissions;
use super::buddy::Buddy;
use super::protection::PagePermissions;
use crate::state_backend::DynCells;
use crate::state_backend::Elem;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerWrite;

/// Machine's memory
pub struct MemoryImpl<const PAGES: usize, const TOTAL_BYTES: usize, B, M: ManagerBase> {
    /// Memory contents
    pub(super) data: DynCells<TOTAL_BYTES, M>,

    /// Read permissions per page
    pub(super) readable_pages: PagePermissions<PAGES, M>,

    /// Write permissions per page
    pub(super) writable_pages: PagePermissions<PAGES, M>,

    /// Execute permissions per page
    pub(super) executable_pages: PagePermissions<PAGES, M>,

    /// Allocation tracker
    pub(super) allocated_pages: B,
}

impl<const PAGES: usize, const TOTAL_BYTES: usize, B, M: ManagerBase>
    MemoryImpl<PAGES, TOTAL_BYTES, B, M>
{
    /// Ensure the access is within bounds.
    #[inline]
    fn check_bounds<E>(address: Address, length: usize, error: E) -> Result<(), E> {
        if length > TOTAL_BYTES.saturating_sub(address as usize) {
            return Err(error);
        }

        Ok(())
    }

    /// Mark the whole memory as readable and writeable
    #[cfg(test)]
    pub(crate) fn set_all_readable_writeable(&mut self)
    where
        B: Buddy<M>,
        M: ManagerReadWrite,
    {
        self.protect_pages(0, TOTAL_BYTES, Permissions::ReadWrite)
            .unwrap();
    }

    /// Update an element in the region without checking memory protections. `address` is in bytes.
    #[cfg(test)]
    pub(crate) fn write_instruction_unchecked<E>(
        &mut self,
        address: Address,
        value: E,
    ) -> Result<(), BadMemoryAccess>
    where
        E: Elem,
        M: ManagerWrite,
    {
        let length = mem::size_of::<E>();
        Self::check_bounds(address, length, BadMemoryAccess)?;

        self.data.write(address as usize, value);
        self.readable_pages.modify_access(address, length, true);
        self.executable_pages.modify_access(address, length, true);
        Ok(())
    }
}

impl<const PAGES: usize, const TOTAL_BYTES: usize, B, M> Memory<M>
    for MemoryImpl<PAGES, TOTAL_BYTES, B, M>
where
    B: Buddy<M>,
    M: ManagerBase,
{
    #[inline]
    fn read<E>(&self, address: Address) -> Result<E, BadMemoryAccess>
    where
        E: Elem,
        M: ManagerRead,
    {
        Self::check_bounds(address, mem::size_of::<E>(), BadMemoryAccess)?;

        // SAFETY: The bounds check above ensures the access check below is safe
        unsafe {
            if !self.readable_pages.can_access(address, mem::size_of::<E>()) {
                return Err(BadMemoryAccess);
            }
        }

        Ok(self.data.read(address as usize))
    }

    #[inline]
    fn read_exec<E>(&self, address: Address) -> Result<E, BadMemoryAccess>
    where
        E: Elem,
        M: ManagerRead,
    {
        Self::check_bounds(address, mem::size_of::<E>(), BadMemoryAccess)?;

        // SAFETY: The bounds check above ensures the access check below is safe
        unsafe {
            // Checking for executable access is sufficient as that implies read access
            if !self
                .executable_pages
                .can_access(address, mem::size_of::<E>())
            {
                return Err(BadMemoryAccess);
            }
        }

        Ok(self.data.read(address as usize))
    }

    fn read_all<E>(&self, address: Address, values: &mut [E]) -> Result<(), BadMemoryAccess>
    where
        E: Elem,
        M: ManagerRead,
    {
        Self::check_bounds(address, mem::size_of_val(values), BadMemoryAccess)?;

        // SAFETY: The bounds check above ensures the access check below is safe
        unsafe {
            if !self
                .readable_pages
                .can_access(address, mem::size_of_val(values))
            {
                return Err(BadMemoryAccess);
            }
        }

        self.data.read_all(address as usize, values);
        Ok(())
    }

    #[inline]
    fn write<E>(&mut self, address: Address, value: E) -> Result<(), BadMemoryAccess>
    where
        E: Elem,
        M: ManagerReadWrite,
    {
        Self::check_bounds(address, mem::size_of::<E>(), BadMemoryAccess)?;

        // SAFETY: The bounds check above ensures the access check below is safe
        unsafe {
            if !self.writable_pages.can_access(address, mem::size_of::<E>()) {
                return Err(BadMemoryAccess);
            }
        }

        self.data.write(address as usize, value);
        Ok(())
    }

    fn write_all<E>(&mut self, address: Address, values: &[E]) -> Result<(), BadMemoryAccess>
    where
        E: Elem,
        M: ManagerReadWrite,
    {
        Self::check_bounds(address, mem::size_of_val(values), BadMemoryAccess)?;

        // SAFETY: The bounds check above ensures the access check below is safe
        unsafe {
            if !self
                .writable_pages
                .can_access(address, mem::size_of_val(values))
            {
                return Err(BadMemoryAccess);
            }
        }

        self.data.write_all(address as usize, values);
        Ok(())
    }

    fn clone(&self) -> Self
    where
        M: ManagerClone,
    {
        Self {
            data: self.data.clone(),
            readable_pages: self.readable_pages.clone(),
            writable_pages: self.writable_pages.clone(),
            executable_pages: self.executable_pages.clone(),
            allocated_pages: self.allocated_pages.clone(),
        }
    }

    fn reset(&mut self)
    where
        M: ManagerWrite,
    {
        const SIZE_OF_U64: usize = mem::size_of::<u64>();

        let mut address = 0;
        let mut outstanding = TOTAL_BYTES;

        // Write 64-bit chunks
        while outstanding >= SIZE_OF_U64 {
            self.data.write(address, 0u64);
            address += SIZE_OF_U64;
            outstanding -= SIZE_OF_U64;
        }

        // Write remaining bytes
        for i in 0..outstanding {
            self.data.write(address.saturating_add(i), 0u8);
        }
    }

    fn protect_pages(
        &mut self,
        address: Address,
        length: usize,
        perms: Permissions,
    ) -> Result<(), super::MemoryGovernanceError>
    where
        M: ManagerWrite,
    {
        Self::check_bounds(address, length, super::MemoryGovernanceError)?;

        self.readable_pages
            .modify_access(address, length, perms.can_read());
        self.writable_pages
            .modify_access(address, length, perms.can_write());
        self.executable_pages
            .modify_access(address, length, perms.can_exec());

        Ok(())
    }

    fn deallocate_pages(
        &mut self,
        address: Address,
        length: usize,
    ) -> Result<(), super::MemoryGovernanceError>
    where
        M: ManagerReadWrite,
    {
        Self::check_bounds(address, length, super::MemoryGovernanceError)?;

        // See RV-561: Use `u64` for indices and lengths that come from the PVM
        let pages = (length as u64).div_ceil(super::PAGE_SIZE.get());

        // Buddy memory manager works on page indices, not addresses
        let idx = address >> super::OFFSET_BITS;
        self.allocated_pages.deallocate(idx, pages);

        Ok(())
    }

    fn allocate_pages(
        &mut self,
        address_hint: Option<Address>,
        length: usize,
        allow_replace: bool,
    ) -> Result<Address, super::MemoryGovernanceError>
    where
        M: ManagerReadWrite,
    {
        // The interface works on usize at the moment, however, going forward we'll convert all
        // length types to u64 to avoid machine-specific behavior for lengths.
        // See RV-561: Use `u64` for indices and lengths that come from the PVM
        let pages = (length as u64).div_ceil(super::PAGE_SIZE.get());

        match address_hint {
            // Caller wants to allocate at a specific address
            Some(address) => {
                Self::check_bounds(address, length, super::MemoryGovernanceError)?;

                // Buddy memory manager works on page indices, not addresses
                let idx = address >> super::OFFSET_BITS;
                self.allocated_pages
                    .allocate_fixed(idx, pages, allow_replace)
                    .map(|()| address)
            }

            // Allocate anywhere
            None => self.allocated_pages.allocate(pages).map(|idx| {
                // Convert page index to address
                idx << super::OFFSET_BITS
            }),
        }
        .ok_or(super::MemoryGovernanceError)
    }

    fn allocate_and_protect_pages(
        &mut self,
        address_hint: Option<Address>,
        length: usize,
        perms: Permissions,
        allow_replace: bool,
    ) -> Result<Address, super::MemoryGovernanceError>
    where
        M: ManagerReadWrite,
    {
        // Mark the page range as occupied
        let address = self.allocate_pages(address_hint, length, allow_replace)?;

        // Configure the permissions on the page range
        if self.protect_pages(address, length, perms).is_err() {
            self.deallocate_pages(address, length)?;
        }

        // Zero initialise in 8-byte chunks. Using larger writes first, means we do fewer writes
        // altogether. This speeds things up.
        // As we allocate in multiples of pages, we must also clear in multiples of pages.
        let mut remaining = length
            .div_ceil(PAGE_SIZE.get() as usize)
            .saturating_mul(PAGE_SIZE.get() as usize);

        while remaining >= 8 {
            remaining -= 8;
            let address = (address as usize).saturating_add(remaining);
            self.data.write(address, 0u64);
        }

        // Zero initialise the tail byte by byte
        for i in 0..remaining {
            let address = (address as usize).saturating_add(i);
            self.data.write(address, 0u8);
        }

        Ok(address)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::backend_test;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::memory::MemoryConfig;
    use crate::state::NewState;
    use crate::state_backend::owned_backend::Owned;

    #[test]
    fn bounds_check() {
        type OwnedM0 = MemoryImpl<0, 0, (), Owned>;
        type OwnedM4K = <M4K as MemoryConfig>::State<Owned>;

        // Zero-sized reads or writes are always valid
        assert!(OwnedM0::check_bounds(0, 0, ()).is_ok());
        assert!(OwnedM0::check_bounds(1, 0, ()).is_ok());
        assert!(OwnedM4K::check_bounds(0, 0, ()).is_ok());
        assert!(OwnedM4K::check_bounds(4096, 0, ()).is_ok());
        assert!(OwnedM4K::check_bounds(2 * 4096, 0, ()).is_ok());

        // Bounds checks
        assert!(OwnedM0::check_bounds(0, 1, ()).is_err());
        assert!(OwnedM0::check_bounds(1, 1, ()).is_err());
        assert!(OwnedM4K::check_bounds(4096, 1, ()).is_err());
        assert!(OwnedM4K::check_bounds(2 * 4096, 1, ()).is_err());
    }

    // This test verifies that memory is fully zeroed up to the page boundary, not just the
    // requested length, when allocating memory.
    backend_test!(test_memory_fully_zeroed_on_allocation, F, {
        use crate::machine_state::memory::PAGE_SIZE;
        use crate::machine_state::memory::Permissions;

        let mut manager = F::manager();
        let mut memory = <<M4K as MemoryConfig>::State<_>>::new(&mut manager);

        // Write a pattern to ensure memory contains non-zero values
        for i in 0..PAGE_SIZE.get() {
            memory.data.write(i as usize, 0xFFu8);
        }

        // Request size that's not a multiple of page size
        let requested_size = (PAGE_SIZE.get() as usize) - 100;
        let address = memory
            .allocate_and_protect_pages(None, requested_size, Permissions::ReadWrite, false)
            .expect("Memory allocation should succeed");

        // Verify that memory is zeroed for the entire page, not just the requested length
        for i in 0..PAGE_SIZE.get() {
            let offset = i as usize;
            let value = memory.data.read::<u8>((address as usize) + offset);
            assert_eq!(
                value,
                0,
                "Memory at offset {} (address: {:#x}) should be zero, found {:#x}",
                offset,
                address + i,
                value
            );
        }
    });

    backend_test!(test_endianess, F, {
        let mut manager = F::manager();
        let mut memory = <<M4K as MemoryConfig>::State<_>>::new(&mut manager);

        memory
            .write_instruction_unchecked(0, 0x1122334455667788u64)
            .unwrap();

        macro_rules! check_address {
            ($ty:ty, $addr:expr, $value:expr) => {
                assert_eq!(memory.read::<$ty>($addr), Ok($value));
            };
        }

        check_address!(u64, 0, 0x1122334455667788);

        check_address!(u32, 0, 0x55667788);
        check_address!(u32, 4, 0x11223344);

        check_address!(u16, 0, 0x7788);
        check_address!(u16, 2, 0x5566);
        check_address!(u16, 4, 0x3344);
        check_address!(u16, 6, 0x1122);

        check_address!(u8, 0, 0x88);
        check_address!(u8, 1, 0x77);
        check_address!(u8, 2, 0x66);
        check_address!(u8, 3, 0x55);
        check_address!(u8, 4, 0x44);
        check_address!(u8, 5, 0x33);
        check_address!(u8, 6, 0x22);
        check_address!(u8, 7, 0x11);
    });
}
