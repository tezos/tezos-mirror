// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::mem;

use super::Address;
use super::Memory;
use super::OutOfBounds;
#[cfg(feature = "supervisor")]
use super::Permissions;
#[cfg(feature = "supervisor")]
use super::protection::PagePermissions;
use crate::state_backend::DynCells;
use crate::state_backend::Elem;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerWrite;

/// Machine's memory
pub struct MemoryImpl<const PAGES: usize, const TOTAL_BYTES: usize, M: ManagerBase> {
    /// Memory contents
    pub(super) data: DynCells<TOTAL_BYTES, M>,

    /// Read permissions per page
    #[cfg(feature = "supervisor")]
    pub(super) readable_pages: PagePermissions<PAGES, M>,

    /// Write permissions per page
    #[cfg(feature = "supervisor")]
    pub(super) writable_pages: PagePermissions<PAGES, M>,

    /// Execute permissions per page
    #[cfg(feature = "supervisor")]
    pub(super) executable_pages: PagePermissions<PAGES, M>,
}

impl<const PAGES: usize, const TOTAL_BYTES: usize, M: ManagerBase>
    MemoryImpl<PAGES, TOTAL_BYTES, M>
{
    /// Ensure the access is within bounds.
    #[inline]
    fn check_bounds(address: Address, length: usize) -> Result<(), OutOfBounds> {
        if length > TOTAL_BYTES.saturating_sub(address as usize) {
            return Err(OutOfBounds);
        }

        Ok(())
    }
}

impl<const PAGES: usize, const TOTAL_BYTES: usize, M> Memory<M>
    for MemoryImpl<PAGES, TOTAL_BYTES, M>
where
    M: ManagerBase,
{
    #[inline]
    fn read<E>(&self, address: Address) -> Result<E, OutOfBounds>
    where
        E: Elem,
        M: ManagerRead,
    {
        Self::check_bounds(address, mem::size_of::<E>())?;

        // SAFETY: The bounds check above ensures the access check below is safe
        #[cfg(feature = "supervisor")]
        unsafe {
            if !self.readable_pages.can_access(address, mem::size_of::<E>()) {
                return Err(OutOfBounds);
            }
        }

        Ok(self.data.read(address as usize))
    }

    #[inline]
    fn read_exec<E>(&self, address: Address) -> Result<E, OutOfBounds>
    where
        E: Elem,
        M: ManagerRead,
    {
        Self::check_bounds(address, mem::size_of::<E>())?;

        // SAFETY: The bounds check above ensures the access check below is safe
        #[cfg(feature = "supervisor")]
        unsafe {
            // Checking for executable access is sufficient as that implies read access
            if !self
                .executable_pages
                .can_access(address, mem::size_of::<E>())
            {
                return Err(OutOfBounds);
            }
        }

        Ok(self.data.read(address as usize))
    }

    fn read_all<E>(&self, address: Address, values: &mut [E]) -> Result<(), OutOfBounds>
    where
        E: Elem,
        M: ManagerRead,
    {
        Self::check_bounds(address, mem::size_of_val(values))?;

        // SAFETY: The bounds check above ensures the access check below is safe
        #[cfg(feature = "supervisor")]
        unsafe {
            if !self
                .readable_pages
                .can_access(address, mem::size_of_val(values))
            {
                return Err(OutOfBounds);
            }
        }

        self.data.read_all(address as usize, values);
        Ok(())
    }

    #[inline]
    fn write<E>(&mut self, address: Address, value: E) -> Result<(), OutOfBounds>
    where
        E: Elem,
        M: ManagerReadWrite,
    {
        Self::check_bounds(address, mem::size_of::<E>())?;

        // SAFETY: The bounds check above ensures the access check below is safe
        #[cfg(feature = "supervisor")]
        unsafe {
            if !self.writable_pages.can_access(address, mem::size_of::<E>()) {
                return Err(OutOfBounds);
            }
        }

        self.data.write(address as usize, value);
        Ok(())
    }

    fn write_all<E>(&mut self, address: Address, values: &[E]) -> Result<(), OutOfBounds>
    where
        E: Elem,
        M: ManagerReadWrite,
    {
        Self::check_bounds(address, mem::size_of_val(values))?;

        // SAFETY: The bounds check above ensures the access check below is safe
        #[cfg(feature = "supervisor")]
        unsafe {
            if !self
                .writable_pages
                .can_access(address, mem::size_of_val(values))
            {
                return Err(OutOfBounds);
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
            #[cfg(feature = "supervisor")]
            readable_pages: self.readable_pages.clone(),
            #[cfg(feature = "supervisor")]
            writable_pages: self.writable_pages.clone(),
            #[cfg(feature = "supervisor")]
            executable_pages: self.executable_pages.clone(),
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

    #[cfg(feature = "supervisor")]
    fn protect_pages(
        &mut self,
        address: Address,
        length: usize,
        perms: Permissions,
    ) -> Result<(), OutOfBounds>
    where
        M: ManagerWrite,
    {
        Self::check_bounds(address, length)?;

        self.readable_pages
            .modify_access(address, length, perms.can_read());
        self.writable_pages
            .modify_access(address, length, perms.can_write());
        self.executable_pages
            .modify_access(address, length, perms.can_exec());

        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::backend_test;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::memory::MemoryConfig;
    use crate::state_backend::owned_backend::Owned;

    #[test]
    fn bounds_check() {
        type OwnedM0 = MemoryImpl<0, 0, Owned>;
        type OwnedM4K = <M4K as MemoryConfig>::State<Owned>;

        // Zero-sized reads or writes are always valid
        assert!(OwnedM0::check_bounds(0, 0).is_ok());
        assert!(OwnedM0::check_bounds(1, 0).is_ok());
        assert!(OwnedM4K::check_bounds(0, 0).is_ok());
        assert!(OwnedM4K::check_bounds(4096, 0).is_ok());
        assert!(OwnedM4K::check_bounds(2 * 4096, 0).is_ok());

        // Bounds checks
        assert!(OwnedM0::check_bounds(0, 1).is_err());
        assert!(OwnedM0::check_bounds(1, 1).is_err());
        assert!(OwnedM4K::check_bounds(4096, 1).is_err());
        assert!(OwnedM4K::check_bounds(2 * 4096, 1).is_err());
    }

    backend_test!(test_endianess, F, {
        let space = F::allocate::<<M4K as MemoryConfig>::Layout>();
        let mut memory = M4K::bind(space);

        memory.write(0, 0x1122334455667788u64).unwrap();

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
