// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{
    hash::{Hash, HashError, RootHashable},
    owned_backend::Owned,
    AllocatedOf, Elem, Layout, ManagerAlloc, ManagerBase, ManagerRead, ManagerReadWrite,
    ManagerSerialise, ManagerWrite, StaticCopy,
};
use rand::Fill;
use std::{alloc, boxed::Box, slice};

/// Captured randomised region that can be compared and hashed
#[derive(Debug)]
pub struct Captured {
    layout: alloc::Layout,
    data: *const u8,
}

impl AsRef<[u8]> for Captured {
    fn as_ref(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.data, self.layout.size()) }
    }
}

impl PartialEq for Captured {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl RootHashable for Captured {
    fn hash(&self) -> Result<Hash, HashError> {
        Hash::blake2b_hash_bytes(self.as_ref())
    }
}

impl Drop for Captured {
    fn drop(&mut self) {
        unsafe {
            alloc::dealloc(self.data as *mut u8, self.layout);
        }
    }
}

/// Backend manager that randomises the allocated regions
pub struct Randomised<'a> {
    rng: rand::rngs::OsRng,
    regions: &'a mut Vec<Captured>,
}

impl<'a> Randomised<'a> {
    /// Allocate randomised regions.
    pub fn allocate<L: Layout>(regions: &'a mut Vec<Captured>) -> AllocatedOf<L, Self> {
        let mut backend = Self {
            rng: rand::rngs::OsRng,
            regions,
        };

        L::allocate(&mut backend)
    }
}

impl<'a> ManagerBase for Randomised<'a> {
    type Region<E: 'static, const LEN: usize> = &'a mut [E; LEN];

    type DynRegion<const LEN: usize> = &'a mut [u8; LEN];
}

impl<'a> ManagerAlloc for Randomised<'a> {
    fn allocate_region<E: 'static, const LEN: usize>(&mut self) -> Self::Region<E, LEN> {
        unsafe {
            let layout = alloc::Layout::new::<[E; LEN]>();
            let data = alloc::alloc(layout);

            slice::from_raw_parts_mut(data.cast::<u8>(), layout.size())
                .try_fill(&mut self.rng)
                .unwrap();

            self.regions.push(Captured { layout, data });

            &mut *data.cast::<[E; LEN]>()
        }
    }

    fn allocate_dyn_region<const LEN: usize>(&mut self) -> Self::DynRegion<LEN> {
        unsafe {
            let layout = alloc::Layout::new::<[u8; LEN]>();
            let data = alloc::alloc(layout);

            slice::from_raw_parts_mut(data.cast::<u8>(), layout.size())
                .try_fill(&mut self.rng)
                .unwrap();

            self.regions.push(Captured { layout, data });

            &mut *data.cast::<[u8; LEN]>()
        }
    }
}

impl<'a> ManagerRead for Randomised<'a> {
    fn region_read<E: StaticCopy, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        index: usize,
    ) -> E {
        Owned::region_read(region, index)
    }

    fn region_read_all<E: StaticCopy, const LEN: usize>(region: &Self::Region<E, LEN>) -> Vec<E> {
        Owned::region_read_all(region)
    }

    fn region_read_some<E: StaticCopy, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        offset: usize,
        buffer: &mut [E],
    ) {
        Owned::region_read_some(region, offset, buffer)
    }

    fn dyn_region_read<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
    ) -> E {
        // SAFETY: The pointer has been allocated using the global allocator and can therefore be
        // used with [`Box`]. Additionally, the box is leaked to prevent its destructor from being
        // called.
        let boxed = unsafe { Box::from_raw(region.as_ptr() as *mut [u8; LEN]) };

        let value = Owned::dyn_region_read(&boxed, address);

        // Prevent the destructor from running.
        Box::leak(boxed);

        value
    }

    fn dyn_region_read_all<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
        values: &mut [E],
    ) {
        // SAFETY: The pointer has been allocated using the global allocator and can therefore be
        // used with [`Box`]. Additionally, the box is leaked to prevent its destructor from being
        // called.
        let boxed = unsafe { Box::from_raw(region.as_ptr() as *mut [u8; LEN]) };

        Owned::dyn_region_read_all(&boxed, address, values);

        // Prevent the destructor from running.
        Box::leak(boxed);
    }
}

impl<'a> ManagerWrite for Randomised<'a> {
    fn region_write<E: StaticCopy, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) {
        Owned::region_write(region, index, value);
    }

    fn region_write_all<E: StaticCopy, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        value: &[E],
    ) {
        Owned::region_write_all(region, value);
    }

    fn region_write_some<E: StaticCopy, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        buffer: &[E],
    ) {
        Owned::region_write_some(region, index, buffer);
    }

    fn dyn_region_write<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        value: E,
    ) {
        // SAFETY: The pointer has been allocated using the global allocator and can therefore be
        // used with [`Box`]. Additionally, the box is leaked to prevent its destructor from being
        // called.
        let mut boxed = unsafe { Box::from_raw(region.as_mut_ptr() as *mut [u8; LEN]) };

        Owned::dyn_region_write(&mut boxed, address, value);

        // Prevent the destructor from running.
        Box::leak(boxed);
    }

    fn dyn_region_write_all<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        values: &[E],
    ) {
        // SAFETY: The pointer has been allocated using the global allocator and can therefore be
        // used with [`Box`]. Additionally, the box is leaked to prevent its destructor from being
        // called.
        let mut boxed = unsafe { Box::from_raw(region.as_mut_ptr() as *mut [u8; LEN]) };

        Owned::dyn_region_write_all(&mut boxed, address, values);

        // Prevent the destructor from running.
        Box::leak(boxed);
    }
}

impl<'a> ManagerReadWrite for Randomised<'a> {
    fn region_replace<E: StaticCopy, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) -> E {
        Owned::region_replace(region, index, value)
    }
}

impl<'a> ManagerSerialise for Randomised<'a> {
    fn serialise_region<E: serde::Serialize + 'static, const LEN: usize, S: serde::Serializer>(
        region: &Self::Region<E, LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        Owned::serialise_region(region, serializer)
    }

    fn serialise_dyn_region<const LEN: usize, S: serde::Serializer>(
        region: &Self::DynRegion<LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        // SAFETY: The pointer has been allocated using the global allocator and can therefore be
        // used with [`Box`]. Additionally, the box is leaked to prevent its destructor from being
        // called.
        let boxed = unsafe { Box::from_raw(region.as_ptr() as *mut [u8; LEN]) };

        let value = Owned::serialise_dyn_region(&boxed, serializer);

        // Prevent the destructor from running.
        Box::leak(boxed);

        value
    }
}
