// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{
    AllocatedOf, Elem, Layout, Location, ManagerAlloc, ManagerBase, ManagerRead, ManagerReadWrite,
    ManagerWrite,
};
use std::mem;

/// Manager that allows state binders to own the state storage
pub struct Owned;

impl Owned {
    /// Allocate regions for the given layout.
    pub fn allocate<L: Layout>() -> AllocatedOf<L, Self> {
        let places = L::placed();
        L::allocate(&mut Self, places.into_location())
    }
}

impl ManagerBase for Owned {
    type Region<E: Elem, const LEN: usize> = [E; LEN];

    type DynRegion<const LEN: usize> = Box<[u8; LEN]>;
}

impl ManagerAlloc for Owned {
    fn allocate_region<E: Elem, const LEN: usize>(
        &mut self,
        _loc: Location<[E; LEN]>,
    ) -> Self::Region<E, LEN> {
        unsafe { std::mem::zeroed() }
    }

    fn allocate_dyn_region<const LEN: usize>(
        &mut self,
        _loc: Location<[u8; LEN]>,
    ) -> Self::DynRegion<LEN> {
        unsafe {
            let layout = std::alloc::Layout::new::<[u8; LEN]>();
            let alloc = std::alloc::alloc_zeroed(layout);
            Box::from_raw(alloc.cast())
        }
    }
}

impl ManagerRead for Owned {
    fn region_read<E: Elem, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> E {
        region[index]
    }

    fn region_read_all<E: Elem, const LEN: usize>(region: &Self::Region<E, LEN>) -> Vec<E> {
        region.to_vec()
    }

    fn region_read_some<E: Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        offset: usize,
        buffer: &mut [E],
    ) {
        let slice = &region[offset..][..buffer.len()];
        buffer.copy_from_slice(slice)
    }

    fn dyn_region_read<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
    ) -> E {
        {
            assert!(address + mem::size_of::<E>() <= LEN);

            let mut result = unsafe { region.as_ptr().add(address).cast::<E>().read_unaligned() };
            result.from_stored_in_place();

            result
        }
    }

    fn dyn_region_read_all<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
        values: &mut [E],
    ) {
        assert!(address + mem::size_of_val(values) <= LEN);

        unsafe {
            region
                .as_ptr()
                .add(address)
                .cast::<E>()
                .copy_to(values.as_mut_ptr(), values.len());
        }

        for elem in values.iter_mut() {
            elem.from_stored_in_place();
        }
    }
}

impl ManagerWrite for Owned {
    fn region_write<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) {
        region[index] = value;
    }

    fn region_write_all<E: Elem, const LEN: usize>(region: &mut Self::Region<E, LEN>, value: &[E]) {
        region.copy_from_slice(value)
    }

    fn region_write_some<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        buffer: &[E],
    ) {
        region[index..][..buffer.len()].copy_from_slice(buffer)
    }

    fn dyn_region_write<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        mut value: E,
    ) {
        assert!(address + mem::size_of_val(&value) <= LEN);

        value.to_stored_in_place();

        unsafe {
            region
                .as_mut_ptr()
                .add(address)
                .cast::<E>()
                .write_unaligned(value);
        }
    }

    fn dyn_region_write_all<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        values: &[E],
    ) {
        assert!(address + mem::size_of_val(values) <= LEN);

        unsafe {
            let ptr = region.as_mut_ptr().add(address).cast::<E>();

            for (i, mut value) in values.iter().copied().enumerate() {
                value.to_stored_in_place();
                ptr.add(i).write_unaligned(value)
            }
        }
    }
}

impl ManagerReadWrite for Owned {
    fn region_replace<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) -> E {
        mem::replace(&mut region[index], value)
    }
}
