// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{
    owned_backend::Owned, Elem, Location, ManagerAlloc, ManagerBase, ManagerRead, ManagerReadWrite,
    ManagerWrite,
};
use rand::Fill;
use std::{cell, mem, rc, slice};

/// Backend manager that randomises the allocated regions
#[derive(Default)]
pub struct Randomised {
    rng: rand::rngs::OsRng,
    snapshoters: Vec<Box<dyn Fn() -> Vec<u8>>>,
}

impl Randomised {
    /// Collect snapshots of all allocated regions.
    pub fn snapshot_regions(&self) -> Vec<Vec<u8>> {
        self.snapshoters.iter().map(|f| f()).collect()
    }
}

impl ManagerBase for Randomised {
    type Region<E: Elem, const LEN: usize> = rc::Rc<cell::RefCell<[E; LEN]>>;

    type DynRegion<const LEN: usize> = rc::Rc<cell::RefCell<Box<[u8; LEN]>>>;
}

impl ManagerAlloc for Randomised {
    fn allocate_region<E: Elem, const LEN: usize>(
        &mut self,
        _loc: Location<[E; LEN]>,
    ) -> Self::Region<E, LEN> {
        let region = unsafe {
            let mut zeroed: [E; LEN] = mem::zeroed();

            slice::from_raw_parts_mut(zeroed.as_mut_ptr().cast::<u8>(), mem::size_of_val(&zeroed))
                .try_fill(&mut self.rng)
                .unwrap();

            zeroed
        };

        let region = rc::Rc::new(cell::RefCell::new(region));

        let region_clone = region.clone();
        self.snapshoters.push(Box::new(move || {
            let region = region_clone.borrow();
            let slice = unsafe {
                slice::from_raw_parts(region.as_ptr().cast::<u8>(), mem::size_of::<[E; LEN]>())
            };
            slice.to_vec()
        }));

        region
    }

    fn allocate_dyn_region<const LEN: usize>(
        &mut self,
        _loc: Location<[u8; LEN]>,
    ) -> Self::DynRegion<LEN> {
        let mut boxed: Box<[u8; LEN]> = unsafe {
            let layout = std::alloc::Layout::new::<[u8; LEN]>();
            let alloc = std::alloc::alloc_zeroed(layout);
            Box::from_raw(alloc.cast())
        };

        boxed.try_fill(&mut self.rng).unwrap();

        let region = rc::Rc::new(cell::RefCell::new(boxed));

        let region_clone = region.clone();
        self.snapshoters.push(Box::new(move || {
            let region = region_clone.borrow();
            let slice = unsafe { &*region.as_ptr().cast::<[u8; LEN]>() };
            slice.to_vec()
        }));

        region
    }
}

impl ManagerRead for Randomised {
    fn region_read<E: Elem, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> E {
        Owned::region_read(&region.borrow(), index)
    }

    fn region_read_all<E: Elem, const LEN: usize>(region: &Self::Region<E, LEN>) -> Vec<E> {
        Owned::region_read_all(&region.borrow())
    }

    fn region_read_some<E: Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        offset: usize,
        buffer: &mut [E],
    ) {
        Owned::region_read_some(&region.borrow(), offset, buffer)
    }

    fn dyn_region_read<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
    ) -> E {
        Owned::dyn_region_read(&region.borrow(), address)
    }

    fn dyn_region_read_all<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
        values: &mut [E],
    ) {
        Owned::dyn_region_read_all(&region.borrow(), address, values)
    }
}

impl ManagerWrite for Randomised {
    fn region_write<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) {
        Owned::region_write(&mut region.borrow_mut(), index, value)
    }

    fn region_write_all<E: Elem, const LEN: usize>(region: &mut Self::Region<E, LEN>, value: &[E]) {
        Owned::region_write_all(&mut region.borrow_mut(), value)
    }

    fn region_write_some<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        buffer: &[E],
    ) {
        Owned::region_write_some(&mut region.borrow_mut(), index, buffer)
    }

    fn dyn_region_write<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        value: E,
    ) {
        Owned::dyn_region_write(&mut region.borrow_mut(), address, value)
    }

    fn dyn_region_write_all<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        values: &[E],
    ) {
        Owned::dyn_region_write_all(&mut region.borrow_mut(), address, values)
    }
}

impl ManagerReadWrite for Randomised {
    fn region_replace<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) -> E {
        Owned::region_replace(&mut region.borrow_mut(), index, value)
    }
}
