// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::state_backend::{self as backend, Layout};
use std::{alloc, marker::PhantomData, mem, ptr, slice};

/// In-memory state backend
pub struct InMemoryBackend<L> {
    backing_storage: *mut u8,
    layout: alloc::Layout,
    _pd: PhantomData<L>,
}

// We don't want to implement `Borrow` and `BorrowMut` at the moment.
#[allow(clippy::should_implement_trait)]
impl<L> InMemoryBackend<L>
where
    L: Layout,
{
    /// Allocate a backend for state types of layout `L`.
    pub fn new() -> (Self, L::Placed) {
        let placed = L::placed();

        let layout = alloc::Layout::from_size_align(placed.size(), placed.align()).unwrap();
        let backing_storage = unsafe { alloc::alloc_zeroed(layout) };

        let backend = Self {
            backing_storage,
            layout,
            _pd: PhantomData,
        };

        (backend, placed.into_location())
    }

    /// Borrow the backing storage.
    pub fn borrow(&self) -> &[u8] {
        // SAFETY: [slice::from_raw_parts_mut] is layout safe given we allocated t using
        // [self.layout] (u8 makes this easy). This slice is lifetime safe because of `'backend`.
        unsafe { slice::from_raw_parts(self.backing_storage, self.layout.size()) }
    }

    /// Borrow the backing storage mutably.
    pub fn borrow_mut(&mut self) -> &mut [u8] {
        // SAFETY: [slice::from_raw_parts_mut] is layout safe given we allocated t using
        // [self.layout] (u8 makes this easy). This slice is lifetime safe because of `'backend`.
        unsafe { slice::from_raw_parts_mut(self.backing_storage, self.layout.size()) }
    }
}

impl<T> Drop for InMemoryBackend<T> {
    fn drop(&mut self) {
        unsafe { alloc::dealloc(self.backing_storage, self.layout) }
    }
}

impl<T> Clone for InMemoryBackend<T> {
    fn clone(&self) -> Self {
        let backing_storage = unsafe {
            let ptr = alloc::alloc_zeroed(self.layout);
            ptr::copy_nonoverlapping(self.backing_storage, ptr, self.layout.size());
            ptr
        };
        Self {
            backing_storage,
            layout: self.layout,
            _pd: self._pd,
        }
    }
}

impl<L: Layout> backend::BackendManagement for InMemoryBackend<L> {
    type Manager<'backend> = SliceManager<'backend>;

    type ManagerRO<'backend> = SliceManagerRO<'backend>;
}

impl<L: Layout> backend::Backend for InMemoryBackend<L> {
    type Layout = L;

    fn allocate(
        &mut self,
        placed: backend::PlacedOf<Self::Layout>,
    ) -> backend::AllocatedOf<Self::Layout, Self::Manager<'_>> {
        let mut manager = SliceManager::new(self.borrow_mut());
        L::allocate(&mut manager, placed)
    }

    fn allocate_ro(
        &self,
        placed: backend::PlacedOf<Self::Layout>,
    ) -> backend::AllocatedOf<Self::Layout, Self::ManagerRO<'_>> {
        let mut manager = SliceManagerRO::new(self.borrow());
        L::allocate(&mut manager, placed)
    }

    fn read(&self, index: usize, buffer: &mut [u8]) {
        let len = buffer.len();
        assert!(index <= self.layout.size() - len);
        buffer.copy_from_slice(&self.borrow()[index..index + len]);
    }

    fn write(&mut self, index: usize, buffer: &[u8]) {
        let len = buffer.len();
        assert!(index <= self.layout.size() - len);
        self.borrow_mut()[index..index + len].copy_from_slice(buffer);
    }
}

/// Manager for in-memory backing storage
pub struct SliceManager<'backend> {
    backing_storage: usize,
    _lifetime: PhantomData<&'backend mut [u8]>,
}

impl<'backend> SliceManager<'backend> {
    /// Manage the given slice.
    pub fn new(backing_storage: &'backend mut [u8]) -> Self {
        Self {
            backing_storage: backing_storage.as_mut_ptr() as usize,
            _lifetime: PhantomData,
        }
    }
}

impl<'backend> backend::Manager for SliceManager<'backend> {
    type Region<E: backend::Elem, const LEN: usize> = &'backend mut [E; LEN];

    fn allocate_region<E: backend::Elem, const LEN: usize>(
        &mut self,
        loc: backend::Location<[E; LEN]>,
    ) -> Self::Region<E, LEN> {
        unsafe {
            let ptr = self.backing_storage + loc.offset();
            &mut *(ptr as *mut [E; LEN])
        }
    }

    type DynRegion<const LEN: usize> = &'backend mut [u8; LEN];

    fn allocate_dyn_region<const LEN: usize>(
        &mut self,
        loc: backend::Location<[u8; LEN]>,
    ) -> Self::DynRegion<LEN> {
        self.allocate_region::<u8, LEN>(loc)
    }

    #[inline]
    fn region_read<E: backend::Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        index: usize,
    ) -> E {
        // NOTE: This implementation must match that of SliceManagerRO.
        E::from_stored(&region[index])
    }

    #[inline]
    fn region_read_all<E: backend::Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
    ) -> Vec<E> {
        // NOTE: This implementation must match that of SliceManagerRO.

        let mut result = region.to_vec();

        // NOTE: If [E::from_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in result.iter_mut() {
            elem.from_stored_in_place();
        }

        result
    }

    #[inline]
    fn region_read_some<E: backend::Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        offset: usize,
        buffer: &mut [E],
    ) {
        // NOTE: This implementation must match that of SliceManagerRO.

        let length = buffer.len();

        // We copy first because this allows the compiler to pick a faster
        // copy mechanism instead of doing the copying in the for loop.
        buffer.copy_from_slice(&region[offset..offset + length]);

        // NOTE: If [E::from_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in buffer {
            elem.from_stored_in_place();
        }
    }

    #[inline]
    fn region_write<E: backend::Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) {
        region[index].store(&value)
    }

    #[inline]
    fn region_write_all<E: backend::Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        value: &[E],
    ) {
        // We copy first because this allows the compiler to pick a faster
        // copy mechanism instead of doing the copying in the for loop.
        region.copy_from_slice(value);

        // NOTE: If [E::to_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in region.iter_mut() {
            elem.to_stored_in_place();
        }
    }

    #[inline]
    fn region_write_some<E: backend::Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        buffer: &[E],
    ) {
        let length = buffer.len();
        let target = &mut region[index..index + length];

        // We copy first because this allows the compiler to pick a faster
        // copy mechanism instead of doing the copying in the for loop.
        target.copy_from_slice(buffer);

        // NOTE: If [E::to_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in target {
            elem.to_stored_in_place();
        }
    }

    #[inline]
    fn region_replace<E: backend::Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        mut value: E,
    ) -> E {
        value.to_stored_in_place();
        let mut value = mem::replace(&mut region[index], value);
        value.from_stored_in_place();
        value
    }
}

/// Read-only manager for in-memory backing storage
pub struct SliceManagerRO<'backend> {
    backing_storage: usize,
    _lifetime: PhantomData<&'backend [u8]>,
}

impl<'backend> SliceManagerRO<'backend> {
    /// Manage the given slice.
    pub fn new(backing_storage: &'backend [u8]) -> Self {
        Self {
            backing_storage: backing_storage.as_ptr() as usize,
            _lifetime: PhantomData,
        }
    }
}

macro_rules! read_only_write {
    () => {
        panic!("cannot write to an immutable reference to a region")
    };
}

impl<'backend> backend::Manager for SliceManagerRO<'backend> {
    type Region<E: backend::Elem, const LEN: usize> = &'backend [E; LEN];

    fn allocate_region<E: backend::Elem, const LEN: usize>(
        &mut self,
        loc: backend::Location<[E; LEN]>,
    ) -> Self::Region<E, LEN> {
        unsafe {
            let ptr = self.backing_storage + loc.offset();
            &*(ptr as *const [E; LEN])
        }
    }

    type DynRegion<const LEN: usize> = &'backend [u8; LEN];

    fn allocate_dyn_region<const LEN: usize>(
        &mut self,
        loc: backend::Location<[u8; LEN]>,
    ) -> Self::DynRegion<LEN> {
        self.allocate_region::<u8, LEN>(loc)
    }

    #[inline]
    fn region_read<E: backend::Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        index: usize,
    ) -> E {
        E::from_stored(&region[index])
    }

    #[inline]
    fn region_read_all<E: backend::Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
    ) -> Vec<E> {
        let mut result = region.to_vec();

        // NOTE: If [E::from_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in result.iter_mut() {
            elem.from_stored_in_place();
        }

        result
    }

    #[inline]
    fn region_read_some<E: backend::Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        offset: usize,
        buffer: &mut [E],
    ) {
        let length = buffer.len();

        // We copy first because this allows the compiler to pick a faster
        // copy mechanism instead of doing the copying in the for loop.
        buffer.copy_from_slice(&region[offset..offset + length]);

        // NOTE: If [E::from_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in buffer {
            elem.from_stored_in_place();
        }
    }

    #[inline]
    fn region_write<E: backend::Elem, const LEN: usize>(
        _region: &mut Self::Region<E, LEN>,
        _index: usize,
        _value: E,
    ) {
        read_only_write!()
    }

    #[inline]
    fn region_write_all<E: backend::Elem, const LEN: usize>(
        _region: &mut Self::Region<E, LEN>,
        _value: &[E],
    ) {
        read_only_write!()
    }

    #[inline]
    fn region_write_some<E: backend::Elem, const LEN: usize>(
        _region: &mut Self::Region<E, LEN>,
        _index: usize,
        _buffer: &[E],
    ) {
        read_only_write!()
    }

    #[inline]
    fn region_replace<E: backend::Elem, const LEN: usize>(
        _region: &mut Self::Region<E, LEN>,
        _index: usize,
        _value: E,
    ) -> E {
        read_only_write!()
    }
}

pub mod test_helpers {
    use super::InMemoryBackend;
    use crate::state_backend::{test_helpers::TestBackendFactory, Layout};

    pub struct InMemoryBackendFactory;

    impl TestBackendFactory for InMemoryBackendFactory {
        type Backend<L: Layout> = InMemoryBackend<L>;

        fn new<L: Layout>() -> Self::Backend<L> {
            InMemoryBackend::<L>::new().0
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::state_backend::{
        AllocatedOf, Array, Atom, Backend, Cell, CellRead, CellWrite, Cells, Manager,
    };

    #[test]
    fn test_ro() {
        type L = (Atom<u64>, Array<u32, 4>);

        struct T<M: Manager> {
            first: Cell<u64, M>,
            second: Cells<u32, 4, M>,
        }

        impl<M: Manager> T<M> {
            fn bind(space: AllocatedOf<L, M>) -> Self {
                T {
                    first: space.0,
                    second: space.1,
                }
            }
        }

        let (backend, placed) = InMemoryBackend::<L>::new();

        let mut t = T::bind(backend.allocate_ro(placed));
        let r = t.second.read(0);

        assert_eq!(r, 0);

        let result = std::panic::catch_unwind(move || t.first.write(1373));
        assert!(result.is_err());
    }

    #[test]
    fn test_backend_reuse() {
        type L = (Atom<u64>, Array<u32, 4>);

        struct T<M: Manager> {
            first: Cell<u64, M>,
            second: Cells<u32, 4, M>,
        }

        impl<M: Manager> T<M> {
            fn bind(space: AllocatedOf<L, M>) -> Self {
                T {
                    first: space.0,
                    second: space.1,
                }
            }
        }

        let (mut backend, placed) = InMemoryBackend::<L>::new();

        const FIRST: u64 = 1337;
        const SECOND: [u32; 4] = [1, 3, 3, 7];

        {
            let mut instance = T::bind(backend.allocate(placed));
            instance.first.write(FIRST);
            instance.second.write_all(&SECOND);
        }

        {
            let instance = T::bind(backend.allocate(L::placed().into_location()));
            assert_eq!(instance.first.read(), FIRST);
            assert_eq!(instance.second.read_all(), SECOND);
        }
    }
}
