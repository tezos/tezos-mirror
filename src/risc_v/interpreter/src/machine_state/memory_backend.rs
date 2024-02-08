// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::backend::{self, Layout};
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

    type VolatileRegion<E: backend::Elem, const LEN: usize> = VolatileRegion<'backend, E, LEN>;

    fn allocate_volatile_region<E: backend::Elem, const LEN: usize>(
        &mut self,
        loc: backend::Volatile<backend::Location<[E; LEN]>>,
    ) -> Self::VolatileRegion<E, LEN> {
        let backing_storage = (self.backing_storage + loc.offset()) as *mut E;
        VolatileRegion {
            backing_storage,
            _pd: PhantomData,
        }
    }
}

/// Region whose memory accesses are marked as "volatile"
///
/// See [std::ptr::read_volatile] and [std::ptr::write_volatile] for more information.
#[repr(transparent)]
pub struct VolatileRegion<'backend, E, const LEN: usize> {
    backing_storage: *mut E,
    _pd: PhantomData<&'backend mut [u8]>,
}

impl<'backend, E: backend::Elem, const LEN: usize> backend::VolatileRegion<E>
    for VolatileRegion<'backend, E, LEN>
{
    #[inline(always)]
    fn read(&self, index: usize) -> E {
        // Make sure the access is within bounds.
        debug_assert!(index < LEN);

        // The backing storage needs to be aligend properly for the resulting memory access to be
        // properly aligned as well.
        debug_assert_eq!(self.backing_storage.align_offset(mem::align_of::<E>()), 0);

        let mut elem = unsafe { self.backing_storage.add(index).read_volatile() };
        elem.from_stored_in_place();
        elem
    }

    #[inline(always)]
    fn write(&mut self, index: usize, mut value: E) {
        // Make sure the access is within bounds.
        debug_assert!(index < LEN);

        // The backing storage needs to be aligend properly for the resulting memory access to be
        // properly aligned as well.
        debug_assert_eq!(self.backing_storage.align_offset(mem::align_of::<E>()), 0);

        value.to_stored_in_place();
        unsafe { self.backing_storage.add(index).write_volatile(value) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::machine_state::backend::{AllocatedOf, Array, Atom, Backend, Cell, Manager, Region};

    #[test]
    fn test_backend_reuse() {
        type L = (Atom<u64>, Array<u32, 4>);

        struct T<M: Manager> {
            first: Cell<u64, M>,
            second: M::Region<u32, 4>,
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

    #[test]
    fn test_backend() {
        struct InMemoryBackendFactory;

        impl backend::tests::TestBackendFactory for InMemoryBackendFactory {
            type Backend<L: Layout> = InMemoryBackend<L>;

            fn new<L: Layout>() -> Self::Backend<L> {
                InMemoryBackend::<L>::new().0
            }
        }

        backend::tests::test_backend::<InMemoryBackendFactory>();
    }
}
