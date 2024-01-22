// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::backend::{self, Layout};
use std::{alloc, marker::PhantomData, ptr, slice};

/// In-memory state backend
pub struct InMemoryBackend<L> {
    backing_storage: *mut u8,
    layout: alloc::Layout,
    _pd: PhantomData<L>,
}

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
    pub fn borrow<'backend>(&'backend self) -> &'backend [u8] {
        // SAFETY: [slice::from_raw_parts_mut] is layout safe given we allocated t using
        // [self.layout] (u8 makes this easy). This slice is lifetime safe because of `'backend`.
        unsafe { slice::from_raw_parts(self.backing_storage, self.layout.size()) }
    }

    /// Borrow the backing storage mutably.
    pub fn borrow_mut<'backend>(&'backend mut self) -> &'backend mut [u8] {
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

    fn allocate<'backend>(
        &'backend mut self,
        placed: backend::PlacedOf<Self::Layout>,
    ) -> backend::AllocatedOf<Self::Layout, Self::Manager<'backend>> {
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
    _lifetime: &'backend mut [u8],
}

impl<'backend> SliceManager<'backend> {
    /// Manage the given slice.
    pub fn new(backing_storage: &'backend mut [u8]) -> Self {
        Self {
            backing_storage: backing_storage.as_mut_ptr() as usize,
            _lifetime: backing_storage,
        }
    }
}

impl<'backend> backend::Manager for SliceManager<'backend> {
    type Region<E: backend::Elem, const LEN: usize> = &'backend mut [E; LEN];

    fn allocate_region<E: backend::Elem, const LEN: usize>(
        &mut self,
        loc: backend::Location<[E; LEN]>,
    ) -> Self::Region<E, LEN> {
        let backing_storage = unsafe {
            let ptr = self.backing_storage + loc.offset();
            &mut *(ptr as *mut [E; LEN])
        };

        backing_storage
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{AllocatedOf, Array, Atom, Backend, Cell, Manager, Region};

    #[test]
    fn test_backend_reuse() {
        type L = (Atom<u64>, Array<u32, 4>);

        struct T<M: Manager> {
            first: Cell<u64, M>,
            second: M::Region<u32, 4>,
        }

        impl<M: Manager> T<M> {
            fn new_in(space: AllocatedOf<L, M>) -> Self {
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
            let mut instance = T::new_in(backend.allocate(placed));
            instance.first.write(FIRST);
            instance.second.write_all(&SECOND);
        }

        {
            let instance = T::new_in(backend.allocate(L::placed().into_location()));
            assert_eq!(instance.first.read(), FIRST);
            assert_eq!(instance.second.read_all(), SECOND);
        }
    }

    #[test]
    fn test_backend() {
        struct InMemoryBackendFactory;

        impl backend::tests::TestBackendFactory for InMemoryBackendFactory {
            type Backend<L: Layout> = InMemoryBackend<L>;

            fn make<L: Layout>(&mut self) -> Self::Backend<L> {
                InMemoryBackend::<L>::new().0
            }
        }

        backend::tests::test_backend(&mut InMemoryBackendFactory);
    }
}
