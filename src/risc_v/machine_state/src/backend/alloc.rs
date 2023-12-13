// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::{
    any::type_name,
    cell::{self, RefCell},
    cmp,
    marker::PhantomData,
    mem,
    ops::Deref,
    rc::Rc,
};

/// Aligns the given `address` to `align` bytes.
pub(crate) const fn align_address(mut address: usize, align: usize) -> usize {
    let offset = address.rem_euclid(align);
    if offset > 0 {
        address += align - offset;
    }
    address
}

/// Location of state backend storage
#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location<T> {
    offset: usize,
    _pd: PhantomData<T>,
}

impl<T> Location<T> {
    /// Offset into the storage managed by the state backend.
    pub const fn offset(&self) -> usize {
        self.offset
    }

    /// Number of bytes occupied by the state.
    pub const fn size(&self) -> usize {
        mem::size_of::<T>()
    }
}

impl<T> Location<[T; 1]> {
    pub const fn as_atom(self) -> Location<T> {
        Location {
            offset: self.offset,
            _pd: PhantomData,
        }
    }
}

impl<T> Location<T> {
    pub const fn as_array(self) -> Location<[T; 1]> {
        Location {
            offset: self.offset,
            _pd: PhantomData,
        }
    }
}

/// Volatile is a wrapper that indicates locations in `T` may be aliased.
///
/// For example, overlapping locations must be marked as volatile. Volatile
/// regions may have multiple writers. Therefore memory access optimisations
/// must not be applied by the compiler. Consumers of this wrapper use this
/// information to inform the compiler of this.
#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Volatile<T> {
    inner: T,
}

impl<T> Volatile<Volatile<T>> {
    pub fn flatten(self) -> Volatile<T> {
        self.inner
    }
}

impl<A, B> Volatile<(Volatile<A>, Volatile<B>)> {
    /// Split up a volatile wrapper that consists of two inner volatile wrappers.
    pub fn split(self) -> (Volatile<A>, Volatile<B>) {
        (
            Volatile {
                inner: self.inner.0.inner,
            },
            Volatile {
                inner: self.inner.1.inner,
            },
        )
    }
}

impl<T> Deref for Volatile<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Location for something that must be treated as volatile
pub type VolatileLocation<T> = Volatile<Location<T>>;

/// Successfully placed location `L` with metadata
#[derive(Debug)]
pub struct Placed<L> {
    size: usize,
    align: usize,
    location: L,
}

impl<T> Placed<T> {
    /// Retrieve the total size of the state.
    pub const fn size(&self) -> usize {
        self.size
    }

    /// Retrieve the alignment required by the state.
    pub const fn align(&self) -> usize {
        self.align
    }

    /// Retrieve a reference to the location of the state.
    pub const fn location(&self) -> &T {
        &self.location
    }

    /// Get the owned location.
    pub fn into_location(self) -> T {
        self.location
    }
}

/// Allocator for locations in the state backend storage
pub struct Choreographer {
    first_unallocated_byte: usize,
    max_align: usize,
    one_time_align_hook: Option<Box<dyn FnOnce(usize) -> usize>>,
}

impl Choreographer {
    fn new() -> Self {
        Self {
            first_unallocated_byte: 0,
            max_align: 1,
            one_time_align_hook: None,
        }
    }

    /// Determines locations for all atoms in the given layout.
    pub fn place<L: super::Layout + ?Sized>() -> Placed<L::Placed> {
        let mut alloc = Self::new();
        let location = L::place_with(&mut alloc);

        // Prevent empty layouts.
        assert!(
            alloc.first_unallocated_byte > 0,
            "{}: Empty layouts are not allowed",
            type_name::<L>()
        );

        Placed {
            size: alloc.first_unallocated_byte,
            align: alloc.max_align,
            location,
        }
    }

    /// Allocate a location for `T`.
    pub fn alloc<T>(&mut self) -> Location<T> {
        let size = mem::size_of::<T>();
        assert!(
            size > 0,
            "{}: Zero-sized locations are not allowed",
            type_name::<T>()
        );

        let align = mem::align_of::<T>();
        let align = if let Some(with_align) = self.one_time_align_hook.take() {
            let new_align = (with_align)(align);
            assert!(new_align >= align);
            new_align
        } else {
            align
        };

        let offset = align_address(self.first_unallocated_byte, align);

        self.first_unallocated_byte = offset + size;
        self.max_align = self.max_align.max(align);

        Location {
            offset,
            _pd: PhantomData,
        }
    }

    /// Execute two location generating functions in parallel. This is useful
    /// for layouts that overlap.
    pub fn overlapping<F1, R1, F2, R2>(
        &mut self,
        top: F1,
        bottom: F2,
    ) -> (Volatile<R1>, Volatile<R2>)
    where
        F1: FnOnce(&mut Self) -> R1,
        F2: FnOnce(&mut Self) -> R2 + 'static,
        R2: 'static,
    {
        // XXX: [Rc] is used to not have to wrangle the borrow checker.
        let bottom = Rc::new(RefCell::new(Some(bottom)));
        let r2_cell = Rc::new(RefCell::new(None));
        let self_hook = Rc::new(RefCell::new(self.one_time_align_hook.take()));

        let (top_state, r1) = {
            let top_hook = {
                // We copy a few values so we can move them into the closure.
                let first_unallocated_byte = self.first_unallocated_byte;
                let max_align = self.max_align;
                let bottom = bottom.clone();
                let r2_cell = r2_cell.clone();
                let self_hook = self_hook.clone();

                // This hook shall figure out the alignment for the first location in the [top]
                // layout. It does so by trying to advance the [bottom] layouting process up to the
                // first allocation as well. At the point in time we have [top_alignment] and
                // [bottom_alignment]. The alignment we want to both layouts is exactly
                // `max(top_alignment, bottom_alignment)`.
                move |top_alignment| {
                    // This cell holds the alignment that we need for both layouts to overlap.
                    // Initially this is [top_alignment], but when we instantiate the bottom layout,
                    // this may be updated in case that layout requires a larger alignment.
                    // XXX: [Rc] is used to not have to wrangle the borrow checker.
                    let top_alignment = Rc::new(cell::Cell::new(top_alignment));

                    let bottom_hook = {
                        // Copy things that need to be moved into the closure.
                        let top_alignment = top_alignment.clone();
                        let self_hook = self_hook.clone();

                        move |bottom_alignment: usize| {
                            let align = cmp::max(bottom_alignment, top_alignment.get());

                            // We may need to apply the parent's alignment hook.
                            let align = match self_hook.borrow_mut().take() {
                                Some(self_hook) => self_hook(align),
                                None => align,
                            };

                            top_alignment.set(align);
                            align
                        }
                    };

                    let mut state = Choreographer {
                        first_unallocated_byte,
                        max_align,
                        one_time_align_hook: Some(Box::new(bottom_hook)),
                    };

                    // Consume [borrow] and populate [r2_cell].
                    let r2 = bottom.borrow_mut().take().unwrap()(&mut state);
                    *r2_cell.borrow_mut() = Some((state, r2));

                    let top_alignment = top_alignment.get();

                    // We must not forget to apply the hook in case it hasn't been applied already.
                    match self_hook.borrow_mut().take() {
                        Some(self_hook) => self_hook(top_alignment),
                        None => top_alignment,
                    }
                }
            };

            let mut state = Choreographer {
                first_unallocated_byte: self.first_unallocated_byte,
                max_align: self.max_align,
                one_time_align_hook: Some(Box::new(top_hook)),
            };

            let r1 = top(&mut state);

            (state, r1)
        };

        let (bottom_state, r2) = match bottom.take() {
            // This case occurs when the [top] layout allocated nothing, therefore we didn't call
            // the [bottom] layout either.
            Some(bottom) => {
                let mut state = Choreographer {
                    first_unallocated_byte: self.first_unallocated_byte,
                    max_align: self.max_align,
                    one_time_align_hook: self_hook.borrow_mut().take(),
                };

                let r2 = bottom(&mut state);

                // We have to write back the hook in case the [bottom] layout did not use it.
                // We only need to do this here because the hook would have been used in all other cases.
                self.one_time_align_hook = state.one_time_align_hook.take();

                (state, r2)
            }

            None => r2_cell.take().unwrap(),
        };

        // Write back the state.
        self.first_unallocated_byte = cmp::max(
            top_state.first_unallocated_byte,
            bottom_state.first_unallocated_byte,
        );
        self.max_align = cmp::max(top_state.max_align, bottom_state.max_align);

        (Volatile { inner: r1 }, Volatile { inner: r2 })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::{
        bits,
        prelude::Arbitrary,
        strategy::{self, Strategy},
    };
    use std::{mem, ops::Range};

    /// 48-bit address
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy)]
    pub(crate) struct ArbAddr(pub(crate) usize);

    impl Arbitrary for ArbAddr {
        type Parameters = ();

        type Strategy = strategy::Map<bits::BitSetStrategy<usize>, fn(usize) -> ArbAddr>;

        fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
            proptest::bits::usize::between(0, 48).prop_map(ArbAddr)
        }

        // ArbAddr(value)
    }

    /// 12-bit alignment
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy)]
    pub(crate) struct ArbAlign(pub(crate) usize);

    impl Arbitrary for ArbAlign {
        type Parameters = ();

        type Strategy = strategy::Map<Range<usize>, fn(usize) -> ArbAlign>;

        fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
            (0..12usize).prop_map(|bit| ArbAlign(1usize << bit))
        }
    }

    #[test]
    fn test_align_address() {
        proptest::proptest!(|(addr: ArbAddr, align: ArbAlign)| {
            proptest::prop_assert_eq!(
                super::align_address(addr.0, align.0).rem_euclid(align.0),
                0
            );
        });
    }

    #[test]
    fn test_choreographer_align() {
        use crate::backend::{Atom, Layout};

        type TestLayout = (Atom<[u8; 5]>, Atom<u64>);

        let placed = TestLayout::placed();

        assert_eq!(placed.size, 16);
        assert_eq!(placed.align, mem::align_of::<u64>());
        assert_eq!(placed.location.0.offset, 0);
        assert_eq!(placed.location.1.offset, placed.align);
    }

    #[test]
    fn test_choreographer_overlapping_align() {
        let mut alloc = Choreographer::new();

        alloc.alloc::<u8>();

        let (a, b) = alloc.overlapping(|alloc| alloc.alloc::<u8>(), |alloc| alloc.alloc::<u64>());

        assert_eq!(a.offset(), b.offset());
    }
}
