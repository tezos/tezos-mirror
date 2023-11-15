// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::{any::type_name, marker::PhantomData, mem};

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
#[derive(Debug)]
pub struct Choreographer {
    first_unallocated_byte: usize,
    max_align: usize,
}

impl Choreographer {
    /// Determines locations for all atoms in the given layout.
    pub fn place<L: super::Layout + ?Sized>() -> Placed<L::Placed> {
        let mut alloc = Self {
            first_unallocated_byte: 0,
            max_align: 1,
        };
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
        let offset = align_address(self.first_unallocated_byte, align);

        self.first_unallocated_byte = offset + size;
        self.max_align = self.max_align.max(align);

        Location {
            offset,
            _pd: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
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
}
