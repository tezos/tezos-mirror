// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{AllocatedOf, Atom, Elem, Manager};
use std::mem;

/// Dedicated region in a [`super::Backend`]
pub trait Region {
    /// Type of elements in the region
    type Elem: Elem;

    /// Number of elements in the region
    const LEN: usize;

    /// Read an element in the region.
    fn read(&self, index: usize) -> Self::Elem;

    /// Read all elements in the region.
    fn read_all(&self) -> Vec<Self::Elem>;

    /// Read `buffer.len()` elements from the region, starting at `offset`.
    fn read_some(&self, offset: usize, buffer: &mut [Self::Elem]);

    /// Update an element in the region.
    fn write(&mut self, index: usize, value: Self::Elem);

    /// Update all elements in the region.
    fn write_all(&mut self, value: &[Self::Elem]);

    /// Update a subset of elements in the region starting at `index`.
    fn write_some(&mut self, index: usize, buffer: &[Self::Elem]);

    /// Update the element in the region and return the previous value.
    fn replace(&mut self, index: usize, value: Self::Elem) -> Self::Elem;
}

impl<E: Elem, const LEN: usize> Region for [E; LEN] {
    type Elem = E;

    const LEN: usize = LEN;

    #[inline(always)]
    fn read(&self, index: usize) -> E {
        E::from_stored(&self[index])
    }

    #[inline(always)]
    fn read_all(&self) -> Vec<E> {
        let mut result = self.to_vec();

        // NOTE: If [E::from_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in result.iter_mut() {
            elem.from_stored_in_place();
        }

        result
    }

    #[inline(always)]
    fn read_some(&self, offset: usize, buffer: &mut [E]) {
        let length = buffer.len();

        // We copy first because this allows the compiler to pick a faster
        // copy mechanism instead of doing the copying in the for loop.
        buffer.copy_from_slice(&self[offset..offset + length]);

        // NOTE: If [E::from_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in buffer {
            elem.from_stored_in_place();
        }
    }

    #[inline(always)]
    fn write(&mut self, index: usize, value: E) {
        self[index].store(&value)
    }

    #[inline(always)]
    fn write_all(&mut self, value: &[E]) {
        // We copy first because this allows the compiler to pick a faster
        // copy mechanism instead of doing the copying in the for loop.
        self.copy_from_slice(value);

        // NOTE: If [E::to_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in self {
            elem.to_stored_in_place();
        }
    }

    #[inline(always)]
    fn write_some(&mut self, index: usize, buffer: &[E]) {
        let length = buffer.len();
        let target = &mut self[index..index + length];

        // We copy first because this allows the compiler to pick a faster
        // copy mechanism instead of doing the copying in the for loop.
        target.copy_from_slice(buffer);

        // NOTE: If [E::to_stored_in_place] is inlined and ends up as a no-op, then
        // the optimiser can hopefully eliminate the entire loop.
        for elem in target {
            elem.to_stored_in_place();
        }
    }

    fn replace(&mut self, index: usize, mut value: E) -> E {
        value.to_stored_in_place();
        let mut value = mem::replace(&mut self[index], value);
        value.from_stored_in_place();
        value
    }
}

impl<E: Elem, T: Region<Elem = E>> Region for &mut T {
    type Elem = E;

    const LEN: usize = T::LEN;

    #[inline(always)]
    fn read(&self, index: usize) -> E {
        (self as &T).read(index)
    }

    #[inline(always)]
    fn read_all(&self) -> Vec<E> {
        (self as &T).read_all()
    }

    #[inline(always)]
    fn read_some(&self, offset: usize, buffer: &mut [E]) {
        (self as &T).read_some(offset, buffer)
    }

    #[inline(always)]
    fn write(&mut self, index: usize, value: E) {
        (self as &mut T).write(index, value)
    }

    #[inline(always)]
    fn write_all(&mut self, value: &[E]) {
        (self as &mut T).write_all(value)
    }

    #[inline(always)]
    fn write_some(&mut self, index: usize, buffer: &[E]) {
        (self as &mut T).write_some(index, buffer)
    }

    fn replace(&mut self, index: usize, value: E) -> E {
        (self as &mut T).replace(index, value)
    }
}

/// Convenience wrapper for [`Manager::Region<E, 1>`]
#[repr(transparent)]
pub struct Cell<E: Elem, M: Manager + ?Sized> {
    pub region: M::Region<E, 1>,
}

impl<E: Elem, M: Manager + ?Sized> Cell<E, M> {
    /// Read the value managed by the cell.
    #[inline(always)]
    pub fn read(&self) -> E {
        self.region.read(0)
    }

    /// Write the value managed by the cell.
    #[inline(always)]
    pub fn write(&mut self, value: E) {
        self.region.write(0, value)
    }
}

impl<E: Elem, M: Manager> Cell<E, M> {
    pub fn bind(space: AllocatedOf<Atom<E>, M>) -> Self {
        Self {
            region: space.region,
        }
    }
}

/// Dynamic region
pub trait DynRegion {
    /// Number of bytes in the region
    const LEN: usize;

    /// Read an element in the region. `address` is in bytes.
    fn read<E: Elem>(&self, address: usize) -> E;

    /// Update an element in the region. `address` is in bytes.
    fn write<E: Elem>(&mut self, address: usize, value: E);
}

impl<T, const LEN: usize> DynRegion for [T; LEN] {
    const LEN: usize = mem::size_of::<T>() * LEN;

    fn read<E: Elem>(&self, address: usize) -> E {
        assert!(address + mem::size_of::<E>() <= Self::LEN);
        unsafe {
            self.as_ptr()
                .cast::<u8>() // Calculate the offset in bytes
                .add(address)
                .cast::<E>()
                .read_unaligned()
        }
    }

    fn write<E: Elem>(&mut self, address: usize, value: E) {
        assert!(address + mem::size_of_val(&value) <= Self::LEN);
        unsafe {
            self.as_mut_ptr()
                .cast::<u8>() // Calculate the offset in bytes
                .add(address)
                .cast::<E>()
                .write_unaligned(value)
        }
    }
}

impl<T: DynRegion> DynRegion for &mut T {
    const LEN: usize = T::LEN;

    fn read<E: Elem>(&self, address: usize) -> E {
        (self as &T).read(address)
    }

    fn write<E: Elem>(&mut self, address: usize, value: E) {
        (self as &mut T).write(address, value)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::{
        backend_test,
        state_backend::{
            layout::{Atom, Layout},
            Array, Backend, Elem, Region,
        },
    };

    backend_test!(test_region_overlap, F, {
        const LEN: usize = 64;
        type OurLayout = (Array<u64, LEN>, Array<u64, LEN>);
        let mut backend = F::new::<OurLayout>();

        let (mut array1, mut array2) = backend.allocate(OurLayout::placed().into_location());

        // Allocate two consecutive arrays
        // let mut array1 = manager.allocate_region(array1_place);
        let mut array1_mirror = [0; LEN];

        for (i, item) in array1_mirror.iter_mut().enumerate() {
            // Ensure the array is zero-initialised.
            assert_eq!(array1.read(i), 0);

            // Then write something random in it.
            let value = rand::random();
            array1.write(i, value);
            assert_eq!(array1.read(i), value);

            // Retain the value for later.
            *item = value;
        }

        let array1_vec = array1.read_all();
        assert_eq!(array1_vec, array1_mirror);

        for i in 0..LEN {
            // Check the array is zero-initialised and that the first array
            // did not mess with the second array.
            assert_eq!(array2.read(i), 0);

            // Write a random value to it.
            let value = rand::random();
            array2.write(i, value);
            assert_eq!(array2.read(i), value);
        }

        for (i, item) in array1_mirror.into_iter().enumerate() {
            // Ensure that writing to the second array didn't mess with the
            // first array.
            assert_eq!(item, array1.read(i));
        }
    });

    backend_test!(test_cell_overlap, F, {
        type OurLayout = (Atom<[u64; 4]>, Atom<[u64; 4]>);
        let mut backend = F::new::<OurLayout>();
        let (mut cell1, mut cell2) = backend.allocate(OurLayout::placed().into_location());

        // Cell should be zero-initialised.
        assert_eq!(cell1.read(), [0; 4]);
        assert_eq!(cell2.read(), [0; 4]);

        // Write something to cell 1 and check.
        let cell1_value: [u64; 4] = rand::random();
        cell1.write(cell1_value);
        assert_eq!(cell1.read(), cell1_value);

        // Second cell should still be zero-initialised
        assert_eq!(cell2.read(), [0; 4]);

        // Write something to cell 2 and check.
        let cell2_value: [u64; 4] = rand::random();
        cell2.write(cell2_value);
        assert_eq!(cell2.read(), cell2_value);

        // Cell 1 should not have its value changed.
        assert_eq!(cell1.read(), cell1_value);
    });

    backend_test!(test_region_stored_format, F, {
        /// Dummy type that helps us implement custom normalisation via [Elem]
        #[repr(packed)]
        #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq)]
        struct Flipper {
            a: u8,
            b: u8,
        }

        impl Elem for Flipper {
            fn store(&mut self, source: &Self) {
                self.a = source.b;
                self.b = source.a;
            }

            fn to_stored_in_place(&mut self) {
                std::mem::swap(&mut self.a, &mut self.b);
            }

            fn from_stored_in_place(&mut self) {
                std::mem::swap(&mut self.b, &mut self.a);
            }

            fn from_stored(source: &Self) -> Self {
                Self {
                    a: source.b,
                    b: source.a,
                }
            }
        }

        type FlipperLayout = Array<Flipper, 4>;
        let mut backend = F::new::<FlipperLayout>();

        // Writing to one item of the region must convert to stored format.
        {
            let mut region = backend.allocate(FlipperLayout::placed().into_location());

            region.write(0, Flipper { a: 13, b: 37 });
            assert_eq!(region.read(0), Flipper { a: 13, b: 37 });
        }

        let mut buffer = [0; 2];
        backend.read(0, &mut buffer);
        assert_eq!(buffer, [37, 13]);

        // Replacing a value in the region must convert to and from stored format.
        {
            let mut region = backend.allocate(FlipperLayout::placed().into_location());
            let old = region.replace(0, Flipper { a: 26, b: 74 });
            assert_eq!(old, Flipper { a: 13, b: 37 });
        }

        let mut buffer = [0; 2];
        backend.read(0, &mut buffer);
        assert_eq!(buffer, [74, 26]);

        // Writing to sub-section must convert to stored format.
        {
            let mut region = backend.allocate(FlipperLayout::placed().into_location());

            region.write_some(1, &[Flipper { a: 1, b: 2 }, Flipper { a: 3, b: 4 }]);

            let mut buffer = [Flipper { a: 0, b: 0 }; 2];
            region.read_some(1, &mut buffer);
            assert_eq!(buffer, [Flipper { a: 1, b: 2 }, Flipper { a: 3, b: 4 }]);
        }

        let mut buffer = [0; 4];
        backend.read(2, &mut buffer);
        assert_eq!(buffer, [2, 1, 4, 3]);

        // Writing to the entire region must convert properly to stored format.
        {
            let mut region = backend.allocate(FlipperLayout::placed().into_location());

            region.write_all(&[
                Flipper { a: 11, b: 22 },
                Flipper { a: 13, b: 24 },
                Flipper { a: 15, b: 26 },
                Flipper { a: 17, b: 28 },
            ]);

            assert_eq!(
                region.read_all(),
                [
                    Flipper { a: 11, b: 22 },
                    Flipper { a: 13, b: 24 },
                    Flipper { a: 15, b: 26 },
                    Flipper { a: 17, b: 28 },
                ]
            );
        }

        let mut buffer = [0; 8];
        backend.read(0, &mut buffer);
        assert_eq!(buffer, [22, 11, 24, 13, 26, 15, 28, 17]);
    });
}
