// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{Elem, Manager};
use std::mem;

/// Single element of type `E`
#[repr(transparent)]
pub struct Cell<E: Elem, M: Manager + ?Sized> {
    region: Cells<E, 1, M>,
}

impl<E: Elem, M: Manager> Cell<E, M> {
    /// Bind this state to the single element region.
    pub fn bind(region: M::Region<E, 1>) -> Self {
        Self {
            region: Cells::bind(region),
        }
    }
}

/// A cell that support reading only.
pub trait CellRead {
    /// Element type managed by the cell.
    type Value;

    /// Read the value managed by the cell.
    fn read(&self) -> Self::Value;
}

impl<E: Elem, M: Manager> CellRead for Cell<E, M> {
    type Value = E;

    #[inline(always)]
    fn read(&self) -> E {
        self.region.read(0)
    }
}

impl<E: CellRead> CellRead for &E {
    type Value = E::Value;

    fn read(&self) -> Self::Value {
        E::read(self)
    }
}

impl<E: CellRead> CellRead for &mut E {
    type Value = E::Value;

    fn read(&self) -> Self::Value {
        E::read(self)
    }
}

/// A cell that support writing.
pub trait CellWrite: CellRead {
    /// Write the value managed by the cell.
    fn write(&mut self, value: Self::Value);

    /// Replace the value managed by the cell, returning the old value.
    fn replace(&mut self, value: Self::Value) -> Self::Value;
}

impl<E: Elem, M: Manager> CellWrite for Cell<E, M> {
    #[inline(always)]
    fn write(&mut self, value: E) {
        self.region.write(0, value)
    }

    #[inline(always)]
    fn replace(&mut self, value: E) -> E {
        self.region.replace(0, value)
    }
}

impl<E: CellWrite> CellWrite for &mut E {
    fn write(&mut self, value: Self::Value) {
        E::write(self, value)
    }

    #[inline(always)]
    fn replace(&mut self, value: Self::Value) -> Self::Value {
        E::replace(self, value)
    }
}

/// Multiple elements of type `E`
#[repr(transparent)]
pub struct Cells<E: Elem, const LEN: usize, M: Manager + ?Sized> {
    region: M::Region<E, LEN>,
}

impl<E: Elem, const LEN: usize, M: Manager> Cells<E, LEN, M> {
    /// Bind this state to the given region.
    pub fn bind(region: M::Region<E, LEN>) -> Self {
        Self { region }
    }

    /// Read an element in the region.
    #[inline]
    pub fn read(&self, index: usize) -> E {
        M::region_read(&self.region, index)
    }

    /// Read all elements in the region.
    #[inline]
    pub fn read_all(&self) -> Vec<E> {
        M::region_read_all(&self.region)
    }

    /// Read `buffer.len()` elements from the region, starting at `offset`.
    #[inline]
    pub fn read_some(&self, offset: usize, buffer: &mut [E]) {
        M::region_read_some(&self.region, offset, buffer)
    }

    /// Update an element in the region.
    #[inline]
    pub fn write(&mut self, index: usize, value: E) {
        M::region_write(&mut self.region, index, value)
    }

    /// Update all elements in the region.
    #[inline]
    pub fn write_all(&mut self, value: &[E]) {
        M::region_write_all(&mut self.region, value)
    }

    /// Update a subset of elements in the region starting at `index`.
    #[inline]
    pub fn write_some(&mut self, index: usize, buffer: &[E]) {
        M::region_write_some(&mut self.region, index, buffer)
    }

    /// Update the element in the region and return the previous value.
    #[inline]
    pub fn replace(&mut self, index: usize, value: E) -> E {
        M::region_replace(&mut self.region, index, value)
    }
}

/// Dynamic region
pub trait DynRegion {
    /// Number of bytes in the region
    const LEN: usize;

    /// Read an element in the region. `address` is in bytes.
    fn read<E: Elem>(&self, address: usize) -> E;

    /// Read elements from the region. `address` is in bytes.
    fn read_all<E: Elem>(&self, address: usize, values: &mut [E]);

    /// Update an element in the region. `address` is in bytes.
    fn write<E: Elem>(&mut self, address: usize, value: E);

    /// Update multiple elements in the region. `address` is in bytes.
    fn write_all<E: Elem>(&mut self, address: usize, values: &[E]);
}

impl<T, const LEN: usize> DynRegion for [T; LEN] {
    const LEN: usize = mem::size_of::<T>() * LEN;

    fn read<E: Elem>(&self, address: usize) -> E {
        assert!(address + mem::size_of::<E>() <= Self::LEN);
        let mut result = unsafe {
            self.as_ptr()
                .cast::<u8>() // Calculate the offset in bytes
                .add(address)
                .cast::<E>()
                .read_unaligned()
        };
        result.from_stored_in_place();
        result
    }

    fn read_all<E: Elem>(&self, address: usize, values: &mut [E]) {
        assert!(address + mem::size_of_val(values) <= Self::LEN);
        unsafe {
            self.as_ptr()
                .cast::<u8>() // Calculate the offset in bytes
                .add(address)
                .cast::<E>()
                .copy_to(values.as_mut_ptr(), values.len())
        };
        for v in values.iter_mut() {
            v.from_stored_in_place();
        }
    }

    fn write<E: Elem>(&mut self, address: usize, mut value: E) {
        assert!(address + mem::size_of_val(&value) <= Self::LEN);
        value.to_stored_in_place();
        unsafe {
            self.as_mut_ptr()
                .cast::<u8>() // Calculate the offset in bytes
                .add(address)
                .cast::<E>()
                .write_unaligned(value)
        }
    }

    fn write_all<E: Elem>(&mut self, address: usize, values: &[E]) {
        assert!(address + mem::size_of_val(values) <= Self::LEN);

        unsafe {
            let ptr = self
                .as_mut_ptr()
                .cast::<u8>() // Calculate the offset in bytes
                .add(address)
                .cast::<E>();

            for (i, mut value) in values.iter().copied().enumerate() {
                value.to_stored_in_place();
                ptr.add(i).write_unaligned(value)
            }
        }
    }
}

impl<T: DynRegion> DynRegion for &mut T {
    const LEN: usize = T::LEN;

    fn read<E: Elem>(&self, address: usize) -> E {
        (self as &T).read(address)
    }

    fn read_all<E: Elem>(&self, address: usize, values: &mut [E]) {
        (self as &T).read_all(address, values)
    }

    fn write<E: Elem>(&mut self, address: usize, value: E) {
        (self as &mut T).write(address, value)
    }

    fn write_all<E: Elem>(&mut self, address: usize, values: &[E]) {
        (self as &mut T).write_all(address, values)
    }
}

macro_rules! read_only_write {
    () => {
        panic!("cannot write to an immutable reference to a region")
    };
}

impl<T: DynRegion> DynRegion for &T {
    const LEN: usize = T::LEN;

    fn read<E: Elem>(&self, address: usize) -> E {
        (self as &T).read(address)
    }

    fn read_all<E: Elem>(&self, address: usize, values: &mut [E]) {
        (self as &T).read_all(address, values)
    }

    fn write<E: Elem>(&mut self, _address: usize, _value: E) {
        read_only_write!()
    }

    fn write_all<E: Elem>(&mut self, _address: usize, _values: &[E]) {
        read_only_write!()
    }
}

/// Multiple elements of an unspecified type
pub struct DynCells<const LEN: usize, M: Manager + ?Sized> {
    region: M::DynRegion<LEN>,
}

impl<const LEN: usize, M: Manager> DynCells<LEN, M> {
    /// Bind this state to the given dynamic region.
    pub fn bind(region: M::DynRegion<LEN>) -> Self {
        Self { region }
    }

    /// Read an element in the region. `address` is in bytes.
    #[inline]
    pub fn read<E: Elem>(&self, address: usize) -> E {
        M::DynRegion::read(&self.region, address)
    }

    /// Read elements from the region. `address` is in bytes.
    #[inline]
    pub fn read_all<E: Elem>(&self, address: usize, values: &mut [E]) {
        M::DynRegion::read_all(&self.region, address, values)
    }

    /// Update an element in the region. `address` is in bytes.
    #[inline]
    pub fn write<E: Elem>(&mut self, address: usize, value: E) {
        M::DynRegion::write(&mut self.region, address, value)
    }

    /// Update multiple elements in the region. `address` is in bytes.
    #[inline]
    pub fn write_all<E: Elem>(&mut self, address: usize, values: &[E]) {
        M::DynRegion::write_all(&mut self.region, address, values)
    }
}

impl<const LEN: usize, M: Manager> DynRegion for DynCells<LEN, M> {
    const LEN: usize = LEN;

    #[inline]
    fn read<E: Elem>(&self, address: usize) -> E {
        M::DynRegion::read(&self.region, address)
    }

    #[inline]
    fn read_all<E: Elem>(&self, address: usize, values: &mut [E]) {
        M::DynRegion::read_all(&self.region, address, values)
    }

    #[inline]
    fn write<E: Elem>(&mut self, address: usize, value: E) {
        M::DynRegion::write(&mut self.region, address, value)
    }

    #[inline]
    fn write_all<E: Elem>(&mut self, address: usize, values: &[E]) {
        M::DynRegion::write_all(&mut self.region, address, values)
    }
}
#[cfg(test)]
pub(crate) mod tests {
    use super::DynRegion;
    use crate::{
        backend_test, create_backend,
        state_backend::{
            layout::{Atom, Layout},
            Array, Backend, CellRead, CellWrite, Choreographer, Elem, Location,
        },
    };

    /// Dummy type that helps us implement custom normalisation via [Elem]
    #[repr(packed)]
    #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Default)]
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

    const FLIPPER_SIZE: usize = std::mem::size_of::<Flipper>();

    backend_test!(test_region_overlap, F, {
        const LEN: usize = 64;
        type OurLayout = (Array<u64, LEN>, Array<u64, LEN>);

        let mut backend = create_backend!(OurLayout, F);
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
        let mut backend = create_backend!(OurLayout, F);
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

    #[should_panic]
    #[test]
    fn test_dynregion_oob() {
        const LEN: usize = 8;

        let mut flipper_array = [Flipper { a: 0, b: 0 }; LEN];

        // This should panic because we are trying to write an element at the address which corresponds to the end of the buffer
        DynRegion::write::<Flipper>(
            &mut flipper_array,
            LEN * FLIPPER_SIZE,
            Flipper { a: 1, b: 2 },
        );
    }

    backend_test!(test_dynregion_stored_format, F, {
        struct FlipperLayout;

        impl Layout for FlipperLayout {
            type Placed = Location<[u8; 1024]>;

            fn place_with(alloc: &mut Choreographer) -> Self::Placed {
                alloc.alloc()
            }

            type Allocated<B: super::Manager> = B::DynRegion<1024>;

            fn allocate<B: super::Manager>(
                backend: &mut B,
                placed: Self::Placed,
            ) -> Self::Allocated<B> {
                backend.allocate_dyn_region(placed)
            }
        }

        let mut backend = create_backend!(FlipperLayout, F);

        // Writing to one item of the region must convert to stored format.
        {
            let mut region = backend.allocate(FlipperLayout::placed().into_location());

            region.write(0, Flipper { a: 13, b: 37 });
            assert_eq!(region.read::<Flipper>(0), Flipper { a: 13, b: 37 });
        }

        let mut buffer = [0; 2];
        backend.read(0, &mut buffer);
        assert_eq!(buffer, [37, 13]);

        // Writing to the entire region must convert properly to stored format.
        {
            let mut region = backend.allocate(FlipperLayout::placed().into_location());

            region.write_all::<Flipper>(
                0,
                &[
                    Flipper { a: 11, b: 22 },
                    Flipper { a: 13, b: 24 },
                    Flipper { a: 15, b: 26 },
                    Flipper { a: 17, b: 28 },
                ],
            );

            let mut buff = [Flipper::default(); 4];
            region.read_all::<Flipper>(0, &mut buff);
            assert_eq!(
                buff,
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

    backend_test!(test_region_stored_format, F, {
        type FlipperLayout = Array<Flipper, 4>;

        let mut backend = create_backend!(FlipperLayout, F);

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
