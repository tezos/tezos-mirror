// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{Elem, Manager, ManagerBase};

/// Single element of type `E`
#[repr(transparent)]
pub struct Cell<E: Elem, M: ManagerBase + ?Sized> {
    region: Cells<E, 1, M>,
}

impl<E: Elem, M: ManagerBase> Cell<E, M> {
    /// Bind this state to the single element region.
    pub fn bind(region: M::Region<E, 1>) -> Self {
        Self {
            region: Cells::bind(region),
        }
    }
}

/// A cell that support reading only.
pub trait CellBase {
    /// Element type managed by the cell.
    type Value;
}

impl<E: Elem, M: ManagerBase> CellBase for Cell<E, M> {
    type Value = E;
}

impl<E: CellBase> CellBase for &E {
    type Value = E::Value;
}

impl<E: CellBase> CellBase for &mut E {
    type Value = E::Value;
}

/// A cell that support reading only.
pub trait CellRead: CellBase {
    /// Read the value managed by the cell.
    fn read(&self) -> Self::Value;
}

impl<E: Elem, M: Manager> CellRead for Cell<E, M> {
    #[inline(always)]
    fn read(&self) -> E {
        self.region.read(0)
    }
}

impl<E: CellRead> CellRead for &E {
    fn read(&self) -> Self::Value {
        E::read(self)
    }
}

impl<E: CellRead> CellRead for &mut E {
    fn read(&self) -> Self::Value {
        E::read(self)
    }
}

/// A cell that support writing.
pub trait CellWrite: CellBase {
    /// Write the value managed by the cell.
    fn write(&mut self, value: Self::Value);
}

impl<E: Elem, M: Manager> CellWrite for Cell<E, M> {
    #[inline(always)]
    fn write(&mut self, value: E) {
        self.region.write(0, value)
    }
}

impl<E: CellWrite> CellWrite for &mut E {
    #[inline(always)]
    fn write(&mut self, value: Self::Value) {
        E::write(self, value)
    }
}

/// A cell that support reading and writing.
pub trait CellReadWrite: CellRead + CellWrite {
    /// Replace the value managed by the cell, returning the old value.
    fn replace(&mut self, value: Self::Value) -> Self::Value;
}

impl<E: Elem, M: Manager> CellReadWrite for Cell<E, M> {
    #[inline(always)]
    fn replace(&mut self, value: E) -> E {
        self.region.replace(0, value)
    }
}

impl<E: CellReadWrite> CellReadWrite for &mut E {
    #[inline(always)]
    fn replace(&mut self, value: Self::Value) -> Self::Value {
        E::replace(self, value)
    }
}

/// Multiple elements of type `E`
#[repr(transparent)]
pub struct Cells<E: Elem, const LEN: usize, M: ManagerBase + ?Sized> {
    region: M::Region<E, LEN>,
}

impl<E: Elem, const LEN: usize, M: ManagerBase> Cells<E, LEN, M> {
    /// Bind this state to the given region.
    pub fn bind(region: M::Region<E, LEN>) -> Self {
        Self { region }
    }
}

impl<E: Elem, const LEN: usize, M: Manager> Cells<E, LEN, M> {
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

/// Multiple elements of an unspecified type
pub struct DynCells<const LEN: usize, M: ManagerBase + ?Sized> {
    region: M::DynRegion<LEN>,
}

impl<const LEN: usize, M: ManagerBase> DynCells<LEN, M> {
    /// Bind this state to the given dynamic region.
    pub fn bind(region: M::DynRegion<LEN>) -> Self {
        Self { region }
    }
}

impl<const LEN: usize, M: Manager> DynCells<LEN, M> {
    /// Read an element in the region. `address` is in bytes.
    #[inline]
    pub fn read<E: Elem>(&self, address: usize) -> E {
        M::dyn_region_read(&self.region, address)
    }

    /// Read elements from the region. `address` is in bytes.
    #[inline]
    pub fn read_all<E: Elem>(&self, address: usize, values: &mut [E]) {
        M::dyn_region_read_all(&self.region, address, values)
    }

    /// Update an element in the region. `address` is in bytes.
    #[inline]
    pub fn write<E: Elem>(&mut self, address: usize, value: E) {
        M::dyn_region_write(&mut self.region, address, value)
    }

    /// Update multiple elements in the region. `address` is in bytes.
    #[inline]
    pub fn write_all<E: Elem>(&mut self, address: usize, values: &[E]) {
        M::dyn_region_write_all(&mut self.region, address, values)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::{
        backend_test, create_backend,
        state_backend::{
            layout::{Atom, Layout},
            Array, Backend, CellRead, CellWrite, Choreographer, DynCells, Elem, Location,
            ManagerAlloc, ManagerBase,
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

    backend_test!(
        #[should_panic]
        test_dynregion_oob_2,
        F,
        {
            const LEN: usize = 8;

            struct FlipperLayout;

            impl Layout for FlipperLayout {
                type Placed = Location<[u8; LEN]>;

                fn place_with(alloc: &mut Choreographer) -> Self::Placed {
                    alloc.alloc()
                }

                type Allocated<M: ManagerBase> = DynCells<LEN, M>;

                fn allocate<M: ManagerAlloc>(
                    backend: &mut M,
                    placed: Self::Placed,
                ) -> Self::Allocated<M> {
                    DynCells::bind(backend.allocate_dyn_region(placed))
                }
            }

            let mut backend = create_backend!(FlipperLayout, F);
            let mut state = backend.allocate(FlipperLayout::placed().into_location());

            // This should panic because we are trying to write an element at the address which
            // corresponds to the end of the buffer.
            state.write(LEN * FLIPPER_SIZE, Flipper { a: 1, b: 2 });
        }
    );

    backend_test!(test_dynregion_stored_format, F, {
        struct FlipperLayout;

        impl Layout for FlipperLayout {
            type Placed = Location<[u8; 1024]>;

            fn place_with(alloc: &mut Choreographer) -> Self::Placed {
                alloc.alloc()
            }

            type Allocated<B: ManagerBase> = DynCells<1024, B>;

            fn allocate<B: ManagerAlloc>(
                backend: &mut B,
                placed: Self::Placed,
            ) -> Self::Allocated<B> {
                DynCells::bind(backend.allocate_dyn_region(placed))
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
