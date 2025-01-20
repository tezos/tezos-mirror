// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::{
    proof_backend::{
        merkle::{AccessInfo, AccessInfoAggregatable},
        ProofGen,
    },
    Elem, EnrichedValue, EnrichedValueLinked, FnManager, ManagerBase, ManagerClone,
    ManagerDeserialise, ManagerRead, ManagerReadWrite, ManagerSerialise, ManagerWrite, Ref,
};
use std::borrow::Borrow;
use std::num::NonZeroUsize;

/// Link a stored value directly with a derived value -
/// that would either be expensive to compute each time, or cannot
/// itself be stored.
///
/// Only the value of `V::E` forms part of the 'state' for the purposes of commitments etc.
pub struct EnrichedCell<V: EnrichedValue, M: ManagerBase> {
    cell: M::EnrichedCell<V>,
}

impl<V: EnrichedValue, M: ManagerBase> EnrichedCell<V, M> {
    /// Bind this state to the enriched cell.
    pub fn bind(cell: M::EnrichedCell<V>) -> Self {
        Self { cell }
    }

    /// Obtain a reference to the underlying cell.
    pub fn cell_ref(&self) -> &M::EnrichedCell<V> {
        &self.cell
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> EnrichedCell<V, F::Output> {
        EnrichedCell {
            cell: F::map_enriched_cell(self.cell.borrow()),
        }
    }

    /// Write the value back to the enriched cell.
    ///
    /// Reading the new value will produce the new derived value also.
    pub fn write(&mut self, value: V::E)
    where
        M: ManagerWrite,
        V: EnrichedValueLinked<M>,
    {
        M::enriched_cell_write(&mut self.cell, value)
    }

    /// Read the stored value from the enriched cell.
    pub fn read_stored(&self) -> V::E
    where
        M: ManagerRead,
        V::E: Copy,
    {
        M::enriched_cell_read_stored(&self.cell)
    }

    /// Read the derived value from the enriched cell.
    pub fn read_derived(&self) -> V::D<M::ManagerRoot>
    where
        M: ManagerRead,
        V: EnrichedValueLinked<M::ManagerRoot>,
        V::D<M::ManagerRoot>: Copy,
    {
        M::enriched_cell_read_derived(&self.cell)
    }

    /// Obtain a reference to the value contained within the cell.
    pub fn read_ref_stored(&self) -> &V::E
    where
        M: ManagerRead,
    {
        M::enriched_cell_ref_stored(&self.cell)
    }
}

impl<V: EnrichedValue, M: ManagerClone> Clone for EnrichedCell<V, M>
where
    V::E: Copy,
    V::D<M>: Copy,
{
    fn clone(&self) -> Self {
        Self {
            cell: M::clone_enriched_cell(&self.cell),
        }
    }
}

impl<V, M: ManagerRead> PartialEq for EnrichedCell<V, M>
where
    V: EnrichedValueLinked<M::ManagerRoot>,
    V::E: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        M::enriched_cell_ref_stored(&self.cell) == M::enriched_cell_ref_stored(&other.cell)
    }
}

/// Single element of type `E`
#[repr(transparent)]
pub struct Cell<E: 'static, M: ManagerBase + ?Sized> {
    region: Cells<E, 1, M>,
}

impl<E: 'static, M: ManagerBase> Cell<E, M> {
    /// Bind this state to the single element region.
    pub const fn bind(region: M::Region<E, 1>) -> Self {
        Self {
            region: Cells::bind(region),
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> Cell<E, F::Output> {
        Cell {
            region: self.region.struct_ref::<F>(),
        }
    }

    /// Obtain the underlying region.
    pub fn into_region(self) -> M::Region<E, 1> {
        self.region.into_region()
    }
}

impl<E: Copy, M: ManagerBase> Cell<E, M> {
    /// Read the value managed by the cell.
    #[inline(always)]
    pub fn read(&self) -> E
    where
        M: ManagerRead,
    {
        self.region.read(0)
    }

    /// Write the value managed by the cell.
    #[inline(always)]
    pub fn write(&mut self, value: E)
    where
        M: ManagerWrite,
    {
        self.region.write(0, value)
    }

    /// Replace the value managed by the cell, returning the old value.
    #[inline(always)]
    pub fn replace(&mut self, value: E) -> E
    where
        M: ManagerReadWrite,
    {
        self.region.replace(0, value)
    }
}

impl<E: 'static, M: ManagerBase> From<Cells<E, 1, M>> for Cell<E, M> {
    fn from(region: Cells<E, 1, M>) -> Self {
        Self { region }
    }
}

impl<E: serde::Serialize, M: ManagerSerialise> serde::Serialize for Cell<E, M> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.region.serialize(serializer)
    }
}

impl<'de, E: serde::Deserialize<'de>, M: ManagerDeserialise> serde::Deserialize<'de>
    for Cell<E, M>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let region = Cells::deserialize(deserializer)?;
        Ok(Self { region })
    }
}

impl<A: PartialEq<B> + Copy, B: Copy, M: ManagerRead, N: ManagerRead> PartialEq<Cell<B, N>>
    for Cell<A, M>
{
    fn eq(&self, other: &Cell<B, N>) -> bool {
        self.read() == other.read()
    }
}

impl<E: serde::Serialize, M: ManagerSerialise> AccessInfoAggregatable
    for Cell<E, Ref<'_, ProofGen<M>>>
{
    fn aggregate_access_info(&self) -> AccessInfo {
        self.region.region.get_access_info()
    }
}

impl<E: Eq + Copy, M: ManagerRead> Eq for Cell<E, M> {}

impl<E: Copy, M: ManagerClone> Clone for Cell<E, M> {
    fn clone(&self) -> Self {
        Self {
            region: self.region.clone(),
        }
    }
}

impl<E, M: ManagerRead> AsRef<E> for Cell<E, M> {
    fn as_ref(&self) -> &E {
        M::region_ref(&self.region.region, 0)
    }
}

/// A cell that support reading only.
pub trait CellBase {
    /// Element type managed by the cell.
    type Value;
}

impl<E: 'static, M: ManagerBase> CellBase for Cell<E, M> {
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

impl<E: Copy, M: ManagerRead> CellRead for Cell<E, M> {
    #[inline(always)]
    fn read(&self) -> E {
        Cell::read(self)
    }
}

impl<E: CellRead> CellRead for &E {
    #[inline(always)]
    fn read(&self) -> Self::Value {
        E::read(self)
    }
}

impl<E: CellRead> CellRead for &mut E {
    #[inline(always)]
    fn read(&self) -> Self::Value {
        E::read(self)
    }
}

/// A cell that support writing.
pub trait CellWrite: CellBase {
    /// Write the value managed by the cell.
    fn write(&mut self, value: Self::Value);
}

impl<E: Copy, M: ManagerWrite> CellWrite for Cell<E, M> {
    #[inline(always)]
    fn write(&mut self, value: E) {
        Cell::write(self, value)
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

impl<E: Copy, M: ManagerReadWrite> CellReadWrite for Cell<E, M> {
    #[inline(always)]
    fn replace(&mut self, value: E) -> E {
        Cell::replace(self, value)
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
pub struct Cells<E: 'static, const LEN: usize, M: ManagerBase + ?Sized> {
    region: M::Region<E, LEN>,
}

impl<E: 'static, const LEN: usize, M: ManagerBase> Cells<E, LEN, M> {
    /// Bind this state to the given region.
    pub const fn bind(region: M::Region<E, LEN>) -> Self {
        Self { region }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> Cells<E, LEN, F::Output> {
        Cells {
            region: F::map_region(&self.region),
        }
    }

    /// Obtain a reference to the underlying region.
    pub fn region_ref(&self) -> &M::Region<E, LEN> {
        &self.region
    }

    /// Obtain the underlying region.
    pub fn into_region(self) -> M::Region<E, LEN> {
        self.region
    }
}

impl<E: Copy, const LEN: usize, M: ManagerBase> Cells<E, LEN, M> {
    /// Read an element in the region.
    #[inline]
    pub fn read(&self, index: usize) -> E
    where
        M: ManagerRead,
    {
        M::region_read(&self.region, index)
    }

    /// Read all elements in the region.
    #[inline]
    pub fn read_all(&self) -> Vec<E>
    where
        M: ManagerRead,
    {
        M::region_read_all(&self.region)
    }

    /// Update an element in the region.
    #[inline]
    pub fn write(&mut self, index: usize, value: E)
    where
        M: ManagerWrite,
    {
        M::region_write(&mut self.region, index, value)
    }

    /// Update all elements in the region.
    #[inline]
    pub fn write_all(&mut self, value: &[E])
    where
        M: ManagerWrite,
    {
        M::region_write_all(&mut self.region, value)
    }

    /// Update the element in the region and return the previous value.
    #[inline]
    pub fn replace(&mut self, index: usize, value: E) -> E
    where
        M: ManagerReadWrite,
    {
        M::region_replace(&mut self.region, index, value)
    }
}

impl<E: serde::Serialize, const LEN: usize, M: ManagerSerialise> serde::Serialize
    for Cells<E, LEN, M>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        M::serialise_region(&self.region, serializer)
    }
}

impl<V: EnrichedValue, M: ManagerSerialise> serde::Serialize for EnrichedCell<V, M>
where
    V::E: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        M::serialise_enriched_cell(&self.cell, serializer)
    }
}

impl<V: EnrichedValue, M: ManagerSerialise> AccessInfoAggregatable
    for EnrichedCell<V, Ref<'_, ProofGen<M>>>
where
    V::E: serde::Serialize,
{
    fn aggregate_access_info(&self) -> AccessInfo {
        self.cell.get_access_info()
    }
}

impl<'de, E: serde::Deserialize<'de>, const LEN: usize, M: ManagerDeserialise>
    serde::Deserialize<'de> for Cells<E, LEN, M>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let region = M::deserialise_region(deserializer)?;
        Ok(Self { region })
    }
}

impl<'de, V, M: ManagerDeserialise> serde::Deserialize<'de> for EnrichedCell<V, M>
where
    V: EnrichedValueLinked<M>,
    V::E: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let cell = M::deserialise_enriched_cell(deserializer)?;
        Ok(Self { cell })
    }
}

impl<A: PartialEq<B> + Copy, B: Copy, const LEN: usize, M: ManagerRead, N: ManagerRead>
    PartialEq<Cells<B, LEN, N>> for Cells<A, LEN, M>
{
    fn eq(&self, other: &Cells<B, LEN, N>) -> bool {
        (0..LEN).all(|i| self.read(i) == other.read(i))
    }
}

impl<E: serde::Serialize, const LEN: usize, M: ManagerSerialise> AccessInfoAggregatable
    for Cells<E, LEN, Ref<'_, ProofGen<M>>>
{
    fn aggregate_access_info(&self) -> AccessInfo {
        self.region.get_access_info()
    }
}

impl<E: Copy, const LEN: usize, M: ManagerClone> Clone for Cells<E, LEN, M> {
    fn clone(&self) -> Self {
        Self {
            region: M::clone_region(&self.region),
        }
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

    /// Obtain a reference to the underlying dynamic region.
    pub fn region_ref(&self) -> &M::DynRegion<LEN> {
        &self.region
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> DynCells<LEN, F::Output> {
        DynCells {
            region: F::map_dyn_region(&self.region),
        }
    }

    /// Read an element in the region. `address` is in bytes.
    #[inline]
    pub fn read<E: Elem>(&self, address: usize) -> E
    where
        M: ManagerRead,
    {
        M::dyn_region_read(&self.region, address)
    }

    /// Read elements from the region. `address` is in bytes.
    #[inline]
    pub fn read_all<E: Elem>(&self, address: usize, values: &mut [E])
    where
        M: ManagerRead,
    {
        M::dyn_region_read_all(&self.region, address, values)
    }

    /// Update an element in the region. `address` is in bytes.
    #[inline]
    pub fn write<E: Elem>(&mut self, address: usize, value: E)
    where
        M: ManagerWrite,
    {
        M::dyn_region_write(&mut self.region, address, value)
    }

    /// Update multiple elements in the region. `address` is in bytes.
    #[inline]
    pub fn write_all<E: Elem>(&mut self, address: usize, values: &[E])
    where
        M: ManagerWrite,
    {
        M::dyn_region_write_all(&mut self.region, address, values)
    }
}

impl<const LEN: usize, M: ManagerSerialise> serde::Serialize for DynCells<LEN, M> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        M::serialise_dyn_region(&self.region, serializer)
    }
}

impl<'de, const LEN: usize, M: ManagerDeserialise> serde::Deserialize<'de> for DynCells<LEN, M> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let region = M::deserialise_dyn_region(deserializer)?;
        Ok(DynCells { region })
    }
}

impl<const LEN: usize, M: ManagerRead, N: ManagerRead> PartialEq<DynCells<LEN, N>>
    for DynCells<LEN, M>
{
    fn eq(&self, other: &DynCells<LEN, N>) -> bool {
        for i in 0..LEN {
            if self.read::<u8>(i) != other.read::<u8>(i) {
                return false;
            }
        }
        true
    }
}

impl<const LEN: usize, M: ManagerRead> Eq for DynCells<LEN, M> {}

// TODO RV-322: Choose optimal Merkleisation parameters for main memory.
/// Size of the Merkle leaf used for Merkleising [`DynCells`].
pub const MERKLE_LEAF_SIZE: NonZeroUsize =
    // SAFETY: This constant is non-zero.
    unsafe { NonZeroUsize::new_unchecked(4096) };

// TODO RV-322: Choose optimal Merkleisation parameters for main memory.
/// Arity of the Merkle tree used for Merkleising [`DynCells`].
pub const MERKLE_ARITY: usize = 3;

// TODO RV-398: Move to `merkle` once `RootHashable` is removed
/// Helper function which allows iterating over chunks of a dynamic region
/// and writing them to a writer. The last chunk may be smaller than the
/// Merkle leaf size. The implementations of `CommitmentLayout` and `ProofLayout`
/// for dynamic regions both use it, ensuring consistency between the two.
pub(crate) fn chunks_to_writer<
    const LEN: usize,
    T: std::io::Write,
    F: Fn(usize) -> [u8; MERKLE_LEAF_SIZE.get()],
>(
    writer: &mut T,
    read: F,
) -> Result<(), std::io::Error> {
    let merkle_leaf_size = MERKLE_LEAF_SIZE.get();
    assert!(LEN >= merkle_leaf_size);

    let mut address = 0;

    while address + merkle_leaf_size <= LEN {
        writer.write_all(read(address).as_slice())?;
        address += merkle_leaf_size;
    }

    // When the last chunk is smaller than `MERKLE_LEAF_SIZE`,
    // read the last `MERKLE_LEAF_SIZE` bytes and pass a subslice containing
    // only the bytes not previously read to the writer.
    if address != LEN {
        address += merkle_leaf_size;
        let buffer = read(LEN.saturating_sub(merkle_leaf_size));
        writer.write_all(&buffer[address.saturating_sub(LEN)..])?;
    };

    Ok(())
}

impl<const LEN: usize, M: ManagerClone> Clone for DynCells<LEN, M> {
    fn clone(&self) -> Self {
        Self {
            region: M::clone_dyn_region(&self.region),
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::{chunks_to_writer, MERKLE_LEAF_SIZE};
    use crate::{
        backend_test,
        default::ConstDefault,
        state_backend::{
            layout::{Atom, Layout},
            Array, DynCells, Elem, FnManagerIdent, ManagerAlloc, ManagerBase,
        },
    };
    use proptest::prelude::*;
    use serde::ser::SerializeTuple;
    use std::io::Cursor;

    /// Dummy type that helps us implement custom normalisation via [Elem]
    #[repr(packed)]
    #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Default)]
    struct Flipper {
        a: u8,
        b: u8,
    }

    impl ConstDefault for Flipper {
        const DEFAULT: Self = Self { a: 0, b: 0 };
    }

    impl serde::Serialize for Flipper {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            let mut serializer = serializer.serialize_tuple(2)?;
            serializer.serialize_element(&self.b)?;
            serializer.serialize_element(&self.a)?;
            serializer.end()
        }
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

        let (mut array1, mut array2) = F::allocate::<OurLayout>();

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
        let (mut cell1, mut cell2) = F::allocate::<OurLayout>();

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
                type Allocated<M: ManagerBase> = DynCells<LEN, M>;

                fn allocate<M: ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
                    DynCells::bind(backend.allocate_dyn_region())
                }
            }

            let mut state = F::allocate::<FlipperLayout>();

            // This should panic because we are trying to write an element at the address which
            // corresponds to the end of the buffer.
            state.write(LEN * FLIPPER_SIZE, Flipper { a: 1, b: 2 });
        }
    );

    backend_test!(test_dynregion_stored_format, F, {
        struct FlipperLayout;

        impl Layout for FlipperLayout {
            type Allocated<B: ManagerBase> = DynCells<1024, B>;

            fn allocate<B: ManagerAlloc>(backend: &mut B) -> Self::Allocated<B> {
                DynCells::bind(backend.allocate_dyn_region())
            }
        }

        // Writing to one item of the region must convert to stored format.
        let mut region = F::allocate::<FlipperLayout>();

        region.write(0, Flipper { a: 13, b: 37 });
        assert_eq!(region.read::<Flipper>(0), Flipper { a: 13, b: 37 });

        let buffer = region.read::<[u8; 2]>(0);
        assert_eq!(buffer, [37, 13]);

        // Writing to the entire region must convert properly to stored format.
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

        let buffer = region.read::<[u8; 8]>(0);
        assert_eq!(buffer, [22, 11, 24, 13, 26, 15, 28, 17]);
    });

    backend_test!(test_region_stored_format, F, {
        type FlipperLayout = Array<Flipper, 4>;

        // Writing to one item of the region must convert to stored format.
        let mut region = F::allocate::<FlipperLayout>();

        region.write(0, Flipper { a: 13, b: 37 });
        assert_eq!(region.read(0), Flipper { a: 13, b: 37 });

        let buffer = bincode::serialize(&region.struct_ref::<FnManagerIdent>()).unwrap();
        assert_eq!(buffer[..2], [37, 13]);

        // Replacing a value in the region must convert to and from stored format.
        let old = region.replace(0, Flipper { a: 26, b: 74 });
        assert_eq!(old, Flipper { a: 13, b: 37 });

        let buffer = bincode::serialize(&region.struct_ref::<FnManagerIdent>()).unwrap();
        assert_eq!(buffer[..2], [74, 26]);

        // Writing to the entire region must convert properly to stored format.
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

        let buffer = bincode::serialize(&region.struct_ref::<FnManagerIdent>()).unwrap();
        assert_eq!(buffer[..8], [22, 11, 24, 13, 26, 15, 28, 17]);
    });

    const LENS: [usize; 4] = [0, 1, 535, 4095];
    const _: () = {
        if MERKLE_LEAF_SIZE.get() != 4096 {
            panic!(
                "Test values in `LENS` assume a specific MERKLE_LEAF_SIZE, change them accordingly"
            );
        }
    };

    // Test `chunks_to_writer` with a variety of lengths
    macro_rules! generate_test_chunks_to_writer {
        ( $( $name:ident, $i:expr ),* ) => {
            $(
                proptest! {
                    #[test]
                    fn $name(
                        data in proptest::collection::vec(any::<u8>(), 4 * MERKLE_LEAF_SIZE.get() + LENS[$i])
                    ) {
                        const LEN: usize = 4 * MERKLE_LEAF_SIZE.get() + LENS[$i];

                        let read = |pos: usize| {
                            assert!(pos + MERKLE_LEAF_SIZE.get() <= LEN);
                            data[pos..pos + MERKLE_LEAF_SIZE.get()].try_into().unwrap()
                        };

                        let mut writer = Cursor::new(Vec::new());
                        chunks_to_writer::<LEN, _, _>(&mut writer, read).unwrap();
                        assert_eq!(writer.into_inner(), data);
                    }
                }
            )*
        }
    }

    generate_test_chunks_to_writer!(test_chunks_to_writer_0, 0);
    generate_test_chunks_to_writer!(test_chunks_to_writer_1, 1);
    generate_test_chunks_to_writer!(test_chunks_to_writer_2, 2);
    generate_test_chunks_to_writer!(test_chunks_to_writer_3, 3);
}
