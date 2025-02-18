// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Proof-generating backend
//!
//! Generic backend used for PVM proof generation, which wraps a manager and
//! records all state accesses performed during an evaluation step.
//! After evaluation, a [`MerkleTree`] over the PVM state can be obtained,
//! which can be partially blinded to produce a proof as a partial Merkle tree.
//! The structure of the Merkle tree is informed by the layout of the state,
//! which needs to implement [`ProofLayout`].
//!
//! [`MerkleTree`]: merkle::MerkleTree
//! [`ProofLayout`]: super::ProofLayout

use super::{
    EnrichedValue, EnrichedValueLinked, ManagerBase, ManagerRead, ManagerReadWrite,
    ManagerSerialise, ManagerWrite,
};
use merkle::AccessInfo;
use serde::{Serialize, ser::SerializeTuple};
use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, BTreeSet},
    mem, slice,
};

pub mod merkle;
pub mod proof;
pub mod tree;

/// Proof-generating backend
pub struct ProofGen<M: ManagerBase> {
    _pd: std::marker::PhantomData<M>,
}

impl<M: ManagerBase> ManagerBase for ProofGen<M> {
    type Region<E: 'static, const LEN: usize> = ProofRegion<E, LEN, M>;

    type DynRegion<const LEN: usize> = ProofDynRegion<LEN, M>;

    type EnrichedCell<V: EnrichedValue> = ProofEnrichedCell<V, M>;

    type ManagerRoot = Self;
}

/// Implementation of [`ManagerRead`] which wraps another manager and
/// additionally records read locations.
impl<M: ManagerRead> ManagerRead for ProofGen<M> {
    fn region_read<E: Copy, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> E {
        region.set_read();
        match region.writes.get(&index) {
            Some(elem) => *elem,
            None => M::region_read(&region.source, index),
        }
    }

    fn region_ref<E: 'static, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> &E {
        region.set_read();
        region.unrecorded_ref(index)
    }

    fn region_read_all<E: Copy, const LEN: usize>(region: &Self::Region<E, LEN>) -> Vec<E> {
        region.set_read();
        let mut elems = M::region_read_all(&region.source);
        for (index, elem) in region.writes.iter() {
            elems[*index] = *elem;
        }
        elems
    }

    fn dyn_region_read<E: super::Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
    ) -> E {
        region.reads.borrow_mut().insert::<E>(address);
        region.unrecorded_read(address)
    }

    fn dyn_region_read_all<E: super::Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
        values: &mut [E],
    ) {
        assert!(address + mem::size_of_val(values) <= LEN);

        for (offset, value) in values.iter_mut().enumerate() {
            *value = Self::dyn_region_read(region, address + offset * mem::size_of::<E>());
        }
    }

    fn enriched_cell_read_stored<V>(cell: &Self::EnrichedCell<V>) -> V::E
    where
        V: EnrichedValue,
        V::E: Copy,
    {
        cell.set_read();
        match &cell.written {
            None => M::enriched_cell_read_stored(&cell.source),
            Some(value) => *value,
        }
    }

    fn enriched_cell_read_derived<V>(cell: &Self::EnrichedCell<V>) -> V::D<Self::ManagerRoot>
    where
        V: EnrichedValueLinked<Self::ManagerRoot>,
        V::D<Self::ManagerRoot>: Copy,
    {
        V::derive(Self::enriched_cell_ref_stored(cell))
    }

    fn enriched_cell_ref_stored<V>(cell: &Self::EnrichedCell<V>) -> &V::E
    where
        V: EnrichedValue,
    {
        cell.set_read();
        cell.unrecorded_ref_stored()
    }
}

/// Implementation of [`ManagerWrite`] which wraps another manager and
/// records written locations but does not write to the wrapped region directly.
impl<M: ManagerBase> ManagerWrite for ProofGen<M> {
    fn region_write<E, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) {
        region.set_write();
        region.writes.insert(index, value);
    }

    fn region_write_all<E: Copy, const LEN: usize>(region: &mut Self::Region<E, LEN>, value: &[E]) {
        region.set_write();
        for (index, elem) in value.iter().enumerate() {
            region.writes.insert(index, *elem);
        }
    }

    fn dyn_region_write<E: super::Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        mut value: E,
    ) {
        assert!(address + mem::size_of_val(&value) <= LEN);

        value.to_stored_in_place();

        // Get a mutable slice of bytes over the value to be written and iterate over it
        // in order to record every byte to the write log. The wrapped region is not modified.
        let value_bytes = unsafe {
            // SAFETY: Obtaining a slice of `mem::size_of::<E>()` bytes from a reference
            // to one value of type `E` should be safe, assuming `value` is not the result of
            // multiple allocations.
            // Cannot use `mem::transmute` because `E` does not have a constant size.
            slice::from_raw_parts((&value as *const E) as *const u8, mem::size_of::<E>())
        };
        for (offset, byte) in value_bytes.iter().enumerate() {
            region.writes.insert(address + offset, *byte);
        }
    }

    fn dyn_region_write_all<E: super::Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        values: &[E],
    ) {
        assert!(address + mem::size_of_val(values) <= LEN);

        for (offset, value) in values.iter().enumerate() {
            Self::dyn_region_write(region, address + offset * mem::size_of::<E>(), *value)
        }
    }

    fn enriched_cell_write<V>(cell: &mut Self::EnrichedCell<V>, value: V::E)
    where
        V: EnrichedValueLinked<Self>,
    {
        cell.set_write();
        cell.written = Some(value);
    }
}

/// Implementation of [`ManagerReadWrite`] which wraps another manager and
/// additionally records read and written locations.
impl<M: ManagerRead> ManagerReadWrite for ProofGen<M> {
    fn region_replace<E: Copy, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) -> E {
        region.set_read_write();
        let elem = M::region_read(&region.source, index);
        region.writes.insert(index, value);
        elem
    }
}

/// Implementation of [`ManagerSerialise`] which wraps another manager and
/// serialises data as recorded by the `ProofGen` backend, reconstructed
/// via variants of [`ManagerRead`] functions which do not record access
/// information.
impl<M: ManagerSerialise> ManagerSerialise for ProofGen<M> {
    fn serialise_region<E: serde::Serialize, const LEN: usize, S: serde::Serializer>(
        region: &Self::Region<E, LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        if LEN == 1 {
            let elem = region.unrecorded_ref(0);
            return elem.serialize(serializer);
        }

        let mut serializer = serializer.serialize_tuple(LEN)?;
        for i in 0..LEN {
            let elem = region.unrecorded_ref(i);
            serializer.serialize_element(elem)?;
        }
        serializer.end()
    }

    fn serialise_dyn_region<const LEN: usize, S: serde::Serializer>(
        region: &Self::DynRegion<LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let mut values = vec![0u8; LEN];
        region.unrecorded_read_all(0, &mut values);
        serializer.serialize_bytes(values.as_slice())
    }

    fn serialise_enriched_cell<V, S>(
        cell: &Self::EnrichedCell<V>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        V: EnrichedValue,
        V::E: serde::Serialize,
        S: serde::Serializer,
    {
        let elem = cell.unrecorded_ref_stored();
        elem.serialize(serializer)
    }
}

/// Proof region which wraps a region managed by another manager.
///
/// A [`ManagerBase::Region`] is never split across multiple leaves when Merkleised.
/// An access to any part of the region is thus recorded as an access to the region as a whole.
/// The underlying region is never mutated, but all written values are recorded
/// in order to preserve the integrity of subsequent reads.
pub struct ProofRegion<E: 'static, const LEN: usize, M: ManagerBase> {
    source: M::Region<E, LEN>,
    writes: BTreeMap<usize, E>,
    access: Cell<AccessInfo>,
}

impl<M: ManagerBase, E: 'static, const LEN: usize> ProofRegion<E, LEN, M> {
    /// Bind a pre-existing region.
    pub fn bind(source: M::Region<E, LEN>) -> Self {
        Self {
            source,
            writes: BTreeMap::new(),
            access: Cell::new(AccessInfo::NoAccess),
        }
    }

    /// Get a copy of the access log.
    pub fn get_access_info(&self) -> AccessInfo {
        self.access.get()
    }

    /// Set the access log to `Read` or, if previously `Write`, to `ReadWrite`.
    pub fn set_read(&self) {
        self.access.set(self.access.get().and_read())
    }

    /// Set the access log to `Write` or, if previously `Read`, to `ReadWrite`.
    pub fn set_write(&self) {
        self.access.set(self.access.get().and_write())
    }

    /// Set the access log to `ReadWrite`.
    pub fn set_read_write(&self) {
        self.access.set(AccessInfo::ReadWrite)
    }

    /// Get a reference to the wrapper region.
    pub fn inner_region_ref(&self) -> &M::Region<E, LEN> {
        &self.source
    }
}

impl<M: ManagerRead, E: 'static, const LEN: usize> ProofRegion<E, LEN, M> {
    /// Version of [`ManagerRead::region_ref`] which does not record
    /// the access as a read.
    fn unrecorded_ref(&self, index: usize) -> &E {
        self.writes
            .get(&index)
            .unwrap_or_else(|| M::region_ref(&self.source, index))
    }
}
/// Proof dynamic region which wraps a dynamic region managed by another manager.
///
/// When Merkleising a [`ManagerBase::DynRegion`], its data can be split into multiple leaves.
/// Accesses are thus recorded for each address.
/// The underlying dynamic region is never mutated, but all written bytes are
/// recorded in order to preserve the integrity of subsequent reads.
pub struct ProofDynRegion<const LEN: usize, M: ManagerBase> {
    source: M::DynRegion<LEN>,
    reads: RefCell<DynAccess>,
    writes: BTreeMap<usize, u8>,
}

impl<M: ManagerBase, const LEN: usize> ProofDynRegion<LEN, M> {
    /// Bind a pre-existing dynamic region.
    pub fn bind(source: M::DynRegion<LEN>) -> Self {
        Self {
            source,
            reads: RefCell::default(),
            writes: BTreeMap::new(),
        }
    }

    /// Get the set of addresses of the region that were read from.
    /// This function is meant to be called once when Merkleising the region.
    pub fn get_read(&self) -> DynAccess {
        self.reads.take()
    }

    /// Get the set of addresses of the region that were written to.
    /// This function is meant to be called once when Merkleising the region.
    pub fn get_write(&self) -> DynAccess {
        let writes: BTreeSet<_> = self.writes.keys().copied().collect();
        DynAccess(writes)
    }
}

impl<M: ManagerRead, const LEN: usize> ProofDynRegion<LEN, M> {
    /// Read from the wrapped dynamic region.
    pub fn inner_dyn_region_read<E: super::Elem>(&self, address: usize) -> E {
        M::dyn_region_read(&self.source, address)
    }

    /// Version of [`ManagerRead::dyn_region_read`] which does not record
    /// the access as a read.
    fn unrecorded_read<E: super::Elem>(&self, address: usize) -> E {
        let elem_size = mem::size_of::<E>();

        // Read a value from the wrapped region and convert it to the stored representation.
        let mut value: E = M::dyn_region_read(&self.source, address);
        value.to_stored_in_place();

        // Get a mutable slice of bytes over the value and overwrite any byte that has been written
        // during the proof step.
        let value_bytes: &mut [u8] = unsafe {
            // SAFETY: Obtaining a mutable slice of `mem::size_of::<E>()` bytes from a mutable reference
            // to one value of type `E` should be safe, assuming `value` is not the result of
            // multiple allocations.
            // Cannot use `mem::transmute` because `E` does not have a constant size.
            slice::from_raw_parts_mut((&mut value as *mut E) as *mut u8, elem_size)
        };
        for (i, byte) in self.writes.range(address..address + elem_size) {
            value_bytes[*i - address] = *byte;
        }

        // Convert back from the stored representation and return.
        value.from_stored_in_place();
        value
    }

    /// Version of [`ManagerRead::dyn_region_read_all`] which does not record
    /// the access as a read.
    fn unrecorded_read_all<E: super::Elem>(&self, address: usize, values: &mut [E]) {
        assert!(address + mem::size_of_val(values) <= LEN);

        for (offset, value) in values.iter_mut().enumerate() {
            *value = self.unrecorded_read(address + offset * mem::size_of::<E>());
        }
    }
}

/// Proof enriched cell which wraps an enriched cell managed by another manager.
///
/// Similar to [`ManagerBase::Region`], a [`ManagerBase::EnrichedCell`] is never
/// split across multiple leaves when Merkleised.
/// The underlying cell is never mutated, but written values are recorded
/// in order to preserve the integrity of subsequent reads.
pub struct ProofEnrichedCell<V: EnrichedValue, M: ManagerBase> {
    source: M::EnrichedCell<V>,
    written: Option<V::E>,
    access: Cell<AccessInfo>,
}

impl<V: EnrichedValue, M: ManagerBase> ProofEnrichedCell<V, M> {
    /// Bind a pre-existing enriched cell.
    pub fn bind(source: M::EnrichedCell<V>) -> Self {
        Self {
            source,
            written: None,
            access: Cell::new(AccessInfo::NoAccess),
        }
    }

    /// Get a copy of the access log.
    pub fn get_access_info(&self) -> AccessInfo {
        self.access.get()
    }

    /// Set the access log to `Read` or, if previously `Write`, to `ReadWrite`.
    pub fn set_read(&self) {
        self.access.set(AccessInfo::and_read(self.access.get()))
    }

    /// Set the access log to `Write` or, if previously `Read`, to `ReadWrite`.
    pub fn set_write(&mut self) {
        self.access.set(AccessInfo::and_write(self.access.get()))
    }

    /// Set the access log to `ReadWrite`.
    pub fn set_read_write(&self) {
        self.access.set(AccessInfo::ReadWrite)
    }
}

impl<V: EnrichedValue, M: ManagerRead> ProofEnrichedCell<V, M> {
    /// Version of [`ManagerRead::enriched_cell_ref_stored`] which does not
    /// record the access as a read.
    fn unrecorded_ref_stored(&self) -> &V::E
    where
        V: EnrichedValue,
    {
        match &self.written {
            None => M::enriched_cell_ref_stored(&self.source),
            Some(value) => value,
        }
    }
}

/// A record of accessed addresses in a dynamic region
#[derive(Default)]
pub struct DynAccess(BTreeSet<usize>);

impl DynAccess {
    /// Insert all addresses touched while accessing an element of a given size.
    pub fn insert<E>(&mut self, address: usize) {
        self.0.extend(address..address + mem::size_of::<E>())
    }

    /// Check whether any address within a given range of addresses
    /// has been accessed.
    pub fn includes_range(&self, r: std::ops::Range<usize>) -> bool {
        self.0.range(r).next().is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::merkle::MERKLE_LEAF_SIZE;
    use super::*;
    use crate::state_backend::{
        Cells, CommitmentLayout, DynArray, DynCells, ProofLayout, Ref, layout::Array,
        owned_backend::Owned,
    };
    use proptest::{array, prop_assert_eq, proptest};
    use std::collections::VecDeque;
    use tests::merkle::MerkleTree;

    const CELLS_SIZE: usize = 32;

    #[test]
    fn test_proof_gen_region() {
        proptest!(|(value_before: u64, value_after: u64, i in 0..CELLS_SIZE)| {
            // A read followed by a write
            let cells = [value_before; CELLS_SIZE];
            let region: ProofRegion<u64, CELLS_SIZE, Ref<'_, Owned>> = ProofRegion::bind(&cells);
            let mut region: Cells<u64, CELLS_SIZE, ProofGen<Ref<'_, Owned>>> = Cells::bind(region);

            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::NoAccess);
            let value = region.read(i);
            prop_assert_eq!(value, value_before);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::Read);
            region.write(i, value_after);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::ReadWrite);

            // A write followed by a read
            let cells = [value_before; CELLS_SIZE];
            let region: ProofRegion<u64, CELLS_SIZE, Ref<'_, Owned>> = ProofRegion::bind(&cells);
            let mut region: Cells<u64, CELLS_SIZE, ProofGen<Ref<'_, Owned>>> = Cells::bind(region);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::NoAccess);
            region.write(i, value_after);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::Write);
            let value = region.read(i);
            prop_assert_eq!(value, value_after);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::ReadWrite);

            // Replace
            let cells = [value_before; CELLS_SIZE];
            let region: ProofRegion<u64, CELLS_SIZE, Ref<'_, Owned>> = ProofRegion::bind(&cells);
            let mut region: Cells<u64, CELLS_SIZE, ProofGen<Ref<'_, Owned>>> = Cells::bind(region);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::NoAccess);
            let value = region.replace(i, value_after);
            prop_assert_eq!(value, value_before);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::ReadWrite);

            let data_before = [value_before; CELLS_SIZE];
            let data_after = [value_after; CELLS_SIZE];

            // A read_all followed by a write_all
            let cells = data_before;
            let region: ProofRegion<u64, CELLS_SIZE, Ref<'_, Owned>> = ProofRegion::bind(&cells);
            let mut region: Cells<u64, CELLS_SIZE, ProofGen<Ref<'_, Owned>>> = Cells::bind(region);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::NoAccess);
            let values = region.read_all();
            prop_assert_eq!(values.as_slice(), data_before);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::Read);
            region.write_all(&data_after);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::ReadWrite);

            // A write_all followed by a read_all
            let cells = data_before;
            let region: ProofRegion<u64, CELLS_SIZE, Ref<'_, Owned>> = ProofRegion::bind(&cells);
            let mut region: Cells<u64, CELLS_SIZE, ProofGen<Ref<'_, Owned>>> = Cells::bind(region);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::NoAccess);
            region.write_all(&data_after);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::Write);
            let values = region.read_all();
            prop_assert_eq!(values.as_slice(), data_after);
            prop_assert_eq!(region.region_ref().get_access_info(), AccessInfo::ReadWrite);

            // Check correct Merkleisation
            let cells = [value_before; CELLS_SIZE];
            let cells_owned: Cells<u64, CELLS_SIZE, Ref<'_, Owned>> = Cells::bind(&cells);
            let initial_root_hash =
                <Array<u64, CELLS_SIZE> as CommitmentLayout>::state_hash(cells_owned).unwrap();

            let mut proof_region: ProofRegion<u64, CELLS_SIZE, Ref<'_, Owned>> =
                ProofRegion::bind(&cells);
            ProofGen::<Ref<'_, Owned>>::region_write(&mut proof_region, i, value_after);
            let proof_cells: Cells<u64, CELLS_SIZE, Ref<'_, ProofGen<Ref<'_, Owned>>>> =
                Cells::bind(&proof_region);

            let merkle_tree =
                <Array<u64, CELLS_SIZE> as ProofLayout>::to_merkle_tree(proof_cells).unwrap();
            merkle_tree.check_root_hash();
            match merkle_tree {
                MerkleTree::Leaf(hash, access_info, _) => {
                    prop_assert_eq!(hash, initial_root_hash);
                    prop_assert_eq!(access_info, AccessInfo::Write);
                }
                _ => panic!("Expected Merkle tree to contain a single written leaf"),
            }
        });
    }

    const LEAVES: usize = 8;
    const DYN_REGION_SIZE: usize = MERKLE_LEAF_SIZE.get() * LEAVES;
    const ELEM_SIZE: usize = mem::size_of::<u64>();

    #[test]
    fn test_proof_gen_dyn_region() {
        if ELEM_SIZE > MERKLE_LEAF_SIZE.get() {
            unreachable!(
                "This test assumes that a single element does not span more than 2 leaves"
            );
        }
        let address_range = 0..DYN_REGION_SIZE - ELEM_SIZE;

        // Check that writing to an address in the proof region makes subsequent reads return
        // the overwritten value.
        proptest!(|(byte_before: u8,
                    bytes_after: [u8; ELEM_SIZE],
                    write_address in &address_range)| {
            let cells = Box::new([byte_before; DYN_REGION_SIZE]);
            let dyn_region: ProofDynRegion<DYN_REGION_SIZE, Owned> = ProofDynRegion::bind(cells);
            let mut dyn_cells: DynCells<DYN_REGION_SIZE, ProofGen<Owned>> =
                DynCells::bind(dyn_region);

            // Perform static memory accesses
            let value_before = u64::from_le_bytes([byte_before; ELEM_SIZE]);
            let value_after = u64::from_le_bytes(bytes_after);

            let value: u64 = dyn_cells.read(write_address);
            assert_eq!(value, value_before);
            dyn_cells.write(write_address, value_after);
            let value: u64 = dyn_cells.read(write_address);
            assert_eq!(value, value_after);

            let cells = Box::new([byte_before; DYN_REGION_SIZE]);
            let dyn_region: ProofDynRegion<DYN_REGION_SIZE, Owned> = ProofDynRegion::bind(cells);
            let mut dyn_cells: DynCells<DYN_REGION_SIZE, ProofGen<Owned>> =
                DynCells::bind(dyn_region);

            // Perform dynamic memory accesses as `u16`
            let value_before = [u16::from_le_bytes([byte_before; 2]); ELEM_SIZE / 2];
            let value_after = [
                u16::from_le_bytes([bytes_after[0], bytes_after[1]]),
                u16::from_le_bytes([bytes_after[2], bytes_after[3]]),
                u16::from_le_bytes([bytes_after[4], bytes_after[5]]),
                u16::from_le_bytes([bytes_after[6], bytes_after[7]]),
            ];

            let mut value = [0u16; ELEM_SIZE / 2];
            dyn_cells.read_all(write_address, &mut value);
            assert_eq!(value, value_before);
            dyn_cells.write_all(write_address, &value_after);
            dyn_cells.read_all(write_address, &mut value);
            assert_eq!(value, value_after);

            let cells = Box::new([byte_before; DYN_REGION_SIZE]);
            let dyn_region: ProofDynRegion<DYN_REGION_SIZE, Owned> = ProofDynRegion::bind(cells);
            let mut dyn_cells: DynCells<DYN_REGION_SIZE, ProofGen<Owned>> =
                DynCells::bind(dyn_region);

            // Perform dynamic memory accesses as bytes
            let value_before = [byte_before; ELEM_SIZE];

            let mut value = [0u8; ELEM_SIZE];
            dyn_cells.read_all(write_address, &mut value);
            assert_eq!(value, value_before);
            dyn_cells.write_all(write_address, &bytes_after);
            dyn_cells.read_all(write_address, &mut value);
            assert_eq!(value, bytes_after);
        });

        // Check correct Merkleisation of a dynamic region which was read from and written to
        proptest!(|(byte_before: u8,
                    bytes_after: [u8; ELEM_SIZE],
                    reads in array::uniform2(&address_range),
                    writes in array::uniform2(&address_range))| {
            let dyn_array = Box::new([byte_before; DYN_REGION_SIZE]);
            let owned_dyn_cells: DynCells<DYN_REGION_SIZE, Ref<'_, Owned>> =
                DynCells::bind(&dyn_array);
            let initial_root_hash =
                <DynArray<DYN_REGION_SIZE> as CommitmentLayout>::state_hash(owned_dyn_cells)
                    .unwrap();

            let mut proof_dyn_region: ProofDynRegion<DYN_REGION_SIZE, Ref<'_, Owned>> =
                ProofDynRegion::bind(&dyn_array);

            // Perform memory accesses
            let value_before = [byte_before; ELEM_SIZE];
            reads.iter().for_each(|i| {
                let mut value = [0u8; ELEM_SIZE];
                ProofGen::<Ref<'_, Owned>>::dyn_region_read_all(&proof_dyn_region, *i, &mut value);
                assert_eq!(value, value_before)
            });
            writes.iter().for_each(|i| {
                ProofGen::<Ref<'_, Owned>>::dyn_region_write_all(
                    &mut proof_dyn_region,
                    *i,
                    &bytes_after,
                );
            });

            // Build the Merkle tree and check that it has the root hash of the
            // initial wrapped region.
            let proof_dyn_cells: DynCells<DYN_REGION_SIZE, Ref<'_, ProofGen<Ref<'_, Owned>>>> =
                DynCells::bind(&proof_dyn_region);
            let merkle_tree =
                <DynArray<DYN_REGION_SIZE> as ProofLayout>::to_merkle_tree(proof_dyn_cells).unwrap();
            merkle_tree.check_root_hash();
            prop_assert_eq!(merkle_tree.root_hash(), initial_root_hash);

            // Compute expected access info for each leaf, assuming that an access
            // cannot span more than 2 leaves.
            let expected_leaves = |accesses: &[usize]| {
                let mut leaves = BTreeSet::<usize>::new();
                for i in accesses {
                    leaves.insert(*i / MERKLE_LEAF_SIZE);
                    leaves.insert((i + ELEM_SIZE - 1) / MERKLE_LEAF_SIZE);
                }
                leaves
            };
            let read_leaves = expected_leaves(&reads);
            let written_leaves = expected_leaves(&writes);

            // Traverse the generated Merkle tree and check each leaf's access log
            let mut queue = VecDeque::with_capacity(LEAVES + 1);
            queue.push_back(merkle_tree);
            let mut leaf: usize = 0;
            while let Some(node) = queue.pop_front() {
                match node {
                    MerkleTree::Node(_, children) => queue.extend(children),
                    MerkleTree::Leaf(_, access_info, _) => {
                        assert_eq!(
                            access_info,
                            AccessInfo::from_bools(
                                read_leaves.contains(&leaf),
                                written_leaves.contains(&leaf)
                            )
                        );
                        leaf += 1;
                    }
                }
            }
        });
    }

    #[test]
    fn test_proof_gen_enriched_cell() {
        pub struct Enriching;

        impl EnrichedValue for Enriching {
            type E = u64;
            type D<M: ManagerBase + ?Sized> = T;
        }

        #[derive(Clone, Copy, Debug, PartialEq)]
        pub struct T(u64);

        impl<'a> From<&'a u64> for T {
            fn from(value: &'a u64) -> Self {
                T(value.wrapping_add(1))
            }
        }

        proptest!(|(value_before: u64, value_after: u64)| {
            // A read followed by a write
            let cell = (value_before, T::from(&value_before));
            let mut proof_cell: ProofEnrichedCell<Enriching, Ref<'_, Owned>> =
                ProofEnrichedCell::bind(&cell);
            prop_assert_eq!(proof_cell.get_access_info(), AccessInfo::NoAccess);
            let value = ProofGen::<Ref<'_, Owned>>::enriched_cell_read_stored(&proof_cell);
            prop_assert_eq!(value, value_before);
            let derived = ProofGen::<Ref<'_, Owned>>::enriched_cell_read_derived(&proof_cell);
            prop_assert_eq!(derived, T::from(&value_before));
            prop_assert_eq!(proof_cell.get_access_info(), AccessInfo::Read);
            ProofGen::<Ref<'_, Owned>>::enriched_cell_write(&mut proof_cell, value_after);
            prop_assert_eq!(proof_cell.get_access_info(), AccessInfo::ReadWrite);

            // A write followed by a read
            let cell = (value_before, T::from(&value_before));
            let mut proof_cell: ProofEnrichedCell<Enriching, Ref<'_, Owned>> =
                ProofEnrichedCell::bind(&cell);
            prop_assert_eq!(proof_cell.get_access_info(), AccessInfo::NoAccess);
            ProofGen::<Ref<'_, Owned>>::enriched_cell_write(&mut proof_cell, value_after);
            prop_assert_eq!(proof_cell.get_access_info(), AccessInfo::Write);
            let value = ProofGen::<Ref<'_, Owned>>::enriched_cell_read_stored(&proof_cell);
            prop_assert_eq!(value, value_after);
            let derived = ProofGen::<Ref<'_, Owned>>::enriched_cell_read_derived(&proof_cell);
            prop_assert_eq!(derived, T::from(&value_after));
            prop_assert_eq!(proof_cell.get_access_info(), AccessInfo::ReadWrite);
        });
    }
}
