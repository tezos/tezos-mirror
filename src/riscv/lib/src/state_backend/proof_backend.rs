// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Proof-generating backend
//!
//! Generic backend used for PVM proof generation, which wraps a manager and
//! records all state accesses performed during an evaluation step.
//! After evaluation, a [`MerkleTree`] over the PVM state can be obtained,
//! which can be partially blinded to produce a proof as a partial Merkle tree.
//! The structure of the Merkle tree is informed by the layout of the state, by
//! implementing [`Merkleisable`] for each of its components.
//!
//! [`MerkleTree`]: merkle::MerkleTree
//! [`Merkleisable`]: merkle::Merkleisable

use super::{
    EnrichedValue, EnrichedValueLinked, ManagerBase, ManagerRead, ManagerReadWrite,
    ManagerSerialise, ManagerWrite,
};
use merkle::AccessInfo;
use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, BTreeSet},
    mem, slice,
};

pub mod merkle;
pub mod proof;
mod tree;

/// Proof-generating backend
pub struct ProofGen<'a, M: ManagerBase> {
    _pd: std::marker::PhantomData<&'a M>,
}

impl<'a, M: ManagerBase> ManagerBase for ProofGen<'a, M> {
    type Region<E: 'static, const LEN: usize> = ProofRegion<E, LEN, M>;

    type DynRegion<const LEN: usize> = ProofDynRegion<LEN, M>;

    type EnrichedCell<V: EnrichedValue> = ProofEnrichedCell<V, M>;

    type ManagerRoot = Self;
}

/// Implementation of [`ManagerRead`] which wraps another manager and
/// additionally records read locations.
impl<'a, M: ManagerRead> ManagerRead for ProofGen<'a, M> {
    fn region_read<E: Copy, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> E {
        region.set_read();
        match region.writes.get(&index) {
            Some(elem) => *elem,
            None => M::region_read(&region.source, index),
        }
    }

    fn region_ref<E: 'static, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> &E {
        region.set_read();
        region
            .writes
            .get(&index)
            .unwrap_or_else(|| M::region_ref(&region.source, index))
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
        let elem_size = mem::size_of::<E>();
        region.reads.borrow_mut().insert::<E>(address);

        // Read a value from the wrapped region and convert it to the stored representation.
        let mut value: E = M::dyn_region_read(&region.source, address);
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
        for (i, byte) in region.writes.range(address..address + elem_size) {
            value_bytes[*i - address] = *byte;
        }

        // Convert back from the stored representation and return.
        value.from_stored_in_place();
        value
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

    fn enriched_cell_read<V>(_cell: &Self::EnrichedCell<V>) -> (V::E, V::D<Self::ManagerRoot>)
    where
        V: EnrichedValueLinked<Self::ManagerRoot>,
        V::E: Copy,
        V::D<Self>: Copy,
    {
        // TODO: RV-325 Support for `EnrichedCell` in the proof-generating backend
        todo!()
    }

    fn enriched_cell_ref<V>(_cell: &Self::EnrichedCell<V>) -> (&V::E, &V::D<Self::ManagerRoot>)
    where
        V: EnrichedValueLinked<Self::ManagerRoot>,
    {
        // TODO: RV-325 Support for `EnrichedCell` in the proof-generating backend
        todo!()
    }

    fn enriched_cell_read_stored<V>(_cell: &Self::EnrichedCell<V>) -> V::E
    where
        V: EnrichedValue,
        V::E: Copy,
    {
        // TODO: RV-325 Support for `EnrichedCell` in the proof-generating backend
        todo!()
    }
}

/// Implementation of [`ManagerWrite`] which wraps another manager and
/// records written locations but does not write to the wrapped region directly.
impl<'a, M: ManagerWrite> ManagerWrite for ProofGen<'a, M> {
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

    fn enriched_cell_write<V>(_cell: &mut Self::EnrichedCell<V>, _value: V::E)
    where
        V: EnrichedValueLinked<Self>,
    {
        // TODO: RV-325 Support for `EnrichedCell` in the proof-generating backend
        todo!()
    }
}

/// Implementation of [`ManagerReadWrite`] which wraps another manager and
/// additionally records read and written locations.
impl<'a, M: ManagerReadWrite> ManagerReadWrite for ProofGen<'a, M> {
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

impl<'a, M: ManagerSerialise> ManagerSerialise for ProofGen<'a, M> {
    fn serialise_region<E: serde::Serialize, const LEN: usize, S: serde::Serializer>(
        region: &Self::Region<E, LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        M::serialise_region(&region.source, serializer)
    }

    fn serialise_dyn_region<const LEN: usize, S: serde::Serializer>(
        region: &Self::DynRegion<LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        M::serialise_dyn_region(&region.source, serializer)
    }

    fn serialise_enriched_cell<V, S>(
        _cell: &Self::EnrichedCell<V>,
        _serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        V: EnrichedValue,
        V::E: serde::Serialize,
        S: serde::Serializer,
    {
        // TODO: RV-325 Support for `EnrichedCell` in the proof-generating backend
        todo!()
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
    /// Get a copy of the access log.
    pub fn get_access_info(&self) -> AccessInfo {
        self.access.get()
    }

    /// Set the access log to `Read` or, if previously `Write`, to `ReadWrite`.
    pub fn set_read(&self) {
        match self.access.get() {
            AccessInfo::NoAccess => self.access.set(AccessInfo::Read),
            AccessInfo::Write => self.access.set(AccessInfo::ReadWrite),
            _ => (),
        }
    }

    /// Set the access log to `Write` or, if previously `Read`, to `ReadWrite`.
    pub fn set_write(&self) {
        match self.access.get() {
            AccessInfo::NoAccess => self.access.set(AccessInfo::Write),
            AccessInfo::Read => self.access.set(AccessInfo::ReadWrite),
            _ => (),
        }
    }

    /// Set the access log to `ReadWrite`.
    pub fn set_read_write(&self) {
        self.access.set(AccessInfo::ReadWrite)
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
}

// TODO: RV-325 Support for `EnrichedCell` in the proof-generating backend
#[allow(dead_code)]
pub struct ProofEnrichedCell<V: EnrichedValue, M: ManagerBase> {
    source: M::EnrichedCell<V>,
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
    use super::*;
    use crate::state_backend::{
        owned_backend::Owned,
        region::{DynCells, MERKLE_LEAF_SIZE},
    };
    use proptest::{prop_assert_eq, proptest};

    impl<M: ManagerBase, E: 'static, const LEN: usize> ProofRegion<E, LEN, M> {
        pub fn from_source_region(source: M::Region<E, LEN>) -> Self {
            Self {
                source,
                writes: BTreeMap::new(),
                access: Cell::new(AccessInfo::NoAccess),
            }
        }
    }

    const CELLS_SIZE: usize = 255;

    #[test]
    fn test_proof_gen_region() {
        proptest!(|(value_before: u64, value_after: u64, i in 0..CELLS_SIZE)| {
            // A read followed by a write
            let cells = [value_before; CELLS_SIZE];
            let mut region: ProofRegion<u64, CELLS_SIZE, Owned> =
                ProofRegion::from_source_region(cells);
            prop_assert_eq!(region.get_access_info(), AccessInfo::NoAccess);
            let value = ProofGen::<'_, Owned>::region_read(&region, i);
            prop_assert_eq!(value, value_before);
            prop_assert_eq!(region.get_access_info(), AccessInfo::Read);
            ProofGen::<'_, Owned>::region_write(&mut region, i, value_after);
            prop_assert_eq!(region.get_access_info(), AccessInfo::ReadWrite);

            // A write followed by a read
            let cells = [value_before; CELLS_SIZE];
            let mut region: ProofRegion<u64, CELLS_SIZE, Owned> =
                ProofRegion::from_source_region(cells);
            prop_assert_eq!(region.get_access_info(), AccessInfo::NoAccess);
            ProofGen::<'_, Owned>::region_write(&mut region, i, value_after);
            prop_assert_eq!(region.get_access_info(), AccessInfo::Write);
            let value = ProofGen::<'_, Owned>::region_read(&region, i);
            prop_assert_eq!(value, value_after);
            prop_assert_eq!(region.get_access_info(), AccessInfo::ReadWrite);

            // Replace
            let cells = [value_before; CELLS_SIZE];
            let mut region: ProofRegion<u64, CELLS_SIZE, Owned> =
                ProofRegion::from_source_region(cells);
            prop_assert_eq!(region.get_access_info(), AccessInfo::NoAccess);
            let value = ProofGen::<'_, Owned>::region_replace(&mut region, i, value_after);
            prop_assert_eq!(value, value_before);
            prop_assert_eq!(region.get_access_info(), AccessInfo::ReadWrite);

            let data_before = [value_before; CELLS_SIZE];
            let data_after = [value_after; CELLS_SIZE];

            // A read_all followed by a write_all
            let cells = data_before;
            let mut region: ProofRegion<u64, CELLS_SIZE, Owned> =
                ProofRegion::from_source_region(cells);
            prop_assert_eq!(region.get_access_info(), AccessInfo::NoAccess);
            let values = ProofGen::<'_, Owned>::region_read_all(&region);
            prop_assert_eq!(values.as_slice(), data_before);
            prop_assert_eq!(region.get_access_info(), AccessInfo::Read);
            ProofGen::<'_, Owned>::region_write_all(&mut region, &data_after);
            prop_assert_eq!(region.get_access_info(), AccessInfo::ReadWrite);

            // A write_all followed by a read_all
            let cells = data_before;
            let mut region: ProofRegion<u64, CELLS_SIZE, Owned> =
                ProofRegion::from_source_region(cells);
            prop_assert_eq!(region.get_access_info(), AccessInfo::NoAccess);
            ProofGen::<'_, Owned>::region_write_all(&mut region, &data_after);
            prop_assert_eq!(region.get_access_info(), AccessInfo::Write);
            let values = ProofGen::<'_, Owned>::region_read_all(&region);
            prop_assert_eq!(values.as_slice(), data_after);
            prop_assert_eq!(region.get_access_info(), AccessInfo::ReadWrite);
        });
    }

    impl<M: ManagerBase, const LEN: usize> ProofDynRegion<LEN, M> {
        pub fn from_source_region(source: M::DynRegion<LEN>) -> Self {
            Self {
                source,
                reads: RefCell::default(),
                writes: BTreeMap::new(),
            }
        }
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
            let dyn_region: ProofDynRegion<DYN_REGION_SIZE, Owned> =
                ProofDynRegion::from_source_region(cells);
            let mut dyn_cells: DynCells<DYN_REGION_SIZE, ProofGen<'_, Owned>> =
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
            let dyn_region: ProofDynRegion<DYN_REGION_SIZE, Owned> =
                ProofDynRegion::from_source_region(cells);
            let mut dyn_cells: DynCells<DYN_REGION_SIZE, ProofGen<'_, Owned>> =
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
            let dyn_region: ProofDynRegion<DYN_REGION_SIZE, Owned> =
                ProofDynRegion::from_source_region(cells);
            let mut dyn_cells: DynCells<DYN_REGION_SIZE, ProofGen<'_, Owned>> =
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
    }
}
