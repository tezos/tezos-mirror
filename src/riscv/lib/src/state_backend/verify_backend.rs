// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::array;
use std::collections::BTreeMap;
use std::mem;
use std::mem::MaybeUninit;
use std::ops::Index;
use std::panic::resume_unwind;
use std::slice;

use range_collections::RangeSet2;
use serde::ser::SerializeTuple;

use super::Cell;
use super::EnrichedValue;
use super::ManagerBase;
use super::ManagerClone;
use super::ManagerRead;
use super::ManagerReadWrite;
use super::ManagerWrite;
use super::PartialHashError;
use super::Ref;
use crate::state_backend::hash::Hash;
use crate::state_backend::owned_backend::Owned;
use crate::state_backend::proof_backend::merkle::MERKLE_LEAF_SIZE;

/// Panic payload that is raised when a value isn't present in a part of the Verifier backend.
#[derive(Copy, Clone, Debug, Eq, PartialEq, derive_more::Display, thiserror::Error)]
pub struct NotFound;

/// Raise a [`NotFound`] panic.
fn not_found() -> ! {
    // We use [`resume_unwind`] over [`panic_any`] to avoid calling the panic hook.
    // XXX: This fails without a message when there is no matching [`handle_stepper_panics`] wrapper.
    resume_unwind(Box::new(NotFound))
}

/// Catch errors that the verifier backend may raise during the invocation of `f` and return them
/// as [`Err`].
pub(crate) fn handle_stepper_panics<R, F: FnOnce() -> R + std::panic::UnwindSafe>(
    f: F,
) -> Result<R, ProofVerificationFailure> {
    match std::panic::catch_unwind(f) {
        Ok(res) => Ok(res),
        Err(err) => match err.downcast::<NotFound>() {
            Ok(not_found) => Err(ProofVerificationFailure::AbsentDataAccess(*not_found)),
            Err(other) => Err(ProofVerificationFailure::StepperPanic(other)),
        },
    }
}

/// Error during proof verification
#[derive(Debug, thiserror::Error)]
pub enum ProofVerificationFailure {
    #[error("Unexpected proof shape")]
    UnexpectedProofShape,

    #[error("Stepper error")]
    StepperError,

    #[error("Stepper panic")]
    StepperPanic(Box<dyn std::any::Any + Send>),

    #[error("Attempted to access absent data")]
    AbsentDataAccess(#[from] NotFound),

    #[error("Error computing final state hash: {0}")]
    PartialHashError(#[from] PartialHashError),

    #[error("Final state hash mismatch (expected {expected}, computed {computed})")]
    FinalHashMismatch { expected: Hash, computed: Hash },
}

/// Proof verification backend
pub struct Verifier;

impl ManagerBase for Verifier {
    type Region<E: 'static, const LEN: usize> = Region<E, LEN>;

    type DynRegion<const LEN: usize> = DynRegion<{ MERKLE_LEAF_SIZE.get() }, LEN>;

    type EnrichedCell<V: EnrichedValue> = EnrichedCell<V>;

    type ManagerRoot = Self;

    fn enrich_cell<V: super::EnrichedValueLinked>(
        underlying: Self::Region<V::E, 1>,
    ) -> Self::EnrichedCell<V> {
        EnrichedCell { underlying }
    }

    fn as_devalued_cell<V: EnrichedValue>(cell: &Self::EnrichedCell<V>) -> &Self::Region<V::E, 1> {
        &cell.underlying
    }
}

impl ManagerRead for Verifier {
    fn region_read<E: Copy, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> E {
        region[index]
    }

    fn region_ref<E: 'static, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> &E {
        &region[index]
    }

    fn region_read_all<E: Copy, const LEN: usize>(region: &Self::Region<E, LEN>) -> Vec<E> {
        (0..LEN).map(|index| region[index]).collect()
    }

    fn dyn_region_read<E: super::Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
    ) -> E {
        let mut value = MaybeUninit::<E>::uninit();

        // SAFETY: `raw_data` points to a byte slice which has same size as `E`.
        let raw_data = unsafe {
            slice::from_raw_parts_mut(value.as_mut_ptr().cast::<u8>(), mem::size_of::<E>())
        };

        region.read_bytes(address, raw_data);

        // SAFETY: `read_bytes` fully populates the contents of `values`. Additionally, `E: Elem`
        // lets us know that any byte combination is valid.
        let mut value = unsafe { value.assume_init() };

        value.from_stored_in_place();

        value
    }

    fn dyn_region_read_all<E: super::Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
        values: &mut [E],
    ) {
        // SAFETY: `E: Elem` tells us that values of that type would be arranged contiguously in
        // addition to values of `E` being valid for any raw byte combination.
        // Hence, obtain a slice that points to the same underlying memory as `values` and populate
        // it with the raw bytes.
        let raw_values = unsafe {
            let data = values.as_mut_ptr().cast::<u8>();
            let len = mem::size_of_val(values);
            slice::from_raw_parts_mut(data, len)
        };

        // `read_bytes` fills the entire slice.
        region.read_bytes(address, raw_values);

        for value in values {
            value.from_stored_in_place();
        }
    }

    fn enriched_cell_read_stored<V>(cell: &Self::EnrichedCell<V>) -> V::E
    where
        V: EnrichedValue,
        V::E: Copy,
    {
        *Self::enriched_cell_ref_stored(cell)
    }

    fn enriched_cell_read_derived<V>(cell: &Self::EnrichedCell<V>) -> V::D
    where
        V: super::EnrichedValueLinked,
        V::D: Copy,
    {
        let stored = Self::enriched_cell_ref_stored(cell);
        V::derive(stored)
    }

    fn enriched_cell_ref_stored<V>(cell: &Self::EnrichedCell<V>) -> &V::E
    where
        V: EnrichedValue,
    {
        Self::region_ref(&cell.underlying, 0)
    }
}

impl ManagerWrite for Verifier {
    fn region_write<E: 'static, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) {
        match region {
            Region::Absent => {
                // We can't uses `[None; LEN]` because `E: Copy` is not given.
                let mut data = Box::new(array::from_fn(|_| None));

                data[index] = Some(value);

                *region = Region::Partial(data);
            }

            Region::Partial(data) => {
                data[index] = Some(value);
            }
        }
    }

    fn region_write_all<E: Copy, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        values: &[E],
    ) {
        for (i, &value) in values.iter().enumerate() {
            Self::region_write(region, i, value);
        }
    }

    fn dyn_region_write<E: super::Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        mut value: E,
    ) {
        value.to_stored_in_place();

        let raw_data = unsafe {
            let raw_ptr = (&value as *const E).cast::<u8>();
            let len = mem::size_of::<E>();
            slice::from_raw_parts(raw_ptr, len)
        };

        region.write_bytes(address, raw_data);
    }

    fn dyn_region_write_all<E: super::Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        base_address: usize,
        values: &[E],
    ) {
        for (i, &value) in values.iter().enumerate() {
            let address = base_address + i * mem::size_of::<E>();
            Self::dyn_region_write(region, address, value);
        }
    }

    fn enriched_cell_write<V>(cell: &mut Self::EnrichedCell<V>, value: V::E)
    where
        V: super::EnrichedValueLinked,
    {
        Self::region_write(&mut cell.underlying, 0, value);
    }
}

impl ManagerReadWrite for Verifier {
    fn region_replace<E: Copy, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) -> E {
        let old = Self::region_read(region, index);
        Self::region_write(region, index, value);
        old
    }
}

impl ManagerClone for Verifier {
    fn clone_region<E: Clone + 'static, const LEN: usize>(
        region: &Self::Region<E, LEN>,
    ) -> Self::Region<E, LEN> {
        region.clone()
    }

    fn clone_dyn_region<const LEN: usize>(region: &Self::DynRegion<LEN>) -> Self::DynRegion<LEN> {
        region.clone()
    }

    fn clone_enriched_cell<V>(cell: &Self::EnrichedCell<V>) -> Self::EnrichedCell<V>
    where
        V: EnrichedValue,
        V::E: Clone,
        V::D: Clone,
    {
        cell.clone()
    }
}

/// Verifier region
#[derive(Clone)]
pub enum Region<E: 'static, const LEN: usize> {
    // We maintain a separate [`Absent`] variant in order to save space for regions that aren't
    // accessed at all.
    Absent,
    Partial(
        // This needs to be boxed to prevent inflating the size of this type for absent regions.
        Box<[Option<E>; LEN]>,
    ),
}

/// Represents either a present and complete region of the `Verifier` state
/// or specifies whether it is only partially present or completely absent.
pub enum PartialState<T> {
    /// A region is fully present
    Complete(T),
    /// A region is absent
    Absent,
    /// A region is only partially present
    Incomplete,
}

/// Reference to a complete region of the Verifier backend
pub struct CompleteRegionRef<'a, E, const LEN: usize> {
    region: &'a [Option<E>; LEN],
}

impl<E: serde::Serialize, const LEN: usize> serde::Serialize for CompleteRegionRef<'_, E, LEN> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Replicate [`<Owned as ManagerSerialise>::serialise_region`]

        // A special encoding for single-element regions helps clean up encoding for serialisation
        // formats that contain structures. For example, JSON, where single-element regions would
        // be represented as array singletons.
        if LEN == 1 {
            return self
                .region
                .first()
                .and_then(Option::as_ref)
                .ok_or_else(|| <S::Error as serde::ser::Error>::custom("Region is not complete"))?
                .serialize(serializer);
        }

        // We're serialising this as a fixed-sized tuple because otherwise `bincode` would prefix
        // the length of this array, which is not needed.
        let mut serializer = serializer.serialize_tuple(LEN)?;

        for item in self.region.iter() {
            serializer.serialize_element(item.as_ref().ok_or_else(|| {
                <S::Error as serde::ser::Error>::custom("Region is not complete")
            })?)?;
        }

        serializer.end()
    }
}

impl<E, const LEN: usize> Region<E, LEN> {
    /// Get the contents of the region if it is fully present or its status otherwise.
    pub fn get_partial_region(&self) -> PartialState<CompleteRegionRef<'_, E, LEN>> {
        let region = match self {
            Region::Absent => return PartialState::Absent,
            Region::Partial(region) => region,
        };

        for value in region.iter() {
            if value.is_none() {
                return PartialState::Incomplete;
            }
        }

        PartialState::Complete(CompleteRegionRef {
            region: region.as_ref(),
        })
    }
}

impl<E, const LEN: usize> Index<usize> for Region<E, LEN> {
    type Output = E;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Region::Absent => not_found(),
            Region::Partial(region) => match region.get(index).and_then(Option::as_ref) {
                Some(value) => value,
                None => not_found(),
            },
        }
    }
}

/// Page identifier
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct PageId<const LEAF_SIZE: usize>(usize);

impl<const LEAF_SIZE: usize> PageId<LEAF_SIZE> {
    const LEAF_SIZE: usize = {
        if LEAF_SIZE.count_ones() != 1 {
            panic!("LEAF_SIZE must be a power of 2");
        }

        LEAF_SIZE
    };

    const LEAF_MASK: usize = !(Self::LEAF_SIZE - 1);

    /// Construct a page idetifier from an address.
    pub fn from_address(address: usize) -> Self {
        PageId(address & Self::LEAF_MASK)
    }

    /// Calculate the offset of an address relative to the start of the identified page.
    pub fn offset(&self, address: usize) -> Option<usize> {
        address.checked_sub(self.0)
    }
}

/// Page of a dynamic region where sub-ranges may not be present
#[derive(Clone, Debug)]
pub struct Page<const LEAF_SIZE: usize> {
    data: Box<[u8; LEAF_SIZE]>,
    available: RangeSet2<usize>,
}

impl<const LEAF_SIZE: usize> Page<LEAF_SIZE> {
    /// Construct a page where the entire data is present.
    fn from_full(data: Box<[u8; LEAF_SIZE]>) -> Self {
        let available = RangeSet2::from(0..LEAF_SIZE);
        Page { data, available }
    }

    /// Read a sub-range of the page. Only returns `Some` if the entire range is present.
    fn get(&self, start: usize, len: usize) -> Option<&[u8]> {
        if len > LEAF_SIZE.saturating_sub(start) {
            return None;
        }

        let range = start..start.saturating_add(len);

        // Superset means that `self.available` fully covers `range`. In other words, everything in
        // `range` is also in `self.available`.
        if !self.available.is_superset(&RangeSet2::<usize>::from(range)) {
            return None;
        }

        Some(&self.data[start..][..len])
    }

    /// Write to a range in the page. This makes that range available to subsequent reads.
    fn put(&mut self, start: usize, data: &[u8]) -> bool {
        if data.len() > LEAF_SIZE.saturating_sub(start) {
            return false;
        }

        self.available.union_with(&RangeSet2::<usize>::from(
            start..data.len().saturating_add(start),
        ));

        self.data[start..][..data.len()].copy_from_slice(data);

        true
    }

    /// Returns true if every byte of the page is available.
    fn is_fully_available(&self) -> bool {
        self.available.boundaries() == [0, LEAF_SIZE]
    }
}

impl<const LEAF_SIZE: usize> Default for Page<LEAF_SIZE> {
    fn default() -> Self {
        Page {
            data: Box::new([0; LEAF_SIZE]),
            available: RangeSet2::empty(),
        }
    }
}

/// Verifier dynamic region
#[derive(Clone, Debug)]
pub struct DynRegion<const LEAF_SIZE: usize, const LEN: usize> {
    pages: BTreeMap<PageId<LEAF_SIZE>, Page<LEAF_SIZE>>,
}

impl<const LEAF_SIZE: usize, const LEN: usize> DynRegion<LEAF_SIZE, LEN> {
    const SANE: bool = {
        if LEN.rem_euclid(LEAF_SIZE) != 0 {
            panic!("LEN must be a multiple of LEAF_SIZE")
        }

        true
    };

    /// Construct a verifier dynamic region using the given known pages.
    pub fn from_pages(
        pages: impl IntoIterator<Item = (PageId<LEAF_SIZE>, Box<[u8; LEAF_SIZE]>)>,
    ) -> Self {
        if !Self::SANE {
            unreachable!()
        }

        let pages = pages
            .into_iter()
            .map(|(id, data)| (id, Page::from_full(data)))
            .collect();

        DynRegion { pages }
    }

    /// Read bytes from the dynamic region.
    pub fn read_bytes(&self, mut address: usize, mut buffer: &mut [u8]) {
        if buffer.is_empty() {
            return;
        }

        if buffer.len() > LEN.saturating_sub(address) {
            not_found()
        }

        while !buffer.is_empty() {
            let page_index = PageId::from_address(address);

            let Some(page) = self.pages.get(&page_index) else {
                not_found()
            };

            let Some(offset) = page_index.offset(address) else {
                not_found()
            };

            let chunk_length = buffer.len().min(LEAF_SIZE.saturating_sub(offset));

            let dst = &mut buffer[..chunk_length];
            let Some(src) = page.get(offset, chunk_length) else {
                not_found()
            };
            dst.copy_from_slice(src);

            address = address.saturating_add(chunk_length);
            buffer = &mut buffer[chunk_length..];
        }
    }

    /// Write bytes to the dynamic region.
    pub fn write_bytes(&mut self, mut address: usize, mut buffer: &[u8]) {
        if buffer.is_empty() {
            return;
        }

        if buffer.len() > LEN.saturating_sub(address) {
            not_found()
        }

        while !buffer.is_empty() {
            let page_index = PageId::from_address(address);
            let page = self.pages.entry(page_index).or_default();

            let Some(offset) = page_index.offset(address) else {
                not_found()
            };

            let chunk_length = buffer.len().min(LEAF_SIZE.saturating_sub(offset));

            let src = &buffer[..chunk_length];
            if !page.put(offset, src) {
                not_found()
            };

            address = address.saturating_add(chunk_length);
            buffer = &buffer[chunk_length..];
        }
    }

    /// Get the contents of a page if it is fully present or its status otherwise.
    pub fn get_partial_page(&self, id: PageId<LEAF_SIZE>) -> PartialState<&[u8; LEAF_SIZE]> {
        match self.pages.get(&id) {
            Some(page) if page.is_fully_available() => PartialState::Complete(page.data.as_ref()),
            Some(_) => PartialState::Incomplete,
            None => PartialState::Absent,
        }
    }
}

impl<const LEAF_SIZE: usize, const LEN: usize> Default for DynRegion<LEAF_SIZE, LEN> {
    fn default() -> Self {
        DynRegion {
            pages: BTreeMap::new(),
        }
    }
}

/// Verifier enriched cell
pub struct EnrichedCell<V: EnrichedValue> {
    underlying: Region<V::E, 1>,
}

impl<V: EnrichedValue> Clone for EnrichedCell<V>
where
    V::E: Clone,
{
    fn clone(&self) -> Self {
        Self {
            underlying: self.underlying.clone(),
        }
    }
}

impl<E> Cell<E, Verifier> {
    /// Construct an absent verifier cell.
    pub const fn absent() -> Self {
        Cell::bind(Region::Absent)
    }

    /// Construct a verifier cell with a value.
    pub fn from_owned(cell: Cell<E, Owned>) -> Self {
        let values = Box::new(cell.into_region().map(Some));
        let region = Region::Partial(values);
        Cell::bind(region)
    }
}

impl<E: Clone> TryFrom<Cell<E, Ref<'_, Verifier>>> for Cell<E, Owned> {
    type Error = PartialHashError;

    fn try_from(cell: Cell<E, Ref<'_, Verifier>>) -> Result<Self, Self::Error> {
        match cell.into_region() {
            Region::Absent => Err(PartialHashError::PotentiallyRecoverable),
            Region::Partial(value) => match value.as_ref() {
                [Some(v)] => Ok(Cell::bind([v.clone()])),
                [None] => Err(PartialHashError::PotentiallyRecoverable),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state_backend;
    use crate::state_backend::Cells;
    use crate::state_backend::DynCells;
    use crate::state_backend::EnrichedValueLinked;

    /// Ensures that page indices are properly calculated.
    #[test]
    fn page_index() {
        let page0 = [
            PageId::<4>::from_address(0),
            PageId::<4>::from_address(1),
            PageId::<4>::from_address(2),
            PageId::<4>::from_address(3),
        ];

        let page4 = [
            PageId::<4>::from_address(4),
            PageId::<4>::from_address(5),
            PageId::<4>::from_address(6),
            PageId::<4>::from_address(7),
        ];

        let page8 = [
            PageId::<4>::from_address(8),
            PageId::<4>::from_address(9),
            PageId::<4>::from_address(10),
            PageId::<4>::from_address(11),
        ];

        page0.into_iter().fold((), |_, item| {
            assert_eq!(item, page0[0]);
            assert!(item < page4[0]);
            assert!(item < page8[0]);
        });

        page4.into_iter().fold((), |_, item| {
            assert!(item > page0[0]);
            assert_eq!(item, page4[0]);
            assert!(item < page8[0]);
        });

        page8.into_iter().fold((), |_, item| {
            assert!(item > page0[0]);
            assert!(item > page4[0]);
            assert_eq!(item, page8[0]);
        });
    }

    /// Proptest value for a partial region
    type PartialRegionArb<E, const LEN: usize> = Box<[Option<E>; LEN]>;

    /// Proptest value for a region
    type RegionArb<E, const LEN: usize> = Option<PartialRegionArb<E, LEN>>;

    /// Construct [`Cells`] from a proptest value.
    fn arb_to_cells<E, const LEN: usize>(region: RegionArb<E, LEN>) -> Cells<E, LEN, Verifier> {
        let region = match region {
            Some(data) => Region::Partial(data),
            None => Region::Absent,
        };

        Cells::bind(region)
    }

    /// Check functionality of a region that is partially present.
    #[test]
    fn region_present() {
        proptest::proptest!(|(reg: PartialRegionArb<u64, 32>)| {
            let mut cells: Cells<_, 32, Verifier> = arb_to_cells(Some(reg.clone()));

            for i in 0..32 {
                let value = handle_stepper_panics(|| cells.read(i)).ok();
                proptest::prop_assert_eq!(value, reg[i]);

                let new_value = rand::random();
                cells.write(i, new_value);

                let read_value = cells.read(i);
                proptest::prop_assert_eq!(read_value, new_value);
            }
        });
    }

    /// Check functionality of a region that is absent.
    #[test]
    fn region_absent() {
        let cells: Cells<u64, 32, Verifier> = arb_to_cells(None);

        for i in 0..32 {
            let value = handle_stepper_panics(|| cells.read(i)).ok();
            assert_eq!(value, None);
        }
    }

    /// Construct a [`state_backend::EnrichedCell`] from a proptest value.
    fn arb_to_enriched_cell<V: EnrichedValueLinked>(
        value: Option<V::E>,
    ) -> state_backend::EnrichedCell<V, Verifier> {
        let cell = match value {
            Some(value) => Region::Partial(Box::new([Some(value)])),
            None => Region::Absent,
        };
        let cell = Cell::bind(cell);
        state_backend::EnrichedCell::bind(cell)
    }

    /// Check the functionality of an enriched cell whose value may or may not be present.
    #[test]
    fn enriched_cell() {
        struct Ident;

        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        struct Derived(u64);

        impl From<&'_ u64> for Derived {
            fn from(value: &u64) -> Self {
                Derived(*value)
            }
        }

        impl EnrichedValue for Ident {
            type E = u64;

            type D = Derived;
        }

        proptest::proptest!(|(initial: Option<u64>)| {
            let mut cell = arb_to_enriched_cell::<Ident>(initial);

            let stored = handle_stepper_panics(|| cell.read_stored()).ok();
            proptest::prop_assert_eq!(stored, initial);

            let derived = handle_stepper_panics(|| cell.read_derived()).ok();
            let expected_derived = initial.as_ref().map(<Ident as EnrichedValueLinked>::derive);
            proptest::prop_assert_eq!(derived, expected_derived);

            let new_value = rand::random();
            cell.write(new_value);

            let read_value = cell.read_stored();
            proptest::prop_assert_eq!(read_value, new_value);

            let new_derived = <Ident as EnrichedValueLinked>::derive(&new_value);
            let read_derived = cell.read_derived();
            proptest::prop_assert_eq!(read_derived, new_derived);
        });
    }

    macro_rules! assert_eq_found {
        ( $left:expr, $right:expr ) => {
            assert!(handle_stepper_panics(|| { $left }).is_ok_and(|v| v == $right))
        };
    }

    macro_rules! assert_not_found {
        ( $body:expr ) => {
            assert!(
                handle_stepper_panics(|| { $body })
                    .is_err_and(|e| matches!(e, ProofVerificationFailure::AbsentDataAccess(_)))
            )
        };
    }

    /// Check the read functionality of a region that has no gaps between its pages.
    #[test]
    fn dyn_region_continuous() {
        const LEAF_SIZE: usize = MERKLE_LEAF_SIZE.get();

        let mut dyn_region = DynRegion::default();
        dyn_region.write_bytes(
            0,
            [1, 3, 3, 7]
                .into_iter()
                .cycle()
                .take(LEAF_SIZE)
                .collect::<Vec<_>>()
                .as_slice(),
        );
        dyn_region.write_bytes(
            LEAF_SIZE,
            [11, 14, 14, 15]
                .into_iter()
                .cycle()
                .take(LEAF_SIZE)
                .collect::<Vec<_>>()
                .as_slice(),
        );

        let mut dyn_cells: DynCells<{ 3 * LEAF_SIZE }, Verifier> = DynCells::bind(dyn_region);

        // Read things that are contained in the first leaf.
        assert_eq_found!(dyn_cells.read::<[u8; 4]>(0), [1, 3, 3, 7]);
        assert_eq_found!(dyn_cells.read::<[u8; 4]>(1), [3, 3, 7, 1]);
        assert_eq_found!(dyn_cells.read::<[u8; 4]>(LEAF_SIZE - 4), [1, 3, 3, 7]);

        // Read things that span the first and second leaf.
        assert_eq_found!(dyn_cells.read::<[u8; 4]>(LEAF_SIZE - 2), [3, 7, 11, 14]);

        // Read things that are contained in the second leaf.
        assert_eq_found!(dyn_cells.read::<[u8; 4]>(LEAF_SIZE), [11, 14, 14, 15]);
        assert_eq_found!(dyn_cells.read::<[u8; 4]>(LEAF_SIZE + 1), [14, 14, 15, 11]);

        // Read more than is available.
        assert_not_found!(dyn_cells.read::<[u8; LEAF_SIZE * 3 + 1]>(0));

        // Read at an offset that is out of bounds.
        assert_not_found!(dyn_cells.read::<u8>(LEAF_SIZE * 2));

        // Write to an index that is out of bounds.
        assert_not_found!(dyn_cells.clone().write(LEAF_SIZE * 3, 0u8));

        // Add more to the third leaf.
        let dyn_cells = handle_stepper_panics(move || {
            dyn_cells.write(LEAF_SIZE * 2, [255u8, 0]);
            dyn_cells
        })
        .unwrap();
        assert_eq_found!(dyn_cells.read::<[u8; 6]>(LEAF_SIZE * 2 - 4), [
            11, 14, 14, 15, 255, 0
        ]);
        assert_eq_found!(dyn_cells.read::<[u8; 2]>(LEAF_SIZE * 2), [255, 0]);
        assert_not_found!(dyn_cells.read::<[u8; 4]>(LEAF_SIZE * 2));
        assert_not_found!(dyn_cells.read::<[u8; 2]>(LEAF_SIZE * 2 + 2));

        // Read at an offset that is out of bounds.
        assert_not_found!(dyn_cells.read::<u8>(LEAF_SIZE * 3));
    }

    /// Check the functionality of a region that has gaps between its pages.
    #[test]
    fn dyn_region_gaps() {
        const LEAF_SIZE: usize = MERKLE_LEAF_SIZE.get();

        let mut dyn_region = DynRegion::default();
        dyn_region.write_bytes(
            0,
            [7, 3, 3]
                .into_iter()
                .cycle()
                .take(LEAF_SIZE)
                .collect::<Vec<_>>()
                .as_slice(),
        );
        dyn_region.write_bytes(
            LEAF_SIZE * 2,
            [42, 41]
                .into_iter()
                .cycle()
                .take(LEAF_SIZE)
                .collect::<Vec<_>>()
                .as_slice(),
        );

        let mut dyn_cells: DynCells<{ 3 * LEAF_SIZE }, Verifier> = DynCells::bind(dyn_region);

        assert_eq_found!(dyn_cells.read::<[u8; 3]>(0), [7, 3, 3]);
        assert_eq_found!(dyn_cells.read::<[u8; 2]>(1), [3, 3]);
        assert_eq_found!(dyn_cells.read::<[u8; 1]>(LEAF_SIZE * 2), [42]);
        assert_eq_found!(dyn_cells.read::<[u8; 1]>(LEAF_SIZE * 2 + 1), [41]);

        // Read a range that covers a gap.
        assert_not_found!(dyn_cells.read::<[u8; LEAF_SIZE + 4]>(LEAF_SIZE - 2));
        assert_not_found!(dyn_cells.read::<[u8; LEAF_SIZE]>(LEAF_SIZE));

        // Write within the gap.
        let dyn_cells = handle_stepper_panics(move || {
            dyn_cells.write(LEAF_SIZE - 1, [1u8, 1, 3]);
            dyn_cells
        })
        .unwrap();

        assert_eq_found!(dyn_cells.read::<[u8; 3]>(LEAF_SIZE - 1), [1, 1, 3]);
        assert_eq_found!(dyn_cells.read::<[u8; 2]>(LEAF_SIZE), [1, 3]);
        assert_eq_found!(dyn_cells.read::<[u8; 4]>(LEAF_SIZE - 2), [3, 1, 1, 3]);

        assert_not_found!(dyn_cells.read::<[u8; 6]>(LEAF_SIZE - 1));
        assert_not_found!(dyn_cells.read::<[u8; 4]>(LEAF_SIZE));
    }
}
