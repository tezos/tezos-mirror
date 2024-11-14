// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::bus::Address,
    parser::{
        instruction::{Instr, InstrCacheable},
        parse,
    },
    state_backend::{Choreographer, Elem, Layout, ManagerAlloc, ManagerBase, Many, Placed},
};
use std::{convert::Infallible, marker::PhantomData};

/// Integer to keep track of the fence counter
#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub struct FenceCounter(pub u32);

impl FenceCounter {
    /// Initial fence counter
    pub const INITIAL: Self = Self(0);

    /// Maximum fence counter value
    pub const MAX: Self = Self(u32::MAX);

    /// Increment the fence counter.
    #[inline]
    pub fn next(self) -> Self {
        Self(self.0.wrapping_add(1))
    }
}

impl Elem for FenceCounter {
    #[inline(always)]
    fn store(&mut self, source: &Self) {
        self.0.store(&source.0);
    }

    #[inline(always)]
    fn to_stored_in_place(&mut self) {
        self.0.to_stored_in_place();
    }

    #[inline(always)]
    fn from_stored_in_place(&mut self) {
        self.0.from_stored_in_place();
    }

    #[inline(always)]
    fn from_stored(source: &Self) -> Self {
        Self(u32::from_stored(&source.0))
    }
}

/// Unparsed instruction used for storing cached instructions in the state.
///
/// Compressed instructions are represented as the lower-16 bits of the u32, with upper-16 bits
/// set to zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[repr(transparent)]
pub struct Unparsed(pub u32);

impl Elem for Unparsed {
    #[inline(always)]
    fn store(&mut self, source: &Self) {
        self.0.store(&source.0)
    }

    #[inline(always)]
    fn to_stored_in_place(&mut self) {
        self.0.to_stored_in_place()
    }

    #[inline(always)]
    fn from_stored_in_place(&mut self) {
        self.0.from_stored_in_place()
    }

    #[inline(always)]
    fn from_stored(source: &Self) -> Self {
        Self(u32::from_stored(&source.0))
    }
}

impl From<Unparsed> for (InstrCacheable, Unparsed) {
    fn from(unparsed: Unparsed) -> Self {
        let bytes = unparsed.0;
        let upper = bytes as u16;

        let instr = parse(upper, || {
            Result::<u16, Infallible>::Ok((bytes >> 16) as u16)
        })
        .unwrap();

        match instr {
            Instr::Cacheable(i) => (i, unparsed),
            // As written, this code path is unreachable.
            // We can convert it into a static requirement by allowing
            // errors on bind, instead
            //
            // TODO RV-221: on bind, we should error if an instruction's
            //      bytes correspond to an Uncacheable instruction, rather
            //      than returning an 'Unknown' instruction.
            Instr::Uncacheable(_) => (InstrCacheable::Unknown { instr: bytes }, unparsed),
        }
    }
}

impl From<(InstrCacheable, Unparsed)> for Unparsed {
    fn from((_, unparsed): (InstrCacheable, Unparsed)) -> Self {
        unparsed
    }
}

/// Configuration object for the size of a cache indexed by physical address.
///
/// *NB* you should ensure `SIZE == 1 << BITS`, otherwise a compilation error will occur.
pub struct Sizes<const BITS: usize, const SIZE: usize, CachedLayout>(
    PhantomData<CachedLayout>,
    Infallible,
);

impl<const BITS: usize, const SIZE: usize, CachedLayout> Sizes<BITS, SIZE, CachedLayout> {
    pub const CACHE_SIZE: usize = if 1 << BITS == SIZE {
        SIZE
    } else {
        panic!("BITS parameter does not match SIZE parameter");
    };

    const CACHE_MASK: usize = {
        Self::fence_counter_wrapping_protection();
        Self::CACHE_SIZE - 1
    };

    // We know that phys_addr here is always u16-aligned.
    // Therefore, we can safely halve the number of buckets we
    // look at.
    #[inline(always)]
    pub const fn cache_index(phys_addr: Address) -> usize {
        (phys_addr >> 1) as usize & Self::CACHE_MASK
    }

    /// Assert that the fence counter would not wrap before every cache entry has been invalidated
    /// _at least_ once.
    const fn fence_counter_wrapping_protection() {
        let invalidation_count_until_wrapping = FenceCounter::MAX.0 as usize;
        let cache_entries = Self::CACHE_SIZE;

        assert!(
            invalidation_count_until_wrapping > cache_entries,
            "The fence counter does a full cycle before all cache entries could be invalidated!"
        );
    }
}

type SizesLayout<const SIZE: usize, CachedLayout> = Many<CachedLayout, SIZE>;

impl<const BITS: usize, const SIZE: usize, CachedLayout: Layout> Layout
    for Sizes<BITS, SIZE, CachedLayout>
{
    type Placed = <SizesLayout<SIZE, CachedLayout> as Layout>::Placed;

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        SizesLayout::<SIZE, CachedLayout>::place_with(alloc)
    }

    fn placed() -> Placed<Self::Placed> {
        SizesLayout::<SIZE, CachedLayout>::placed()
    }

    type Allocated<M: ManagerBase> = <SizesLayout<SIZE, CachedLayout> as Layout>::Allocated<M>;

    fn allocate<M: ManagerAlloc>(backend: &mut M, placed: Self::Placed) -> Self::Allocated<M> {
        SizesLayout::<SIZE, CachedLayout>::allocate(backend, placed)
    }
}
