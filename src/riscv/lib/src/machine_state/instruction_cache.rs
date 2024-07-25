// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module contains a cache mapping certain physical addresses to
//! instructions.
//!
//! The cache is structured as a flat array mapping
//! `phys_addr / 2 % CACHE_SIZE` to a possible cached instruction.
//!
//! Due to the bucketing, multiple physical addresses map to the same
//! index in the cache, while only one entry is stored. Therefore, on
//! each fetch, an additional check is made to ensure the cache entry
//! does indeed correspond to the requested physical address.
//!
//! If the cache entry [`Cached`] is indeed an entry for the given physical
//! address, it is probed for validity. If it is valid, the parsed
//! instruction is returned directly.
//!
//! A cache entry `c: Cached` is valid if
//! `c.fence_counter == i.fence_counter` for an `i: InstructionCache`
//! that contains the given cache entry.
//!
//! To avoid invalid-to-valid transitions when bumping the fence counter,
//! all cache entries need to be cleared at least once
//! before the fence counter inhabits a previous value.
//! The cache can't invalidate all entries at once because that would
//! result in a very large proof for a single invalidation step.
//! Therefore, we do it gradually.
//!
//! *NB* uncompressed instructions that cross page boundaries are not
//! cached, as they would have to be invalidated whenever the address
//! translation changes - as the upper address may no longer point
//! to the same relative physical address.

/// The number of entries in the instruction cache.
pub const CACHE_BITS: usize = 21;
pub const CACHE_SIZE: usize = 1 << CACHE_BITS;
pub const CACHE_MASK: usize = CACHE_SIZE - 1;

use crate::cache_utils::FenceCounter;
use crate::machine_state::address_translation::PAGE_SIZE;
use crate::machine_state::bus::Address;
use crate::parser::instruction::Instr;
use crate::parser::{is_compressed, parse};
use crate::state_backend::{
    AllocatedOf, Atom, Cell, CellRead, CellWrite, Elem, LazyCell, ManagerBase, ManagerRead,
    ManagerReadWrite, ManagerWrite, Many, Ref,
};
use std::convert::Infallible;

pub type CachedLayout = (Atom<FenceCounter>, Atom<u64>, Atom<Unparsed>);

pub struct Cached<M: ManagerBase> {
    fence_counter: Cell<FenceCounter, M>,
    phys_addr: Cell<Address, M>,
    instr: LazyCell<Unparsed, (Instr, Unparsed), M>,
}

impl<M: ManagerBase> Cached<M> {
    /// Bind the allocated cells.
    pub fn bind(space: AllocatedOf<CachedLayout, M>) -> Self {
        Self {
            fence_counter: space.0,
            phys_addr: space.1,
            instr: LazyCell::wrap(space.2),
        }
    }

    /// Reset the underlying storage.
    pub fn reset(&mut self)
    where
        M: ManagerWrite,
    {
        self.fence_counter.write(FenceCounter::INITIAL);
        self.phys_addr.write(0);
        self.instr.reset(Unparsed(0));
    }

    /// Obatin a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> AllocatedOf<CachedLayout, Ref<'_, M>> {
        (
            self.fence_counter.struct_ref(),
            self.phys_addr.struct_ref(),
            self.instr.struct_ref(),
        )
    }

    /// No valid instruction can be read from the zero-address.
    /// Therefore, we can invalidate a cached instruction by failing
    /// the `phys_addr` check.
    fn invalidate(&mut self)
    where
        M: ManagerWrite,
    {
        self.phys_addr.write(!0);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Unparsed(u32);

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

impl From<Unparsed> for (Instr, Unparsed) {
    fn from(unparsed: Unparsed) -> Self {
        let bytes = unparsed.0;
        let upper = bytes as u16;

        let instr = parse(upper, || {
            Result::<u16, Infallible>::Ok((bytes >> 16) as u16)
        })
        .unwrap();

        (instr, unparsed)
    }
}

impl From<(Instr, Unparsed)> for Unparsed {
    fn from((_, unparsed): (Instr, Unparsed)) -> Self {
        unparsed
    }
}

pub type InstructionCacheLayout = (Atom<FenceCounter>, Many<CachedLayout, CACHE_SIZE>);

pub struct InstructionCache<M: ManagerBase> {
    fence_counter: Cell<FenceCounter, M>,
    entries: Box<[Cached<M>; CACHE_SIZE]>,
}

impl<M: ManagerBase> InstructionCache<M> {
    /// Bind the instruction cache to the allocated storage.
    pub fn bind(space: AllocatedOf<InstructionCacheLayout, M>) -> Self {
        Self {
            fence_counter: space.0,
            entries: space
                .1
                .into_iter()
                .map(Cached::bind)
                .collect::<Vec<_>>()
                .into_boxed_slice()
                .try_into()
                .map_err(|_| "mismatching vector lengths for instruction cache")
                .unwrap(),
        }
    }

    /// Reset the underlying storage.
    pub fn reset(&mut self)
    where
        M: ManagerWrite,
    {
        self.fence_counter.write(FenceCounter::INITIAL);
        self.entries.iter_mut().for_each(Cached::reset);
    }

    /// Invalidate the instruction cache, subsequent fetches will return `None`.
    pub fn invalidate(&mut self)
    where
        M: ManagerReadWrite,
    {
        let counter = self.fence_counter.read();
        self.fence_counter.write(counter.next());

        // Ensure that every entry is invalidated at least once
        // before the counter wraps.
        self.entries[counter.0 as usize & CACHE_MASK].invalidate();
    }

    /// Obatin a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> AllocatedOf<InstructionCacheLayout, Ref<'_, M>> {
        (
            self.fence_counter.struct_ref(),
            self.entries.iter().map(Cached::struct_ref).collect(),
        )
    }

    /// Fetch the cached instruction at the given address, if an entry exists.
    #[inline(always)]
    pub fn fetch_instr(&self, phys_addr: Address) -> Option<Instr>
    where
        M: ManagerRead,
    {
        if phys_addr == 0 {
            return None;
        }

        let index = cache_index(phys_addr);
        let cached = &self.entries[index];

        if cached.fence_counter.read() == self.fence_counter.read()
            && cached.phys_addr.read() == phys_addr
        {
            let (instr, _) = cached.instr.read();
            Some(instr)
        } else {
            None
        }
    }

    /// Cache an instruction at the physical address.
    ///
    /// Both the parsed instruction and raw bytes should be supplied.
    #[inline(never)]
    pub fn cache_instr(&mut self, phys_addr: Address, raw: u32, instr: Instr)
    where
        M: ManagerReadWrite,
    {
        let unparsed = Unparsed(raw);
        debug_assert!(
            (instr, unparsed) == unparsed.into(),
            "The raw bytes {raw:#x} do not match the given instruction {instr:?}"
        );

        if !cacheable(phys_addr, raw) {
            return;
        }

        let index = cache_index(phys_addr);

        let fence_counter = self.fence_counter.read();

        let cached = &mut self.entries[index];
        cached.instr.write((instr, unparsed));
        cached.phys_addr.write(phys_addr);
        cached.fence_counter.write(fence_counter);
    }
}

// We know that phys_addr here is always u16-aligned.
// Therefore, we can safely halve the number of buckets we
// look at.
#[inline(always)]
const fn cache_index(phys_addr: Address) -> usize {
    (phys_addr >> 1) as usize & CACHE_MASK
}

/// An uncompressed instruction is not cacheable, if it crosses
/// page boundaries.
#[inline]
const fn cacheable(phys_addr: Address, raw: u32) -> bool {
    const END_OF_PAGE: Address = PAGE_SIZE - 2;

    is_compressed(raw) || phys_addr % PAGE_SIZE != END_OF_PAGE
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::registers::{a0, t0, t1},
        parser::instruction::{CIBTypeArgs, SBTypeArgs},
    };

    #[test]
    fn test_fence_counter_wrapping_protection() {
        let invalidation_count_until_wrapping = FenceCounter::MAX.0 as usize;
        let cache_entries = CACHE_SIZE;

        assert!(
            invalidation_count_until_wrapping > cache_entries,
            "The fence counter does a full cycle before all cache entries could be invalidated!"
        );
    }

    // Ensure that the initialised values for the instruction cache (ie phys_addr = 0)
    // are not actually returned from the cache. This would result in an incorrect
    // `UnknownInstr` being executed, rather than an `OutOfBounds` error (for reading from
    // devies).
    backend_test!(test_fetch_before_cache_misses, F, {
        let mut backend = create_backend!(InstructionCacheLayout, F);
        let state = create_state!(InstructionCache, InstructionCacheLayout, F, backend);

        let init_addr = state.entries[0].phys_addr.read();
        let fetched = state.fetch_instr(init_addr);

        assert!(fetched.is_none());
    });

    // Ensure that we do not cache an instruction that crosses page boundaries.
    // This prevents the (unlikely) scenario where an instruction's second half
    // changes due to a change in the virtual page mapping - in such a scenario,
    // we do not invalidate the instruction cache, although the physical address of
    // the second half of the instruction does actually change!
    backend_test!(test_never_cache_across_page_boundaries, F, {
        let mut backend = create_backend!(InstructionCacheLayout, F);
        let mut state = create_state!(InstructionCache, InstructionCacheLayout, F, backend);

        let compressed_bytes = 0x4505;
        let compressed = Instr::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let uncompressed_bytes = 0x00533423;
        let uncompressed = Instr::Sd(SBTypeArgs {
            rs1: t1,
            rs2: t0,
            imm: 8,
        });

        let phys_addr = PAGE_SIZE - 2;

        // Compressed instruction can be cached
        state.cache_instr(phys_addr, compressed_bytes, compressed);
        assert_eq!(Some(compressed), state.fetch_instr(phys_addr));

        // Caching an uncompressed instruction across page boundary should fail - the old
        // uncompressed instruction is still there.
        state.cache_instr(phys_addr, uncompressed_bytes, uncompressed);
        assert_eq!(Some(compressed), state.fetch_instr(phys_addr));
    });

    // Ensure
    // - on rebind the cached instructions are still present
    // - able to cache possibly overlapping instructions
    backend_test!(test_rebind, F, {
        let mut backend = create_backend!(InstructionCacheLayout, F);

        let compressed_bytes = 0x4505;
        let compressed = Instr::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let uncompressed_bytes = 0x00533423;
        let uncompressed = Instr::Sd(SBTypeArgs {
            rs1: t1,
            rs2: t0,
            imm: 8,
        });

        let phys_addr_uncompressed = 6;
        let phys_addr_compressed = 8;

        {
            let mut state = create_state!(InstructionCache, InstructionCacheLayout, F, backend);

            state.cache_instr(phys_addr_compressed, compressed_bytes, compressed);
            assert_eq!(Some(compressed), state.fetch_instr(phys_addr_compressed));

            state.cache_instr(phys_addr_uncompressed, uncompressed_bytes, uncompressed);
            assert_eq!(
                Some(uncompressed),
                state.fetch_instr(phys_addr_uncompressed)
            );
        }

        // Rebind state
        {
            let state = create_state!(InstructionCache, InstructionCacheLayout, F, backend);

            assert_eq!(Some(compressed), state.fetch_instr(phys_addr_compressed));
            assert_eq!(
                Some(uncompressed),
                state.fetch_instr(phys_addr_uncompressed)
            );
        }
    });
}
