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

use crate::cache_utils::FenceCounter;
use crate::machine_state::address_translation::PAGE_SIZE;
use crate::machine_state::bus::Address;
use crate::parser::instruction::Instr;
use crate::parser::{parse, parse_compressed_instruction, parse_uncompressed_instruction};
use crate::state_backend::{
    self, AllocatedOf, Atom, Cell, CellRead, CellWrite, Choreographer, Elem, Layout, LazyCell,
    ManagerAlloc, ManagerBase, ManagerRead, ManagerReadWrite, ManagerWrite, Many, Placed, Ref,
};
use std::convert::Infallible;

/// The layout of an entry in the instruction cache.
pub type CachedLayout = (Atom<FenceCounter>, Atom<u64>, Atom<Unparsed>);

/// An entry in the instruction cache - containing both the physical address & instruction.
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

/// Unparsed instruction used for storing cached instructions in the state.
///
/// Compressed instructions are represented as the lower-16 bits of the u32, with upper-16 bits
/// set to zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

/// Configuration object for the size of the instruction cache.
///
/// *NB* you should ensure `SIZE == 1 << BITS`, otherwise a compilation error will occur:
///
/// The following fails to compile:
/// ```compile_fail
/// # use octez_riscv::machine_state::instruction_cache::Sizes;
/// pub type SizesX = Sizes<15, { 1 << 16 }>;
/// println!("{}", SizesX::CACHE_SIZE);
/// ```
///
/// But the following would succeed:
/// ```no_run
/// # use octez_riscv::machine_state::instruction_cache::Sizes;
/// pub type SizesY = Sizes<15, { 1 << 15 }>;
/// println!("{}", SizesY::CACHE_SIZE);
/// ```
pub enum Sizes<const BITS: usize, const SIZE: usize> {}

impl<const BITS: usize, const SIZE: usize> Sizes<BITS, SIZE> {
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
    const fn cache_index(phys_addr: Address) -> usize {
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

/// The default instruction cache layout used in the sandbox/rollup node when running kernels.
pub type DefaultInstructionCacheLayout = Sizes<16, 65536>;

/// The instruction cache layout recommended for use in tests.
pub type TestInstructionCacheLayout = Sizes<12, 4096>;

/// Trait for capturing the different possible layouts of the instruction cache (i.e.
/// controlling the number of cache entries present).
pub trait InstructionCacheLayout: state_backend::Layout {
    type Entries<M: ManagerBase>;

    fn refl<M: state_backend::ManagerBase>(
        space: state_backend::AllocatedOf<Self, M>,
    ) -> InstructionCache<Self, M>
    where
        Self: Sized;

    fn entry<M: ManagerBase>(entries: &Self::Entries<M>, phys_addr: Address) -> &Cached<M>;

    fn entry_mut<M: ManagerBase>(
        entries: &mut Self::Entries<M>,
        phys_addr: Address,
    ) -> &mut Cached<M>;

    fn entries_reset<M: ManagerWrite>(entries: &mut Self::Entries<M>);

    fn struct_ref<M: ManagerBase>(
        cache: &InstructionCache<Self, M>,
    ) -> AllocatedOf<Self, Ref<'_, M>>
    where
        Self: Sized;
}

type SizesLayout<const SIZE: usize> = (Atom<FenceCounter>, Many<CachedLayout, SIZE>);

impl<const BITS: usize, const SIZE: usize> Layout for Sizes<BITS, SIZE> {
    type Placed = <SizesLayout<SIZE> as Layout>::Placed;

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        SizesLayout::<SIZE>::place_with(alloc)
    }

    fn placed() -> Placed<Self::Placed> {
        SizesLayout::<SIZE>::placed()
    }

    type Allocated<M: ManagerBase> = <SizesLayout<SIZE> as Layout>::Allocated<M>;

    fn allocate<M: ManagerAlloc>(backend: &mut M, placed: Self::Placed) -> Self::Allocated<M> {
        SizesLayout::<SIZE>::allocate(backend, placed)
    }
}

impl<const BITS: usize, const SIZE: usize> InstructionCacheLayout for Sizes<BITS, SIZE> {
    type Entries<M: ManagerBase> = Box<[Cached<M>; SIZE]>;

    fn refl<M: ManagerBase>(space: AllocatedOf<Self, M>) -> InstructionCache<Self, M> {
        InstructionCache {
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

    fn entry<M: ManagerBase>(entries: &Self::Entries<M>, phys_addr: Address) -> &Cached<M> {
        &entries[Self::cache_index(phys_addr)]
    }

    fn entry_mut<M: ManagerBase>(
        entries: &mut Self::Entries<M>,
        phys_addr: Address,
    ) -> &mut Cached<M> {
        &mut entries[Self::cache_index(phys_addr)]
    }

    fn entries_reset<M: ManagerWrite>(entries: &mut Self::Entries<M>) {
        entries.iter_mut().for_each(Cached::reset)
    }

    fn struct_ref<M: ManagerBase>(
        cache: &InstructionCache<Self, M>,
    ) -> AllocatedOf<Self, Ref<'_, M>> {
        (
            cache.fence_counter.struct_ref(),
            cache.entries.iter().map(Cached::struct_ref).collect(),
        )
    }
}

/// Instruction Cache caches instructions by physical address, allowing them to be
/// fetched directly rather than going through the *fetch memory/parse* cycle.
///
/// The number of cache entries is controlled by the `ICL` layout parameter.
pub struct InstructionCache<ICL: InstructionCacheLayout, M: ManagerBase> {
    fence_counter: Cell<FenceCounter, M>,
    entries: ICL::Entries<M>,
}

impl<ICL: InstructionCacheLayout, M: ManagerBase> InstructionCache<ICL, M> {
    /// Bind the instruction cache to the given allocated state.
    pub fn bind(space: AllocatedOf<ICL, M>) -> InstructionCache<ICL, M> {
        ICL::refl(space)
    }

    /// Reset the underlying storage.
    pub fn reset(&mut self)
    where
        M: ManagerWrite,
    {
        self.fence_counter.write(FenceCounter::INITIAL);
        ICL::entries_reset(&mut self.entries);
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
        ICL::entry_mut(&mut self.entries, counter.0 as Address).invalidate();
    }

    /// Obatin a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> AllocatedOf<ICL, Ref<'_, M>> {
        ICL::struct_ref(self)
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

        let cached = ICL::entry(&self.entries, phys_addr);

        if cached.fence_counter.read() == self.fence_counter.read()
            && cached.phys_addr.read() == phys_addr
        {
            let (instr, _) = cached.instr.read();
            Some(instr)
        } else {
            None
        }
    }

    /// Cache a compressed instruction at the physical address, returning the parsed instruction.
    #[inline]
    pub fn cache_compressed(&mut self, phys_addr: Address, raw: u16) -> Instr
    where
        M: ManagerReadWrite,
    {
        let instr = parse_compressed_instruction(raw);

        self.cache_inner(phys_addr, Unparsed(raw as u32), instr);

        instr
    }

    /// Cache an uncompressed instruction at the physical address, returning the parsed instruction.
    ///
    /// If the instruction crossed page boundaries, it will still be returned, but not cached.
    #[inline(never)]
    pub fn cache_uncompressed(&mut self, phys_addr: Address, raw: u32) -> Instr
    where
        M: ManagerReadWrite,
    {
        let instr = parse_uncompressed_instruction(raw);

        if cacheable_uncompressed(phys_addr) {
            self.cache_inner(phys_addr, Unparsed(raw), instr);
        }

        instr
    }

    #[inline]
    fn cache_inner(&mut self, phys_addr: Address, unparsed: Unparsed, instr: Instr)
    where
        M: ManagerReadWrite,
    {
        let fence_counter = self.fence_counter.read();

        let cached = ICL::entry_mut(&mut self.entries, phys_addr);
        cached.instr.write((instr, unparsed));
        cached.phys_addr.write(phys_addr);
        cached.fence_counter.write(fence_counter);
    }
}

/// An uncompressed instruction is not cacheable, if it crosses
/// page boundaries.
#[inline]
const fn cacheable_uncompressed(phys_addr: Address) -> bool {
    const END_OF_PAGE: Address = PAGE_SIZE - 2;

    phys_addr % PAGE_SIZE != END_OF_PAGE
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::registers::{a0, t0, t1},
        parser::instruction::{CIBTypeArgs, InstrCacheable, SBTypeArgs},
    };

    // Ensure that the initialised values for the instruction cache (ie phys_addr = 0)
    // are not actually returned from the cache. This would result in an incorrect
    // `UnknownInstr` being executed, rather than an `OutOfBounds` error (for reading from
    // devices).
    backend_test!(test_fetch_before_cache_misses, F, {
        let mut backend = create_backend!(TestInstructionCacheLayout, F);
        let state = create_state!(
            InstructionCache,
            TestInstructionCacheLayout,
            F,
            backend,
            TestInstructionCacheLayout
        );

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
        let mut backend = create_backend!(TestInstructionCacheLayout, F);
        let mut state = create_state!(
            InstructionCache,
            TestInstructionCacheLayout,
            F,
            backend,
            TestInstructionCacheLayout
        );

        let compressed_bytes = 0x4505;
        let compressed = Instr::Cacheable(InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 }));

        let uncompressed_bytes = 0x00533423;

        let phys_addr = PAGE_SIZE - 2;

        // Compressed instruction can be cached
        state.cache_compressed(phys_addr, compressed_bytes);
        assert_eq!(Some(compressed), state.fetch_instr(phys_addr));

        // Caching an uncompressed instruction across page boundary should fail - the old
        // uncompressed instruction is still there.
        state.cache_uncompressed(phys_addr, uncompressed_bytes);
        assert_eq!(Some(compressed), state.fetch_instr(phys_addr));
    });

    // Ensure
    // - on rebind the cached instructions are still present
    // - able to cache possibly overlapping instructions
    #[test]
    fn test_rebind() {
        // TODO: RV-210: Generalise for all testable backends.
        type F = crate::state_backend::memory_backend::test_helpers::InMemoryBackendFactory;

        let mut backend = create_backend!(TestInstructionCacheLayout, F);

        let compressed_bytes = 0x4505;
        let compressed = Instr::Cacheable(InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 }));

        let uncompressed_bytes = 0x00533423;
        let uncompressed = Instr::Cacheable(InstrCacheable::Sd(SBTypeArgs {
            rs1: t1,
            rs2: t0,
            imm: 8,
        }));

        let phys_addr_uncompressed = 6;
        let phys_addr_compressed = 8;

        {
            let mut state = create_state!(
                InstructionCache,
                TestInstructionCacheLayout,
                F,
                backend,
                TestInstructionCacheLayout
            );

            state.cache_compressed(phys_addr_compressed, compressed_bytes);
            assert_eq!(Some(compressed), state.fetch_instr(phys_addr_compressed));

            state.cache_uncompressed(phys_addr_uncompressed, uncompressed_bytes);
            assert_eq!(
                Some(uncompressed),
                state.fetch_instr(phys_addr_uncompressed)
            );
        }

        // Rebind state
        {
            let state = create_state!(
                InstructionCache,
                TestInstructionCacheLayout,
                F,
                backend,
                TestInstructionCacheLayout
            );

            assert_eq!(Some(compressed), state.fetch_instr(phys_addr_compressed));
            assert_eq!(
                Some(uncompressed),
                state.fetch_instr(phys_addr_uncompressed)
            );
        }
    }
}
