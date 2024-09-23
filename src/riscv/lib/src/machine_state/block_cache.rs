// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::state_backend::{self, CellWrite};
use crate::{
    cache_utils::FenceCounter,
    parser::instruction::InstrCacheable,
    state_backend::{
        AllocatedOf, Atom, Cell, LazyCell, ManagerBase, ManagerRead, ManagerReadWrite,
        ManagerWrite, Ref,
    },
    traps::Exception,
};

use super::address_translation::PAGE_SIZE;
use super::instruction_cache::ValidatedCacheEntry;
use super::MachineCoreState;
use super::{
    bus::{main_memory, Address},
    ProgramCounterUpdate,
};
use crate::cache_utils::{Sizes, Unparsed};

const CACHE_INSTR: usize = 20;

pub type CachedLayout = (
    Atom<Address>,
    Atom<FenceCounter>,
    Atom<u8>,
    [Atom<Unparsed>; CACHE_INSTR],
);

pub struct Cached<M: ManagerBase> {
    address: Cell<Address, M>,
    fence_counter: Cell<FenceCounter, M>,
    len_instr: Cell<u8, M>,
    instr: [LazyCell<Unparsed, (InstrCacheable, Unparsed), M>; CACHE_INSTR],
}

impl<M: ManagerBase> Cached<M> {
    fn bind(space: AllocatedOf<CachedLayout, M>) -> Self {
        Self {
            address: space.0,
            fence_counter: space.1,
            len_instr: space.2,
            instr: space
                .3
                .into_iter()
                .map(|p| LazyCell::wrap(p))
                .collect::<Vec<_>>()
                .try_into()
                .ok()
                .unwrap(),
        }
    }

    fn invalidate(&mut self)
    where
        M: ManagerWrite,
    {
        self.address.write(0);
        self.len_instr.write(0);
    }

    fn reset(&mut self)
    where
        M: ManagerWrite,
    {
        self.address.write(0);
        self.fence_counter.write(FenceCounter::INITIAL);
        self.len_instr.write(0);
        self.instr.iter_mut().for_each(|lc| lc.reset(Unparsed(0)));
    }

    fn block(&mut self) -> Block<M>
    where
        M: ManagerRead,
    {
        Block {
            instr: &mut self.instr[..self.len_instr.read() as usize],
        }
    }

    fn struct_ref(&self) -> AllocatedOf<CachedLayout, Ref<'_, M>> {
        (
            self.address.struct_ref(),
            self.fence_counter.struct_ref(),
            self.len_instr.struct_ref(),
            self.instr
                .iter()
                .map(LazyCell::struct_ref)
                .collect::<Vec<_>>()
                .try_into()
                .map_err(|_| "mismatching lengths for block instructions")
                .unwrap(),
        )
    }
}

/// The default instruction cache index bits.
pub const DEFAULT_CACHE_BITS: usize = 16;

/// The default instruction cache size.
pub const DEFAULT_CACHE_SIZE: usize = 1 << DEFAULT_CACHE_BITS;

/// The default instruction cache index bits for tests.
pub const TEST_CACHE_BITS: usize = 12;

/// The default instruction cache for tests.
pub const TEST_CACHE_SIZE: usize = 1 << TEST_CACHE_BITS;

/// Trait for capturing the different possible layouts of the instruction cache (i.e.
/// controlling the number of cache entries present).
pub trait BlockCacheLayout: state_backend::Layout {
    type Entries<M: ManagerBase>;
    type Sizes;

    fn refl<M: state_backend::ManagerBase>(
        space: state_backend::AllocatedOf<Self, M>,
    ) -> BlockCache<Self, M>
    where
        Self: Sized;

    fn entry<M: ManagerBase>(entries: &Self::Entries<M>, phys_addr: Address) -> &Cached<M>;

    fn entry_mut<M: ManagerBase>(
        entries: &mut Self::Entries<M>,
        phys_addr: Address,
    ) -> &mut Cached<M>;

    fn entries_reset<M: ManagerWrite>(entries: &mut Self::Entries<M>);

    fn struct_ref<M: ManagerBase>(cache: &BlockCache<Self, M>) -> AllocatedOf<Self, Ref<'_, M>>
    where
        Self: Sized;
}

pub type Layout<const BITS: usize, const SIZE: usize> =
    (Atom<Address>, Atom<u8>, Sizes<BITS, SIZE, CachedLayout>);

impl<const BITS: usize, const SIZE: usize> BlockCacheLayout for Layout<BITS, SIZE> {
    type Entries<M: ManagerBase> = Box<[Cached<M>; SIZE]>;
    type Sizes = Sizes<BITS, SIZE, CachedLayout>;

    fn refl<M: ManagerBase>(space: AllocatedOf<Self, M>) -> BlockCache<Self, M> {
        BlockCache {
            phys_addr: space.0,
            offset: space.1,
            fence_counter: space.2 .0,
            entries: space
                .2
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
        &entries[Self::Sizes::cache_index(phys_addr)]
    }

    fn entry_mut<M: ManagerBase>(
        entries: &mut Self::Entries<M>,
        phys_addr: Address,
    ) -> &mut Cached<M> {
        &mut entries[Self::Sizes::cache_index(phys_addr)]
    }

    fn entries_reset<M: ManagerWrite>(entries: &mut Self::Entries<M>) {
        entries.iter_mut().for_each(Cached::reset)
    }

    fn struct_ref<M: ManagerBase>(cache: &BlockCache<Self, M>) -> AllocatedOf<Self, Ref<'_, M>> {
        (
            cache.phys_addr.struct_ref(),
            cache.offset.struct_ref(),
            (
                cache.fence_counter.struct_ref(),
                cache.entries.iter().map(Cached::struct_ref).collect(),
            ),
        )
    }
}

pub struct BlockCache<BCL: BlockCacheLayout, M: ManagerBase> {
    phys_addr: Cell<Address, M>,
    fence_counter: Cell<FenceCounter, M>,
    offset: Cell<u8, M>,
    entries: BCL::Entries<M>,
}

impl<BCL: BlockCacheLayout, M: ManagerBase> BlockCache<BCL, M> {
    pub fn bind(space: AllocatedOf<BCL, M>) -> Self {
        BCL::refl(space)
    }

    pub fn invalidate(&mut self)
    where
        M: ManagerReadWrite,
    {
        let counter = self.fence_counter.read();
        self.fence_counter.write(counter.next());
        self.reset_to(0);
        BCL::entry_mut(&mut self.entries, counter.0 as Address).invalidate();
    }

    pub fn reset(&mut self)
    where
        M: ManagerWrite,
    {
        self.fence_counter.write(FenceCounter::INITIAL);
        self.reset_to(0);
        BCL::entries_reset(&mut self.entries);
    }

    pub fn struct_ref(&self) -> AllocatedOf<BCL, Ref<'_, M>> {
        BCL::struct_ref(self)
    }

    pub fn push_instr<const WIDTH: u8>(&mut self, entry: ValidatedCacheEntry)
    where
        M: ManagerReadWrite,
    {
        let (phys_addr, instr, unparsed) = entry.unwrap();

        let mut current_start = self.phys_addr.read();
        let mut offset = self.offset.read();

        if phys_addr % PAGE_SIZE == 0 || phys_addr != current_start + offset as Address {
            self.reset_to(phys_addr);
            current_start = phys_addr;
            offset = 0;
        }

        let next = offset + WIDTH;
        if self.cache_inner(current_start, phys_addr, instr, unparsed) {
            self.reset_to(next as u64)
        } else {
            self.offset.write(offset + WIDTH);
        }
    }

    fn reset_to(&mut self, pa: Address)
    where
        M: ManagerWrite,
    {
        self.phys_addr.write(pa);
        self.offset.write(0);
    }

    fn cache_inner(
        &mut self,
        block_addr: Address,
        phys_addr: Address,
        instr: InstrCacheable,
        bytes: Unparsed,
    ) -> bool
    where
        M: ManagerReadWrite,
    {
        let entry = BCL::entry_mut(&mut self.entries, block_addr);

        let start = entry.address.read();
        if start != block_addr || entry.address.read() == phys_addr {
            entry.address.write(block_addr);
            entry.len_instr.write(0);
            entry.fence_counter.write(self.fence_counter.read());
        }

        let len_instr = entry.len_instr.read();

        entry.instr[len_instr as usize].write((instr, bytes));

        let new_len = len_instr + 1;
        entry.len_instr.write(new_len);

        new_len == CACHE_INSTR as u8
    }

    pub fn get_block(&mut self, phys_addr: Address) -> Option<Block<M>>
    where
        M: ManagerReadWrite,
    {
        let entry = BCL::entry_mut(&mut self.entries, phys_addr);

        if entry.address.read() == phys_addr
            && self.fence_counter.read() == entry.fence_counter.read()
        {
            let block = entry.block();
            Some(block)
        } else {
            None
        }
    }
}

pub struct Block<'a, M: ManagerBase> {
    instr: &'a mut [LazyCell<Unparsed, (InstrCacheable, Unparsed), M>],
}

impl<'a, M: ManagerRead> Block<'a, M> {
    pub fn num_instr(&self) -> usize {
        self.instr.len()
    }

    pub(super) fn run_block<ML>(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        instr_pc: &mut Address,
        _phys_addr: Address,
        steps: &mut usize,
    ) -> Result<ProgramCounterUpdate, Exception>
    where
        ML: main_memory::MainMemoryLayout,
        M: ManagerReadWrite,
    {
        for i in self.instr.iter_mut().map(|i| &i.read_ref().0) {
            match core.run_instr_cacheable(i) {
                Err(e) => return Err(e),
                Ok(r @ ProgramCounterUpdate::Set(_)) => {
                    return Ok(r);
                }
                Ok(ProgramCounterUpdate::Add(width)) => {
                    *instr_pc += width;
                }
            }
            core.hart.pc.write(*instr_pc);
            *steps += 1;
        }
        Ok(ProgramCounterUpdate::Set(*instr_pc))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::registers::{a0, t0, t1},
        parser::instruction::{CIBTypeArgs, SBTypeArgs},
    };

    pub type TestLayout = Layout<TEST_CACHE_BITS, TEST_CACHE_SIZE>;

    // writing CACHE_INSTR to the block cache creates new block
    backend_test!(test_writing_full_block_fetchable_uncompressed, F, {
        let mut backend = create_backend!(TestLayout, F);
        let mut state = create_state!(BlockCache, TestLayout, F, backend, TestLayout);

        let uncompressed_bytes = 0x00533423;
        let uncompressed = InstrCacheable::Sd(SBTypeArgs {
            rs1: t1,
            rs2: t0,
            imm: 8,
        });

        let phys_addr = 10;

        for offset in 0..(CACHE_INSTR as u64) {
            let entry = ValidatedCacheEntry::from_unchecked(
                phys_addr + (offset * 4),
                uncompressed,
                Unparsed(uncompressed_bytes),
            );
            state.push_instr::<4>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());
    });

    backend_test!(test_writing_full_block_fetchable_compressed, F, {
        let mut backend = create_backend!(TestLayout, F);
        let mut state = create_state!(BlockCache, TestLayout, F, backend, TestLayout);

        let compressed_bytes = 0x4505;
        let compressed = InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let phys_addr = 10;

        for offset in 0..(CACHE_INSTR as u64) {
            let entry = ValidatedCacheEntry::from_unchecked(
                phys_addr + (offset * 2),
                compressed,
                Unparsed(compressed_bytes),
            );
            state.push_instr::<2>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());
    });

    // writing instructions immediately creates block
    backend_test!(test_writing_half_block_fetchable_compressed, F, {
        let mut backend = create_backend!(TestLayout, F);
        let mut state = create_state!(BlockCache, TestLayout, F, backend, TestLayout);

        let compressed_bytes = 0x4505;
        let compressed = InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let phys_addr = 10;

        for offset in 0..((CACHE_INSTR / 2) as u64) {
            let entry = ValidatedCacheEntry::from_unchecked(
                phys_addr + (offset * 2),
                compressed,
                Unparsed(compressed_bytes),
            );
            state.push_instr::<2>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR / 2, block.unwrap().num_instr());
    });

    backend_test!(test_writing_two_blocks_fetchable_compressed, F, {
        let mut backend = create_backend!(TestLayout, F);
        let mut state = create_state!(BlockCache, TestLayout, F, backend, TestLayout);

        let compressed_bytes = 0x4505;
        let compressed = InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let phys_addr = 10;

        for offset in 0..((CACHE_INSTR * 2) as u64) {
            let entry = ValidatedCacheEntry::from_unchecked(
                phys_addr + (offset * 2),
                compressed,
                Unparsed(compressed_bytes),
            );
            state.push_instr::<2>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());

        let block = state.get_block(50);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());
    });

    // writing across pages offset two blocks next to each other
    backend_test!(test_crossing_page_exactly_creates_new_block, F, {
        let mut backend = create_backend!(TestLayout, F);
        let mut state = create_state!(BlockCache, TestLayout, F, backend, TestLayout);

        let compressed_bytes = 0x4505;
        let compressed = InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let phys_addr = PAGE_SIZE - 10;

        for offset in 0..10 {
            let entry = ValidatedCacheEntry::from_unchecked(
                phys_addr + (offset * 2),
                compressed,
                Unparsed(compressed_bytes),
            );
            state.push_instr::<2>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(5, block.unwrap().num_instr());

        let block = state.get_block(phys_addr + 10);
        assert!(block.is_some());
        assert_eq!(5, block.unwrap().num_instr());
    });

    // writing across pages offset creates two blocks with a gap
    // TODO: needs to be done at the machine state level
    backend_test!(
        #[ignore]
        test_crossing_page_offset_creates_new_block,
        F,
        {
            let mut backend = create_backend!(TestLayout, F);
            let mut state = create_state!(BlockCache, TestLayout, F, backend, TestLayout);

            let uncompressed_bytes = 0x00533423;
            let uncompressed = InstrCacheable::Sd(SBTypeArgs {
                rs1: t1,
                rs2: t0,
                imm: 8,
            });

            let phys_addr = PAGE_SIZE - 6;

            for offset in 0..3 {
                let entry = ValidatedCacheEntry::from_unchecked(
                    phys_addr + (offset * 4),
                    uncompressed,
                    Unparsed(uncompressed_bytes),
                );
                state.push_instr::<4>(entry);
            }

            let block = state.get_block(phys_addr);
            assert!(block.is_some());
            assert_eq!(1, block.unwrap().num_instr());

            let block = state.get_block(phys_addr + 4);
            assert!(block.is_none());

            let block = state.get_block(phys_addr + 8);
            assert!(block.is_some());
            assert_eq!(1, block.unwrap().num_instr());
        }
    );
}
