// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! The block cache maps certain physical addresses to contiguous sequences of instructions, or
//! 'blocks'. These blocks will never cross page boundaries.
//!
//! Blocks are sets of instructions that are often - but not always - executed immediately after each
//! other. The main exceptions to this are:
//! - instructions may trigger exceptions (e.g. `OutOfBounds` when interacting with memory)
//! - branching instructions
//!
//! Specifically, blocks that contain a backwards branching instruction will often terminate with
//! that instruction, and blocks that contain a forwards jump will often contain those internally.
//!
//! This is due to the recommended behaviour mentioned in the ISA:
//! > Software should also assume that backward branches will be predicted taken and forward branches
//! > as not taken, at least the first time they are encountered.
//!
//! Therefore, sets of instructions that form tight loops will naturally form a block, and
//! sequences that may branch elsewhere - but normally fall-through - also form a block.
//!
//! Blocks must never cross page boundaries, as two pages that lie next to each other in physical
//! memory have no guarantees of being consecutive in virtual memory - even if they are at one
//! point.
//!
//! # Determinism
//!
//! Some special care is required when thinking about block execution
//! with respect to determinism.
//!
//! Blocks fundamentally rely on being executed 'as a whole'. This is
//! required to be able to make the most of various optimisations we
//! can apply to groups of instructions, while ensuring that we are
//! compatible with the remaining infrastructure of the stepper at block
//! entry/exit.
//! For example, this could include something as simple as not
//! updating the step counter until we exit a block.
//!
//! Only ever running blocks when sufficient steps remain, however,
//! causes determinism issues. Namely, when insufficient steps remain
//! to run a block, what do we do?
//!
//! The obvious solution is to fall back to the instruction cache, and
//! continue our *fetch/parse/run* cycle. This exact solution, however,
//! causes divergence: the instructions we end up executing from the
//! instruction cache, could be completely different to those stored in
//! the block cache, for the same physical address.
//!
//! This can occur due to the differring nature of cache entries between
//! the instruction & block cache: entries in the instruction cache are
//! 1 instruction per slot, whereas in the block cache it's instead a
//! block (a sequence of instructions) per slot. Therefore, an entry
//! getting overriden in the instruction/block cache at address *A*, does
//! not invalidate all instructions in the block cache that correspond
//! to *A*, as they could also exist in blocks at nearby preceding
//! addresses.
//!
//! ## Solution
//!
//! Instead, we introduce the notion of a [`PartialBlock`], that can
//! remember that we were executing a _specific_ entry in the block cache
//! - and indeed the progress made.
//!
//! Then, when insufficient steps are remaining to run a block in full,
//! we proceed to run the block anyway, but step-by-step. Once we
//! exhaust any remaining steps, we save progress in a partial block,
//! and execute the remainder of it with [`BlockCache::complete_current_block`]
//! on the next iteration.
//!
//! Since we now guarantee that we always execute the _same_ set of instructions,
//! no matter how many steps are remaining, we solve this possible divergence.

use crate::cache_utils::Sizes;
use crate::state_backend::{self, ManagerClone};
use crate::traps::EnvironException;
use crate::{
    cache_utils::FenceCounter,
    parser::instruction::InstrCacheable,
    state_backend::{
        AllocatedOf, Atom, Cell, ManagerBase, ManagerRead, ManagerReadWrite, ManagerWrite, Ref,
    },
    traps::Exception,
};

use super::address_translation::PAGE_OFFSET_WIDTH;
use super::bus::main_memory::MainMemoryLayout;
use super::instruction_cache::ValidatedCacheEntry;
use super::MachineCoreState;
use super::{
    bus::{main_memory, Address},
    ProgramCounterUpdate,
};

/// Mask for getting the offset within a page
const PAGE_OFFSET_MASK: usize = (1 << PAGE_OFFSET_WIDTH) - 1;

/// The maximum number of instructions that may be contained in a block.
const CACHE_INSTR: usize = 20;

/// The layout of block cache entries, see [`Cached`] for more information.
pub type CachedLayout = (
    Atom<Address>,
    Atom<FenceCounter>,
    Atom<u8>,
    [Atom<InstrCacheable>; CACHE_INSTR],
);

/// Block cache entry.
///
/// Contains the physical address & fence counter for validity checks, and
/// the instructions buffer - nominally containing `len_instr` instructions.
pub struct Cached<M: ManagerBase> {
    address: Cell<Address, M>,
    fence_counter: Cell<FenceCounter, M>,
    len_instr: Cell<u8, M>,
    instr: [Cell<InstrCacheable, M>; CACHE_INSTR],
}

impl<M: ManagerBase> Cached<M> {
    fn bind(space: AllocatedOf<CachedLayout, M>) -> Self {
        Self {
            address: space.0,
            fence_counter: space.1,
            len_instr: space.2,
            instr: space.3,
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
        self.instr
            .iter_mut()
            .for_each(|lc| lc.write(InstrCacheable::Unknown { instr: 0 }));
    }

    fn start_block(&mut self, block_addr: Address, fence_counter: FenceCounter)
    where
        M: ManagerWrite,
    {
        self.address.write(block_addr);
        self.len_instr.write(0);
        self.fence_counter.write(fence_counter);
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
            self.instr.each_ref().map(Cell::struct_ref),
        )
    }
}

impl<M: ManagerClone> Clone for Cached<M> {
    fn clone(&self) -> Self {
        Self {
            address: self.address.clone(),
            fence_counter: self.fence_counter.clone(),
            len_instr: self.len_instr.clone(),
            instr: self.instr.clone(),
        }
    }
}

/// The default instruction cache index bits.
pub const DEFAULT_CACHE_BITS: usize = 20;

/// The default instruction cache size.
pub const DEFAULT_CACHE_SIZE: usize = 1 << DEFAULT_CACHE_BITS;

/// The default instruction cache index bits for tests.
pub const TEST_CACHE_BITS: usize = 12;

/// The default instruction cache for tests.
pub const TEST_CACHE_SIZE: usize = 1 << TEST_CACHE_BITS;

/// Layout of a partial block.
pub type PartialBlockLayout = (Atom<u64>, Atom<bool>, Atom<u8>);

/// Structure used to remember that a block was only partway executed
/// before needing to pause due to `steps == steps_max`.
///
/// If a block is being partially executed, if either:
/// - an error occurs
/// - a jump or branch occurs
/// then the partial block is reset, and execution will continue with
/// a potentially different block.
pub struct PartialBlock<M: ManagerBase> {
    phys_addr: Cell<u64, M>,
    in_progress: Cell<bool, M>,
    progress: Cell<u8, M>,
}

impl<M: ManagerClone> Clone for PartialBlock<M> {
    fn clone(&self) -> Self {
        Self {
            phys_addr: self.phys_addr.clone(),
            in_progress: self.in_progress.clone(),
            progress: self.progress.clone(),
        }
    }
}

impl<M: ManagerBase> PartialBlock<M> {
    fn bind(space: AllocatedOf<PartialBlockLayout, M>) -> Self {
        Self {
            phys_addr: space.0,
            in_progress: space.1,
            progress: space.2,
        }
    }

    fn struct_ref(&self) -> AllocatedOf<PartialBlockLayout, Ref<'_, M>> {
        (
            self.phys_addr.struct_ref(),
            self.in_progress.struct_ref(),
            self.progress.struct_ref(),
        )
    }

    fn reset(&mut self)
    where
        M: ManagerWrite,
    {
        self.in_progress.write(false);
        self.phys_addr.write(0);
        self.progress.write(0);
    }
}

/// Trait for capturing the different possible layouts of the instruction cache (i.e.
/// controlling the number of cache entries present).
pub trait BlockCacheLayout: state_backend::Layout {
    type Entries<M: ManagerBase>;
    type Sizes;

    fn bind<M: state_backend::ManagerBase>(
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

    fn clone_entries<M: ManagerClone>(entries: &Self::Entries<M>) -> Self::Entries<M>;
}

/// The layout of the block cache.
pub type Layout<const BITS: usize, const SIZE: usize> = (
    Atom<Address>,
    Atom<Address>,
    Atom<FenceCounter>,
    PartialBlockLayout,
    Sizes<BITS, SIZE, CachedLayout>,
);

impl<const BITS: usize, const SIZE: usize> BlockCacheLayout for Layout<BITS, SIZE> {
    type Entries<M: ManagerBase> = Box<[Cached<M>; SIZE]>;
    type Sizes = Sizes<BITS, SIZE, CachedLayout>;

    fn bind<M: ManagerBase>(space: AllocatedOf<Self, M>) -> BlockCache<Self, M> {
        BlockCache {
            current_block_addr: space.0,
            next_instr_addr: space.1,
            fence_counter: space.2,
            partial_block: PartialBlock::bind(space.3),
            entries: space
                .4
                .into_iter()
                .map(Cached::bind)
                .collect::<Vec<_>>()
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
            cache.current_block_addr.struct_ref(),
            cache.next_instr_addr.struct_ref(),
            cache.fence_counter.struct_ref(),
            cache.partial_block.struct_ref(),
            cache.entries.iter().map(Cached::struct_ref).collect(),
        )
    }

    fn clone_entries<M: ManagerClone>(entries: &Self::Entries<M>) -> Self::Entries<M> {
        entries
            .to_vec()
            .try_into()
            .map_err(|_| "mismatching vector lengths in block cache")
            .unwrap()
    }
}

/// The block cache - caching sequences of instructions by physical address.
///
/// The number of entries is controlled by the `BCL` layout parameter.
pub struct BlockCache<BCL: BlockCacheLayout, M: ManagerBase> {
    current_block_addr: Cell<Address, M>,
    next_instr_addr: Cell<Address, M>,
    fence_counter: Cell<FenceCounter, M>,
    partial_block: PartialBlock<M>,
    entries: BCL::Entries<M>,
}

impl<BCL: BlockCacheLayout, M: ManagerBase> BlockCache<BCL, M> {
    /// Bind the block cache to the given allocated state.
    pub fn bind(space: AllocatedOf<BCL, M>) -> Self {
        BCL::bind(space)
    }

    /// Invalidate all entries in the block cache.
    pub fn invalidate(&mut self)
    where
        M: ManagerReadWrite,
    {
        let counter = self.fence_counter.read();
        self.fence_counter.write(counter.next());
        self.reset_to(0);
        BCL::entry_mut(&mut self.entries, counter.0 as Address).invalidate();
    }

    /// Reset the underlying storage.
    pub fn reset(&mut self)
    where
        M: ManagerWrite,
    {
        self.fence_counter.write(FenceCounter::INITIAL);
        self.reset_to(0);
        BCL::entries_reset(&mut self.entries);
        self.partial_block.reset();
    }

    /// Obtain a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> AllocatedOf<BCL, Ref<'_, M>> {
        BCL::struct_ref(self)
    }

    /// Push an instruction to the block cache.
    ///
    /// By virtue of [`ValidatedCacheEntry`], this is safe to place in the cache.
    pub fn push_instr<const WIDTH: u64>(&mut self, entry: ValidatedCacheEntry)
    where
        M: ManagerReadWrite,
    {
        let (phys_addr, instr) = entry.to_inner();

        let next_addr = self.next_instr_addr.read();

        // If the instruction is at the start of the page, we _must_ start a new block,
        // as we cannot allow blocks to cross page boundaries.
        if phys_addr & PAGE_OFFSET_MASK as u64 == 0 || phys_addr != next_addr {
            self.reset_to(phys_addr);
        }

        self.cache_inner::<WIDTH>(phys_addr, instr);
    }

    fn reset_to(&mut self, phys_addr: Address)
    where
        M: ManagerWrite,
    {
        self.current_block_addr.write(phys_addr);
        self.next_instr_addr.write(0);
    }

    /// Add the instruction into a block.
    ///
    /// If the block is full, a new block will be started.
    fn cache_inner<const WIDTH: u64>(&mut self, phys_addr: Address, instr: InstrCacheable)
    where
        M: ManagerReadWrite,
    {
        let mut block_addr = self.current_block_addr.read();
        let fence_counter = self.fence_counter.read();

        let mut entry = BCL::entry_mut(&mut self.entries, block_addr);
        let start = entry.address.read();

        let mut len_instr = entry.len_instr.read();

        if start != block_addr || entry.address.read() == phys_addr {
            entry.start_block(block_addr, fence_counter);
            len_instr = 0;
        } else if len_instr == CACHE_INSTR as u8 {
            // The current block is full, start a new one
            self.reset_to(phys_addr);
            block_addr = phys_addr;

            entry = BCL::entry_mut(&mut self.entries, block_addr);
            entry.start_block(block_addr, fence_counter);

            len_instr = 0;
        }

        entry.instr[len_instr as usize].write(instr);

        let new_len = len_instr + 1;
        entry.len_instr.write(new_len);

        self.next_instr_addr.write(phys_addr + WIDTH);
    }

    /// Lookup a block by a physical address.
    ///
    /// If one is found it can then be executed with [`Block::run_block`].
    ///
    /// *NB* before running any block, you must ensure no partial block
    /// is in progress with [`BlockCache::complete_current_block`].
    pub fn get_block(&mut self, phys_addr: Address) -> Option<Block<M>>
    where
        M: ManagerRead,
    {
        debug_assert!(
            !self.partial_block.in_progress.read(),
            "Get block was called with a partial block in progress"
        );

        let entry = BCL::entry_mut(&mut self.entries, phys_addr);

        if entry.address.read() == phys_addr
            && self.fence_counter.read() == entry.fence_counter.read()
        {
            Some(entry.block())
        } else {
            None
        }
    }

    /// Complete a block that was only partially executed.
    ///
    /// This can happen when `steps + block.len_instr() > steps_max`, in
    /// which case we only executed instructions until `steps == steps_max`.
    pub fn complete_current_block<ML: MainMemoryLayout>(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        steps: &mut usize,
        max_steps: usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        if !self.partial_block.in_progress.read() {
            return Ok(());
        }

        self.run_partial_inner(core, steps, max_steps)
    }

    /// Run a block against the machine state.
    ///
    /// When calling this function, there must be no partial block in progress. To ensure
    /// this, you must always run [`BlockCache::complete_current_block`] prior to fetching
    /// and running a new block.
    pub fn run_block_partial<ML>(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        block_addr: Address,
        steps: &mut usize,
        max_steps: usize,
    ) -> Result<(), EnvironException>
    where
        ML: main_memory::MainMemoryLayout,
        M: ManagerReadWrite,
    {
        // start a new block
        self.partial_block.in_progress.write(true);
        self.partial_block.progress.write(0);
        self.partial_block.phys_addr.write(block_addr);

        self.run_partial_inner(core, steps, max_steps)
    }

    fn run_partial_inner<ML>(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        steps: &mut usize,
        max_steps: usize,
    ) -> Result<(), EnvironException>
    where
        ML: main_memory::MainMemoryLayout,
        M: ManagerReadWrite,
    {
        // Protect against partial blocks being executed when
        // no steps are remaining
        if *steps >= max_steps {
            return Ok(());
        }

        let mut progress = self.partial_block.progress.read();
        let address = self.partial_block.phys_addr.read();

        let entry = BCL::entry_mut(&mut self.entries, address);
        let mut instr_pc = core.hart.pc.read();

        let range = progress as usize..entry.len_instr.read() as usize;
        for i in entry.instr[range].iter() {
            match core.run_instr_cacheable(i.as_ref()) {
                Ok(ProgramCounterUpdate::Next(width)) => {
                    instr_pc += width as u64;
                    core.hart.pc.write(instr_pc);
                    *steps += 1;
                    progress += 1;

                    if *steps >= max_steps {
                        break;
                    }
                }
                Ok(ProgramCounterUpdate::Set(instr_pc)) => {
                    // Setting the instr_pc implies execution continuing
                    // elsewhere - and no longer within the current block.
                    //
                    // TODO RV-245
                    // We should make branching instructions return `Add`
                    // variant, in the case that the jump condition is
                    // not met - to allow further instructions in the
                    // block to be executed directly.
                    core.hart.pc.write(instr_pc);
                    *steps += 1;
                    self.partial_block.reset();
                    return Ok(());
                }
                Err(e) => {
                    self.partial_block.reset();
                    // Exceptions lead to a new address being set to handle it,
                    // with no guarantee of it being the next instruction.
                    core.handle_step_result(instr_pc, Err(e))?;
                    // If we succesfully handled an error, need to increment steps one more.
                    *steps += 1;
                    return Ok(());
                }
            }
        }

        if progress == entry.len_instr.read() {
            // We finished the block in exactly the number of steps left
            self.partial_block.reset();
        } else {
            // Remember the progress made through the block, when we later
            // continue executing it
            self.partial_block.progress.write(progress);
        }

        Ok(())
    }
}

impl<BCL: BlockCacheLayout, M: ManagerClone> Clone for BlockCache<BCL, M> {
    fn clone(&self) -> Self {
        Self {
            current_block_addr: self.current_block_addr.clone(),
            fence_counter: self.fence_counter.clone(),
            next_instr_addr: self.next_instr_addr.clone(),
            partial_block: self.partial_block.clone(),
            entries: BCL::clone_entries(&self.entries),
        }
    }
}

/// A block fetched from the block cache, that can be executed against the machine state.
pub struct Block<'a, M: ManagerBase> {
    instr: &'a mut [Cell<InstrCacheable, M>],
}

impl<'a, M: ManagerRead> Block<'a, M> {
    /// The number of instructions contained in the block.
    pub fn num_instr(&self) -> usize {
        self.instr.len()
    }

    /// Run a block against the machine state.
    ///
    /// When calling this function, there must be no partial block in progress. To ensure
    /// this, you must always run [`BlockCache::complete_current_block`] prior to fetching
    /// and running a new block.
    ///
    /// There _must_ also be sufficient steps remaining, to execute the block in full.
    pub fn run_block<ML>(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        mut instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        ML: main_memory::MainMemoryLayout,
        M: ManagerReadWrite,
    {
        if let Err(e) = self.run_block_inner(core, &mut instr_pc, steps) {
            core.handle_step_result(instr_pc, Err(e))?;
            // If we succesfully handled an error, need to increment steps one more.
            *steps += 1;
        }

        Ok(())
    }

    fn run_block_inner<ML>(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        instr_pc: &mut Address,
        steps: &mut usize,
    ) -> Result<(), Exception>
    where
        ML: main_memory::MainMemoryLayout,
        M: ManagerReadWrite,
    {
        for i in self.instr.iter() {
            match core.run_instr_cacheable(i.as_ref()) {
                Ok(ProgramCounterUpdate::Next(width)) => {
                    *instr_pc += width as u64;
                    core.hart.pc.write(*instr_pc);
                    *steps += 1;
                }
                Ok(ProgramCounterUpdate::Set(instr_pc)) => {
                    // Setting the instr_pc implies execution continuing
                    // elsewhere - and no longer within the current block.
                    //
                    // TODO RV-245
                    // We should make branching instructions return `Add`
                    // variant, in the case that the jump condition is
                    // not met - to allow further instructions in the
                    // block to be executed directly.
                    core.hart.pc.write(instr_pc);
                    *steps += 1;
                    break;
                }
                Err(e) => {
                    // Exceptions lead to a new address being set to handle it,
                    // with no guarantee of it being the next instruction.
                    return Err(e);
                }
            }
        }

        Ok(())
    }

    /// Retrieve the underlying instructions contained in the given block.
    #[cfg(test)]
    pub(super) fn to_vec(&self) -> Vec<InstrCacheable>
    where
        M: ManagerRead,
    {
        self.instr.iter().map(|cell| cell.read()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend_test, create_state,
        machine_state::{
            address_translation::PAGE_SIZE,
            bus,
            main_memory::tests::T1K,
            registers::{a0, a1, t0, t1},
            MachineCoreState, MachineCoreStateLayout,
        },
        parser::instruction::{CIBTypeArgs, ITypeArgs, SBTypeArgs},
    };

    pub type TestLayout = Layout<TEST_CACHE_BITS, TEST_CACHE_SIZE>;

    // writing CACHE_INSTR to the block cache creates new block
    backend_test!(test_writing_full_block_fetchable_uncompressed, F, {
        let mut state = create_state!(BlockCache, TestLayout, F, TestLayout);

        let uncompressed = InstrCacheable::Sd(SBTypeArgs {
            rs1: t1,
            rs2: t0,
            imm: 8,
        });

        let phys_addr = 10;

        for offset in 0..(CACHE_INSTR as u64) {
            let entry = ValidatedCacheEntry::from_raw(phys_addr + (offset * 4), uncompressed);
            state.push_instr::<4>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());
    });

    backend_test!(test_writing_full_block_fetchable_compressed, F, {
        let mut state = create_state!(BlockCache, TestLayout, F, TestLayout);

        let compressed = InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let phys_addr = 10;

        for offset in 0..(CACHE_INSTR as u64) {
            let entry = ValidatedCacheEntry::from_raw(phys_addr + (offset * 2), compressed);
            state.push_instr::<2>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());
    });

    // writing instructions immediately creates block
    backend_test!(test_writing_half_block_fetchable_compressed, F, {
        let mut state = create_state!(BlockCache, TestLayout, F, TestLayout);

        let compressed = InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let phys_addr = 10;

        for offset in 0..((CACHE_INSTR / 2) as u64) {
            let entry = ValidatedCacheEntry::from_raw(phys_addr + (offset * 2), compressed);
            state.push_instr::<2>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR / 2, block.unwrap().num_instr());
    });

    backend_test!(test_writing_two_blocks_fetchable_compressed, F, {
        let mut state = create_state!(BlockCache, TestLayout, F, TestLayout);

        let compressed = InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let phys_addr = 10;

        for offset in 0..((CACHE_INSTR * 2) as u64) {
            let entry = ValidatedCacheEntry::from_raw(phys_addr + (offset * 2), compressed);
            state.push_instr::<2>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());

        let block = state.get_block(phys_addr + 2 * CACHE_INSTR as u64);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());
    });

    // writing across pages offset two blocks next to each other
    backend_test!(test_crossing_page_exactly_creates_new_block, F, {
        let mut state = create_state!(BlockCache, TestLayout, F, TestLayout);

        let compressed = InstrCacheable::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 });

        let phys_addr = PAGE_SIZE - 10;

        for offset in 0..10 {
            let entry = ValidatedCacheEntry::from_raw(phys_addr + (offset * 2), compressed);
            state.push_instr::<2>(entry);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(5, block.unwrap().num_instr());

        let block = state.get_block(phys_addr + 10);
        assert!(block.is_some());
        assert_eq!(5, block.unwrap().num_instr());
    });

    backend_test!(test_partial_block_executes, F, {
        let mut core_state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
        let mut block_state = create_state!(BlockCache, TestLayout, F, TestLayout);

        let addiw = InstrCacheable::Addiw(ITypeArgs {
            rd: a1,
            rs1: a1,
            imm: 257,
        });

        let block_addr = bus::start_of_main_memory::<T1K>();

        for offset in 0..10 {
            block_state.push_instr::<4>(ValidatedCacheEntry::from_raw(
                block_addr + offset * 4,
                addiw,
            ));
        }

        core_state.hart.pc.write(block_addr);

        // Execute the first 5 instructions
        let mut steps = 0;
        block_state
            .run_block_partial(&mut core_state, block_addr, &mut steps, 5)
            .unwrap();

        assert_eq!(steps, 5);
        assert!(block_state.partial_block.in_progress.read());
        assert_eq!(5, block_state.partial_block.progress.read());
        assert_eq!(block_addr, block_state.partial_block.phys_addr.read());
        assert_eq!(block_addr + 5 * 4, core_state.hart.pc.read());

        // Execute no steps
        let mut steps = 0;
        block_state
            .complete_current_block(&mut core_state, &mut steps, 0)
            .unwrap();

        assert_eq!(steps, 0);
        assert!(block_state.partial_block.in_progress.read());
        assert_eq!(5, block_state.partial_block.progress.read());
        assert_eq!(block_addr, block_state.partial_block.phys_addr.read());
        assert_eq!(block_addr + 5 * 4, core_state.hart.pc.read());

        // Execute the next 2 instructions
        let mut steps = 0;
        block_state
            .complete_current_block(&mut core_state, &mut steps, 2)
            .unwrap();

        assert_eq!(steps, 2);
        assert!(block_state.partial_block.in_progress.read());
        assert_eq!(7, block_state.partial_block.progress.read());
        assert_eq!(block_addr, block_state.partial_block.phys_addr.read());
        assert_eq!(block_addr + 7 * 4, core_state.hart.pc.read());

        // Finish the block. We don't consume all the steps
        let mut steps = 0;
        block_state
            .complete_current_block(&mut core_state, &mut steps, 5)
            .unwrap();

        assert_eq!(steps, 3);
        assert!(!block_state.partial_block.in_progress.read());
        assert_eq!(0, block_state.partial_block.progress.read());
        assert_eq!(0, block_state.partial_block.phys_addr.read());
        assert_eq!(block_addr + 10 * 4, core_state.hart.pc.read());
    });
}
