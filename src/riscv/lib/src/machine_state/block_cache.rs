// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
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
//!
//! # Dispatch
//!
//! The method of dispatch for Blocks can be one of several mechanisms, the current
//! default being [`Interpreted`].
//!
//! [`Interpreted`]: bcall::Interpreted

pub mod bcall;

use super::MachineCoreState;
use super::address_translation::PAGE_OFFSET_WIDTH;
use super::instruction::Instruction;
use super::main_memory::MainMemoryLayout;
use super::{ProgramCounterUpdate, main_memory::Address};
use crate::machine_state::address_translation::PAGE_SIZE;
use crate::parser::instruction::InstrWidth;
use crate::state_backend::{
    self, AllocatedOf, Atom, Cell, EnrichedCell, EnrichedValue, ManagerBase, ManagerClone,
    ManagerRead, ManagerReadWrite, ManagerSerialise, ManagerWrite, Ref, proof_backend,
};
use crate::traps::{EnvironException, Exception};
use crate::{cache_utils::FenceCounter, state_backend::FnManager};
use crate::{
    cache_utils::Sizes,
    storage::{Hash, HashError},
};
use crate::{default::ConstDefault, state_backend::verify_backend};
use crate::{machine_state::instruction::Args, storage::binary};
use bcall::{BCall, Block};
use std::marker::PhantomData;
use std::u64;

/// Mask for getting the offset within a page
const PAGE_OFFSET_MASK: usize = (1 << PAGE_OFFSET_WIDTH) - 1;

/// The maximum number of instructions that may be contained in a block.
pub const CACHE_INSTR: usize = 20;

/// Layout for an [`ICallPlaced`].
pub struct ICallLayout<ML> {
    _pd: PhantomData<ML>,
}

impl<ML: MainMemoryLayout> state_backend::Layout for ICallLayout<ML> {
    type Allocated<M: state_backend::ManagerBase> = EnrichedCell<ICallPlaced<ML>, M>;

    fn allocate<M: state_backend::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        let value = backend.allocate_enriched_cell(Instruction::DEFAULT);
        EnrichedCell::bind(value)
    }
}

impl<ML: MainMemoryLayout> state_backend::CommitmentLayout for ICallLayout<ML> {
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        Hash::blake2b_hash(state)
    }
}

impl<ML: MainMemoryLayout> state_backend::ProofLayout for ICallLayout<ML> {
    fn to_merkle_tree(
        state: state_backend::RefProofGenOwnedAlloc<Self>,
    ) -> Result<proof_backend::merkle::MerkleTree, HashError> {
        let serialised = binary::serialise(&state)?;
        proof_backend::merkle::MerkleTree::make_merkle_leaf(
            serialised,
            state.cell_ref().get_access_info(),
        )
    }

    fn from_proof(proof: state_backend::ProofTree) -> state_backend::FromProofResult<Self> {
        let leaf = proof.into_leaf()?;

        let cell = match leaf {
            state_backend::ProofPart::Present(data) => {
                let value = binary::deserialise(data)?;
                verify_backend::EnrichedCell::Present(value)
            }
            state_backend::ProofPart::Absent => verify_backend::EnrichedCell::Absent,
        };

        Ok(EnrichedCell::bind(cell))
    }
}

/// Bindings for deriving an [`ICall`] from an [`Instruction`] via the [`EnrichedCell`] mechanism.
pub struct ICallPlaced<ML: MainMemoryLayout> {
    _pd: PhantomData<ML>,
}

impl<ML: MainMemoryLayout> EnrichedValue for ICallPlaced<ML> {
    type E = Instruction;

    type D<M: ManagerBase> = ICall<ML, M>;
}

/// A function derived from an [OpCode] that can be directly run over the [MachineCoreState].
///
/// This allows static dispatch of this function during block construction,
/// rather than for each instruction, during each block execution.
///
/// [OpCode]: super::instruction::OpCode
pub struct ICall<ML: MainMemoryLayout, M: ManagerBase> {
    /// SAFETY: This function must be called with an `Args` belonging to the same `OpCode` as
    /// the one used to dispatch this function.
    run_instr:
        unsafe fn(&Args, &mut MachineCoreState<ML, M>) -> Result<ProgramCounterUpdate, Exception>,
}

impl<ML: MainMemoryLayout, M: ManagerBase> Clone for ICall<ML, M> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<ML: MainMemoryLayout, M: ManagerBase> Copy for ICall<ML, M> {}

impl<ML: MainMemoryLayout, M: ManagerReadWrite> ICall<ML, M> {
    // SAFETY: This function must be called with an `Args` belonging to the same `OpCode` as
    // the one used to dispatch this function.
    #[inline(always)]
    unsafe fn run(
        &self,
        args: &Args,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        (self.run_instr)(args, core)
    }
}

impl<'a, ML: MainMemoryLayout, M: ManagerReadWrite> From<&'a Instruction> for ICall<ML, M> {
    fn from(value: &'a Instruction) -> Self {
        let run_instr = value.opcode.to_run::<ML, M>();
        Self { run_instr }
    }
}

/// Layout for an address cell
pub struct AddressCellLayout;

impl state_backend::Layout for AddressCellLayout {
    type Allocated<M: state_backend::ManagerBase> = Cell<Address, M>;

    fn allocate<M: state_backend::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        Cell::bind(backend.allocate_region([!0]))
    }
}

impl state_backend::CommitmentLayout for AddressCellLayout {
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        <Atom<Address> as state_backend::CommitmentLayout>::state_hash(state)
    }
}

impl state_backend::ProofLayout for AddressCellLayout {
    fn to_merkle_tree(
        state: state_backend::RefProofGenOwnedAlloc<Self>,
    ) -> Result<proof_backend::merkle::MerkleTree, HashError> {
        <Atom<Address> as state_backend::ProofLayout>::to_merkle_tree(state)
    }

    fn from_proof(proof: state_backend::ProofTree) -> state_backend::FromProofResult<Self> {
        <Atom<Address> as state_backend::ProofLayout>::from_proof(proof)
    }
}

/// The layout of block cache entries, see [`Cached`] for more information.
pub type CachedLayout<ML> = (
    AddressCellLayout,
    Atom<FenceCounter>,
    Atom<u8>,
    [ICallLayout<ML>; CACHE_INSTR],
);

/// Block cache entry.
///
/// Contains the physical address & fence counter for validity checks, the
/// underlying [`Block`] state.
pub struct Cached<ML: MainMemoryLayout, B: Block<ML, M>, M: ManagerBase> {
    block: B,
    address: Cell<Address, M>,
    fence_counter: Cell<FenceCounter, M>,
    _pd: PhantomData<ML>,
}

impl<ML: MainMemoryLayout, B: Block<ML, M>, M: ManagerBase> Cached<ML, B, M> {
    fn bind(space: AllocatedOf<CachedLayout<ML>, M>) -> Self {
        Self {
            address: space.0,
            fence_counter: space.1,
            block: B::bind((space.2, space.3)),
            _pd: PhantomData,
        }
    }

    fn invalidate(&mut self)
    where
        M: ManagerWrite,
    {
        self.address.write(!0);
        self.block.invalidate();
    }

    fn reset(&mut self)
    where
        M: ManagerReadWrite,
    {
        self.address.write(!0);
        self.fence_counter.write(FenceCounter::INITIAL);
        self.block.reset();
    }

    fn start_block(&mut self, block_addr: Address, fence_counter: FenceCounter)
    where
        M: ManagerWrite,
    {
        self.address.write(block_addr);
        self.block.start_block();
        self.fence_counter.write(fence_counter);
    }

    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<CachedLayout<ML>, F::Output> {
        let (len_instr, instr) = self.block.struct_ref::<'a, F>();
        (
            self.address.struct_ref::<F>(),
            self.fence_counter.struct_ref::<F>(),
            len_instr,
            instr,
        )
    }
}

impl<ML: MainMemoryLayout, B: Block<ML, M> + Clone, M: ManagerClone> Clone for Cached<ML, B, M> {
    fn clone(&self) -> Self {
        Self {
            address: self.address.clone(),
            fence_counter: self.fence_counter.clone(),
            block: self.block.clone(),
            _pd: PhantomData,
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
pub type PartialBlockLayout = (Atom<Address>, Atom<bool>, Atom<u8>);

/// Structure used to remember that a block was only partway executed
/// before needing to pause due to `steps == steps_max`.
///
/// If a block is being partially executed, if either:
/// - an error occurs
/// - a jump or branch occurs
/// then the partial block is reset, and execution will continue with
/// a potentially different block.
pub struct PartialBlock<M: ManagerBase> {
    phys_addr: Cell<Address, M>,
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

    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<PartialBlockLayout, F::Output> {
        (
            self.phys_addr.struct_ref::<F>(),
            self.in_progress.struct_ref::<F>(),
            self.progress.struct_ref::<F>(),
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
pub trait BlockCacheLayout: state_backend::CommitmentLayout + state_backend::ProofLayout {
    type MainMemoryLayout: MainMemoryLayout;
    type Entries<B: Block<Self::MainMemoryLayout, M>, M: ManagerBase>;
    type Sizes;

    fn bind<B: Block<Self::MainMemoryLayout, M>, M: state_backend::ManagerBase>(
        space: state_backend::AllocatedOf<Self, M>,
        block_builder: B::BlockBuilder,
    ) -> BlockCache<Self, B, Self::MainMemoryLayout, M>
    where
        Self: Sized;

    fn entry<B: Block<Self::MainMemoryLayout, M>, M: ManagerBase>(
        entries: &Self::Entries<B, M>,
        phys_addr: Address,
    ) -> &Cached<Self::MainMemoryLayout, B, M>;

    fn entry_mut<B: Block<Self::MainMemoryLayout, M>, M: ManagerBase>(
        entries: &mut Self::Entries<B, M>,
        phys_addr: Address,
    ) -> &mut Cached<Self::MainMemoryLayout, B, M>;

    fn entries_reset<B: Block<Self::MainMemoryLayout, M>, M: ManagerReadWrite>(
        entries: &mut Self::Entries<B, M>,
    );

    fn struct_ref<
        'a,
        B: Block<Self::MainMemoryLayout, M>,
        M: ManagerBase,
        F: FnManager<Ref<'a, M>>,
    >(
        cache: &'a BlockCache<Self, B, Self::MainMemoryLayout, M>,
    ) -> AllocatedOf<Self, F::Output>
    where
        Self: Sized;

    fn clone_entries<B: Block<Self::MainMemoryLayout, M> + Clone, M: ManagerClone>(
        entries: &Self::Entries<B, M>,
    ) -> Self::Entries<B, M>;
}

/// The layout of the block cache.
pub type Layout<ML, const BITS: usize, const SIZE: usize> = (
    AddressCellLayout,
    AddressCellLayout,
    Atom<FenceCounter>,
    PartialBlockLayout,
    Sizes<BITS, SIZE, CachedLayout<ML>>,
);

impl<ML: MainMemoryLayout, const BITS: usize, const SIZE: usize> BlockCacheLayout
    for Layout<ML, BITS, SIZE>
{
    type MainMemoryLayout = ML;

    type Entries<B: Block<Self::MainMemoryLayout, M>, M: ManagerBase> =
        Box<[Cached<ML, B, M>; SIZE]>;

    type Sizes = Sizes<BITS, SIZE, CachedLayout<ML>>;

    fn bind<B: Block<Self::MainMemoryLayout, M>, M: ManagerBase>(
        space: AllocatedOf<Self, M>,
        block_builder: B::BlockBuilder,
    ) -> BlockCache<Self, B, Self::MainMemoryLayout, M> {
        BlockCache {
            current_block_addr: space.0,
            next_instr_addr: space.1,
            fence_counter: space.2,
            partial_block: PartialBlock::bind(space.3),
            entries: space
                .4
                .into_iter()
                .map(|c| Cached::bind(c))
                .collect::<Vec<_>>()
                .try_into()
                .map_err(|_| "mismatching vector lengths for instruction cache")
                .unwrap(),
            block_builder,
        }
    }

    fn entry<B: Block<Self::MainMemoryLayout, M>, M: ManagerBase>(
        entries: &Self::Entries<B, M>,
        phys_addr: Address,
    ) -> &Cached<ML, B, M> {
        &entries[Self::Sizes::cache_index(phys_addr)]
    }

    fn entry_mut<B: Block<Self::MainMemoryLayout, M>, M: ManagerBase>(
        entries: &mut Self::Entries<B, M>,
        phys_addr: Address,
    ) -> &mut Cached<ML, B, M> {
        &mut entries[Self::Sizes::cache_index(phys_addr)]
    }

    fn entries_reset<B: Block<Self::MainMemoryLayout, M>, M: ManagerReadWrite>(
        entries: &mut Self::Entries<B, M>,
    ) {
        entries.iter_mut().for_each(Cached::reset)
    }

    fn struct_ref<
        'a,
        B: Block<Self::MainMemoryLayout, M>,
        M: ManagerBase,
        F: FnManager<Ref<'a, M>>,
    >(
        cache: &'a BlockCache<Self, B, Self::MainMemoryLayout, M>,
    ) -> AllocatedOf<Self, F::Output> {
        (
            cache.current_block_addr.struct_ref::<F>(),
            cache.next_instr_addr.struct_ref::<F>(),
            cache.fence_counter.struct_ref::<F>(),
            cache.partial_block.struct_ref::<F>(),
            cache
                .entries
                .iter()
                .map(|entry| entry.struct_ref::<F>())
                .collect(),
        )
    }

    fn clone_entries<B: Block<Self::MainMemoryLayout, M> + Clone, M: ManagerClone>(
        entries: &Self::Entries<B, M>,
    ) -> Self::Entries<B, M> {
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
pub struct BlockCache<
    BCL: BlockCacheLayout<MainMemoryLayout = ML>,
    B: Block<ML, M>,
    ML: MainMemoryLayout,
    M: ManagerBase,
> {
    current_block_addr: Cell<Address, M>,
    next_instr_addr: Cell<Address, M>,
    fence_counter: Cell<FenceCounter, M>,
    partial_block: PartialBlock<M>,
    entries: BCL::Entries<B, M>,
    block_builder: B::BlockBuilder,
}

impl<
    BCL: BlockCacheLayout<MainMemoryLayout = ML>,
    B: Block<ML, M>,
    ML: MainMemoryLayout,
    M: ManagerBase,
> BlockCache<BCL, B, ML, M>
{
    /// Bind the block cache to the given allocated state and the given [block builder].
    ///
    /// [block builder]: Block::BlockBuilder
    pub fn bind(space: AllocatedOf<BCL, M>, block_builder: B::BlockBuilder) -> Self {
        BCL::bind(space, block_builder)
    }

    /// Invalidate all entries in the block cache.
    pub fn invalidate(&mut self)
    where
        M: ManagerReadWrite,
    {
        let counter = self.fence_counter.read();
        self.fence_counter.write(counter.next());
        self.reset_to(!0);
        BCL::entry_mut(&mut self.entries, counter.0 as Address).invalidate();
    }

    /// Reset the underlying storage.
    pub fn reset(&mut self)
    where
        M: ManagerReadWrite,
    {
        self.fence_counter.write(FenceCounter::INITIAL);
        self.reset_to(!0);
        BCL::entries_reset(&mut self.entries);
        self.partial_block.reset();
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> AllocatedOf<BCL, F::Output> {
        BCL::struct_ref::<_, _, F>(self)
    }

    /// Push a compressed instruction to the block cache.
    pub fn push_instr_compressed(&mut self, phys_addr: Address, instr: Instruction)
    where
        M: ManagerReadWrite,
    {
        debug_assert_eq!(
            instr.width(),
            InstrWidth::Compressed,
            "expected compressed instruction, found: {instr:?}"
        );

        let next_addr = self.next_instr_addr.read();

        // If the instruction is at the start of the page, we _must_ start a new block,
        // as we cannot allow blocks to cross page boundaries.
        if phys_addr & PAGE_OFFSET_MASK as u64 == 0 || phys_addr != next_addr {
            let previous = BCL::entry_mut(&mut self.entries, self.current_block_addr.read());
            previous.block.complete_block(&mut self.block_builder);

            self.reset_to(phys_addr);
        }

        self.cache_inner::<{ InstrWidth::Compressed as u64 }>(phys_addr, instr);
    }

    /// Push an uncompressed instruction to the block cache.
    pub fn push_instr_uncompressed(&mut self, phys_addr: Address, instr: Instruction)
    where
        M: ManagerReadWrite,
    {
        debug_assert_eq!(
            instr.width(),
            InstrWidth::Uncompressed,
            "expected uncompressed instruction, found: {instr:?}"
        );

        // ensure uncompressed does not cross page boundaries
        const END_OF_PAGE: Address = PAGE_SIZE - 2;
        if phys_addr % PAGE_SIZE == END_OF_PAGE {
            return;
        }

        let next_addr = self.next_instr_addr.read();

        // If the instruction is at the start of the page, we _must_ start a new block,
        // as we cannot allow blocks to cross page boundaries.
        if phys_addr & PAGE_OFFSET_MASK as u64 == 0 || phys_addr != next_addr {
            let previous = BCL::entry_mut(&mut self.entries, self.current_block_addr.read());
            previous.block.complete_block(&mut self.block_builder);

            self.reset_to(phys_addr);
        }

        self.cache_inner::<{ InstrWidth::Uncompressed as u64 }>(phys_addr, instr);
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
    ///
    /// If there is a block at the next address, we will
    /// merge it into the current block if possible. We
    /// ensure it is valid (it is part of the same fence,
    /// on the same page and that the combined block will
    /// not exceed the maximum number of instructions).
    fn cache_inner<const WIDTH: u64>(&mut self, phys_addr: Address, instr: Instruction)
    where
        M: ManagerReadWrite,
    {
        let mut block_addr = self.current_block_addr.read();
        let fence_counter = self.fence_counter.read();

        let mut entry = BCL::entry_mut(&mut self.entries, block_addr);
        let start = entry.address.read();

        let mut len_instr = entry.block.num_instr();

        if start != block_addr || start == phys_addr {
            entry.start_block(block_addr, fence_counter);
            len_instr = 0;
        } else if len_instr == CACHE_INSTR {
            // complete previous block
            entry.block.complete_block(&mut self.block_builder);

            // The current block is full, start a new one
            self.reset_to(phys_addr);
            block_addr = phys_addr;

            entry = BCL::entry_mut(&mut self.entries, block_addr);
            entry.start_block(block_addr, fence_counter);

            len_instr = 0;
        }

        entry.block.push_instr(instr);
        let new_len = len_instr + 1;

        let next_phys_addr = phys_addr + WIDTH;
        self.next_instr_addr.write(next_phys_addr);

        let possible_block = BCL::entry(&self.entries, next_phys_addr);
        let adjacent_block_found = possible_block.address.read() == next_phys_addr
            && possible_block.fence_counter.read() == fence_counter
            && next_phys_addr & PAGE_OFFSET_MASK as u64 != 0
            && possible_block.block.num_instr() + new_len <= CACHE_INSTR;

        if adjacent_block_found {
            let num_instr = possible_block.block.num_instr();
            for i in 0..num_instr {
                // Need to resolve the adjacent block again because we may only keep one reference at a time
                // to `self.entries`.
                let new_block = BCL::entry(&self.entries, next_phys_addr);
                let new_instr = new_block.block.instr()[i].read_stored();
                // Need to resolve the target block again because we may only keep one reference at a time
                // to `self.entries`.
                let current_entry = BCL::entry_mut(&mut self.entries, block_addr);
                current_entry.block.push_instr(new_instr);
            }
            self.next_instr_addr.write(!0);
            self.current_block_addr.write(!0);

            let current_entry = BCL::entry_mut(&mut self.entries, block_addr);
            current_entry.block.complete_block(&mut self.block_builder);
        }
    }

    /// Lookup a block by a physical address.
    ///
    /// If one is found it can then be executed with [`BCall::run_block`].
    ///
    /// *NB* before running any block, you must ensure no partial block
    /// is in progress with [`BlockCache::complete_current_block`].
    pub fn get_block(
        &mut self,
        phys_addr: Address,
    ) -> Option<&mut (impl BCall<ML, M> + ?Sized + '_)>
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
            entry.block.callable()
        } else {
            None
        }
    }

    /// Complete a block that was only partially executed.
    ///
    /// This can happen when `steps + block.len_instr() > steps_max`, in
    /// which case we only executed instructions until `steps == steps_max`.
    pub fn complete_current_block(
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
    pub fn run_block_partial(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        block_addr: Address,
        steps: &mut usize,
        max_steps: usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        // start a new block
        self.partial_block.in_progress.write(true);
        self.partial_block.progress.write(0);
        self.partial_block.phys_addr.write(block_addr);

        self.run_partial_inner(core, steps, max_steps)
    }

    fn run_partial_inner(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        steps: &mut usize,
        max_steps: usize,
    ) -> Result<(), EnvironException>
    where
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

        let range = progress as usize..;
        for instr in entry.block.instr()[range].iter() {
            match run_instr(instr, core) {
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

        if progress as usize == entry.block.num_instr() {
            // We finished the block in exactly the number of steps left
            self.partial_block.reset();
        } else {
            // Remember the progress made through the block, when we later
            // continue executing it
            self.partial_block.progress.write(progress);
        }

        Ok(())
    }

    /// *TEST ONLY* - retrieve the underlying instructions contained in the entry at the given
    /// address.
    #[cfg(test)]
    pub(crate) fn get_block_instr(&mut self, phys_addr: Address) -> Vec<Instruction>
    where
        M: ManagerRead,
    {
        let entry = BCL::entry_mut(&mut self.entries, phys_addr);

        let instr = entry.block.instr();
        instr.iter().map(|cell| cell.read_stored()).collect()
    }
}

impl<
    BCL: BlockCacheLayout<MainMemoryLayout = ML>,
    B: Block<ML, M> + Clone,
    ML: MainMemoryLayout,
    M: ManagerClone,
> Clone for BlockCache<BCL, B, ML, M>
{
    fn clone(&self) -> Self {
        Self {
            current_block_addr: self.current_block_addr.clone(),
            fence_counter: self.fence_counter.clone(),
            next_instr_addr: self.next_instr_addr.clone(),
            partial_block: self.partial_block.clone(),
            entries: BCL::clone_entries(&self.entries),
            block_builder: Default::default(),
        }
    }
}

#[inline(always)]
fn run_instr<ML: MainMemoryLayout, M: ManagerReadWrite>(
    instr: &EnrichedCell<ICallPlaced<ML>, M>,
    core: &mut MachineCoreState<ML, M>,
) -> Result<ProgramCounterUpdate, Exception> {
    let args = instr.read_ref_stored().args();
    let icall = instr.read_derived();

    // SAFETY: This is safe, as the function we are calling is derived directly from the
    // same instruction as the `Args` we are calling with. Therefore `args` will be of the
    // required shape.
    unsafe { icall.run(args, core) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend_test, create_state,
        machine_state::{
            MachineCoreState, MachineCoreStateLayout, MachineState, MachineStateLayout,
            TestCacheLayouts,
            address_translation::PAGE_SIZE,
            block_cache::bcall::{Interpreted, InterpretedBlockBuilder},
            instruction::{
                Instruction, OpCode,
                tagged_instruction::{TaggedArgs, TaggedInstruction, TaggedRegister},
            },
            main_memory::{self, tests::T1K},
            mode::Mode,
            registers::{XRegister, a1, nz, t0, t1},
        },
        state_backend::{CommitmentLayout, owned_backend::Owned},
    };

    pub type TestLayout<ML> = Layout<ML, TEST_CACHE_BITS, TEST_CACHE_SIZE>;

    // writing CACHE_INSTR to the block cache creates new block
    backend_test!(test_writing_full_block_fetchable_uncompressed, F, {
        let mut state = create_state!(BlockCache, TestLayout<T1K>, F, TestLayout<T1K>, Interpreted<T1K, F::Manager>, T1K, || InterpretedBlockBuilder);

        let uncompressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::Sd,
            args: TaggedArgs {
                rs1: t1.into(),
                rs2: t0.into(),
                rd: TaggedRegister::X(XRegister::x1),
                imm: 8,
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let phys_addr = 10;

        for offset in 0..(CACHE_INSTR as u64) {
            state.push_instr_uncompressed(phys_addr + offset * 4, uncompressed);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());
    });

    backend_test!(test_writing_full_block_fetchable_compressed, F, {
        let mut state = create_state!(BlockCache, TestLayout<T1K>, F, TestLayout<T1K>, Interpreted<T1K, F::Manager>, T1K, || InterpretedBlockBuilder);

        let compressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::Li,
            args: TaggedArgs {
                rd: nz::a0.into(),
                imm: 1,
                rs1: nz::ra.into(),
                rs2: nz::ra.into(),
                width: InstrWidth::Compressed,
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let phys_addr = 10;

        for offset in 0..(CACHE_INSTR as u64) {
            state.push_instr_compressed(phys_addr + offset * 2, compressed);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());
    });

    // writing instructions immediately creates block
    backend_test!(test_writing_half_block_fetchable_compressed, F, {
        let mut state = create_state!(BlockCache, TestLayout<T1K>, F, TestLayout<T1K>, Interpreted<T1K, F::Manager>, T1K, || InterpretedBlockBuilder);

        let compressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::Li,
            args: TaggedArgs {
                rd: nz::a0.into(),
                imm: 1,
                rs1: nz::ra.into(),
                rs2: nz::ra.into(),
                width: InstrWidth::Compressed,
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let phys_addr = 10;

        for offset in 0..((CACHE_INSTR / 2) as u64) {
            state.push_instr_compressed(phys_addr + offset * 2, compressed);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR / 2, block.unwrap().num_instr());
    });

    backend_test!(test_writing_two_blocks_fetchable_compressed, F, {
        let mut state = create_state!(BlockCache, TestLayout<T1K>, F, TestLayout<T1K>, Interpreted<T1K, F::Manager>, T1K, || InterpretedBlockBuilder);

        let compressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::Li,
            args: TaggedArgs {
                rd: nz::a0.into(),
                imm: 1,
                rs1: nz::ra.into(),
                rs2: nz::ra.into(),
                width: InstrWidth::Compressed,
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let phys_addr = 10;

        for offset in 0..((CACHE_INSTR * 2) as u64) {
            state.push_instr_compressed(phys_addr + offset * 2, compressed);
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
        let mut state = create_state!(BlockCache, TestLayout<T1K>, F, TestLayout<T1K>, Interpreted<T1K, F::Manager>, T1K, || InterpretedBlockBuilder);

        let compressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::Li,
            args: TaggedArgs {
                rd: nz::a0.into(),
                imm: 1,
                rs1: nz::ra.into(),
                rs2: nz::ra.into(),
                width: InstrWidth::Compressed,
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let phys_addr = PAGE_SIZE - 10;

        for offset in 0..10 {
            state.push_instr_compressed(phys_addr + offset * 2, compressed);
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
        let mut block_state = create_state!(BlockCache, TestLayout<T1K>, F, TestLayout<T1K>, Interpreted<T1K, F::Manager>, T1K, || InterpretedBlockBuilder);

        let addiw = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::Addiw,
            args: TaggedArgs {
                rd: nz::a1.into(),
                rs1: a1.into(),
                imm: 257,
                rs2: TaggedRegister::X(XRegister::x1),
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let block_addr = main_memory::FIRST_ADDRESS;

        for offset in 0..10 {
            block_state.push_instr_uncompressed(block_addr + offset * 4, addiw);
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

    backend_test!(test_concat_blocks_suitable, F, {
        let mut state = create_state!(BlockCache, TestLayout<T1K>, F, TestLayout<T1K>, Interpreted<T1K, F::Manager>, T1K, || InterpretedBlockBuilder);

        let uncompressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::Sd,
            args: TaggedArgs {
                rs1: t1.into(),
                rs2: t0.into(),
                rd: TaggedRegister::X(XRegister::x1),
                imm: 8,
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let phys_addr = 30;
        let preceding_num_instr: u64 = 5;

        for offset in 0..(CACHE_INSTR as u64 - preceding_num_instr) {
            state.push_instr_uncompressed(phys_addr + offset * 4, uncompressed);
        }

        for offset in 0..preceding_num_instr {
            state.push_instr_uncompressed(
                phys_addr - preceding_num_instr * 4 + offset * 4,
                uncompressed,
            );
        }

        let block = state.get_block(phys_addr - 20);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().num_instr());

        let old_block = state.get_block(phys_addr);
        assert!(old_block.is_some());
        assert_eq!(15, old_block.unwrap().num_instr());
    });

    backend_test!(test_concat_blocks_too_big, F, {
        let mut state = create_state!(BlockCache, TestLayout<T1K>, F, TestLayout<T1K>, Interpreted<T1K, F::Manager>, T1K, || InterpretedBlockBuilder);

        let uncompressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::Sd,
            args: TaggedArgs {
                rs1: t1.into(),
                rs2: t0.into(),
                rd: TaggedRegister::X(XRegister::x1),
                imm: 8,
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let phys_addr = 30;
        let preceding_num_instr: u64 = 5;

        for offset in 0..(CACHE_INSTR as u64 - preceding_num_instr + 1) {
            state.push_instr_uncompressed(phys_addr + offset * 4, uncompressed);
        }

        for offset in 0..preceding_num_instr {
            state.push_instr_uncompressed(
                phys_addr - preceding_num_instr * 4 + offset * 4,
                uncompressed,
            );
        }

        let first_block = state.get_block(phys_addr - preceding_num_instr * 4);
        assert!(first_block.is_some());
        assert_eq!(preceding_num_instr, first_block.unwrap().num_instr() as u64);

        let second_block = state.get_block(phys_addr);
        assert!(second_block.is_some());
        assert_eq!(
            CACHE_INSTR - preceding_num_instr as usize + 1,
            second_block.unwrap().num_instr()
        );
    });

    /// Tests that a layout which contains an [`EnrichedCell`] is hashed identically as
    /// a layout which contains a [`Cell`] of the same value.
    #[test]
    fn test_enriched_cell_hashing() {
        let instr = Instruction::DEFAULT;

        let ec_value = (instr, ICall::<T1K, Owned>::from(&instr));
        let ec: EnrichedCell<ICallPlaced<T1K>, Ref<'_, Owned>> = EnrichedCell::bind(&ec_value);
        let ec_hash = <ICallLayout<T1K> as CommitmentLayout>::state_hash(ec).unwrap();

        let c_value = [instr; 1];
        let c: Cell<Instruction, Ref<'_, Owned>> = Cell::bind(&c_value);
        let c_hash = <Atom<Instruction> as CommitmentLayout>::state_hash(c).unwrap();
        assert_eq!(ec_hash, c_hash);
    }

    /// The initialised block cache must not return any blocks. This is especially important for
    /// blocks at address 0 which at one point were accidentally valid but empty which caused loops.
    #[test]
    fn test_init_block() {
        type Layout = super::Layout<T1K, TEST_CACHE_BITS, TEST_CACHE_SIZE>;

        // This test only makes sense if the test cache size isn't 0.
        if TEST_CACHE_SIZE < 1 {
            panic!("Test cache size must be at least 1");
        }

        let check_block = |block: &mut BlockCache<Layout, _, T1K, Owned>| {
            for i in 0..TEST_CACHE_SIZE {
                assert!(block.get_block(i as Address).is_none());
            }
        };

        let populate_block = |block: &mut BlockCache<Layout, _, T1K, Owned>| {
            for i in 0..TEST_CACHE_SIZE {
                block.push_instr_uncompressed(
                    i as Address,
                    Instruction::try_from(TaggedInstruction {
                        opcode: OpCode::Add,
                        args: TaggedArgs {
                            rd: nz::a1.into(),
                            rs1: nz::a1.into(),
                            rs2: nz::a2.into(),
                            ..TaggedArgs::DEFAULT
                        },
                    })
                    .unwrap(),
                );
            }
        };

        let mut block: BlockCache<Layout, Interpreted<T1K, Owned>, T1K, Owned> =
            BlockCache::bind(Owned::allocate::<Layout>(), InterpretedBlockBuilder);

        // The initial block cache should not return any blocks.
        check_block(&mut block);

        populate_block(&mut block);
        block.reset();

        // The reset block cache should not return any blocks.
        check_block(&mut block);

        // We check the invalidation logic multiple times because it works progressively, not in
        // one go.
        for _ in 0..TEST_CACHE_SIZE {
            populate_block(&mut block);
            block.invalidate();

            // The invalidated block cache should not return any blocks.
            check_block(&mut block);
        }
    }

    /// The initialised block cache has an entry for address 0. The block at address 0 happens to
    /// be empty, which causes the step function to loop indefinitely when it runs the block.
    #[test]
    fn test_run_addr_zero() {
        type StateLayout = MachineStateLayout<T1K, TestCacheLayouts>;

        let mut state: MachineState<T1K, TestCacheLayouts, Interpreted<T1K, Owned>, Owned> =
            MachineState::bind(Owned::allocate::<StateLayout>(), InterpretedBlockBuilder);

        // Encoding of ECALL instruction
        const ECALL: u32 = 0b1110011;

        state.core.hart.mode.write(Mode::Machine);
        state.core.hart.pc.write(0);
        state.core.main_memory.write(0, ECALL).unwrap();

        let result = state.step();
        assert_eq!(result, Err(EnvironException::EnvCallFromMMode));
    }
}
