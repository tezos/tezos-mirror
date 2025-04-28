// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! The block cache maps certain physical addresses to contiguous sequences of instructions, or
//! 'blocks'. These blocks will never cross page boundaries.
//!
//! Blocks are sets of instructions that are often - but not always - executed immediately after each
//! other. The main exceptions to this are:
//! - instructions may trigger exceptions (e.g. `BadMemoryAccess` when interacting with memory)
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
//! [`Interpreted`]: block::Interpreted

pub mod block;
pub mod metrics;

use std::marker::PhantomData;

use block::Block;

use super::MachineCoreState;
use super::ProgramCounterUpdate;
use super::instruction::Instruction;
use super::instruction::RunInstr;
use super::memory::Address;
use super::memory::MemoryConfig;
use crate::cache_utils::FenceCounter;
use crate::cache_utils::Sizes;
use crate::machine_state::instruction::Args;
use crate::machine_state::memory::OFFSET_MASK;
use crate::machine_state::memory::PAGE_SIZE;
use crate::parser::instruction::InstrWidth;
use crate::state::NewState;
use crate::state_backend;
use crate::state_backend::AllocatedOf;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::state_backend::EnrichedCell;
use crate::state_backend::EnrichedValue;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerSerialise;
use crate::state_backend::ManagerWrite;
use crate::state_backend::Ref;
use crate::state_backend::proof_backend;
use crate::storage::Hash;
use crate::storage::HashError;
use crate::traps::EnvironException;
use crate::traps::Exception;

/// The maximum number of instructions that may be contained in a block.
pub const CACHE_INSTR: usize = 20;

/// Bindings for deriving an [`ICall`] from an [`Instruction`] via the [`EnrichedCell`] mechanism.
pub struct ICallPlaced<MC: MemoryConfig, M: ManagerBase> {
    _pd0: PhantomData<MC>,
    _pd1: PhantomData<M>,
}

impl<MC: MemoryConfig, M: ManagerBase> EnrichedValue for ICallPlaced<MC, M> {
    type E = Instruction;

    type D = ICall<MC, M::ManagerRoot>;
}

/// A function derived from an [OpCode] that can be directly run over the [MachineCoreState].
///
/// This allows static dispatch of this function during block construction,
/// rather than for each instruction, during each block execution.
///
/// [OpCode]: super::instruction::OpCode
pub struct ICall<MC: MemoryConfig, M: ManagerBase> {
    run_instr: RunInstr<MC, M>,
}

impl<MC: MemoryConfig, M: ManagerBase> Clone for ICall<MC, M> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<MC: MemoryConfig, M: ManagerBase> Copy for ICall<MC, M> {}

impl<MC: MemoryConfig, M: ManagerReadWrite> ICall<MC, M> {
    // SAFETY: This function must be called with an `Args` belonging to the same `OpCode` as
    // the one used to dispatch this function.
    #[inline(always)]
    unsafe fn run(
        &self,
        args: &Args,
        core: &mut MachineCoreState<MC, M>,
    ) -> Result<ProgramCounterUpdate<Address>, Exception> {
        unsafe { (self.run_instr)(args, core) }
    }
}

impl<'a, MC: MemoryConfig, M: ManagerReadWrite> From<&'a Instruction> for ICall<MC, M> {
    fn from(value: &'a Instruction) -> Self {
        let run_instr = value.opcode.to_run::<MC, M>();
        Self { run_instr }
    }
}

/// Layout for an address cell
pub struct AddressCellLayout;

impl state_backend::Layout for AddressCellLayout {
    type Allocated<M: state_backend::ManagerBase> = Cell<Address, M>;
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

    fn partial_state_hash(
        state: state_backend::RefVerifierAlloc<Self>,
        proof: state_backend::ProofTree,
    ) -> Result<Hash, state_backend::PartialHashError> {
        <Atom<Address> as state_backend::ProofLayout>::partial_state_hash(state, proof)
    }
}

/// The layout of block cache entries, see [`Cached`] for more information.
pub type CachedLayout = (
    AddressCellLayout,
    Atom<FenceCounter>,
    Atom<u8>,
    [Atom<Instruction>; CACHE_INSTR],
);

/// Block cache entry.
///
/// Contains the physical address & fence counter for validity checks, the
/// underlying [`Block`] state.
pub struct Cached<MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase> {
    block: B,
    address: Cell<Address, M>,
    fence_counter: Cell<FenceCounter, M>,
    _pd: PhantomData<MC>,
}

impl<MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase> Cached<MC, B, M> {
    fn bind(space: AllocatedOf<CachedLayout, M>) -> Self
    where
        M::ManagerRoot: ManagerReadWrite,
    {
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

    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> AllocatedOf<CachedLayout, F::Output> {
        let (len_instr, instr) = self.block.struct_ref::<'a, F>();
        (
            self.address.struct_ref::<F>(),
            self.fence_counter.struct_ref::<F>(),
            len_instr,
            instr,
        )
    }
}

impl<MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase> NewState<M> for Cached<MC, B, M> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        Self {
            address: Cell::new_with(manager, !0),
            block: B::new(manager),
            fence_counter: Cell::new(manager),
            _pd: PhantomData,
        }
    }
}

impl<MC: MemoryConfig, B: Block<MC, M> + Clone, M: ManagerClone> Clone for Cached<MC, B, M> {
    fn clone(&self) -> Self {
        Self {
            address: self.address.clone(),
            block: self.block.clone(),
            fence_counter: self.fence_counter.clone(),
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
/// - a jump or branch occurs then the partial block is reset, and execution will continue with a
///   potentially different block.
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

    /// Run a block against the machine state.
    ///
    /// When calling this function, there must be no partial block in progress. To ensure
    /// this, you must always run [`BlockCache::complete_current_block`] prior to fetching
    /// and running a new block.
    #[cold]
    fn run_block_partial<B: Block<MC, M>, MC: MemoryConfig>(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        steps: &mut usize,
        max_steps: usize,
        entry: &mut Cached<MC, B, M>,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        // start a new block
        self.in_progress.write(true);
        self.progress.write(0);
        self.phys_addr.write(entry.address.read());

        self.run_partial_inner(core, steps, max_steps, entry)
    }

    fn run_partial_inner<B: Block<MC, M>, MC: MemoryConfig>(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        steps: &mut usize,
        max_steps: usize,
        entry: &mut Cached<MC, B, M>,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        // Protect against partial blocks being executed when
        // no steps are remaining
        if *steps >= max_steps {
            return Ok(());
        }

        let mut progress = self.progress.read();
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
                    self.reset();
                    return Ok(());
                }
                Err(e) => {
                    self.reset();
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
            self.reset();
        } else {
            // Remember the progress made through the block, when we later
            // continue executing it
            self.progress.write(progress);
        }

        Ok(())
    }
}

impl<M: ManagerBase> NewState<M> for PartialBlock<M> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        Self {
            phys_addr: Cell::new(manager),
            in_progress: Cell::new(manager),
            progress: Cell::new(manager),
        }
    }
}

/// Trait for capturing the different possible layouts of the instruction cache (i.e.
/// controlling the number of cache entries present).
pub trait BlockCacheLayout: state_backend::CommitmentLayout + state_backend::ProofLayout {
    type Entries<MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase>: NewState<M>;

    type Sizes;

    fn bind<MC, B, M>(
        space: state_backend::AllocatedOf<Self, M>,
        block_builder: B::BlockBuilder,
    ) -> BlockCache<Self, B, MC, M>
    where
        Self: Sized,
        M: state_backend::ManagerBase,
        M::ManagerRoot: ManagerReadWrite,
        MC: MemoryConfig,
        B: Block<MC, M>;

    fn entry<MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase>(
        entries: &Self::Entries<MC, B, M>,
        phys_addr: Address,
    ) -> &Cached<MC, B, M>;

    fn entry_mut<MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase>(
        entries: &mut Self::Entries<MC, B, M>,
        phys_addr: Address,
    ) -> &mut Cached<MC, B, M>;

    fn entries_reset<MC: MemoryConfig, B: Block<MC, M>, M: ManagerReadWrite>(
        entries: &mut Self::Entries<MC, B, M>,
    );

    fn struct_ref<'a, MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase, F: FnManager<Ref<'a, M>>>(
        cache: &'a BlockCache<Self, B, MC, M>,
    ) -> AllocatedOf<Self, F::Output>
    where
        Self: Sized;

    fn clone_entries<MC: MemoryConfig, B: Block<MC, M> + Clone, M: ManagerClone>(
        entries: &Self::Entries<MC, B, M>,
    ) -> Self::Entries<MC, B, M>;
}

/// The layout of the block cache.
pub type Layout<const BITS: usize, const SIZE: usize> = (
    AddressCellLayout,
    AddressCellLayout,
    Atom<FenceCounter>,
    PartialBlockLayout,
    Sizes<BITS, SIZE, CachedLayout>,
);

impl<const BITS: usize, const SIZE: usize> BlockCacheLayout for Layout<BITS, SIZE> {
    type Entries<MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase> = Box<[Cached<MC, B, M>; SIZE]>;

    type Sizes = Sizes<BITS, SIZE, CachedLayout>;

    fn bind<MC, B, M>(
        space: state_backend::AllocatedOf<Self, M>,
        block_builder: B::BlockBuilder,
    ) -> BlockCache<Self, B, MC, M>
    where
        Self: Sized,
        M: state_backend::ManagerBase,
        M::ManagerRoot: ManagerReadWrite,
        MC: MemoryConfig,
        B: Block<MC, M>,
    {
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

    fn entry<MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase>(
        entries: &Self::Entries<MC, B, M>,
        phys_addr: Address,
    ) -> &Cached<MC, B, M> {
        &entries[Self::Sizes::cache_index(phys_addr)]
    }

    fn entry_mut<MC: MemoryConfig, B: Block<MC, M>, M: ManagerBase>(
        entries: &mut Self::Entries<MC, B, M>,
        phys_addr: Address,
    ) -> &mut Cached<MC, B, M> {
        &mut entries[Self::Sizes::cache_index(phys_addr)]
    }

    fn entries_reset<MC: MemoryConfig, B: Block<MC, M>, M: ManagerReadWrite>(
        entries: &mut Self::Entries<MC, B, M>,
    ) {
        entries.iter_mut().for_each(Cached::reset)
    }

    fn struct_ref<
        'a,
        MC: MemoryConfig,
        B: Block<MC, M>,
        M: ManagerBase,
        F: FnManager<Ref<'a, M>>,
    >(
        cache: &'a BlockCache<Self, B, MC, M>,
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

    fn clone_entries<MC: MemoryConfig, B: Block<MC, M> + Clone, M: ManagerClone>(
        entries: &Self::Entries<MC, B, M>,
    ) -> Self::Entries<MC, B, M> {
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
pub struct BlockCache<BCL: BlockCacheLayout, B: Block<MC, M>, MC: MemoryConfig, M: ManagerBase> {
    current_block_addr: Cell<Address, M>,
    next_instr_addr: Cell<Address, M>,
    fence_counter: Cell<FenceCounter, M>,
    partial_block: PartialBlock<M>,
    entries: BCL::Entries<MC, B, M>,
    /// The block builder is the mechanism used to construct blocks for calling in a
    /// (potentially) more efficient manner. For example - by JIT compiling them.
    pub block_builder: B::BlockBuilder,
}

impl<BCL: BlockCacheLayout, B: Block<MC, M>, MC: MemoryConfig, M: ManagerBase>
    BlockCache<BCL, B, MC, M>
{
    /// Allocate a new block cache.
    pub fn new(manager: &mut M, block_builder: B::BlockBuilder) -> Self
    where
        M: ManagerAlloc,
    {
        Self {
            current_block_addr: Cell::new_with(manager, !0),
            next_instr_addr: Cell::new_with(manager, !0),
            fence_counter: Cell::new(manager),
            partial_block: PartialBlock::new(manager),
            entries: NewState::new(manager),
            block_builder,
        }
    }

    /// Bind the block cache to the given allocated state and the given [block builder].
    ///
    /// [block builder]: Block::BlockBuilder
    pub fn bind(space: AllocatedOf<BCL, M>, block_builder: B::BlockBuilder) -> Self
    where
        M::ManagerRoot: ManagerReadWrite,
    {
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
        BCL::struct_ref::<_, _, _, F>(self)
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
        if phys_addr & OFFSET_MASK == 0 || phys_addr != next_addr {
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
        const END_OF_PAGE: Address = PAGE_SIZE.get() - 2;
        if phys_addr % PAGE_SIZE.get() == END_OF_PAGE {
            return;
        }

        let next_addr = self.next_instr_addr.read();

        // If the instruction is at the start of the page, we _must_ start a new block,
        // as we cannot allow blocks to cross page boundaries.
        if phys_addr & OFFSET_MASK == 0 || phys_addr != next_addr {
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
            && next_phys_addr & OFFSET_MASK != 0
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
        }
    }

    /// Lookup a block by a physical address.
    ///
    /// If one is found it can then be executed with [`BlockCall::run_block`].
    ///
    /// *NB* before running any block, you must ensure no partial block
    /// is in progress with [`BlockCache::complete_current_block`].
    #[inline(always)]
    pub fn get_block(&mut self, phys_addr: Address) -> Option<BlockCall<'_, B, MC, M>>
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
            && entry.block.num_instr() > 0
        {
            Some(BlockCall {
                entry,
                builder: &mut self.block_builder,
                partial: &mut self.partial_block,
            })
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
        core: &mut MachineCoreState<MC, M>,
        steps: &mut usize,
        max_steps: usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        if !self.partial_block.in_progress.read() {
            return Ok(());
        }

        let entry = BCL::entry_mut(&mut self.entries, self.partial_block.phys_addr.read());

        self.partial_block
            .run_partial_inner(core, steps, max_steps, entry)
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

impl<BCL: BlockCacheLayout, B: Block<MC, M> + Clone, MC: MemoryConfig, M: ManagerClone> Clone
    for BlockCache<BCL, B, MC, M>
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

/// A block that is available to be run.
///
/// If there are sufficiently many steps remaining, the entire block is executed in one go.
/// Otherwise, it will fall back to partial evaluation.
///
/// As a result, before starting to run blocks from the block cache, you must first ensure that
/// any left-over partially-run block is cleared up with [`BlockCache::complete_current_block`].
pub struct BlockCall<'a, B: Block<MC, M>, MC: MemoryConfig, M: ManagerBase> {
    entry: &'a mut Cached<MC, B, M>,
    /// # Safety
    ///
    /// The same block builder must always be passed through to `run_block`.
    builder: &'a mut B::BlockBuilder,
    partial: &'a mut PartialBlock<M>,
}

impl<B: Block<MC, M>, MC: MemoryConfig, M: ManagerReadWrite> BlockCall<'_, B, MC, M> {
    /// Run a block, either fully or partially, depending on the number of steps remaining.
    #[inline(always)]
    pub fn run_block(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        instr_pc: Address,
        steps: &mut usize,
        max_steps: usize,
    ) -> Result<(), EnvironException> {
        if *steps + self.entry.block.num_instr() <= max_steps {
            // Safety: the same block builder is passed through every time.
            unsafe {
                self.entry
                    .block
                    .run_block(core, instr_pc, steps, self.builder)
            }
        } else {
            self.partial
                .run_block_partial(core, steps, max_steps, self.entry)
        }
    }
}

#[inline(always)]
fn run_instr<MC: MemoryConfig, M: ManagerReadWrite>(
    instr: &EnrichedCell<ICallPlaced<MC, M>, M>,
    core: &mut MachineCoreState<MC, M>,
) -> Result<ProgramCounterUpdate<Address>, Exception> {
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
    use crate::backend_test;
    use crate::default::ConstDefault;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineState;
    use crate::machine_state::TestCacheLayouts;
    use crate::machine_state::block_cache::block::Interpreted;
    use crate::machine_state::block_cache::block::InterpretedBlockBuilder;
    use crate::machine_state::instruction::Instruction;
    use crate::machine_state::instruction::OpCode;
    use crate::machine_state::instruction::tagged_instruction::TaggedArgs;
    use crate::machine_state::instruction::tagged_instruction::TaggedInstruction;
    use crate::machine_state::instruction::tagged_instruction::TaggedRegister;
    use crate::machine_state::memory;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::XRegister;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::nz;
    use crate::machine_state::registers::t0;
    use crate::machine_state::registers::t1;
    use crate::state_backend::owned_backend::Owned;

    pub type TestLayout = Layout<TEST_CACHE_BITS, TEST_CACHE_SIZE>;

    // writing CACHE_INSTR to the block cache creates new block
    backend_test!(test_writing_full_block_fetchable_uncompressed, F, {
        let mut state = BlockCache::<TestLayout, Interpreted<M4K, _>, _, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

        let uncompressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::X64Store,
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
        assert_eq!(CACHE_INSTR, block.unwrap().entry.block.num_instr());
    });

    backend_test!(test_writing_full_block_fetchable_compressed, F, {
        let mut state = BlockCache::<TestLayout, Interpreted<M4K, _>, _, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

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
        assert_eq!(CACHE_INSTR, block.unwrap().entry.block.num_instr());
    });

    // writing instructions immediately creates block
    backend_test!(test_writing_half_block_fetchable_compressed, F, {
        let mut state = BlockCache::<TestLayout, Interpreted<M4K, _>, _, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

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
        assert_eq!(CACHE_INSTR / 2, block.unwrap().entry.block.num_instr());
    });

    backend_test!(test_writing_two_blocks_fetchable_compressed, F, {
        let mut state = BlockCache::<TestLayout, Interpreted<M4K, _>, _, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

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
        assert_eq!(CACHE_INSTR, block.unwrap().entry.block.num_instr());

        let block = state.get_block(phys_addr + 2 * CACHE_INSTR as u64);
        assert!(block.is_some());
        assert_eq!(CACHE_INSTR, block.unwrap().entry.block.num_instr());
    });

    // writing across pages offset two blocks next to each other
    backend_test!(test_crossing_page_exactly_creates_new_block, F, {
        let mut state = BlockCache::<TestLayout, Interpreted<M4K, _>, _, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

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

        let phys_addr = PAGE_SIZE.get() - 10;

        for offset in 0..10 {
            state.push_instr_compressed(phys_addr + offset * 2, compressed);
        }

        let block = state.get_block(phys_addr);
        assert!(block.is_some());
        assert_eq!(5, block.unwrap().entry.block.num_instr());

        let block = state.get_block(phys_addr + 10);
        assert!(block.is_some());
        assert_eq!(5, block.unwrap().entry.block.num_instr());
    });

    backend_test!(test_partial_block_executes, F, {
        let mut manager = F::manager();
        let mut core_state = MachineCoreState::<M4K, _>::new(&mut manager);
        let mut block_state = BlockCache::<TestLayout, Interpreted<M4K, _>, _, _>::new(
            &mut manager,
            InterpretedBlockBuilder,
        );

        let addiw = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::AddWordImmediate,
            args: TaggedArgs {
                rd: nz::a1.into(),
                rs1: a1.into(),
                imm: 257,
                rs2: TaggedRegister::X(XRegister::x1),
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let block_addr = memory::FIRST_ADDRESS;

        for offset in 0..10 {
            block_state.push_instr_uncompressed(block_addr + offset * 4, addiw);
        }

        core_state.hart.pc.write(block_addr);

        // Execute the first 5 instructions
        let mut steps = 0;
        let block = block_state.get_block(block_addr).unwrap();
        block
            .partial
            .run_block_partial(&mut core_state, &mut steps, 5, block.entry)
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
        let mut state = BlockCache::<TestLayout, Interpreted<M4K, _>, _, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

        let uncompressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::X64Store,
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
        assert_eq!(CACHE_INSTR, block.unwrap().entry.block.num_instr());

        let old_block = state.get_block(phys_addr);
        assert!(old_block.is_some());
        assert_eq!(15, old_block.unwrap().entry.block.num_instr());
    });

    backend_test!(test_concat_blocks_too_big, F, {
        let mut state = BlockCache::<TestLayout, Interpreted<M4K, _>, _, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

        let uncompressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::X64Store,
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
        assert_eq!(
            preceding_num_instr,
            first_block.unwrap().entry.block.num_instr() as u64
        );

        let second_block = state.get_block(phys_addr);
        assert!(second_block.is_some());
        assert_eq!(
            CACHE_INSTR - preceding_num_instr as usize + 1,
            second_block.unwrap().entry.block.num_instr()
        );
    });

    /// The initialised block cache must not return any blocks. This is especially important for
    /// blocks at address 0 which at one point were accidentally valid but empty which caused loops.
    #[test]
    fn test_init_block() {
        type Layout = super::Layout<TEST_CACHE_BITS, TEST_CACHE_SIZE>;

        // This test only makes sense if the test cache size isn't 0.
        if TEST_CACHE_SIZE < 1 {
            panic!("Test cache size must be at least 1");
        }

        let check_block = |block: &mut BlockCache<Layout, _, M4K, Owned>| {
            for i in 0..TEST_CACHE_SIZE {
                assert!(block.get_block(i as Address).is_none());
            }
        };

        let populate_block = |block: &mut BlockCache<Layout, _, M4K, Owned>| {
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

        let mut block: BlockCache<Layout, Interpreted<M4K, Owned>, M4K, Owned> =
            BlockCache::new(&mut Owned, InterpretedBlockBuilder);

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
        let mut state: MachineState<M4K, TestCacheLayouts, Interpreted<M4K, Owned>, Owned> =
            MachineState::new(&mut Owned, InterpretedBlockBuilder);

        // Encoding of ECALL instruction
        const ECALL: u32 = 0b1110011;

        state.core.hart.pc.write(0);
        state
            .core
            .main_memory
            .write_instruction_unchecked(0, ECALL)
            .unwrap();

        let result = state.step();
        assert_eq!(result, Err(EnvironException::EnvCall));
    }

    /// The initialised block cache has an entry for address 0. The block at address 0 happens to
    /// be empty, which causes the step function to loop indefinitely when it runs the block.
    #[test]
    fn test_get_empty_block_fails() {
        type Layout = super::Layout<TEST_CACHE_BITS, TEST_CACHE_SIZE>;

        let mut block_cache: BlockCache<Layout, Interpreted<M4K, Owned>, M4K, Owned> =
            BlockCache::new(&mut Owned, InterpretedBlockBuilder);

        // Fetching empty block fails
        assert!(block_cache.get_block(0).is_none());

        block_cache.push_instr_compressed(0, Instruction::new_nop(InstrWidth::Compressed));

        // Fetching non-empty block succeeds
        assert!(block_cache.get_block(0).is_some());
    }
}
