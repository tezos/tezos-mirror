// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Switching of execution strategy for blocks.

mod dispatch;

use dispatch::DispatchCompiler;
use dispatch::DispatchTarget;
pub use dispatch::OutlineCompiler;

use super::CACHE_INSTR;
use super::ICallPlaced;
use super::run_instr;
use crate::default::ConstDefault;
use crate::jit::state_access::JitStateAccess;
use crate::machine_state::MachineCoreState;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::instruction::Instruction;
use crate::machine_state::memory::Address;
use crate::machine_state::memory::MemoryConfig;
use crate::state::NewState;
use crate::state_backend::AllocatedOf;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::state_backend::EnrichedCell;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerWrite;
use crate::state_backend::Ref;
use crate::traps::EnvironException;
use crate::traps::Exception;

/// State Layout for Blocks
pub type BlockLayout = (Atom<u8>, [Atom<Instruction>; CACHE_INSTR]);

/// Functionality required to construct & execute blocks.
///
/// A block is a sequence of at least one instruction, which may be executed sequentially.
/// Blocks will never contain more than [`CACHE_INSTR`] instructions.
pub trait Block<MC: MemoryConfig, M: ManagerBase>: NewState<M> {
    /// Block construction may require additional state not kept in storage,
    /// this is then passed as a parameter to [`Block::run_block`].
    ///
    /// `Sized` bound is required to ensure any reference to `BlockBuilder` will be thin -
    /// see [`DispatchFn`].
    type BlockBuilder: Default + Sized;

    /// Bind the block to the given allocated state.
    fn bind(allocated: AllocatedOf<BlockLayout, M>) -> Self
    where
        M::ManagerRoot: ManagerReadWrite;

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> AllocatedOf<BlockLayout, F::Output>;

    /// Ready a block for construction.
    ///
    /// Previous instructions are removed.
    fn start_block(&mut self)
    where
        M: ManagerWrite;

    /// Push an instruction to the block.
    fn push_instr(&mut self, instr: Instruction)
    where
        M: ManagerReadWrite;

    fn num_instr(&self) -> usize
    where
        M: ManagerRead;

    /// Invalidate a block, meaning it should no longer be run.
    fn invalidate(&mut self)
    where
        M: ManagerWrite;

    /// Reset a block to the default state, it should no longer be run.
    fn reset(&mut self)
    where
        M: ManagerReadWrite;

    /// Returns the underlying slice of instructions stored in the block.
    fn instr(&self) -> &[EnrichedCell<ICallPlaced<MC, M>, M>]
    where
        M: ManagerRead;

    /// Run a block against the machine state.
    ///
    /// When calling this function, there must be no partial block in progress. To ensure
    /// this, you must always run [`BlockCache::complete_current_block`] prior to fetching
    /// and running a new block.
    ///
    /// There _must_ also be sufficient steps remaining, to execute the block in full.
    ///
    /// # Safety
    ///
    /// The `block_builder` must be the same as the block builder given to the `compile` call that
    /// (may) have natively compiled this block to machine code.
    ///
    /// This ensures that the builder in question is guaranteed to be alive, for at least as long
    /// as this block may be run.
    ///
    /// [`BlockCache::complete_current_block`]: super::BlockCache::complete_current_block
    unsafe fn run_block(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        instr_pc: Address,
        steps: &mut usize,
        block_builder: &mut Self::BlockBuilder,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite;
}

/// Interpreted blocks are built automatically, and require no additional context.
#[derive(Debug, Default)]
pub struct InterpretedBlockBuilder;

/// Blocks that are executed via intepreting the individual instructions.
///
/// Interpreted blocks use the [`EnrichedCell`] mechanism, in order to dispatch
/// opcode to function statically during block construction. This saves time over
/// dispatching on every 'instruction run'. See [`ICall`] for more information.
///
/// [`ICall`]: super::ICall
pub struct Interpreted<MC: MemoryConfig, M: ManagerBase> {
    instr: [EnrichedCell<ICallPlaced<MC, M>, M>; CACHE_INSTR],
    len_instr: Cell<u8, M>,
}

impl<MC: MemoryConfig, M: ManagerBase> NewState<M> for Interpreted<MC, M> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        Self {
            len_instr: Cell::new(manager),
            instr: NewState::new(manager),
        }
    }
}

impl<MC: MemoryConfig, M: ManagerBase> Block<MC, M> for Interpreted<MC, M> {
    type BlockBuilder = InterpretedBlockBuilder;

    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.len_instr.read() as usize
    }

    #[inline]
    fn instr(&self) -> &[EnrichedCell<ICallPlaced<MC, M>, M>]
    where
        M: ManagerRead,
    {
        &self.instr[..self.num_instr()]
    }

    fn invalidate(&mut self)
    where
        M: ManagerWrite,
    {
        self.len_instr.write(0);
    }

    fn push_instr(&mut self, instr: Instruction)
    where
        M: ManagerReadWrite,
    {
        let len = self.len_instr.read();
        self.instr[len as usize].write(instr);
        self.len_instr.write(len + 1);
    }

    fn reset(&mut self)
    where
        M: ManagerReadWrite,
    {
        self.len_instr.write(0);
        self.instr
            .iter_mut()
            .for_each(|lc| lc.write(Instruction::DEFAULT));
    }

    fn start_block(&mut self)
    where
        M: ManagerWrite,
    {
        self.len_instr.write(0);
    }

    fn bind(space: AllocatedOf<BlockLayout, M>) -> Self
    where
        M::ManagerRoot: ManagerReadWrite,
    {
        Self {
            len_instr: space.0,
            instr: space.1.map(EnrichedCell::bind),
        }
    }

    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> AllocatedOf<BlockLayout, F::Output> {
        (
            self.len_instr.struct_ref::<F>(),
            self.instr.each_ref().map(|entry| entry.struct_ref::<F>()),
        )
    }

    /// # SAFETY
    ///
    /// This function is always safe to call.
    #[inline(always)]
    unsafe fn run_block(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        mut instr_pc: Address,
        steps: &mut usize,
        _block_builder: &mut Self::BlockBuilder,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        if let Err(e) = run_block_inner(self.instr(), core, &mut instr_pc, steps) {
            core.handle_step_result(instr_pc, Err(e))?;
            // If we succesfully handled an error, need to increment steps one more.
            *steps += 1;
        }

        Ok(())
    }
}

impl<MC: MemoryConfig, M: ManagerClone> Clone for Interpreted<MC, M> {
    fn clone(&self) -> Self {
        Self {
            len_instr: self.len_instr.clone(),
            instr: self.instr.clone(),
        }
    }
}

/// The function signature for dispatching a block run.
///
/// Internally, this may be interpreted, just-in-time compiled, or do
/// additional work over just execution.
///
/// The first and last parameters must be thin-references, for ABI-compatability reasons.
#[expect(
    improper_ctypes_definitions,
    reason = "The receiving functions know the layout of the referenced types"
)]
pub type DispatchFn<D, MC, M> = unsafe extern "C" fn(
    &mut Jitted<D, MC, M>,
    &mut MachineCoreState<MC, M>,
    Address,
    &mut usize,
    &mut Result<(), EnvironException>,
    &mut <Jitted<D, MC, M> as Block<MC, M>>::BlockBuilder,
);

/// Blocks that are compiled to native code for execution, when possible.
///
/// Not all instructions are currently supported, when a block contains
/// unsupported instructions, a fallback to [`Interpreted`] mode occurs.
///
/// Blocks are compiled upon calling [`Block::run_block`], in a *stop the world* fashion.
pub struct Jitted<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess> {
    fallback: Interpreted<MC, M>,
    dispatch: DispatchTarget<D, MC, M>,
}

impl<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess> Jitted<D, MC, M> {
    /// The default initial dispatcher for inline jit.
    ///
    /// This will run the block in interpreted mode by default, but will attempt to JIT-compile
    /// the block.
    ///
    /// # SAFETY
    ///
    /// The `block_builder` must be the same every time this function is called.
    ///
    /// This ensures that the builder in question is guaranteed to be alive, for at least as long
    /// as this block may be run via [`Block::run_block`].
    unsafe extern "C" fn run_block_interpreted(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        instr_pc: Address,
        steps: &mut usize,
        result: &mut Result<(), EnvironException>,
        block_builder: &mut <Self as Block<MC, M>>::BlockBuilder,
    ) {
        if !block_builder.0.should_compile(&mut self.dispatch) {
            unsafe { self.run_block_not_compiled(core, instr_pc, steps, result, block_builder) }
            return;
        }

        // trigger JIT compilation
        let instr = self
            .fallback
            .instr
            .iter()
            .take(<Self as Block<MC, M>>::num_instr(self))
            .map(|i| i.read_stored())
            .collect::<Vec<_>>();

        let fun = block_builder.0.compile(&mut self.dispatch, instr);

        // Safety: the block builder passed to this function is always the same for the
        // lifetime of the block
        unsafe { (fun)(self, core, instr_pc, steps, result, block_builder) }
    }

    /// Run a block where JIT-compilation has been attempted, but failed for any reason.
    ///
    /// # SAFETY
    ///
    /// The `block_builder` must be the same every time this function is called.
    ///
    /// This ensures that the builder in question is guaranteed to be alive, for at least as long
    /// as this block may be run via [`Block::run_block`].
    unsafe extern "C" fn run_block_not_compiled(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        instr_pc: Address,
        steps: &mut usize,
        result: &mut Result<(), EnvironException>,
        block_builder: &mut <Self as Block<MC, M>>::BlockBuilder,
    ) {
        *result = unsafe {
            // Safety: this function is always safe to call
            self.fallback
                .run_block(core, instr_pc, steps, &mut block_builder.1)
        };
    }
}

impl<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess> NewState<M>
    for Jitted<D, MC, M>
{
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        Self {
            fallback: Interpreted::new(manager),
            dispatch: DispatchTarget::default(),
        }
    }
}

impl<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess> Block<MC, M>
    for Jitted<D, MC, M>
{
    type BlockBuilder = (D, InterpretedBlockBuilder);

    fn start_block(&mut self)
    where
        M: ManagerWrite,
    {
        self.dispatch.reset();
        self.fallback.start_block()
    }

    fn invalidate(&mut self)
    where
        M: ManagerWrite,
    {
        self.dispatch.reset();
        self.fallback.invalidate()
    }

    fn reset(&mut self)
    where
        M: ManagerReadWrite,
    {
        self.dispatch.reset();
        self.fallback.reset()
    }

    fn push_instr(&mut self, instr: Instruction)
    where
        M: ManagerReadWrite,
    {
        self.dispatch.reset();
        self.fallback.push_instr(instr)
    }

    fn instr(&self) -> &[EnrichedCell<ICallPlaced<MC, M>, M>]
    where
        M: ManagerRead,
    {
        self.fallback.instr()
    }

    fn bind(allocated: AllocatedOf<BlockLayout, M>) -> Self {
        Self {
            fallback: Interpreted::bind(allocated),
            dispatch: DispatchTarget::default(),
        }
    }

    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> AllocatedOf<BlockLayout, F::Output> {
        self.fallback.struct_ref::<F>()
    }

    /// Run a block, using the currently selected dispatch mechanism
    ///
    /// # SAFETY
    ///
    /// The `block_builder` must be the same every time this function is called.
    ///
    /// This ensures that the builder in question is guaranteed to be alive, for at least as long
    /// as this block may be run via [`Block::run_block`].
    unsafe fn run_block(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        instr_pc: Address,
        steps: &mut usize,
        block_builder: &mut Self::BlockBuilder,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        let mut result = Ok(());

        let fun = self.dispatch.get();

        // Safety: the block builder is always the same instance, guarantee-ing that any
        // jit-compiled function is still alive.
        unsafe { (fun)(self, core, instr_pc, steps, &mut result, block_builder) };

        result
    }

    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.fallback.num_instr()
    }
}

impl<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess + ManagerClone> Clone
    for Jitted<D, MC, M>
{
    fn clone(&self) -> Self {
        Self {
            fallback: self.fallback.clone(),
            dispatch: DispatchTarget::default(),
        }
    }
}

fn run_block_inner<MC: MemoryConfig, M: ManagerReadWrite>(
    instr: &[EnrichedCell<ICallPlaced<MC, M>, M>],
    core: &mut MachineCoreState<MC, M>,
    instr_pc: &mut Address,
    steps: &mut usize,
) -> Result<(), Exception> {
    for instr in instr.iter() {
        match run_instr(instr, core) {
            Ok(ProgramCounterUpdate::Next(width)) => {
                *instr_pc += width as u64;
                core.hart.pc.write(*instr_pc);
                *steps += 1;
            }
            Ok(ProgramCounterUpdate::Set(instr_pc)) => {
                // Setting the instr_pc implies execution continuing
                // elsewhere - and no longer within the current block.
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
