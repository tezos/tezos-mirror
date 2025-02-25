// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Switching of execution strategy for blocks.
//!
//! Currently just for interperation only, but will expand to cover JIT.

use super::{CACHE_INSTR, ICallPlaced, run_instr};
use crate::{
    default::ConstDefault,
    jit::{JCall, JIT, state_access::JitStateAccess},
    machine_state::{
        MachineCoreState, ProgramCounterUpdate,
        instruction::Instruction,
        memory::{Address, MemoryConfig},
    },
    state_backend::{
        AllocatedOf, Atom, Cell, EnrichedCell, FnManager, ManagerBase, ManagerClone, ManagerRead,
        ManagerReadWrite, ManagerWrite, Ref,
    },
    traps::{EnvironException, Exception},
};

/// A block derived from a sequence of [`Instruction`] that can be directly run
/// over the [`MachineCoreState`].
///
/// This allows static dispatch of this block, via different strategies. Namely:
/// interpretation and Just-In-Time compilation.
pub trait BCall<MC: MemoryConfig, M: ManagerBase> {
    /// The number of instructions contained in the block.
    ///
    /// Executing a block will consume up to `num_instr` steps.
    fn num_instr(&self) -> usize
    where
        M: ManagerRead;

    /// Run a block against the machine state.
    ///
    /// When calling this function, there must be no partial block in progress. To ensure
    /// this, you must always run [`Block::complete_block`] prior to fetching
    /// and running a new block.
    ///
    /// There _must_ also be sufficient steps remaining, to execute the block in full.
    fn run_block(
        &self,
        core: &mut MachineCoreState<MC, M>,
        instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite;
}

/// State Layout for Blocks
pub type BlockLayout = (Atom<u8>, [Atom<Instruction>; CACHE_INSTR]);

/// Functionality required to construct & execute blocks.
///
/// A block is a sequence of at least one instruction, which may be executed sequentially.
/// Blocks will never contain more than [`CACHE_INSTR`] instructions.
pub trait Block<MC: MemoryConfig, M: ManagerBase> {
    /// Block construction may require additional state not kept in storage,
    /// this is then passed as a parameter to [`Block::complete_block`].
    type BlockBuilder: Default;

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

    /// Mark a block as complete.
    ///
    /// This may trigger effects such as JIT-compilation.
    fn complete_block(&mut self, builder: &mut Self::BlockBuilder)
    where
        M: ManagerReadWrite;

    /// Invalidate a block, it will no longer be callable.
    fn invalidate(&mut self)
    where
        M: ManagerWrite;

    /// Reset a block to the default state, it will no longer be callable.
    fn reset(&mut self)
    where
        M: ManagerReadWrite;

    /// Returns the underlying slice of instructions stored in the block.
    fn instr(&self) -> &[EnrichedCell<ICallPlaced<MC, M>, M>]
    where
        M: ManagerRead;

    /// Get a callable block from an entry. The entry must have passed the address and fence
    /// checks.
    ///
    /// # Safety
    ///
    /// The `block_builder` must be the same as the block builder given to the `compile` call that
    /// (may) have natively compiled this block to machine code.
    ///
    /// This ensures that the builder in question is guaranteed to be alive, for at least as long
    /// as this block may be run via `BCall::run_block`.
    unsafe fn callable<'a>(
        &mut self,
        block_builder: &'a Self::BlockBuilder,
    ) -> Option<&mut (impl BCall<MC, M> + ?Sized + 'a)>
    where
        M: ManagerRead + 'a;
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

impl<MC: MemoryConfig, M: ManagerBase> BCall<MC, M> for [EnrichedCell<ICallPlaced<MC, M>, M>] {
    #[inline]
    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.len()
    }

    fn run_block(
        &self,
        core: &mut MachineCoreState<MC, M>,
        mut instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        if let Err(e) = run_block_inner(self, core, &mut instr_pc, steps) {
            core.handle_step_result(instr_pc, Err(e))?;
            // If we succesfully handled an error, need to increment steps one more.
            *steps += 1;
        }

        Ok(())
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

    fn complete_block(&mut self, _builder: &mut Self::BlockBuilder)
    where
        M: ManagerReadWrite,
    {
        // This does nothing in the interpreted world, but will e.g. under JIT
        // (compilation gets triggered)
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
    #[inline]
    unsafe fn callable<'a>(
        &mut self,
        _bb: &'a Self::BlockBuilder,
    ) -> Option<&mut (impl BCall<MC, M> + ?Sized + 'a)>
    where
        M: ManagerRead + 'a,
    {
        let len = self.len_instr.read();
        if len > 0 {
            Some(&mut self.instr[0..len as usize])
        } else {
            None
        }
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

/// Blocks that are compiled to native code for execution, when possible.
///
/// Not all instructions are currently supported, when a block contains
/// unsupported instructions, a fallback to [`Interpreted`] mode occurs.
///
/// Blocks are compiled upon calling [`Block::complete_block`], in a *stop the world* fashion.
pub struct InlineJit<MC: MemoryConfig, M: JitStateAccess> {
    fallback: Interpreted<MC, M>,
    jit_fn: Option<JCall<MC, M>>,
}

impl<MC: MemoryConfig, M: JitStateAccess> Block<MC, M> for InlineJit<MC, M> {
    type BlockBuilder = (JIT<MC, M>, InterpretedBlockBuilder);

    fn start_block(&mut self)
    where
        M: ManagerWrite,
    {
        self.jit_fn = None;
        self.fallback.start_block()
    }

    fn invalidate(&mut self)
    where
        M: ManagerWrite,
    {
        self.jit_fn = None;
        self.fallback.invalidate()
    }

    fn reset(&mut self)
    where
        M: ManagerReadWrite,
    {
        self.jit_fn = None;
        self.fallback.reset()
    }

    fn push_instr(&mut self, instr: Instruction)
    where
        M: ManagerReadWrite,
    {
        self.jit_fn = None;
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
            jit_fn: None,
        }
    }

    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> AllocatedOf<BlockLayout, F::Output> {
        self.fallback.struct_ref::<F>()
    }

    fn complete_block(&mut self, jit: &mut Self::BlockBuilder) {
        self.fallback.complete_block(&mut jit.1);

        if <Self as Block<MC, M>>::num_instr(self) > 0 {
            let instr = self
                .fallback
                .instr
                .iter()
                .take(<Self as Block<MC, M>>::num_instr(self))
                .map(|i| i.read_ref_stored());

            let jitfn = jit.0.compile(instr);

            self.jit_fn = jitfn;
        }
    }

    /// # SAFETY
    ///
    /// The `block_builder` must be the same as the block builder given to the `compile` call that
    /// (may) have natively compiled this block to machine code.
    ///
    /// This ensures that the builder in question is guaranteed to be alive, for at least as long
    /// as this block may be run via [`BCall::run_block`].
    unsafe fn callable<'a>(
        &mut self,
        block_builder: &'a Self::BlockBuilder,
    ) -> Option<&mut (impl BCall<MC, M> + ?Sized + 'a)>
    where
        M: ManagerRead + 'a,
    {
        if self.fallback.callable(&block_builder.1).is_some() {
            Some(self)
        } else {
            None
        }
    }

    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.fallback.num_instr()
    }
}

impl<MC: MemoryConfig, M: JitStateAccess> BCall<MC, M> for InlineJit<MC, M> {
    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.fallback.num_instr()
    }

    fn run_block(
        &self,
        core: &mut MachineCoreState<MC, M>,
        instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        match &self.jit_fn {
            // SAFETY: JIT is guaranteed to be alive here by the caller.
            //         this is due to the only way to run a block being
            //         by calling `Block::callable` first. That function
            //         requires the caller uphold the invariant that
            //         the builder be alive for the lifetime of the
            //         `BCall`.
            Some(jcall) => unsafe { jcall.call(core, instr_pc, steps) },
            None => self.fallback.instr().run_block(core, instr_pc, steps),
        }
    }
}

fn run_block_inner<MC: MemoryConfig, M: ManagerReadWrite>(
    instr: &[EnrichedCell<ICallPlaced<MC, M>, M>],
    core: &mut MachineCoreState<MC, M>,
    instr_pc: &mut Address,
    steps: &mut usize,
) -> Result<(), Exception>
where
    M: ManagerReadWrite,
{
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
