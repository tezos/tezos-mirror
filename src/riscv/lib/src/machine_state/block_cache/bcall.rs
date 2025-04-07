// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Switching of execution strategy for blocks.
//!
//! Currently just for interperation only, but will expand to cover JIT.

use super::{CACHE_INSTR, ICallLayout, ICallPlaced, run_instr};
use crate::{
    default::ConstDefault,
    machine_state::{
        MachineCoreState, ProgramCounterUpdate,
        instruction::Instruction,
        main_memory::{Address, MainMemoryLayout},
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
pub trait BCall<ML: MainMemoryLayout, M: ManagerBase> {
    /// The number of instructions contained in the block.
    ///
    /// Executing a block will consume up to `num_instr` steps.
    fn num_instr(&self) -> usize
    where
        M: ManagerRead;

    /// Run a block against the machine state.
    ///
    /// When calling this function, there must be no partial block in progress. To ensure
    /// this, you must always run [`complete_current_block`] prior to fetching
    /// and running a new block.
    ///
    /// There _must_ also be sufficient steps remaining, to execute the block in full.
    ///
    /// [`complete_current_block`]: super::BlockCache::complete_current_block
    fn run_block(
        &self,
        core: &mut MachineCoreState<ML, M>,
        instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite;
}

/// State Layout for Blocks
pub type BlockLayout<ML> = (Atom<u8>, [ICallLayout<ML>; CACHE_INSTR]);

/// Functionality required to construct & execute blocks.
///
/// A block is a sequence of at least one instruction, which may be executed sequentially.
/// Blocks will never contain more than [`CACHE_INSTR`] instructions.
pub trait Block<ML: MainMemoryLayout, M: ManagerBase> {
    /// Block construction may require additional state not kept in storage,
    /// this is then passed as a parameter to [`Block::complete_block`].
    type BlockBuilder: Default;

    /// Bind the block to the given allocated state.
    fn bind(allocated: AllocatedOf<BlockLayout<ML>, M>) -> Self;

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<BlockLayout<ML>, F::Output>;

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
    fn instr(&self) -> &[EnrichedCell<ICallPlaced<ML>, M>]
    where
        M: ManagerRead;

    /// Get a callable block from an entry. The entry must have passed the address and fence
    /// checks.
    fn callable(&mut self) -> Option<&mut (impl BCall<ML, M> + ?Sized)>
    where
        M: ManagerRead;
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
pub struct Interpreted<ML: MainMemoryLayout, M: ManagerBase> {
    instr: [EnrichedCell<ICallPlaced<ML>, M>; CACHE_INSTR],
    len_instr: Cell<u8, M>,
}

impl<ML: MainMemoryLayout, M: ManagerBase> BCall<ML, M> for [EnrichedCell<ICallPlaced<ML>, M>] {
    #[inline]
    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.len()
    }

    fn run_block(
        &self,
        core: &mut MachineCoreState<ML, M>,
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

impl<ML: MainMemoryLayout, M: ManagerBase> Block<ML, M> for Interpreted<ML, M> {
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
    fn instr(&self) -> &[EnrichedCell<ICallPlaced<ML>, M>]
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

    fn bind((len_instr, instr): AllocatedOf<BlockLayout<ML>, M>) -> Self {
        Self { len_instr, instr }
    }

    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<BlockLayout<ML>, F::Output> {
        (
            self.len_instr.struct_ref::<F>(),
            self.instr.each_ref().map(|entry| entry.struct_ref::<F>()),
        )
    }

    #[inline]
    fn callable(&mut self) -> Option<&mut (impl BCall<ML, M> + ?Sized)>
    where
        M: ManagerRead,
    {
        let len = self.len_instr.read();
        if len > 0 {
            Some(&mut self.instr[0..len as usize])
        } else {
            None
        }
    }
}

impl<ML: MainMemoryLayout, M: ManagerClone> Clone for Interpreted<ML, M> {
    fn clone(&self) -> Self {
        Self {
            len_instr: self.len_instr.clone(),
            instr: self.instr.clone(),
        }
    }
}

fn run_block_inner<ML: MainMemoryLayout, M: ManagerReadWrite>(
    instr: &[EnrichedCell<ICallPlaced<ML>, M>],
    core: &mut MachineCoreState<ML, M>,
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
