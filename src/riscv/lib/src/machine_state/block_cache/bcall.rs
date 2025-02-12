// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Switching of execution strategy for blocks.
//!
//! Currently just for interperation only, but will expand to cover JIT.

use super::{run_instr, Cached, ICallPlaced};
use crate::{
    machine_state::{
        main_memory::{Address, MainMemoryLayout},
        MachineCoreState, ProgramCounterUpdate,
    },
    state_backend::{EnrichedCell, ManagerBase, ManagerRead, ManagerReadWrite},
    traps::{EnvironException, Exception},
};

/// Functionality required to execute blocks.
///
/// In future, will also cover 'construction' of blocks.
pub trait BCall<'a, ML: MainMemoryLayout, M: ManagerBase> {
    /// The number of instructions contained in the block.
    fn num_instr(&self) -> usize;

    /// Get a callable block from an entry. The entry must contain a valid block.
    fn callable(entry: &'a mut Cached<ML, M>) -> Self
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
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite;
}

/// A block fetched from the block cache, that can be executed against the machine state.
pub struct Block<'a, ML: MainMemoryLayout, M: ManagerBase> {
    pub(super) instr: &'a mut [EnrichedCell<ICallPlaced<ML>, M>],
}

impl<'a, ML: MainMemoryLayout, M: ManagerRead> Block<'a, ML, M> {
    fn run_block_inner(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        instr_pc: &mut Address,
        steps: &mut usize,
    ) -> Result<(), Exception>
    where
        M: ManagerReadWrite,
    {
        for instr in self.instr.iter() {
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
}

impl<'a, ML: MainMemoryLayout, M: ManagerBase> BCall<'a, ML, M> for Block<'a, ML, M> {
    fn num_instr(&self) -> usize {
        self.instr.len()
    }

    fn callable(entry: &'a mut Cached<ML, M>) -> Self
    where
        M: ManagerRead,
    {
        Block {
            instr: &mut entry.instr[..entry.len_instr.read() as usize],
        }
    }

    fn run_block(
        &mut self,
        core: &mut MachineCoreState<ML, M>,
        mut instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        if let Err(e) = self.run_block_inner(core, &mut instr_pc, steps) {
            core.handle_step_result(instr_pc, Err(e))?;
            // If we succesfully handled an error, need to increment steps one more.
            *steps += 1;
        }

        Ok(())
    }
}

impl<'a, ML: MainMemoryLayout, M: ManagerBase> From<&'a mut [EnrichedCell<ICallPlaced<ML>, M>]>
    for Block<'a, ML, M>
{
    fn from(value: &'a mut [EnrichedCell<ICallPlaced<ML>, M>]) -> Self {
        Self { instr: value }
    }
}
