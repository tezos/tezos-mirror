// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::machine_state::MachineCoreState;
use crate::machine_state::memory;
use crate::machine_state::registers::XRegister;
use crate::state_backend as backend;
use crate::traps::Exception;

impl<MC, M> MachineCoreState<MC, M>
where
    MC: memory::MemoryConfig,
    M: backend::ManagerReadWrite,
{
    /// `SFENCE.VMA` instruction
    ///
    /// The supervisor memory-management fence instruction SFENCE.VMA is used to
    /// synchronize updates to in-memory memory-management data structures
    /// with current execution. See sections 3.1.6.5, 3.7.2.
    ///
    /// Section 5.2.1: It is always legal to over-fence.
    #[inline(always)]
    pub fn run_sfence_vma(&mut self, _asid: XRegister, _vaddr: XRegister) -> Result<(), Exception> {
        // Even if we over-fence, thus ignoring asid and vaddr, this instruction
        // is still no-op since memory loads/stores are not cached currently.
        Ok(())
    }
}
