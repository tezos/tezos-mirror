// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::machine_state::CacheLayouts;
use crate::machine_state::MachineState;
use crate::machine_state::block_cache::block::Block;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::registers::a0;
use crate::machine_state::registers::a7;
use crate::state::NewState;
use crate::state_backend::Cell;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;

/// Posix execution environment state
pub struct PosixState<M: ManagerBase> {
    code: Cell<u64, M>,
    exited: Cell<u8, M>,
}

impl<M: ManagerBase> PosixState<M> {
    /// If an exit has been requested, return the exit code.
    pub fn exit_code(&self) -> Option<u64>
    where
        M: ManagerRead,
    {
        self.exited().then(|| self.code.read())
    }

    /// Has an exit been requested?
    pub fn exited(&self) -> bool
    where
        M: ManagerRead,
    {
        self.exited.read() > 0
    }

    /// Handle a POSIX system call. Returns `Ok(true)` if it makes sense to continue execution.
    pub fn handle_call<MC: MemoryConfig, CL: CacheLayouts, B: Block<MC, M>>(
        &mut self,
        machine: &mut MachineState<MC, CL, B, M>,
    ) -> Result<bool, String>
    where
        M: ManagerReadWrite,
    {
        if self.exited() {
            // Can't exit twice
            return Err("Machine has already exited".to_owned());
        }

        let mut handle_exit = |code| {
            let exited = self.exited.read();
            self.exited.write(exited.saturating_add(1));
            self.code.write(code);

            Ok(false)
        };

        // Successful physical memory tests set
        //   a7 = 93 & a0 = 0
        // Successful virtual memory tests set
        //   a7 = 0 (a7 never gets set) & a0 = 1
        // Failed physical memory tests set
        //   a7 = 93 & a0 = 1 | (TEST_FAILED << 1)
        // Failed virtual memory tests set
        //   a7 = 0 (a7 never gets set) & a0 = 1 | (TEST_FAILED << 1)
        let a7_val = machine.core.hart.xregisters.read(a7);
        let a0_val = machine.core.hart.xregisters.read(a0);
        match (a7_val, a0_val) {
            // Exit (test pass, physical | virtual)
            (93, 0) | (0, 1) => handle_exit(0),

            // Exit (test fail, physical | virtual)
            (93, code) | (0, code) => handle_exit(code),

            // Unimplemented
            _ => Err(format!("Unknown system call number {a7_val}")),
        }
    }
}

impl<M: ManagerBase> NewState<M> for PosixState<M> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        PosixState {
            code: Cell::new(manager),
            exited: Cell::new(manager),
        }
    }
}
