// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::{
        main_memory::{Address, MainMemoryLayout},
        mode::Mode,
        registers, CacheLayouts, MachineError, MachineState,
    },
    program::Program,
    state_backend::{ManagerBase, ManagerRead, ManagerReadWrite, ManagerWrite},
};
use std::ffi::CStr;

/// Size of a memory page in bytes
pub const PAGE_SIZE: u64 = 4096;

impl<ML: MainMemoryLayout, CL: CacheLayouts, M: ManagerBase> MachineState<ML, CL, M> {
    /// Add data to the stack, returning the updated stack pointer.
    fn push_stack(&mut self, align: u64, data: impl AsRef<[u8]>) -> Result<Address, MachineError>
    where
        M: ManagerRead + ManagerWrite,
    {
        let data = data.as_ref();

        let stack_ptr = self.core.hart.xregisters.read(registers::sp);
        let stack_ptr = stack_ptr
            .saturating_sub(stack_ptr % align)
            .saturating_sub(data.len() as u64);

        self.core.hart.xregisters.write(registers::sp, stack_ptr);
        self.core.main_memory.write_all(stack_ptr, data)?;

        Ok(stack_ptr)
    }

    /// Configure the stack for a new process.
    fn prepare_stack(&mut self)
    where
        M: ManagerWrite,
    {
        let mem_size = ML::BYTES as u64;
        let init_stack_ptr = mem_size.saturating_sub(mem_size % PAGE_SIZE);
        self.core
            .hart
            .xregisters
            .write(registers::sp, init_stack_ptr);
    }

    /// Initialise the stack for a Linux program. Preparing the stack is a major part of Linux's
    /// process initialisation. Musl programs extract valuable information from the stack such as
    /// the program name, command-line arguments, environment variables and other auxiliary
    /// information.
    fn init_linux_stack(&mut self, args: &[&CStr], env: &[&CStr]) -> Result<(), MachineError>
    where
        M: ManagerRead + ManagerWrite,
    {
        // First we push all constants so that they are at the top of the stack
        let arg_ptrs = args
            .iter()
            .map(|arg| self.push_stack(1, arg.to_bytes_with_nul()))
            .collect::<Result<Vec<_>, _>>()?;
        let env_ptrs = env
            .iter()
            .map(|arg| self.push_stack(1, arg.to_bytes_with_nul()))
            .collect::<Result<Vec<_>, _>>()?;

        // auxv[0] = [null, null]
        self.push_stack(8, 0u64.to_le_bytes())?;
        self.push_stack(8, 0u64.to_le_bytes())?;

        // envp[n] = null
        self.push_stack(8, 0u64.to_le_bytes())?;

        for &env_ptr in env_ptrs.iter().rev() {
            // envp[i]
            self.push_stack(8, env_ptr.to_le_bytes())?;
        }

        // argv[n] = null
        self.push_stack(8, 0u64.to_le_bytes())?;

        for &arg_ptr in arg_ptrs.iter().rev() {
            // argv[i]
            self.push_stack(8, arg_ptr.to_le_bytes())?;
        }

        // argc
        self.push_stack(8, (arg_ptrs.len() as u64).to_le_bytes())?;

        Ok(())
    }

    /// Load the program into memory and set the PC to its entrypoint.
    fn load_program(&mut self, program: &Program<ML>) -> Result<(), MachineError>
    where
        M: ManagerWrite,
    {
        // Reset hart state & set pc to entrypoint
        self.core.hart.reset(program.entrypoint);

        // Write program to main memory
        for (addr, data) in program.segments.iter() {
            self.core.main_memory.write_all(*addr, data)?;
        }

        Ok(())
    }

    /// Install a Linux program and configure the Hart to start it.
    pub fn setup_linux_process(&mut self, program: &Program<ML>) -> Result<(), MachineError>
    where
        M: ManagerReadWrite,
    {
        self.load_program(program)?;
        self.prepare_stack();
        self.init_linux_stack(&[c"tezos-smart-rollup"], &[])?;

        // The user program may not access the M or S privilege level
        self.core.hart.mode.write(Mode::User);

        Ok(())
    }
}
