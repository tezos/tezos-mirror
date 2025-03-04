// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod addr;
mod error;
mod fds;
mod fs;
mod rng;

use std::ffi::CStr;
use std::num::NonZeroU64;

use tezos_smart_rollup_constants::riscv::SBI_FIRMWARE_TEZOS;

use self::addr::VirtAddr;
use self::error::Error;
use super::Pvm;
use super::PvmHooks;
use crate::machine_state::CacheLayouts;
use crate::machine_state::MachineCoreState;
use crate::machine_state::MachineError;
use crate::machine_state::MachineState;
use crate::machine_state::block_cache::bcall::Block;
use crate::machine_state::memory::Address;
use crate::machine_state::memory::Memory;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::memory::PAGE_SIZE;
use crate::machine_state::memory::Permissions;
use crate::machine_state::mode::Mode;
use crate::machine_state::registers;
use crate::program::Program;
use crate::state_backend::AllocatedOf;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerWrite;
use crate::state_backend::Ref;

/// Thread identifier for the main thread
const MAIN_THREAD_ID: u64 = 1;

/// System call number for `getcwd` on RISC-V
const GETCWD: u64 = 17;

/// System call number for `openat` on RISC-V
const OPENAT: u64 = 56;

/// System call number for `write` on RISC-V
const WRITE: u64 = 64;

/// System call number for `writev` on RISC-V
const WRITEV: u64 = 66;

/// System call number for `ppoll` on RISC-V
const PPOLL: u64 = 73;

/// System call number for `readlinkat` on RISC-V
const READLINKAT: u64 = 78;

/// System call number for `exit` on RISC-V
const EXIT: u64 = 93;

/// System call number for `exit_group` on RISC-V
const EXITGROUP: u64 = 94;

/// System call number for `set_tid_address` on RISC-V
const SET_TID_ADDRESS: u64 = 96;

/// System call number for `tkill` on RISC-V
const TKILL: u64 = 130;

/// System call number for `sigaltstack` on RISC-V
const SIGALTSTACK: u64 = 132;

/// System call number for `rt_sigaction` on RISC-V
const RT_SIGACTION: u64 = 134;

/// System call number for `rt_sigprocmask` on RISC-V
const RT_SIGPROCMASK: u64 = 135;

/// System call number for `getrandom` on RISC-V
const GETRANDOM: u64 = 278;

/// Size of the `sigset_t` type in bytes
///
/// As we're building a 64-bit system, the sigset should be 64-bit wide as well.
const SIGSET_SIZE: u64 = 8;

/// Key into the auxiliary vector which informs supervised processes of auxiliary information
#[derive(Clone, Copy)]
#[repr(u64)]
enum AuxVectorKey {
    /// [AT_PAGESZ](https://github.com/torvalds/linux/blob/bb066fe812d6fb3a9d01c073d9f1e2fd5a63403b/include/uapi/linux/auxvec.h#L15)
    PageSize = 6,

    /// [AT_PHNUM](https://github.com/torvalds/linux/blob/bb066fe812d6fb3a9d01c073d9f1e2fd5a63403b/include/uapi/linux/auxvec.h#L14)
    NumProgramHeaders = 5,

    /// [AT_PHENT](https://github.com/torvalds/linux/blob/bb066fe812d6fb3a9d01c073d9f1e2fd5a63403b/include/uapi/linux/auxvec.h#L13)
    ProgramHeaderSize = 4,

    /// [AT_PHDR](https://github.com/torvalds/linux/blob/bb066fe812d6fb3a9d01c073d9f1e2fd5a63403b/include/uapi/linux/auxvec.h#L12)
    ProgramHeadersPtr = 3,
}

impl<MC: MemoryConfig, CL: CacheLayouts, B: Block<MC, M>, M: ManagerBase>
    MachineState<MC, CL, B, M>
{
    /// Add data to the stack, returning the updated stack pointer.
    fn push_stack(&mut self, align: u64, data: impl AsRef<[u8]>) -> Result<Address, MachineError>
    where
        M: ManagerReadWrite,
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
        let mem_size = MC::TOTAL_BYTES as u64;
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
    fn init_linux_stack(
        &mut self,
        args: &[&CStr],
        env: &[&CStr],
        auxv: &[(AuxVectorKey, u64)],
    ) -> Result<(), MachineError>
    where
        M: ManagerReadWrite,
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

        // auxv[n] = [null, null]
        self.push_stack(8, 0u64.to_le_bytes())?;
        self.push_stack(8, 0u64.to_le_bytes())?;

        // auxv[..] = [key, value]
        for (key, value) in auxv.iter() {
            self.push_stack(8, value.to_le_bytes())?;
            self.push_stack(8, (*key as u64).to_le_bytes())?;
        }

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
}

impl<MC: MemoryConfig, CL: CacheLayouts, B: Block<MC, M>, M: ManagerBase> Pvm<MC, CL, B, M> {
    /// Load the program into memory and set the PC to its entrypoint.
    fn load_program(&mut self, program: &Program<MC>) -> Result<(), MachineError>
    where
        M: ManagerReadWrite,
    {
        // Reset hart state & set pc to entrypoint
        self.machine_state.core.hart.reset(program.entrypoint);

        let program_start = program.segments.keys().min().copied().unwrap_or(0);
        let program_end = program
            .segments
            .iter()
            .map(|(addr, data)| addr.saturating_add(data.len() as u64))
            .max()
            .unwrap_or(0);
        let program_length = program_end.saturating_sub(program_start) as usize;

        // Allow the program to be written to main memory
        self.machine_state.core.main_memory.protect_pages(
            program_start,
            program_length,
            Permissions::Write,
        )?;

        // Write program to main memory
        for (&addr, data) in program.segments.iter() {
            self.machine_state.core.main_memory.write_all(addr, data)?;
        }

        // Remove access to the program that has just been placed into memory
        self.machine_state.core.main_memory.protect_pages(
            program_start,
            program_length,
            Permissions::None,
        )?;

        // Configure memory permissions using the ELF program headers, if present
        if let Some(program_headers) = &program.program_headers {
            for mem_perms in program_headers.permissions.iter() {
                self.machine_state.core.main_memory.protect_pages(
                    mem_perms.start_address,
                    mem_perms.length as usize,
                    mem_perms.permissions,
                )?;
            }
        }

        // Other parts of the supervisor make use of program start and end to properly divide the
        // memory. These addresses need to be properly aligned.
        let program_start = VirtAddr::new(program_start).align_down(PAGE_SIZE);
        self.system_state.program_start.write(program_start);

        let program_end = VirtAddr::new(program_end)
            .align_up(PAGE_SIZE)
            .ok_or(MachineError::MemoryTooSmall)?;
        self.system_state.program_end.write(program_end);

        Ok(())
    }

    /// Install a Linux program and configure the Hart to start it.
    pub fn setup_linux_process(&mut self, program: &Program<MC>) -> Result<(), MachineError>
    where
        M: ManagerReadWrite,
    {
        self.load_program(program)?;

        // The stack needs to be prepared before we can push anything to it
        self.machine_state.prepare_stack();

        // Auxiliary values vector
        let mut auxv = vec![(AuxVectorKey::PageSize, PAGE_SIZE.get())];

        // If program headers are available, then we should inform the supervised process of them
        if let Some(prog_headers) = &program.program_headers {
            // Program headers are an array of a C struct. The struct for 64-bit ELF requires 8
            // byte alignment.
            let prog_headers_ptr = self.machine_state.push_stack(8, prog_headers.contents)?;

            auxv.push((AuxVectorKey::NumProgramHeaders, prog_headers.num_entries));
            auxv.push((AuxVectorKey::ProgramHeaderSize, prog_headers.entry_size));
            auxv.push((AuxVectorKey::ProgramHeadersPtr, prog_headers_ptr));
        }

        self.machine_state
            .init_linux_stack(&[c"tezos-smart-rollup"], &[], &auxv)?;

        // The user program may not access the M or S privilege level
        self.machine_state.core.hart.mode.write(Mode::User);

        Ok(())
    }

    /// Check if the supervised process has requested an exit.
    pub fn has_exited(&self) -> Option<u64>
    where
        M: ManagerRead,
    {
        if self.system_state.exited.read() {
            let code = self.system_state.exit_code.read();
            return Some(code);
        }

        None
    }
}

/// Layout for the Linux supervisor state
pub type SupervisorStateLayout = (
    Atom<u64>,
    Atom<bool>,
    Atom<u64>,
    Atom<VirtAddr>,
    Atom<VirtAddr>,
);

/// Linux supervisor state
pub struct SupervisorState<M: ManagerBase> {
    tid_address: Cell<u64, M>,
    exited: Cell<bool, M>,
    exit_code: Cell<u64, M>,
    program_start: Cell<VirtAddr, M>,
    program_end: Cell<VirtAddr, M>,
}

impl<M: ManagerBase> SupervisorState<M> {
    /// Bind the given allocated regions to the supervisor state.
    pub fn bind(space: AllocatedOf<SupervisorStateLayout, M>) -> Self {
        SupervisorState {
            tid_address: space.0,
            exited: space.1,
            exit_code: space.2,
            program_start: space.3,
            program_end: space.4,
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<SupervisorStateLayout, F::Output> {
        (
            self.tid_address.struct_ref::<F>(),
            self.exited.struct_ref::<F>(),
            self.exit_code.struct_ref::<F>(),
            self.program_start.struct_ref::<F>(),
            self.program_end.struct_ref::<F>(),
        )
    }

    /// Handle a Linux system call.
    pub fn handle_system_call<MC>(
        &mut self,
        core: &mut MachineCoreState<MC, M>,
        hooks: &mut PvmHooks,
        on_tezos: impl FnOnce(&mut MachineCoreState<MC, M>) -> bool,
    ) -> bool
    where
        MC: MemoryConfig,
        M: ManagerReadWrite,
    {
        // We need to jump to the next instruction. The ECall instruction which triggered this
        // function is 4 byte wide.
        let pc = core.hart.pc.read().saturating_add(4);
        core.hart.pc.write(pc);

        // Programs targeting a Linux kernel pass the system call number in register a7
        let system_call_no = core.hart.xregisters.read(registers::a7);

        match system_call_no {
            GETCWD => return self.handle_getcwd(core),
            OPENAT => return self.handle_openat(core),
            WRITE => return self.handle_write(core, hooks),
            WRITEV => return self.handle_writev(core, hooks),
            PPOLL => return self.handle_ppoll(core),
            READLINKAT => return self.handle_readlinkat(core),
            EXIT | EXITGROUP => return self.handle_exit(core),
            SET_TID_ADDRESS => return self.handle_set_tid_address(core),
            TKILL => return self.handle_tkill(core),
            SIGALTSTACK => return self.handle_sigaltstack(core),
            RT_SIGACTION => return self.handle_rt_sigaction(core),
            RT_SIGPROCMASK => return self.handle_rt_sigprocmask(core),
            GETRANDOM => return self.handle_getrandom(core),
            SBI_FIRMWARE_TEZOS => return on_tezos(core),
            _ => {}
        }

        let xregisters = &core.hart.xregisters;

        // TODO: RV-413: Don't use `eprintln!`
        eprintln!("> Unimplemented system call: {system_call_no}");
        eprintln!("\ta0 = {}", xregisters.read(registers::a0));
        eprintln!("\ta1 = {}", xregisters.read(registers::a1));
        eprintln!("\ta2 = {}", xregisters.read(registers::a2));
        eprintln!("\ta3 = {}", xregisters.read(registers::a3));
        eprintln!("\ta4 = {}", xregisters.read(registers::a4));
        eprintln!("\ta5 = {}", xregisters.read(registers::a5));
        eprintln!("\ta6 = {}", xregisters.read(registers::a6));

        core.hart
            .xregisters
            .write_system_call_error(Error::NoSystemCall);

        false
    }

    /// Handle `set_tid_address` system call.
    ///
    /// See: <https://www.man7.org/linux/man-pages/man2/set_tid_address.2.html>
    fn handle_set_tid_address(&mut self, core: &mut MachineCoreState<impl MemoryConfig, M>) -> bool
    where
        M: ManagerRead + ManagerWrite,
    {
        // NOTE: `set_tid_address` is mostly important for when a thread terminates. As we don't
        // really support threading yet, we only save the address and do nothing else.
        // In the future, when we add threading, this system call needs to be implemented to
        // support informing other (waiting) threads of termination.

        // The address is passed as the first and only parameter
        let tid_address = core.hart.xregisters.read(registers::a0);
        self.tid_address.write(tid_address);

        // The caller expects the Thread ID to be returned
        core.hart.xregisters.write(registers::a0, MAIN_THREAD_ID);

        true
    }

    fn handle_exit(&mut self, core: &mut MachineCoreState<impl MemoryConfig, M>) -> bool
    where
        M: ManagerReadWrite,
    {
        let status = core.hart.xregisters.read(registers::a0);
        self.exit_code.write(status);
        self.exited.write(true);

        false
    }

    /// Handle `sigaltstack` system call. The new signal stack configuration is discarded. If the
    /// old signal stack configuration is requested, it will be zeroed out.
    fn handle_sigaltstack(&mut self, core: &mut MachineCoreState<impl MemoryConfig, M>) -> bool
    where
        M: ManagerReadWrite,
    {
        let old = core.hart.xregisters.read(registers::a1);

        /// `sizeof(struct sigaltstack)` on the Kernel side
        const SIZE_SIGALTSTACK: usize = 24;

        if let Some(old) = NonZeroU64::new(old) {
            let Ok(()) = core.main_memory.write(old.get(), [0u8; SIZE_SIGALTSTACK]) else {
                core.hart.xregisters.write_system_call_error(Error::Fault);
                return true;
            };
        }

        // Return 0 as an indicator of success
        core.hart.xregisters.write(registers::a0, 0);

        true
    }

    /// Handle `rt_sigaction` system call. This does nothing effectively. It does not support
    /// retrieving the previous handler for a signal - it just zeroes out the memory.
    ///
    /// See: <https://www.man7.org/linux/man-pages/man2/rt_sigaction.2.html>
    fn handle_rt_sigaction(&mut self, core: &mut MachineCoreState<impl MemoryConfig, M>) -> bool
    where
        M: ManagerReadWrite,
    {
        let old_action = core.hart.xregisters.read(registers::a2);
        let sigset_t_size = core.hart.xregisters.read(registers::a3);

        // As we're implementing a 64-bit system, the size of `sigset_t` must be 8 bytes.
        // This is an assumption which is used in the remainder of the function body.
        if sigset_t_size != SIGSET_SIZE {
            core.hart
                .xregisters
                .write_system_call_error(Error::InvalidArgument);
            return true;
        }

        /// `sizeof(struct sigaction)` on the Kernel side
        const SIZE_SIGACTION: usize = 32;

        if let Some(old_action) = NonZeroU64::new(old_action) {
            // As we don't store the previous signal handler, we just zero out the memory
            let Ok(()) = core
                .main_memory
                .write(old_action.get(), [0u8; SIZE_SIGACTION])
            else {
                core.hart.xregisters.write_system_call_error(Error::Fault);
                return true;
            };
        }

        // Return 0 as an indicator of success
        core.hart.xregisters.write(registers::a0, 0);

        true
    }

    /// Handle `rt_sigprocmask` system call. This does nothing effectively. If the previous mask is
    /// requested, it will simply be zeroed out.
    fn handle_rt_sigprocmask(&mut self, core: &mut MachineCoreState<impl MemoryConfig, M>) -> bool
    where
        M: ManagerReadWrite,
    {
        let old = core.hart.xregisters.read(registers::a2);
        let sigset_t_size = core.hart.xregisters.read(registers::a3);

        // As we're implementing a 64-bit system, the size of `sigset_t` must be 8 bytes.
        // This is an assumption which is used in the remainder of the function body.
        if sigset_t_size != SIGSET_SIZE {
            core.hart
                .xregisters
                .write_system_call_error(Error::InvalidArgument);
            return true;
        }

        if let Some(old_action) = NonZeroU64::new(old) {
            // As we don't store the previous mask, we just zero out the memory
            let Ok(()) = core
                .main_memory
                .write(old_action.get(), [0u8; SIGSET_SIZE as usize])
            else {
                core.hart.xregisters.write_system_call_error(Error::Fault);
                return true;
            };
        }

        // Return 0 as an indicator of success
        core.hart.xregisters.write(registers::a0, 0);

        true
    }

    /// Handle `tkill` system call. As there is only one thread at the moment, this system call
    /// will return an error if the thread ID is not the main thread ID.
    fn handle_tkill(&mut self, core: &mut MachineCoreState<impl MemoryConfig, M>) -> bool
    where
        M: ManagerReadWrite,
    {
        let thread_id = core.hart.xregisters.read(registers::a0);
        let signal = core.hart.xregisters.read(registers::a1);

        if thread_id != MAIN_THREAD_ID {
            // We only support exiting the main thread
            core.hart.xregisters.write_system_call_error(Error::Search);
            return true;
        }

        // Indicate that we have exited
        self.exited.write(true);

        /// Setting bit 2^7 of the exit code indicates that the process was killed by a signal
        const EXIT_BY_SIGNAL: u64 = 1 << 7;

        /// Only 7 bits may be used to indicate the signal that terminated the process
        const EXIT_SIGNAL_MASK: u64 = EXIT_BY_SIGNAL - 1;

        self.exit_code
            .write(EXIT_BY_SIGNAL | signal & EXIT_SIGNAL_MASK);

        // Return 0 as an indicator of success, even if this might not actually be used
        core.hart.xregisters.write(registers::a0, 0);

        false
    }
}

impl<M: ManagerClone> Clone for SupervisorState<M> {
    fn clone(&self) -> Self {
        Self {
            tid_address: self.tid_address.clone(),
            exited: self.exited.clone(),
            exit_code: self.exit_code.clone(),
            program_start: self.program_start.clone(),
            program_end: self.program_end.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::array;

    use rand::Rng;

    use super::*;
    use crate::backend_test;
    use crate::create_state;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::memory::M4K;

    /// Default handler for the `on_tezos` parameter of [`SupervisorState::handle_system_call`]
    fn default_on_tezos_handler<MC, M>(core: &mut MachineCoreState<MC, M>) -> bool
    where
        MC: MemoryConfig,
        M: ManagerWrite,
    {
        core.hart
            .xregisters
            .write_system_call_error(Error::NoSystemCall);
        true
    }

    // Check that the `set_tid_address` system call is working correctly.
    backend_test!(set_tid_address, F, {
        type MemLayout = M4K;
        const MEM_BYTES: usize = MemLayout::TOTAL_BYTES;

        let mut machine_state = create_state!(
            MachineCoreState,
            MachineCoreStateLayout<MemLayout>,
            F,
            MemLayout
        );

        let mut supervisor_state = create_state!(SupervisorState, SupervisorStateLayout, F);

        machine_state
            .hart
            .xregisters
            .write(registers::a7, SET_TID_ADDRESS);

        let tid_address = rand::thread_rng().gen_range(0..MEM_BYTES as Address);
        machine_state
            .hart
            .xregisters
            .write(registers::a0, tid_address);

        let result = supervisor_state.handle_system_call(
            &mut machine_state,
            &mut PvmHooks::default(),
            default_on_tezos_handler,
        );
        assert!(result);

        assert_eq!(supervisor_state.tid_address.read(), tid_address);
    });

    // Check `ppoll` system call the way it is used in Musl and Rust's initialisation code.
    backend_test!(ppoll_init_fds, F, {
        type MemLayout = M4K;

        let mut machine_state = create_state!(
            MachineCoreState,
            MachineCoreStateLayout<MemLayout>,
            F,
            MemLayout
        );

        for fd in [0i32, 1, 2] {
            let mut supervisor_state = create_state!(SupervisorState, SupervisorStateLayout, F);

            let base_address = 0x10;
            machine_state.main_memory.write(base_address, fd).unwrap();
            machine_state
                .main_memory
                .write(base_address + 4, -1i16)
                .unwrap();
            machine_state
                .main_memory
                .write(base_address + 6, -1i16)
                .unwrap();

            machine_state
                .hart
                .xregisters
                .write(registers::a0, base_address);
            machine_state.hart.xregisters.write(registers::a1, 1);
            machine_state.hart.xregisters.write(registers::a2, 0);
            machine_state.hart.xregisters.write(registers::a3, 0);
            machine_state.hart.xregisters.write(registers::a7, PPOLL);

            let result = supervisor_state.handle_system_call(
                &mut machine_state,
                &mut PvmHooks::default(),
                default_on_tezos_handler,
            );
            assert!(result);

            let ret = machine_state.hart.xregisters.read(registers::a0);
            assert_eq!(ret, 0);

            let revents = machine_state
                .main_memory
                .read::<i16>(base_address + 6)
                .unwrap();
            assert_eq!(revents, 0);
        }
    });

    // Check that the `rt_sigaction` system call is working correctly for a basic case.
    backend_test!(rt_sigaction_no_handler, F, {
        type MemLayout = M4K;

        let mut machine_state = create_state!(
            MachineCoreState,
            MachineCoreStateLayout<MemLayout>,
            F,
            MemLayout
        );
        let mut supervisor_state = create_state!(SupervisorState, SupervisorStateLayout, F);

        // System call number
        machine_state
            .hart
            .xregisters
            .write(registers::a7, RT_SIGACTION);

        // Signal is SIGPIPE
        machine_state
            .hart
            .xregisters
            .write(registers::a0, 13i32 as u64);

        // New handler is located at this address
        machine_state.hart.xregisters.write(registers::a1, 0x20);

        // Old handler will be written to this address
        machine_state.hart.xregisters.write(registers::a2, 0x40);
        machine_state
            .main_memory
            .write(0x40, array::from_fn::<u8, 32, _>(|i| i as u8))
            .unwrap();

        // Size of sigset_t
        machine_state.hart.xregisters.write(registers::a3, 8);

        // Perform the system call
        let result = supervisor_state.handle_system_call(
            &mut machine_state,
            &mut PvmHooks::default(),
            default_on_tezos_handler,
        );
        assert!(result);

        // Check if the location where the old handler was is now zeroed out
        let old_action = machine_state.main_memory.read::<[u8; 32]>(0x40).unwrap();
        assert_eq!(old_action, [0u8; 32]);
    });
}
