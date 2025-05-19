// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod addr;
pub mod error;
mod fds;
mod fs;
mod memory;
mod parameters;
mod rng;

use std::convert::Infallible;
use std::ffi::CStr;
use std::ops::Range;

use tezos_smart_rollup_constants::riscv::SBI_FIRMWARE_TEZOS;

use self::addr::VirtAddr;
use self::error::Error;
use self::memory::STACK_SIZE;
use super::Pvm;
use super::PvmHooks;
use crate::machine_state::CacheLayouts;
use crate::machine_state::MachineCoreState;
use crate::machine_state::MachineError;
use crate::machine_state::MachineState;
use crate::machine_state::block_cache::block::Block;
use crate::machine_state::memory::Address;
use crate::machine_state::memory::Memory;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::memory::PAGE_SIZE;
use crate::machine_state::memory::Permissions;
use crate::machine_state::registers;
use crate::program::Program;
use crate::state::NewState;
use crate::state_backend::AllocatedOf;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerWrite;
use crate::state_backend::Ref;
use crate::struct_layout;

/// Thread identifier for the main thread
const MAIN_THREAD_ID: u64 = 1;

/// System call number for `getcwd` on RISC-V
const GETCWD: u64 = 17;

/// System call number for `openat` on RISC-V
const OPENAT: u64 = 56;

/// System call number for `write` on RISC-V
pub(crate) const WRITE: u64 = 64;

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

/// System call number for `brk` on RISC-V
const BRK: u64 = 214;

/// System call number for `munmap` on RISC-V
const MUNMAP: u64 = 215;

/// System call number for `mmap` on RISC-V
const MMAP: u64 = 222;

/// System call number for `mprotect` on RISC-V
const MPROTECT: u64 = 226;

/// System call number for `madvise` on RISC-V
const MADVISE: u64 = 233;

/// System call number for `getrandom` on RISC-V
const GETRANDOM: u64 = 278;

/// System call number for `clock_gettime` on RISC-V
const CLOCK_GETTIME: u64 = 113;

/// System call number for `gettimeofday` on RISC-V
const GETTIMEOFDAY: u64 = 169;

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

impl<MC, CL, B, M> Pvm<MC, CL, B, M>
where
    MC: MemoryConfig,
    CL: CacheLayouts,
    B: Block<MC, M>,
    M: ManagerBase,
{
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
        let program_end = VirtAddr::new(program_end)
            .align_up(PAGE_SIZE)
            .ok_or(MachineError::MemoryTooSmall)?;
        self.system_state.program.write(program_start..program_end);

        Ok(())
    }

    /// Configure the stack for a new process.
    fn prepare_stack(&mut self) -> Result<(), MachineError>
    where
        M: ManagerReadWrite,
    {
        let stack_top = VirtAddr::new(MC::TOTAL_BYTES as u64);

        // We must fit at least one guard page between the program break and the stack
        let guarded_stack_space = stack_top - self.system_state.program.end;
        if guarded_stack_space < PAGE_SIZE.get() as i64 {
            return Err(MachineError::MemoryTooSmall);
        }

        let unaligned_stack_space = STACK_SIZE.min(guarded_stack_space as u64 - PAGE_SIZE.get());
        let stack_bottom = (stack_top - unaligned_stack_space)
            .align_up(PAGE_SIZE)
            .ok_or(MachineError::MemoryTooSmall)?;

        // If the stack top wasn't aligned, the stack bottom may be higher than the stack top after
        // aligning it upwards
        if stack_top < stack_bottom {
            return Err(MachineError::MemoryTooSmall);
        }

        // At this point we know that `stack_top` >= `stack_bottom`
        let stack_space = (stack_top - stack_bottom) as usize;

        // Guard the stack with a guard page. This prevents stack overflows spilling into the heap
        // or even worse, the program's .bss or .data area.
        let stack_guard = stack_bottom - PAGE_SIZE.get();
        self.machine_state.core.main_memory.protect_pages(
            stack_guard.to_machine_address(),
            PAGE_SIZE.get() as usize,
            Permissions::None,
        )?;

        // Make sure the stack region is readable and writable
        self.machine_state.core.main_memory.protect_pages(
            stack_bottom.to_machine_address(),
            stack_space,
            Permissions::ReadWrite,
        )?;

        self.machine_state
            .core
            .hart
            .xregisters
            .write(registers::sp, stack_top.to_machine_address());

        // Remember the stack guard for later use
        self.system_state
            .stack_guard
            .write(stack_guard..stack_bottom);

        Ok(())
    }

    /// Install a Linux program and configure the Hart to start it.
    pub fn setup_linux_process(&mut self, program: &Program<MC>) -> Result<(), MachineError>
    where
        M: ManagerReadWrite,
    {
        self.load_program(program)?;

        // The stack needs to be prepared before we can push anything to it
        self.prepare_stack()?;

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

        self.machine_state.init_linux_stack(
            &[c"tezos-smart-rollup"],
            &[c"RUST_BACKTRACE=full"],
            &auxv,
        )?;

        // Setup heap addresses
        let program_end = self.system_state.program.end;
        let heap_start = program_end
            .align_up(PAGE_SIZE)
            .ok_or(MachineError::MemoryTooSmall)?;

        self.system_state
            .heap
            .write(heap_start..self.system_state.stack_guard.start);

        // Mark all memory as allocated. This also has the benefit of initialising the buddy memory
        // manager properly.
        self.machine_state
            .core
            .main_memory
            .allocate_pages(Some(0), MC::TOTAL_BYTES, true)?;

        // Make sure only the heap can be used for allocation by the user kernel.
        self.machine_state.core.main_memory.deallocate_pages(
            self.system_state.heap.start.to_machine_address(),
            (self.system_state.heap.end - self.system_state.heap.start) as usize,
        )?;

        Ok(())
    }

    /// Check if the supervised process has requested an exit.
    ///
    /// # Safety
    ///
    /// This function should only be used as an optimization to terminate the PVM stepper early.
    /// Do not use this result in production PVM contexts as it may cause state divergence.
    /// The return value depends on internal state that is not tracked by the PVM state,
    /// which means different PVM implementations could return different values for the
    /// same input state.
    pub(crate) unsafe fn has_exited(&self) -> Option<u64> {
        if self.system_state.exited {
            return Some(self.system_state.exit_code);
        }

        None
    }
}

struct_layout! {
    pub struct SupervisorStateLayout {
        tid_address: Atom<VirtAddr>,
        program: Atom<Range<VirtAddr>>,
        heap: Atom<Range<VirtAddr>>,
        stack_guard: Atom<Range<VirtAddr>>,
    }
}

/// Linux supervisor state
pub struct SupervisorState<M: ManagerBase> {
    /// Thread lock address
    tid_address: Cell<VirtAddr, M>,

    /// Has the process exited?
    exited: bool,

    /// Exit code for when the process exited
    exit_code: u64,

    /// Program in memory
    program: Cell<Range<VirtAddr>, M>,

    /// Heap memory
    heap: Cell<Range<VirtAddr>, M>,

    /// Stack guard
    stack_guard: Cell<Range<VirtAddr>, M>,
}

impl<M: ManagerBase> SupervisorState<M> {
    /// Allocate a new supervisor state.
    pub fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        SupervisorState {
            tid_address: Cell::new(manager),
            exited: false,
            exit_code: 0,
            program: Cell::new(manager),
            heap: Cell::new(manager),
            stack_guard: Cell::new(manager),
        }
    }

    /// Bind the given allocated regions to the supervisor state.
    pub fn bind(space: AllocatedOf<SupervisorStateLayout, M>) -> Self {
        SupervisorState {
            tid_address: space.tid_address,
            exited: false,
            exit_code: 0,
            program: space.program,
            stack_guard: space.stack_guard,
            heap: space.heap,
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<SupervisorStateLayout, F::Output> {
        SupervisorStateLayoutF {
            tid_address: self.tid_address.struct_ref::<F>(),
            program: self.program.struct_ref::<F>(),
            stack_guard: self.stack_guard.struct_ref::<F>(),
            heap: self.heap.struct_ref::<F>(),
        }
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

        /// Read an argument from a register and interpret it as a system call argument.
        /// If that fails, log the failure.
        macro_rules! read_arg {
            ($system_call:ident, $reg:ident) => {
                core.hart
                    .xregisters
                    .try_read(registers::$reg)
                    .inspect_err(|err| {
                        crate::log::warning! {
                            argument = stringify!($reg),
                            value = core.hart.xregisters.read(registers::$reg),
                            error = format!("{err:?}"),
                            pc,
                            "{} failed",
                            stringify!($system_call),
                        }
                    })?
            };
        }

        /// Log the system call name and its arguments.
        macro_rules! trace_call {
            ($system_call:ident $(, $arg:expr)*) => {
                crate::log::trace! {
                    "{}{:?}",
                    stringify!($system_call),
                    ($(&$arg),*)
                }
            };
        }

        /// Check the result of a system call. If it failed, log the error.
        macro_rules! check_result {
            ($system_call:ident, $result:expr) => {{
                let parameters::SystemCallResultExecution {
                    result,
                    control_flow,
                } = $result
                    .inspect(|res| {
                        crate::log::trace! {
                            result = format!("{res:?}"),
                            "{} succeeded",
                            stringify!($system_call),
                        }
                    })
                    .inspect_err(|err| {
                        crate::log::warning! {
                            error = format!("{err:?}"),
                            pc,
                            "{} failed",
                            stringify!($system_call),
                        }
                    })?
                    .into();
                core.hart.xregisters.write(registers::a0, result);
                control_flow
            }};
        }

        // `dispatch0!(system_call_no [, optional_arguments_passed_to_handler])`
        // Converts the system call name to the handler
        macro_rules! dispatch0 {
            ($system_call:ty$(, $arg:ident)*) => {{
                try_blocks::try_block! {
                    paste::paste! {
                        trace_call!($system_call);
                        let result = self.[<handle_$system_call>]($($arg)*);
                        check_result!($system_call, result)
                    }
                }
            }};
        }

        // `dispatch1!(system_call_no [, optional_arguments_passed_to_handler])`
        // Converts the system call name to the handler
        macro_rules! dispatch1 {
            ($system_call:ty$(, $arg:ident)*) => {{
                try_blocks::try_block! {
                    paste::paste! {
                        let arg1 = read_arg!($system_call, a0);
                        trace_call!($system_call, arg1);
                        let result = self.[<handle_$system_call>]($($arg,)* arg1);
                        check_result!($system_call, result)
                    }
                }
            }};
        }

        // `dispatch2!(system_call_no [, optional_arguments_passed_to_handler])`
        // Converts the system call name to the handler
        macro_rules! dispatch2 {
            ($system_call:ty$(, $arg:ident)*) => {{
                try_blocks::try_block! {
                    paste::paste! {
                        let arg1 = read_arg!($system_call, a0);
                        let arg2 = read_arg!($system_call, a1);
                        trace_call!($system_call, arg1, arg2);
                        let result = self.[<handle_$system_call>]($($arg,)* arg1, arg2);
                        check_result!($system_call, result)
                    }
                }
            }};
        }

        // `dispatch3!(system_call_no [, optional_arguments_passed_to_handler])`
        // Converts the system call name to the handler
        macro_rules! dispatch3 {
            ($system_call:ty$(, $arg:ident)*) => {{
                try_blocks::try_block! {
                    paste::paste! {
                        let arg1 = read_arg!($system_call, a0);
                        let arg2 = read_arg!($system_call, a1);
                        let arg3 = read_arg!($system_call, a2);
                        trace_call!($system_call, arg1, arg2, arg3);
                        let result = self.[<handle_$system_call>]($($arg,)* arg1, arg2, arg3);
                        check_result!($system_call, result)
                    }
                }
            }};
        }

        // `dispatch4!(system_call_no [, optional_arguments_passed_to_handler])`
        // Converts the system call name to the handler
        macro_rules! dispatch4 {
            ($system_call:ty$(, $arg:ident)*) => {{
                try_blocks::try_block! {
                    paste::paste! {
                        let arg1 = read_arg!($system_call, a0);
                        let arg2 = read_arg!($system_call, a1);
                        let arg3 = read_arg!($system_call, a2);
                        let arg4 = read_arg!($system_call, a3);
                        trace_call!($system_call, arg1, arg2, arg3, arg4);
                        let result = self.[<handle_$system_call>]($($arg,)* arg1, arg2, arg3, arg4);
                        check_result!($system_call, result)
                    }
                }
            }};
        }

        // `dispatch5!(system_call_no [, optional_arguments_passed_to_handler])`
        // Converts the system call name to the handler
        #[expect(
            unused_macros,
            reason = "There is no system call handler with 5 parameters yet"
        )]
        macro_rules! dispatch5 {
            ($system_call:ty$(, $arg:ident)*) => {{
                try_blocks::try_block! {
                    paste::paste! {
                        let arg1 = read_arg!($system_call, a0);
                        let arg2 = read_arg!($system_call, a1);
                        let arg3 = read_arg!($system_call, a2);
                        let arg4 = read_arg!($system_call, a3);
                        let arg5 = read_arg!($system_call, a4);
                        trace_call!($system_call, arg1, arg2, arg3, arg4, arg5);
                        let result = self.[<handle_$system_call>]($($arg,)* arg1, arg2, arg3, arg4, arg5);
                        check_result!($system_call, result)
                    }
                }
            }};
        }

        // `dispatch6!(system_call_no [, optional_arguments_passed_to_handler])`
        // Converts the system call name to the handler
        macro_rules! dispatch6 {
            ($system_call:ty$(, $arg:ident)*) => {{
                try_blocks::try_block! {
                    paste::paste! {
                        let arg1 = read_arg!($system_call, a0);
                        let arg2 = read_arg!($system_call, a1);
                        let arg3 = read_arg!($system_call, a2);
                        let arg4 = read_arg!($system_call, a3);
                        let arg5 = read_arg!($system_call, a4);
                        let arg6 = read_arg!($system_call, a5);
                        trace_call!($system_call, arg1, arg2, arg3, arg4, arg5, arg6);
                        let result = self.[<handle_$system_call>]($($arg,)* arg1, arg2, arg3, arg4, arg5, arg6);
                        check_result!($system_call, result)
                    }
                }
            }};
        }

        // `dispatch7!(system_call_no [, optional_arguments_passed_to_handler])`
        // Converts the system call name to the handler
        #[expect(
            unused_macros,
            reason = "There is no system call handler with 7 parameters yet"
        )]
        macro_rules! dispatch7 {
            ($system_call:ty$(, $arg:ident)*) => {{
                try_blocks::try_block! {
                    paste::paste! {
                        let arg1 = read_arg!($system_call, a0);
                        let arg2 = read_arg!($system_call, a1);
                        let arg3 = read_arg!($system_call, a2);
                        let arg4 = read_arg!($system_call, a3);
                        let arg5 = read_arg!($system_call, a4);
                        let arg6 = read_arg!($system_call, a5);
                        let arg7 = read_arg!($system_call, a6);
                        trace_call!($system_call, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                        let result = self.[<handle_$system_call>]($($arg,)* arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                        check_result!($system_call, result)
                    }
                }
            }};
        }

        // Programs targeting a Linux kernel pass the system call number in register a7
        let system_call_no = core.hart.xregisters.read(registers::a7);

        let result = match system_call_no {
            GETCWD => dispatch2!(getcwd, core),
            OPENAT => dispatch0!(openat),
            WRITE => dispatch3!(write, core, hooks),
            WRITEV => dispatch3!(writev, core, hooks),
            PPOLL => dispatch2!(ppoll, core),
            READLINKAT => dispatch0!(readlinkat),
            EXIT | EXITGROUP => dispatch1!(exit),
            SET_TID_ADDRESS => dispatch1!(set_tid_address, core),
            TKILL => dispatch2!(tkill),
            SIGALTSTACK => dispatch2!(sigaltstack, core),
            RT_SIGACTION => dispatch4!(rt_sigaction, core),
            RT_SIGPROCMASK => dispatch4!(rt_sigprocmask, core),
            BRK => dispatch0!(brk),
            MMAP => dispatch6!(mmap, core),
            MPROTECT => dispatch3!(mprotect, core),
            MUNMAP => dispatch2!(munmap, core),
            MADVISE => dispatch0!(madvise),
            GETRANDOM => dispatch2!(getrandom, core),
            CLOCK_GETTIME => dispatch2!(clock_gettime, core),
            GETTIMEOFDAY => dispatch2!(gettimeofday, core),
            SBI_FIRMWARE_TEZOS => return on_tezos(core),
            _ => Err(Error::NoSystemCall),
        };

        match result {
            Err(Error::NoSystemCall) => {
                core.hart
                    .xregisters
                    .write_system_call_error(Error::NoSystemCall);
                false
            }

            Err(error) => {
                core.hart.xregisters.write_system_call_error(error);
                true
            }

            Ok(continue_eval) => continue_eval,
        }
    }

    /// Handle `set_tid_address` system call.
    ///
    /// See: <https://www.man7.org/linux/man-pages/man2/set_tid_address.2.html>
    fn handle_set_tid_address(
        &mut self,
        _: &mut MachineCoreState<impl MemoryConfig, M>,
        tid_address: VirtAddr,
    ) -> Result<u64, Error>
    where
        M: ManagerRead + ManagerWrite,
    {
        // NOTE: `set_tid_address` is mostly important for when a thread terminates. As we don't
        // really support threading yet, we only save the address and do nothing else.
        // In the future, when we add threading, this system call needs to be implemented to
        // support informing other (waiting) threads of termination.

        self.tid_address.write(tid_address);
        // The caller expects the Thread ID to be returned
        Ok(MAIN_THREAD_ID)
    }

    fn handle_exit(
        &mut self,
        status: parameters::ExitStatus,
    ) -> Result<parameters::SystemCallResultExecution, Infallible>
    where
        M: ManagerReadWrite,
    {
        self.exit_code = status.exit_code();
        self.exited = true;

        Ok(parameters::SystemCallResultExecution {
            result: status.exit_code(),
            control_flow: false,
        })
    }

    /// Handle `sigaltstack` system call. The new signal stack configuration is discarded. If the
    /// old signal stack configuration is requested, it will be zeroed out.
    fn handle_sigaltstack(
        &mut self,
        core: &mut MachineCoreState<impl MemoryConfig, M>,
        _: u64,
        old: parameters::SignalAction,
    ) -> Result<u64, Error>
    where
        M: ManagerReadWrite,
    {
        /// `sizeof(struct sigaltstack)` on the Kernel side
        const SIZE_SIGALTSTACK: usize = 24;

        if let Some(old) = old.address() {
            core.main_memory.write(old, [0u8; SIZE_SIGALTSTACK])?;
        }

        // Return 0 as an indicator of success
        Ok(0)
    }

    /// Handle `rt_sigaction` system call. This does nothing effectively. It does not support
    /// retrieving the previous handler for a signal - it just zeroes out the memory.
    ///
    /// See: <https://www.man7.org/linux/man-pages/man2/rt_sigaction.2.html>
    fn handle_rt_sigaction(
        &mut self,
        core: &mut MachineCoreState<impl MemoryConfig, M>,
        _: u64,
        _: u64,
        old: parameters::SignalAction,
        _: parameters::SigsetTSizeEightBytes,
    ) -> Result<u64, Error>
    where
        M: ManagerReadWrite,
    {
        /// `sizeof(struct sigaction)` on the Kernel side
        const SIZE_SIGACTION: usize = 32;

        if let Some(old) = old.address() {
            // As we don't store the previous signal handler, we just zero out the memory
            core.main_memory.write(old, [0u8; SIZE_SIGACTION])?;
        }

        // Return 0 as an indicator of success
        Ok(0)
    }

    /// Handle `rt_sigprocmask` system call. This does nothing effectively. If the previous mask is
    /// requested, it will simply be zeroed out.
    fn handle_rt_sigprocmask(
        &mut self,
        core: &mut MachineCoreState<impl MemoryConfig, M>,
        _: u64,
        _: u64,
        old: parameters::SignalAction,
        _: parameters::SigsetTSizeEightBytes,
    ) -> Result<u64, Error>
    where
        M: ManagerReadWrite,
    {
        if let Some(old) = old.address() {
            // As we don't store the previous mask, we just zero out the memory
            core.main_memory
                .write(old, [0u8; parameters::SIGSET_SIZE as usize])?;
        }

        // Return 0 as an indicator of success
        Ok(0)
    }

    /// Handle `tkill` system call. As there is only one thread at the moment, this system call
    /// will return an error if the thread ID is not the main thread ID.
    fn handle_tkill(
        &mut self,
        _: parameters::MainThreadId,
        signal: parameters::Signal,
    ) -> Result<parameters::SystemCallResultExecution, Infallible>
    where
        M: ManagerReadWrite,
    {
        // Indicate that we have exited
        self.exited = true;
        self.exit_code = signal.exit_code();

        // Return 0 as an indicator of success, even if this might not actually be used
        Ok(parameters::SystemCallResultExecution {
            result: 0,
            control_flow: false,
        })
    }

    /// Handle `clock_gettime` system call. Fills the timespec structure with zeros.
    ///
    /// See: <https://www.man7.org/linux/man-pages/man2/clock_gettime.2.html>
    fn handle_clock_gettime(
        &mut self,
        core: &mut MachineCoreState<impl MemoryConfig, M>,
        _clockid: u64,
        tp: u64,
    ) -> Result<u64, Error>
    where
        M: ManagerReadWrite,
    {
        // Size of struct timespec (8 bytes for tv_sec + 8 bytes for tv_nsec)
        const TIMESPEC_SIZE: usize = 16;

        // Write zeros to the timespec structure
        if tp != 0 {
            core.main_memory.write(tp, [0u8; TIMESPEC_SIZE])?;
        } else {
            return Err(Error::InvalidArgument);
        }

        // Return 0 as an indicator of success
        Ok(0)
    }

    /// Handle `gettimeofday` system call. Fills the timeval and timezone structures with zeros.
    ///
    /// See: <https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html>
    fn handle_gettimeofday(
        &mut self,
        core: &mut MachineCoreState<impl MemoryConfig, M>,
        tv: u64,
        tz: u64,
    ) -> Result<u64, Error>
    where
        M: ManagerReadWrite,
    {
        // Size of struct timeval (8 bytes for tv_sec + 8 bytes for tv_usec)
        const TIMEVAL_SIZE: usize = 16;

        // Size of struct timezone (4 bytes for tz_minuteswest + 4 bytes for tz_dsttime)
        const TIMEZONE_SIZE: usize = 8;

        // Write zeros to the timeval structure if it's not NULL
        if tv != 0 {
            core.main_memory.write(tv, [0u8; TIMEVAL_SIZE])?;
        }

        // Write zeros to the timezone structure if it's not NULL
        if tz != 0 {
            core.main_memory.write(tz, [0u8; TIMEZONE_SIZE])?;
        }

        // Return 0 as an indicator of success
        Ok(0)
    }
}

impl<M: ManagerClone> Clone for SupervisorState<M> {
    fn clone(&self) -> Self {
        Self {
            tid_address: self.tid_address.clone(),
            exited: self.exited,
            exit_code: self.exit_code,
            program: self.program.clone(),
            stack_guard: self.stack_guard.clone(),
            heap: self.heap.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::array;
    use std::num::NonZeroU64;

    use rand::Rng;

    use super::parameters::AddressHint;
    use super::parameters::Backend;
    use super::parameters::Flags;
    use super::parameters::Visibility;
    use super::*;
    use crate::backend_test;
    use crate::machine_state::memory::M4K;
    use crate::pvm::linux::error::Error;
    use crate::pvm::linux::parameters::NoFileDescriptor;
    use crate::pvm::linux::parameters::Zero;

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

        let mut manager = F::manager();
        let mut machine_state = MachineCoreState::<MemLayout, _>::new(&mut manager);
        let mut supervisor_state = SupervisorState::new(&mut manager);

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

        let mut manager = F::manager();
        let mut machine_state = MachineCoreState::<MemLayout, _>::new(&mut manager);
        machine_state.reset();

        // Make sure everything is readable and writable. Otherwise, we'd get access faults.
        machine_state
            .main_memory
            .protect_pages(0, MemLayout::TOTAL_BYTES, Permissions::ReadWrite)
            .unwrap();

        for fd in [0i32, 1, 2] {
            let mut supervisor_state = SupervisorState::new(&mut manager);

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

        let mut manager = F::manager();
        let mut machine_state = MachineCoreState::<MemLayout, _>::new(&mut manager);
        machine_state.reset();

        // Make sure everything is readable and writable. Otherwise, we'd get access faults.
        machine_state
            .main_memory
            .protect_pages(0, MemLayout::TOTAL_BYTES, Permissions::ReadWrite)
            .unwrap();

        let mut supervisor_state = SupervisorState::new(&mut manager);

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

    // Check that the `sigaltstack` system call can accept 0 for the `old` parameter.
    backend_test!(sigaltstack_zero_parameter, F, {
        type MemLayout = M4K;

        let mut manager = F::manager();
        let mut machine_state = MachineCoreState::<MemLayout, _>::new(&mut manager);
        let mut supervisor_state = SupervisorState::new(&mut manager);

        // System call number
        machine_state
            .hart
            .xregisters
            .write(registers::a7, SIGALTSTACK);

        // Zero old signal
        machine_state.hart.xregisters.write(registers::a0, 0u64);

        // Perform the system call
        let result = supervisor_state.handle_system_call(
            &mut machine_state,
            &mut PvmHooks::default(),
            default_on_tezos_handler,
        );
        assert!(result);
    });

    // Check that the `rt_sigaction system call can accept 0 for the `old` parameter.
    backend_test!(rt_sigaction_zero_parameter, F, {
        type MemLayout = M4K;

        let mut manager = F::manager();
        let mut machine_state = MachineCoreState::<MemLayout, _>::new(&mut manager);
        let mut supervisor_state = SupervisorState::new(&mut manager);

        // System call number
        machine_state
            .hart
            .xregisters
            .write(registers::a7, RT_SIGACTION);

        machine_state.hart.xregisters.write(registers::a0, 0u64);

        machine_state.hart.xregisters.write(registers::a1, 0u64);

        // Zero old signal
        machine_state.hart.xregisters.write(registers::a2, 0u64);

        // Size of sigset_t
        machine_state.hart.xregisters.write(registers::a3, 8u64);

        // Perform the system call
        let result = supervisor_state.handle_system_call(
            &mut machine_state,
            &mut PvmHooks::default(),
            default_on_tezos_handler,
        );
        assert!(result);
    });

    // Check that the `rt_sigprocmask system call can accept 0 for the `old` parameter.
    backend_test!(rt_sigprocmask_zero_parameter, F, {
        type MemLayout = M4K;

        let mut manager = F::manager();
        let mut machine_state = MachineCoreState::<MemLayout, _>::new(&mut manager);
        let mut supervisor_state = SupervisorState::new(&mut manager);

        // System call number
        machine_state
            .hart
            .xregisters
            .write(registers::a7, RT_SIGPROCMASK);

        machine_state.hart.xregisters.write(registers::a0, 0u64);

        machine_state.hart.xregisters.write(registers::a1, 0u64);

        // Zero old signal
        machine_state.hart.xregisters.write(registers::a2, 0u64);

        // Size of sigset_t
        machine_state.hart.xregisters.write(registers::a3, 8u64);

        // Perform the system call
        let result = supervisor_state.handle_system_call(
            &mut machine_state,
            &mut PvmHooks::default(),
            default_on_tezos_handler,
        );
        assert!(result);
    });

    // Check that the `clock_gettime` system call fills the timespec with zeros.
    backend_test!(clock_gettime_fills_with_zeros, F, {
        type MemLayout = M4K;

        let mut manager = F::manager();
        let mut machine_state = MachineCoreState::<MemLayout, _>::new(&mut manager);
        machine_state.reset();

        // Make sure everything is readable and writable. Otherwise, we'd get access faults.
        machine_state
            .main_memory
            .protect_pages(0, MemLayout::TOTAL_BYTES, Permissions::ReadWrite)
            .unwrap();

        let mut supervisor_state = SupervisorState::new(&mut manager);

        // System call number
        machine_state
            .hart
            .xregisters
            .write(registers::a7, CLOCK_GETTIME);

        // Any clock ID (we ignore it anyway)
        machine_state.hart.xregisters.write(registers::a0, 1u64);

        // Timespec pointer (must be non-zero)
        let timespec_ptr = 0x100;

        // Fill the timespec struct with non-zero values to verify they are zeroed
        machine_state
            .main_memory
            .write(timespec_ptr, [0xFF; 16])
            .unwrap();

        machine_state
            .hart
            .xregisters
            .write(registers::a1, timespec_ptr);

        // Perform the system call
        let result = supervisor_state.handle_system_call(
            &mut machine_state,
            &mut PvmHooks::default(),
            default_on_tezos_handler,
        );
        assert!(result);

        // Verify that a0 contains 0 (success)
        let ret = machine_state.hart.xregisters.read(registers::a0);
        assert_eq!(ret, 0);

        // Verify that the timespec is zeroed out
        let timespec = machine_state
            .main_memory
            .read::<[u8; 16]>(timespec_ptr)
            .unwrap();
        assert_eq!(timespec, [0u8; 16]);
    });

    // Check that the `gettimeofday` system call fills the timeval and timezone with zeros.
    backend_test!(gettimeofday_fills_with_zeros, F, {
        type MemLayout = M4K;

        let mut manager = F::manager();
        let mut machine_state = MachineCoreState::<MemLayout, _>::new(&mut manager);
        machine_state.reset();

        // Make sure everything is readable and writable. Otherwise, we'd get access faults.
        machine_state
            .main_memory
            .protect_pages(0, MemLayout::TOTAL_BYTES, Permissions::ReadWrite)
            .unwrap();

        let mut supervisor_state = SupervisorState::new(&mut manager);

        // System call number
        machine_state
            .hart
            .xregisters
            .write(registers::a7, GETTIMEOFDAY);

        // Timeval pointer
        let timeval_ptr = 0x100;

        // Fill the timeval struct with non-zero values to verify they are zeroed
        machine_state
            .main_memory
            .write(timeval_ptr, [0xFF; 16])
            .unwrap();

        machine_state
            .hart
            .xregisters
            .write(registers::a0, timeval_ptr);

        // Timezone pointer
        let timezone_ptr = 0x200;

        // Fill the timezone struct with non-zero values to verify they are zeroed
        machine_state
            .main_memory
            .write(timezone_ptr, [0xFF; 8])
            .unwrap();

        machine_state
            .hart
            .xregisters
            .write(registers::a1, timezone_ptr);

        // Perform the system call
        let result = supervisor_state.handle_system_call(
            &mut machine_state,
            &mut PvmHooks::default(),
            default_on_tezos_handler,
        );
        assert!(result);

        // Verify that a0 contains 0 (success)
        let ret = machine_state.hart.xregisters.read(registers::a0);
        assert_eq!(ret, 0);

        // Verify that the timeval is zeroed out
        let timeval = machine_state
            .main_memory
            .read::<[u8; 16]>(timeval_ptr)
            .unwrap();
        assert_eq!(timeval, [0u8; 16]);

        // Verify that the timezone is zeroed out
        let timezone = machine_state
            .main_memory
            .read::<[u8; 8]>(timezone_ptr)
            .unwrap();
        assert_eq!(timezone, [0u8; 8]);
    });

    // Check that `mmap` returns `Error::NoMemory` when allocate_and_protect_pages fails.
    backend_test!(mmap_returns_enomem_when_allocation_fails, F, {
        type MemLayout = M4K;

        let mut manager = F::manager();
        let mut machine_state = MachineCoreState::<MemLayout, _>::new(&mut manager);
        machine_state.reset();

        // Allocate all memory to ensure subsequent allocations will fail
        machine_state
            .main_memory
            .allocate_and_protect_pages(
                Some(0),
                MemLayout::TOTAL_BYTES,
                Permissions::ReadWrite,
                true,
            )
            .unwrap();

        let mut supervisor_state = SupervisorState::new(&mut manager);

        // Set up necessary registers for mmap
        machine_state
            .hart
            .xregisters
            .write(registers::a4, -1i64 as u64); // NoFileDescriptor
        machine_state.hart.xregisters.write(registers::a5, 0); // Zero offset

        // Case 1: Test with AddressHint::Hint
        {
            let addr = 0; // will be ignored with AddressHint::Hint
            let length = NonZeroU64::new(PAGE_SIZE.get() * 4).unwrap(); // Arbitrary non-zero size
            let perms = Permissions::ReadWrite;
            let flags = Flags {
                addr_hint: AddressHint::Hint,
                visibility: Visibility::Private,
                backend: Backend::None,
            };

            // Call the function under test
            let result = supervisor_state.handle_mmap(
                &mut machine_state,
                addr.into(),
                length,
                perms,
                flags,
                NoFileDescriptor,
                Zero,
            );

            // Verify we get the expected error
            assert_eq!(result, Err(Error::NoMemory));
        }

        // Case 2: Test with AddressHint::Fixed
        {
            let addr = PAGE_SIZE.get() * 10; // Some arbitrary page-aligned address
            let length = NonZeroU64::new(PAGE_SIZE.get() * 4).unwrap();
            let perms = Permissions::ReadWrite;
            let flags = Flags {
                addr_hint: AddressHint::Fixed {
                    allow_replace: false,
                },
                visibility: Visibility::Private,
                backend: Backend::None,
            };

            // Call the function under test
            let result = supervisor_state.handle_mmap(
                &mut machine_state,
                VirtAddr::new(addr),
                length,
                perms,
                flags,
                NoFileDescriptor,
                Zero,
            );

            // Verify we get the expected error
            assert_eq!(result, Err(Error::NoMemory));
        }
    });
}
