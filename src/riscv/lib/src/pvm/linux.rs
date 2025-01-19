// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::{
        main_memory::{Address, MainMemoryLayout},
        mode::Mode,
        registers, CacheLayouts, MachineCoreState, MachineError, MachineState,
    },
    program::Program,
    state_backend::{
        AllocatedOf, Atom, Cell, FnManager, ManagerBase, ManagerClone, ManagerRead,
        ManagerReadWrite, ManagerWrite, Ref,
    },
};
use std::ffi::CStr;

/// Size of a memory page in bytes
pub const PAGE_SIZE: u64 = 4096;

/// Thread identifier for the main thread
const MAIN_THREAD_ID: u64 = 1;

/// Indicates a system call is not supported
const ENOSYS: i64 = 38;

/// Indicates a memory address was faulty
const EFAULT: i64 = 14;

/// Indicates an invalid argument
const EINVAL: i64 = 22;

/// System call number for `ppoll` on RISC-V
const PPOLL: u64 = 73;

/// System call number for `set_tid_address` on RISC-V
const SET_TID_ADDRESS: u64 = 96;

/// Hard limit on the number of file descriptors that a system call can work with
///
/// We also use this constant to implictly limit how much memory can be associated with a system
/// call. For example, `ppoll` takes a pointer to an array of `struct pollfd`. If we don't limit
/// the length of that array, then we might read an arbitrary amount of memory. This impacts the
/// proof size dramatically as everything read would also be in the proof.
const RLIMIT_NOFILE: u64 = 512;

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

/// Layout for the Linux supervisor state
pub type SupervisorStateLayout = Atom<u64>;

/// Linux supervisor state
pub struct SupervisorState<M: ManagerBase> {
    tid_address: Cell<u64, M>,
}

impl<M: ManagerBase> SupervisorState<M> {
    /// Bind the given allocated regions to the supervisor state.
    pub fn bind(space: AllocatedOf<SupervisorStateLayout, M>) -> Self {
        SupervisorState { tid_address: space }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<SupervisorStateLayout, F::Output> {
        self.tid_address.struct_ref::<F>()
    }

    /// Handle a Linux system call.
    pub fn handle_system_call(
        &mut self,
        core: &mut MachineCoreState<impl MainMemoryLayout, M>,
    ) -> bool
    where
        M: ManagerRead + ManagerWrite,
    {
        // We need to jump to the next instruction. The ECall instruction which triggered this
        // function is 4 byte wide.
        let pc = core.hart.pc.read().saturating_add(4);
        core.hart.pc.write(pc);

        // Programs targeting a Linux kernel pass the system call number in register a7
        let system_call_no = core.hart.xregisters.read(registers::a7);

        #[allow(clippy::single_match)]
        match system_call_no {
            PPOLL => return self.handle_ppoll(core),
            SET_TID_ADDRESS => return self.handle_set_tid_address(core),
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

        core.hart.xregisters.write(registers::a0, -ENOSYS as u64);

        false
    }

    /// Handle `set_tid_address` system call.
    ///
    /// See: https://www.man7.org/linux/man-pages/man2/set_tid_address.2.html
    fn handle_set_tid_address(
        &mut self,
        core: &mut MachineCoreState<impl MainMemoryLayout, M>,
    ) -> bool
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

    /// Handle `ppoll` system call in a way that only satisfies the usage by Musl's and the Rust
    /// standard library's initialisation code. It supports only very basic functionality. For
    /// example, the `timeout` parameter is ignored entirely.
    ///
    /// See: https://man7.org/linux/man-pages/man2/poll.2.html
    fn handle_ppoll(&mut self, core: &mut MachineCoreState<impl MainMemoryLayout, M>) -> bool
    where
        M: ManagerRead + ManagerWrite,
    {
        let fd_ptrs = core.hart.xregisters.read(registers::a0);
        let num_fds = core.hart.xregisters.read(registers::a1);

        // Enforce a limit on the number of file descriptors to prevent proof-size explosion.
        // This is akin to enforcing RLIMIT_NOFILE in a real system.
        if num_fds > RLIMIT_NOFILE {
            core.hart.xregisters.write(registers::a0, -EINVAL as u64);
            return true;
        }

        // The file descriptors are passed as `struct pollfd[]`.
        //
        // ```
        // struct pollfd {
        //     int   fd;         /* file descriptor */
        //     short events;     /* requested events */
        //     short revents;    /* returned events */
        // };
        // ```

        /// sizeof(struct pollfd)
        const SIZE_POLLFD: u64 = 8;

        /// offsetof(struct pollfd, fd)
        const OFFSET_FD: u64 = 0;

        /// offsetof(struct pollfd, revents)
        const OFFSET_REVENTS: u64 = 6;

        let Ok(fds) = (0..num_fds)
            .map(|i| {
                core.main_memory.read::<i32>(
                    i.wrapping_mul(SIZE_POLLFD)
                        .wrapping_add(OFFSET_FD)
                        .wrapping_add(fd_ptrs),
                )
            })
            .collect::<Result<Vec<_>, _>>()
        else {
            core.hart.xregisters.write(registers::a0, -EFAULT as u64);
            return true;
        };

        // Only support the initial ppoll that Musl and Rust's init code issue
        if !fds.iter().all(|fd| [0, 1, 2].contains(fd)) {
            core.hart.xregisters.write(registers::a0, -ENOSYS as u64);
            return true;
        }

        for i in 0..num_fds {
            let revents_ptr = i
                .wrapping_mul(SIZE_POLLFD)
                .wrapping_add(OFFSET_REVENTS)
                .wrapping_add(fd_ptrs);
            let Ok(()) = core.main_memory.write_all(revents_ptr, &0u16.to_le_bytes()) else {
                core.hart.xregisters.write(registers::a0, -EFAULT as u64);
                return true;
            };
        }

        // Indicate success by returning 0
        core.hart.xregisters.write(registers::a0, 0);

        true
    }
}

impl<M: ManagerClone> Clone for SupervisorState<M> {
    fn clone(&self) -> Self {
        Self {
            tid_address: self.tid_address.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend_test, create_state, machine_state::MachineCoreStateLayout, state_backend::DynArray,
    };
    use rand::Rng;

    // Check that the `set_tid_address` system call is working correctly.
    backend_test!(set_tid_address, F, {
        const MEM_BYTES: usize = 1024;
        type MemLayout = DynArray<MEM_BYTES>;

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

        let result = supervisor_state.handle_system_call(&mut machine_state);
        assert!(result);

        assert_eq!(supervisor_state.tid_address.read(), tid_address);
    });

    // Check `ppoll` system call the way it is used in Musl and Rust's initialisation code.
    backend_test!(ppoll_init_fds, F, {
        const MEM_BYTES: usize = 1024;
        type MemLayout = DynArray<MEM_BYTES>;

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

            let result = supervisor_state.handle_system_call(&mut machine_state);
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
}
