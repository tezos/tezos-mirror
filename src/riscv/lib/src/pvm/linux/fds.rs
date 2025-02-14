// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>fds
//
// SPDX-License-Identifier: MIT

//! Implementations of system calls related to file descriptors

use super::{SupervisorState, error::Error};
use crate::{
    machine_state::{MachineCoreState, main_memory::MainMemoryLayout, registers},
    pvm::{PvmHooks, linux::PAGE_SIZE},
    state_backend::{ManagerBase, ManagerRead, ManagerReadWrite},
};

/// Hard limit on the number of file descriptors that a system call can work with
///
/// We also use this constant to implictly limit how much memory can be associated with a system
/// call. For example, `ppoll` takes a pointer to an array of `struct pollfd`. If we don't limit
/// the length of that array, then we might read an arbitrary amount of memory. This impacts the
/// proof size dramatically as everything read would also be in the proof.
const RLIMIT_NOFILE: u64 = 512;

impl<M: ManagerBase> SupervisorState<M> {
    /// Write to a file descriptor.
    fn write_to_fd(
        &self,
        core: &mut MachineCoreState<impl MainMemoryLayout, M>,
        hooks: &mut PvmHooks,
        fd: i32,
        addr: u64,
        length: u64,
    ) -> Result<u64, Error>
    where
        M: ManagerRead,
    {
        // Limit how much data we can write to prevent proof-size explosion
        let length = length.min(PAGE_SIZE);

        // Check if the file desciprtor is valid and can be written to.
        // In our case, it's just standard output (1) and standard error (2).
        let mut write_data = match fd {
            1 | 2 => |data| {
                for &byte in data {
                    (hooks.putchar_hook)(byte);
                }
            },
            _ => return Err(Error::BadFileDescriptor),
        };

        // TODO: RV-487: Memory mappings are not yet protected. We assume the kernel knows what
        // it's doing for now.
        let mut data = vec![0u8; length as usize];
        let Ok(()) = core.main_memory.read_all(addr, &mut data) else {
            return Err(Error::Fault);
        };

        write_data(data.as_slice());

        // Returning a positive value indicates success
        Ok(length)
    }

    /// Handle `write` system call. Currently only supports writing to standard output and standard
    /// error.
    ///
    /// See <https://man7.org/linux/man-pages/man2/write.2.html>
    pub(super) fn handle_write(
        &mut self,
        core: &mut MachineCoreState<impl MainMemoryLayout, M>,
        hooks: &mut PvmHooks,
    ) -> bool
    where
        M: ManagerReadWrite,
    {
        let fd = core.hart.xregisters.read(registers::a0) as i32;
        let addr = core.hart.xregisters.read(registers::a1);
        let length = core.hart.xregisters.read(registers::a2);

        match self.write_to_fd(core, hooks, fd, addr, length) {
            Ok(res) => core.hart.xregisters.write(registers::a0, res),
            Err(err) => core.hart.xregisters.write_system_call_error(err),
        }

        true
    }

    /// Handle `writev` system call. Writes only to the first entry of the `iovec` array which has
    /// data to be written. Otherwise it works the same as `write`.
    ///
    /// See <https://man7.org/linux/man-pages/man3/writev.3p.html>
    pub(super) fn handle_writev(
        &mut self,
        core: &mut MachineCoreState<impl MainMemoryLayout, M>,
        hooks: &mut PvmHooks,
    ) -> bool
    where
        M: ManagerReadWrite,
    {
        let fd = core.hart.xregisters.read(registers::a0) as i32;
        let iovec = core.hart.xregisters.read(registers::a1);
        let len = core.hart.xregisters.read(registers::a2);

        if len < 1 {
            core.hart.xregisters.write(registers::a0, 0);
            return true;
        }

        // `iovec` is a `struct iovec[]`.
        //
        // ```
        // struct iovec {
        //   void* base;
        //   size_t len;
        // };
        // ```

        /// `sizeof(struct iovec)`
        const SIZE_IOVEC: u64 = 16;

        /// `offsetof(struct iovec, base)`
        const OFFSET_BASE: u64 = 0;

        /// `offsetof(struct iovec, len)`
        const OFFSET_LEN: u64 = 8;

        let (addr, length) = 'search: {
            // Limit the number of entries through which we iterate to prevent proof-size explosion
            let max_entries = len.min(PAGE_SIZE / SIZE_IOVEC);

            for item in 0..max_entries {
                let struct_addr = SIZE_IOVEC.wrapping_mul(item).wrapping_add(iovec);
                let struct_addr_base = struct_addr.wrapping_add(OFFSET_BASE);
                let struct_addr_len = struct_addr.wrapping_add(OFFSET_LEN);

                // TODO: RV-487: Memory mappings are not yet protected. We assume the kernel knows
                // what it's doing for now.
                let Ok(addr) = core.main_memory.read(struct_addr_base) else {
                    core.hart.xregisters.write_system_call_error(Error::Fault);
                    return true;
                };

                // TODO: RV-487: Memory mappings are not yet protected. We assume the kernel knows
                // what it's doing for now.
                let Ok(length) = core.main_memory.read(struct_addr_len) else {
                    core.hart.xregisters.write_system_call_error(Error::Fault);
                    return true;
                };

                if length > 0 {
                    // Once we found a non-zero data segment to write, we break out of the loop
                    break 'search (addr, length);
                }
            }

            // We haven't found any data to write
            core.hart.xregisters.write(registers::a0, 0);
            return true;
        };

        match self.write_to_fd(core, hooks, fd, addr, length) {
            Ok(res) => core.hart.xregisters.write(registers::a0, res),
            Err(err) => core.hart.xregisters.write_system_call_error(err),
        }

        true
    }

    /// Handle `ppoll` system call in a way that only satisfies the usage by Musl's and the Rust
    /// standard library's initialisation code. It supports only very basic functionality. For
    /// example, the `timeout` parameter is ignored entirely.
    ///
    /// See: <https://man7.org/linux/man-pages/man2/poll.2.html>
    pub(super) fn handle_ppoll(
        &mut self,
        core: &mut MachineCoreState<impl MainMemoryLayout, M>,
    ) -> bool
    where
        M: ManagerReadWrite,
    {
        let fd_ptrs = core.hart.xregisters.read(registers::a0);
        let num_fds = core.hart.xregisters.read(registers::a1);

        // Enforce a limit on the number of file descriptors to prevent proof-size explosion.
        // This is akin to enforcing RLIMIT_NOFILE in a real system.
        if num_fds > RLIMIT_NOFILE {
            core.hart
                .xregisters
                .write_system_call_error(Error::InvalidArgument);
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
            core.hart.xregisters.write_system_call_error(Error::Fault);
            return true;
        };

        // Only support the initial ppoll that Musl and Rust's init code issue
        if !fds.iter().all(|fd| [0, 1, 2].contains(fd)) {
            core.hart
                .xregisters
                .write_system_call_error(Error::NoSystemCall);
            return true;
        }

        for i in 0..num_fds {
            let revents_ptr = i
                .wrapping_mul(SIZE_POLLFD)
                .wrapping_add(OFFSET_REVENTS)
                .wrapping_add(fd_ptrs);
            let Ok(()) = core.main_memory.write_all(revents_ptr, &0u16.to_le_bytes()) else {
                core.hart.xregisters.write_system_call_error(Error::Fault);
                return true;
            };
        }

        // Indicate success by returning 0
        core.hart.xregisters.write(registers::a0, 0);

        true
    }
}
