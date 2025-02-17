// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>fds
//
// SPDX-License-Identifier: MIT

//! Implementations of system calls related to file descriptors

use super::{SupervisorState, error::Error};
use crate::{
    machine_state::{MachineCoreState, main_memory::MainMemoryLayout, registers},
    state_backend::{ManagerBase, ManagerReadWrite},
};

/// Hard limit on the number of file descriptors that a system call can work with
///
/// We also use this constant to implictly limit how much memory can be associated with a system
/// call. For example, `ppoll` takes a pointer to an array of `struct pollfd`. If we don't limit
/// the length of that array, then we might read an arbitrary amount of memory. This impacts the
/// proof size dramatically as everything read would also be in the proof.
const RLIMIT_NOFILE: u64 = 512;

impl<M: ManagerBase> SupervisorState<M> {
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
