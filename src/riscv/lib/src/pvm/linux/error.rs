// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::convert::Infallible;
use std::num::TryFromIntError;

use arbitrary_int::TryNewError;

use crate::machine_state::memory::BadMemoryAccess;
use crate::machine_state::memory::MemoryGovernanceError;
use crate::machine_state::registers::XRegisters;
use crate::machine_state::registers::XValue;
use crate::machine_state::registers::a0;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerWrite;

/// Linux system call error codes
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(i32)]
pub enum Error {
    /// Process or thread not found
    ///
    /// See [`ESRCH`](https://github.com/torvalds/linux/blob/0ad2507d5d93f39619fc42372c347d6006b64319/include/uapi/asm-generic/errno-base.h#L7)
    Search = 3,

    /// File descriptor is bad
    ///
    /// See [`EBADF`](https://github.com/torvalds/linux/blob/0ad2507d5d93f39619fc42372c347d6006b64319/include/uapi/asm-generic/errno-base.h#L13)
    BadFileDescriptor = 9,

    /// Access denied
    ///
    /// See [`EACCESS`](https://github.com/torvalds/linux/blob/0ad2507d5d93f39619fc42372c347d6006b64319/include/uapi/asm-generic/errno-base.h#L17)
    Access = 13,

    /// Out of memory
    ///
    /// See [`ENOMEM`](https://github.com/torvalds/linux/blob/0ad2507d5d93f39619fc42372c347d6006b64319/include/uapi/asm-generic/errno-base.h#L16)
    NoMemory = 12,

    /// Fault during memory access
    ///
    /// See [`EFAULT`](https://github.com/torvalds/linux/blob/0ad2507d5d93f39619fc42372c347d6006b64319/include/uapi/asm-generic/errno-base.h#L18)
    Fault = 14,

    /// Invalid argument
    ///
    /// See [`EINVAL`](https://github.com/torvalds/linux/blob/0ad2507d5d93f39619fc42372c347d6006b64319/include/uapi/asm-generic/errno-base.h#L26)
    InvalidArgument = 22,

    /// Out of range
    ///
    /// See [`ERANGE`](https://github.com/torvalds/linux/blob/0ad2507d5d93f39619fc42372c347d6006b64319/include/uapi/asm-generic/errno-base.h#L38)
    Range = 34,

    /// System call is not supported
    ///
    /// See [`ENOSYS`](https://github.com/torvalds/linux/blob/0ad2507d5d93f39619fc42372c347d6006b64319/tools/include/uapi/asm-generic/errno.h#L18)
    NoSystemCall = 38,
}

impl Error {
    /// Turn into an error code that can be returned via an integer register.
    pub fn into_xvalue(self) -> XValue {
        // The discriminant matches the error code
        let error_code = -(self as i32);
        error_code as u64
    }
}

impl From<Infallible> for Error {
    fn from(infallible: Infallible) -> Self {
        match infallible {}
    }
}

impl From<BadMemoryAccess> for Error {
    fn from(BadMemoryAccess: BadMemoryAccess) -> Self {
        Self::Fault
    }
}

impl From<MemoryGovernanceError> for Error {
    fn from(MemoryGovernanceError: MemoryGovernanceError) -> Self {
        Self::NoMemory
    }
}

impl From<TryFromIntError> for Error {
    fn from(_: TryFromIntError) -> Self {
        Error::InvalidArgument
    }
}

impl From<TryNewError> for Error {
    fn from(_: TryNewError) -> Self {
        Error::InvalidArgument
    }
}

impl<M: ManagerBase> XRegisters<M> {
    /// Write the error result of a system call to the return value register.
    pub fn write_system_call_error(&mut self, error: Error)
    where
        M: ManagerWrite,
    {
        self.write(a0, error.into_xvalue());
    }
}
