// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use core::num::NonZeroU64;
use std::fmt;

use arbitrary_int::u7;

use super::MAIN_THREAD_ID;
use super::error::Error;

/// Size of the `sigset_t` type in bytes
///
/// As we're building a 64-bit system, the sigset should be 64-bit wide as well.
pub const SIGSET_SIZE: u64 = 8;

/// A type coupling the result of the system call with how the program should continue.
#[derive(Debug, Clone, Copy)]
pub struct SystemCallResultExecution {
    pub result: u64,
    pub control_flow: bool,
}

impl<T: Into<u64>> From<T> for SystemCallResultExecution {
    fn from(value: T) -> Self {
        // The default action is to continue execution after the system call. In cases where the
        // execution should halt, this should be specified.
        SystemCallResultExecution {
            result: value.into(),
            control_flow: true,
        }
    }
}

/// The status of the program upon exit. While the C standard specifies that this should be equal
/// to EXIT_SUCCESS or EXIT_FAILURE, this is rarely enforced, and can be any int - where `0`
/// indicates success.
#[derive(Debug, Clone, Copy)]
pub struct ExitStatus(u64);

impl From<ExitStatus> for u64 {
    fn from(value: ExitStatus) -> Self {
        value.0
    }
}

impl From<u64> for ExitStatus {
    fn from(value: u64) -> Self {
        ExitStatus(value)
    }
}

impl ExitStatus {
    /// Extract the exit code from the status stored in this type
    pub fn exit_code(&self) -> u64 {
        self.0
    }
}

/// Known to be a valid thread ID. As we currently only support single-thread execution, this will
/// be the main thread.
#[derive(Debug, Clone, Copy)]
pub struct MainThreadId;

impl TryFrom<u64> for MainThreadId {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        // We only support exiting the main thread
        if value != MAIN_THREAD_ID {
            return Err(Error::Search);
        }
        Ok(MainThreadId)
    }
}

/// A signal passed to a thread, see `tkill(2)`
#[derive(Debug, Clone, Copy)]
pub struct Signal(u7);

impl TryFrom<u64> for Signal {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(Signal(u7::try_new(value.try_into()?)?))
    }
}

impl Signal {
    /// Extract the exit code from the signal stored in this type
    pub fn exit_code(&self) -> u64 {
        // Setting bit 2^7 of the exit code indicates that the process was killed by a signal
        const EXIT_BY_SIGNAL: u8 = 1 << 7;

        (EXIT_BY_SIGNAL | self.0.value()) as u64
    }
}

/// An address of a signal action in the VM memory
#[derive(Clone, Copy)]
pub struct SignalAction(pub Option<NonZeroU64>);

impl fmt::Debug for SignalAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}", self.0.map(|nz| nz.get()).unwrap_or(0))
    }
}

impl From<u64> for SignalAction {
    fn from(value: u64) -> Self {
        SignalAction(NonZeroU64::new(value))
    }
}

impl SignalAction {
    /// Extract the address of the signal action in the VM memory
    pub fn address(&self) -> Option<u64> {
        self.0.map(|nz| nz.get())
    }
}

/// A valid size of `sigset_t`
#[derive(Clone, Copy, Debug)]
pub struct SigsetTSizeEightBytes;

impl TryFrom<u64> for SigsetTSizeEightBytes {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        // As we're implementing a 64-bit system, the size of `sigset_t` must be 8 bytes.
        // This is an assumption which is used in the remainder of the function body.
        match value {
            SIGSET_SIZE => Ok(SigsetTSizeEightBytes),
            _ => Err(Error::InvalidArgument),
        }
    }
}

/// A valid file descriptor, see write(2)
#[derive(Clone, Copy, Debug)]
pub enum FileDescriptorWriteable {
    StandardOutput,
    StandardError,
}

impl TryFrom<u64> for FileDescriptorWriteable {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        // Check if the file descriptor is valid and can be written to.
        // In our case, it's just standard output (1) and standard error (2).
        match value {
            1 => Ok(FileDescriptorWriteable::StandardOutput),
            2 => Ok(FileDescriptorWriteable::StandardError),
            _ => Err(Error::BadFileDescriptor),
        }
    }
}

/// The number (count) of file descriptors
#[derive(Clone, Copy)]
pub struct FileDescriptorCount(u64);

impl FileDescriptorCount {
    /// Extract the file descriptor count as a [`u64`].
    pub fn count(&self) -> u64 {
        self.0
    }
}

impl fmt::Debug for FileDescriptorCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Hard limit on the number of file descriptors that a system call can work with
///
/// We also use this constant to implictly limit how much memory can be associated with a system
/// call. For example, `ppoll` takes a pointer to an array of `struct pollfd`. If we don't limit
/// the length of that array, then we might read an arbitrary amount of memory. This impacts the
/// proof size dramatically as everything read would also be in the proof.
const RLIMIT_NOFILE: u64 = 512;

impl TryFrom<u64> for FileDescriptorCount {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        // Enforce a limit on the number of file descriptors to prevent proof-size explosion.
        // This is akin to enforcing RLIMIT_NOFILE in a real system.
        if value > RLIMIT_NOFILE {
            return Err(Error::InvalidArgument);
        }
        Ok(FileDescriptorCount(value))
    }
}

/// Definitely zero
#[derive(Debug, Clone, Copy)]
pub struct Zero;

impl TryFrom<u64> for Zero {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        if value != 0 {
            return Err(Error::InvalidArgument);
        }

        Ok(Zero)
    }
}

/// Special file descriptor meaning no file descriptor
#[derive(Debug, Clone, Copy)]
pub struct NoFileDescriptor;

impl TryFrom<u64> for NoFileDescriptor {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        if value != -1i64 as u64 {
            return Err(Error::BadFileDescriptor);
        }

        Ok(NoFileDescriptor)
    }
}

/// Visibility of a memory mapping
#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    /// Only visible to the current task
    Private,

    // Shared with other tasks
    Shared,
}

/// Backing of a memory mapping
#[derive(Debug, Clone, Copy)]
pub enum Backend {
    /// Just memory
    None,

    /// File-backend memory
    File,
}

/// Hint on how to interpret the address argument when memory mapping
#[derive(Debug, Clone, Copy)]
pub enum AddressHint {
    /// May ignore the address hint
    Hint,

    /// Fixed address
    Fixed { allow_replace: bool },
}

/// Memory mapping request flags
#[derive(Debug, Clone)]
pub struct Flags {
    /// Visibility of the memory mapping
    pub visibility: Visibility,

    /// Memory backing
    pub backend: Backend,

    /// How to interpret the address hint
    pub addr_hint: AddressHint,
}

impl TryFrom<u64> for Flags {
    type Error = Error;

    fn try_from(mut flags: u64) -> Result<Self, Self::Error> {
        // Check if a bit is set, and clear it if it is
        let mut probe_and_clear = |mask: u64| {
            let r = flags & mask == mask;
            flags &= !mask;
            r
        };

        let visibility = {
            const MAP_SHARED: u64 = 0x1;
            const MAP_PRIVATE: u64 = 0x2;

            let shared = probe_and_clear(MAP_SHARED);

            // `MAP_SHARED_VALIDATE` translates to `MAP_PRIVATE | MAP_SHARED` for some reason. We
            // must make sure that private is requested only when `MAP_SHARED` is not set.
            let private = probe_and_clear(MAP_PRIVATE) && !shared;

            if private {
                Visibility::Private
            } else if shared {
                Visibility::Shared
            } else {
                return Err(Error::InvalidArgument);
            }
        };

        let backend = {
            const MAP_ANON: u64 = 0x20;

            if probe_and_clear(MAP_ANON) {
                Backend::None
            } else {
                Backend::File
            }
        };

        let addr_hint = {
            const MAP_FIXED: u64 = 0x10;
            const MAP_FIXED_NOREPLACE: u64 = 0x100000;

            if probe_and_clear(MAP_FIXED) {
                AddressHint::Fixed {
                    allow_replace: !probe_and_clear(MAP_FIXED_NOREPLACE),
                }
            } else if probe_and_clear(MAP_FIXED_NOREPLACE) {
                AddressHint::Fixed {
                    allow_replace: false,
                }
            } else {
                AddressHint::Hint
            }
        };

        // `MAP_NORESERVE` does nothing for us as we have no swap
        const MAP_NORESERVE: u64 = 0x4000;
        probe_and_clear(MAP_NORESERVE);

        // If there are other bits set, that means we likely don't support them
        if flags != 0 {
            return Err(Error::InvalidArgument);
        }

        Ok(Self {
            visibility,
            backend,
            addr_hint,
        })
    }
}
