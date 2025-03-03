// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use core::num::NonZeroU64;

use arbitrary_int::u7;

use super::MAIN_THREAD_ID;
use super::error::Error;

/// Size of the `sigset_t` type in bytes
///
/// As we're building a 64-bit system, the sigset should be 64-bit wide as well.
pub const SIGSET_SIZE: u64 = 8;

/// The status of the program upon exit. While the C standard specifies that this should be equal
/// to EXIT_SUCCESS or EXIT_FAILURE, this is rarely enforeced, and can be any int - where `0`
/// indicates success
pub struct ExitStatus(u64);

impl TryFrom<u64> for ExitStatus {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(ExitStatus(value))
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
pub struct SignalAction(pub NonZeroU64);

impl TryFrom<u64> for SignalAction {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        match NonZeroU64::new(value) {
            Some(old_action) => Ok(SignalAction(old_action)),
            _ => Err(Error::InvalidArgument),
        }
    }
}

impl SignalAction {
    /// Extract the address of the signal action in the VM memory
    pub fn address(&self) -> u64 {
        self.0.get()
    }
}

/// A valid size of `sigset_t`
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
