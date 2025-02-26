// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use arbitrary_int::u7;

use super::MAIN_THREAD_ID;
use super::error::Error;

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
