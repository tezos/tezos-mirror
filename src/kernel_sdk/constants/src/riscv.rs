// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

/// Extension ID for `sbi_console_putchar`
pub const SBI_CONSOLE_PUTCHAR: u64 = 0x01;

/// Extension ID for `sbi_shutdown`
pub const SBI_SHUTDOWN: u64 = 0x08;

/// Extension ID for Debug Console
pub const SBI_DBCN: u64 = 0x4442434E;

/// Function ID for `sbi_debug_console_write_byte`
pub const SBI_DBCN_CONSOLE_WRITE_BYTE: u64 = 0x02;

/// Extension ID for System Reset
pub const SBI_SRST: u64 = 0x53525354;

/// Function ID for `sbi_system_reset`
pub const SBI_SRST_SYSTEM_RESET: u64 = 0x00;

/// Extension ID for Tezos-specific functions
// IDs from 0x0A000000 to 0x0AFFFFFF are "firmware-specific" extension IDs
pub const SBI_FIRMWARE_TEZOS: u64 = 0x0A000000;

/// Function ID for `sbi_tezos_inbox_next`
pub const SBI_TEZOS_INBOX_NEXT: u64 = 0x01;

/// Function ID for `sbi_tezos_write_output`
pub const SBI_TEZOS_WRITE_OUTPUT: u64 = 0x03;

/// Function ID for `sbi_tezos_ed25519_verify`
pub const SBI_TEZOS_ED25519_VERIFY: u64 = 0x05;

/// Function ID for `sbi_tezos_ed25519_sign`
pub const SBI_TEZOS_ED25519_SIGN: u64 = 0x06;

/// Function ID for `sbi_tezos_blake2b_hash256`
pub const SBI_TEZOS_BLAKE2B_HASH256: u64 = 0x07;

/// Function ID for `sbi tezos reveal`
pub const SBI_TEZOS_REVEAL: u64 = 0x09;

/// Function ID for `sbi_tezos_secp256k1_verify`
pub const SBI_TEZOS_SECP256K1_VERIFY: u64 = 0x0a;

/// Function ID for `sbi_tezos_keccak_hash256`
pub const SBI_TEZOS_KECCAK256_HASH: u64 = 0x0b;

/// Maximum size of Reveal Request
pub const REVEAL_REQUEST_MAX_SIZE: usize = 4096;

/// Maximum size of Reveal Response Data
pub const REVEAL_DATA_MAX_SIZE: usize = 4096;

/// Standard SBI errors
#[derive(Debug, Copy, Clone)]
#[repr(i64)]
pub enum SbiError {
    Failed = -1,
    NotSupported = -2,
    InvalidParam = -3,
    Denied = -4,
    InvalidAddress = -5,
    AlreadyAvailable = -6,
    AlreadyStarted = -7,
    AlreadyStopped = -8,
    NoSharedMemory = -9,
    OutputTooLarge = -10,
    FullOutbox = -11,
    Unknown = i64::MIN,
}

impl SbiError {
    /// Interpret the given integer as an SBI error.
    pub fn from_result(result: isize) -> Option<Self> {
        match result {
            -1 => Some(Self::Failed),
            -2 => Some(Self::NotSupported),
            -3 => Some(Self::InvalidParam),
            -4 => Some(Self::Denied),
            -5 => Some(Self::InvalidAddress),
            -6 => Some(Self::AlreadyAvailable),
            -7 => Some(Self::AlreadyStarted),
            -8 => Some(Self::AlreadyStopped),
            -9 => Some(Self::NoSharedMemory),
            -10 => Some(Self::OutputTooLarge),
            -11 => Some(Self::FullOutbox),
            _ if result < 0 => Some(Self::Unknown),
            _ => None,
        }
    }
}
