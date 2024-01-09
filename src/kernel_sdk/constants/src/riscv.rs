// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// Extension ID for `sbi_console_putchar`
pub const SBI_CONSOLE_PUTCHAR: u64 = 0x01;

/// Extension ID for `sbi_shutdown`
pub const SBI_SHUTDOWN: u64 = 0x08;

/// Extension ID for Tezos-specific functions
// IDs from 0x0A000000 to 0x0AFFFFFF are "firmware-specific" extension IDs
pub const SBI_FIRMWARE_TEZOS: u64 = 0x0A000000;

/// Function ID for `sbi_tezos_inbox_next`
pub const SBI_TEZOS_INBOX_NEXT: u64 = 0x01;
