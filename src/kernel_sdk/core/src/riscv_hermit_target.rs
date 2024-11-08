// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! This module defines the host capabilities of the RISC-V PVM with HermitOS. Host capabilities are
//! accessed via SBI environment calls.
//!
//! For the RISC-V PVM, we use the custom SBI extension as defined by [`SBI_FIRMWARE_TEZOS`].
//!
//! This means, when we try to get the PVM to do something for us we follow those steps:
//! - Set register a7 to SBI_FIRMWARE_TEZOS
//! - Set register a6 to the PVM function identifier (check constants)
//! - Set registers a0-a5 to transport arguments
//! - Trigger an ECALL
//! - Once the PVM returns to us, the results are stored in a0 and/or a1 in addition to the
//!   out-parameters passed via a0-a5

extern crate std;

use crate::smart_rollup_core::ReadInputMessageInfo;
use std::{
    io::{self, Write},
    slice::from_raw_parts,
};
use tezos_smart_rollup_constants::{
    core::{
        GENERIC_INVALID_ACCESS, MEMORY_INVALID_ACCESS, METADATA_LENGTH,
        ORIGINATION_LEVEL_LENGTH, ROLLUP_ADDRESS_LENGTH,
    },
    riscv::{
        SbiError, SBI_FIRMWARE_TEZOS, SBI_TEZOS_INBOX_NEXT, SBI_TEZOS_METADATA_REVEAL,
    },
};

/// Check the SBI return value for errors.
#[inline]
fn check_sbi_result(result: isize) -> Result<usize, i32> {
    match SbiError::from_result(result) {
        None => Ok(result as usize),

        // The SBI call was not supported. This is a fatal error.
        Some(SbiError::NotSupported) => panic!("SBI call not supported"),

        // Indicates a bad address or memory access.
        Some(SbiError::InvalidAddress) => Err(MEMORY_INVALID_ACCESS),

        // Uncategorised error.
        Some(_) => Err(GENERIC_INVALID_ACCESS),
    }
}

#[inline]
pub(super) unsafe fn read_input(
    message_info: *mut ReadInputMessageInfo,
    dst: *mut u8,
    max_bytes: usize,
) -> i32 {
    let message_info = &mut *message_info;
    let result: isize;

    // SBI call
    //   extension = SBI_FIRMWARE_TEZOS
    //   function = SBI_TEZOS_INBOX_NEXT
    core::arch::asm!(
        "ecall",
        in("a0") dst,
        in("a1") max_bytes,
        in("a2") &mut message_info.level,
        in("a3") &mut message_info.id,
        in("a6") SBI_TEZOS_INBOX_NEXT,
        in("a7") SBI_FIRMWARE_TEZOS,
        lateout("a0") result,
    );

    match check_sbi_result(result) {
        Ok(result) => result as i32,
        Err(err) => err,
    }
}

#[inline]
pub(super) unsafe fn write_output(_src: *const u8, _num_bytes: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn write_debug(src: *const u8, num_bytes: usize) {
    let buffer = unsafe { from_raw_parts(src, num_bytes) };
    io::stdout()
        .write_all(buffer)
        .expect("Writing to stdout failed");
}

#[inline]
pub(super) unsafe fn store_has(_path: *const u8, _path_len: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_read(
    _path: *const u8,
    _path_len: usize,
    _offset: usize,
    _dst: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_write(
    _path: *const u8,
    _path_len: usize,
    _offset: usize,
    _src: *const u8,
    _num_bytes: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_delete(_path: *const u8, _len: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_delete_value(_path: *const u8, _len: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_list_size(_path: *const u8, _path_len: usize) -> i64 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_move(
    _from_path: *const u8,
    _from_path_len: usize,
    _to_path: *const u8,
    _to_path_len: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_copy(
    _from_path: *const u8,
    _from_path_len: usize,
    _to_path: *const u8,
    _to_path_len: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn reveal_preimage(
    _hash_addr: *const u8,
    _hash_len: usize,
    _destination_addr: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn reveal(
    _payload_addr: *const u8,
    _payload_len: usize,
    _destination_addr: *mut u8,
    _max_bytes: usize,
) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn store_value_size(_path: *const u8, _path_len: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn reveal_metadata(buffer: *mut u8, max_bytes: usize) -> i32 {
    let mut sbi_buffer = [0u8; METADATA_LENGTH];
    let origin_level = {
        let result: isize;

        // SBI call
        //   extension = SBI_FIRMWARE_TEZOS
        //   function = SBI_TEZOS_METADATA_REVEAL
        core::arch::asm!(
            "ecall",
            in("a0") sbi_buffer.as_mut_ptr(),
            in("a6") SBI_TEZOS_METADATA_REVEAL,
            in("a7") SBI_FIRMWARE_TEZOS,
            lateout("a0") result,
        );

        match check_sbi_result(result) {
            Err(err) => return err,
            Ok(result) => result,
        }
    };

    match ORIGINATION_LEVEL_LENGTH {
        4 => {
            sbi_buffer[ROLLUP_ADDRESS_LENGTH
                ..(ROLLUP_ADDRESS_LENGTH + ORIGINATION_LEVEL_LENGTH)]
                .copy_from_slice(&(origin_level as i32).to_be_bytes());
        }

        8 => {
            sbi_buffer[ROLLUP_ADDRESS_LENGTH
                ..(ROLLUP_ADDRESS_LENGTH + ORIGINATION_LEVEL_LENGTH)]
                .copy_from_slice(&(origin_level as i64).to_be_bytes());
        }

        _ => {
            panic!("Unknown origination level type length {ORIGINATION_LEVEL_LENGTH}")
        }
    }

    let written = max_bytes.min(sbi_buffer.len());
    buffer.copy_from(sbi_buffer.as_mut_ptr(), written);

    written as i32
}
