// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

extern crate alloc;
extern crate std;

use crate::smart_rollup_core::ReadInputMessageInfo;
use alloc::vec;
use std::{
    io::{self, Write},
    slice::from_raw_parts,
};
use tezos_smart_rollup_constants::{
    core::{
        FULL_OUTBOX, GENERIC_INVALID_ACCESS, INPUT_OUTPUT_TOO_LARGE,
        MEMORY_INVALID_ACCESS,
    },
    riscv::{
        SbiError, SBI_FIRMWARE_TEZOS, SBI_TEZOS_INBOX_NEXT, SBI_TEZOS_REVEAL,
        SBI_TEZOS_WRITE_OUTPUT,
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

        // The kernel tried to write an outbox message which is too large.
        Some(SbiError::OutputTooLarge) => Err(INPUT_OUTPUT_TOO_LARGE),

        // The kernel tried to write an outbox message but the outbox is full.
        Some(SbiError::FullOutbox) => Err(FULL_OUTBOX),

        // Uncategorised error.
        Some(_) => Err(GENERIC_INVALID_ACCESS),
    }
}

#[inline]
pub(crate) unsafe fn read_input(
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
pub(crate) unsafe fn reveal(
    request_addr: *const u8,
    request_len: usize,
    response_addr: *mut u8,
    response_max_bytes: usize,
) -> i32 {
    let result: isize;

    // SBI call
    //   extension = SBI_FIRMWARE_TEZOS
    //   function = SBI_TEZOS_REVEAL
    core::arch::asm!(
        "ecall",
        in("a0") request_addr,
        in("a1") request_len,
        in("a2") response_addr,
        in("a3") response_max_bytes,
        in("a6") SBI_TEZOS_REVEAL,
        in("a7") SBI_FIRMWARE_TEZOS,
        lateout("a0") result,
    );

    match check_sbi_result(result) {
        Err(err) => err,
        Ok(result) => result as i32,
    }
}

#[inline]
pub(crate) unsafe fn reveal_preimage(
    hash_addr: *const u8,
    hash_len: usize,
    destination_addr: *mut u8,
    max_bytes: usize,
) -> i32 {
    let mut payload = vec![0u8; hash_len + 1];
    hash_addr.copy_to_nonoverlapping(payload.as_mut_ptr().add(1), hash_len);

    reveal(payload.as_ptr(), payload.len(), destination_addr, max_bytes)
}

#[inline]
pub(crate) unsafe fn reveal_metadata(buffer: *mut u8, max_bytes: usize) -> i32 {
    let request_payload = [1u8]; // reveal request tag

    reveal(
        request_payload.as_ptr(),
        std::mem::size_of_val(&request_payload),
        buffer,
        max_bytes,
    )
}

#[inline]
pub(crate) unsafe fn write_output(src: *const u8, num_bytes: usize) -> i32 {
    let result: isize;

    // SBI call
    //   extension = SBI_FIRMWARE_TEZOS
    //   function = SBI_TEZOS_WRITE_OUTPUT
    core::arch::asm!(
        "ecall",
        in("a0") src,
        in("a1") num_bytes,
        in("a6") SBI_TEZOS_WRITE_OUTPUT,
        in("a7") SBI_FIRMWARE_TEZOS,
        lateout("a0") result,
    );

    match check_sbi_result(result) {
        Ok(result) => result as i32,
        Err(err) => err,
    }
}

#[inline]
pub(crate) unsafe fn write_debug(src: *const u8, num_bytes: usize) {
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
pub(super) unsafe fn store_value_size(_path: *const u8, _path_len: usize) -> i32 {
    unimplemented!()
}

#[inline]
pub(super) unsafe fn __internal_store_get_hash(
    _path: *const u8,
    _path_len: usize,
    _dst: *mut u8,
    _max_size: usize,
) -> i32 {
    unimplemented!()
}
