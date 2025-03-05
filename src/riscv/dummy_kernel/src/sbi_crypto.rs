// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// TODO: RV-121: We want to access the crypto functions through the Tezos crypto crate instead of
// needing to define them here.

use tezos_smart_rollup_constants::riscv::SBI_FIRMWARE_TEZOS;
use tezos_smart_rollup_constants::riscv::SBI_TEZOS_BLAKE2B_HASH256;
use tezos_smart_rollup_constants::riscv::SBI_TEZOS_ED25519_SIGN;
use tezos_smart_rollup_constants::riscv::SBI_TEZOS_ED25519_VERIFY;

pub unsafe fn ed25519_verify(pk: &[u8; 32], sig: &[u8; 64], msg: &[u8]) -> bool {
    let result: isize;

    core::arch::asm!(
        "ecall",
        in("a6") SBI_TEZOS_ED25519_VERIFY,
        in("a7") SBI_FIRMWARE_TEZOS,
        in("a0") pk.as_ptr(),
        in("a1") sig.as_ptr(),
        in("a2") msg.as_ptr(),
        in("a3") msg.len(),
        lateout("a0") result,
    );

    result == 1
}

pub unsafe fn ed25519_sign(sk: &[u8; 32], msg: &[u8]) -> [u8; 64] {
    let mut out = [0u8; 64];
    let result: isize;

    core::arch::asm!(
        "ecall",
        in("a6") SBI_TEZOS_ED25519_SIGN,
        in("a7") SBI_FIRMWARE_TEZOS,
        in("a0") sk.as_ptr(),
        in("a1") msg.as_ptr(),
        in("a2") msg.len(),
        in("a3") out.as_mut_ptr(),
        lateout("a0") result,
    );

    assert_eq!(
        result, 64,
        "SBI_TEZOS_ED25519_SIGN call returned unexpected value: {result}"
    );

    out
}

pub unsafe fn blake2b_hash256(msg: &[u8]) -> [u8; 32] {
    let mut out = [0u8; 32];
    let result: isize;

    core::arch::asm!(
        "ecall",
        in("a6") SBI_TEZOS_BLAKE2B_HASH256,
        in("a7") SBI_FIRMWARE_TEZOS,
        in("a0") out.as_mut_ptr(),
        in("a1") msg.as_ptr(),
        in("a2") msg.len(),
        lateout("a0") result,
    );

    assert_eq!(
        result, 32,
        "SBI_TEZOS_BLAKE2B_HASH256 call returned unexpected value: {result}"
    );

    out
}
