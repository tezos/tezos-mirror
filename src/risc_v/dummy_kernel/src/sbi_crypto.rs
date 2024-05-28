// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// TODO: https://gitlab.com/tezos/tezos/-/issues/6809
// Move host crypto functionality into Kernel SDK!

use tezos_smart_rollup_constants::riscv::{
    SBI_FIRMWARE_TEZOS, SBI_TEZOS_BLAKE2B_HASH256, SBI_TEZOS_ED25519_SIGN, SBI_TEZOS_ED25519_VERIFY,
};

pub unsafe fn ed25519_verify(pk: &[u8; 32], sig: &[u8; 64], msg: &[u8]) -> bool {
    let result: u64;

    core::arch::asm!(
        "ecall",
        in("a6") SBI_TEZOS_ED25519_VERIFY,
        in("a7") SBI_FIRMWARE_TEZOS,
        in("a0") pk.as_ptr(),
        in("a1") sig.as_ptr(),
        in("a2") msg.as_ptr(),
        in("a3") msg.len() as u64,
        lateout("a0") result,
    );

    result != 0
}

pub unsafe fn ed25519_sign(sk: &[u8; 32], msg: &[u8]) -> [u8; 64] {
    let mut out = [0u8; 64];

    core::arch::asm!(
        "ecall",
        in("a6") SBI_TEZOS_ED25519_SIGN,
        in("a7") SBI_FIRMWARE_TEZOS,
        in("a0") sk.as_ptr(),
        in("a1") msg.as_ptr(),
        in("a2") msg.len() as u64,
        in("a3") out.as_mut_ptr(),
    );

    out
}

pub unsafe fn blake2b_hash256(msg: &[u8]) -> [u8; 32] {
    let mut out = [0u8; 32];

    core::arch::asm!(
        "ecall",
        in("a6") SBI_TEZOS_BLAKE2B_HASH256,
        in("a7") SBI_FIRMWARE_TEZOS,
        in("a0") out.as_mut_ptr(),
        in("a1") msg.as_ptr(),
        in("a2") msg.len() as u64,
    );

    out
}
