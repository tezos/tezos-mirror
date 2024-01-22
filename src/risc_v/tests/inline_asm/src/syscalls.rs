// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// Code copied from kernel_sdk/core

use core::arch::asm;

/// Issues a system call with 1 argument.
fn call1(number: u64, arg0: u64) -> u64 {
    let ret;
    unsafe {
        asm!(
            "ecall",
            in("a7") number,
            in("a0") arg0,
            lateout("a0") ret
        )
    }
    ret
}

/// Exit the kernel with a status code.
pub fn exit(code: i32) -> ! {
    call1(93, code as u64) as i64;
    unreachable!()
}
