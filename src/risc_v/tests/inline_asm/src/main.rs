// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![no_std]
#![no_main]

mod syscalls;

use core::arch::asm;
use core::panic::PanicInfo;
use syscalls::exit;

#[no_mangle]
pub extern "C" fn _start() -> ! {
    let mut acc: u64 = 0;
    unsafe {
        asm!("addi {0}, {0}, 42", inout(reg) acc);
    }
    if acc != 42 {
        exit(1)
    } else {
        exit(0)
    }
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}
