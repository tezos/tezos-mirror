// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! More information on SBI specification:
//!   - https://www.scs.stanford.edu/~zyedidia/docs/riscv/riscv-sbi.pdf

use crate::rv::{A0, A6, A7};
use rvemu::emulator::Emulator;
use std::{
    error::Error,
    io::{self, Write},
    process::exit,
};

/// Extension ID for [sbi_console_putchar]
const SBI_CONSOLE_PUTCHAR: u64 = 0x01;

/// SBI extension ID 0x01
fn sbi_console_putchar(emu: &mut Emulator) -> Result<(), Box<dyn Error>> {
    let c = emu.cpu.xregs.read(A0) as u8;
    io::stdout().lock().write_all(&[c])?;
    emu.cpu.xregs.write(A0, 0);
    Ok(())
}

/// Extension ID for [sbi_shutdown]
const SBI_SHUTDOWN: u64 = 0x08;

/// SBI extension ID 0x08
fn sbi_shutdown() -> ! {
    eprintln!("Received SBI shutdown request");
    exit(0)
}

/// Extension ID for Tezos-specific functions
// IDs from 0x0A000000 to 0x0AFFFFFF are "firmware-specific" extension IDs
const SBI_FIRMWARE_TEZOS: u64 = 0x0A000000;

/// Handle a system call originating from the user program.
pub fn handle_sbi(emu: &mut Emulator) -> Result<(), Box<dyn Error>> {
    // SBI extension is contained in a7.
    let sbi_extension = emu.cpu.xregs.read(A7);
    match sbi_extension {
        SBI_CONSOLE_PUTCHAR => sbi_console_putchar(emu),
        SBI_SHUTDOWN => sbi_shutdown(),
        SBI_FIRMWARE_TEZOS => {
            let sbi_function = emu.cpu.xregs.read(A6);
            #[allow(clippy::match_single_binding)]
            match sbi_function {
                _ => Err(format!(
                    "Unimplemented Tezos SBI extension ({sbi_extension}) function {sbi_function}"
                )
                .into()),
            }
        }
        _ => Err(format!("Unimplemented SBI extension {sbi_extension}").into()),
    }
}

// System calls
const EXIT: u64 = 93;

/// Handle a system call originating from the user program in POSIX style.
pub fn handle_posix(emu: &mut Emulator) -> Result<(), Box<dyn Error>> {
    // System call number is contained in a7.
    let syscall_number = emu.cpu.xregs.read(A7);
    match syscall_number {
        EXIT => {
            let code = emu.cpu.xregs.read(A0);
            eprintln!("Received request to exit with code {}", code);
            exit(code as i32);
        }

        _ => Err(format!("Unimplemented system call {syscall_number}").into()),
    }
}
