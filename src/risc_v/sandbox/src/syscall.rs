// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! More information on SBI specification:
//!   - https://www.scs.stanford.edu/~zyedidia/docs/riscv/riscv-sbi.pdf

use crate::inbox::Inbox;
use crate::rv::{A0, A1, A2, A6, A7};
use kernel_loader::Memory;
use rvemu::cpu::AccessType;
use rvemu::emulator::Emulator;
use std::{
    error::Error,
    io::{self, Write},
    process::exit,
};
use tezos_smart_rollup_constants::riscv::{
    SBI_CONSOLE_PUTCHAR, SBI_FIRMWARE_TEZOS, SBI_SHUTDOWN, SBI_TEZOS_INBOX_NEXT,
};

/// SBI extension ID 0x01
fn sbi_console_putchar(emu: &mut Emulator) -> Result<(), Box<dyn Error>> {
    let c = emu.cpu.xregs.read(A0) as u8;
    io::stdout().lock().write_all(&[c])?;
    emu.cpu.xregs.write(A0, 0);
    Ok(())
}

/// SBI extension ID 0x08
fn sbi_shutdown() -> ! {
    eprintln!("Received SBI shutdown request");
    exit(0)
}

/// Move the Inbox to the next message.
fn sbi_tezos_inbox_next(emu: &mut Emulator, inbox: &mut Inbox) -> Result<(), Box<dyn Error>> {
    match inbox.next() {
        Some((level, id, data)) => {
            let dest_addr = emu.cpu.xregs.read(A0);
            let max_bytes = emu.cpu.xregs.read(A1);

            let dest_addr = emu
                .cpu
                .translate(dest_addr, AccessType::Store)
                .map_err(super::exception_to_error)?;
            let length = max_bytes.min(data.len() as u64);

            let source_data = &data[0..length as usize];
            emu.cpu.bus.write_bytes(dest_addr, source_data)?;

            emu.cpu.xregs.write(A0, level as u64);
            emu.cpu.xregs.write(A1, id as u64);
            emu.cpu.xregs.write(A2, length);
        }

        None => {
            emu.cpu.xregs.write(A0, 0);
            emu.cpu.xregs.write(A1, 0);
            emu.cpu.xregs.write(A2, 0);
        }
    }

    Ok(())
}

/// Handle a system call originating from the user program.
pub fn handle_sbi(emu: &mut Emulator, inbox: &mut Inbox) -> Result<(), Box<dyn Error>> {
    // TODO: https://gitlab.com/tezos/tezos/-/issues/6767
    // Feed errors back to caller instead of raising them in the sandbox.
    // This means this function most likely should return unit.

    // SBI extension is contained in a7.
    let sbi_extension = emu.cpu.xregs.read(A7);
    match sbi_extension {
        SBI_CONSOLE_PUTCHAR => sbi_console_putchar(emu),
        SBI_SHUTDOWN => sbi_shutdown(),
        SBI_FIRMWARE_TEZOS => {
            let sbi_function = emu.cpu.xregs.read(A6);
            match sbi_function {
                SBI_TEZOS_INBOX_NEXT => sbi_tezos_inbox_next(emu, inbox),
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
