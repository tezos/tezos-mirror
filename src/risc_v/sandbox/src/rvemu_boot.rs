// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// Allow dead code because I don't want to delete or add these register shortcuts
// everytime I change code elsewhere.
#![allow(dead_code)]

use crate::devicetree;
use kernel_loader::{LoadResult, Memory};
use rvemu::{cpu::Mode, emulator::Emulator};
use std::{error::Error, fs};

// Named register mapping, named after RISC-V Spec ABI
// E.g. a7 => x17
pub const A0: u64 = 10;
pub const A1: u64 = 11;
pub const A2: u64 = 12;
pub const A3: u64 = 13;
pub const A4: u64 = 14;
pub const A5: u64 = 15;
pub const A6: u64 = 16;
pub const A7: u64 = 17;

/// Configure the emulator so it is ready to boot.
pub fn setup_boot(
    emu: &mut Emulator,
    contents: &[u8],
    initrd: Option<String>,
) -> Result<(), Box<dyn Error>> {
    let LoadResult {
        entry,
        last_written,
    } = kernel_loader::load_elf(&mut emu.cpu.bus, rvemu::bus::DRAM_BASE, contents)?;

    let initrd_addr = last_written;

    // Setting the program counter (PC) tells the emulator where to start executing.
    emu.initialize_pc(entry);

    // Load the initial ramdisk to memory.
    let initrd_info = initrd
        .map(|initrd_path| -> Result<_, Box<dyn Error>> {
            let initrd = fs::read(initrd_path)?;
            emu.cpu.bus.write_bytes(initrd_addr, initrd.as_slice())?;
            Ok(devicetree::InitialRamDisk {
                start: initrd_addr,
                length: initrd.len() as u64,
            })
        })
        .transpose()?;

    // Generate and load the flattened device tree.
    let dtb_addr = initrd_info
        .as_ref()
        .map(|info| info.start + info.length)
        .unwrap_or(initrd_addr);
    let dtb = devicetree::generate(initrd_info)?;
    emu.cpu.bus.write_bytes(dtb_addr, dtb.as_slice())?;

    // Start off in supervisor mode.
    emu.cpu.mode = Mode::Supervisor;

    // Linux and HermitOS use a0 (x10) to identify the current Hart ID.
    emu.cpu.xregs.write(A0, 0);

    // Linux and HermitOS expect the pointer to the device tree in register a1 (x11).
    emu.cpu.xregs.write(A1, dtb_addr);

    Ok(())
}
