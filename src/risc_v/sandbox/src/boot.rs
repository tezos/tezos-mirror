// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::rv::{A0, A1};
use rvemu::{cpu::Mode, emulator::Emulator};

/// Configure the emulator so it is ready to boot.
pub fn configure(emu: &mut Emulator, dtb_addr: u64) {
    // Start off in supervisor mode.
    emu.cpu.mode = Mode::Supervisor;

    // Linux and HermitOS use a0 (x10) to identify the current Hart ID.
    emu.cpu.xregs.write(A0, 0);

    // Linux and HermitOS expect the pointer to the device tree in register a1 (x11).
    emu.cpu.xregs.write(A1, dtb_addr);
}
