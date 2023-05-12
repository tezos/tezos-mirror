// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::host::Runtime;
use tezos_smart_rollup::storage::path::{Path, RefPath};
use tezos_smart_rollup_installer_config::binary::{read_size, RefConfigInstruction};

use crate::preimage::reveal_root_hash;
use crate::KERNEL_BOOT_PATH;

pub fn read_config_program_size(host: &impl Runtime) -> Result<u32, &'static str> {
    let kernel_size = host
        .store_value_size(&KERNEL_BOOT_PATH)
        .map_err(|_| "Couldn't read kernel boot path size")?;
    let mut config_program_size_start = kernel_size - 4;

    read_size(host, &KERNEL_BOOT_PATH, &mut config_program_size_start)
}

pub fn read_instruction_bytes(
    host: &impl Runtime,
    path: &impl Path,
    offset: &mut usize,
    mut buffer: &mut [u8],
) -> Result<(), &'static str> {
    while !buffer.is_empty() {
        let read_size = Runtime::store_read_slice(host, path, *offset, buffer)
            .map_err(|_| "Failed to read kernel boot path in read_instruction")?;
        *offset += read_size;
        buffer = &mut buffer[read_size..];
    }
    Ok(())
}

pub fn handle_instruction(
    host: &mut impl Runtime,
    instr: RefConfigInstruction,
) -> Result<(), &'static str> {
    match instr {
        RefConfigInstruction::Reveal(instr) => {
            let to_path: RefPath = instr.to;
            reveal_root_hash(host, &instr.hash.into(), to_path)
        }
        RefConfigInstruction::Move(instr) => {
            let from_path: RefPath = instr.from;
            let to_path: RefPath = instr.to;
            Runtime::store_move(host, &from_path, &to_path)
                .map_err(|_| "Couldn't move path during config application")
        }
    }
}
