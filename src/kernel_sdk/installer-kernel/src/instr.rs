// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::host::Runtime;
use tezos_smart_rollup::storage::path::{Path, RefPath};
use tezos_smart_rollup_installer_config::binary::read_size;

pub fn read_config_program_size(
    host: &impl Runtime,
    config_interpretation_path: &RefPath,
) -> Result<u32, &'static str> {
    let kernel_size = host
        .store_value_size(config_interpretation_path)
        .map_err(|_| "Couldn't read kernel boot path size")?;
    let mut config_program_size_start = kernel_size - 4;

    read_size(
        host,
        config_interpretation_path,
        &mut config_program_size_start,
    )
}

pub fn read_instruction_bytes(
    host: &impl Runtime,
    path: &impl Path,
    offset: &mut usize,
    mut buffer: &mut [u8],
) -> Result<(), &'static str> {
    while !buffer.is_empty() {
        let read_size = host
            .store_read_slice(path, *offset, buffer)
            .map_err(|_| "Failed to read kernel boot path in read_instruction")?;
        *offset += read_size;
        buffer = &mut buffer[read_size..];
    }
    Ok(())
}
