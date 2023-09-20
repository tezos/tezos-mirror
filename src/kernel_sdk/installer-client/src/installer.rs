// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::enc::BinWriter;
use tezos_smart_rollup_installer_config::binary::owned::OwnedConfigProgram;
use wasm_gen::write_custom_section;

const INSTALLER_KERNEL: &[u8] = include_bytes!("../installer.wasm");

/// Set the installer config for the reveal installer.
///
/// This is set as a custom section of the installer binary.
/// When run, the the installer kernel interprets commands in the config
/// from the end of its binary file
/// (located at `/kernel/boot.wasm` in durable storage).
///
/// Config provides a way to customise behaviour of installer kernel.
/// For instance, upgrade to kernel given by
/// a root hash, using the _reveal data channel_, is possible with this approach.
///
/// For more information about which instructions config might contain,
/// see in `installer_kernel/src/instr.rs`.
pub fn with_config_program(config_programm: OwnedConfigProgram) -> Vec<u8> {
    let mut installer = INSTALLER_KERNEL.to_vec();

    let mut config_programm_encoded = vec![];
    config_programm
        .bin_write(&mut config_programm_encoded)
        .unwrap();

    write_custom_section(&mut installer, "config", &config_programm_encoded);

    installer
}
