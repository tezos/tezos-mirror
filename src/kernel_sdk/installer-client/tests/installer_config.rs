// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::ffi::OsString;
use std::fs;
use std::path::Path;
use tezos_smart_rollup::core_unsafe::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup::dac::{prepare_preimages, PreimageHash};
use tezos_smart_rollup::host::Runtime;
use tezos_smart_rollup_installer::installer::with_config_program;
use tezos_smart_rollup_installer::{KERNEL_BOOT_PATH, PREPARE_KERNEL_PATH};
use tezos_smart_rollup_installer_config::{bin::ConfigProgram, instr::ConfigInstruction};
use tezos_smart_rollup_mock::MockHost;

#[test]
fn reveal_and_move_config() {
    let mut host = MockHost::default();

    let upgrade_to = OsString::from("tests/resources/single_page_kernel.wasm");
    let upgrade_to = Path::new(&upgrade_to);

    // Prepare preimages

    let original_kernel = fs::read(upgrade_to).unwrap();
    let save_preimages = |_hash: PreimageHash, preimage: Vec<u8>| {
        host.set_preimage(preimage);
    };
    let root_hash = prepare_preimages(&original_kernel, save_preimages).unwrap();

    // Prepend config to the installer.wasm
    let kernel_with_config = with_config_program(ConfigProgram(vec![
        ConfigInstruction::reveal_instr(root_hash.as_ref(), PREPARE_KERNEL_PATH),
        ConfigInstruction::move_instr(PREPARE_KERNEL_PATH, KERNEL_BOOT_PATH),
    ]));

    // Write it to the boot path
    let mut i = 0;
    while i < kernel_with_config.len() {
        let r = usize::min(kernel_with_config.len(), i + MAX_FILE_CHUNK_SIZE);
        host.store_write(&KERNEL_BOOT_PATH, &kernel_with_config[i..r], i)
            .unwrap();
        i = r;
    }

    // Execute config
    installer_kernel::installer(&mut host);

    let boot_kernel = host
        .store_read(&KERNEL_BOOT_PATH, 0, MAX_FILE_CHUNK_SIZE)
        .unwrap();
    assert_eq!(original_kernel, boot_kernel);
}

#[test]
fn empty_config() {
    let mut host = MockHost::default();

    let kernel = with_config_program(ConfigProgram(vec![]));

    // Write it to the boot path
    let mut i = 0;
    while i < kernel.len() {
        let r = usize::min(kernel.len(), i + MAX_FILE_CHUNK_SIZE);
        host.store_write(&KERNEL_BOOT_PATH, &kernel[i..r], i)
            .unwrap();
        i = r;
    }

    // Execute config
    installer_kernel::installer(&mut host);

    let mut boot_kernel = vec![0; kernel.len()];
    i = 0;
    while i < kernel.len() {
        let bts = host
            .store_read_slice(&KERNEL_BOOT_PATH, i, &mut boot_kernel[i..])
            .unwrap();
        i += bts;
    }

    assert_eq!(kernel, boot_kernel);
}
