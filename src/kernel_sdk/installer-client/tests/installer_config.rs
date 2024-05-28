// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::ffi::OsString;
use std::fs;
use std::path::Path;
use tezos_smart_rollup::core_unsafe::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup::dac::pages::prepare_preimages;
use tezos_smart_rollup::dac::PreimageHash;
use tezos_smart_rollup::host::Runtime;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_installer::config::create_installer_config;
use tezos_smart_rollup_installer::installer::with_config_program;
use tezos_smart_rollup_installer::KERNEL_BOOT_PATH;
use tezos_smart_rollup_installer_config::binary::owned::{
    OwnedBytes, OwnedConfigInstruction, OwnedConfigProgram,
};
use tezos_smart_rollup_mock::MockHost;

fn write_kernel_to_boot_path(host: &mut MockHost, kernel: Vec<u8>) {
    host.store_write_all(&KERNEL_BOOT_PATH, &kernel)
        .expect("Failed to write kernel to booth path");
}

#[test]
fn reveal_and_move_binary_config() {
    let mut host = MockHost::default();

    let upgrade_to = OsString::from("tests/resources/single_page_kernel.wasm");
    let upgrade_to = Path::new(&upgrade_to);

    // Prepare preimages

    let original_kernel = fs::read(upgrade_to).unwrap();
    let save_preimages = |_hash: PreimageHash, preimage: Vec<u8>| {
        host.set_preimage(preimage);
    };
    let root_hash = prepare_preimages(&original_kernel, save_preimages).unwrap();

    // Create config consisting of reveal and following move
    let config = create_installer_config(root_hash, None, None).unwrap();
    // Append config to the installer.wasm
    let kernel_with_config = with_config_program(config);

    // Write it to the boot path
    write_kernel_to_boot_path(&mut host, kernel_with_config);

    // Execute config
    installer_kernel::installer(&mut host);

    let boot_kernel = host
        .store_read(&KERNEL_BOOT_PATH, 0, MAX_FILE_CHUNK_SIZE)
        .unwrap();
    assert_eq!(original_kernel, boot_kernel);
}

#[test]
fn set_instr_config() {
    let mut host = MockHost::default();

    let to: OwnedPath =
        OwnedPath::try_from(String::from("/foo/tmp")).expect("Invalid owned path");
    let value_str = "Un festival de GADT";
    let value = OwnedBytes(value_str.as_bytes().to_vec());

    let instrs = vec![OwnedConfigInstruction::set_instr(value, to.clone())];

    let kernel = with_config_program(OwnedConfigProgram(instrs));
    write_kernel_to_boot_path(&mut host, kernel);

    installer_kernel::installer(&mut host);

    let mut buffer = vec![0; value_str.len()];
    host.store_read_slice(&to, 0, &mut buffer)
        .expect("Failed to read previously set value");

    let actual = String::from_utf8(buffer).unwrap();
    assert_eq!(value_str, actual)
}

#[test]
fn empty_binary_config() {
    let mut host = MockHost::default();

    let kernel = with_config_program(OwnedConfigProgram(vec![]));

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

fn kernel_from_setup_file(host: &mut MockHost, setup_file: &str) -> Vec<u8> {
    let upgrade_to = OsString::from("tests/resources/single_page_kernel.wasm");
    let upgrade_to = Path::new(&upgrade_to);

    // Prepare preimages

    let original_kernel = fs::read(upgrade_to).unwrap();
    let save_preimages = |_hash: PreimageHash, preimage: Vec<u8>| {
        host.set_preimage(preimage);
    };
    let root_hash = prepare_preimages(&original_kernel, save_preimages).unwrap();

    // Create config consisting of reveal and following move, then move 2 more times from yaml config
    let config =
        create_installer_config(root_hash, Some(OsString::from(setup_file)), None)
            .unwrap();
    // Append config to the installer.wasm
    let kernel_with_config = with_config_program(config);

    // Write it to the boot path
    write_kernel_to_boot_path(host, kernel_with_config);

    // Execute config
    installer_kernel::installer(host);

    original_kernel
}

#[test]
fn yaml_config_move_execute() {
    let mut host = MockHost::default();

    let original_kernel =
        kernel_from_setup_file(&mut host, "tests/resources/move_config.yaml");

    let temporary_path = RefPath::assert_from(b"/temporary/kernel/boot.wasm");
    let boot_kernel = host
        .store_read(&temporary_path, 0, MAX_FILE_CHUNK_SIZE)
        .unwrap();
    assert_eq!(original_kernel, boot_kernel);

    const AUXILIARY_KERNEL_BOOT_PATH: RefPath =
        RefPath::assert_from(b"/__installer_kernel/auxiliary/kernel/boot.wasm");

    assert_eq!(
        host.store_read(&AUXILIARY_KERNEL_BOOT_PATH, 0, MAX_FILE_CHUNK_SIZE),
        Err(RuntimeError::PathNotFound)
    );
}

#[test]
fn yaml_config_set_execute() {
    let mut host = MockHost::default();

    let _original_kernel =
        kernel_from_setup_file(&mut host, "tests/resources/set_config.yaml");

    let to = RefPath::assert_from(b"/tmp/foo");
    let expected = String::from("Un festival de GADT");

    let mut buffer = vec![0; expected.len()];
    host.store_read_slice(&to, 0, &mut buffer)
        .expect("Failed to read previously set value");
    let actual = String::from_utf8(buffer).unwrap();

    assert_eq!(expected, actual)
}
