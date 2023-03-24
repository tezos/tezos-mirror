// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_encoding::dac::PreimageHash;
use wasm_gen::write_custom_section;

const INSTALLER_KERNEL: &[u8] = include_bytes!("../../installer.wasm");

/// Set the kernel root hash for the reveal installer.
///
/// This is set as a custom section of the installer binary. When run, the
/// the installer kernel looks-up the root hash from the end of its
/// binary file (located at `/kernel/boot.wasm` in durable storage).
///
/// The installer will then proceed to upgrade to the kernel given by
/// the root hash - using the _reveal data channel_.
pub fn with_reveal_hash(root_hash: &PreimageHash) -> Vec<u8> {
    let mut installer = INSTALLER_KERNEL.to_vec();

    write_custom_section(&mut installer, "config", root_hash.as_ref());

    installer
}
