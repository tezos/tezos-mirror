// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::error::UpgradeProcessError;
use crate::parsing::{SIGNATURE_HASH_SIZE, UPGRADE_NONCE_SIZE};
use libsecp256k1::{Message, PublicKey, Signature};
use sha3::{Digest, Keccak256};
use tezos_data_encoding::enc::BinWriter;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::Runtime;
use tezos_smart_rollup_installer_config::binary::owned::{
    OwnedConfigInstruction, OwnedConfigProgram,
};

// TODO: https://gitlab.com/tezos/tezos/-/issues/5894, define the dictator key
// via the config installer set function
pub const DICTATOR_PUBLIC_KEY: &str = "046edc43401193c9321730cdf73e454f68e8aa52e377d001499b0eaa431fa4763102e685fe33851f5f51bd31adb41582bbfb0ad85c1089c0a0b4adc049a271bc01";

// The signature is a combination of the smart rollup address the upgrade nonce and
// the preimage hash signed by the dictator.
// This way we avoid potential replay attack by resending the same kernel upgrade
// thanks to the upgrade nonce field. We ensure that you can not send a kernel upgrade
// to a different smart rollup kernel that has the same dictator key. And ultimately
// that the hash integrity is not corrupted by any means.
pub fn check_dictator_signature(
    sig: [u8; SIGNATURE_HASH_SIZE],
    smart_rollup_address: [u8; 20],
    upgrade_nonce: [u8; UPGRADE_NONCE_SIZE],
    preimage_hash: [u8; PREIMAGE_HASH_SIZE],
) -> Result<(), Error> {
    let mut signed_msg = vec![];
    signed_msg.extend(smart_rollup_address);
    signed_msg.extend(upgrade_nonce);
    signed_msg.extend(preimage_hash);
    let hash_msg: [u8; 32] = Keccak256::digest(signed_msg).into();
    let msg = Message::parse(&hash_msg);
    let dictator =
        hex::decode(DICTATOR_PUBLIC_KEY).map_err(|_| Error::InvalidConversion)?;
    let pk =
        PublicKey::parse_slice(&dictator, None).map_err(|_| Error::InvalidParsing)?;
    let sig = Signature::parse_standard_slice(&sig).map_err(|_| Error::InvalidParsing)?;
    if libsecp256k1::verify(&msg, &sig, &pk) {
        Ok(())
    } else {
        Err(Error::InvalidSignatureCheck)
    }
}

// Boot path for kernels
pub const KERNEL_BOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/boot.wasm");
// Path that will contain the config interpretation.
pub const CONFIG_INTERPRETER_PATH: RefPath =
    RefPath::assert_from(b"/installer/config_interpreter");

// TODO: https://gitlab.com/tezos/tezos/-/issues/5907, expose an explicit kernel
// upgrade function in the SDK that would move most of the code below.
pub fn upgrade_kernel<Host: Runtime>(
    host: &mut Host,
    root_hash: [u8; PREIMAGE_HASH_SIZE],
) -> Result<(), Error> {
    debug_msg!(host, "Kernel upgrade initialisation.\n");
    let root_hash = root_hash.to_vec();

    // Create config consisting of a reveal instruction.
    let reveal_instructions = vec![OwnedConfigInstruction::reveal_instr(
        root_hash.into(),
        OwnedPath::from(KERNEL_BOOT_PATH),
    )];

    let mut kernel_config = Vec::new();
    OwnedConfigProgram(reveal_instructions)
        .bin_write(&mut kernel_config)
        .map_err(UpgradeProcessError::ConfigSerialisation)?;

    host.store_write_all(&CONFIG_INTERPRETER_PATH, &kernel_config)?;

    if let Err(e) = installer_kernel::install_kernel(host, CONFIG_INTERPRETER_PATH) {
        debug_msg!(
            host,
            "Error \"{}\" was detected during kernel installation.\n",
            e
        );
    } else {
        debug_msg!(host, "Kernel is ready to be upgraded.\n");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsString;
    use std::fs;
    use std::path::Path;
    use tezos_smart_rollup_encoding::dac::{prepare_preimages, PreimageHash};
    use tezos_smart_rollup_mock::MockHost;

    #[test]
    // Check if a random message signed by the dictator key is correctly decoded
    // and if we can properly extract the phk of the caller
    fn test_check_dictator_upgrade_signature() {
        let smart_rollup_address: [u8; 20] = [0; 20];
        let upgrade_nonce: [u8; UPGRADE_NONCE_SIZE] = 2u16.to_le_bytes();
        let preimage_hash: [u8; PREIMAGE_HASH_SIZE] = hex::decode(
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        )
        .unwrap()
        .try_into()
        .unwrap();
        let signature: [u8; SIGNATURE_HASH_SIZE] = hex::decode("f6ab7d81a3d2791171b9cfdf41fca31d21dc56a5dd19e797e074ae4c3b2abecd2e450c97d30bbd44f8aa2aa36a348027ea9b8441907eabee3c86c87d0f42fef1").unwrap().try_into().unwrap();
        check_dictator_signature(
            signature,
            smart_rollup_address,
            upgrade_nonce,
            preimage_hash,
        )
        .expect("The upgrade signature check from the dictator failed.");
    }

    fn preliminary_upgrade(host: &mut MockHost) -> (PreimageHash, Vec<u8>) {
        let upgrade_to = OsString::from("tests/resources/debug_kernel.wasm");
        let upgrade_to = Path::new(&upgrade_to);

        // Preimages preparation

        let original_kernel = fs::read(upgrade_to).unwrap();
        let save_preimages = |_hash: PreimageHash, preimage: Vec<u8>| {
            host.set_preimage(preimage);
        };
        (
            prepare_preimages(&original_kernel, save_preimages).unwrap(),
            original_kernel,
        )
    }

    #[test]
    // Test if we manage to upgrade the kernel from the actual one to
    // the debug kernel one from `tests/resources/debug_kernel.wasm`.
    fn test_kernel_upgrade() {
        let mut host = MockHost::default();
        let (root_hash, original_kernel) = preliminary_upgrade(&mut host);
        upgrade_kernel(&mut host, root_hash.into())
            .expect("Kernel upgrade must succeed.");

        let boot_kernel = host.store_read_all(&KERNEL_BOOT_PATH).unwrap();
        assert_eq!(original_kernel, boot_kernel);
    }
}
