// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::str::FromStr;

use crate::error::Error;
use crate::parsing::{SIGNATURE_HASH_SIZE, UPGRADE_NONCE_SIZE};
use crate::CHAIN_ID;
use libsecp256k1::Message;
use primitive_types::{H160, U256};
use sha3::{Digest, Keccak256};
use tezos_ethereum::signatures::{caller, signature};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::Runtime;
use tezos_smart_rollup_installer::installer::with_config_program;
use tezos_smart_rollup_installer::{KERNEL_BOOT_PATH, PREPARE_KERNEL_PATH};
use tezos_smart_rollup_installer_config::binary::owned::{
    OwnedConfigInstruction, OwnedConfigProgram,
};

// TODO: https://gitlab.com/tezos/tezos/-/issues/5894, define the dictator key
// via the config installer set function
pub const DICTATOR_PUBLIC_KEY: &str = "6ce4d79d4E77402e1ef3417Fdda433aA744C6e1c";

// The signature is a combination of the smart rollup address the upgrade nonce and
// the preimage hash signed by the dictator.
// This way we avoid potential replay attack by resending the same kernel upgrade
// thanks to the upgrade nonce field. We ensure that you can not send a kernel upgrade
// to a different smart rollup kernel that has the same dictator key. And ultimately
// that the hash integrity is not corrupted by any means.
// TODO: https://gitlab.com/tezos/tezos/-/issues/5908, tweak signature check from upgrade
// mechanism to actually check the signature and not compare caller addresses.
// The reason for that is to be compatible with a future binary from our own to sign kernel
// upgrade messages in a more conveninent way.
fn upgrade_caller(
    sig: [u8; SIGNATURE_HASH_SIZE],
    smart_rollup_address: [u8; 20],
    upgrade_nonce: [u8; UPGRADE_NONCE_SIZE],
    preimage_hash: [u8; PREIMAGE_HASH_SIZE],
) -> Result<H160, Error> {
    let r: [u8; 32] = sig[0..32]
        .try_into()
        .map_err(|_| Error::InvalidConversion)?;
    let s: [u8; 32] = sig[32..64]
        .try_into()
        .map_err(|_| Error::InvalidConversion)?;
    let v = &sig[64..65];
    let v = U256::from(v);
    // smart_rollup_address length + upgrade_nonce length + preimage_hash length = 57
    let prefix = "\x19Ethereum Signed Message:\n57";
    let mut signed_msg = vec![];
    signed_msg.extend(prefix.as_bytes());
    signed_msg.extend(smart_rollup_address);
    signed_msg.extend(upgrade_nonce);
    signed_msg.extend(preimage_hash);
    let prefixed_hash_msg: [u8; 32] = Keccak256::digest(signed_msg).into();
    let prefixed_msg = Message::parse(&prefixed_hash_msg);
    let (signature, recovery_id) = signature(r, s, v, U256::from(CHAIN_ID), false)
        .map_err(Error::InvalidSignature)?;
    let caller =
        caller(prefixed_msg, signature, recovery_id).map_err(Error::InvalidSignature)?;
    Ok(caller)
}

pub fn check_dictator_signature(
    sig: [u8; SIGNATURE_HASH_SIZE],
    smart_rollup_address: [u8; 20],
    upgrade_nonce: [u8; UPGRADE_NONCE_SIZE],
    preimage_hash: [u8; PREIMAGE_HASH_SIZE],
) -> Result<(), Error> {
    let dictator_pkh =
        H160::from_str(DICTATOR_PUBLIC_KEY).map_err(|_| Error::InvalidConversion)?;
    let caller = upgrade_caller(sig, smart_rollup_address, upgrade_nonce, preimage_hash)?;
    if dictator_pkh == caller {
        Ok(())
    } else {
        Err(Error::InvalidSignatureCheck)
    }
}

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

    // Create config consisting of a reveal instruction, followed by a move one.
    let reveal_instructions = vec![
        OwnedConfigInstruction::reveal_instr(
            root_hash.into(),
            OwnedPath::from(PREPARE_KERNEL_PATH),
        ),
        OwnedConfigInstruction::move_instr(
            OwnedPath::from(PREPARE_KERNEL_PATH),
            OwnedPath::from(KERNEL_BOOT_PATH),
        ),
    ];

    let kernel_config = with_config_program(OwnedConfigProgram(reveal_instructions));

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

    #[test]
    // Check if a random message signed by the dictator key is correctly decoded
    // and if we can properly extract the phk of the caller
    fn test_check_dictator_upgrade_signature() {
        let smart_rollup_address: [u8; 20] = [0; 20];
        let upgrade_nonce: [u8; UPGRADE_NONCE_SIZE] =
            hex::decode("02000000").unwrap().try_into().unwrap();
        let preimage_hash: [u8; PREIMAGE_HASH_SIZE] = hex::decode(
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        )
        .unwrap()
        .try_into()
        .unwrap();
        let signature: [u8; SIGNATURE_HASH_SIZE] = hex::decode("d53b02f4b8075c9caad49fc1dc08eacf8678153004c6e0b078bf13fcce7038f221475b445a9fa56fc5a4569d2161f6b4ca77fa61194f0bd54e3198daa18471641b").unwrap().try_into().unwrap();
        check_dictator_signature(
            signature,
            smart_rollup_address,
            upgrade_nonce,
            preimage_hash,
        )
        .expect("The upgrade signature check from the dictator failed.");
    }
}
