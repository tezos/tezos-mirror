// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    storage::{ADMIN, SEQUENCER},
    EVM_PATH,
};
use rlp::{Decodable, DecoderError, Rlp};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_ethereum::rlp_helpers::{decode_field, next, FromRlpBytes};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_debug::Runtime;
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::ValueType;
use tezos_smart_rollup_installer_config::binary::owned::{
    OwnedConfigInstruction, OwnedConfigProgram,
};

/// This module is a testing device, allowing to replicate the state of an existing EVM rollup
/// chain into a new deployment. It is not tick-safe, and should obviously not be used in a
/// production setup.

const CONFIG_PATH: RefPath = RefPath::assert_from(b"/__tmp/config");
pub const CONFIG_ROOT_HASH_PATH: RefPath =
    RefPath::assert_from(b"/__tmp/config_root_hash");

#[derive(Debug)]
struct Set {
    to: OwnedPath,
    value: Vec<u8>,
}

impl Decodable for Set {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if Ok(2) != decoder.item_count() {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let to: Vec<u8> = decode_field(&next(&mut it)?, "to")?;
        let to: RefPath = RefPath::assert_from(&to);
        let to: OwnedPath = to.into();
        let value: Vec<u8> = decode_field(&next(&mut it)?, "value")?;

        Ok(Self { to, value })
    }
}

#[derive(Debug)]
struct Sets(Vec<Set>);

impl Decodable for Sets {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        let sets = decoder.as_list()?;
        Ok(Sets(sets))
    }
}

pub fn is_revealed_storage(host: &impl Runtime) -> bool {
    matches!(
        host.store_has(&CONFIG_ROOT_HASH_PATH).unwrap_or_default(),
        Some(ValueType::Value)
    )
}

pub fn reveal_storage(
    host: &mut impl Runtime,
    sequencer: Option<PublicKey>,
    admin: Option<ContractKt1Hash>,
) {
    log!(host, Info, "Starting the reveal dump");

    let config_root_hash = host
        .store_read_all(&CONFIG_ROOT_HASH_PATH)
        .expect("Failed reading the config root hash");

    // Reveal RLP encoded list of `Set` instructions in storage.
    let config = OwnedConfigProgram(vec![OwnedConfigInstruction::reveal_instr(
        config_root_hash.into(),
        OwnedPath::from(&CONFIG_PATH),
    )]);
    config.evaluate(host).expect("Revealing the config failed");
    let config_bytes = host
        .store_read_all(&CONFIG_PATH)
        .expect("Failed reading the configuration");

    // Decode the RLP list of instructions.
    let sets =
        Sets::from_rlp_bytes(&config_bytes).expect("Failed to decode the list of set");
    let length = sets.0.len();

    for (index, Set { to, value }) in sets.0.iter().enumerate() {
        if index % 50_000 == 0 {
            log!(host, Info, "{}/{}", index, length)
        };
        host.store_write_all(to, value)
            .expect("Failed to write value");
    }

    // Remove temporary configuration
    host.store_delete(&CONFIG_PATH)
        .expect("Failed to remove the configuration");

    // Remove configuration root hash
    host.store_delete(&CONFIG_ROOT_HASH_PATH)
        .expect("Failed to remove the configuration root hash");

    // Change the sequencer if asked:
    if let Some(sequencer) = sequencer {
        let sequencer_path = concat(&EVM_PATH, &SEQUENCER).unwrap();
        let pk_b58 = PublicKey::to_b58check(&sequencer);
        let bytes = String::as_bytes(&pk_b58);
        host.store_write_all(&sequencer_path, bytes).unwrap();
    }

    // Change the admin if asked:
    if let Some(admin) = admin {
        let sequencer_path = concat(&EVM_PATH, &ADMIN).unwrap();
        let kt1_b58 = admin.to_base58_check();
        let bytes = String::as_bytes(&kt1_b58);
        host.store_write_all(&sequencer_path, bytes).unwrap();
    }

    log!(host, Info, "Done revealing")
}
