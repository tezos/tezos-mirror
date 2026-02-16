// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! This module is a testing device, allowing to replicate the state of an existing EVM rollup
//! chain into a new deployment. It is not tick-safe, and should obviously not be used in a
//! production setup.

use crate::configuration::{fetch_chain_configuration, fetch_configuration, CHAIN_ID};
use crate::storage::{read_chain_id, ADMIN};
use primitive_types::U256;
use revm_etherlink::storage::world_state_handler::SEQUENCER_KEY_PATH;
use rlp::{Decodable, DecoderError, Rlp};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_ethereum::rlp_helpers::{decode_field, next, FromRlpBytes};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::ValueType;

const CONFIG_PATH: RefPath = RefPath::assert_from(b"/__tmp/reveal_config");

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
        host.store_has(&CONFIG_PATH).unwrap_or_default(),
        Some(ValueType::Value)
    )
}

pub fn reveal_storage(
    host: &mut impl Runtime,
    sequencer: Option<PublicKey>,
    admin: Option<ContractKt1Hash>,
) {
    log!(host, Info, "Starting the reveal dump");

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

    // Change the sequencer if asked:
    if let Some(sequencer) = sequencer {
        let pk_b58 = PublicKey::to_b58check(&sequencer);
        let bytes = String::as_bytes(&pk_b58);
        host.store_write_all(&SEQUENCER_KEY_PATH, bytes).unwrap();
    }

    // Change the admin if asked:
    if let Some(admin) = admin {
        let kt1_b58 = admin.to_base58_check();
        let bytes = String::as_bytes(&kt1_b58);
        host.store_write(&ADMIN, bytes, 0).unwrap();
    }

    log!(host, Info, "Done revealing");

    let chain_id = read_chain_id(host).unwrap_or(U256::from(CHAIN_ID));
    let chain_config = fetch_chain_configuration(host, chain_id);
    let configuration = fetch_configuration(host);
    log!(host, Info, "Chain Configuration {}", chain_config);
    log!(host, Info, "Configuration {}", configuration);
}
