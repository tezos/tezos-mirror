// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! This module is a testing device, allowing to replicate the state of an existing EVM rollup
//! chain into a new deployment. It is not tick-safe, and should obviously not be used in a
//! production setup.

use crate::configuration::{fetch_configuration, fetch_tezosx_configuration};
use crate::storage::ADMIN_KEY;
use revm_etherlink::storage::world_state_handler::SEQUENCER_KEY_PATH;
use rlp::{Decodable, DecoderError, Rlp};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_ethereum::rlp_helpers::{decode_field, next, FromRlpBytes};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::IsEvmNode;
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{Key, KeySpace};

// Key to the temporary reveal configuration, inside the `/base` keyspace.
// Resolves to the durable path `/base/__tmp/reveal_config`.
const CONFIG_KEY: Key = Key::from_static(b"/__tmp/reveal_config");

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

pub fn is_revealed_storage(base: &impl KeySpace) -> bool {
    base.contains(&CONFIG_KEY)
}

pub fn reveal_storage<Host>(
    host: &mut Host,
    base: &mut impl KeySpace,
    sequencer: Option<PublicKey>,
    admin: Option<ContractKt1Hash>,
) where
    Host: StorageV1 + IsEvmNode,
{
    log!(Info, "Starting the reveal dump");

    let config_bytes = base
        .get(&CONFIG_KEY)
        .expect("Failed reading the configuration");

    // Decode the RLP list of instructions.
    let sets =
        Sets::from_rlp_bytes(&config_bytes).expect("Failed to decode the list of set");
    let length = sets.0.len();

    for (index, Set { to, value }) in sets.0.iter().enumerate() {
        if index % 50_000 == 0 {
            log!(Info, "{}/{}", index, length)
        };
        host.store_write_all(to, value)
            .expect("Failed to write value");
    }

    // Remove temporary configuration
    base.delete(&CONFIG_KEY);

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
        base.set(&ADMIN_KEY, bytes).unwrap();
    }

    log!(Info, "Done revealing");

    let chain_config = fetch_tezosx_configuration(host, base);
    let configuration = fetch_configuration(host, base);
    log!(Info, "Chain Configuration {chain_config:?}");
    log!(Info, "Configuration {}", configuration);
}
