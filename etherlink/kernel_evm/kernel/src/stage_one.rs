// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::blueprint::Blueprint;
use crate::blueprint_storage::{store_inbox_blueprint, store_sequencer_blueprint};
use crate::configuration::{Configuration, ConfigurationMode, TezosContracts};
use crate::current_timestamp;
use crate::delayed_inbox::DelayedInbox;
use crate::inbox::read_inbox;
use crate::inbox::InboxContent;
use anyhow::Ok;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::metadata::RAW_ROLLUP_ADDRESS_SIZE;

use tezos_smart_rollup_host::runtime::Runtime;

pub fn fetch_inbox_blueprints<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; RAW_ROLLUP_ADDRESS_SIZE],
    tezos_contracts: &TezosContracts,
) -> Result<(), anyhow::Error> {
    if let Some(InboxContent {
        transactions,
        sequencer_blueprints: _,
    }) = read_inbox(host, smart_rollup_address, tezos_contracts, None, None)?
    {
        let timestamp = current_timestamp(host);
        let blueprint = Blueprint {
            transactions,
            timestamp,
        };
        // Store the blueprint.
        store_inbox_blueprint(host, blueprint)?;
    }
    Ok(())
}

fn fetch_sequencer_blueprints<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; RAW_ROLLUP_ADDRESS_SIZE],
    tezos_contracts: &TezosContracts,
    delayed_bridge: ContractKt1Hash,
    delayed_inbox: &mut DelayedInbox,
    sequencer: PublicKey,
) -> Result<(), anyhow::Error> {
    if let Some(InboxContent {
        transactions,
        sequencer_blueprints,
    }) = read_inbox(
        host,
        smart_rollup_address,
        tezos_contracts,
        Some(delayed_bridge),
        Some(sequencer),
    )? {
        // Store the transactions in the delayed inbox.
        for transaction in transactions {
            delayed_inbox.save_transaction(host, transaction)?;
        }

        // Store the blueprints.
        for seq_blueprint in sequencer_blueprints {
            log!(
                host,
                Debug,
                "Storing chunk {} of sequencer blueprint number {}",
                seq_blueprint.blueprint.chunk_index,
                seq_blueprint.blueprint.number
            );
            store_sequencer_blueprint(host, seq_blueprint)?
        }
    }
    Ok(())
}

pub fn fetch<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; RAW_ROLLUP_ADDRESS_SIZE],
    config: &mut Configuration,
) -> Result<(), anyhow::Error> {
    match &mut config.mode {
        ConfigurationMode::Sequencer {
            delayed_bridge,
            delayed_inbox,
            sequencer,
        } => fetch_sequencer_blueprints(
            host,
            smart_rollup_address,
            &config.tezos_contracts,
            delayed_bridge.clone(),
            delayed_inbox,
            sequencer.clone(),
        ),
        ConfigurationMode::Proxy => {
            fetch_inbox_blueprints(host, smart_rollup_address, &config.tezos_contracts)
        }
    }
}
