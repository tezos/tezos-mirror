// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::blueprint::Blueprint;
use crate::blueprint_storage::{store_inbox_blueprint, store_sequencer_blueprint};
use crate::current_timestamp;
use crate::delayed_inbox::DelayedInbox;
use crate::inbox::read_inbox;
use crate::inbox::InboxContent;
use crate::upgrade::store_kernel_upgrade;
use anyhow::Ok;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::metadata::RAW_ROLLUP_ADDRESS_SIZE;

use tezos_smart_rollup_host::runtime::Runtime;

pub enum Configuration {
    Proxy,
    Sequencer {
        delayed_bridge: ContractKt1Hash,
        delayed_inbox: Box<DelayedInbox>,
        sequencer: PublicKey,
    },
}

impl std::fmt::Display for Configuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Configuration::Proxy => write!(f, "Proxy"),
            Configuration::Sequencer {
                delayed_bridge,
                delayed_inbox: _, // Ignoring delayed_inbox
                sequencer,
            } => write!(
                f,
                "Sequencer {{ delayed_bridge: {:?}, sequencer: {:?} }}",
                delayed_bridge, sequencer
            ),
        }
    }
}

pub fn fetch_inbox_blueprints<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; RAW_ROLLUP_ADDRESS_SIZE],
    ticketer: Option<ContractKt1Hash>,
    admin: Option<ContractKt1Hash>,
) -> Result<(), anyhow::Error> {
    if let Some(InboxContent {
        kernel_upgrade,
        transactions,
        sequencer_blueprints: _,
    }) = read_inbox(host, smart_rollup_address, ticketer, admin, None, None)?
    {
        let timestamp = current_timestamp(host);
        let blueprint = Blueprint {
            transactions,
            timestamp,
        };
        // Store the blueprint.
        store_inbox_blueprint(host, blueprint)?;
        // Store kernel upgrade.
        if let Some(kernel_upgrade) = kernel_upgrade {
            store_kernel_upgrade(host, &kernel_upgrade)?;
        };
    }
    Ok(())
}

fn fetch_sequencer_blueprints<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; RAW_ROLLUP_ADDRESS_SIZE],
    ticketer: Option<ContractKt1Hash>,
    admin: Option<ContractKt1Hash>,
    delayed_bridge: ContractKt1Hash,
    delayed_inbox: &mut DelayedInbox,
    sequencer: PublicKey,
) -> Result<(), anyhow::Error> {
    if let Some(InboxContent {
        kernel_upgrade,
        transactions,
        sequencer_blueprints,
    }) = read_inbox(
        host,
        smart_rollup_address,
        ticketer,
        admin,
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
        // Store kernel upgrade.
        if let Some(kernel_upgrade) = kernel_upgrade {
            store_kernel_upgrade(host, &kernel_upgrade)?;
        };
    }
    Ok(())
}

pub fn fetch<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; RAW_ROLLUP_ADDRESS_SIZE],
    ticketer: Option<ContractKt1Hash>,
    admin: Option<ContractKt1Hash>,
    config: &mut Configuration,
) -> Result<(), anyhow::Error> {
    match config {
        Configuration::Sequencer {
            delayed_bridge,
            delayed_inbox,
            sequencer,
        } => fetch_sequencer_blueprints(
            host,
            smart_rollup_address,
            ticketer,
            admin,
            delayed_bridge.clone(),
            delayed_inbox,
            sequencer.clone(),
        ),
        Configuration::Proxy => {
            fetch_inbox_blueprints(host, smart_rollup_address, ticketer, admin)
        }
    }
}
