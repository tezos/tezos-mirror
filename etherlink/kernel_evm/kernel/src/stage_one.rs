// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::blueprint::Blueprint;
use crate::blueprint_storage::{store_inbox_blueprint, store_sequencer_blueprint};
use crate::current_timestamp;
use crate::inbox::read_inbox;
use crate::inbox::InboxContent;
use crate::storage::store_kernel_upgrade;
use anyhow::Ok;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_host::metadata::RAW_ROLLUP_ADDRESS_SIZE;

use tezos_smart_rollup_host::runtime::Runtime;

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
    }) = read_inbox(host, smart_rollup_address, ticketer, admin)?
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
) -> Result<(), anyhow::Error> {
    if let Some(InboxContent {
        kernel_upgrade,
        transactions: _,
        sequencer_blueprints,
    }) = read_inbox(host, smart_rollup_address, ticketer, admin)?
    {
        // TODO: store delayed inbox messages (transactions).
        // Store the blueprints.
        for seq_blueprint in sequencer_blueprints {
            log!(
                host,
                Debug,
                "Storing chunk {} of sequencer blueprint number {}",
                seq_blueprint.chunk_index,
                seq_blueprint.number
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
    is_sequencer: bool,
) -> Result<(), anyhow::Error> {
    if is_sequencer {
        fetch_sequencer_blueprints(host, smart_rollup_address, ticketer, admin)
    } else {
        fetch_inbox_blueprints(host, smart_rollup_address, ticketer, admin)
    }
}
