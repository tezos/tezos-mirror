// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::U256;
use rlp::Encodable;
use tezos_evm_logging::{log, Level::Info};
use tezos_smart_rollup_host::{path::OwnedPath, storage::StorageV1};

use crate::error::EvmDbError;
use crate::helpers::storage::{read_u256_le_default, write_u256_le};
use crate::storage::world_state_handler::{
    SEQUENCER_KEY_CHANGE_COUNTER_PATH, SEQUENCER_KEY_CHANGE_PATH,
};
pub use evm_types::SequencerKeyChange;

pub fn store_sequencer_key_change<Host>(
    host: &mut Host,
    sequencer_key_change: SequencerKeyChange,
) -> Result<(), EvmDbError>
where
    Host: StorageV1,
{
    log!(
        Info,
        "An L2 based sequencer key change is planned: {}",
        sequencer_key_change
    );
    let bytes = &sequencer_key_change.rlp_bytes();
    let path = OwnedPath::from(SEQUENCER_KEY_CHANGE_PATH);
    host.store_write_all(&path, bytes)?;
    Ok(())
}

/// Reads the sequencer key change counter, defaulting to zero when no
/// change has happened yet. This is the counter value the *next* change
/// must be signed against.
pub fn read_sequencer_change_counter<Host>(host: &Host) -> Result<U256, EvmDbError>
where
    Host: StorageV1,
{
    Ok(read_u256_le_default(
        host,
        &SEQUENCER_KEY_CHANGE_COUNTER_PATH,
        U256::ZERO,
    )?)
}

/// Writes the sequencer key change counter to durable storage.
pub fn write_sequencer_change_counter<Host>(
    host: &mut Host,
    value: U256,
) -> Result<(), EvmDbError>
where
    Host: StorageV1,
{
    write_u256_le(host, &SEQUENCER_KEY_CHANGE_COUNTER_PATH, value)?;
    Ok(())
}

/// Increments the sequencer key change counter, so that the calldata of past
/// changes cannot be replayed. Signed (precompile) changes bump at store-time
/// — as soon as the change is stored, invalidating the captured signature —
/// through the layered state (see `LayeredState::store_sequencer_key_change`).
/// This helper is used by unsigned governance changes, which carry no
/// replayable signature and bump at apply-time. Either way a single change
/// advances the counter by exactly one.
pub fn increment_sequencer_change_counter<Host>(host: &mut Host) -> Result<(), EvmDbError>
where
    Host: StorageV1,
{
    let next = read_sequencer_change_counter(host)?.saturating_add(U256::ONE);
    write_sequencer_change_counter(host, next)
}
