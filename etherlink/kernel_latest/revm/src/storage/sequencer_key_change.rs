// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::U256;
use rlp::Encodable;
use tezos_ethereum::keyspace::KeySpaceExtU256;
use tezos_evm_logging::{log, Level::Info};
use tezos_smart_rollup_host::{path::OwnedPath, storage::StorageV1};
use tezos_smart_rollup_keyspace::{KeySpace, KeySpaceError};

use crate::error::EvmDbError;
use crate::storage::world_state_handler::{
    SEQUENCER_KEY_CHANGE_COUNTER_KEY, SEQUENCER_KEY_CHANGE_PATH,
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

/// Reads the sequencer key change counter from the `/evm/world_state`
/// keyspace. Returns zero when the key is absent or holds fewer than 32 bytes,
/// which every caller treats as "no change yet". This is the counter value the
/// *next* change must be signed against.
pub fn read_sequencer_change_counter(evm_world_state: &impl KeySpace) -> U256 {
    evm_world_state.get_evm_u256_le_or(&SEQUENCER_KEY_CHANGE_COUNTER_KEY, U256::ZERO)
}

/// Writes the sequencer key change counter to the `/evm/world_state` keyspace.
/// Errors when the write fails.
pub fn write_sequencer_change_counter(
    evm_world_state: &mut impl KeySpace,
    value: U256,
) -> Result<(), KeySpaceError> {
    evm_world_state.store_evm_u256_le(&SEQUENCER_KEY_CHANGE_COUNTER_KEY, value)
}

/// Increments the sequencer key change counter, so that the calldata of past
/// changes cannot be replayed. Signed (precompile) changes bump at store-time
/// — as soon as the change is stored, invalidating the captured signature —
/// through the layered state (see `LayeredState::store_sequencer_key_change`).
/// This helper is used by unsigned governance changes, which carry no
/// replayable signature and bump at apply-time. Either way a single change
/// advances the counter by exactly one. Errors when the write fails.
pub fn increment_sequencer_change_counter(
    evm_world_state: &mut impl KeySpace,
) -> Result<(), KeySpaceError> {
    let next = read_sequencer_change_counter(evm_world_state).saturating_add(U256::ONE);
    write_sequencer_change_counter(evm_world_state, next)
}
