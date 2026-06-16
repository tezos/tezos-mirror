// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use rlp::Encodable;
use tezos_evm_logging::{log, Level::Info};
use tezos_smart_rollup_host::{path::OwnedPath, storage::StorageV1};

use crate::error::EvmDbError;
use crate::storage::world_state_handler::SEQUENCER_KEY_CHANGE_PATH;
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
