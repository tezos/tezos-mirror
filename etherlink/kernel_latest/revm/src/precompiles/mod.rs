// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use evm_inspectors::TracerContainer;
use revm::{context::ContextTr, primitives::Log};
use tezosx_interfaces::Registry;

use crate::{database::EtherlinkVMDB, journal::Journal};
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::KeySpaceLoader;

pub fn log<'j, 'host, Host, R, CTX>(context: &mut CTX, log: Log)
where
    'host: 'j,
    Host: StorageV1 + KeySpaceLoader + 'host,
    R: Registry<Journal = tezosx_journal::TezosXJournal> + 'j,
    CTX: ContextTr<
        Db = EtherlinkVMDB<'j, 'host, Host, R>,
        Journal = Journal<'j, 'host, Host, R>,
    >,
{
    if let Some(mut tracer) = context.journal_mut().take_tracer() {
        tracer.inject_log(log.clone());
        context.journal_mut().restore_tracer(Some(tracer));
    }

    context.journal_mut().log(log);
}

pub mod change_sequencer_key;
pub mod constants;
pub mod initializer;
pub mod provider;
pub mod runtime_gateway;
pub mod send_outbox_message;

mod global_counter;
mod guard;
mod panic;
mod table;
mod verify_tezos_signature;
