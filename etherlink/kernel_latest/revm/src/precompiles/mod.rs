// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use evm_inspectors::TracerContainer;
use revm::{context::ContextTr, primitives::Log};
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Registry;

use crate::{database::EtherlinkVMDB, journal::Journal};

pub fn log<'j, Host, R, CTX>(context: &mut CTX, log: Log)
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
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
