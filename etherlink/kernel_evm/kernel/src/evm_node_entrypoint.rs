// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! EVM Node Entrypoints.
//!
//! The module contain functions that may be called by the evm node
//! only. It allows to call specific functions of the kernel without
//! using the inbox and a specific message.

use std::marker::PhantomData;

use crate::{
    delayed_inbox::DelayedInbox, inbox::Transaction, storage::read_logs_verbosity,
};
use tezos_ethereum::rlp_helpers::FromRlpBytes;
use tezos_evm_runtime::{internal_runtime::InternalHost, runtime::KernelHost};
use tezos_smart_rollup_core::rollup_host::RollupHost;
use tezos_smart_rollup_host::{path::RefPath, runtime::Runtime};

const DELAYED_INPUT_PATH: RefPath = RefPath::assert_from(b"/__delayed_input");

#[allow(dead_code)]
#[no_mangle]
pub extern "C" fn populate_delayed_inbox() {
    let sdk_host = unsafe { RollupHost::new() };
    let internal = InternalHost();
    let logs_verbosity = read_logs_verbosity(&sdk_host);
    let mut host = KernelHost {
        host: sdk_host,
        internal,
        _pd: PhantomData,
        logs_verbosity,
    };
    let payload = host.store_read_all(&DELAYED_INPUT_PATH).unwrap();
    let transaction = Transaction::from_rlp_bytes(&payload).unwrap();
    let mut delayed_inbox = DelayedInbox::new(&mut host).unwrap();
    delayed_inbox
        .save_transaction(&mut host, transaction, 0.into(), 0u32)
        .unwrap();
}
