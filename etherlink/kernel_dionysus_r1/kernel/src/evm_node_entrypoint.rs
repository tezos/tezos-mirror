// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! EVM Node Entrypoints.
//!
//! The module contain functions that may be called by the evm node
//! only. It allows to call specific functions of the kernel without
//! using the inbox and a specific message.

use crate::{delayed_inbox::DelayedInbox, transaction::Transaction};
use tezos_ethereum::rlp_helpers::FromRlpBytes;
use tezos_evm_runtime::{internal_runtime::InternalRuntime, runtime::KernelHost};
use tezos_smart_rollup_host::{path::RefPath, runtime::Runtime};

const DELAYED_INPUT_PATH: RefPath = RefPath::assert_from(b"/__delayed_input");

pub fn populate_delayed_inbox<Host: Runtime + InternalRuntime>(sdk_host: &mut Host) {
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(sdk_host);
    let payload = host.store_read_all(&DELAYED_INPUT_PATH).unwrap();
    let transaction = Transaction::from_rlp_bytes(&payload).unwrap();
    let mut delayed_inbox = DelayedInbox::new(&mut host).unwrap();
    delayed_inbox
        .save_transaction(&mut host, transaction, 0.into(), 0u32)
        .unwrap();
}
