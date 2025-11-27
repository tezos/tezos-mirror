// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! EVM Node Entrypoints.
//!
//! The module contain functions that may be called by the evm node
//! only. It allows to call specific functions of the kernel without
//! using the inbox and a specific message.

use crate::{delayed_inbox::DelayedInbox, sub_block, transaction::Transaction};
use tezos_ethereum::rlp_helpers::FromRlpBytes;
use tezos_evm_logging::{log, Level::*, Verbosity};
use tezos_evm_runtime::{
    internal_runtime::{InternalRuntime, WasmInternalHost},
    runtime::KernelHost,
};
use tezos_smart_rollup_core::rollup_host::RollupHost;
use tezos_smart_rollup_host::{path::RefPath, runtime::Runtime};

const DELAYED_INPUT_PATH: RefPath = RefPath::assert_from(b"/__delayed_input");

#[allow(dead_code)]
#[no_mangle]
pub extern "C" fn populate_delayed_inbox() {
    let mut sdk_host = unsafe { RollupHost::new() };
    populate_delayed_inbox_with_durable_storage(&mut sdk_host, WasmInternalHost());
}

pub fn populate_delayed_inbox_with_durable_storage<Host, I>(host: &mut Host, internal: I)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
    I: InternalRuntime,
{
    let mut host: KernelHost<Host, &mut Host, I> = KernelHost::init(host, internal);
    let payload = host.store_read_all(&DELAYED_INPUT_PATH).unwrap();
    let transaction = Transaction::from_rlp_bytes(&payload).unwrap();
    let mut delayed_inbox = DelayedInbox::new(&mut host).unwrap();
    delayed_inbox
        .save_transaction(&mut host, transaction, 0.into(), 0u32)
        .unwrap();
}

#[allow(dead_code)]
#[no_mangle]
pub extern "C" fn single_tx_execution() {
    let mut sdk_host = unsafe { RollupHost::new() };
    single_tx_execution_fn(&mut sdk_host, WasmInternalHost());
}

pub fn single_tx_execution_fn<Host, I>(host: &mut Host, internal: I)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
    I: InternalRuntime,
{
    let mut host: KernelHost<Host, &mut Host, I> = KernelHost::init(host, internal);
    let tx_input = match sub_block::read_single_tx_execution_input(&mut host) {
        Ok(Some(input)) => input,
        Ok(None) => {
            log!(
                host,
                Error,
                "No single transaction execution input found in storage"
            );
            return;
        }
        Err(err) => {
            log!(
                host,
                Error,
                "Error while reading single transaction execution input: {:?}",
                err
            );
            return;
        }
    };
    match sub_block::handle_run_transaction(&mut host, tx_input) {
        Ok(()) => (),
        Err(err) => {
            log!(
                host,
                Error,
                "Error during single transaction execution: {:?}",
                err
            );
        }
    }
}

#[allow(dead_code)]
#[no_mangle]
pub extern "C" fn assemble_block() {
    let mut sdk_host = unsafe { RollupHost::new() };
    assemble_block_fn(&mut sdk_host, WasmInternalHost());
}

pub fn assemble_block_fn<Host, I>(host: &mut Host, internal: I)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
    I: InternalRuntime,
{
    let mut host: KernelHost<Host, &mut Host, I> = KernelHost::init(host, internal);
    let assemble_block_input = match sub_block::read_assemble_block_input(&mut host) {
        Ok(Some(input)) => input,
        Ok(None) => {
            log!(host, Error, "No assemble block input found in storage");
            return;
        }
        Err(err) => {
            log!(
                host,
                Error,
                "Error while reading assemble block input: {:?}",
                err
            );
            return;
        }
    };
    match sub_block::assemble_block(&mut host, assemble_block_input) {
        Ok(()) => (),
        Err(err) => {
            log!(host, Error, "Error while assembling block: {:?}", err);
        }
    }
}
