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
use rlp::{Rlp, RlpStream};
use tezos_ethereum::rlp_helpers::{
    append_u64_le, decode_field_bool, decode_field_u64_le, next, FromRlpBytes,
};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::KernelHost;
use tezos_smart_rollup_host::{path::RefPath, storage::StorageV1};

#[cfg(target_arch = "wasm32")]
use tezos_smart_rollup_core::rollup_host::RollupHost;

const DELAYED_INPUT_PATH: RefPath = RefPath::assert_from(b"/__delayed_input");

const TEZOSX_SIMULATION_INPUT: RefPath = RefPath::assert_from(b"/__simulation/input");
const TEZOSX_SIMULATION_RESULT: RefPath = RefPath::assert_from(b"/__simulation/result");

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn populate_delayed_inbox() {
    let mut sdk_host = unsafe { RollupHost::new() };
    populate_delayed_inbox_with_durable_storage(&mut sdk_host);
}

#[allow(dead_code)]
pub fn populate_delayed_inbox_with_durable_storage<Host>(host: &mut Host)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
{
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);
    let payload = host.store_read_all(&DELAYED_INPUT_PATH).unwrap();
    let transaction = Transaction::from_rlp_bytes(&payload).unwrap().into();
    let mut delayed_inbox = DelayedInbox::new(&mut host).unwrap();
    delayed_inbox
        .save_transaction(&mut host, transaction, 0.into(), 0u32)
        .unwrap();
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn single_tx_execution() {
    let mut sdk_host = unsafe { RollupHost::new() };
    single_tx_execution_fn(&mut sdk_host);
}

#[allow(dead_code)]
pub fn single_tx_execution_fn<Host>(host: &mut Host)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
{
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);
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

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn assemble_block() {
    let mut sdk_host = unsafe { RollupHost::new() };
    assemble_block_fn(&mut sdk_host);
}

#[allow(dead_code)]
pub fn assemble_block_fn<Host>(host: &mut Host)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
{
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);
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

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn tezosx_simulate() {
    let mut sdk_host = unsafe { RollupHost::new() };
    tezosx_simulate_fn(&mut sdk_host);
}

#[allow(dead_code)]
pub fn tezosx_simulate_fn<Host>(host: &mut Host)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
{
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);
    let input = match host.store_read_all(&TEZOSX_SIMULATION_INPUT) {
        Ok(bytes) => bytes,
        Err(err) => {
            log!(
                host,
                Error,
                "Error reading Tezos X simulation input: {:?}",
                err
            );
            return;
        }
    };
    // Scaffold: decode RLP input [skip_signature_check_bool, n_u64_le_bytes],
    // compute successor, write RLP-encoded result back.
    let rlp = Rlp::new(&input);
    let mut it = rlp.iter();
    let _skip_signature_check: bool =
        match next(&mut it).and_then(|f| decode_field_bool(&f, "skip_signature_check")) {
            Ok(v) => v,
            Err(err) => {
                log!(
                    host,
                    Error,
                    "Tezos X simulation: failed to decode input: {:?}",
                    err
                );
                return;
            }
        };
    let n: u64 = match next(&mut it).and_then(|f| decode_field_u64_le(&f, "n")) {
        Ok(v) => v,
        Err(err) => {
            log!(
                host,
                Error,
                "Tezos X simulation: failed to decode input: {:?}",
                err
            );
            return;
        }
    };
    let result = n.wrapping_add(1);
    let mut stream = RlpStream::new();
    append_u64_le(&mut stream, &result);
    if let Err(err) = host.store_write_all(&TEZOSX_SIMULATION_RESULT, &stream.out()) {
        log!(
            host,
            Error,
            "Error writing Tezos X simulation result: {:?}",
            err
        );
    }
}
