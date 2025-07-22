// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

// Every tracers rely on the inspector interface which is triggered when there is actual
// EVM execution. For plain inspector hooks will not be activated at any point.
// This module provides an extensible interface so that a plain transfer/blank call can be
// stored as a proper traced transaction.
// The implemention is ad-hoc for each tracer on purpose to match the default cases expected
// for tooling compatibility.

use crate::world_state_handler::StorageAccount;

use super::{call_tracer::CallTrace, EtherlinkInspector};
use revm::{
    context::result::ExecutionResult,
    primitives::{Address, U256},
};
use tezos_evm_logging::{log, Level::Debug};
use tezos_evm_runtime::runtime::Runtime;

pub fn is_plain_transaction(
    host: &mut impl Runtime,
    destination: &Option<Address>,
) -> bool {
    if let Some(destination) = destination {
        match StorageAccount::from_address(destination) {
            Ok(storage_account) => match storage_account.code(host) {
                Ok(None) => true,
                Ok(Some(bytecode)) => bytecode.is_empty(),
                _ => false,
            },
            Err(err) => {
                // Unexpected case, since we're at tracing level we can't do much since there's
                // no point in having errors. We just log what happened:
                log!(
                    host,
                    Debug,
                    "Unexpected error while checking if tx is a plain one: {err:?}"
                );
                false
            }
        }
    } else {
        // It's a contract creation.
        false
    }
}

pub fn minimal_plain_trace(
    host: &mut impl Runtime,
    inspector: EtherlinkInspector,
    from: Address,
    to: Option<Address>,
    value: U256,
    gas_limit: u64,
    result: &ExecutionResult,
) {
    match inspector {
        EtherlinkInspector::NoOp(_) => (),
        EtherlinkInspector::CallTracer(call_tracer) => {
            let mut call_trace =
                CallTrace::new_minimal_trace("CALL".into(), from, value, vec![], 0);
            call_trace.add_to(to);
            call_trace.add_gas(Some(gas_limit));
            call_trace.add_gas_used(21_000);
            match result {
                ExecutionResult::Success { .. } => (),
                ExecutionResult::Revert { output, .. } => {
                    call_trace.add_error(Some("Reverted".into()));
                    call_trace.add_output(Some(output.to_vec()));
                }
                ExecutionResult::Halt { reason, .. } => {
                    call_trace.add_error(Some(format!("{reason:?}").into()));
                }
            }
            call_trace.store(host, &call_tracer.tx_hash());
        }
    }
}
