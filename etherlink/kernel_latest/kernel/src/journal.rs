// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::str::FromStr;

use evm_inspectors::{get_tracer_configuration, TracerInput};
use revm::context::result::ExecutionResult;
use revm::primitives::Address;
use revm_etherlink::{EvmRunError, ExecutionOutcome};
use tezos_ethereum::block::BlockConstants;
use tezos_evm_logging::{
    log,
    Level::{Debug, Error, Fatal},
};
use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
use tezos_smart_rollup::host::StorageV1;
use tezos_smart_rollup_keyspace::KeySpaceLoader;
use tezos_tezlink::operation::ManagerOperationField;
use tezosx_interfaces::{canonicalize_native_address, AliasInfo, Registry, RuntimeId};
use tezosx_journal::{CracId, TezosXJournal};

use crate::chains::{
    tezos_op_evm_gas_limit, DebugFeatures, TezlinkContent, TezlinkOperation,
};

pub use tezosx_journal::TezosXHashes;

/// Build the [`TezosXJournal`] for one transaction application. Every
/// journal built here must eventually reach [`close_tezosx_journal`].
#[allow(clippy::too_many_arguments)]
fn prepare_tezosx_journal(
    crac_id: CracId,
    operation_hashes: &TezosXHashes,
    block_constants: &BlockConstants,
    http_trace_enabled: bool,
    debug_features: &DebugFeatures,
    internal_operations_base: u128,
    tracer_input: Option<TracerInput>,
) -> TezosXJournal {
    let mut journal =
        TezosXJournal::new(crac_id, operation_hashes.clone(), block_constants.clone());
    // Fold the block's prior internal ops into this op's cap (anti-DoS).
    journal
        .michelson
        .set_internal_operation_counter(internal_operations_base);
    journal.set_http_trace_enabled(http_trace_enabled);
    if debug_features.enable_debug_precompiles {
        journal.enable_debug_precompiles();
    }

    let tracer_input = get_tracer_configuration(operation_hashes.evm, tracer_input);

    if let Some(input) = tracer_input {
        journal
            .evm
            .set_tracer(input.tracer(block_constants.spec_id));
    }

    journal
}

/// Close the journal at the end of a transaction application, performing
/// the necessary bookkeeping required by each supported runtime.
///
/// Must be called on every code path (error paths pass `result: None`) so
/// extending the close logic cannot miss one; taking the journal by value
/// prevents use after close.
///
/// Also reports a leaked EVM frame checkpoint, which mis-points the CRAC receipt
/// watermarks. A log and not an assertion: `run_transaction` leaks one until
/// !22693 lands.
fn close_tezosx_journal<Host>(
    host: &mut Host,
    mut journal: TezosXJournal,
    result: Option<&ExecutionResult>,
) -> Result<(), EvmRunError>
where
    Host: StorageV1,
{
    let open_frames = journal.michelson.open_frame_count();
    if open_frames > 0 {
        log!(
            Fatal,
            "closing the journal with {} EVM frame checkpoint(s) still open: a \
             frame was neither committed nor reverted, so the cross-runtime call \
             receipt watermarks no longer match the receipts they index.",
            open_frames
        );
    }
    // Michelson temporary big maps are scoped to the transaction, not just to
    // the frame that allocated them. Failing to clear them leaves dead entries
    // in the durable store, which is not worth losing the transaction over.
    if let Err(err) = tezos_execution::mir_ctx::clear_temporary_big_maps(
        host,
        &journal.michelson.temporary_big_map_id_allocator(),
    ) {
        log!(Error, "Failed to clear the temporary big maps: {err}");
    }
    if let (Some(mut tracer), Some(result)) = (journal.evm.take_tracer(), result) {
        Ok(tracer.finalize(host, result)?)
    } else {
        Ok(())
    }
}

pub trait HasExecutionResult {
    fn result(&self) -> Option<&ExecutionResult>;
}

impl HasExecutionResult for ExecutionOutcome {
    fn result(&self) -> Option<&ExecutionResult> {
        Some(&self.result)
    }
}

impl HasExecutionResult for ExecutionResult {
    fn result(&self) -> Option<&ExecutionResult> {
        Some(self)
    }
}

impl<T: HasExecutionResult> HasExecutionResult for Option<T> {
    fn result(&self) -> Option<&ExecutionResult> {
        self.as_ref().and_then(HasExecutionResult::result)
    }
}

impl<T: HasExecutionResult, E> HasExecutionResult for Result<T, E> {
    fn result(&self) -> Option<&ExecutionResult> {
        self.as_ref().ok().and_then(HasExecutionResult::result)
    }
}

impl<T: HasExecutionResult, U> HasExecutionResult for (T, U) {
    fn result(&self) -> Option<&ExecutionResult> {
        self.0.result()
    }
}

/// Prepare a journal, run the continuation, and close it on success or error.
///
/// The continuation supplies the EVM result used to finalize the tracer, or
/// `None` for invalid operations. Errors close without finalizing the tracer.
/// A close error takes precedence over the continuation's error, as at the
/// explicit prepare/close call sites. Panics do not run the close step.
#[allow(clippy::too_many_arguments)]
pub fn with_tezosx_journal<'host, Host: StorageV1 + KeySpaceLoader, T, E>(
    rk: &mut RuntimeKeyspaces<'host, Host>,
    crac_id: CracId,
    operation_hashes: &TezosXHashes,
    block_constants: &BlockConstants,
    http_trace_enabled: bool,
    debug_features: &DebugFeatures,
    internal_operations_base: u128,
    tracer_input: Option<TracerInput>,
    apply: impl FnOnce(&mut RuntimeKeyspaces<'host, Host>, &mut TezosXJournal) -> Result<T, E>,
) -> Result<T, E>
where
    E: From<EvmRunError>,
    T: HasExecutionResult,
{
    let mut journal = prepare_tezosx_journal(
        crac_id,
        operation_hashes,
        block_constants,
        http_trace_enabled,
        debug_features,
        internal_operations_base,
        tracer_input,
    );
    let outcome = apply(rk, &mut journal);
    close_tezosx_journal(rk.host_mut(), journal, outcome.result())?;
    outcome
}

/// Open the tracer's top-level frame mirroring the fake EVM transaction
/// [op] registers if it CRACs into the EVM runtime, so the frames of the
/// EVM calls it dispatches nest below it. Balanced by
/// [`TezosXJournal::fake_top_level_call_end`] at the apply site.
pub fn fake_top_level_call_from_tezos_operation(
    journal: &mut TezosXJournal,
    registry: &impl Registry,
    op: &TezlinkOperation,
    michelson_to_evm_gas_multiplier: u64,
) {
    if !journal.evm.has_tracer() {
        return;
    }
    // Deposits carry no Michelson source and issue no CRAC, so there is
    // no envelope to mirror.
    let TezlinkContent::Tezos(operation) = &op.content else {
        return;
    };
    let Some(Ok(source)) = operation.content.first().map(|c| c.source()) else {
        return;
    };
    let alias = registry.compute_alias(&AliasInfo {
        runtime: RuntimeId::Ethereum,
        native_address: canonicalize_native_address(
            RuntimeId::Tezos,
            &source.to_b58check(),
        ),
    });
    let caller = match alias.map(|alias| Address::from_str(&alias)) {
        Ok(Ok(caller)) => caller,
        failure => {
            log!(
                Debug,
                "Tracing: could not derive the EVM alias of {}: {failure:?}",
                source.to_b58check()
            );
            return;
        }
    };
    let gas_limit = match tezos_op_evm_gas_limit(op, michelson_to_evm_gas_multiplier) {
        Ok(gas_limit) => gas_limit,
        Err(err) => {
            log!(
                Debug,
                "Tracing: could not derive the EVM gas limit of the operation: {err:?}"
            );
            return;
        }
    };

    journal.fake_top_level_call(caller, gas_limit);
}
