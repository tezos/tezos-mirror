// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use evm_inspectors::TracerInput;
use revm::context::result::ExecutionResult;
use revm_etherlink::EvmRunError;
use tezos_crypto_rs::hash::OperationHash;
use tezos_ethereum::block::BlockConstants;
use tezos_smart_rollup::host::StorageV1;
use tezosx_journal::{CracId, TezosXJournal};

use crate::chains::DebugFeatures;

/// Build the [`TezosXJournal`] for one transaction application. Every
/// journal built here must eventually reach [`close_tezosx_journal`].
#[allow(clippy::too_many_arguments)]
pub fn prepare_tezosx_journal(
    crac_id: CracId,
    operation_hash: &OperationHash,
    block_constants: &BlockConstants,
    http_trace_enabled: bool,
    debug_features: &DebugFeatures,
    internal_operations_base: u128,
    tracer_input: Option<TracerInput>,
) -> TezosXJournal {
    let mut journal =
        TezosXJournal::new(crac_id, operation_hash.clone(), block_constants.clone());
    // Fold the block's prior internal ops into this op's cap (anti-DoS).
    journal
        .michelson
        .set_internal_operation_counter(internal_operations_base);
    journal.set_http_trace_enabled(http_trace_enabled);
    if debug_features.enable_debug_precompiles {
        journal.enable_debug_precompiles();
    }

    if let Some(input) = tracer_input {
        journal
            .evm
            .set_tracer(input.tracer(block_constants.spec_id));
    }

    journal
}

/// Close the journal at the end of a transaction application, finalizing
/// the attached tracer, if any. Must be called on every code path (error
/// paths pass `result: None`) so extending the close logic cannot miss
/// one; taking the journal by value prevents use after close.
pub fn close_tezosx_journal<Host>(
    host: &mut Host,
    mut journal: TezosXJournal,
    result: Option<&ExecutionResult>,
) -> Result<(), EvmRunError>
where
    Host: StorageV1,
{
    if let (Some(mut tracer), Some(result)) = (journal.evm.take_tracer(), result) {
        Ok(tracer.finalize(host, result)?)
    } else {
        Ok(())
    }
}
