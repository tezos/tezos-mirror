// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use evm_inspectors::{Tracer, TracerInput};
use revm::primitives::hardfork::SpecId;
use tezos_crypto_rs::hash::OperationHash;
use tezos_ethereum::block::BlockConstants;
use tezosx_journal::{CracId, TezosXJournal};

use crate::chains::DebugFeatures;

#[allow(clippy::too_many_arguments)]
pub fn prepare_tezosx_journal(
    crac_id: CracId,
    operation_hash: &OperationHash,
    block_constants: &BlockConstants,
    spec_id: &SpecId,
    http_trace_enabled: bool,
    debug_features: &DebugFeatures,
    internal_operations_base: u128,
    tracer_input: Option<TracerInput>,
) -> (TezosXJournal, Option<Tracer>) {
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

    let tracer = tracer_input.map(|input| input.tracer(*spec_id));

    (journal, tracer)
}
