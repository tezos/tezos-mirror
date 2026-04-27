// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023, 2025-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolCall};
use anyhow::anyhow;
use mir::ast::ChainId;
use primitive_types::{H160, U256};
use revm::primitives::alloy_primitives::IntoLogData;
use revm::primitives::hardfork::SpecId;
use revm::primitives::{Address, Bytes, B256};
use revm_etherlink::helpers::legacy::{alloy_to_h160, FaDeposit, FaDepositWithProxy};
use revm_etherlink::inspectors::call_tracer::CallTracerInput;
use revm_etherlink::inspectors::struct_logger::StructLoggerInput;
use revm_etherlink::inspectors::{get_tracer_configuration, TracerInput};
use revm_etherlink::journal::commit_evm_journal_from_external;
use revm_etherlink::precompiles::constants::{
    FA_BRIDGE_SOL_ADDR, FA_DEPOSIT_EXECUTION_COST, FEED_DEPOSIT_ADDR,
    RUNTIME_GATEWAY_PRECOMPILE_ADDRESS, XTZ_BRIDGE_SOL_ADDR, XTZ_DEPOSIT_EXECUTION_COST,
};
use revm_etherlink::precompiles::send_outbox_message::{
    FastWithdrawalInterface, RouterInterface, Withdrawal,
};
use revm_etherlink::storage::world_state_handler::StorageAccount;
use revm_etherlink::GasData;
use revm_etherlink::{
    helpers::legacy::{h160_to_alloy, u256_to_alloy},
    ExecutionOutcome, TransactionOrigin,
};
use tezos_crypto_rs::hash::HashTrait;
use tezos_ethereum::block::{BlockConstants, BlockFees};
use tezos_ethereum::transaction::{
    TransactionHash, TransactionObject, TransactionType, TRANSACTION_HASH_SIZE,
};
use tezos_ethereum::tx_common::{
    signed_authorization, AuthorizationList, EthereumTransactionCommon,
};
use tezos_evm_logging::{log, tracing::instrument, Level::*};
use tezos_execution::context::Context;
use tezos_execution::mir_ctx::BlockCtx;
use tezos_execution::ProcessedOperation;
use tezos_smart_rollup::outbox::{OutboxMessage, OutboxQueue};
use tezos_smart_rollup::types::Timestamp;
use tezos_smart_rollup_host::path::{Path, RefPath};
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_tezlink::block::AppliedOperation;
use tezos_tezlink::enc_wrappers::BlockNumber;
use tezos_tezlink::operation_result::{
    ApplyOperationErrors, ContentResult, OperationBatchWithMetadata,
    OperationDataAndMetadata, OperationError, OperationResultSum,
};
use tezos_tracing::trace_kernel;
use tezosx_interfaces::{Registry, RuntimeId};
use tezosx_journal::{CracId, TezosXJournal};
use tezosx_tezos_runtime::context::TezosRuntimeContext;

use crate::bridge::{apply_tezosx_xtz_deposit, Deposit};
use crate::chains::{EvmLimits, TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH};
use crate::error::Error;
use crate::fees::{self, tx_execution_gas_limit, FeeUpdates};
use crate::transaction::{Transaction, TransactionContent};

sol! {
    /// Emitted once at the top of every EVM transaction receipt that
    /// involves cross-runtime calls, whether incoming or outgoing.
    /// Allows indexers to correlate operations across derived blocks.
    event CracIdEvent(string cracId);
}

pub struct TransactionReceiptInfo {
    pub tx_hash: TransactionHash,
    pub index: u32,
    pub execution_outcome: ExecutionOutcome,
    pub caller: H160,
    pub to: Option<H160>,
    pub effective_gas_price: U256,
    pub type_: TransactionType,
    pub overall_gas_used: U256,
}

#[inline(always)]
#[allow(clippy::too_many_arguments)]
fn make_receipt_info(
    tx_hash: TransactionHash,
    index: u32,
    execution_outcome: ExecutionOutcome,
    caller: H160,
    to: Option<H160>,
    effective_gas_price: U256,
    type_: TransactionType,
    overall_gas_used: U256,
) -> TransactionReceiptInfo {
    TransactionReceiptInfo {
        tx_hash,
        index,
        execution_outcome,
        caller,
        to,
        effective_gas_price,
        type_,
        overall_gas_used,
    }
}

#[inline(always)]
fn make_object(
    block_number: U256,
    transaction: Transaction,
    from: H160,
    index: u32,
    fee_updates: &FeeUpdates,
) -> Result<TransactionObject, anyhow::Error> {
    let (gas, gas_price) = match &transaction.content {
        TransactionContent::Ethereum(e) | TransactionContent::EthereumDelayed(e) => {
            (e.gas_limit_with_fees().into(), e.max_fee_per_gas)
        }
        TransactionContent::Deposit(_) | TransactionContent::FaDeposit(_) => {
            // The gas and gas_price fields of the operation object
            // represent the gas limit and max fee per gas of the
            // input transaction; since deposits and FA deposits are
            // crafted ex nihilo, we don't have a gas limit nor a max
            // fee per gas in these cases. For lack of a better
            // alternative, we put the used gas and the actual gas
            // price instead.
            (fee_updates.overall_gas_used, fee_updates.overall_gas_price)
        }
        TransactionContent::TezosDelayed(_) => {
            // Gas and gas price are not yet part of the Michelson
            // runtime cost model.
            (fee_updates.overall_gas_used, fee_updates.overall_gas_price)
        }
    };

    let hash = transaction.tx_hash;
    let nonce = transaction.nonce();
    let to = transaction.to()?;
    let value = transaction.value();
    let signature = transaction.signature();

    Ok(TransactionObject {
        block_number,
        from,
        gas,
        gas_price,
        hash,
        input: transaction.data(),
        nonce,
        to,
        index,
        value,
        signature,
    })
}

#[derive(Debug, PartialEq)]
pub enum Validity {
    Valid(H160, u64),
    InvalidChainId,
    InvalidSignature,
    InvalidNonce,
    InvalidPrePay,
    InvalidCode,
    InvalidMaxBaseFee,
    InvalidNotEnoughGasForFees,
}

// TODO: https://gitlab.com/tezos/tezos/-/issues/6812
//       arguably, effective_gas_price should be set on EthereumTransactionCommon
//       directly - initialised when constructed.
#[instrument(skip_all)]
pub fn is_valid_ethereum_transaction_common<Host>(
    host: &mut Host,
    transaction: &EthereumTransactionCommon,
    block_constant: &BlockConstants,
    effective_gas_price: U256,
    is_delayed: bool,
    limits: &EvmLimits,
) -> Result<Validity, Error>
where
    Host: StorageV1,
{
    // Chain id is correct.
    if transaction.chain_id.is_some()
        && Some(block_constant.chain_id) != transaction.chain_id
    {
        log!(Benchmarking, "Transaction status: ERROR_CHAINID");
        return Ok(Validity::InvalidChainId);
    }

    // ensure that the user was willing to at least pay the base fee
    if transaction.max_fee_per_gas < block_constant.base_fee_per_gas() {
        log!(Benchmarking, "Transaction status: ERROR_MAX_BASE_FEE");
        return Ok(Validity::InvalidMaxBaseFee);
    }

    // The transaction signature is valid.
    let caller = match transaction.caller() {
        Ok(caller) => caller,
        Err(_err) => {
            log!(Benchmarking, "Transaction status: ERROR_SIGNATURE.");
            // Transaction with undefined caller are ignored, i.e. the caller
            // could not be derived from the signature.
            return Ok(Validity::InvalidSignature);
        }
    };

    let account = StorageAccount::from_address(&h160_to_alloy(&caller))?;
    let info = account.info(host)?;

    // The transaction nonce is valid.
    if info.nonce != transaction.nonce {
        log!(Benchmarking, "Transaction status: ERROR_NONCE.");
        return Ok(Validity::InvalidNonce);
    };

    // The sender account balance contains at least the cost.
    let total_gas_limit = U256::from(transaction.gas_limit_with_fees());
    let cost = total_gas_limit.saturating_mul(effective_gas_price);
    // The sender can afford the max gas fee he set, see EIP-1559
    let max_fee = total_gas_limit.saturating_mul(transaction.max_fee_per_gas);

    if info.balance < u256_to_alloy(&cost) || info.balance < u256_to_alloy(&max_fee) {
        log!(Benchmarking, "Transaction status: ERROR_PRE_PAY.");
        return Ok(Validity::InvalidPrePay);
    }

    if let Some(code) = revm_etherlink::storage::code::CodeStorage::new(&info.code_hash)?
        .get_code(host)?
    {
        // The sender does not have code (EIP-3607) or isn't an EIP-7702 authorized account.
        if !code.is_empty()
            && !code.original_byte_slice().starts_with(&[0xef, 0x01, 0x00])
        {
            log!(Benchmarking, "Transaction status: ERROR_CODE.");
            return Ok(Validity::InvalidCode);
        }
    }

    // check that enough gas is provided to cover fees
    let Ok(gas_limit) =
        tx_execution_gas_limit(transaction, &block_constant.block_fees, is_delayed)
    else {
        log!(Benchmarking, "Transaction status: ERROR_GAS_FEE.");
        return Ok(Validity::InvalidNotEnoughGasForFees);
    };
    let capped_gas_limit = u64::min(gas_limit, limits.maximum_gas_limit);
    Ok(Validity::Valid(caller, capped_gas_limit))
}

/// Result of executing an Ethereum transaction.
pub struct EthereumTransactionResult {
    pub caller: H160,
    pub execution_outcome: ExecutionOutcome,
    /// CRAC receipts for Michelson operations triggered during this EVM tx.
    pub crac_receipts: Vec<AppliedOperation>,
}

/// Enum distinguishing between Ethereum and Tezos transaction results.
pub enum RuntimeTransactionResult {
    Ethereum(EthereumTransactionResult),
    Tezos {
        op: AppliedOperation,
        etherlink_withdrawals: Vec<Withdrawal>,
        cross_runtime_effects: Vec<CrossRuntimeEffect>,
        /// Total milligas consumed by the batch (from gas tracker).
        consumed_milligas: u64,
    },
}

/// Extract cross-runtime side effects accumulated in the journal
/// during a Michelson transaction that may have CRACed into EVM.
pub fn extract_cross_runtime_effects(
    journal: &mut TezosXJournal,
    consumed_milligas: u64,
) -> Vec<CrossRuntimeEffect> {
    let mut effects = Vec::new();

    if let Some(tx_info) = journal.evm.take_crac_data() {
        let crac_id = journal.crac_id().to_string();

        // Build a synthetic CracIdEvent log as the first log in the receipt.
        let crac_id_log = revm::primitives::Log {
            address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
            data: CracIdEvent {
                cracId: crac_id.clone(),
            }
            .to_log_data(),
        };

        let mut logs = vec![crac_id_log];
        logs.extend(journal.evm.inner.logs.iter().cloned());

        effects.push(CrossRuntimeEffect::Evm(EvmCracEffect {
            crac_id,
            logs,
            source: H160(*tx_info.source.0),
            sender: H160(*tx_info.sender.0),
            gas_limit: U256::from_little_endian(&tx_info.gas_limit.to_le_bytes::<32>()),
            amount: U256::from_little_endian(&tx_info.amount.to_le_bytes::<32>()),
            gas_used: U256::from(
                tezosx_interfaces::gas::convert(
                    RuntimeId::Tezos,
                    RuntimeId::Ethereum,
                    consumed_milligas,
                )
                .unwrap_or(0),
            ),
        }));
    }

    effects
}

/// Drain CRAC receipts from the journal, merging all CRACs from the same
/// EVM transaction into a single manager operation.
///
/// Per RFC principle 6 ("CRAC-ID per top-level transaction"), all CRAC
/// sub-calls within one EVM transaction share one CRAC-ID and therefore
/// produce a single top-level Michelson operation with all internal
/// transactions merged and one CRAC event (RFC Example 5).
///
/// Both successful and failed receipts must be merged: failed receipts
/// alone (e.g. several `try { CRAC(); } catch {}` blocks all reverting on
/// the Michelson side) are otherwise emitted as separate top-level
/// operations, violating principle 6.
///
/// Receipts carry a monotonic sequence number assigned at push time
/// (shared across the pending and failed lists), so sorting by that
/// sequence recovers the original execution order regardless of which
/// list each receipt landed in.  This matters when an EVM transaction
/// interleaves the two kinds, e.g.
/// `try { CRAC_1 } catch {}; CRAC_2; try { CRAC_3 } catch {}`:
/// without the sort, a merged operation would expose internals in
/// bucket order (failed first) rather than execution order, and
/// `renumber_nonces` would then assign nonces to internals out of
/// order as well.
pub fn drain_pending_crac_receipts(journal: &mut TezosXJournal) -> Vec<AppliedOperation> {
    // Tri-state captured before draining so the merged top-level
    // status can be reconciled below.  `has_applied` reflects CRACs
    // currently still Applied (pending list); `has_failed` reflects
    // CRACs that hit a Michelson-side failure; receipts in the
    // backtracked list contribute neither flag — they record what
    // was attempted but rolled back via an EVM revert.
    let has_applied = !journal.michelson.pending_crac_receipts.is_empty();
    let has_failed = !journal.michelson.failed_crac_receipts.is_empty();

    let mut all = std::mem::take(&mut journal.michelson.failed_crac_receipts);
    all.extend(std::mem::take(
        &mut journal.michelson.backtracked_crac_receipts,
    ));
    all.extend(std::mem::take(&mut journal.michelson.pending_crac_receipts));
    all.sort_by_key(|(seq, _)| *seq);

    let mut iter = all.into_iter().map(|(_, receipt)| receipt);
    let Some(mut merged) = iter.next() else {
        return Vec::new();
    };
    for other in iter {
        merge_crac_internals(&mut merged, other);
    }
    // `merge_crac_internals` only touches internals — the top-level
    // `ContentResult` is inherited from the first-executed receipt.
    // Reconcile so the merged top-level reflects the EVM tx's overall
    // CRAC outcome and respects the L1 invariant that internals under
    // a Failed top-level must be Backtracked/Failed/Skipped:
    //
    //   has_applied                       → Applied (force).
    //   else has_failed                   → Failed  (force, even if
    //                                       first-by-seq was a
    //                                       Backtracked-on-revert
    //                                       receipt).
    //   else (all entries Backtracked)    → keep the inherited
    //                                       Backtracked top-level
    //                                       (no-op).
    if has_applied {
        force_top_level_applied(&mut merged);
    } else if has_failed {
        force_top_level_failed(&mut merged);
    }
    vec![merged]
}

/// Overwrite the top-level `ContentResult` of a merged CRAC receipt with
/// `Applied`, preserving the synthetic handler→alias transfer shape.
/// Called after merging when at least one successful CRAC participated,
/// to avoid the L1-invalid combination of `Applied` internals under a
/// `Failed` parent.  Individual internals keep their own statuses.
///
/// The success shape is shared with `build_crac_receipt` via
/// `tezosx_tezos_runtime::crac_top_level_applied_result` so the two
/// producers cannot drift.
fn force_top_level_applied(receipt: &mut AppliedOperation) {
    let OperationDataAndMetadata::OperationWithMetadata(ref mut batch) =
        receipt.op_and_receipt;
    // A merged CRAC receipt is always a single-op batch (every CRAC
    // receipt is built that way by `build_crac_receipt` /
    // `build_failed_crac_receipt`, and merging only rewrites internals).
    debug_assert_eq!(batch.operations.len(), 1);
    if let Some(op) = batch.operations.first_mut() {
        if let OperationResultSum::Transfer(ref mut result) = op.receipt {
            if !matches!(result.result, ContentResult::Applied(_)) {
                result.result = tezosx_tezos_runtime::crac_top_level_applied_result();
            }
        }
    }
}

/// Overwrite the top-level `ContentResult` of a merged CRAC receipt
/// with `Failed`.  Mirror of `force_top_level_applied` for the case
/// where no currently-Applied CRAC contributed to the merge but at
/// least one Michelson-side failure did and the seq-first receipt
/// happens to be a Backtracked-on-revert one — without this, the
/// inherited Backtracked top-level would understate the EVM tx's
/// actual outcome (a CRAC failure surfaced).
///
/// The synthesized error vector is intentionally empty: indexers
/// already see the original Failed sibling's errors on its body
/// internals (which are merged in unchanged), so the top-level
/// merely needs to advertise `status: failed`.
fn force_top_level_failed(receipt: &mut AppliedOperation) {
    let OperationDataAndMetadata::OperationWithMetadata(ref mut batch) =
        receipt.op_and_receipt;
    debug_assert_eq!(batch.operations.len(), 1);
    if let Some(op) = batch.operations.first_mut() {
        if let OperationResultSum::Transfer(ref mut result) = op.receipt {
            if !matches!(result.result, ContentResult::Failed(_)) {
                result.result =
                    ContentResult::Failed(ApplyOperationErrors { errors: vec![] });
            }
        }
    }
}

/// Assign block-sequential nonces to all internal operations across
/// all applied operations, matching Tezos L1 semantics where nonces
/// are shared across all operations in a block and never reset.
///
/// Called once at block finalization so that individual operations can
/// use 0-based local nonces during execution.
pub fn renumber_nonces(operations: &mut [AppliedOperation]) {
    use tezos_tezlink::operation_result::{OperationDataAndMetadata, OperationResultSum};
    let mut counter: u16 = 0;
    for op in operations.iter_mut() {
        let OperationDataAndMetadata::OperationWithMetadata(ref mut batch) =
            op.op_and_receipt;
        for op_with_meta in batch.operations.iter_mut() {
            let internals = match &mut op_with_meta.receipt {
                OperationResultSum::Transfer(ref mut result) => {
                    &mut result.internal_operation_results
                }
                OperationResultSum::Origination(ref mut result) => {
                    &mut result.internal_operation_results
                }
                OperationResultSum::Reveal(_) => continue,
            };
            for iop in internals.iter_mut() {
                iop.set_nonce(counter);
                counter = counter.saturating_add(1);
            }
        }
    }
}

/// Merge `other`'s internal operations into `target`.
/// The synthetic CRAC-ID event stays as the first internal operation
/// in `target`.  Non-synthetic-event operations from `other` (regular
/// transfers, originations, AND user `EMIT` events) are appended
/// after `target`'s existing operations.  If `target` has no synthetic
/// CRAC-ID event but `other` does, the event is prepended.  Duplicate
/// synthetic CRAC-ID events from `other` are dropped — but user EMITs
/// always pass through.
/// Nonces are left as-is; renumber_nonces() fixes them at block finalization.
fn merge_crac_internals(target: &mut AppliedOperation, other: AppliedOperation) {
    use tezos_execution::enshrined_contracts::is_synthetic_crac_event;
    use tezos_tezlink::operation_result::{OperationDataAndMetadata, OperationResultSum};
    // Partition `other`'s internals into non-synthetic-event and
    // synthetic-event entries.  User EMITs (Event variants with a
    // non-`crac` tag) join the non-synthetic bucket so they are
    // appended after `target`'s existing operations rather than being
    // mistakenly treated as duplicates of the synthetic CRAC-ID event.
    let (other_non_synthetic, other_synthetic_events): (Vec<_>, Vec<_>) = match other
        .op_and_receipt
    {
        OperationDataAndMetadata::OperationWithMetadata(batch) => batch
            .operations
            .into_iter()
            .flat_map(|op| match op.receipt {
                OperationResultSum::Transfer(result) => result.internal_operation_results,
                _ => vec![],
            })
            .partition(|iop| !is_synthetic_crac_event(iop)),
    };
    // Append to `target`'s internal operations, before the CRAC event.
    // CRAC receipts are built by build_crac_receipt which guarantees exactly
    // one operation (a Transfer).  Assert this so violations are caught early.
    let OperationDataAndMetadata::OperationWithMetadata(ref mut batch) =
        target.op_and_receipt;
    {
        debug_assert!(
            !batch.operations.is_empty(),
            "CRAC receipt must have at least one operation"
        );
        if let Some(op) = batch.operations.first_mut() {
            debug_assert!(
                matches!(op.receipt, OperationResultSum::Transfer(_)),
                "CRAC receipt operation must be a Transfer"
            );
            if let OperationResultSum::Transfer(ref mut result) = op.receipt {
                let has_synthetic_event = result
                    .internal_operation_results
                    .iter()
                    .any(is_synthetic_crac_event);
                if !has_synthetic_event && !other_synthetic_events.is_empty() {
                    // Target has no synthetic CRAC-ID event — prepend
                    // other's first one, then re-append existing
                    // operations.  Duplicate synthetic events past the
                    // first one are dropped.
                    let existing = std::mem::take(&mut result.internal_operation_results);
                    result
                        .internal_operation_results
                        .extend(other_synthetic_events.into_iter().take(1));
                    result.internal_operation_results.extend(existing);
                }
                // Append other's non-synthetic-event entries after
                // existing operations, preserving execution order.
                // User EMITs flow through here unchanged.
                result
                    .internal_operation_results
                    .extend(other_non_synthetic);
            }
        }
    }
}

/// Technically incorrect: it is possible to do a call without sending any data,
/// however it's done for benchmarking only, and benchmarking doesn't include
/// such a scenario
fn log_transaction_type(to: Option<H160>, data: &[u8]) {
    if to.is_none() {
        log!(Benchmarking, "Transaction type: CREATE");
    } else if data.is_empty() {
        log!(Benchmarking, "Transaction type: TRANSFER");
    } else {
        log!(Benchmarking, "Transaction type: CALL");
    }
}

#[trace_kernel]
#[allow(clippy::too_many_arguments)]
#[instrument(skip_all)]
pub fn revm_run_transaction<Host>(
    host: &mut Host,
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    block_constants: &BlockConstants,
    transaction_hash: Option<[u8; TRANSACTION_HASH_SIZE]>,
    caller: H160,
    to: Option<H160>,
    value: U256,
    call_data: Vec<u8>,
    gas_limit: u64,
    effective_gas_price: U256,
    maximum_gas_per_transaction: u64,
    authorization_list: Option<AuthorizationList>,
    spec_id: &SpecId,
    tracer_input: Option<TracerInput>,
    is_simulation: bool,
    origin: revm_etherlink::TransactionOrigin,
) -> Result<ExecutionOutcome, Error>
where
    Host: StorageV1,
{
    // Disclaimer:
    // The following code is over-complicated because we maintain
    // two sets of primitives inside the kernel's codebase.
    // There's a lot of dummy conversions that are
    // needed to make the translation from our type to the
    // ones from REVM (and the other way round).
    //
    // NB:
    // None of the revm primitives are imported globally on purpose to
    // avoid disturbing the workflow of other engineers. This part of the
    // code is extremely self-contained on purpose.
    //
    // TODO: Simplify all the base structures to avoid these translations
    // once we fully make the switch to REVM.
    let mut bytes = vec![0u8; 32];
    value.to_little_endian(&mut bytes);
    let effective_gas_price = u128::from_le_bytes(if effective_gas_price.bits() < 128 {
        effective_gas_price.low_u128().to_le_bytes()
    } else {
        return Err(Error::Overflow(
            "Given amount does not fit in a u128".to_string(),
        ));
    });
    let gas_data =
        GasData::new(gas_limit, effective_gas_price, maximum_gas_per_transaction);
    revm_etherlink::run_transaction(
        host,
        registry,
        journal,
        *spec_id,
        block_constants,
        transaction_hash,
        Address::from_slice(&caller.0),
        to.map(|to| Address::from_slice(&to.0)),
        Bytes::from(call_data),
        gas_data,
        revm::primitives::U256::from_le_slice(&bytes),
        authorization_list.map(signed_authorization),
        tracer_input.map(|tracer_input| match tracer_input {
            TracerInput::CallTracer(CallTracerInput {
                transaction_hash,
                config,
            }) => revm_etherlink::inspectors::TracerInput::CallTracer(
                revm_etherlink::inspectors::call_tracer::CallTracerInput {
                    config: revm_etherlink::inspectors::call_tracer::CallTracerConfig {
                        only_top_call: config.only_top_call,
                        with_logs: config.with_logs,
                    },
                    transaction_hash: transaction_hash.map(|hash| B256::from(hash.0)),
                },
            ),
            TracerInput::StructLogger(StructLoggerInput {
                transaction_hash,
                config,
            }) => revm_etherlink::inspectors::TracerInput::StructLogger(
                revm_etherlink::inspectors::struct_logger::StructLoggerInput {
                    config:
                        revm_etherlink::inspectors::struct_logger::StructLoggerConfig {
                            enable_memory: config.enable_memory,
                            enable_return_data: config.enable_return_data,
                            disable_stack: config.disable_stack,
                            disable_storage: config.disable_storage,
                        },
                    transaction_hash: transaction_hash.map(|hash| B256::from(hash.0)),
                },
            ),
        }),
        is_simulation,
        origin,
    )
    .map_err(Error::InvalidRunTransaction)
}

#[allow(clippy::too_many_arguments)]
#[instrument(skip_all)]
fn apply_ethereum_transaction_common<Host>(
    host: &mut Host,
    registry: &impl Registry,
    block_constants: &BlockConstants,
    transaction: &EthereumTransactionCommon,
    transaction_hash: [u8; TRANSACTION_HASH_SIZE],
    is_delayed: bool,
    tracer_input: Option<TracerInput>,
    spec_id: &SpecId,
    limits: &EvmLimits,
    crac_id: CracId,
    http_trace_enabled: bool,
) -> Result<ExecutionResult<RuntimeTransactionResult>, anyhow::Error>
where
    Host: StorageV1,
{
    let effective_gas_price = block_constants.base_fee_per_gas();
    let (caller, gas_limit) = match is_valid_ethereum_transaction_common(
        host,
        transaction,
        block_constants,
        effective_gas_price,
        is_delayed,
        limits,
    )? {
        Validity::Valid(caller, gas_limit) => (caller, gas_limit),
        _reason => {
            log!(Benchmarking, "Transaction type: INVALID");
            return Ok(ExecutionResult::Invalid);
        }
    };

    if !is_delayed {
        // Deduct the DA fee before EVM execution, so that the caller cannot
        // transfer it out during execution (which would make the post-execution
        // fee settlement fail and revert the entire block).
        //
        // Safety: `is_valid_ethereum_transaction_common` (above) already
        // verified that `balance >= gas_limit_with_fees * base_fee_per_gas`.
        // Since `gas_for_fees = ceil(da_fee / minimum_base_fee_per_gas)` and
        // `base_fee_per_gas >= minimum_base_fee_per_gas`, we have:
        //   balance >= gas_limit_with_fees * base_fee_per_gas
        //           >= gas_for_fees * base_fee_per_gas
        //           >= gas_for_fees * minimum_base_fee_per_gas
        //           >= da_fee
        // Nothing modifies the caller's balance between the validation check
        // and this point, so `sub_balance` cannot fail.
        let cost = fees::da_fee(
            block_constants.block_fees.da_fee_per_byte(),
            &transaction.data,
            &transaction.access_list,
            transaction
                .authorization_list
                .as_ref()
                .map_or(0, |al| al.len()),
        );

        let mut caller_account = StorageAccount::from_address(&h160_to_alloy(&caller))?;

        if let Err(e) = caller_account.sub_balance(host, u256_to_alloy(&cost)) {
            return Err(anyhow::anyhow!(
                "Failed to charge {caller} additional fees of {}: {}",
                cost,
                e
            ));
        }
    }

    let to = transaction.to;
    let call_data = transaction.data.clone();
    log_transaction_type(to, &call_data);
    let value = transaction.value;
    let mut journal = TezosXJournal::new(crac_id);
    let run_result = revm_run_transaction(
        host,
        registry,
        &mut journal,
        block_constants,
        Some(transaction_hash),
        caller,
        to,
        value,
        call_data,
        gas_limit,
        effective_gas_price,
        limits.maximum_gas_limit,
        transaction.authorization_list.clone(),
        spec_id,
        tracer_input,
        false,
        TransactionOrigin::UserInput {
            access_list: revm_etherlink::helpers::legacy::access_list_to_revm(
                transaction.access_list.clone(),
            ),
        },
    );

    // Capture HTTP traces before branching on the execution outcome so that
    // partial traces from a transaction that failed mid-execution remain
    // observable through the [http_trace*] RPCs. Matches the behavior of the
    // other two journal sites (Tezos / TezosDelayed).
    crate::storage::maybe_store_http_traces_for_tx(
        host,
        http_trace_enabled,
        &transaction_hash,
        &journal,
    );

    let execution_outcome = run_result?;

    // Drain any CRAC receipts produced by EVM→Michelson calls during
    // this transaction so they can be included in the Michelson runtime block.
    let crac_receipts = drain_pending_crac_receipts(&mut journal);

    let transaction_result =
        RuntimeTransactionResult::Ethereum(EthereumTransactionResult {
            caller,
            execution_outcome,
            crac_receipts,
        });

    Ok(ExecutionResult::Valid(transaction_result))
}

sol! {
    struct SolXTZDeposit {
        address receiver;
        uint256 inbox_level;
        uint256 inbox_msg_id;
    }

    function handle_xtz_deposit(SolXTZDeposit memory deposit) external;
}

impl From<&Deposit> for SolXTZDeposit {
    fn from(deposit: &Deposit) -> Self {
        SolXTZDeposit {
            receiver: h160_to_alloy(&deposit.receiver.to_h160().unwrap_or_default()),
            inbox_level: u256_to_alloy(&U256::from(deposit.inbox_level)),
            inbox_msg_id: u256_to_alloy(&U256::from(deposit.inbox_msg_id)),
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn pure_xtz_deposit<Host>(
    host: &mut Host,
    registry: &impl Registry,
    deposit: &Deposit,
    block_constants: &BlockConstants,
    transaction_hash: [u8; TRANSACTION_HASH_SIZE],
    maximum_gas_limit: u64,
    spec_id: &SpecId,
    tracer_input: Option<TracerInput>,
) -> Result<ExecutionOutcome, Error>
where
    Host: StorageV1,
{
    // Fees are set to zero, this is an internal call to the XTZ bridge
    // solidity contract.
    // It isn't required for anyone to pay for the execution cost.
    let block_constants = BlockConstants {
        block_fees: BlockFees::new(U256::zero(), U256::zero(), U256::zero()),
        ..*block_constants
    };

    let caller = alloy_to_h160(&FEED_DEPOSIT_ADDR);
    let mut caller_account = StorageAccount::from_address(&FEED_DEPOSIT_ADDR)?;
    let to = Some(alloy_to_h160(&XTZ_BRIDGE_SOL_ADDR));
    let gas_limit = XTZ_DEPOSIT_EXECUTION_COST;
    let value = deposit.amount;
    // We prefund the feeder address for the xtz deposit.
    caller_account.add_balance(host, u256_to_alloy(&value))?;
    let call_data = handle_xtz_depositCall {
        deposit: SolXTZDeposit::from(deposit),
    }
    .abi_encode();
    let effective_gas_price = block_constants.base_fee_per_gas();
    let mut journal = TezosXJournal::default();
    match revm_run_transaction(
        host,
        registry,
        &mut journal,
        &block_constants,
        Some(transaction_hash),
        caller,
        to,
        value,
        call_data,
        gas_limit,
        effective_gas_price,
        maximum_gas_limit,
        None,
        spec_id,
        tracer_input,
        false,
        TransactionOrigin::UserInput {
            access_list: revm::context::transaction::AccessList::default(),
        },
    ) {
        Ok(execution_outcome) => Ok(execution_outcome),
        Err(err) => {
            // Something went wrong, we remove the added balance for the xtz deposit.
            caller_account.sub_balance(host, u256_to_alloy(&value))?;
            Err(err)
        }
    }
}

sol! {
    struct SolFaDepositWithProxy {
        uint256 amount;
        address receiver;
        address proxy;
        uint256 ticket_hash;
        uint256 inbox_level;
        uint256 inbox_msg_id;
    }

    struct SolFaDepositWithoutProxy {
        uint256 amount;
        address receiver;
        uint256 ticket_hash;
        uint256 inbox_level;
        uint256 inbox_msg_id;
    }

    function queue(SolFaDepositWithProxy memory deposit) external;

    function execute_without_proxy(SolFaDepositWithoutProxy memory deposit);
}

impl From<&FaDepositWithProxy> for SolFaDepositWithProxy {
    fn from(deposit: &FaDepositWithProxy) -> Self {
        SolFaDepositWithProxy {
            amount: u256_to_alloy(&deposit.amount),
            receiver: h160_to_alloy(&deposit.receiver),
            proxy: h160_to_alloy(&deposit.proxy),
            inbox_level: u256_to_alloy(&U256::from(deposit.inbox_level)),
            inbox_msg_id: u256_to_alloy(&U256::from(deposit.inbox_msg_id)),
            ticket_hash: u256_to_alloy(&U256::from_big_endian(
                deposit.ticket_hash.as_bytes(),
            )),
        }
    }
}

impl From<&FaDeposit> for SolFaDepositWithoutProxy {
    fn from(deposit: &FaDeposit) -> Self {
        SolFaDepositWithoutProxy {
            amount: u256_to_alloy(&deposit.amount),
            receiver: h160_to_alloy(&deposit.receiver),
            inbox_level: u256_to_alloy(&U256::from(deposit.inbox_level)),
            inbox_msg_id: u256_to_alloy(&U256::from(deposit.inbox_msg_id)),
            ticket_hash: u256_to_alloy(&U256::from_big_endian(
                deposit.ticket_hash.as_bytes(),
            )),
        }
    }
}

#[allow(clippy::too_many_arguments)]
#[trace_kernel]
pub fn pure_fa_deposit<Host>(
    host: &mut Host,
    registry: &impl Registry,
    fa_deposit: &FaDeposit,
    block_constants: &BlockConstants,
    transaction_hash: [u8; TRANSACTION_HASH_SIZE],
    maximum_gas_limit: u64,
    spec_id: &SpecId,
    tracer_input: Option<TracerInput>,
) -> Result<ExecutionOutcome, Error>
where
    Host: StorageV1,
{
    // Fees are set to zero, this is an internal call from the system address to the FA bridge solidity contract.
    // We do not require the system address to pay for the execution cost.
    let block_constants = BlockConstants {
        block_fees: BlockFees::new(U256::zero(), U256::zero(), U256::zero()),
        ..*block_constants
    };

    // A specific address is allocated for queue call
    // System address can only be used as caller for simulations
    let caller = alloy_to_h160(&FEED_DEPOSIT_ADDR);
    let to = Some(alloy_to_h160(&FA_BRIDGE_SOL_ADDR));
    let value = U256::zero();
    let gas_limit = FA_DEPOSIT_EXECUTION_COST;
    let call_data = match fa_deposit.to_fa_deposit_with_proxy() {
        Some(deposit) => queueCall {
            deposit: SolFaDepositWithProxy::from(&deposit),
        }
        .abi_encode(),
        None => execute_without_proxyCall {
            deposit: SolFaDepositWithoutProxy::from(fa_deposit),
        }
        .abi_encode(),
    };
    let effective_gas_price = block_constants.base_fee_per_gas();
    let mut journal = TezosXJournal::default();
    revm_run_transaction(
        host,
        registry,
        &mut journal,
        &block_constants,
        Some(transaction_hash),
        caller,
        to,
        value,
        call_data,
        gas_limit,
        effective_gas_price,
        maximum_gas_limit,
        None,
        spec_id,
        tracer_input,
        false,
        TransactionOrigin::UserInput {
            access_list: revm::context::transaction::AccessList::default(),
        },
    )
}

#[allow(clippy::too_many_arguments)]
fn apply_fa_deposit<Host>(
    host: &mut Host,
    registry: &impl Registry,
    fa_deposit: &FaDeposit,
    block_constants: &BlockConstants,
    transaction_hash: [u8; TRANSACTION_HASH_SIZE],
    tracer_input: Option<TracerInput>,
    spec_id: &SpecId,
    limits: &EvmLimits,
) -> Result<ExecutionResult<RuntimeTransactionResult>, Error>
where
    Host: StorageV1,
{
    let execution_outcome = pure_fa_deposit(
        host,
        registry,
        fa_deposit,
        block_constants,
        transaction_hash,
        limits.maximum_gas_limit,
        spec_id,
        tracer_input,
    )?;

    // A specific address is allocated for queue call
    // System address can only be used as caller for simulations
    let transaction_result =
        RuntimeTransactionResult::Ethereum(EthereumTransactionResult {
            caller: alloy_to_h160(&FEED_DEPOSIT_ADDR),
            execution_outcome,
            crac_receipts: vec![],
        });

    Ok(ExecutionResult::Valid(transaction_result))
}

pub const WITHDRAWAL_OUTBOX_QUEUE: RefPath =
    RefPath::assert_from(b"/evm/world_state/__outbox_queue");

/// Execution info for an Ethereum transaction.
pub struct EthereumExecutionInfo {
    pub receipt_info: TransactionReceiptInfo,
    pub tx_object: TransactionObject,
    /// CRAC receipts for Michelson operations triggered during this EVM tx.
    pub pending_crac_receipts: Vec<AppliedOperation>,
}

/// Side effect from a cross-runtime call that needs to be registered
/// in the target runtime's derived block.
pub enum CrossRuntimeEffect {
    /// EVM logs accumulated from CRAC executions that need to appear
    /// as a fake transaction in the EVM block.
    Evm(EvmCracEffect),
}

/// Data needed to construct a fake EVM transaction from incoming CRACs.
#[allow(dead_code)]
pub struct EvmCracEffect {
    /// CRAC-ID shared by all CRACs in this transaction.
    pub crac_id: String,
    /// Logs accumulated from all `serve()` calls.
    pub logs: Vec<revm::primitives::Log>,
    /// EVM address (alias) of the top-level sender (from X-Tezos-Source).
    pub source: H160,
    /// EVM address (alias) of the immediate caller (from X-Tezos-Sender).
    pub sender: H160,
    /// Gas limit forwarded to the call.
    pub gas_limit: U256,
    /// Value attached to the call (in wei).
    pub amount: U256,
    /// Cumulative gas used across all CRAC executions.
    pub gas_used: U256,
}

/// Enum distinguishing between Ethereum and Tezos execution info.
#[allow(clippy::large_enum_variant)]
pub enum RuntimeExecutionInfo {
    Ethereum(EthereumExecutionInfo),
    Tezos {
        op: AppliedOperation,
        cross_runtime_effects: Vec<CrossRuntimeEffect>,
        /// Total milligas consumed by the batch, computed from the gas
        /// tracker (correct for all outcomes including Failed).
        consumed_milligas: u64,
    },
}

pub enum ExecutionResult<T> {
    Valid(T),
    Invalid,
}

impl<T> From<Option<T>> for ExecutionResult<T> {
    fn from(opt: Option<T>) -> ExecutionResult<T> {
        match opt {
            Some(v) => ExecutionResult::Valid(v),
            None => ExecutionResult::Invalid,
        }
    }
}

pub fn push_withdrawals_to_outbox<Host>(
    host: &mut Host,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    withdrawals: Vec<Withdrawal>,
) -> Result<(), anyhow::Error>
where
    Host: StorageV1,
{
    for message in withdrawals {
        match message {
            Withdrawal::Standard(message) => {
                let outbox_message: OutboxMessage<RouterInterface> = message;
                let len = outbox_queue.queue_message(host, outbox_message)?;
                log!(Debug, "Length of the outbox queue: {}", len);
            }
            Withdrawal::Fast(message) => {
                let outbox_message: OutboxMessage<FastWithdrawalInterface> = message;
                let len = outbox_queue.queue_message(host, outbox_message)?;
                log!(Debug, "Length of the outbox queue: {}", len);
            }
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
#[instrument(skip_all)]
pub fn handle_transaction_result<Host>(
    host: &mut Host,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_constants: &BlockConstants,
    transaction: Transaction,
    index: u32,
    transaction_result: RuntimeTransactionResult,
    pay_fees: bool,
    sequencer_pool_address: Option<H160>,
) -> Result<RuntimeExecutionInfo, anyhow::Error>
where
    Host: StorageV1,
{
    match transaction_result {
        RuntimeTransactionResult::Ethereum(EthereumTransactionResult {
            caller,
            mut execution_outcome,
            crac_receipts,
        }) => {
            let to = transaction.to()?;

            let tx_hash = transaction.tx_hash;
            let tx_type = transaction.type_();
            let gas_used = execution_outcome.result.gas_used();

            let fee_updates = transaction
                .content
                .fee_updates(&block_constants.block_fees, gas_used.into());

            log!(
                Debug,
                "Transaction executed, outcome: {:?}",
                execution_outcome
            );
            log!(Benchmarking, "gas_used: {:?}", gas_used);
            log!(Benchmarking, "reason: {:?}", execution_outcome.result);

            let withdrawals = std::mem::take(&mut execution_outcome.withdrawals);
            push_withdrawals_to_outbox(host, outbox_queue, withdrawals)?;

            if pay_fees {
                fee_updates.apply(host, caller, sequencer_pool_address)?;
            }

            let tx_object = make_object(
                block_constants.number,
                transaction,
                caller,
                index,
                &fee_updates,
            )?;

            let receipt_info = make_receipt_info(
                tx_hash,
                index,
                execution_outcome,
                caller,
                to,
                fee_updates.overall_gas_price,
                tx_type,
                fee_updates.overall_gas_used,
            );

            Ok(RuntimeExecutionInfo::Ethereum(EthereumExecutionInfo {
                receipt_info,
                tx_object,
                pending_crac_receipts: crac_receipts,
            }))
        }
        RuntimeTransactionResult::Tezos {
            op,
            etherlink_withdrawals,
            cross_runtime_effects,
            consumed_milligas,
        } => {
            push_withdrawals_to_outbox(host, outbox_queue, etherlink_withdrawals)?;
            Ok(RuntimeExecutionInfo::Tezos {
                op,
                cross_runtime_effects,
                consumed_milligas,
            })
        }
    }
}

#[allow(clippy::too_many_arguments)]
#[instrument(skip_all)]
pub fn apply_transaction<Host>(
    host: &mut Host,
    registry: &impl Registry,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_constants: &BlockConstants,
    transaction: Transaction,
    crac_id: CracId,
    index: u32,
    sequencer_pool_address: Option<H160>,
    tracer_input: Option<TracerInput>,
    spec_id: &SpecId,
    limits: &EvmLimits,
    http_trace_enabled: bool,
    // SafeStorage roots to snapshot around a [TezosDelayed] operation.
    // Mirrors [TezlinkBlockConstants::safe_roots]; threaded in from the
    // caller because [BlockConstants] is EVM-flavored and doesn't carry
    // Tezos-side storage boundaries.
    tezos_safe_roots: &[tezos_smart_rollup_host::path::OwnedPath],
) -> Result<ExecutionResult<RuntimeExecutionInfo>, anyhow::Error>
where
    Host: StorageV1,
{
    let tracer_input = get_tracer_configuration(
        revm::primitives::B256::from_slice(&transaction.tx_hash),
        tracer_input,
    );
    let apply_result = match &transaction.content {
        TransactionContent::Ethereum(tx) => apply_ethereum_transaction_common(
            host,
            registry,
            block_constants,
            tx,
            transaction.tx_hash,
            false,
            tracer_input,
            spec_id,
            limits,
            crac_id,
            http_trace_enabled,
        )?,
        TransactionContent::EthereumDelayed(tx) => apply_ethereum_transaction_common(
            host,
            registry,
            block_constants,
            tx,
            transaction.tx_hash,
            true,
            tracer_input,
            spec_id,
            limits,
            crac_id,
            http_trace_enabled,
        )?,
        TransactionContent::Deposit(deposit) => {
            log!(Benchmarking, "Transaction type: DEPOSIT");
            apply_tezosx_xtz_deposit(
                host,
                registry,
                deposit,
                block_constants,
                transaction.tx_hash,
                tracer_input,
                spec_id,
                limits,
            )?
        }
        TransactionContent::FaDeposit(fa_deposit) => {
            log!(Benchmarking, "Transaction type: FA_DEPOSIT");
            apply_fa_deposit(
                host,
                registry,
                fa_deposit,
                block_constants,
                transaction.tx_hash,
                tracer_input,
                spec_id,
                limits,
            )?
        }
        TransactionContent::TezosDelayed(op) => {
            // TODO: If we need to use storage root of tezlink pass it as parameter.
            let context =
                TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;
            let skip_signature_check = false;
            let i128_timestamp: i128 = block_constants
                .timestamp
                .try_into()
                .map_err(|e| anyhow!("Failed to convert timestamp: {e}"))?;
            let i64_timestamp: i64 = i128_timestamp
                .try_into()
                .map_err(|e| anyhow!("Failed to convert timestamp to i64: {e}"))?;
            let mut chain_id_bytes = vec![0u8; 32];
            block_constants
                .chain_id
                .to_little_endian(&mut chain_id_bytes);
            let op_hash = op.hash()?;
            // Delayed operations already paid L1 fees through the delayed inbox,
            // so DA fee check is not applicable.
            let mut tezosx_journal = TezosXJournal::new(crac_id);
            let validation_result = tezos_execution::validate_and_apply_operation(
                host,
                registry,
                &mut tezosx_journal,
                &context,
                op_hash.clone(),
                // TODO: !20198 avoid this clone.
                op.clone(),
                &BlockCtx {
                    level: &BlockNumber {
                        block_number: block_constants
                            .number
                            .try_into()
                            .map_err(|e| anyhow!("{e}"))?,
                    },
                    now: &Timestamp::from(i64_timestamp),
                    // SAFETY: chain_id_bytes is defined as 32 bytes long.
                    chain_id: &ChainId::try_from_bytes(&chain_id_bytes[..4])?,
                },
                skip_signature_check,
                None,
                None, // No fee refund for delayed inbox operations
                tezos_safe_roots,
            );

            // Persist HTTP traces collected in [tezosx_journal] regardless of
            // the validation outcome so that operations that failed
            // mid-execution still expose their captured CRAC exchanges.
            // Matches the Tezos / Ethereum apply sites.
            crate::storage::maybe_store_http_traces_for_tx(
                host,
                http_trace_enabled,
                &transaction.tx_hash,
                &tezosx_journal,
            );

            match validation_result {
                Ok(processed_operations) => {
                    log!(
                        Debug,
                        "Delayed Tezos operation status: SUCCESS - {:?}",
                        processed_operations
                    );
                    let consumed_milligas = ProcessedOperation::total_consumed_milligas(
                        &processed_operations,
                    );
                    let operations =
                        ProcessedOperation::into_receipts(processed_operations);
                    let operation_and_receipt = AppliedOperation {
                        hash: op_hash,
                        branch: op.branch.clone(),
                        op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                            OperationBatchWithMetadata {
                                operations,
                                signature: op.signature.clone(),
                            },
                        ),
                    };
                    // Extract cross-runtime side effects accumulated
                    // during the Michelson execution (e.g. CRAC into EVM)
                    // BEFORE the commit: `commit_evm_journal_from_external`
                    // runs `JournalInner::finalize()` which clears
                    // `inner.logs` as part of revm's standard cleanup, so
                    // reading them after gives an empty buffer.
                    let cross_runtime_effects = extract_cross_runtime_effects(
                        &mut tezosx_journal,
                        consumed_milligas,
                    );
                    let etherlink_withdrawals = commit_evm_journal_from_external(
                        host,
                        registry,
                        block_constants,
                        &mut tezosx_journal,
                    )?;
                    Ok::<_, anyhow::Error>(ExecutionResult::Valid(
                        RuntimeTransactionResult::Tezos {
                            op: operation_and_receipt,
                            etherlink_withdrawals,
                            cross_runtime_effects,
                            consumed_milligas,
                        },
                    ))
                }
                Err(OperationError::Validation(err)) => {
                    log!(Info, "Delayed Tezos operation status: ERROR - {:?}", err);
                    Ok::<_, anyhow::Error>(ExecutionResult::Invalid)
                }
                Err(OperationError::RuntimeError(err)) => {
                    log!(Info, "Delayed Tezos operation runtime error: {:?}", err);
                    return Err(err.into());
                }
                Err(OperationError::BlockAbort(msg)) => {
                    log!(Error, "CRAC block abort: {msg}");
                    return Err(anyhow::anyhow!("CRAC block abort: {msg}"));
                }
            }?
        }
    };

    match apply_result {
        ExecutionResult::Valid(tx_result) => {
            let execution_result = handle_transaction_result(
                host,
                outbox_queue,
                block_constants,
                transaction,
                index,
                tx_result,
                true,
                sequencer_pool_address,
            )?;
            Ok(ExecutionResult::Valid(execution_result))
        }
        ExecutionResult::Invalid => Ok(ExecutionResult::Invalid),
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        apply::{is_valid_ethereum_transaction_common, Validity},
        chains::EvmLimits,
        fees::gas_for_fees,
    };
    use primitive_types::{H160, U256};
    use revm_etherlink::{
        helpers::legacy::{h160_to_alloy, u256_to_alloy},
        storage::world_state_handler::StorageAccount,
    };
    use tezos_ethereum::{
        block::{BlockConstants, BlockFees},
        transaction::TransactionType,
        tx_common::EthereumTransactionCommon,
    };
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    const CHAIN_ID: u32 = 1337;

    fn mock_block_constants() -> BlockConstants {
        let block_fees = BlockFees::new(
            U256::from(12345),
            U256::from(12345),
            U256::from(2_000_000_000_000u64),
        );
        BlockConstants::first_block(
            U256::from(Timestamp::from(0).as_u64()),
            CHAIN_ID.into(),
            block_fees,
            crate::block::GAS_LIMIT,
            H160::zero(),
        )
    }

    fn address_from_str(s: &str) -> H160 {
        let data = &hex::decode(s).unwrap();
        H160::from_slice(data)
    }

    fn set_balance(host: &mut MockKernelHost, address: &H160, balance: U256) {
        let mut account = StorageAccount::from_address(&h160_to_alloy(address)).unwrap();
        let mut info = account.info(host).unwrap_or_default();
        info.balance = u256_to_alloy(&balance);
        account.set_info(host, info).unwrap();
    }

    fn resign(transaction: EthereumTransactionCommon) -> EthereumTransactionCommon {
        // corresponding caller's address is 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        let private_key =
            "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701";
        transaction
            .sign_transaction(private_key.to_string())
            .expect("Should have been able to sign")
    }

    fn valid_tx(gas_limit: u64) -> EthereumTransactionCommon {
        let transaction = EthereumTransactionCommon::new(
            TransactionType::Eip1559,
            Some(CHAIN_ID.into()),
            0,
            U256::zero(),
            U256::from(21000),
            gas_limit,
            Some(H160::zero()),
            U256::zero(),
            vec![],
            vec![],
            None,
            None,
        );
        // sign tx
        resign(transaction)
    }

    fn gas_for_fees_no_data(block_constants: &BlockConstants) -> u64 {
        gas_for_fees(
            block_constants.block_fees.da_fee_per_byte(),
            block_constants.block_fees.minimum_base_fee_per_gas(),
            vec![].as_slice(),
            vec![].as_slice(),
            0,
        )
        .expect("should have been able to calculate fees")
    }

    #[test]
    fn test_tx_is_valid() {
        let mut host = MockKernelHost::default();
        let block_constants = mock_block_constants();
        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = gas_for_fees_no_data(&block_constants);
        let balance = U256::from(fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas;
        let transaction = valid_tx(gas_limit);
        // fund account
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::Valid(address, 21000),
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_cannot_prepay() {
        let mut host = MockKernelHost::default();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = gas_for_fees_no_data(&block_constants);
        // account doesnt have enough funds for execution
        let balance = U256::from(fee_gas) * gas_price;
        let gas_limit = 21000 + fee_gas;
        let transaction = valid_tx(gas_limit);
        // fund account
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidPrePay,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_signature() {
        let mut host = MockKernelHost::default();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = gas_for_fees_no_data(&block_constants);
        let balance = U256::from(fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas;
        let mut transaction = valid_tx(gas_limit);
        transaction.signature = None;
        // fund account
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidSignature,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_wrong_nonce() {
        let mut host = MockKernelHost::default();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = gas_for_fees_no_data(&block_constants);
        let balance = U256::from(fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas;
        let mut transaction = valid_tx(gas_limit);
        transaction.nonce = 42;
        transaction = resign(transaction);

        // fund account
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidNonce,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_wrong_chain_id() {
        let mut host = MockKernelHost::default();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let balance = U256::from(21000) * gas_price;
        let mut transaction = valid_tx(1);
        transaction.chain_id = Some(U256::from(42));
        transaction = resign(transaction);

        // fund account
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidChainId,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_max_fee_less_than_base_fee() {
        let mut host = MockKernelHost::default();
        let block_constants = mock_block_constants();

        // setup
        let gas_price = U256::from(21000);
        let max_gas_price = U256::one();
        // account doesnt have enough funds for execution
        let fee_gas = gas_for_fees_no_data(&block_constants);
        let gas_limit = 21000 + fee_gas;
        let mut transaction = valid_tx(gas_limit);
        // set a max base fee too low
        transaction.max_fee_per_gas = max_gas_price;
        transaction = resign(transaction);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidMaxBaseFee,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_invalid_not_enough_gas_for_fee() {
        let mut host = MockKernelHost::default();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let balance = U256::from(21000) * gas_price;
        // fund account
        set_balance(&mut host, &address, balance);

        let gas_limit = 21000; // gas limit is not enough to cover fees
        let mut transaction = valid_tx(gas_limit);
        transaction.data = vec![1u8];
        transaction = resign(transaction);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidNotEnoughGasForFees,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );

        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &transaction,
            &block_constants,
            gas_price,
            true,
            &EvmLimits::default(),
        );
        assert!(
            matches!(
                res.expect("Verification should not have raise an error"),
                Validity::Valid(_, _)
            ),
            "Transaction should have been accepted through delayed inbox"
        );
    }

    /// Build a dummy CRAC receipt with the given internal operations.
    fn dummy_crac_receipt(
        internals: Vec<tezos_tezlink::operation_result::InternalOperationSum>,
    ) -> tezos_tezlink::block::AppliedOperation {
        use tezos_crypto_rs::hash::{BlockHash, OperationHash, UnknownSignature};
        use tezos_data_encoding::types::Narith;
        use tezos_tezlink::operation::{
            ManagerOperation, ManagerOperationContent, Parameters, TransferContent,
        };
        use tezos_tezlink::operation_result::{
            ContentResult, OperationBatchWithMetadata, OperationDataAndMetadata,
            OperationResult, OperationResultSum, OperationWithMetadata, TransferSuccess,
            TransferTarget,
        };
        let signature = UnknownSignature::try_from([0u8; 64].as_slice()).unwrap();
        let source = tezos_smart_rollup::types::PublicKeyHash::from_b58check(
            "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU",
        )
        .unwrap();
        let destination = tezos_protocol::contract::Contract::Originated(
            tezos_crypto_rs::hash::ContractKt1Hash::from_base58_check(
                "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT",
            )
            .unwrap(),
        );
        tezos_tezlink::block::AppliedOperation {
            hash: OperationHash::default(),
            branch: BlockHash::default(),
            op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                OperationBatchWithMetadata {
                    operations: vec![OperationWithMetadata {
                        content: ManagerOperationContent::Transaction(ManagerOperation {
                            source,
                            fee: Narith(0u64.into()),
                            counter: Narith(0u64.into()),
                            gas_limit: Narith(0u64.into()),
                            storage_limit: Narith(0u64.into()),
                            operation: TransferContent {
                                amount: Narith(0u64.into()),
                                destination,
                                parameters: Parameters {
                                    entrypoint: mir::ast::Entrypoint::default(),
                                    value: vec![],
                                },
                            },
                        }),
                        receipt: OperationResultSum::Transfer(OperationResult {
                            balance_updates: vec![],
                            result: ContentResult::Applied(TransferTarget::from(
                                TransferSuccess::default(),
                            )),
                            internal_operation_results: internals,
                        }),
                    }],
                    signature,
                },
            ),
        }
    }

    fn make_transfer(
        nonce: u16,
        amount: u64,
    ) -> tezos_tezlink::operation_result::InternalOperationSum {
        use tezos_crypto_rs::hash::ContractKt1Hash;
        use tezos_data_encoding::types::Narith;
        use tezos_tezlink::operation::{Parameters, TransferContent};
        use tezos_tezlink::operation_result::{
            ContentResult, InternalContentWithMetadata, InternalOperationSum,
            TransferSuccess, TransferTarget,
        };
        InternalOperationSum::Transfer(InternalContentWithMetadata {
            sender: tezos_protocol::contract::Contract::Originated(
                ContractKt1Hash::from_base58_check(
                    "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT",
                )
                .unwrap(),
            ),
            nonce,
            content: TransferContent {
                amount: Narith(amount.into()),
                destination: tezos_protocol::contract::Contract::Originated(
                    ContractKt1Hash::from_base58_check(
                        "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT",
                    )
                    .unwrap(),
                ),
                parameters: Parameters {
                    entrypoint: mir::ast::Entrypoint::default(),
                    value: vec![],
                },
            },
            result: ContentResult::Applied(TransferTarget::from(
                TransferSuccess::default(),
            )),
        })
    }

    fn make_crac_event(
        nonce: u16,
    ) -> tezos_tezlink::operation_result::InternalOperationSum {
        use tezos_data_encoding::types::Narith;
        use tezos_execution::NULL_PKH;
        use tezos_tezlink::operation_result::{
            ContentResult, EventContent, EventSuccess, InternalContentWithMetadata,
            InternalOperationSum, MichelineExpr,
        };
        InternalOperationSum::Event(InternalContentWithMetadata {
            sender: tezos_protocol::contract::Contract::Implicit(
                tezos_smart_rollup::types::PublicKeyHash::from_b58check(NULL_PKH)
                    .unwrap(),
            ),
            nonce,
            content: EventContent {
                tag: Some(mir::ast::Entrypoint::from_string_unchecked(
                    tezos_execution::enshrined_contracts::SYNTHETIC_CRAC_EVENT_TAG.into(),
                )),
                payload: Some(MichelineExpr(vec![
                    0x01, 0x00, 0x00, 0x00, 0x03, 0x31, 0x2d, 0x30,
                ])),
                ty: MichelineExpr(vec![0x03, 0x68]),
            },
            result: ContentResult::Applied(EventSuccess {
                consumed_milligas: Narith(0u64.into()),
            }),
        })
    }

    fn extract_nonces(receipt: &tezos_tezlink::block::AppliedOperation) -> Vec<u16> {
        use tezos_tezlink::operation_result::{
            InternalOperationSum, OperationDataAndMetadata, OperationResultSum,
        };
        let OperationDataAndMetadata::OperationWithMetadata(ref batch) =
            receipt.op_and_receipt;
        let Some(op) = batch.operations.first() else {
            return vec![];
        };
        let OperationResultSum::Transfer(ref result) = op.receipt else {
            return vec![];
        };
        result
            .internal_operation_results
            .iter()
            .map(|iop| match iop {
                InternalOperationSum::Transfer(m) => m.nonce,
                InternalOperationSum::Origination(m) => m.nonce,
                InternalOperationSum::Event(m) => m.nonce,
            })
            .collect()
    }

    fn extract_amounts(receipt: &tezos_tezlink::block::AppliedOperation) -> Vec<u64> {
        use tezos_tezlink::operation_result::{
            InternalOperationSum, OperationDataAndMetadata, OperationResultSum,
        };
        let OperationDataAndMetadata::OperationWithMetadata(ref batch) =
            receipt.op_and_receipt;
        let Some(op) = batch.operations.first() else {
            return vec![];
        };
        let OperationResultSum::Transfer(ref result) = op.receipt else {
            return vec![];
        };
        result
            .internal_operation_results
            .iter()
            .filter_map(|iop| match iop {
                InternalOperationSum::Transfer(m) => {
                    Some(m.content.amount.0.clone().try_into().unwrap())
                }
                _ => None,
            })
            .collect()
    }

    /// Mutate a dummy CRAC receipt to carry a `Failed` top-level, matching
    /// what `build_failed_crac_receipt` produces in production.
    fn make_top_level_failed(receipt: &mut tezos_tezlink::block::AppliedOperation) {
        use tezos_tezlink::operation_result::{
            ApplyOperationError, ApplyOperationErrors, ContentResult,
            OperationDataAndMetadata, OperationResultSum, TransferError,
        };
        let OperationDataAndMetadata::OperationWithMetadata(ref mut batch) =
            receipt.op_and_receipt;
        for op in batch.operations.iter_mut() {
            if let OperationResultSum::Transfer(ref mut result) = op.receipt {
                result.result = ContentResult::Failed(ApplyOperationErrors::from(
                    ApplyOperationError::Transfer(
                        TransferError::NonSmartContractExecutionCall,
                    ),
                ));
            }
        }
    }

    fn top_level_is_applied(receipt: &tezos_tezlink::block::AppliedOperation) -> bool {
        use tezos_tezlink::operation_result::{
            ContentResult, OperationDataAndMetadata, OperationResultSum,
        };
        let OperationDataAndMetadata::OperationWithMetadata(ref batch) =
            receipt.op_and_receipt;
        batch.operations.first().is_some_and(|op| {
            matches!(op.receipt, OperationResultSum::Transfer(ref r) if matches!(r.result, ContentResult::Applied(_)))
        })
    }

    /// Multiple failed CRACs from the same EVM transaction must merge into
    /// a single top-level Michelson operation, per RFC principle 6
    /// ("CRAC-ID per top-level transaction"). Currently
    /// `drain_pending_crac_receipts` only merges successful receipts and
    /// passes failed receipts through as-is, producing N separate ops.
    #[test]
    fn test_drain_merges_multiple_failed_receipts() {
        use tezosx_journal::{CracId, TezosXJournal};
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        journal
            .michelson
            .push_failed_crac_receipt(dummy_crac_receipt(vec![
                make_crac_event(0),
                make_transfer(1, 100),
            ]));
        journal
            .michelson
            .push_failed_crac_receipt(dummy_crac_receipt(vec![make_transfer(2, 200)]));
        journal
            .michelson
            .push_failed_crac_receipt(dummy_crac_receipt(vec![make_transfer(3, 300)]));

        let result = super::drain_pending_crac_receipts(&mut journal);

        assert_eq!(
            result.len(),
            1,
            "RFC principle 6: failed receipts from same EVM tx must be merged \
             into a single top-level Michelson operation"
        );
        assert_eq!(
            extract_amounts(&result[0]),
            vec![100, 200, 300],
            "all transfers preserved in execution order"
        );
        assert_eq!(
            extract_nonces(&result[0]),
            vec![0, 1, 2, 3],
            "single CRAC event at the front, then transfers"
        );
    }

    /// When a merge contains at least one successful CRAC, the top-level
    /// `ContentResult` must be `Applied`.  Leaving it as `Failed`
    /// (inherited from the first-executed receipt) would produce the
    /// L1-invalid combination of `Applied` internals under a `Failed`
    /// parent.
    #[test]
    fn test_drain_mixed_forces_applied_top_level() {
        use tezosx_journal::{CracId, TezosXJournal};
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        // First-executed CRAC fails (top=Failed in the source receipt),
        // second one succeeds.
        let mut failed = dummy_crac_receipt(vec![make_transfer(0, 100)]);
        make_top_level_failed(&mut failed);
        journal.michelson.push_failed_crac_receipt(failed);
        journal
            .michelson
            .push_pending_crac_receipt(dummy_crac_receipt(vec![make_transfer(1, 200)]));

        let result = super::drain_pending_crac_receipts(&mut journal);
        assert_eq!(result.len(), 1);
        assert!(
            top_level_is_applied(&result[0]),
            "mixed failed+pending merge must expose an Applied top-level to \
             avoid Applied-internals-under-Failed-parent"
        );
    }

    /// All-failed merges keep their `Failed` top-level (RFC Example 4) —
    /// the forcing only triggers when at least one CRAC succeeded.
    #[test]
    fn test_drain_all_failed_keeps_failed_top_level() {
        use tezosx_journal::{CracId, TezosXJournal};
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut r1 = dummy_crac_receipt(vec![make_transfer(0, 100)]);
        make_top_level_failed(&mut r1);
        let mut r2 = dummy_crac_receipt(vec![make_transfer(1, 200)]);
        make_top_level_failed(&mut r2);
        journal.michelson.push_failed_crac_receipt(r1);
        journal.michelson.push_failed_crac_receipt(r2);

        let result = super::drain_pending_crac_receipts(&mut journal);
        assert_eq!(result.len(), 1);
        assert!(
            !top_level_is_applied(&result[0]),
            "all-failed merge must keep Failed top-level"
        );
    }

    /// When failed and successful CRAC receipts are interleaved within
    /// one EVM transaction, the merged internal operations must reflect
    /// execution order — not all-failed-before-all-pending.
    ///
    /// Simulates: `try { CRAC_1 } catch {}; CRAC_2; try { CRAC_3 } catch {}`
    /// where CRAC_1 and CRAC_3 fail, CRAC_2 succeeds.
    #[test]
    fn test_drain_preserves_interleaved_execution_order() {
        use tezosx_journal::{CracId, TezosXJournal};
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        // Execution order: failed(100), pending(200), failed(300)
        journal
            .michelson
            .push_failed_crac_receipt(dummy_crac_receipt(vec![
                make_crac_event(0),
                make_transfer(1, 100),
            ]));
        journal
            .michelson
            .push_pending_crac_receipt(dummy_crac_receipt(vec![make_transfer(2, 200)]));
        journal
            .michelson
            .push_failed_crac_receipt(dummy_crac_receipt(vec![make_transfer(3, 300)]));

        let result = super::drain_pending_crac_receipts(&mut journal);

        assert_eq!(result.len(), 1);
        assert_eq!(
            extract_amounts(&result[0]),
            vec![100, 200, 300],
            "internal transfers must appear in execution order, not \
             all-failed-before-all-pending"
        );
    }

    /// Merging two CRAC receipts preserves block-global nonces.
    ///
    /// Receipt 1: [Event("crac", nonce=0), Transfer(M_1, nonce=1, amount=100)]
    /// Receipt 2: [Event("crac", nonce=2), Transfer(M_2, nonce=3, amount=200)]
    /// Expected:  [Event("crac", nonce=0), Transfer(M_1, nonce=1), Transfer(M_2, nonce=3)]
    ///
    /// The duplicate event (nonce=2) is dropped.  Nonces are block-global
    /// (assigned at creation time, matching L1 semantics) so no renumbering.
    #[test]
    fn test_merge_crac_internals_preserves_nonces() {
        let receipt_1 =
            dummy_crac_receipt(vec![make_crac_event(0), make_transfer(1, 100)]);
        let receipt_2 =
            dummy_crac_receipt(vec![make_crac_event(2), make_transfer(3, 200)]);

        let mut receipts = vec![receipt_1, receipt_2];
        let rest = receipts.split_off(1);
        let target = &mut receipts[0];
        for other in rest {
            super::merge_crac_internals(target, other);
        }

        // Nonces are preserved from creation time (block-global counter).
        // Event nonce=2 from receipt_2 is dropped (duplicate), so: 0, 1, 3.
        assert_eq!(
            extract_nonces(target),
            vec![0, 1, 3],
            "nonces must be preserved from creation time"
        );

        // Transfer ordering: M_1 (100), then M_2 (200)
        assert_eq!(
            extract_amounts(target),
            vec![100, 200],
            "transfers must be in receipt order"
        );

        // First op is the single CRAC event (duplicate filtered)
        use tezos_tezlink::operation_result::{
            InternalOperationSum, OperationDataAndMetadata, OperationResultSum,
        };
        let OperationDataAndMetadata::OperationWithMetadata(ref batch) =
            target.op_and_receipt;
        let OperationResultSum::Transfer(ref result) = batch.operations[0].receipt else {
            panic!("expected Transfer receipt");
        };
        assert_eq!(
            result.internal_operation_results.len(),
            3,
            "1 event + 2 transfers"
        );
        assert!(
            matches!(
                result.internal_operation_results[0],
                InternalOperationSum::Event(_)
            ),
            "first internal op must be the CRAC event"
        );
    }
}
