// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use http::StatusCode;
use mir::ast::{big_map::BigMapId, AddressHash};
use mir::gas::{Gas, OutOfGas};
use primitive_types::U256;
use std::collections::BTreeMap;
use tezos_crypto_rs::{
    blake2b,
    hash::{BlockHash, ChainId, ContractKt1Hash, OperationHash, UnknownSignature},
};
// UnknownSignature has a private constructor; use try_from to build one.
const ZERO_SIGNATURE: [u8; 64] = [0u8; 64];
use tezos_data_encoding::{
    enc::BinWriter,
    types::{Narith, Zarith},
};
use tezos_evm_logging::{log, Level::*};
use tezos_execution::{
    account_storage::{TezlinkAccount, TezosOriginatedAccount},
    context::Context,
    cross_runtime_transfer,
    enshrined_contracts::CracError,
    mir_ctx::{clear_temporary_big_maps, OperationCtx, TcCtx},
    originate_contract, typecheck_code_and_storage, CracTransferError, OriginationNonce,
    TezlinkOperationGas,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_smart_rollup_host::storage::StorageV1;
// `Parameters` could come from `tezos_protocol::operation`, but we also need
// `tezos_tezlink` for types that live only there (OperationHash, BlockNumber,
// TransferError). To avoid the dependency altogether, those types would need
// to be moved to a shared crate.
use tezos_tezlink::{
    block::AppliedOperation,
    operation::{
        ManagerOperation, ManagerOperationContent, OriginationContent, Parameters,
        Script, TransferContent,
    },
    operation_result::{
        ApplyOperationError, ApplyOperationErrors, BacktrackedResult, ContentResult,
        EventContent, EventSuccess, InternalContentWithMetadata, InternalOperationSum,
        OperationBatchWithMetadata, OperationDataAndMetadata, OperationResult,
        OperationResultSum, OperationWithMetadata, OriginationSuccess, TransferError,
        TransferSuccess, TransferTarget,
    },
};
use tezosx_interfaces::{
    AliasInfo, Classification, CrossRuntimeContext, Origin, Registry, RuntimeId,
    RuntimeInterface, TezosXRuntimeError, ALIAS_LOOKUP_MILLIGAS, X_TEZOS_GAS_CONSUMED,
};
use tezosx_journal::{DispatchSlotError, TezosXJournal};

use tezos_smart_rollup_host::path::RefPath;

/// Unified storage root for all Tezos account state — Michelson originated
/// KT1 contracts (under `contracts/`), big_maps (under `big_map/`) and
/// TezosX projected accounts (under `tezosx/`). Mirrors the kernel's
/// `chains::TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH` (the kernel and the
/// TezosX runtime live in separate crates so the constant is duplicated
/// to avoid a circular dependency).
pub(crate) const TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH: RefPath =
    RefPath::assert_from(b"/tez/tez_accounts");

/// Re-export the canonical null PKH from `tezos_execution` so the rest
/// of this crate can keep its short name.  Both the synthetic
/// CRAC-ID event built here and the `is_synthetic_crac_event`
/// predicate in `tezos_execution::enshrined_contracts` match against
/// the same constant.
pub(crate) use tezos_execution::NULL_PKH;

use crate::{
    account::{
        get_origin_at, get_tezos_account_info_or_init, narith_to_u256, set_origin_at,
        set_tezos_account_info,
    },
    context::TezosRuntimeContext,
};

pub struct TezosRuntime(pub ChainId);

pub mod account;
pub mod alias_forwarder;
pub mod context;
pub mod headers;
pub mod url;
pub mod view;

/// Synthetic "applied" top-level result used by both the success-path
/// CRAC receipt builder and the post-merge reconciliation in the kernel.
/// Exposed so the two sites cannot drift: the top-level is a wrapper —
/// real gas / storage-size / balance-updates live on the internal ops.
pub fn crac_top_level_applied_result() -> ContentResult<TransferContent> {
    ContentResult::Applied(TransferTarget::from(TransferSuccess::default()))
}

/// Build an HTTP response from the result of [`execute_request`].
///
/// `frame_result` carries the `%collect_result` payload taken out of the
/// dispatch slot that [`TezosRuntime::serve`] opened for this call. It is
/// only surfaced on 2xx responses — on 4xx/5xx the operation is about to
/// be reverted, so the payload is discarded.
///
/// Request-level errors are turned into HTTP error responses:
/// - `Ok(())` → 200, body = `frame_result` (or empty), `Content-Type: application/octet-stream`
/// - `BadRequest` → 400
/// - `NotFound` → 404
///
/// All other errors propagate as `Err` — they indicate infrastructure
/// problems that cannot be meaningfully represented as HTTP responses.
///
// TODO: Review `Custom` errors in `execute_request` to extract more
// structured variants (and corresponding HTTP status codes) where
// possible.
fn build_response(
    result: Result<(), TezosXRuntimeError>,
    consumed_milligas: u64,
    frame_result: Option<Vec<u8>>,
) -> http::Response<Vec<u8>> {
    let gas_header = consumed_milligas.to_string();
    let mut builder = http::Response::builder().header(X_TEZOS_GAS_CONSUMED, &gas_header);
    let (status, body) = match result {
        Ok(()) => {
            builder =
                builder.header(http::header::CONTENT_TYPE, "application/octet-stream");
            (StatusCode::OK, frame_result.unwrap_or_default())
        }
        Err(TezosXRuntimeError::BadRequest(msg)) => {
            (StatusCode::BAD_REQUEST, msg.into_bytes())
        }
        Err(TezosXRuntimeError::NotFound(msg)) => {
            (StatusCode::NOT_FOUND, msg.into_bytes())
        }
        Err(TezosXRuntimeError::MethodNotAllowed(msg)) => {
            (StatusCode::METHOD_NOT_ALLOWED, msg.into_bytes())
        }
        Err(TezosXRuntimeError::OutOfGas) => {
            (StatusCode::TOO_MANY_REQUESTS, b"OOG".to_vec())
        }
        Err(e) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("{e:?}").into_bytes(),
        ),
    };
    // Safe to unwrap: status is a predefined constant, header names are
    // static ASCII strings, and the values are ASCII-only.
    builder.status(status).body(body).unwrap()
}

/// Reconcile the `execute_request` outcome with the dispatch-slot take
/// and build the HTTP response.
///
/// The dispatch slot is opened at `serve` entry and taken here.
/// `Err(NoSlot)` means the slot was popped by an intermediate caller
/// — a kernel bug.  When `result` is `Ok(())` we escalate that case to
/// a `Custom` error so the response is a 500 rather than an empty 2xx
/// that hides the inconsistency; when `result` is already `Err` the
/// request error takes precedence and the dispatch payload (if any)
/// is dropped on the floor, as it would be for any non-2xx.
fn finalize_response(
    result: Result<(), TezosXRuntimeError>,
    consumed_milligas: u64,
    dispatch: Result<Option<Vec<u8>>, DispatchSlotError>,
) -> http::Response<Vec<u8>> {
    let result = match (result, &dispatch) {
        (Ok(()), Err(e)) => Err(TezosXRuntimeError::Custom(format!(
            "dispatch slot inconsistency: {e}"
        ))),
        (other, _) => other,
    };
    let frame_result = dispatch.ok().flatten();
    build_response(result, consumed_milligas, frame_result)
}

/// Build a serialized two-step receipt for an incoming CRAC (EVM → Michelson).
///
/// The RFC (CRAC Derived Block Contents) specifies the receipt structure:
///
/// **Top-level**: handler (tz1) → source_alias (alias(E_0)), Applied
///   - **Internal op #0** (if CRAC event): CRAC event with CRAC-ID
///   - **Internal op #1**: sender_alias (alias(E_1)) → target, Applied(transfer result)
///   - **Internal ops 2..N**: further ops from Michelson execution
///
/// `source_contract` is alias(E_0) — the alias of the EVM tx originator
/// (from `X-Tezos-Source`). It is the destination of the top-level op.
///
/// `sender_contract` is alias(E_1) — the alias of the immediate EVM caller
/// (from `X-Tezos-Sender`). It is the sender of the internal op.
///
/// Returns the `AppliedOperation` that the block builder will store
/// in the Michelson runtime block.
#[allow(clippy::too_many_arguments)]
fn build_crac_receipt(
    null_pkh: &PublicKeyHash,
    source_contract: &Contract,
    sender_contract: &Contract,
    amount: &Narith,
    destination: &Contract,
    parameters: &Parameters,
    target: TransferTarget,
    pre_transfer_internals: Vec<InternalOperationSum>,
    internal_receipts: Vec<InternalOperationSum>,
    crac_id: Option<&str>,
    base_nonce: u16,
) -> Result<AppliedOperation, TezosXRuntimeError> {
    // Combine, in execution order:
    //   [CRAC event, ...pre_transfer_internals, alias(E_1)→target, ...further internal ops]
    // Per RFC, the CRAC event is always the first internal operation (#0).
    // `pre_transfer_internals` carries the alias-forwarder origination
    // internal ops materialized during alias resolution — they must
    // appear BEFORE the synthetic transfer because the EVM precompile
    // materializes the aliases before initiating the cross-runtime
    // call those aliases participate in.
    // Nonces written here are local placeholders; `renumber_nonces`
    // reassigns them block-wide from the flat list order at block
    // finalization, so what matters is the position in
    // `internal_operation_results`, not the numeric value below.
    let mut all_internal = Vec::new();
    let mut next_nonce = base_nonce;

    if let Some(id) = crac_id {
        use mir::{
            ast::annotations::NO_ANNS, ast::micheline::Micheline, ast::Entrypoint, lexer,
        };
        // Synthetic kernel-emitted CRAC event. The event is
        // *sponsored*: it is Micheline-encoded against a dedicated
        // kernel-owned gas counter (`Gas::default()` — the standard L1
        // per-operation cap, the idiom for non-user-billed kernel
        // bookkeeping) rather than the caller's remaining budget. The
        // encoded payload is tiny and bounded, so against this counter
        // the encode can never run out of gas: the event always lands
        // in the receipt. The internal-op `EventSuccess.consumed_milligas`
        // is hardcoded to 0 (RFC); the top-level receipt's
        // `consumed_milligas` no longer includes this encode cost — the
        // kernel absorbs it instead of charging the user (L2-1464).
        let mut sponsored_event_gas = Gas::default();
        let ty = Micheline::App(lexer::Prim::string, &[], NO_ANNS)
            .encode(&mut sponsored_event_gas)
            .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to encode CRAC event type: {e}"
                ))
            })?;
        let payload = Micheline::from(id.to_string())
            .encode(&mut sponsored_event_gas)
            .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to encode CRAC event payload: {e}"
                ))
            })?;
        let event_nonce = next_nonce;
        next_nonce = next_nonce.saturating_add(1);
        all_internal.push(InternalOperationSum::Event(InternalContentWithMetadata {
            content: EventContent {
                tag: Some(Entrypoint::from_string_unchecked(
                    tezos_execution::enshrined_contracts::SYNTHETIC_CRAC_EVENT_TAG.into(),
                )),
                payload: Some(payload.into()),
                ty: ty.into(),
            },
            sender: Contract::Implicit(null_pkh.clone()),
            nonce: event_nonce,
            result: ContentResult::Applied(EventSuccess {
                consumed_milligas: Narith(0u64.into()),
            }),
        }));
    }

    all_internal.extend(pre_transfer_internals);

    let transfer_internal = InternalOperationSum::Transfer(InternalContentWithMetadata {
        sender: sender_contract.clone(),
        nonce: next_nonce,
        content: TransferContent {
            amount: amount.clone(),
            destination: destination.clone(),
            parameters: parameters.clone(),
        },
        result: ContentResult::Applied(target),
    });
    all_internal.push(transfer_internal);
    all_internal.extend(internal_receipts);

    // Top-level result: handler → alias(E_0) (applied, with all internals nested).
    // TransferSuccess::default() is intentional — this is a synthetic wrapper;
    // actual consumed_milligas, storage_size, etc. are in the internal ops.
    let top_level_result = OperationResult {
        balance_updates: vec![],
        result: crac_top_level_applied_result(),
        internal_operation_results: all_internal,
    };

    let signature =
        UnknownSignature::try_from(ZERO_SIGNATURE.as_slice()).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to build zero signature: {e}"))
        })?;

    let op_data =
        OperationDataAndMetadata::OperationWithMetadata(OperationBatchWithMetadata {
            operations: vec![OperationWithMetadata {
                content: ManagerOperationContent::Transaction(ManagerOperation {
                    source: null_pkh.clone(),
                    fee: Narith(0u64.into()),
                    counter: Narith(0u64.into()),
                    gas_limit: Narith(0u64.into()),
                    storage_limit: Narith(0u64.into()),
                    operation: TransferContent {
                        // The top-level op is a synthetic container
                        // (handler → alias(E_0)); real amount is on the
                        // internal op, so we zero it like fee/gas_limit.
                        amount: Narith(0u64.into()),
                        destination: source_contract.clone(),
                        // Only the internal op carries the real target
                        // parameters.
                        parameters: Parameters::default(),
                    },
                }),
                receipt: OperationResultSum::Transfer(top_level_result),
            }],
            signature,
        });

    let mut serialized = Vec::new();
    op_data.bin_write(&mut serialized).map_err(|e| {
        TezosXRuntimeError::Custom(format!("Failed to serialize op: {e}"))
    })?;
    let hash = OperationHash::from(blake2b::digest_256(&serialized));

    Ok(AppliedOperation {
        hash,
        branch: BlockHash::default(),
        op_and_receipt: op_data,
    })
}

/// Build a failed CRAC receipt (RFC Example 4).
///
/// When `cross_runtime_transfer` fails, we still need a receipt in the
/// Michelson block so indexers see the failed CRAC.  The top-level
/// operation has `status: failed` and carries any partial internal
/// operations (with backtracked / failed / skipped statuses) so
/// indexers can see what was attempted.
#[allow(clippy::too_many_arguments)]
fn build_failed_crac_receipt(
    null_pkh: &PublicKeyHash,
    source_contract: &Contract,
    sender_contract: &Contract,
    amount: &Narith,
    destination: &Contract,
    parameters: &Parameters,
    error: TransferError,
    pre_transfer_internals: Vec<InternalOperationSum>,
    internal_receipts: Vec<InternalOperationSum>,
    crac_id: Option<&str>,
    base_nonce: u16,
) -> Result<AppliedOperation, TezosXRuntimeError> {
    // Per RFC, the CRAC event is always first, even on failure.
    // Since the downstream transfer failed, the event is backtracked
    // (matching Tezos protocol semantics where all applied internal ops
    // preceding a failure are backtracked).
    // `pre_transfer_internals` (alias-forwarder originations) keep
    // their applied status — those materializations happened before
    // the CRAC body ran and their storage effects are persistent.
    let mut all_internal = Vec::new();
    let mut next_nonce = base_nonce;
    if let Some(id) = crac_id {
        use mir::{
            ast::annotations::NO_ANNS, ast::micheline::Micheline, ast::Entrypoint, lexer,
        };
        // Synthetic kernel-emitted CRAC event (backtracked variant for
        // failed CRAC receipt). Like the success-path builder, the event
        // is *sponsored*: encoded against a dedicated kernel-owned gas
        // counter (`Gas::default()`) rather than the caller's remaining
        // budget. This is the crux of L2-1464: a gas-tight failed CRAC
        // must still record its failed receipt for indexers, so crafting
        // the event must never depend on (and never exhaust) the caller's
        // leftover gas. The internal-op `EventSuccess`'s `consumed_milligas`
        // stays 0 inside the `BacktrackedResult` (RFC).
        let mut sponsored_event_gas = Gas::default();
        let ty = Micheline::App(lexer::Prim::string, &[], NO_ANNS)
            .encode(&mut sponsored_event_gas)
            .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to encode CRAC event type: {e}"
                ))
            })?;
        let payload = Micheline::from(id.to_string())
            .encode(&mut sponsored_event_gas)
            .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to encode CRAC event payload: {e}"
                ))
            })?;
        let event_nonce = next_nonce;
        next_nonce = next_nonce.saturating_add(1);
        all_internal.push(InternalOperationSum::Event(InternalContentWithMetadata {
            content: EventContent {
                tag: Some(Entrypoint::from_string_unchecked(
                    tezos_execution::enshrined_contracts::SYNTHETIC_CRAC_EVENT_TAG.into(),
                )),
                payload: Some(payload.into()),
                ty: ty.into(),
            },
            sender: Contract::Implicit(null_pkh.clone()),
            nonce: event_nonce,
            result: ContentResult::BackTracked(BacktrackedResult {
                errors: None,
                result: EventSuccess {
                    consumed_milligas: Narith(0u64.into()),
                },
            }),
        }));
    }

    all_internal.extend(pre_transfer_internals);

    // Per RFC, include the failed transfer (alias(E_1) → target) so
    // indexers can see which contract call was attempted.
    all_internal.push(InternalOperationSum::Transfer(
        InternalContentWithMetadata {
            sender: sender_contract.clone(),
            nonce: next_nonce,
            content: TransferContent {
                amount: amount.clone(),
                destination: destination.clone(),
                parameters: parameters.clone(),
            },
            result: ContentResult::Failed(ApplyOperationErrors::from(
                ApplyOperationError::Transfer(error.clone()),
            )),
        },
    ));
    // Internal receipts already have block-global nonces from execution.
    all_internal.extend(internal_receipts);

    let top_level_result = OperationResult {
        balance_updates: vec![],
        result: ContentResult::Failed(ApplyOperationErrors::from(
            ApplyOperationError::Transfer(error),
        )),
        internal_operation_results: all_internal,
    };

    let signature =
        UnknownSignature::try_from(ZERO_SIGNATURE.as_slice()).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to build zero signature: {e}"))
        })?;

    let op_data =
        OperationDataAndMetadata::OperationWithMetadata(OperationBatchWithMetadata {
            operations: vec![OperationWithMetadata {
                content: ManagerOperationContent::Transaction(ManagerOperation {
                    source: null_pkh.clone(),
                    fee: Narith(0u64.into()),
                    counter: Narith(0u64.into()),
                    gas_limit: Narith(0u64.into()),
                    storage_limit: Narith(0u64.into()),
                    operation: TransferContent {
                        amount: Narith(0u64.into()),
                        destination: source_contract.clone(),
                        parameters: Parameters::default(),
                    },
                }),
                receipt: OperationResultSum::Transfer(top_level_result),
            }],
            signature,
        });

    let mut serialized = Vec::new();
    op_data.bin_write(&mut serialized).map_err(|e| {
        TezosXRuntimeError::Custom(format!("Failed to serialize op: {e}"))
    })?;
    let hash = OperationHash::from(blake2b::digest_256(&serialized));

    Ok(AppliedOperation {
        hash,
        branch: BlockHash::default(),
        op_and_receipt: op_data,
    })
}

/// Build an [`InternalOperationSum::Origination`] surfacing an
/// alias-forwarder materialization (`ensure_alias` branch 3). The
/// `nonce` is a 0 placeholder; the receiving runtime stamps the real
/// nonce when assembling the CRAC receipt. The result carries the
/// [`OriginationSuccess`] returned by [`originate_contract`] —
/// script, balance updates, consumed gas, `originated_contracts`.
fn build_alias_origination_internal(
    null_pkh: &PublicKeyHash,
    script: Script,
    success: OriginationSuccess,
) -> InternalOperationSum {
    InternalOperationSum::Origination(InternalContentWithMetadata {
        sender: Contract::Implicit(null_pkh.clone()),
        nonce: 0,
        content: OriginationContent {
            balance: Narith(0u64.into()),
            delegate: None,
            script,
        },
        result: ContentResult::Applied(success),
    })
}

/// Execute a cross-runtime request: dispatches on the HTTP method.
///
/// - `POST` → entrypoint call (existing state-mutating transfer path).
/// - `GET`  → view call (read-only execution of a named Michelson view).
/// - anything else → catchable 405 (`MethodNotAllowed`).
///
/// This is the core logic behind [`TezosRuntime::serve`], separated so
/// that `serve` only handles the error-to-HTTP-status mapping.
///
/// `consumed_milligas` is an output parameter rather than part of the
/// return type so that early `?` returns keep their ergonomics, since
/// this result is used as-is to build the http reponse.
fn execute_request<Host>(
    chain_id: &ChainId,
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
    consumed_milligas: &mut u64,
) -> Result<(), TezosXRuntimeError>
where
    Host: StorageV1,
{
    match *request.method() {
        http::Method::POST => {
            execute_entrypoint_call(
                chain_id,
                registry,
                host,
                journal,
                request,
                consumed_milligas,
            )
            .map(|_| ())
        }
        http::Method::GET => {
            // The view path doesn't mutate storage or push a CRAC
            // receipt, but it still deposits its Micheline result into
            // the dispatch slot opened by `serve` (same slot the
            // entrypoint path fills via `%collect_result`), so the
            // journal is threaded through. The registry is threaded
            // too so the view can issue nested cross-runtime reads
            // through the gateway's `staticcall_evm` synthetic view
            // (L2-1259).
            view::execute_view_call(
                chain_id,
                registry,
                host,
                journal,
                request,
                consumed_milligas,
            )
        }
        ref other => Err(TezosXRuntimeError::MethodNotAllowed(format!(
            "HTTP method {other} not allowed (use POST for entrypoint calls or GET for views)"
        ))),
    }
}

/// Execute an entrypoint call (state-mutating transfer targeting a
/// Michelson contract or implicit account).
fn execute_entrypoint_call<Host>(
    chain_id: &ChainId,
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
    consumed_milligas: &mut u64,
) -> Result<TransferSuccess, TezosXRuntimeError>
where
    Host: StorageV1,
{
    // Drain on entry, NOT on exit: pending alias-origination
    // internal ops in the journal at this point can only belong
    // to THIS call's gateway precompile invocation, since:
    //   (a) any prior outer cross-runtime call has already entered
    //       its own `execute_entrypoint_call` and drained whatever
    //       was pending for IT,
    //   (b) any re-entrant inner cross-runtime call that
    //       `cross_runtime_transfer` triggers below will, in turn,
    //       drain its own aliases at its entry — never seeing our
    //       captured ones because we already moved them out of the
    //       journal here.
    // The result is per-call scoping with no watermarking logic.
    let alias_origination_internals =
        journal.michelson.take_pending_alias_origination_internals();
    let parsed = url::parse_tezos_url(request.uri())?;
    let hdrs = headers::parse_request_headers(request.headers())?;

    // Start the op-gas counter early so the Unit fallback below can be
    // metered against the user's actual budget.
    let mut gas = TezlinkOperationGas::start_milligas(hdrs.gas_limit)
        .map_err(|e| TezosXRuntimeError::Custom(format!("Failed to start gas: {e:?}")))?;

    let body = request.into_body();
    // An empty body means "no parameters" which defaults to Micheline Unit.
    // This is required for implicit account transfers where the Michelson VM
    // checks that param == Unit.
    let value = if body.is_empty() {
        mir::ast::micheline::Micheline::from(())
            .encode(&mut gas.remaining)
            .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!("Failed to encode Unit: {e}"))
            })?
    } else {
        body
    };
    let parameters = Parameters {
        entrypoint: parsed.entrypoint,
        value,
    };

    let context =
        TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;

    // Verify the incoming CRAC ID matches the journal's (set by the
    // block builder).  This ensures consistency across runtime boundaries.
    if let Some(ref crac_id) = hdrs.crac_id {
        journal
            .verify_crac_id(crac_id)
            .map_err(|e| TezosXRuntimeError::Custom(format!("CRAC-ID mismatch: {e}")))?;
    }

    // SOURCE is always the null implicit account.  Michelson requires
    // SOURCE to be tz1/tz2/tz3; the null PKH fills that role for both
    // CRAC and non-CRAC requests.
    let is_crac = hdrs.crac_origin_contract.is_some();
    // Every CRAC receipt carries the synthetic CRAC-ID event.  The
    // merge stage (`merge_crac_internals` for top-level CRACs and
    // `drain_reentrant_crac_ops` for re-entrant inner CRACs) dedupes
    // events tagged `"crac"` with the null implicit sender so the
    // final synthetic Michelson manager-op carries exactly one.
    // Always-emitting keeps each receipt self-contained: when an EVM
    // revert wipes pending receipts but failed ones survive (see
    // `MichelsonJournal::revert_frame`), the surviving failed
    // receipt brings its own CRAC-ID event with it.
    //
    // The synthetic event is only useful when the originating
    // runtime is *not* Tezos: Tezos-originated CRACs already surface
    // naturally on the Michelson side and don't need the
    // cross-runtime correlation key.  Phrasing the predicate as
    // "not Tezos" rather than "is Ethereum" keeps the semantics
    // forward-compatible with hypothetical additional non-Tezos
    // runtimes.
    //
    // `CracId::origin_runtime` is a raw `u8` (the journal crate
    // cannot import `RuntimeId` because of the inverse dep), but
    // `u8::from(RuntimeId::Tezos)` resolves to the same canonical
    // discriminant so the comparison stays typed-by-construction.
    let crac_id_for_event =
        if is_crac && journal.crac_id().origin_runtime != u8::from(RuntimeId::Tezos) {
            Some(journal.crac_id().to_string())
        } else {
            None
        };
    let source_pkh = PublicKeyHash::from_b58check(NULL_PKH).map_err(|e| {
        TezosXRuntimeError::ConversionError(format!("Failed to parse null address: {e}"))
    })?;
    let source_account =
        context
            .implicit_from_public_key_hash(&source_pkh)
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to fetch source account: {e:?}"
                ))
            })?;

    // Seed the per-operation temporary big-map id to -1 (negative =
    // temporary, per `BigMapId::is_temporary`), matching the native
    // batch path (`tezos_execution::apply_operations`) and the
    // alias-origination path below. A non-negative seed (e.g. 0) is
    // NOT recognized as temporary, so transient big-maps would be
    // written to — and leak into — the durable `/big_map/<id>`
    // namespace.
    let mut next_temp_id = BigMapId {
        value: Zarith((-1).into()),
    };
    let mut tc_ctx = TcCtx {
        host: &mut *host,
        context: &context,
        operation_gas: &mut gas,
        big_map_diff: BTreeMap::new(),
        next_temporary_id: &mut next_temp_id,
    };
    // Resume the inbound-CRAC origination nonce from the Michelson
    // journal so two sequential inbound CRACs in the same synthetic
    // op (e.g. an EVM transaction calling two distinct Michelson
    // callees in turn) keep monotonically increasing indices and
    // therefore derive distinct child KT1s — child KT1 is
    // `digest_160(operation_hash[32] || index[4])`, so without this
    // each handler invocation would restart from index 0 and the
    // two CRACs' `CREATE_CONTRACT`s would land on the same KT1
    // (L2-1366).
    //
    // The operation hash is set on the journal at construction time
    // by the caller (`TezosXJournal::new`) and is stable for the
    // rest of the journal's lifetime; the canonical derivation
    // (`crac:<block_number>:<crac_id>`) makes two different
    // synthetic Michelson manager operations see distinct seeds
    // (cross-operation and cross-block isolation), mirroring the
    // per-op-hash entropy the native L1 path obtains from the real
    // operation hash in `tezos_execution::apply_operation`.
    let mut nonce = OriginationNonce {
        operation: journal.michelson.operation_hash().clone(),
        index: journal.michelson.origination_index(),
    };
    let mut counter = 0u128;
    let mut operation_ctx = OperationCtx {
        source: &source_account,
        origination_nonce: &mut nonce,
        counter: &mut counter,
        level: &hdrs.block_number,
        now: &hdrs.timestamp,
        chain_id,
        source_public_key: &[],
        crac_chain_depth: hdrs.crac_depth,
        // Retain the inbound CRAC originator so the gateway can forward
        // it (translated) as the outbound source, keeping `tx.origin`
        // invariant across runtime hops (L2-1363). May be `Originated(KT1)`
        // (EVM alias) or `Implicit(pkh)` (native Michelson origin). `None`
        // only for a top-level Michelson tx (absent `X-Tezos-Source`).
        crac_origin: hdrs.crac_origin_contract.clone(),
    };
    let parser = mir::parser::Parser::new();

    // Each operation uses 0-based nonces; block-sequential nonces are
    // assigned at block finalization by renumber_nonces().
    let base_nonce: u16 = 0;
    // Pre-allocate nonce slots for the event and transfer wrapper that
    // build_crac_receipt will prepend, so MIR ops start after them.
    let mut nonce_counter: u16 = if is_crac {
        if crac_id_for_event.is_some() {
            2
        } else {
            1
        }
    } else {
        0
    };
    // Stamp the alias-forwarder origination internal ops (drained
    // from the journal at function entry) with placeholder nonces
    // taken from `nonce_counter` BEFORE running `cross_runtime_transfer`,
    // so MIR ops receive higher local nonces than the alias ops. The
    // alias ops are passed to `build_crac_receipt` separately and get
    // placed between the synthetic CRAC event and the synthetic
    // transfer in the final receipt — the cross-runtime call enters
    // Michelson AFTER its aliases have been materialized. The flat-
    // list order is what matters: `renumber_nonces` reassigns
    // block-global nonces at block finalization based on that order.
    let alias_origination_internals: Vec<InternalOperationSum> =
        alias_origination_internals
            .into_iter()
            .map(|op| {
                let n = nonce_counter;
                nonce_counter = nonce_counter.checked_add(1).ok_or_else(|| {
                    TezosXRuntimeError::Custom(
                        "alias-origination internal-op nonce counter overflowed"
                            .to_string(),
                    )
                })?;
                Ok(match op {
                    InternalOperationSum::Origination(mut meta) => {
                        meta.nonce = n;
                        InternalOperationSum::Origination(meta)
                    }
                    other => other,
                })
            })
            .collect::<Result<_, TezosXRuntimeError>>()?;
    // Dispatch on the parsed `Contract` variant to build the right
    // concrete account, mirroring the pattern used by
    // `tezos_execution::transfer` for the destination.
    let transfer_result = match &hdrs.sender {
        Contract::Originated(kt1) => {
            let sender_account = context.originated_from_kt1(kt1).map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to fetch sender account: {e:?}"
                ))
            })?;
            cross_runtime_transfer(
                &mut tc_ctx,
                &mut operation_ctx,
                registry,
                journal,
                &sender_account,
                &hdrs.amount,
                &parsed.destination,
                &parameters,
                &parser,
                &mut nonce_counter,
            )
        }
        Contract::Implicit(pkh) => {
            let sender_account =
                context.implicit_from_public_key_hash(pkh).map_err(|e| {
                    TezosXRuntimeError::Custom(format!(
                        "Failed to fetch sender account: {e:?}"
                    ))
                })?;
            cross_runtime_transfer(
                &mut tc_ctx,
                &mut operation_ctx,
                registry,
                journal,
                &sender_account,
                &hdrs.amount,
                &parsed.destination,
                &parameters,
                &parser,
                &mut nonce_counter,
            )
        }
    };
    // Persist the post-execution origination index back to the
    // journal on every path (success, failure, OOG): MIR has had
    // its mutable borrow of `nonce` released by `cross_runtime_transfer`
    // returning, so `nonce.index` now reflects all `CREATE_CONTRACT`s
    // performed during this inbound CRAC.  Writing it back here lets
    // the next inbound CRAC in the same synthetic op (e.g. a second
    // EVM→Michelson call in the same transaction) resume from a
    // distinct index rather than re-using an already-consumed slot.
    // A subsequent `revert_frame` on the enclosing EVM frame will
    // roll the index back to the value captured at frame entry.
    journal.michelson.set_origination_index(nonce.index);
    let result = match transfer_result {
        Ok(r) => r,
        Err(CracTransferError {
            error,
            internal_receipts,
        }) => {
            // Map CracError to its TezosXRuntimeError HTTP-status equivalent.
            // OutOfGas → 429 (catchable revert); user-facing TransferError
            // variants → 400 (catchable revert); BlockAbort → 500 (abort
            // the EVM caller's frame, then the block).
            //
            // The infra-class TransferError variants (FailedTo*) are routed
            // to BlockAbort upstream by `From<TransferError> for CracError`
            // — see `enshrined_contracts.rs:56-`. They never appear here as
            // CracError::Operation, so this match doesn't enumerate them.
            let rt_err = match &error {
                CracError::Operation(TransferError::OutOfGas(OutOfGas)) => {
                    TezosXRuntimeError::OutOfGas
                }
                CracError::Operation(e) => TezosXRuntimeError::BadRequest(e.to_string()),
                CracError::BlockAbort(msg) => {
                    TezosXRuntimeError::Custom(format!("block abort: {msg}"))
                }
            };
            if is_crac {
                // Build a failed receipt so the Michelson block records the
                // failed CRAC (RFC Example 4), including any partial
                // internal operations with backtracked/failed/skipped statuses.
                // Only emitted for operation-level failures; BlockAbort aborts
                // the whole block, so no per-operation receipt is produced.
                //
                // The builder sponsors its own synthetic-event encoding
                // (see `build_failed_crac_receipt`), so it does NOT draw on
                // the caller's leftover gas and cannot fail with OutOfGas —
                // a gas-tight failed CRAC still records its receipt here
                // (L2-1464). The `Err` arm below therefore only covers the
                // (practically unreachable) serialization/signature paths.
                if let (Some(source_contract), CracError::Operation(te)) =
                    (hdrs.crac_origin_contract.as_ref(), error)
                {
                    match build_failed_crac_receipt(
                        &source_pkh,
                        source_contract,
                        &hdrs.sender,
                        &hdrs.amount,
                        &parsed.destination,
                        &parameters,
                        te,
                        alias_origination_internals,
                        internal_receipts,
                        crac_id_for_event.as_deref(),
                        base_nonce,
                    ) {
                        Ok(receipt) => {
                            journal.michelson.push_failed_crac_receipt(receipt);
                        }
                        Err(e) => {
                            log!(Error, "Failed to build failed CRAC receipt: {e:?}");
                        }
                    }
                }
            }
            // Report consumed gas even on failure so the caller doesn't
            // see u64::MAX (which would exhaust the EVM gas budget and
            // prevent try/catch from working).
            *consumed_milligas = gas.total_milligas_consumed().into();
            return Err(rt_err);
        }
    };

    // Clear the temporary big-maps allocated during this CRAC's
    // Michelson execution, mirroring the native batch path
    // (`tezos_execution::apply_operations`). Without this, the
    // transient big-maps persist in durable storage after the
    // operation. Only the success path needs it: every failure path in
    // `cross_runtime_transfer` reverts the world-state checkpoint,
    // which already removes any temp big-maps written during the
    // reverted execution.
    if let Err(e) = clear_temporary_big_maps(host, &context, &mut next_temp_id) {
        log!(Error, "Failed to clear temporary big-maps after CRAC: {e}");
    }

    // For cross-runtime calls (CRAC), build the two-step receipt
    // structure per RFC: CRAC Derived Block Contents and store it
    // in the journal for the block builder.
    //
    // source_contract = alias(E_0) from X-Tezos-Source (top-level destination)
    // hdrs.sender    = alias(E_1) from X-Tezos-Sender (internal sender)
    //
    // The builder sponsors its own synthetic-event encoding (against a
    // dedicated kernel-owned gas counter), so it neither charges the
    // caller for the kernel bookkeeping nor can it downgrade a
    // successful CRAC to an OutOfGas error when the caller's leftover
    // budget is tight (L2-1464).
    let transfer = if is_crac {
        let source_contract = hdrs.crac_origin_contract.as_ref().ok_or_else(|| {
            TezosXRuntimeError::Custom(
                "is_crac set but crac_origin_contract is None".into(),
            )
        })?;
        // Pass the alias-forwarder origination internals separately so
        // `build_crac_receipt` can insert them BETWEEN the CRAC event
        // and the synthetic transfer — the materializations happen
        // before the cross-runtime transfer they participate in.
        let receipt = build_crac_receipt(
            &source_pkh,
            source_contract,
            &hdrs.sender,
            &hdrs.amount,
            &parsed.destination,
            &parameters,
            result.target,
            alias_origination_internals,
            result.internal_receipts,
            crac_id_for_event.as_deref(),
            base_nonce,
        )?;
        journal.michelson.push_pending_crac_receipt(receipt);
        TransferSuccess::default()
    } else {
        result.target.into()
    };
    *consumed_milligas = gas.total_milligas_consumed().into();
    Ok(transfer)
}

// --- Alias generation gas constants (milligas) ---
//
// Alias generation runs inside an existing CRAC frame whose outer
// transaction already paid the manager-operation envelope. Branch 3
// (full materialization) is metered end-to-end through
// `originate_contract`'s receipt (`consumed_milligas`), including the
// classification write — `originate_contract` writes the origin
// itself when given `Origin::Alias`. Branch 2 (legacy patch-only)
// performs a single classification write outside any receipt and
// hand-rolls its gas envelope below. Computational steps (the BLAKE2b
// digest, base58 encoding, alias_info parsing) are absorbed by the
// surrounding gas budget and are not charged separately.
//
// TODO https://linear.app/tezos/issue/L2-435/durable-storage-readwrites-induces-gas-costs
// `originate_contract`'s receipt currently underreports gas: the
// kernel does not charge gas envelopes for durable-store writes,
// unlike L1's carbonated storage layer. Once L2-435 lands and the
// storage primitives meter every read/write, every alias branch
// inherits correct accounting automatically — and the
// `STORAGE_WRITE_BASE_MILLIGAS` charge in branch 2 should fold into
// the same uniform helper.

/// Fixed overhead for a single durable-store write (independent of
/// payload size): path resolution, PVM host call boundary, journal
/// bookkeeping. Used in branch 2 for the standalone classification
/// write.
const STORAGE_WRITE_BASE_MILLIGAS: u64 = 2_000;

impl RuntimeInterface for TezosRuntime {
    fn ensure_alias<Host>(
        &self,
        _registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        alias_info: AliasInfo,
        _native_public_key: Option<&[u8]>,
        _context: CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        // The native address is stored in `alias_info` as the UTF-8
        // bytes of the canonical address string. Decode once for the
        // forwarder storage payload below; the hash and the
        // classification record both work on the bytes directly.
        let native_address =
            std::str::from_utf8(&alias_info.native_address).map_err(|e| {
                TezosXRuntimeError::ConversionError(format!(
                    "alias_info.native_address is not valid UTF-8: {e}"
                ))
            })?;

        // Gas costs in milligas, charged incrementally so we fail early.
        // The closure pattern would persist a borrow on `remaining`; a
        // free function lets the borrow last only for the duration of
        // each call so we can also pass `remaining` to
        // `TezlinkOperationGas::start_milligas` below.
        let mut remaining = gas_remaining;
        fn consume(remaining: &mut u64, cost: u64) -> Result<(), TezosXRuntimeError> {
            *remaining = remaining.checked_sub(cost).ok_or_else(|| {
                TezosXRuntimeError::Custom(
                    "Out of gas during alias generation".to_string(),
                )
            })?;
            Ok(())
        }

        // Derive the deterministic alias address. The BLAKE2b digest
        // and the base58 encoding are computational steps absorbed by
        // the surrounding gas budget — alias generation runs inside an
        // existing CRAC frame whose envelope already covers the
        // computation. Only durable-store writes are metered below.
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(&alias_info.native_address));
        let kt1_str = kt1.to_base58_check();

        let context =
            TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;
        let account = context.originated_from_kt1(&kt1)?;
        let account_path = account.path().clone();

        // Branch 1: already classified as alias. Returning early
        // preserves the gas budget and performs no durable writes.
        match get_origin_at(host, &account_path)? {
            Some(Origin::Alias(_)) => {
                return Ok((kt1_str, remaining));
            }
            Some(Origin::Native) => {
                return Err(TezosXRuntimeError::Custom(format!(
                    "ensure_alias: account {kt1_str} is recorded as Native, refusing to overwrite"
                )));
            }
            None => {}
        }

        // Branch 2: a forwarder is already deployed but the
        // classification path is empty. Write the classification only
        // and skip the redeploy. The patch costs one durable write.
        if account.exists(host).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to check alias existence: {e}"))
        })? {
            consume(&mut remaining, STORAGE_WRITE_BASE_MILLIGAS)?;
            let new_origin = Origin::Alias(alias_info);
            set_origin_at(host, &account_path, &new_origin)?;
            return Ok((kt1_str, remaining));
        }

        // Branch 3: full materialization. Deploy the forwarder via the
        // shared `originate_contract` path, passing `Origin::Alias` so
        // the classification write is performed in the same call.
        let code = alias_forwarder::forwarder_code().map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "Failed to decode forwarder code from hex: {e}"
            ))
        })?;
        // Initialize the encoding counter with the remaining op-gas
        // budget so the encoding work is metered against the user's
        // actual budget; OOG inside the encode means insufficient
        // budget, not a synthetic high-cap failure.
        let mut encoding_gas = Gas::new(u32::try_from(remaining).unwrap_or(u32::MAX));
        let storage =
            alias_forwarder::forwarder_storage(native_address, &mut encoding_gas)
                .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
                .map_err(|e| {
                    TezosXRuntimeError::Custom(format!(
                        "Failed to encode forwarder storage: {e}"
                    ))
                })?;

        // `forwarder_storage` returned `Ok`, so the counter cannot be
        // exhausted. Surface a clear error rather than silently
        // miscounting if a future refactor ever breaks that invariant.
        let remaining_after_encode = encoding_gas.milligas().ok_or_else(|| {
            TezosXRuntimeError::Custom(
                "internal invariant violated: forwarder_storage returned Ok \
                 but encoding_gas counter is exhausted"
                    .into(),
            )
        })?;
        remaining = remaining_after_encode.into();

        // Build the synthetic source/sender account. NULL_PKH carries
        // zero balance, which matches the initial-balance transfer of
        // zero requested below.
        let null_pkh = PublicKeyHash::from_b58check(NULL_PKH).map_err(|e| {
            TezosXRuntimeError::ConversionError(format!("Failed to parse null PKH: {e}"))
        })?;
        let source_account =
            context
                .implicit_from_public_key_hash(&null_pkh)
                .map_err(|e| {
                    TezosXRuntimeError::Custom(format!(
                        "Failed to fetch null source account: {e:?}"
                    ))
                })?;

        let script = Script { code, storage };
        let origin = Origin::Alias(alias_info);
        let receipt = {
            let mut gas =
                TezlinkOperationGas::start_milligas(remaining).map_err(|e| {
                    TezosXRuntimeError::Custom(format!("Failed to start gas: {e:?}"))
                })?;
            let mut next_temp_id = BigMapId {
                value: Zarith((-1).into()),
            };
            let mut tc_ctx = TcCtx {
                host: &mut *host,
                context: &context,
                operation_gas: &mut gas,
                big_map_diff: BTreeMap::new(),
                next_temporary_id: &mut next_temp_id,
            };
            let parser = mir::parser::Parser::new();
            let typed_storage = typecheck_code_and_storage(&mut tc_ctx, &parser, &script)
                .map_err(|e| {
                    TezosXRuntimeError::Custom(format!(
                        "Failed to typecheck forwarder script: {e:?}"
                    ))
                })?;
            originate_contract(
                &mut tc_ctx,
                kt1,
                &source_account,
                &Narith(0u64.into()),
                &script.code,
                typed_storage,
                &origin,
            )
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to originate alias forwarder: {e:?}"
                ))
            })?
        };
        let consumed: u64 = (&receipt.consumed_milligas.0).try_into().map_err(|_| {
            TezosXRuntimeError::Custom(
                "consumed_milligas does not fit in u64".to_string(),
            )
        })?;
        consume(&mut remaining, consumed)?;

        // Park the internal `Origination` on the Michelson journal.
        // `execute_entrypoint_call` drains the journal at entry of
        // every cross-runtime call it serves and folds the entries
        // into THAT call's CRAC receipt. The kernel never sees a
        // separate AppliedOperation for the alias materialization;
        // it surfaces only as an internal op of the enclosing CRAC.
        let internal_op = build_alias_origination_internal(&null_pkh, script, receipt);
        journal
            .michelson
            .push_pending_alias_origination_internal(internal_op);

        Ok((kt1_str, remaining))
    }

    fn compute_alias(&self, native_address: &[u8]) -> Result<String, TezosXRuntimeError> {
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(native_address));
        Ok(kt1.to_base58_check())
    }

    /// Execute a cross-runtime call where the sender's balance was already
    /// debited by the calling runtime (e.g. EVM gateway). This handles both
    /// implicit and originated destinations, including Michelson code execution
    /// and internal operations.
    fn serve<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1,
    {
        // Open a dispatch slot for this CRAC entry so that any inner
        // %collect_result deposits land here. The slot is owned by
        // `serve` and is disjoint from REVM's external_checkpoints
        // stack — durable-state isolation across CRAC boundaries is
        // handled separately by `cross_runtime_transfer`'s own
        // snapshot mechanism.
        journal.michelson.push_dispatch_slot();
        // Default to max: if execute_request fails before writing the
        // actual value (early setup error), the caller sees full gas
        // consumption rather than a free call.
        let mut consumed_milligas = u64::MAX;
        let result = execute_request(
            &self.0,
            registry,
            host,
            journal,
            request,
            &mut consumed_milligas,
        );
        finalize_response(
            result,
            consumed_milligas,
            journal.michelson.take_dispatch_result(),
        )
    }

    fn host(&self) -> &'static str {
        "tezos"
    }

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        let contract = Contract::from_b58check(address_str).map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to parse address from string: {e}"
            ))
        })?;
        contract.to_bytes().map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to encode address to bytes: {e}"
            ))
        })
    }

    fn read_origin<Host>(
        &self,
        host: &Host,
        addr: &str,
        budget: u64,
    ) -> Result<(Classification, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        // Malformed → Unknown, no charge.
        let contract = match Contract::from_b58check(addr) {
            Ok(c) => c,
            Err(_) => return Ok((Classification::Unknown, 0)),
        };

        let consumed = ALIAS_LOOKUP_MILLIGAS;
        if budget < consumed {
            return Err(TezosXRuntimeError::OutOfGas);
        }
        let address_hash = match contract {
            Contract::Implicit(pkh) => AddressHash::Implicit(pkh),
            Contract::Originated(kt1) => AddressHash::Kt1(kt1),
        };
        let context =
            TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;
        let origin = context.read_origin_for_address(host, &address_hash)?;
        Ok((Classification::from(origin), consumed))
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn get_balance(
        &self,
        _host: &mut impl StorageV1,
        _address: &[u8],
    ) -> Result<U256, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn string_from_address(&self, _address: &[u8]) -> Result<String, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }
}

impl TezosRuntime {
    pub fn new(chain_id: ChainId) -> Self {
        Self(chain_id)
    }

    pub fn add_balance(
        host: &mut impl StorageV1,
        pub_key_hash: &PublicKeyHash,
        amount: U256,
    ) -> Result<(), TezosXRuntimeError> {
        let mut info = get_tezos_account_info_or_init(host, pub_key_hash)?;
        info.balance = info
            .balance
            .checked_add(amount)
            .ok_or(TezosXRuntimeError::Custom("Balance overflow".to_string()))?;
        set_tezos_account_info(host, pub_key_hash, info)
    }

    // Used for debug while we don't have our own originated account implementation.
    pub fn get_originated_account_balance(
        host: &impl StorageV1,
        kt1: &ContractKt1Hash,
    ) -> Result<U256, TezosXRuntimeError> {
        let context =
            TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;
        let originated_account = context.originated_from_kt1(kt1)?;
        let balance = originated_account.balance(host)?;
        narith_to_u256(&balance)
    }
}

#[cfg(all(test, feature = "testing"))]
mod tests {
    use tezos_crypto_rs::hash::HashTrait;
    use tezos_ethereum::block::BlockConstants;

    use super::*;
    use tezos_tezlink::operation_result::OperationKind;

    trait ContentResultExt<M: OperationKind> {
        fn is_failed(&self) -> bool;
    }

    impl<M: OperationKind> ContentResultExt<M> for ContentResult<M> {
        fn is_failed(&self) -> bool {
            matches!(self, ContentResult::Failed(_))
        }
    }

    #[test]
    fn build_response_success_uses_frame_result() {
        let resp = build_response(Ok(()), 0, Some(b"collected".to_vec()));
        assert_eq!(resp.status(), StatusCode::OK);
        assert_eq!(resp.body(), b"collected");
        assert_eq!(
            resp.headers()
                .get(http::header::CONTENT_TYPE)
                .and_then(|v| v.to_str().ok()),
            Some("application/octet-stream")
        );
    }

    #[test]
    fn build_response_success_no_frame_result() {
        let resp = build_response(Ok(()), 0, None);
        assert_eq!(resp.status(), StatusCode::OK);
        assert!(resp.body().is_empty());
        assert_eq!(
            resp.headers()
                .get(http::header::CONTENT_TYPE)
                .and_then(|v| v.to_str().ok()),
            Some("application/octet-stream")
        );
    }

    #[test]
    fn build_response_success_empty_frame_result() {
        let resp = build_response(Ok(()), 0, Some(vec![]));
        assert_eq!(resp.status(), StatusCode::OK);
        assert!(resp.body().is_empty());
        assert_eq!(
            resp.headers()
                .get(http::header::CONTENT_TYPE)
                .and_then(|v| v.to_str().ok()),
            Some("application/octet-stream")
        );
    }

    #[test]
    fn build_response_error_discards_frame_result() {
        // On revert, the %collect_result payload must not surface.
        let resp = build_response(
            Err(TezosXRuntimeError::BadRequest("invalid URL".into())),
            0,
            Some(b"leaked".to_vec()),
        );
        assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
        assert!(String::from_utf8_lossy(resp.body()).contains("invalid URL"));
        assert!(
            !resp.body().windows(6).any(|w| w == b"leaked"),
            "frame_result bytes must not appear in error body"
        );
        assert!(
            resp.headers().get(http::header::CONTENT_TYPE).is_none(),
            "Content-Type must not be set on error responses"
        );
    }

    #[test]
    fn build_response_bad_request() {
        let resp = build_response(
            Err(TezosXRuntimeError::BadRequest("invalid URL".into())),
            0,
            None,
        );
        assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
        assert!(String::from_utf8_lossy(resp.body()).contains("invalid URL"));
    }

    #[test]
    fn build_response_not_found() {
        let resp = build_response(
            Err(TezosXRuntimeError::NotFound("KT1 not found".into())),
            0,
            None,
        );
        assert_eq!(resp.status(), StatusCode::NOT_FOUND);
        assert!(String::from_utf8_lossy(resp.body()).contains("KT1 not found"));
    }

    #[test]
    fn build_response_method_not_allowed() {
        let resp = build_response(
            Err(TezosXRuntimeError::MethodNotAllowed(
                "PUT not allowed".into(),
            )),
            0,
            None,
        );
        assert_eq!(resp.status(), StatusCode::METHOD_NOT_ALLOWED);
        assert!(String::from_utf8_lossy(resp.body()).contains("PUT not allowed"));
    }

    #[test]
    fn build_response_success_has_gas_consumed_header() {
        // consumed_milligas = 0 → reported as 0
        let resp = build_response(Ok(()), 0, None);
        assert_eq!(
            resp.headers()
                .get(X_TEZOS_GAS_CONSUMED)
                .and_then(|v| v.to_str().ok()),
            Some("0")
        );
    }

    #[test]
    fn build_response_gas_consumed_reports_milligas() {
        // 5000 milligas → reported as 5000
        let resp = build_response(Ok(()), 5000, None);
        assert_eq!(
            resp.headers()
                .get(X_TEZOS_GAS_CONSUMED)
                .and_then(|v| v.to_str().ok()),
            Some("5000")
        );
    }

    #[test]
    fn build_response_error_has_no_gas_consumed_header() {
        let resp =
            build_response(Err(TezosXRuntimeError::BadRequest("err".into())), 0, None);
        assert_eq!(
            resp.headers()
                .get(X_TEZOS_GAS_CONSUMED)
                .and_then(|v| v.to_str().ok()),
            Some("0")
        );
    }

    // --- finalize_response tests ---
    //
    // Four cases of `(execute_request result, dispatch slot take)`:
    // success+payload, error+payload, success+unbalanced, error+
    // unbalanced.  Pin the contract that `finalize_response` reconciles
    // them as serve's tail.

    // Happy path: execution succeeded and the dispatch slot holds a
    // payload — 200 with the payload as body.
    #[test]
    fn finalize_response_success_with_payload() {
        let resp = finalize_response(Ok(()), 42, Ok(Some(b"collected".to_vec())));
        assert_eq!(resp.status(), StatusCode::OK);
        assert_eq!(resp.body(), b"collected");
        assert_eq!(
            resp.headers()
                .get(http::header::CONTENT_TYPE)
                .and_then(|v| v.to_str().ok()),
            Some("application/octet-stream")
        );
    }

    // Execute_request returned an error while the dispatch slot still
    // holds a payload: the request error wins, the payload is dropped,
    // and no Content-Type is set on the error body.
    #[test]
    fn finalize_response_error_discards_payload() {
        let resp = finalize_response(
            Err(TezosXRuntimeError::BadRequest("nope".into())),
            0,
            Ok(Some(b"leaked".to_vec())),
        );
        assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
        assert!(
            !resp.body().windows(6).any(|w| w == b"leaked"),
            "dispatch payload must not leak into error body"
        );
        assert!(
            resp.headers().get(http::header::CONTENT_TYPE).is_none(),
            "Content-Type must not be set on error responses"
        );
    }

    // Kernel bug: execute_request returned `Ok` but the dispatch slot
    // is missing — escalate to 500 rather than masking as an empty
    // 2xx response.
    #[test]
    fn finalize_response_success_with_unbalanced_slot_escalates_to_500() {
        let resp = finalize_response(Ok(()), 7, Err(DispatchSlotError::NoSlot));
        assert_eq!(resp.status(), StatusCode::INTERNAL_SERVER_ERROR);
        assert!(
            String::from_utf8_lossy(resp.body()).contains("dispatch slot inconsistency"),
            "body should explain the inconsistency: {:?}",
            String::from_utf8_lossy(resp.body())
        );
    }

    // Kernel bug coincident with a request error: the request error
    // takes precedence over the dispatch-slot inconsistency.  We don't
    // need to surface both; the user-visible failure is the request,
    // and the slot bug is a separate concern best caught by tests and
    // assertions elsewhere.
    #[test]
    fn finalize_response_error_with_unbalanced_slot_preserves_request_error() {
        let resp = finalize_response(
            Err(TezosXRuntimeError::NotFound("KT1 missing".into())),
            0,
            Err(DispatchSlotError::NoSlot),
        );
        assert_eq!(resp.status(), StatusCode::NOT_FOUND);
        assert!(
            String::from_utf8_lossy(resp.body()).contains("KT1 missing"),
            "original request error must dominate"
        );
    }

    // --- ensure_alias tests ---

    use tezos_crypto_rs::hash::ChainId;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::testing::NotWiredRegistry;
    use tezosx_interfaces::{AliasInfo, RuntimeId};

    fn test_context() -> CrossRuntimeContext {
        CrossRuntimeContext {
            gas_limit: 5_000_000,
            timestamp: U256::from(0),
            block_number: U256::from(0),
        }
    }

    fn test_runtime() -> TezosRuntime {
        TezosRuntime::new(ChainId::default())
    }

    fn evm_alias_info(addr: &str) -> AliasInfo {
        AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: addr.as_bytes().to_vec(),
        }
    }

    #[test]
    fn ensure_alias_returns_valid_kt1_string() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();

        let alias = runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info("0x1234567890abcdef1234567890abcdef12345678"),
                None,
                test_context(),
                5_000_000,
            )
            .expect("ensure_alias should succeed");

        // The alias should be a valid KT1 base58check string
        assert!(
            alias.0.starts_with("KT1"),
            "Alias should be a KT1 address: {}",
            alias.0
        );
    }

    #[test]
    fn ensure_alias_deploys_forwarder_code() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0x1234567890abcdef1234567890abcdef12345678";

        runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                5_000_000,
            )
            .expect("ensure_alias should succeed");

        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address.as_bytes()));
        let context =
            TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)
                .unwrap();
        let account = context.originated_from_kt1(&kt1).unwrap();

        let code = account.code(&host).unwrap();
        match code {
            tezos_execution::account_storage::Code::Code(bytes) => {
                assert_eq!(
                    bytes,
                    alias_forwarder::forwarder_code()
                        .expect("FORWARDER_CODE_HEX is a valid hex constant")
                );
            }
            _ => panic!("Expected regular code, not enshrined"),
        }
    }

    #[test]
    fn ensure_alias_stores_evm_address_in_storage() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef";

        runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                5_000_000,
            )
            .expect("ensure_alias should succeed");

        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address.as_bytes()));
        let context =
            TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)
                .unwrap();
        let account = context.originated_from_kt1(&kt1).unwrap();

        let storage = account.storage(&host).unwrap();
        let expected =
            alias_forwarder::forwarder_storage(evm_address, &mut Gas::default())
                .unwrap()
                .unwrap();
        assert_eq!(storage, expected);
    }

    #[test]
    fn ensure_alias_sets_zero_balance() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0xabcdef";

        runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                5_000_000,
            )
            .expect("ensure_alias should succeed");

        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address.as_bytes()));
        let balance = TezosRuntime::get_originated_account_balance(&host, &kt1)
            .expect("should read balance");
        assert_eq!(balance, U256::zero());
    }

    #[test]
    fn ensure_alias_is_deterministic() {
        let mut host1 = MockKernelHost::default();
        let mut host2 = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0x1111111111111111111111111111111111111111";

        let alias1 = runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host1,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                5_000_000,
            )
            .unwrap();
        let alias2 = runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host2,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                5_000_000,
            )
            .unwrap();

        assert_eq!(alias1.0, alias2.0);
    }

    #[test]
    fn ensure_alias_different_addresses_produce_different_aliases() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();

        let alias1 = runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info("0x1111111111111111111111111111111111111111"),
                None,
                test_context(),
                5_000_000,
            )
            .unwrap();
        let alias2 = runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info("0x2222222222222222222222222222222222222222"),
                None,
                test_context(),
                5_000_000,
            )
            .unwrap();

        assert_ne!(alias1.0, alias2.0);
    }

    #[test]
    fn ensure_alias_is_idempotent_no_op() {
        // First branch of the function. A second call with the same
        // input must return the same address with the gas budget
        // unchanged. The first call deploys; the second is just a
        // read of the classification path.
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0x3333333333333333333333333333333333333333";

        let first = runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                5_000_000,
            )
            .unwrap();

        let second = runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                500_000,
            )
            .unwrap();

        assert_eq!(first.0, second.0);
        // The no-op branch performs no durable writes, so its metered
        // cost is zero — only the first call pays for the storage init
        // (via the `originate_contract` receipt) and the classification
        // write.
        let first_consumed = 5_000_000 - first.1;
        let second_consumed = 500_000 - second.1;
        assert!(
            second_consumed < first_consumed / 10,
            "second call must be vastly cheaper than the first (first {first_consumed}, second {second_consumed})"
        );
    }

    #[test]
    fn ensure_alias_patch_only_branch_writes_classification() {
        // Branch 2: a forwarder is deployed but the classification
        // path is empty. The kernel writes the classification and
        // skips the redeploy. We simulate the legacy state by running
        // ensure_alias once, then deleting the origin path, then
        // running it again. The second call should restore the
        // classification without redeploying.
        use crate::account::{get_origin_at, ORIGIN_PATH};
        use tezos_smart_rollup_host::path::concat;

        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0x4444444444444444444444444444444444444444";

        runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                5_000_000,
            )
            .unwrap();

        // Locate the alias account and delete its origin path to
        // simulate a legacy account written before this work.
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address.as_bytes()));
        let context =
            TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)
                .unwrap();
        let account = context.originated_from_kt1(&kt1).unwrap();
        let origin_path = concat(account.path(), &ORIGIN_PATH).unwrap();
        host.store_delete(&origin_path).unwrap();
        assert!(get_origin_at(&host, &account.path().clone())
            .unwrap()
            .is_none());

        // Run again. The patch branch must re-record the classification.
        runtime
            .ensure_alias(
                &NotWiredRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                5_000_000,
            )
            .unwrap();

        match get_origin_at(&host, &account.path().clone()).unwrap() {
            Some(Origin::Alias(info)) => {
                assert_eq!(info.runtime, RuntimeId::Ethereum);
                assert_eq!(info.native_address, evm_address.as_bytes().to_vec());
            }
            other => panic!("expected Alias classification, got {other:?}"),
        }
    }

    #[test]
    fn ensure_alias_rejects_native_classification() {
        // The kernel must never reach an alias address that has been
        // classified as Native. If it does, the call returns an error
        // rather than overwriting the classification.
        use crate::account::set_origin_at;

        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0x5555555555555555555555555555555555555555";
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address.as_bytes()));
        let context =
            TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)
                .unwrap();
        let account = context.originated_from_kt1(&kt1).unwrap();

        set_origin_at(&mut host, &account.path().clone(), &Origin::Native).unwrap();

        let res = runtime.ensure_alias(
            &NotWiredRegistry,
            &mut host,
            &mut journal,
            evm_alias_info(evm_address),
            None,
            test_context(),
            5_000_000,
        );
        assert!(res.is_err());
    }

    // ── RFC Example 2: EVM → Michelson (incoming CRAC receipt) ──────────
    //
    // When a CRAC enters Michelson, the Michelson block shows:
    //
    //   Top-level:
    //     source: tz1<Handler_M>
    //     destination: KT1<alias(E_0)>
    //     Internal op #0:
    //       sender: KT1<alias(E_1)>
    //       destination: KT1<M_1>
    //       amount: 50 tez
    //       entrypoint: swap
    //
    // `build_crac_receipt` constructs this structure and serialises it.
    // This test verifies the round-trip and the key structural properties.

    #[test]
    fn build_crac_receipt_top_level_source_is_handler() {
        let null_pkh = PublicKeyHash::from_b58check(NULL_PKH).unwrap();
        // alias(E_0) — container destination of the top-level op
        let source_contract = Contract::Originated(
            ContractKt1Hash::from_b58check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
                .unwrap(),
        );
        // alias(E_1) — sender of the first internal op
        let sender_contract = Contract::Originated(
            ContractKt1Hash::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2")
                .unwrap(),
        );
        // M_1 — target Michelson contract
        let destination = Contract::Originated(
            ContractKt1Hash::from_b58check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw")
                .unwrap(),
        );
        let amount = Narith(50_000_000u64.into()); // 50 tez in mutez
        let parameters = Parameters {
            entrypoint: mir::ast::Entrypoint::from_string_unchecked("swap".into()),
            value: mir::ast::micheline::Micheline::from(())
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
        };
        let target = TransferTarget::from(TransferSuccess::default());

        let crac_id = "1-0";
        let applied = build_crac_receipt(
            &null_pkh,
            &source_contract,
            &sender_contract,
            &amount,
            &destination,
            &parameters,
            target,
            vec![],
            vec![],
            Some(crac_id),
            0,
        )
        .expect("build_crac_receipt should succeed");

        let OperationDataAndMetadata::OperationWithMetadata(batch) =
            &applied.op_and_receipt;
        assert_eq!(batch.operations.len(), 1, "one top-level operation");

        let op = &batch.operations[0];

        // Top-level source must be the handler implicit account.
        let ManagerOperationContent::Transaction(ref mgr_op) = op.content else {
            panic!("expected Transaction content");
        };
        assert_eq!(
            mgr_op.source, null_pkh,
            "top-level source must be Handler_M"
        );

        // Top-level destination must be alias(E_0).
        assert_eq!(
            mgr_op.operation.destination, source_contract,
            "top-level destination must be alias(E_0)"
        );

        // Check internal operations
        let OperationResultSum::Transfer(ref result) = op.receipt else {
            panic!("expected Transfer receipt");
        };
        // Internal op #0 = CRAC event, internal op #1 = transfer
        assert_eq!(
            result.internal_operation_results.len(),
            2,
            "two internal operations (CRAC event + transfer)"
        );

        // Internal op #0: CRAC event
        let InternalOperationSum::Event(ref event) = result.internal_operation_results[0]
        else {
            panic!(
                "expected Event, got {:?}",
                result.internal_operation_results[0]
            );
        };
        assert_eq!(
            event.content.tag.as_ref().and_then(|e| e.as_str()),
            Some("crac"),
            "event tag must be 'crac'"
        );
        assert_eq!(
            event.sender,
            Contract::Implicit(null_pkh.clone()),
            "event sender must be the handler implicit account"
        );

        // Internal op #1: alias(E_1) → M_1
        let InternalOperationSum::Transfer(ref internal) =
            result.internal_operation_results[1]
        else {
            panic!("expected internal Transfer");
        };
        assert_eq!(
            internal.sender, sender_contract,
            "internal sender must be alias(E_1)"
        );
        assert_eq!(
            internal.content.destination, destination,
            "internal destination must be M_1"
        );
        assert_eq!(
            internal.content.amount, amount,
            "internal amount must match"
        );

        // Round-trip: serialize and read back
        use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
        let bytes = applied.op_and_receipt.to_bytes().expect("serialization");
        let (remaining, round_tripped) =
            OperationDataAndMetadata::nom_read(&bytes).expect("deserialization");
        assert!(remaining.is_empty(), "all bytes consumed");
        assert_eq!(
            applied.op_and_receipt, round_tripped,
            "round-trip must be identical"
        );
    }

    /// RFC Example 2 variant: when alias(E_0) == alias(E_1) (direct call,
    /// no intermediary), the container and caller are the same alias.
    #[test]
    fn build_crac_receipt_direct_caller_alias_equals_source() {
        let null_pkh = PublicKeyHash::from_b58check(NULL_PKH).unwrap();
        // E_0 calls gateway directly: alias(E_0) == alias(E_1)
        let alias = Contract::Originated(
            ContractKt1Hash::from_b58check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
                .unwrap(),
        );
        let destination = Contract::Originated(
            ContractKt1Hash::from_b58check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw")
                .unwrap(),
        );
        let amount = Narith(100_000_000u64.into());
        let parameters = Parameters {
            entrypoint: mir::ast::Entrypoint::default(),
            value: mir::ast::micheline::Micheline::from(())
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
        };
        let target = TransferTarget::from(TransferSuccess::default());

        let applied = build_crac_receipt(
            &null_pkh,
            &alias, // source_contract = alias(E_0)
            &alias, // sender_contract = alias(E_1) = alias(E_0) when direct call
            &amount,
            &destination,
            &parameters,
            target,
            vec![],
            vec![],
            None, // no CRAC-ID in this variant test
            0,
        )
        .expect("build_crac_receipt should succeed");

        let OperationDataAndMetadata::OperationWithMetadata(batch) =
            &applied.op_and_receipt;
        assert_eq!(batch.operations.len(), 1, "one top-level operation");

        let op = &batch.operations[0];
        let ManagerOperationContent::Transaction(ref mgr_op) = op.content else {
            panic!("expected Transaction content");
        };

        // Top-level source is the handler.
        assert_eq!(mgr_op.source, null_pkh);
        // Top-level destination is alias(E_0).
        assert_eq!(
            mgr_op.operation.destination, alias,
            "top-level destination must be alias"
        );

        let OperationResultSum::Transfer(ref result) = op.receipt else {
            panic!("expected Transfer receipt");
        };
        // Only the synthetic transfer (no CRAC event since crac_id=None).
        assert_eq!(
            result.internal_operation_results.len(),
            1,
            "one internal operation (transfer only, no CRAC event)"
        );

        let InternalOperationSum::Transfer(ref internal) =
            result.internal_operation_results[0]
        else {
            panic!("expected internal Transfer");
        };
        // When E_0 == E_1, the internal sender equals the top-level destination.
        assert_eq!(
            internal.sender,
            Contract::Originated(
                ContractKt1Hash::from_b58check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
                    .unwrap()
            ),
            "internal sender must be alias (= top-level destination)"
        );
        assert_eq!(
            internal.content.destination, destination,
            "internal destination must be the target contract"
        );
        assert_eq!(
            internal.content.amount, amount,
            "internal amount must match"
        );
        assert!(
            matches!(internal.result, ContentResult::Applied(_)),
            "internal op must be applied"
        );
    }

    #[test]
    fn build_failed_crac_receipt_backtracks_event() {
        let null_pkh = PublicKeyHash::from_b58check(NULL_PKH).unwrap();
        let source_contract = Contract::Originated(
            ContractKt1Hash::from_b58check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
                .unwrap(),
        );
        let sender_contract = Contract::Originated(
            ContractKt1Hash::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2")
                .unwrap(),
        );
        let destination = Contract::Originated(
            ContractKt1Hash::from_b58check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw")
                .unwrap(),
        );
        let amount = Narith(50_000_000u64.into());
        let parameters = Parameters {
            entrypoint: mir::ast::Entrypoint::from_string_unchecked("swap".into()),
            value: mir::ast::micheline::Micheline::from(())
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
        };
        let error =
            TransferError::FailedToExecuteInternalOperation("test failure".into());

        let applied = build_failed_crac_receipt(
            &null_pkh,
            &source_contract,
            &sender_contract,
            &amount,
            &destination,
            &parameters,
            error,
            vec![],
            vec![],
            Some("1-0"),
            0,
        )
        .expect("build_failed_crac_receipt should succeed");

        let OperationDataAndMetadata::OperationWithMetadata(batch) =
            &applied.op_and_receipt;
        let op = &batch.operations[0];
        let OperationResultSum::Transfer(ref result) = op.receipt else {
            panic!("expected Transfer receipt");
        };

        // Top-level must be failed
        assert!(result.result.is_failed(), "top-level result must be failed");

        assert_eq!(
            result.internal_operation_results.len(),
            2,
            "two internal operations (CRAC event + failed transfer)"
        );

        // Internal op #0: CRAC event must be backtracked
        let InternalOperationSum::Event(ref event) = result.internal_operation_results[0]
        else {
            panic!("expected Event");
        };
        assert!(
            matches!(event.result, ContentResult::BackTracked(_)),
            "CRAC event must be backtracked when downstream transfer fails"
        );

        // Internal op #1: transfer must be failed
        let InternalOperationSum::Transfer(ref transfer) =
            result.internal_operation_results[1]
        else {
            panic!("expected Transfer");
        };
        assert!(transfer.result.is_failed(), "transfer must be failed");
    }

    // `serve` opens its own dispatch slot and only takes from that one;
    // an outer slot sitting on the stack must survive untouched.  This
    // test fails at URL parsing, so it does NOT exercise serve's
    // error-with-payload tail (covered separately by the
    // `finalize_response_*` tests above); the property it pins is the
    // slot-stack discipline.
    #[test]
    fn serve_does_not_touch_outer_dispatch_slot() {
        let mut host = MockKernelHost::default();
        let runtime = test_runtime();
        let registry = NotWiredRegistry;

        let mut journal = TezosXJournal::default();
        journal.michelson.push_dispatch_slot();
        journal
            .michelson
            .set_dispatch_result(b"TOP-SECRET".to_vec())
            .unwrap();

        // `http://evm/...` has the wrong authority for the Tezos runtime;
        // the URL parser maps it to `NotFound` (HTTP 404). This fails
        // before any account lookup, so no storage setup is needed.
        let request = http::Request::builder()
            .uri("http://evm/KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
            .body(vec![])
            .unwrap();
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);

        assert_eq!(resp.status(), StatusCode::NOT_FOUND);
        assert!(
            !resp.body().windows(10).any(|w| w == b"TOP-SECRET"),
            "seeded dispatch result must not appear in error body"
        );
        assert!(
            resp.headers().get(http::header::CONTENT_TYPE).is_none(),
            "Content-Type must not be set on error responses"
        );
        // The outer slot is untouched: `serve` opened its own slot and
        // took only that one.
        assert_eq!(
            journal.michelson.take_dispatch_result(),
            Ok(Some(b"TOP-SECRET".to_vec()))
        );
    }

    /// Regression test for L2-1464: a gas-tight failed CRAC must still
    /// record its failed receipt for indexers / call-graph
    /// reconstruction.
    ///
    /// Drives a real inbound EVM→Michelson CRAC (origin runtime =
    /// Ethereum, so the kernel emits the synthetic CRAC-ID event — the
    /// gas-charged step the bug hinged on) whose transfer targets a
    /// never-originated KT1. The transfer fails with the catchable
    /// operation-level error `ContractDoesNotExist`, and the gas limit
    /// is set below what encoding the synthetic CRAC event would cost.
    ///
    /// Before the fix, `build_failed_crac_receipt` encoded that event
    /// against the caller's (now near-empty) gas counter, hit
    /// `OutOfGas`, and the receipt was logged-and-dropped — leaving
    /// `failed_crac_receipts` empty while still returning a catchable
    /// `BadRequest` to the EVM caller. With the event encoding now
    /// sponsored against a dedicated kernel-owned counter, the receipt
    /// is built regardless of the caller's leftover gas, so it survives.
    /// Reverting the sponsoring fix makes this test fail (receipt count
    /// drops to 0).
    #[test]
    fn gas_tight_failed_crac_still_records_failed_receipt() {
        use crate::headers::{
            X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER, X_TEZOS_CRAC_ID, X_TEZOS_GAS_LIMIT,
            X_TEZOS_SENDER, X_TEZOS_SOURCE, X_TEZOS_TIMESTAMP,
        };
        use tezos_crypto_rs::hash::OperationHash;
        use tezosx_journal::CracId;

        // alias(E_0): EVM tx originator alias — top-level CRAC source.
        const SOURCE_KT1: &str = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT";
        // alias(E_1): immediate EVM caller alias — internal-op sender.
        const SENDER_KT1: &str = "KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2";
        // Never originated in this host → ContractDoesNotExist on transfer.
        const MISSING_DEST_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";

        let mut host = MockKernelHost::default();
        let runtime = test_runtime();
        let registry = NotWiredRegistry;

        // Ethereum-origin CRAC (origin_runtime != Tezos) so the kernel
        // emits the synthetic CRAC-ID event. The CRAC-Id header must
        // match the journal's id for `verify_crac_id` to pass.
        let crac_id = CracId::new(u8::from(RuntimeId::Ethereum), 0);
        let crac_id_str = crac_id.to_string();
        let mut journal = TezosXJournal::new(
            crac_id,
            OperationHash::default(),
            BlockConstants::dummy(),
        );

        // Non-empty body holding an encoded Unit: skips the empty-body
        // Unit fallback (which would itself draw on the tight gas) and
        // decodes cheaply, so essentially the whole budget is still
        // available for the would-be event encode after the transfer
        // fails.
        let body = mir::ast::micheline::Micheline::from(())
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        // Enough milligas to decode the Unit body and reach the
        // (storage-read-only, uncharged) destination-existence guard,
        // but below the cost of Micheline-encoding the synthetic CRAC
        // event — the window in which the unfixed builder ran out of gas
        // and dropped the receipt (the L2-1464 repro found the boundary
        // around 230 milligas).
        let request = http::Request::builder()
            .method(http::Method::POST)
            .uri(format!("http://tezos/{MISSING_DEST_KT1}"))
            .header(X_TEZOS_AMOUNT, "0")
            .header(X_TEZOS_GAS_LIMIT, "150")
            .header(X_TEZOS_TIMESTAMP, "1000000")
            .header(X_TEZOS_BLOCK_NUMBER, "1")
            .header(X_TEZOS_SENDER, SENDER_KT1)
            .header(X_TEZOS_SOURCE, SOURCE_KT1)
            .header(X_TEZOS_CRAC_ID, crac_id_str.as_str())
            .body(body)
            .unwrap();

        let resp = runtime.serve(&registry, &mut host, &mut journal, request);

        // The EVM caller still observes a catchable failure ...
        assert_eq!(
            resp.status(),
            StatusCode::BAD_REQUEST,
            "operation-level CRAC failure must stay a catchable 4xx, got {:?} ({})",
            resp.status(),
            String::from_utf8_lossy(resp.body())
        );

        // ... and crucially the failed CRAC receipt is preserved despite
        // the gas-tight builder (the property L2-1464 fixes).
        assert_eq!(
            journal.michelson.failed_crac_receipts.len(),
            1,
            "gas-tight failed CRAC must still record its failed receipt"
        );

        // The receipt is a Failed top-level whose first internal op is
        // the synthetic CRAC-ID event — i.e. the very thing whose
        // encoding was previously dropping the whole receipt.
        let (_seq, receipt) = &journal.michelson.failed_crac_receipts[0];
        let OperationDataAndMetadata::OperationWithMetadata(batch) =
            &receipt.op_and_receipt;
        let op = &batch.operations[0];
        let OperationResultSum::Transfer(ref result) = op.receipt else {
            panic!("expected Transfer receipt");
        };
        assert!(
            matches!(result.result, ContentResult::Failed(_)),
            "top-level result must be Failed"
        );
        let InternalOperationSum::Event(ref event) = result.internal_operation_results[0]
        else {
            panic!("expected synthetic CRAC event as first internal op");
        };
        assert_eq!(
            event.content.tag.as_ref().and_then(|e| e.as_str()),
            Some("crac"),
            "first internal op must be the synthetic CRAC-ID event"
        );
    }

    // ── TezosRuntime::read_origin tests ──────────────────────────────────

    mod read_origin_tests {
        use super::*;
        use tezos_crypto_rs::hash::ChainId;
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezosx_interfaces::{
            AliasInfo, Classification, Origin, RuntimeId, RuntimeInterface,
            ALIAS_LOOKUP_MILLIGAS,
        };

        use crate::{
            account::{set_origin_at, set_origin_for_implicit},
            TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
        };

        fn test_runtime() -> TezosRuntime {
            TezosRuntime::new(ChainId::default())
        }

        // (a) Storage hit on implicit address (tz1) → Native, charges ALIAS_LOOKUP_MILLIGAS
        #[test]
        fn read_origin_implicit_native_returns_native() {
            use tezos_crypto_rs::public_key_hash::PublicKeyHash;

            let mut host = MockKernelHost::default();
            let runtime = test_runtime();
            let pkh =
                PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                    .unwrap();
            set_origin_for_implicit(&mut host, &pkh, &Origin::Native).unwrap();

            let budget = 1_000_000;
            let (class, consumed) = runtime
                .read_origin(&host, "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx", budget)
                .unwrap();
            assert_eq!(class, Classification::Native);
            assert_eq!(consumed, ALIAS_LOOKUP_MILLIGAS);
        }

        // (b) Storage hit with Alias classification
        #[test]
        fn read_origin_alias_returns_alias() {
            use tezos_crypto_rs::public_key_hash::PublicKeyHash;

            let mut host = MockKernelHost::default();
            let runtime = test_runtime();
            let pkh =
                PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                    .unwrap();
            let alias_info = AliasInfo {
                runtime: RuntimeId::Ethereum,
                native_address: b"0xdeadbeef".to_vec(),
            };
            set_origin_for_implicit(&mut host, &pkh, &Origin::Alias(alias_info.clone()))
                .unwrap();

            let budget = 1_000_000;
            let (class, consumed) = runtime
                .read_origin(&host, "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx", budget)
                .unwrap();
            assert_eq!(class, Classification::Alias(alias_info));
            assert_eq!(consumed, ALIAS_LOOKUP_MILLIGAS);
        }

        // (c) Storage miss → Unknown, charges ALIAS_LOOKUP_MILLIGAS (no back-stop in Tezos)
        #[test]
        fn read_origin_unrecorded_address_returns_unknown() {
            let host = MockKernelHost::default();
            let runtime = test_runtime();

            let budget = 1_000_000;
            let (class, consumed) = runtime
                .read_origin(&host, "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx", budget)
                .unwrap();
            assert_eq!(class, Classification::Unknown);
            assert_eq!(consumed, ALIAS_LOOKUP_MILLIGAS); // no back-stop on Tezos
        }

        // (d) Malformed address → Unknown, no charge
        #[test]
        fn read_origin_malformed_address_returns_unknown_no_charge() {
            let host = MockKernelHost::default();
            let runtime = test_runtime();

            let budget = 1_000_000;
            let (class, consumed) = runtime
                .read_origin(&host, "not-a-tezos-address", budget)
                .unwrap();
            assert_eq!(class, Classification::Unknown);
            assert_eq!(consumed, 0); // malformed → no charge
        }

        // (e) KT1 address with recorded classification
        #[test]
        fn read_origin_kt1_address_returns_native() {
            use tezos_crypto_rs::hash::ContractKt1Hash;

            let mut host = MockKernelHost::default();
            let runtime = test_runtime();

            let context = crate::context::TezosRuntimeContext::from_root(
                &TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
            )
            .unwrap();
            // Use a real-looking KT1 derived from blake2b to avoid parse errors.
            let kt1 = ContractKt1Hash::from(tezos_crypto_rs::blake2b::digest_160(
                b"test_kt1_seed",
            ));
            let kt1_b58 = kt1.to_base58_check();
            let account = context.originated_from_kt1(&kt1).unwrap();
            let account_path = account.path().clone();
            set_origin_at(&mut host, &account_path, &Origin::Native).unwrap();

            let budget = 1_000_000;
            let (class, consumed) = runtime.read_origin(&host, &kt1_b58, budget).unwrap();
            assert_eq!(class, Classification::Native);
            assert_eq!(consumed, ALIAS_LOOKUP_MILLIGAS);
        }
    }
}
