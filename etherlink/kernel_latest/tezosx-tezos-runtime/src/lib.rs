// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use http::StatusCode;
use mir::ast::big_map::BigMapId;
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
    account_storage::{StorageSpace, TezlinkAccount, TezosOriginatedAccount},
    context::Context,
    cross_runtime_transfer,
    enshrined_contracts::CracError,
    mir_ctx::{OperationCtx, TcCtx},
    CracTransferError, OriginationNonce, TezlinkOperationGas,
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
    operation::{ManagerOperation, ManagerOperationContent, Parameters, TransferContent},
    operation_result::{
        ApplyOperationError, ApplyOperationErrors, BacktrackedResult, ContentResult,
        EventContent, EventSuccess, InternalContentWithMetadata, InternalOperationSum,
        OperationBatchWithMetadata, OperationDataAndMetadata, OperationResult,
        OperationResultSum, OperationWithMetadata, TransferError, TransferSuccess,
        TransferTarget,
    },
};
use tezosx_interfaces::{
    AliasInfo, CrossRuntimeContext, Origin, Registry, RuntimeId, RuntimeInterface,
    TezosXRuntimeError, X_TEZOS_GAS_CONSUMED,
};
use tezosx_journal::TezosXJournal;

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
/// `frame_result` carries the payload deposited on the topmost CRAC
/// frame — by Michelson via `%collect_result` for entrypoint calls, or
/// by [`view::execute_view_call`] for view calls. Peeked by the caller
/// before REVM commits or reverts the frame. On 4xx/5xx the frame is
/// about to be reverted, so the payload is discarded.
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
    internal_receipts: Vec<InternalOperationSum>,
    crac_id: Option<&str>,
    base_nonce: u16,
) -> Result<AppliedOperation, TezosXRuntimeError> {
    // Combine: [CRAC event, alias(E_1)→target, ...further internal ops]
    // Per RFC, the CRAC event is always the first internal operation (#0).
    // Nonces are pre-allocated from the block-global counter (L1 semantics):
    //   event = base_nonce, transfer = base_nonce+1 (or base_nonce if no event).
    // MIR internal ops already have correct nonces assigned during execution.
    let mut all_internal = Vec::new();
    let mut next_nonce = base_nonce;

    if let Some(id) = crac_id {
        use mir::{
            ast::annotations::NO_ANNS, ast::micheline::Micheline, ast::Entrypoint, lexer,
        };
        let ty = Micheline::App(lexer::Prim::string, &[], NO_ANNS).encode();
        let payload = Micheline::from(id.to_string()).encode();
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
    internal_receipts: Vec<InternalOperationSum>,
    crac_id: Option<&str>,
    base_nonce: u16,
) -> Result<AppliedOperation, TezosXRuntimeError> {
    // Per RFC, the CRAC event is always first, even on failure.
    // Since the downstream transfer failed, the event is backtracked
    // (matching Tezos protocol semantics where all applied internal ops
    // preceding a failure are backtracked).
    // Nonces are pre-allocated from the block-global counter (L1 semantics).
    let mut all_internal = Vec::new();
    let mut next_nonce = base_nonce;
    if let Some(id) = crac_id {
        use mir::{
            ast::annotations::NO_ANNS, ast::micheline::Micheline, ast::Entrypoint, lexer,
        };
        let ty = Micheline::App(lexer::Prim::string, &[], NO_ANNS).encode();
        let payload = Micheline::from(id.to_string()).encode();
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
            // the topmost frame's `frame_result` slot (same slot the
            // entrypoint path fills via `%collect_result`), so the
            // journal is threaded through.
            view::execute_view_call(
                chain_id,
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
    let parsed = url::parse_tezos_url(request.uri())?;
    let hdrs = headers::parse_request_headers(request.headers())?;

    let body = request.into_body();
    // An empty body means "no parameters" which defaults to Micheline Unit.
    // This is required for implicit account transfers where the Michelson VM
    // checks that param == Unit.
    let value = if body.is_empty() {
        mir::ast::micheline::Micheline::from(()).encode()
    } else {
        body
    };
    let parameters = Parameters {
        entrypoint: parsed.entrypoint,
        value,
    };

    let context =
        TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;

    let sender_account = context.originated_from_kt1(&hdrs.sender).map_err(|e| {
        TezosXRuntimeError::Custom(format!("Failed to fetch sender account: {e:?}"))
    })?;

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

    let mut gas = TezlinkOperationGas::start_milligas(hdrs.gas_limit)
        .map_err(|e| TezosXRuntimeError::Custom(format!("Failed to start gas: {e:?}")))?;
    let mut next_temp_id = BigMapId {
        value: Zarith(0.into()),
    };
    let mut tc_ctx = TcCtx {
        host,
        context: &context,
        operation_gas: &mut gas,
        big_map_diff: BTreeMap::new(),
        next_temporary_id: &mut next_temp_id,
    };
    let mut nonce = OriginationNonce::initial(OperationHash::default());
    let mut counter = 0u128;
    let mut operation_ctx = OperationCtx {
        source: &source_account,
        origination_nonce: &mut nonce,
        counter: &mut counter,
        level: &hdrs.block_number,
        now: &hdrs.timestamp,
        chain_id,
        source_public_key: &[],
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
    let result = match cross_runtime_transfer(
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
    ) {
        Ok(r) => r,
        Err(CracTransferError {
            error,
            internal_receipts,
        }) => {
            // Report consumed gas even on failure so the caller doesn't
            // see u64::MAX (which would exhaust the EVM gas budget and
            // prevent try/catch from working).
            *consumed_milligas = gas.total_milligas_consumed().into();

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
                CracError::Operation(TransferError::OutOfGas) => {
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
                if let (Some(kt1), CracError::Operation(te)) =
                    (hdrs.crac_origin_contract.as_ref(), error)
                {
                    let source_contract = Contract::Originated(kt1.clone());
                    match build_failed_crac_receipt(
                        &source_pkh,
                        &source_contract,
                        &sender_account.contract(),
                        &hdrs.amount,
                        &parsed.destination,
                        &parameters,
                        te,
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
            return Err(rt_err);
        }
    };

    *consumed_milligas = gas.total_milligas_consumed().into();

    // For cross-runtime calls (CRAC), build the two-step receipt
    // structure per RFC: CRAC Derived Block Contents and store it
    // in the journal for the block builder.
    //
    // source_contract = alias(E_0) from X-Tezos-Source (top-level destination)
    // sender_account.contract() = alias(E_1) from X-Tezos-Sender (internal sender)
    if is_crac {
        let source_contract = Contract::Originated(
            hdrs.crac_origin_contract
                .as_ref()
                .ok_or_else(|| {
                    TezosXRuntimeError::Custom(
                        "is_crac set but crac_origin_contract is None".into(),
                    )
                })?
                .clone(),
        );
        let receipt = build_crac_receipt(
            &source_pkh,
            &source_contract,
            &sender_account.contract(),
            &hdrs.amount,
            &parsed.destination,
            &parameters,
            result.target,
            result.internal_receipts,
            crac_id_for_event.as_deref(),
            base_nonce,
        )?;
        journal.michelson.push_pending_crac_receipt(receipt);
        Ok(TransferSuccess::default())
    } else {
        Ok(result.target.into())
    }
}

// --- Alias generation gas constants (milligas) ---
//
// Alias generation runs inside an existing CRAC frame: the outer
// transaction has already paid the manager-operation envelope (e.g.
// EVM's 21,000-gas baseline), and the forwarder script is
// kernel-supplied bytes written verbatim — no MIR decode or typecheck
// is invoked. So the cost here only accounts for the BLAKE2b digest
// and the durable writes performed by `account.init` and
// `set_balance`. The origination storage-burn is handled separately.

/// BLAKE2b-160 of the native address. Derived from the Tezos protocol
/// schedule: `430 + (size/8 + size)` milligas.
const ALIAS_BLAKE2B_DIGEST_MILLIGAS: u64 = 477;

/// Fixed overhead for a single durable-store write (independent of
/// payload size): path resolution, PVM host call boundary, journal
/// bookkeeping.
///
/// TODO(L2-1165): replace with a benchmarked value from `octez-snoop`.
const DURABLE_WRITE_BASE_MILLIGAS: u64 = 2_000;

/// Per-byte cost for a durable-store write. `account.init` performs
/// four writes (code, storage, paid_bytes, used_bytes); `set_balance`
/// adds a fifth. This scales each of them with its payload size rather
/// than charging a flat constant.
///
/// TODO(L2-1165): replace with a benchmarked value from `octez-snoop`.
const DURABLE_WRITE_PER_BYTE_MILLIGAS: u64 = 400;

/// Approximate size of the serialized `Narith` balance (`0u64` →
/// 1 byte of payload plus encoding overhead).
const BALANCE_WRITE_BYTES: u64 = 8;

impl RuntimeInterface for TezosRuntime {
    fn ensure_alias<Host>(
        &self,
        _registry: &impl Registry,
        host: &mut Host,
        _journal: &mut TezosXJournal,
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
        let mut remaining = gas_remaining;
        let mut consume = |cost: u64| -> Result<(), TezosXRuntimeError> {
            remaining = remaining.checked_sub(cost).ok_or_else(|| {
                TezosXRuntimeError::Custom(
                    "Out of gas during alias generation".to_string(),
                )
            })?;
            Ok(())
        };

        // Step 1: derive the deterministic alias address. The hash is
        // unconditional because the kernel needs the address regardless
        // of which branch runs next.
        consume(ALIAS_BLAKE2B_DIGEST_MILLIGAS)?;
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
            consume(DURABLE_WRITE_BASE_MILLIGAS)?;
            let new_origin = Origin::Alias(alias_info);
            set_origin_at(host, &account_path, &new_origin)?;
            return Ok((kt1_str, remaining));
        }

        // Branch 3: full materialization. Deploy the forwarder, set
        // the balance, and record the classification at the end.
        let code = alias_forwarder::forwarder_code().map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "Failed to decode forwarder code from hex: {e}"
            ))
        })?;
        let storage = alias_forwarder::forwarder_storage(native_address);
        let payload_bytes = (code.len() as u64).saturating_add(storage.len() as u64);

        // Durable writes performed by `account.init`: code, storage,
        // paid_bytes, used_bytes. Charge a per-write base plus a
        // per-byte component sized by the actual payload.
        consume(
            DURABLE_WRITE_BASE_MILLIGAS
                .saturating_mul(4)
                .saturating_add(
                    DURABLE_WRITE_PER_BYTE_MILLIGAS.saturating_mul(payload_bytes),
                ),
        )?;
        // TODO(L2-1294): charge alias-forwarder origination at the CRAC
        // boundary; for now `StorageSpace.allocated_bytes` is dropped —
        // the alias-forwarder origination is silently free.
        let _: StorageSpace = account.init(host, &code, &storage).map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "Failed to initialize alias forwarder contract: {e}"
            ))
        })?;

        // Balance write (5th durable write of the origination).
        consume(DURABLE_WRITE_BASE_MILLIGAS.saturating_add(
            DURABLE_WRITE_PER_BYTE_MILLIGAS.saturating_mul(BALANCE_WRITE_BYTES),
        ))?;
        account.set_balance(host, &0u64.into()).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to set alias balance: {e}"))
        })?;

        // Origin classification write. Same persistence model as the
        // surrounding deploy: both go to durable storage, both survive
        // a revert in the surrounding frame, and the next call to
        // ensure_alias finds the no-op branch.
        consume(DURABLE_WRITE_BASE_MILLIGAS)?;
        let new_origin = Origin::Alias(alias_info);
        set_origin_at(host, &account_path, &new_origin)?;

        Ok((kt1_str, remaining))
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
        // Peek the topmost frame's %collect_result payload before REVM
        // unwinds the checkpoint on commit/revert. `build_response` only
        // surfaces it on 2xx; on 4xx/5xx it is discarded.
        let frame_result = journal.michelson.frame_result().map(|b| b.to_vec());
        build_response(result, consumed_milligas, frame_result)
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

#[cfg(test)]
mod tests {
    use tezos_crypto_rs::hash::HashTrait;

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

    // --- ensure_alias tests ---

    use tezos_crypto_rs::hash::ChainId;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::{AliasInfo, RuntimeId};

    struct StubRegistry;

    impl Registry for StubRegistry {
        fn ensure_alias<Host>(
            &self,
            _host: &mut Host,
            _journal: &mut TezosXJournal,
            _alias_info: AliasInfo,
            _native_public_key: Option<&[u8]>,
            target_runtime: RuntimeId,
            _context: CrossRuntimeContext,
            _gas_remaining: u64,
        ) -> Result<(String, u64), TezosXRuntimeError>
        where
            Host: StorageV1,
        {
            Err(TezosXRuntimeError::RuntimeNotFound(target_runtime))
        }

        fn address_from_string(
            &self,
            _address_str: &str,
            runtime_id: RuntimeId,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            Err(TezosXRuntimeError::RuntimeNotFound(runtime_id))
        }

        fn serve<Host>(
            &self,
            _host: &mut Host,
            _journal: &mut TezosXJournal,
            _request: http::Request<Vec<u8>>,
        ) -> http::Response<Vec<u8>>
        where
            Host: StorageV1,
        {
            http::Response::builder()
                .status(http::StatusCode::INTERNAL_SERVER_ERROR)
                .body(b"stub".to_vec())
                .expect("stub response must build")
        }
    }

    fn test_context() -> CrossRuntimeContext {
        CrossRuntimeContext {
            gas_limit: 1_000_000,
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
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info("0x1234567890abcdef1234567890abcdef12345678"),
                None,
                test_context(),
                1_000_000,
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
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                1_000_000,
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
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                1_000_000,
            )
            .expect("ensure_alias should succeed");

        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address.as_bytes()));
        let context =
            TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)
                .unwrap();
        let account = context.originated_from_kt1(&kt1).unwrap();

        let storage = account.storage(&host).unwrap();
        let expected = alias_forwarder::forwarder_storage(evm_address);
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
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                1_000_000,
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
                &StubRegistry,
                &mut host1,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                1_000_000,
            )
            .unwrap();
        let alias2 = runtime
            .ensure_alias(
                &StubRegistry,
                &mut host2,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                1_000_000,
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
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info("0x1111111111111111111111111111111111111111"),
                None,
                test_context(),
                1_000_000,
            )
            .unwrap();
        let alias2 = runtime
            .ensure_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info("0x2222222222222222222222222222222222222222"),
                None,
                test_context(),
                1_000_000,
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

        let (first_alias, first_remaining) = runtime
            .ensure_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                1_000_000,
            )
            .unwrap();

        let (second_alias, second_remaining) = runtime
            .ensure_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                500_000,
            )
            .unwrap();

        assert_eq!(first_alias, second_alias);
        // The no-op branch still pays for the BLAKE2b digest needed to
        // compute the alias address. Anything else (storage init,
        // balance write, classification write) is skipped.
        let first_consumed = 1_000_000 - first_remaining;
        let second_consumed = 500_000 - second_remaining;
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
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                1_000_000,
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
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_alias_info(evm_address),
                None,
                test_context(),
                1_000_000,
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
            &StubRegistry,
            &mut host,
            &mut journal,
            evm_alias_info(evm_address),
            None,
            test_context(),
            1_000_000,
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
            value: mir::ast::micheline::Micheline::from(()).encode(),
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
            event.content.tag.as_ref().map(|e| e.as_str()),
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
            value: mir::ast::micheline::Micheline::from(()).encode(),
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
            value: mir::ast::micheline::Micheline::from(()).encode(),
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

    // `serve` must peek `frame_result` before REVM unwinds, but
    // `build_response` must drop it on 4xx/5xx. This test threads both
    // through the full `serve` path: pre-seed the journal, issue a
    // request that parses but fails at URL-host validation, and verify
    // the seeded bytes never appear in the error body.
    #[test]
    fn serve_error_does_not_leak_frame_result() {
        let mut host = MockKernelHost::default();
        let runtime = test_runtime();
        let registry = StubRegistry;

        let mut journal = TezosXJournal::default();
        journal.michelson.push_external_checkpoint();
        journal
            .michelson
            .set_frame_result(b"TOP-SECRET".to_vec())
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
            "seeded frame_result must not appear in error body"
        );
        assert!(
            resp.headers().get(http::header::CONTENT_TYPE).is_none(),
            "Content-Type must not be set on error responses"
        );
    }
}
