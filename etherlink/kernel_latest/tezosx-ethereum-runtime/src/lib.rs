// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

mod headers;
mod url;

use alloy_primitives::{hex::FromHex, Address, Bytes, Keccak256, U256 as AlloyU256};
use alloy_primitives::{IntoLogData, Log};
use alloy_sol_types::{sol, SolCall};
use http::StatusCode;
use primitive_types::U256;
use revm::context::result::{EVMError, ExecutionResult, HaltReason, Output};
use revm::primitives::KECCAK_EMPTY;
use revm_etherlink::precompiles::constants::RUNTIME_GATEWAY_PRECOMPILE_ADDRESS;
use revm_etherlink::{
    precompiles::constants::{
        alias_forwarder_delegation_code_hash, ALIAS_FORWARDER_PRECOMPILE_ADDRESS,
        ALIAS_FORWARDER_SOL_CONTRACT, TEZOSX_CALLER_ADDRESS,
    },
    run_transaction,
    storage::{
        code::CodeStorage,
        version::read_evm_version,
        world_state_handler::{AccountInfo, AccountOrigin, StorageAccount},
    },
    EvmRunError, ExecutionOutcome, GasData, TransactionOrigin,
};
use tezos_ethereum::block::BlockConstants;
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::{
    AliasInfo, AliasResolution, Classification, CrossRuntimeContext, Origin, Registry,
    RuntimeInterface, TezosXRuntimeError, ALIAS_LOOKUP_COST, X_TEZOS_GAS_CONSUMED,
};
use tezosx_journal::TezosXJournal;

alloy_sol_types::sol! {
    function init_tezosx_alias(string nativeAddress, bytes nativePublicKey) external payable;
}

pub struct EthereumRuntime {
    chain_id: primitive_types::U256,
}

impl Default for EthereumRuntime {
    fn default() -> Self {
        Self::new(U256::from(1337))
    }
}

impl EthereumRuntime {
    pub fn new(chain_id: primitive_types::U256) -> Self {
        Self { chain_id }
    }

    /// Build the `BlockConstants` an inbound CRAC executes against. The
    /// block-level observables (`BASEFEE`, `GASLIMIT` — the block gas
    /// limit, *not* the forwarded call gas — `BLOBBASEFEE`, `PREVRANDAO`,
    /// coinbase, number, timestamp) are inherited from the originating
    /// runtime's block, which the cross-runtime journal carries from its
    /// creation; only `chain_id` is overridden with this runtime's own
    /// identifier. `_host`/`_context` are no longer consulted now that the
    /// block flows through the journal, but stay in the signature to keep
    /// the alias-materialization call path untouched.
    fn create_block_constants(
        &self,
        _host: &impl StorageV1,
        journal: &TezosXJournal,
        _context: &CrossRuntimeContext,
    ) -> BlockConstants {
        BlockConstants {
            chain_id: self.chain_id,
            ..journal.evm.outer_block().clone()
        }
    }

    /// Branch 3 of ensure_alias: ensure the forwarder precompile code and
    /// run init to set forwarder storage and forward any prior balance.
    /// The delegation and classification are staged by the caller.
    #[allow(clippy::too_many_arguments)]
    fn materialize_alias<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        alias: Address,
        native_address: &str,
        native_public_key: Option<&[u8]>,
        context: &CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        // Ensure the AliasForwarder precompile code is available
        // (it should be initialized at kernel startup, but verify it exists)
        let precompile_account =
            StorageAccount::from_address(&ALIAS_FORWARDER_PRECOMPILE_ADDRESS)?;
        let precompile_info = precompile_account.info(host)?;
        if precompile_info.code_hash != ALIAS_FORWARDER_SOL_CONTRACT.code_hash {
            // Initialize the precompile if not already done
            CodeStorage::add(
                host,
                ALIAS_FORWARDER_SOL_CONTRACT.code,
                Some(ALIAS_FORWARDER_SOL_CONTRACT.code_hash),
            )
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to store AliasForwarder precompile code: {e}"
                ))
            })?;
        }

        // Call init_tezosx_alias on the alias address to set up storage.
        // The native address is passed as a string (e.g., "tz1...").
        let call_data = init_tezosx_aliasCall {
            nativeAddress: native_address.to_string(),
            nativePublicKey: native_public_key.unwrap_or_default().to_vec().into(),
        }
        .abi_encode();

        // Set up block constants for EVM execution
        let evm_version = read_evm_version(host);
        let block_constants = self.create_block_constants(host, journal, context);

        // Use the caller's remaining gas so it controls the budget. The
        // `effective_gas_price` is 0, so REVM's pre-flight balance check on the
        // caller (`gas_limit * gas_price + value`) reduces to 0 and no funding
        // is needed for `TEZOSX_CALLER_ADDRESS`. Earlier kernels wrote
        // `U256::MAX` to durable storage here as a "safety" buffer, which
        // leaked as a visible huge balance on Blockscout — kept in
        // `TransactionOrigin::CrossRuntime { credit: ... }` for any non-zero
        // future credit, but here we don't credit anything.
        let gas_data = GasData::new(gas_remaining, 0, gas_remaining);

        // Stage the alias delegation for this init transaction so the call
        // below resolves it and the write commits or reverts with the tx.
        journal.evm.set_pending_alias_delegation(alias);

        // Run the EVM transaction to call init_tezosx_alias
        let outcome = run_transaction(
            host,
            registry,
            journal,
            evm_version.into(),
            &block_constants,
            None, // no transaction hash for internal transactions
            TEZOSX_CALLER_ADDRESS,
            Some(alias), // call to the alias address
            Bytes::from(call_data),
            gas_data,
            AlloyU256::ZERO, // no value transfer
            None,            // no authorization list
            None,            // no tracer
            false,           // not a simulation
            TransactionOrigin::CrossRuntime { credit: None },
        )
        .map_err(|e| classify_evm_run_error("EVM execution failed", e))?;

        // Check that the call succeeded; return remaining EVM gas.
        let gas_used = outcome.result.gas_used();
        let remaining_after = gas_remaining.saturating_sub(gas_used);
        match outcome.result {
            // The classification was staged by the caller; the forwarder
            // storage lives in the EVM journal. Both flush at commit and
            // drop together on revert.
            ExecutionResult::Success { .. } => Ok((alias.to_string(), remaining_after)),
            ExecutionResult::Revert { output, .. } => Err(TezosXRuntimeError::Custom(
                format!("init_tezosx_alias reverted: {output:?}"),
            )),
            ExecutionResult::Halt { reason, .. } => Err(TezosXRuntimeError::Custom(
                format!("init_tezosx_alias halted: {reason:?}"),
            )),
        }
    }
}

/// Build an HTTP response from the result of [`execute_request`].
///
/// Execution results are mapped to HTTP status codes:
/// - `Success` → 200
/// - `Revert` → 400 (the contract rejected the call)
/// - `Halt(OutOfGas)` → 429 (OOG, special-cased so the gateway can
///   distinguish gas exhaustion from other deterministic halts)
/// - `Halt(other)` → 400 (every other halt is a property of the user-
///   supplied bytecode and tx inputs, so it must be a catchable
///   operation-level failure — see L2-1341)
///
/// Pre-execution errors are mapped as:
/// - `BadRequest` → 400
/// - `NotFound` → 404
///
/// All other errors propagate as `Err` — they indicate infrastructure
/// problems that cannot be meaningfully represented as HTTP responses.
fn build_response(
    result: Result<ExecutionOutcome, TezosXRuntimeError>,
) -> http::Response<Vec<u8>> {
    let (status, body, gas_consumed) = match result {
        Ok(outcome) => {
            // X-Tezos-Gas-Consumed is in the called runtime's units (EVM here).
            // The caller is responsible for converting to its own units.
            let gas = outcome.result.gas_used().to_string();
            match outcome.result {
                ExecutionResult::Success { output, .. } => {
                    let body = match output {
                        Output::Call(bytes) => bytes.to_vec(),
                        Output::Create(bytes, _) => bytes.to_vec(),
                    };
                    (StatusCode::OK, body, gas)
                }
                ExecutionResult::Revert { output, .. } => {
                    (StatusCode::BAD_REQUEST, output.to_vec(), gas)
                }
                ExecutionResult::Halt { reason, .. } => {
                    // Every `HaltReason` variant is a deterministic property
                    // of EVM execution (bytecode + tx inputs), so it MUST be
                    // surfaced as a catchable op-level failure (4xx). Any
                    // 5xx here would be classified as `CracError::BlockAbort`
                    // by the gateway and abort the whole block — a
                    // user-triggerable channel that lost adjacent
                    // delayed-bridge deposits in the forced inbox path
                    // (L2-1341). True infrastructure failures arrive via
                    // `Err(...)` from `run_transaction`, mapped to 5xx in
                    // the catch-all arm below — not through this halt
                    // branch.
                    //
                    // The match is intentionally exhaustive (no `_ =>`):
                    // a new REVM `HaltReason` variant must fail to compile
                    // so a human classifies it instead of silently
                    // defaulting to 500 again.
                    let status = match reason {
                        HaltReason::OutOfGas(_) => StatusCode::TOO_MANY_REQUESTS,
                        HaltReason::OpcodeNotFound
                        | HaltReason::InvalidFEOpcode
                        | HaltReason::InvalidJump
                        | HaltReason::NotActivated
                        | HaltReason::StackUnderflow
                        | HaltReason::StackOverflow
                        | HaltReason::OutOfOffset
                        | HaltReason::CreateCollision
                        | HaltReason::PrecompileError
                        | HaltReason::PrecompileErrorWithContext(_)
                        | HaltReason::NonceOverflow
                        | HaltReason::CreateContractSizeLimit
                        | HaltReason::CreateContractStartingWithEF
                        | HaltReason::CreateInitCodeSizeLimit
                        | HaltReason::OverflowPayment
                        | HaltReason::StateChangeDuringStaticCall
                        | HaltReason::CallNotAllowedInsideStatic
                        | HaltReason::OutOfFunds
                        | HaltReason::CallTooDeep => StatusCode::BAD_REQUEST,
                    };
                    (
                        status,
                        format!("EVM execution halted: {reason:?}").into_bytes(),
                        gas,
                    )
                }
            }
        }
        Err(TezosXRuntimeError::BadRequest(msg)) => {
            (StatusCode::BAD_REQUEST, msg.into_bytes(), "0".to_string())
        }
        Err(TezosXRuntimeError::NotFound(msg)) => {
            (StatusCode::NOT_FOUND, msg.into_bytes(), "0".to_string())
        }
        Err(TezosXRuntimeError::MethodNotAllowed(msg)) => (
            StatusCode::METHOD_NOT_ALLOWED,
            msg.into_bytes(),
            "0".to_string(),
        ),
        Err(TezosXRuntimeError::OutOfGas) => (
            StatusCode::TOO_MANY_REQUESTS,
            b"OOG".to_vec(),
            "0".to_string(),
        ),
        Err(e) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("{e:?}").into_bytes(),
            "0".to_string(),
        ),
    };
    // Safe to unwrap: status is a predefined constant, header name is a
    // static ASCII string, and the value is a decimal u64.
    http::Response::builder()
        .status(status)
        .header(X_TEZOS_GAS_CONSUMED, &gas_consumed)
        .body(body)
        .unwrap()
}

sol! {
    event CrossRuntimeCallReceived(
        string crossRuntimeCallId,
        string sourceRuntime,
        string senderAddress,
        string sourceAddress,
        string targetAddress,
        uint256 amount
    );
}

/// Dispatch a cross-runtime request on the HTTP method:
///
/// - `POST` → [`execute_call`] (state-mutating).
/// - `GET`  → [`execute_static_call`] (read-only, top frame runs
///   with `is_static = true` — REVM enforces strict `STATICCALL`).
/// - else  → catchable `405`.
///
/// `serve` then only handles the result-to-HTTP-status mapping.
fn execute_request<Host>(
    runtime: &EthereumRuntime,
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
) -> Result<ExecutionOutcome, TezosXRuntimeError>
where
    Host: StorageV1,
{
    match *request.method() {
        http::Method::POST => execute_call(runtime, registry, host, journal, request),
        http::Method::GET => {
            execute_static_call(runtime, registry, host, journal, request)
        }
        ref other => Err(TezosXRuntimeError::MethodNotAllowed(format!(
            "HTTP method {other} not allowed (use POST for entrypoint calls or GET for static calls)"
        ))),
    }
}

/// Map an [`EvmRunError`] returned by [`run_transaction`] onto a
/// [`TezosXRuntimeError`], separating user-triggerable pre-execution
/// validation failures from genuine infrastructure errors.
///
/// revm's `validate()` runs *before any frame executes* and returns
/// `EVMError::Transaction(_)` when the (attacker-shaped) transaction
/// inputs are themselves invalid — e.g. a forwarded `gas_limit` below the
/// intrinsic cost (`CallGasCostMoreThanGasLimit` /
/// `GasFloorMoreThanGasLimit`), a bad nonce, or insufficient balance.
/// *Every* `InvalidTransaction` variant is a deterministic property of
/// those inputs — never an infrastructure fault — so the whole
/// `Transaction(_)` arm maps to `BadRequest` → `400` →
/// `CracError::Operation`, a catchable op-level failure, exactly like a
/// `HaltReason` (see `build_response` and L2-1341). A `500` here would be
/// reclassified by the gateway as `CracError::BlockAbort` — a
/// user-triggerable block-production DoS (L2-1380).
///
/// Every other `EvmRunError` is infrastructure the user cannot shape and
/// keeps propagating as a `5xx`. Rather than flattening it all into
/// `Custom`, the storage/runtime variants reuse the typed `EvmDbError` /
/// `EvmKernelError` → `TezosXRuntimeError` translations (c840d70cfcc4) so a
/// storage fault stays tagged `Storage`/`Runtime`; only the genuinely
/// opaque `EVMError::Header`/`Custom` fall back to `Custom`. The match is
/// intentionally exhaustive (no `_`): a new `EvmRunError`/`EVMError`
/// variant must fail to compile so a human classifies it instead of
/// silently defaulting to a block-aborting `500` — mirroring the
/// `build_response` halt branch.
fn classify_evm_run_error(context: &str, e: EvmRunError) -> TezosXRuntimeError {
    let message = format!("{context}: {e:?}");
    match e {
        // User-shaped pre-execution validation failure → catchable `400`.
        EvmRunError::RevmDB(EVMError::Transaction(_)) => {
            TezosXRuntimeError::BadRequest(message)
        }
        // Storage / runtime faults: reuse the typed translations so they
        // keep their `Storage` / `Runtime` tag instead of becoming `Custom`.
        EvmRunError::DB(db) | EvmRunError::RevmDB(EVMError::Database(db)) => db.into(),
        EvmRunError::Kernel(kernel) => kernel.into(),
        // No richer typing available for these → keep them opaque.
        EvmRunError::RevmDB(EVMError::Header(_))
        | EvmRunError::RevmDB(EVMError::Custom(_)) => TezosXRuntimeError::Custom(message),
    }
}

/// Execute a state-mutating cross-runtime entrypoint call (POST).
///
/// Unchanged behavior from pre-L2-1259: emits `CrossRuntimeCallReceived`, credits the
/// sender, and runs the EVM transaction with state changes preserved
/// (the journal is not committed here — the outer block builder handles
/// that for the whole CRAC).
fn execute_call<Host>(
    runtime: &EthereumRuntime,
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
) -> Result<ExecutionOutcome, TezosXRuntimeError>
where
    Host: StorageV1,
{
    let parsed = url::parse_ethereum_url(request.uri())?;
    let hdrs = headers::parse_request_headers(request.headers())?;
    let call_data = Bytes::from(request.into_body());

    // Verify CRAC-ID from incoming header (debug/consistency check).
    if let Some(crac_id) = hdrs.crac_id {
        journal
            .verify_crac_id(&crac_id)
            .map_err(|e| TezosXRuntimeError::Custom(e.to_string()))?;
    }
    // Record this mutating crossing so a fake EVM transaction is built
    // to mirror the op in the EVM block. Only the invariant originator
    // is kept; this crossing's own sender / target / amount are recorded
    // in the `CrossRuntimeCallReceived` log emitted below, not aggregated into the
    // fake tx (see `CracTransactionInfo` and L2-1408).
    journal
        .evm
        .record_crac_crossing(hdrs.source.unwrap_or_default(), true);
    // `crac_chain_depth` is per-call: save the outer frame's value, set
    // this call's inbound depth for the duration of the inner EVM
    // execution, and restore it on return (below). Otherwise a
    // re-entrant outer frame (EVM → TEZ → EVM → …) would keep observing
    // the inner depth and stamp an inflated `X-Tezos-CRAC-Depth` on its
    // next outgoing CRAC. Mirrors the `revm_call_depth` save/restore in
    // `Journal::tezosx_call_http` (revm/src/journal.rs).
    let saved_crac_chain_depth = journal.evm.crac_chain_depth();
    journal.evm.set_crac_chain_depth(hdrs.crac_depth);

    let context = CrossRuntimeContext {
        gas_limit: hdrs.gas_limit,
        timestamp: hdrs.timestamp,
        block_number: hdrs.block_number,
    };

    let evm_version = read_evm_version(host);
    let block_constants = runtime.create_block_constants(host, journal, &context);
    let gas_data = GasData::new(hdrs.gas_limit, 0, hdrs.gas_limit);
    let crac_log = Log {
        address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
        data: CrossRuntimeCallReceived {
            crossRuntimeCallId: journal.crac_id().to_string(),
            // The native runtime of `sourceAddress` (forwarded as
            // `X-Tezos-Source-Runtime`), not the immediate sender's. On a
            // nested `EVM -> Michelson -> EVM` CRAC the source is the
            // transitive EVM origin, so this is `ethereum`; pairing it with
            // a hardcoded `tezos` produced an incoherent identity tuple for
            // event consumers.
            sourceRuntime: hdrs.source_runtime.as_host().to_string(),
            senderAddress: hdrs.sender.to_string(),
            sourceAddress: hdrs.source.unwrap_or_default().to_string(),
            targetAddress: parsed.destination.to_string(),
            amount: hdrs.amount,
        }
        .into_log_data(),
    };
    journal.evm.inner.log(crac_log);

    // `tx.origin` returns the inbound CRAC originator
    // (`X-Tezos-Source`) instead of `TxEnv.caller`, via a custom
    // `ORIGIN` opcode handler (see `etherlink_origin` in
    // `revm/src/lib.rs`). `TxEnv.caller` — and therefore `msg.sender`,
    // REVM's pre-execution nonce bump, and the value deduction — stays
    // on the immediate caller (`X-Tezos-Sender`) exactly as it did
    // before L2-1363, so the EOA-only guard is restored without any of
    // the side effects of overloading `TxEnv.caller`. An originator of
    // `None` falls back to the standard `TxEnv.caller` →
    // `tx.origin == msg.sender` semantics (a direct EOA call). See
    // L2-1363 / L2-1441.
    //
    // The originator is per-call: save the outer frame's value and
    // restore it on return (same shape as `crac_chain_depth` above).
    // Otherwise, in a nested EVM → Michelson → EVM scenario, the
    // inner serve would reset the outer frame's originator and the
    // outer EVM's post-bounce `ORIGIN` would fall back to
    // `TxEnv.caller`, observably flipping `tx.origin` mid-frame.
    let saved_cross_runtime_originator = journal.evm.cross_runtime_originator();
    journal.evm.set_cross_runtime_originator(hdrs.source);
    let outcome = run_transaction(
        host,
        registry,
        journal,
        evm_version.into(),
        &block_constants,
        None,
        hdrs.sender,
        Some(parsed.destination),
        call_data,
        gas_data,
        hdrs.amount,
        None,
        None,
        // Disable EIP-3607 (via `is_simulation = true`): an inbound CRAC
        // caller is not a user-signed EVM transaction but a trusted,
        // gateway-supplied address. A same-runtime EVM-to-EVM round-trip
        // forwards the real calling contract as `msg.sender`, whose
        // bytecode is neither empty nor an EIP-7702 delegation; EIP-3607
        // would otherwise reject it (RejectCallerWithCode), turning a
        // catchable revert into a 500 that aborts the whole CRAC block.
        // The static GET path disables it for the same reason.
        true,
        TransactionOrigin::CrossRuntime {
            credit: Some((hdrs.sender, hdrs.amount)),
        },
    );
    // Restore the outer frame's per-call depth and originator now that
    // the inner execution is done (see the saves above).
    journal.evm.set_crac_chain_depth(saved_crac_chain_depth);
    journal
        .evm
        .set_cross_runtime_originator(saved_cross_runtime_originator);
    let outcome =
        outcome.map_err(|e| classify_evm_run_error("EVM execution failed", e))?;

    Ok(outcome)
}

/// Read-only cross-runtime call (HTTP `GET`): the EVM-side entry
/// point for any originating runtime that wants to read EVM state
/// without leaving observable on-chain effects.
///
/// Differences from [`execute_call`]:
/// - rejects `X-Tezos-Amount != 0` (catchable `400`);
/// - no `CrossRuntimeCallReceived` log emission;
/// - runs the EVM transaction under [`TransactionOrigin::CrossRuntimeStatic`],
///   so the top-level frame has `is_static = true` and any state
///   mutation halts with `StateChangeDuringStaticCall` (surfaced as
///   `400`).
fn execute_static_call<Host>(
    runtime: &EthereumRuntime,
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
) -> Result<ExecutionOutcome, TezosXRuntimeError>
where
    Host: StorageV1,
{
    let parsed = url::parse_ethereum_url(request.uri())?;
    let hdrs = headers::parse_request_headers(request.headers())?;
    let call_data = Bytes::from(request.into_body());

    if !hdrs.amount.is_zero() {
        return Err(TezosXRuntimeError::BadRequest(
            "static calls (GET) cannot carry value (X-Tezos-Amount must be 0)".into(),
        ));
    }

    // Verify CRAC-ID from incoming header (debug/consistency check).
    if let Some(crac_id) = hdrs.crac_id {
        journal
            .verify_crac_id(&crac_id)
            .map_err(|e| TezosXRuntimeError::Custom(e.to_string()))?;
    }

    // Record this read-only crossing. It contributes no `has_mutating`,
    // so an op that only ever read EVM state produces no fake tx, and a
    // leading `staticcall_evm` can no longer latch the slot and poison a
    // later value-bearing `%call_evm` (L2-1408).
    journal
        .evm
        .record_crac_crossing(hdrs.source.unwrap_or_default(), false);
    // Per-call `crac_chain_depth`: save / set / restore around the inner
    // execution, same as the POST path. See `execute_call`.
    let saved_crac_chain_depth = journal.evm.crac_chain_depth();
    journal.evm.set_crac_chain_depth(hdrs.crac_depth);

    let context = CrossRuntimeContext {
        gas_limit: hdrs.gas_limit,
        timestamp: hdrs.timestamp,
        block_number: hdrs.block_number,
    };

    let evm_version = read_evm_version(host);
    let block_constants = runtime.create_block_constants(host, journal, &context);
    let gas_data = GasData::new(hdrs.gas_limit, 0, hdrs.gas_limit);

    // Intentionally no `CrossRuntimeCallReceived` log on the static path.

    // Same custom-`ORIGIN` mechanism as the state-mutating path: the
    // inbound originator drives the `ORIGIN` opcode (see
    // `etherlink_origin` in `revm/src/lib.rs`) while `TxEnv.caller`
    // stays the immediate caller. Save/restore around the inner
    // execution mirrors [`execute_call`]; see its comment for the
    // nested-frame rationale. See L2-1363 / L2-1441.
    let saved_cross_runtime_originator = journal.evm.cross_runtime_originator();
    journal.evm.set_cross_runtime_originator(hdrs.source);
    let outcome = run_transaction(
        host,
        registry,
        journal,
        evm_version.into(),
        &block_constants,
        None,
        hdrs.sender,
        Some(parsed.destination),
        call_data,
        gas_data,
        revm::primitives::U256::ZERO,
        None,
        None,
        // `is_simulation = true` disables EIP-3607 so a contract-aliased
        // caller (e.g. a Michelson contract's deterministic EVM alias,
        // which we expect to have code deployed for forwarder usage)
        // can sponsor a static read without being rejected at
        // validation time.
        true,
        TransactionOrigin::CrossRuntimeStatic,
    );
    // Restore the outer frame's per-call depth and originator (see
    // [`execute_call`] for the symmetric handling).
    journal.evm.set_crac_chain_depth(saved_crac_chain_depth);
    journal
        .evm
        .set_cross_runtime_originator(saved_cross_runtime_originator);
    let outcome =
        outcome.map_err(|e| classify_evm_run_error("EVM static execution failed", e))?;

    Ok(outcome)
}

impl RuntimeInterface for EthereumRuntime {
    fn ensure_alias<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        alias_info: AliasInfo,
        native_public_key: Option<&[u8]>,
        context: CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, AliasResolution), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        // The native address is stored in `alias_info` as the UTF-8
        // bytes of the canonical address string. Decode once for the
        // EVM init call below; the hash and the classification record
        // both work on the bytes directly.
        let native_address =
            std::str::from_utf8(&alias_info.native_address).map_err(|e| {
                TezosXRuntimeError::ConversionError(format!(
                    "alias_info.native_address is not valid UTF-8: {e}"
                ))
            })?;

        // Step 1: Compute the alias address deterministically from the native address
        let mut hasher = Keccak256::new();
        hasher.update(&alias_info.native_address);
        let hash = hasher.finalize();
        let alias = Address::from_slice(&hash[0..20]);

        // The delegation designator is the same constant for every alias;
        // the shared helper keeps Branch 2 detection and the installed
        // delegation in agreement.
        let delegation_code_hash = alias_forwarder_delegation_code_hash();

        let alias_account = StorageAccount::from_address(&alias)?;

        // Branch 1: already classified as alias, either staged earlier in
        // this transaction or persisted by a previous one. Returning early
        // preserves the gas budget and performs no writes.
        if journal
            .evm
            .layered_state
            .pending_alias_origin(&alias)
            .is_some()
        {
            return Ok((alias.to_string(), AliasResolution::build(gas_remaining)));
        }
        // One info read serves both the classification and the
        // Branch 2 forwarder detection (the code hash).
        let storage_info = alias_account.info(host)?;
        match storage_info.origin {
            AccountOrigin::Alias(_) => {
                return Ok((alias.to_string(), AliasResolution::build(gas_remaining)));
            }
            AccountOrigin::Native => {
                return Err(TezosXRuntimeError::Custom(format!(
                    "ensure_alias: address {alias} is recorded as Native, refusing to overwrite"
                )));
            }
            AccountOrigin::Unclassified => {}
        }

        // Branch 2: a forwarder is already deployed but the classification
        // path is empty. Stage the classification only and skip the
        // redeploy; the durable write is deferred to commit.
        if storage_info.code_hash == delegation_code_hash {
            journal
                .evm
                .layered_state
                .create_alias(alias, Origin::Alias(alias_info));
            return Ok((alias.to_string(), AliasResolution::build(gas_remaining)));
        }

        // Branch 3: full materialization. Stage the alias, then run init.
        // The staged alias reverts with its EVM frame and flushes only at
        // commit, so a failed parent leaves no durable state.
        journal
            .evm
            .layered_state
            .create_alias(alias, Origin::Alias(alias_info.clone()));

        let (alias_str, remaining_after) = self.materialize_alias(
            registry,
            host,
            journal,
            alias,
            native_address,
            native_public_key,
            &context,
            gas_remaining,
        )?;
        Ok((alias_str, AliasResolution::build(remaining_after)))
    }

    fn compute_alias(&self, native_address: &[u8]) -> Result<String, TezosXRuntimeError> {
        let mut hasher = Keccak256::new();
        hasher.update(native_address);
        let hash = hasher.finalize();
        let alias = Address::from_slice(&hash[0..20]);
        Ok(alias.to_string())
    }

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
        build_response(execute_request(self, registry, host, journal, request))
    }

    fn host(&self) -> &'static str {
        "ethereum"
    }

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        let address = Address::from_hex(address_str).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Invalid address string: {e}"))
        })?;
        Ok(address.0.to_vec())
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
        let address = match Address::from_hex(addr) {
            Ok(a) => a,
            Err(_) => return Ok((Classification::Unknown, 0)),
        };

        // Single durable read: the info record carries the whole
        // classification, alias payload included, and the code hash
        // for the back-stop — the charged cost reflects exactly what
        // happens.
        let consumed = ALIAS_LOOKUP_COST;
        if budget < consumed {
            return Err(TezosXRuntimeError::OutOfGas);
        }
        let account = StorageAccount::from_address(&address)?;
        let info = account.info_without_migration(host).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to read account info: {e}"))
        })?;
        let cls = match info {
            Some(AccountInfo {
                origin: AccountOrigin::Native,
                ..
            }) => Classification::Native,
            Some(AccountInfo {
                origin: AccountOrigin::Alias(alias_info),
                ..
            }) => Classification::from(Origin::Alias(alias_info)),
            // Back-stop for unclassified accounts: code presence
            // promotes to Native (CREATE contracts and EIP-7702
            // delegations deployed before classification existed).
            Some(AccountInfo {
                origin: AccountOrigin::Unclassified,
                code_hash,
                ..
            }) if code_hash != KECCAK_EMPTY => Classification::Native,
            Some(AccountInfo {
                origin: AccountOrigin::Unclassified,
                ..
            })
            | None => Classification::Unknown,
        };
        Ok((cls, consumed))
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn get_balance(
        &self,
        _host: &mut impl StorageV1,
        _address: &[u8],
    ) -> Result<primitive_types::U256, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn string_from_address(&self, _address: &[u8]) -> Result<String, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }
}

#[cfg(all(test, feature = "testing"))]
mod tests {
    use alloy_primitives::{hex::FromHex, Bytes, Keccak256};
    use revm::primitives::Address;
    use revm::state::Bytecode;
    use revm_etherlink::journal::commit_evm_journal_from_external;
    use revm_etherlink::{
        helpers::storage::bytes_hash,
        storage::{
            code::CodeStorage,
            world_state_handler::{AccountInfo, AccountOrigin, StorageAccount},
        },
    };
    use tezos_ethereum::block::BlockConstants;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::testing::UnimplementedRegistry;
    use tezosx_interfaces::RuntimeId;
    use tezosx_interfaces::{RuntimeInterface, TezosXRuntimeError};
    use tezosx_journal::TezosXJournal;

    use crate::EthereumRuntime;

    /// Build an HTTP request for the Ethereum runtime's `serve()` method.
    /// `block` is the block the CRAC is served against — its gas limit is
    /// forwarded as `X-Tezos-Gas-Limit`, so the synthetic transaction
    /// never claims more gas than the served block grants (mirroring
    /// production, where the forwarded gas is bounded by the caller's
    /// budget). Pass the same block the journal carries.
    fn build_serve_request(
        sender: &Address,
        destination: &Address,
        amount: &str,
        body: Vec<u8>,
        block: &BlockConstants,
    ) -> http::Request<Vec<u8>> {
        // POST is the explicit method for state-mutating cross-runtime
        // entrypoint calls. The HTTP default is GET, which since L2-1259
        // routes to the read-only static path; without this explicit
        // method, every existing transfer/invoke test would silently
        // hit the static path and either reject (amount != 0) or revert
        // its writes, producing confusing failures.
        let url = format!(
            "http://ethereum/{}",
            alloy_primitives::hex::encode(destination.0 .0)
        );
        http::Request::builder()
            .method(http::Method::POST)
            .uri(&url)
            .header(
                tezosx_interfaces::X_TEZOS_SENDER,
                format!("0x{}", alloy_primitives::hex::encode(sender.0 .0)),
            )
            .header(tezosx_interfaces::X_TEZOS_AMOUNT, amount)
            .header(
                tezosx_interfaces::X_TEZOS_GAS_LIMIT,
                block.gas_limit.to_string(),
            )
            .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "1")
            .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "1")
            .body(body)
            .unwrap()
    }

    #[test]
    fn test_serve_simple_transfer() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        // No need to fund the sender: serve() credits the sender with
        // the transfer amount (the calling runtime already debited it).
        let five_tez_wei = revm::primitives::U256::from(5_000_000_000_000_000_000u128);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request(
            &sender,
            &destination,
            "5",
            vec![],
            journal.evm.outer_block(),
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        // Verify destination received the transfer (5 TEZ in wei)
        let destination_account = StorageAccount::from_address(&destination).unwrap();
        let info = destination_account.info(&mut host).unwrap();
        assert_eq!(info.balance, five_tez_wei);
    }

    #[test]
    fn test_serve_executes_contract_bytecode() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let contract = Address::from_slice(&[0x22; 20]);

        // Deploy a tiny contract:
        //   PUSH1 0x42   (value to store)
        //   PUSH1 0x01   (storage slot)
        //   SSTORE       (store 0x42 at slot 1)
        //   PUSH1 0x01
        //   SLOAD        (load from slot 1)
        let bytecode_raw = Bytes::from_hex("6042600155600154").unwrap();
        let code_hash = bytes_hash(&bytecode_raw);
        let mut contract_account = StorageAccount::from_address(&contract).unwrap();
        contract_account
            .set_info(
                &mut host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    origin: AccountOrigin::Unclassified,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(&mut host, &bytecode_raw, Some(code_hash)).unwrap();

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request(
            &sender,
            &contract,
            "0",
            vec![],
            journal.evm.outer_block(),
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        // Verify the contract wrote 0x42 to storage slot 1
        let slot_value = contract_account
            .get_storage(&host, &revm::primitives::U256::from(1))
            .unwrap();
        assert_eq!(slot_value, revm::primitives::U256::from(0x42));
    }

    /// Test that serve() correctly passes value to an EVM contract via
    /// X-Tezos-Amount. The contract reads CALLVALUE and stores it.
    #[test]
    fn test_serve_with_value_sets_correct_msg_value() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;
        let block_constants = BlockConstants::test_block_with_no_fees();

        let sender = Address::from_slice(&[0x11; 20]);
        let contract = Address::from_slice(&[0x22; 20]);

        // No need to fund the sender: execute_request credits the sender
        // with the transfer amount (the calling runtime already debited it).

        // Deploy a contract that stores CALLVALUE to slot 0:
        //   CALLVALUE    (0x34)
        //   PUSH1 0x00   (0x6000)
        //   SSTORE       (0x55)
        let bytecode_raw = Bytes::from_hex("34600055").unwrap();
        let code_hash = bytes_hash(&bytecode_raw);
        let mut contract_account = StorageAccount::from_address(&contract).unwrap();
        contract_account
            .set_info(
                &mut host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    origin: AccountOrigin::Unclassified,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(&mut host, &bytecode_raw, Some(code_hash)).unwrap();

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request(
            &sender,
            &contract,
            "42",
            vec![],
            journal.evm.outer_block(),
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        // Contract stored CALLVALUE at slot 0 — verify it saw the real value
        // "42" TEZ = 42 * 10^18 wei
        let forty_two_tez_wei =
            revm::primitives::U256::from(42_000_000_000_000_000_000u128);
        let stored_value = contract_account
            .get_storage(&host, &revm::primitives::U256::ZERO)
            .unwrap();
        assert_eq!(
            stored_value, forty_two_tez_wei,
            "Contract should see msg.value = 42 TEZ in wei"
        );

        // Contract received the transfer
        let contract_info = contract_account.info(&mut host).unwrap();
        assert_eq!(
            contract_info.balance, forty_two_tez_wei,
            "Contract should hold the transferred value"
        );

        // Sender balance should be 0 after sending 42
        let sender_account = StorageAccount::from_address(&sender).unwrap();
        let sender_info = sender_account.info(&mut host).unwrap();
        assert_eq!(
            sender_info.balance,
            revm::primitives::U256::ZERO,
            "Sender balance should be 0 after transfer"
        );
    }

    /// Build a minimal cross-runtime `serve()` request carrying an
    /// inbound `X-Tezos-CRAC-Depth`. Empty body + amount 0 so the call
    /// is a no-op transfer to a codeless account on both the POST and
    /// GET (static) paths.
    fn serve_request_with_depth(
        method: http::Method,
        sender: &Address,
        destination: &Address,
        crac_depth: u32,
    ) -> http::Request<Vec<u8>> {
        let url = format!(
            "http://ethereum/{}",
            alloy_primitives::hex::encode(destination.0 .0)
        );
        http::Request::builder()
            .method(method)
            .uri(&url)
            .header(
                tezosx_interfaces::X_TEZOS_SENDER,
                format!("0x{}", alloy_primitives::hex::encode(sender.0 .0)),
            )
            .header(tezosx_interfaces::X_TEZOS_AMOUNT, "0")
            .header(tezosx_interfaces::X_TEZOS_GAS_LIMIT, u64::MAX.to_string())
            .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "1")
            .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "1")
            .header(
                tezosx_interfaces::X_TEZOS_CRAC_DEPTH,
                crac_depth.to_string(),
            )
            .body(vec![])
            .unwrap()
    }

    // Outer EVM frame already inside a CRAC at this depth; the inbound
    // serve carries a shallower depth. After the inbound execution
    // returns, the outer frame's per-call depth must be the outer value
    // again, not the inner one.
    const OUTER_DEPTH: u32 = 5;
    const INBOUND_DEPTH: u32 = 2;

    #[test]
    fn test_serve_post_restores_crac_chain_depth() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;
        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        journal.evm.set_crac_chain_depth(OUTER_DEPTH);

        let request = serve_request_with_depth(
            http::Method::POST,
            &sender,
            &destination,
            INBOUND_DEPTH,
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);

        // Without the save/restore the outer frame would keep the inner
        // INBOUND_DEPTH and stamp an inflated header on its next CRAC.
        assert_eq!(
            journal.evm.crac_chain_depth(),
            OUTER_DEPTH,
            "POST serve() must restore the outer frame's crac_chain_depth on return"
        );
    }

    #[test]
    fn test_serve_get_restores_crac_chain_depth() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;
        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        journal.evm.set_crac_chain_depth(OUTER_DEPTH);

        let request = serve_request_with_depth(
            http::Method::GET,
            &sender,
            &destination,
            INBOUND_DEPTH,
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);

        assert_eq!(
            journal.evm.crac_chain_depth(),
            OUTER_DEPTH,
            "GET serve() must restore the outer frame's crac_chain_depth on return"
        );
    }

    /// Sibling of [`test_serve_post_restores_crac_chain_depth`] for
    /// the originator. Without the save/restore, a nested
    /// `EVM → Michelson → EVM` would clear the outer frame's
    /// originator on the inner serve's return and the outer EVM's
    /// post-bounce `ORIGIN` would silently fall back to
    /// `TxEnv.caller`, flipping `tx.origin` mid-frame.
    #[test]
    fn test_serve_post_restores_cross_runtime_originator() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;
        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);
        let outer_originator = Address::from_slice(&[0x33; 20]);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        journal
            .evm
            .set_cross_runtime_originator(Some(outer_originator));

        let request = serve_request_with_depth(
            http::Method::POST,
            &sender,
            &destination,
            INBOUND_DEPTH,
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);

        assert_eq!(
            journal.evm.cross_runtime_originator(),
            Some(outer_originator),
            "POST serve() must restore the outer frame's cross_runtime_originator on return"
        );
    }

    #[test]
    fn test_serve_get_restores_cross_runtime_originator() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;
        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);
        let outer_originator = Address::from_slice(&[0x33; 20]);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        journal
            .evm
            .set_cross_runtime_originator(Some(outer_originator));

        let request = serve_request_with_depth(
            http::Method::GET,
            &sender,
            &destination,
            INBOUND_DEPTH,
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);

        assert_eq!(
            journal.evm.cross_runtime_originator(),
            Some(outer_originator),
            "GET serve() must restore the outer frame's cross_runtime_originator on return"
        );
    }

    #[test]
    fn test_serve_preserves_revm_call_depth_control() {
        // Control / sibling: `revm_call_depth` is written only by the
        // OUTBOUND `tezosx_call_http` (revm/src/journal.rs), never by an
        // inbound serve, so it is naturally preserved across an inbound
        // CRAC. This is the bracketed counter `crac_chain_depth` is now
        // aligned with; the assertion holds regardless of the
        // crac_chain_depth fix.
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;
        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        journal.evm.set_revm_call_depth(Some(7));

        let request = serve_request_with_depth(
            http::Method::POST,
            &sender,
            &destination,
            INBOUND_DEPTH,
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);

        assert_eq!(journal.evm.revm_call_depth(), Some(7));
    }

    /// Test that alias addresses are computed deterministically from native addresses.
    #[test]
    fn test_alias_address_is_deterministic() {
        let native_address = b"tz1abc123";

        // Compute alias address using the same algorithm as ensure_alias
        let mut hasher = Keccak256::new();
        hasher.update(native_address);
        let hash = hasher.finalize();
        let alias = &hash[0..20];

        // Verify it's 20 bytes
        assert_eq!(alias.len(), 20);

        // Compute again to verify determinism
        let mut hasher2 = Keccak256::new();
        hasher2.update(native_address);
        let hash2 = hasher2.finalize();
        let alias2 = &hash2[0..20];

        assert_eq!(alias, alias2);
    }

    #[test]
    fn test_different_native_addresses_produce_different_aliases() {
        let compute_alias = |native: &[u8]| {
            let mut hasher = Keccak256::new();
            hasher.update(native);
            let hash = hasher.finalize();
            hash[0..20].to_vec()
        };

        let alias1 = compute_alias(b"tz1abc");
        let alias2 = compute_alias(b"tz1xyz");

        assert_ne!(alias1, alias2);
    }

    mod build_response_tests {
        use super::*;
        use crate::build_response;
        use http::StatusCode;
        use revm::context::result::{
            ExecutionResult, HaltReason, Output, ResultGas, SuccessReason,
        };
        use revm_etherlink::ExecutionOutcome;
        use tezosx_interfaces::X_TEZOS_GAS_CONSUMED;

        fn make_success(output: Vec<u8>) -> ExecutionOutcome {
            ExecutionOutcome {
                result: ExecutionResult::Success {
                    reason: SuccessReason::Return,
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                    output: Output::Call(output.into()),
                    logs: vec![],
                },
                withdrawals: vec![],
            }
        }

        #[test]
        fn success_returns_200() {
            let resp = build_response(Ok(make_success(b"hello".to_vec())));
            assert_eq!(resp.status(), StatusCode::OK);
            assert_eq!(resp.body(), b"hello");
        }

        #[test]
        fn success_empty_body() {
            let resp = build_response(Ok(make_success(vec![])));
            assert_eq!(resp.status(), StatusCode::OK);
            assert!(resp.body().is_empty());
        }

        #[test]
        fn revert_returns_400() {
            let outcome = ExecutionOutcome {
                result: ExecutionResult::Revert {
                    logs: vec![],
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                    output: b"revert reason".to_vec().into(),
                },
                withdrawals: vec![],
            };
            let resp = build_response(Ok(outcome));
            assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
            assert_eq!(resp.body(), b"revert reason");
        }

        #[test]
        fn halt_out_of_gas_returns_429() {
            let outcome = ExecutionOutcome {
                result: ExecutionResult::Halt {
                    reason: HaltReason::OutOfGas(
                        revm::context::result::OutOfGasError::Basic,
                    ),
                    logs: vec![],
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                },
                withdrawals: vec![],
            };
            let resp = build_response(Ok(outcome));
            assert_eq!(resp.status(), StatusCode::TOO_MANY_REQUESTS);
        }

        /// Every non-OOG halt must surface as a catchable 400 (L2-1341).
        /// A 5xx here would be reclassified as `CracError::BlockAbort` by
        /// the gateway, aborting the whole block and (in the forced
        /// delayed-inbox path) dropping adjacent bridge deposits.
        #[test]
        fn user_triggerable_halts_return_400() {
            let user_triggerable = [
                HaltReason::OpcodeNotFound,
                HaltReason::InvalidFEOpcode,
                HaltReason::InvalidJump,
                HaltReason::NotActivated,
                HaltReason::StackUnderflow,
                HaltReason::StackOverflow,
                HaltReason::OutOfOffset,
                HaltReason::CreateCollision,
                HaltReason::PrecompileError,
                HaltReason::PrecompileErrorWithContext("ctx".into()),
                HaltReason::NonceOverflow,
                HaltReason::CreateContractSizeLimit,
                HaltReason::CreateContractStartingWithEF,
                HaltReason::CreateInitCodeSizeLimit,
                HaltReason::OverflowPayment,
                HaltReason::StateChangeDuringStaticCall,
                HaltReason::CallNotAllowedInsideStatic,
                HaltReason::OutOfFunds,
                HaltReason::CallTooDeep,
            ];
            for reason in user_triggerable {
                let outcome = ExecutionOutcome {
                    result: ExecutionResult::Halt {
                        reason: reason.clone(),
                        logs: vec![],
                        gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                    },
                    withdrawals: vec![],
                };
                let resp = build_response(Ok(outcome));
                assert_eq!(
                    resp.status(),
                    StatusCode::BAD_REQUEST,
                    "halt reason {reason:?} should map to 400, not {}",
                    resp.status()
                );
            }
        }

        #[test]
        fn bad_request_error_returns_400() {
            let resp =
                build_response(Err(TezosXRuntimeError::BadRequest("invalid URL".into())));
            assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
            assert!(String::from_utf8_lossy(resp.body()).contains("invalid URL"));
        }

        #[test]
        fn not_found_error_returns_404() {
            let resp = build_response(Err(TezosXRuntimeError::NotFound(
                "no such contract".into(),
            )));
            assert_eq!(resp.status(), StatusCode::NOT_FOUND);
            assert!(String::from_utf8_lossy(resp.body()).contains("no such contract"));
        }

        #[test]
        fn custom_error_returns_500() {
            let resp = build_response(Err(TezosXRuntimeError::Custom("boom".into())));
            assert_eq!(resp.status(), StatusCode::INTERNAL_SERVER_ERROR);
        }

        // Gas header tests: make_success uses ResultGas::new(u64::MAX, 21000, 0, 0, 0)
        // so gas_used() = 21000. X-Tezos-Gas-Consumed is in EVM units (the called runtime).

        #[test]
        fn success_has_gas_consumed_header() {
            let resp = build_response(Ok(make_success(vec![])));
            assert_eq!(
                resp.headers()
                    .get(X_TEZOS_GAS_CONSUMED)
                    .and_then(|v| v.to_str().ok()),
                Some("21000")
            );
        }

        #[test]
        fn revert_has_gas_consumed_header() {
            let outcome = ExecutionOutcome {
                result: ExecutionResult::Revert {
                    logs: vec![],
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                    output: vec![].into(),
                },
                withdrawals: vec![],
            };
            let resp = build_response(Ok(outcome));
            assert_eq!(
                resp.headers()
                    .get(X_TEZOS_GAS_CONSUMED)
                    .and_then(|v| v.to_str().ok()),
                Some("21000")
            );
        }

        #[test]
        fn halt_has_gas_consumed_header() {
            let outcome = ExecutionOutcome {
                result: ExecutionResult::Halt {
                    reason: HaltReason::OutOfGas(
                        revm::context::result::OutOfGasError::Basic,
                    ),
                    logs: vec![],
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                },
                withdrawals: vec![],
            };
            let resp = build_response(Ok(outcome));
            assert_eq!(
                resp.headers()
                    .get(X_TEZOS_GAS_CONSUMED)
                    .and_then(|v| v.to_str().ok()),
                Some("21000")
            );
        }

        #[test]
        fn error_response_has_zero_gas_consumed() {
            let resp = build_response(Err(TezosXRuntimeError::BadRequest("err".into())));
            assert_eq!(resp.headers().get(X_TEZOS_GAS_CONSUMED).unwrap(), "0");
        }

        /// Regression: a revm *pre-execution validation* failure
        /// (`EVMError::Transaction`) is a deterministic property of the
        /// attacker-shaped tx inputs — here a forwarded gas limit below the
        /// intrinsic cost. It must classify as a catchable 400, not a 500
        /// that the gateway reclassifies as `CracError::BlockAbort` (a
        /// user-triggerable block-production DoS).
        #[test]
        fn evm_transaction_validation_error_maps_to_400() {
            use crate::classify_evm_run_error;
            use revm::context::result::{EVMError, InvalidTransaction};
            use revm_etherlink::EvmRunError;

            let err = EvmRunError::RevmDB(EVMError::Transaction(
                InvalidTransaction::CallGasCostMoreThanGasLimit {
                    initial_gas: 21064,
                    gas_limit: 20452,
                },
            ));
            let mapped = classify_evm_run_error("EVM execution failed", err);
            assert!(
                matches!(mapped, TezosXRuntimeError::BadRequest(_)),
                "validation Err must be a catchable BadRequest, got {mapped:?}"
            );
            assert_eq!(
                build_response(Err(mapped)).status(),
                StatusCode::BAD_REQUEST
            );
        }

        /// The dual of the above: genuine infrastructure errors the user
        /// cannot shape (DB / header / custom) must still surface as a 500
        /// so the gateway aborts the block rather than swallowing a real
        /// failure as a catchable op-level revert.
        #[test]
        fn evm_infrastructure_error_maps_to_500() {
            use crate::classify_evm_run_error;
            use revm::context::result::EVMError;
            use revm_etherlink::EvmRunError;

            let err = EvmRunError::RevmDB(EVMError::Custom("storage unavailable".into()));
            let mapped = classify_evm_run_error("EVM execution failed", err);
            assert!(
                matches!(mapped, TezosXRuntimeError::Custom(_)),
                "infra Err must remain a 500-mapped Custom, got {mapped:?}"
            );
            assert_eq!(
                build_response(Err(mapped)).status(),
                StatusCode::INTERNAL_SERVER_ERROR
            );
        }

        /// A storage fault keeps its typed `Storage` tag (reusing the
        /// `EvmDbError` → `TezosXRuntimeError` translation) instead of being
        /// flattened into `Custom`, while still mapping to a 500.
        #[test]
        fn evm_storage_error_is_tagged_storage_not_custom() {
            use crate::classify_evm_run_error;
            use revm_etherlink::{EvmDbError, EvmRunError};

            let err = EvmRunError::DB(EvmDbError::CommitMismatch);
            let mapped = classify_evm_run_error("EVM execution failed", err);
            assert!(
                matches!(mapped, TezosXRuntimeError::Storage(_)),
                "DB fault must be tagged Storage, not Custom, got {mapped:?}"
            );
            assert_eq!(
                build_response(Err(mapped)).status(),
                StatusCode::INTERNAL_SERVER_ERROR
            );
        }
    }

    /// Test that serve() handles zero-amount transfers correctly.
    #[test]
    fn test_serve_zero_amount_transfer() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request(
            &sender,
            &destination,
            "0",
            vec![],
            journal.evm.outer_block(),
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        // Destination should have 0 balance (no transfer)
        let destination_account = StorageAccount::from_address(&destination).unwrap();
        let info = destination_account.info(&mut host).unwrap();
        assert_eq!(info.balance, revm::primitives::U256::ZERO);
    }

    /// Test that serve() correctly handles fractional TEZ amounts.
    #[test]
    fn test_serve_fractional_amount_transfer() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        // 0.5 TEZ = 500_000_000_000_000_000 wei
        let half_tez_wei = revm::primitives::U256::from(500_000_000_000_000_000u64);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request(
            &sender,
            &destination,
            "0.5",
            vec![],
            journal.evm.outer_block(),
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        let destination_account = StorageAccount::from_address(&destination).unwrap();
        let info = destination_account.info(&mut host).unwrap();
        assert_eq!(info.balance, half_tez_wei);
    }

    #[test]
    fn test_serve_calls_contract() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let contract = Address::from_slice(&[0x33; 20]);

        // Deploy a contract that stores CALLVALUE to slot 0 and returns
        // the value from slot 0 (32 bytes).
        //   CALLVALUE      (0x34)
        //   PUSH1 0x00     (0x6000)
        //   SSTORE         (0x55)
        //   PUSH1 0x20     (0x6020)  -- return size: 32 bytes
        //   PUSH1 0x00     (0x6000)  -- return offset: 0
        //   PUSH1 0x00     (0x6000)  -- slot 0
        //   SLOAD          (0x54)    -- load slot 0
        //   PUSH1 0x00     (0x6000)  -- memory offset: 0
        //   MSTORE         (0x52)    -- store to memory
        //   RETURN         (0xF3)
        let bytecode_raw = Bytes::from_hex("34600055602060006000546000525AF3").unwrap();
        let code_hash = bytes_hash(&bytecode_raw);
        let mut contract_account = StorageAccount::from_address(&contract).unwrap();
        contract_account
            .set_info(
                &mut host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    origin: AccountOrigin::Unclassified,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(&mut host, &bytecode_raw, Some(code_hash)).unwrap();

        // The cross-runtime call path uses gas_price = 0 and amount = 0,
        // so REVM's pre-flight balance requirement on the caller is 0 — no
        // need to fund TEZOSX_CALLER_ADDRESS (the production code in
        // `ensure_alias` doesn't either).
        let caller_addr = revm_etherlink::precompiles::constants::TEZOSX_CALLER_ADDRESS;
        let url = format!(
            "http://ethereum/{}",
            alloy_primitives::hex::encode(contract.0 .0)
        );
        let request = http::Request::builder()
            .method(http::Method::POST)
            .uri(&url)
            .header(
                tezosx_interfaces::X_TEZOS_SENDER,
                format!("0x{}", alloy_primitives::hex::encode(caller_addr.0 .0)),
            )
            .header(tezosx_interfaces::X_TEZOS_AMOUNT, "0")
            .header(tezosx_interfaces::X_TEZOS_GAS_LIMIT, u64::MAX.to_string())
            .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "0")
            .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "0")
            .body(vec![])
            .unwrap();

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
    }

    // ── L2-1363: inbound-CRAC tx.origin / msg.sender split ───────────

    /// Build a POST `serve()` request that carries an explicit
    /// `X-Tezos-Source` (the originator) distinct from `X-Tezos-Sender`
    /// (the immediate caller) — the inbound-CRAC shape that triggers the
    /// L2-1363 origin/sender split.
    fn build_serve_request_with_source(
        sender: &Address,
        source: &Address,
        destination: &Address,
        amount: &str,
        body: Vec<u8>,
    ) -> http::Request<Vec<u8>> {
        let url = format!(
            "http://ethereum/{}",
            alloy_primitives::hex::encode(destination.0 .0)
        );
        http::Request::builder()
            .method(http::Method::POST)
            .uri(&url)
            .header(
                tezosx_interfaces::X_TEZOS_SENDER,
                format!("0x{}", alloy_primitives::hex::encode(sender.0 .0)),
            )
            .header(
                tezosx_interfaces::X_TEZOS_SOURCE,
                format!("0x{}", alloy_primitives::hex::encode(source.0 .0)),
            )
            .header(tezosx_interfaces::X_TEZOS_AMOUNT, amount)
            .header(tezosx_interfaces::X_TEZOS_GAS_LIMIT, u64::MAX.to_string())
            .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "1")
            .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "1")
            .body(body)
            .unwrap()
    }

    /// Contract that records `ORIGIN` at storage slot 0 and `CALLER` at
    /// slot 1, so the test can read back what the EVM exposed:
    ///   ORIGIN PUSH1 0x00 SSTORE  CALLER PUSH1 0x01 SSTORE
    const ORIGIN_CALLER_RECORDER: &str = "3260005533600155";

    /// On an inbound CRAC where the originator (`X-Tezos-Source`) is not
    /// the immediate caller (`X-Tezos-Sender`), `tx.origin` is the
    /// originator and `msg.sender` is the immediate caller — they
    /// differ, as on native Ethereum when an EOA calls through a
    /// contract is impossible but the runtime boundary makes it so
    /// (L2-1363). Reverting the fix makes both equal `sender` and fails
    /// the slot-0 assertion.
    #[test]
    fn inbound_crac_origin_is_source_sender_is_caller() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]); // immediate caller (a contract alias)
        let source = Address::from_slice(&[0x22; 20]); // true originator (an EOA alias)
        let contract = Address::from_slice(&[0x33; 20]);
        deploy_at(
            &mut host,
            &contract,
            Bytes::from_hex(ORIGIN_CALLER_RECORDER).unwrap(),
        );

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request =
            build_serve_request_with_source(&sender, &source, &contract, "0", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        let stored_origin = read_slot(&mut host, &contract, revm::primitives::U256::ZERO);
        let stored_caller =
            read_slot(&mut host, &contract, revm::primitives::U256::from(1));
        assert_eq!(
            stored_origin,
            revm::primitives::U256::from_be_slice(source.as_slice()),
            "tx.origin must be the originator (X-Tezos-Source), not the immediate caller"
        );
        assert_eq!(
            stored_caller,
            revm::primitives::U256::from_be_slice(sender.as_slice()),
            "msg.sender must stay the immediate caller (X-Tezos-Sender)"
        );
    }

    /// `CrossRuntimeCallReceived.sourceRuntime` reflects the native runtime of
    /// `X-Tezos-Source` (forwarded as `X-Tezos-Source-Runtime`), not a
    /// hardcoded `"tezos"`. On a nested `EVM -> Michelson -> EVM` CRAC the
    /// forwarded source is the transitive EVM origin, so the pair
    /// `(sourceRuntime, sourceAddress)` must be self-consistent — Ethereum
    /// here. Reverting the fix re-hardcodes `"tezos"` and fails the
    /// assertion.
    #[test]
    fn cracreceived_source_runtime_follows_header() {
        use alloy_sol_types::SolEvent;

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]); // immediate Tezos sender alias
        let source = Address::from_slice(&[0x22; 20]); // transitive EVM origin
        let destination = Address::from_slice(&[0x33; 20]);

        let url = format!(
            "http://ethereum/{}",
            alloy_primitives::hex::encode(destination.0 .0)
        );
        let request = http::Request::builder()
            .method(http::Method::POST)
            .uri(&url)
            .header(
                tezosx_interfaces::X_TEZOS_SENDER,
                format!("0x{}", alloy_primitives::hex::encode(sender.0 .0)),
            )
            .header(
                tezosx_interfaces::X_TEZOS_SOURCE,
                format!("0x{}", alloy_primitives::hex::encode(source.0 .0)),
            )
            // The originator is EVM-native (round-trip alias resolved back
            // to the real EVM address on the Michelson side). The header
            // carries the numeric RuntimeId tag.
            .header(
                tezosx_interfaces::X_TEZOS_SOURCE_RUNTIME,
                u8::from(tezosx_interfaces::RuntimeId::Ethereum).to_string(),
            )
            .header(tezosx_interfaces::X_TEZOS_AMOUNT, "0")
            .header(tezosx_interfaces::X_TEZOS_GAS_LIMIT, u64::MAX.to_string())
            .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "1")
            .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "1")
            .body(vec![])
            .unwrap();

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);

        // A plain transfer to a code-less account emits no contract logs,
        // so the only log is the kernel's `CrossRuntimeCallReceived`.
        let crac_log = journal
            .evm
            .inner
            .logs
            .first()
            .expect("CrossRuntimeCallReceived log emitted");
        let decoded = crate::CrossRuntimeCallReceived::decode_log_data(&crac_log.data)
            .expect("log decodes as CrossRuntimeCallReceived");
        assert_eq!(
            decoded.sourceRuntime, "ethereum",
            "sourceRuntime must follow X-Tezos-Source-Runtime, not be hardcoded tezos"
        );
        assert_eq!(decoded.sourceAddress, source.to_string());
        assert_eq!(decoded.senderAddress, sender.to_string());
    }

    /// Backwards compatibility: a CRAC with no `X-Tezos-Source-Runtime`
    /// header (the historical Tezos-originated shape) still reports
    /// `sourceRuntime = "tezos"`.
    #[test]
    fn cracreceived_source_runtime_defaults_to_tezos_when_absent() {
        use alloy_sol_types::SolEvent;

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let source = Address::from_slice(&[0x22; 20]);
        let destination = Address::from_slice(&[0x33; 20]);

        // `build_serve_request_with_source` omits X-Tezos-Source-Runtime.
        let request =
            build_serve_request_with_source(&sender, &source, &destination, "0", vec![]);
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);

        let crac_log = journal
            .evm
            .inner
            .logs
            .first()
            .expect("CrossRuntimeCallReceived log emitted");
        let decoded = crate::CrossRuntimeCallReceived::decode_log_data(&crac_log.data)
            .expect("log decodes as CrossRuntimeCallReceived");
        assert_eq!(decoded.sourceRuntime, "tezos");
    }

    /// When the originator *is* the immediate caller (the common
    /// direct-EOA case, where the gateway sets `X-Tezos-Source ==
    /// X-Tezos-Sender`), `tx.origin == msg.sender` — the pre-L2-1363
    /// behavior is preserved.
    #[test]
    fn inbound_crac_origin_equals_caller_when_source_is_sender() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = UnimplementedRegistry;

        let eoa = Address::from_slice(&[0x44; 20]);
        let contract = Address::from_slice(&[0x55; 20]);
        deploy_at(
            &mut host,
            &contract,
            Bytes::from_hex(ORIGIN_CALLER_RECORDER).unwrap(),
        );

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request_with_source(&eoa, &eoa, &contract, "0", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        let stored_origin = read_slot(&mut host, &contract, revm::primitives::U256::ZERO);
        let stored_caller =
            read_slot(&mut host, &contract, revm::primitives::U256::from(1));
        let eoa_word = revm::primitives::U256::from_be_slice(eoa.as_slice());
        assert_eq!(stored_origin, eoa_word);
        assert_eq!(stored_caller, eoa_word);
    }

    /// `require(tx.origin == msg.sender)`: revert when they differ,
    /// else STOP.
    ///   ORIGIN CALLER EQ PUSH1 0x0b JUMPI PUSH1 0 PUSH1 0 REVERT
    ///   JUMPDEST STOP
    const EOA_ONLY_GUARD: &str = "323314600b5760006000fd5b00";

    /// The EOA-only guard now holds across the runtime boundary: an
    /// inbound CRAC whose immediate caller is a contract (`source !=
    /// sender`) is rejected (the guard reverts → catchable 400). This is
    /// the H-0094 bypass: before L2-1363 it returned 200 (`passed=1`).
    #[test]
    fn inbound_crac_eoa_only_guard_blocks_contract_caller() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let source = Address::from_slice(&[0x22; 20]);
        let guard = Address::from_slice(&[0x33; 20]);
        deploy_at(&mut host, &guard, Bytes::from_hex(EOA_ONLY_GUARD).unwrap());

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request =
            build_serve_request_with_source(&sender, &source, &guard, "0", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(
            resp.status(),
            http::StatusCode::BAD_REQUEST,
            "EOA-only guard must reject a contract caller (tx.origin != msg.sender)"
        );
    }

    /// Control: the same guard lets a genuine direct-EOA call through
    /// (`source == sender` → `tx.origin == msg.sender`).
    #[test]
    fn inbound_crac_eoa_only_guard_allows_eoa() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let eoa = Address::from_slice(&[0x44; 20]);
        let guard = Address::from_slice(&[0x55; 20]);
        deploy_at(&mut host, &guard, Bytes::from_hex(EOA_ONLY_GUARD).unwrap());

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request_with_source(&eoa, &eoa, &guard, "0", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
    }

    /// Sanity check: the inbound-CRAC originator (`X-Tezos-Source`)
    /// drives the `ORIGIN` opcode via the kernel's custom handler, not
    /// `TxEnv.caller`, so REVM's pre-execution nonce bump stays on the
    /// immediate caller (the sender alias) and never touches the
    /// originator's on-chain nonce — even when the originator is the
    /// outer-tx signing EOA on an `EVM -> Michelson -> EVM` round-trip.
    /// Asserts the persisted nonce of `source` stays at `0` after the
    /// inbound CRAC; a future regression that re-overloads
    /// `TxEnv.caller` would make this test read `1` instead.
    #[test]
    fn inbound_crac_does_not_leak_origin_nonce_bump() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        // Outer-tx originator (EOA) — its on-chain nonce must not drift.
        let source = Address::from_slice(&[0x22; 20]);
        let contract = Address::from_slice(&[0x33; 20]);
        // Target with a single STOP — trivial inner call.
        deploy_at(&mut host, &contract, Bytes::from_hex("00").unwrap());

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request =
            build_serve_request_with_source(&sender, &source, &contract, "0", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        let source_account = StorageAccount::from_address(&source).unwrap();
        let info = source_account.info(&mut host).unwrap();
        assert_eq!(
            info.nonce, 0,
            "inbound CRAC must not leak its pre-execution nonce bump onto the originator"
        );
    }

    #[test]
    fn outgoing_state_mutating_crac_forwards_originator_not_caller() {
        use alloy_sol_types::SolCall;
        use revm_etherlink::precompiles::runtime_gateway::RuntimeGateway;
        use tezosx_interfaces::testing::MockRegistry;

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();

        // A = true originator (X-Tezos-Source), B = immediate caller (X-Tezos-Sender).
        let originator = Address::from_slice(&[0x11; 20]); // A
        let sender = Address::from_slice(&[0x22; 20]); // B
        let contract = Address::from_slice(&[0x33; 20]); // C

        // Contract C: copies all calldata into memory, then forwards it
        // to the runtime-gateway precompile via CALL.
        //
        // CALLDATASIZE PUSH1 0 PUSH1 0 CALLDATACOPY   -- copy calldata → mem
        // PUSH1 0 PUSH1 0 CALLDATASIZE PUSH1 0         -- retLen retOff argsLen argsOff
        // PUSH1 0 PUSH20 <gw> GAS CALL POP STOP        -- value to gas call
        deploy_at(
            &mut host,
            &contract,
            Bytes::from_hex(
                "36600060003760006000366000600073\
                 ff000000000000000000000000000000000000075af15000",
            )
            .unwrap(),
        );

        // ABI-encode call(POST http://tezos/tz1target): the dummy Tezos
        // implicit address is irrelevant to the assertion; only the
        // state-mutating dispatch path (which goes through
        // capture_original_source) matters.
        let calldata = RuntimeGateway::callCall {
            url: "http://tezos/tz1target".to_string(),
            headers: vec![],
            body: vec![].into(),
            method: 1,
        }
        .abi_encode();

        // MockRegistry records every ensure_alias call so we can inspect
        // which address bytes were forwarded as the source alias.
        let registry = MockRegistry::new("tz1_mock_alias");

        // Ethereum-origin CRAC chain: the transitive originator A is an
        // EVM-native account, so `crac_origin_runtime` is Ethereum and the
        // gateway captures A's own EVM address as the source basis. A
        // default (Tezos-origin) journal would instead route A through the
        // foreign-runtime readonly-alias path, yielding the mock's fixed
        // "tz1_mock_alias" — which fails to parse as an EVM address and
        // reverts the precompile before `resolve_aliases` runs.
        let mut journal = TezosXJournal::new(
            tezosx_journal::CracId::new(
                u8::from(tezosx_interfaces::RuntimeId::Ethereum),
                0,
            ),
            tezos_crypto_rs::hash::OperationHash::default(),
            BlockConstants::dummy(),
        );
        // Inbound CRAC: source = A (originator), sender = B (caller).
        // Use a finite gas limit: gas::convert(Ethereum, Tezos, gas) =
        // gas * EVM_GAS_TO_MILLIGAS (22), so u64::MAX overflows and the
        // alias generation returns an error before ensure_alias is called.
        // 30_000_000 (30 M) is large enough for the bytecode + precompile but
        // small enough not to overflow the milligas conversion.
        let gas_limit: u64 = 30_000_000;
        let request = {
            let url = format!(
                "http://ethereum/{}",
                alloy_primitives::hex::encode(contract.as_slice())
            );
            http::Request::builder()
                .method(http::Method::POST)
                .uri(&url)
                .header(
                    tezosx_interfaces::X_TEZOS_SENDER,
                    format!("0x{}", alloy_primitives::hex::encode(sender.as_slice())),
                )
                .header(
                    tezosx_interfaces::X_TEZOS_SOURCE,
                    format!("0x{}", alloy_primitives::hex::encode(originator.as_slice())),
                )
                .header(tezosx_interfaces::X_TEZOS_AMOUNT, "0")
                .header(tezosx_interfaces::X_TEZOS_GAS_LIMIT, gas_limit.to_string())
                .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "1")
                .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "1")
                .body(calldata)
                .unwrap()
        };
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(
            resp.status(),
            http::StatusCode::OK,
            "serve must succeed: {}",
            String::from_utf8_lossy(resp.body())
        );

        // resolve_aliases is called twice inside the call(POST) precompile arm:
        //   [0] sender slot = C (inputs.caller, the contract that called the precompile)
        //   [1] source slot = should be A (originator); is B (caller) on buggy code
        let alias_calls = registry.ensure_alias_calls.borrow();
        assert!(
            alias_calls.len() >= 2,
            "expected at least 2 ensure_alias calls (sender + source), got {}",
            alias_calls.len()
        );

        // For an EVM address with no /origin record, resolve_routing returns
        // RoutingDecision::Native → AliasInfo { runtime: Ethereum,
        // native_address: canonicalize(Ethereum, addr.to_string()) }.
        // canonicalize lowercases the address, so the bytes are "0x11...11".
        let source_native_addr =
            String::from_utf8(alias_calls[1].0.native_address.clone())
                .expect("native_address must be valid UTF-8");
        let expected_originator_hex =
            format!("0x{}", alloy_primitives::hex::encode(originator.as_slice()));
        assert_eq!(
            source_native_addr,
            expected_originator_hex,
            "outgoing transfer must forward originator A ({expected_originator_hex}) \
             as X-Tezos-Source, not caller B (0x{}); \
             bug: capture_original_source uses tx().caller() instead of \
             cross_runtime_originator()",
            alloy_primitives::hex::encode(sender.as_slice()),
        );
    }

    // ── L2-1259: HTTP method dispatch and GET-static path ────────────

    /// Build an HTTP request with an explicit method, used by the
    /// L2-1259 dispatch tests.
    fn build_serve_request_with_method(
        method: http::Method,
        sender: &Address,
        destination: &Address,
        amount: &str,
        body: Vec<u8>,
    ) -> http::Request<Vec<u8>> {
        let url = format!(
            "http://ethereum/{}",
            alloy_primitives::hex::encode(destination.0 .0)
        );
        http::Request::builder()
            .method(method)
            .uri(&url)
            .header(
                tezosx_interfaces::X_TEZOS_SENDER,
                format!("0x{}", alloy_primitives::hex::encode(sender.0 .0)),
            )
            .header(tezosx_interfaces::X_TEZOS_AMOUNT, amount)
            .header(tezosx_interfaces::X_TEZOS_GAS_LIMIT, u64::MAX.to_string())
            .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "1")
            .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "1")
            .body(body)
            .unwrap()
    }

    /// Deploy a bytecode at `address` (test helper).
    fn deploy_at(host: &mut MockKernelHost, address: &Address, bytecode_raw: Bytes) {
        let code_hash = bytes_hash(&bytecode_raw);
        let mut account = StorageAccount::from_address(address).unwrap();
        account
            .set_info(
                host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    origin: AccountOrigin::Unclassified,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(host, &bytecode_raw, Some(code_hash)).unwrap();
    }

    /// Read `slot` from `address`'s storage.
    fn read_slot(
        host: &mut MockKernelHost,
        address: &Address,
        slot: revm::primitives::U256,
    ) -> revm::primitives::U256 {
        let account = StorageAccount::from_address(address).unwrap();
        account.get_storage(host, &slot).unwrap_or_default()
    }

    /// Regression (L2-1370): an inbound CRAC whose caller is a real
    /// code-bearing contract must not be rejected by EIP-3607. Since the
    /// same-runtime EVM-to-EVM round-trip forwards the calling contract
    /// verbatim as `msg.sender` (rather than a re-aliased,
    /// EIP-3607-exempt address), the `execute_call` path must disable
    /// EIP-3607; otherwise the caller's bytecode triggers
    /// `RejectCallerWithCode`, surfaced as a 500 that aborts the whole
    /// CRAC block. Reverting the fix (`is_simulation = false`) turns the
    /// asserted 200 into a 500.
    #[test]
    fn test_serve_accepts_code_bearing_caller() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let contract = Address::from_slice(&[0x22; 20]);

        // Give the caller non-empty, non-EIP-7702 bytecode: this is what
        // EIP-3607 rejects, and what a same-runtime EVM-to-EVM caller now
        // carries.
        deploy_at(&mut host, &sender, Bytes::from_hex("6001").unwrap());

        // Target stores 0x42 at slot 1 (PUSH1 0x42 PUSH1 0x01 SSTORE).
        deploy_at(&mut host, &contract, Bytes::from_hex("6042600155").unwrap());

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request(
            &sender,
            &contract,
            "0",
            vec![],
            journal.evm.outer_block(),
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(
            resp.status(),
            http::StatusCode::OK,
            "a code-bearing CRAC caller must not be rejected by EIP-3607"
        );
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        // The target actually executed (proves the call was not rejected
        // before reaching the EVM frame).
        assert_eq!(
            read_slot(&mut host, &contract, revm::primitives::U256::from(1)),
            revm::primitives::U256::from(0x42)
        );
    }

    #[test]
    fn test_serve_unsupported_method_returns_405() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        for method in [
            http::Method::PUT,
            http::Method::DELETE,
            http::Method::PATCH,
            http::Method::HEAD,
            http::Method::OPTIONS,
        ] {
            let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
            let request = build_serve_request_with_method(
                method.clone(),
                &sender,
                &destination,
                "0",
                vec![],
            );
            let resp = runtime.serve(&registry, &mut host, &mut journal, request);
            assert_eq!(
                resp.status(),
                http::StatusCode::METHOD_NOT_ALLOWED,
                "method {method} should be rejected with 405",
            );
        }
    }

    #[test]
    fn test_static_call_with_nonzero_amount_is_rejected() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request_with_method(
            http::Method::GET,
            &sender,
            &destination,
            "1",
            vec![],
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::BAD_REQUEST);
    }

    #[test]
    fn test_static_call_returns_view_result() {
        // Bytecode that returns the constant 0x42 left-padded to 32 bytes.
        // No state mutation: a pure Solidity-`view`-style read.
        //
        //   PUSH1 0x42     (0x6042)  -- value
        //   PUSH1 0x00     (0x6000)  -- mem offset
        //   MSTORE         (0x52)
        //   PUSH1 0x20     (0x6020)  -- return size
        //   PUSH1 0x00     (0x6000)  -- return offset
        //   RETURN         (0xF3)
        let bytecode_raw = Bytes::from_hex("604260005260206000F3").unwrap();

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x33; 20]);
        deploy_at(&mut host, &destination, bytecode_raw);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request_with_method(
            http::Method::GET,
            &sender,
            &destination,
            "0",
            vec![],
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        // Body is 32 bytes ending in 0x42.
        let body = resp.body();
        assert_eq!(body.len(), 32);
        assert_eq!(body[31], 0x42);
        for &b in &body[..31] {
            assert_eq!(b, 0);
        }
    }

    #[test]
    fn test_static_call_rejects_state_mutation() {
        // A `GET` runs the EVM transaction with `is_static = true` on
        // the top-level frame, so REVM enforces the standard
        // `STATICCALL` contract: any state-mutating opcode (`SSTORE`,
        // `LOG*`, `CREATE*`, `SELFDESTRUCT`, value-bearing `CALL`)
        // halts the call with `StateChangeDuringStaticCall`. We
        // surface that as a catchable 400 BadRequest so the
        // originating runtime can revert just this call.
        //
        // Bytecode: SSTORE 1 into slot 0 then RETURN — the SSTORE
        // alone is enough to trip the static check.
        //   PUSH1 0x01     (0x6001)
        //   PUSH1 0x00     (0x6000)
        //   SSTORE         (0x55)
        //   PUSH1 0x00     (0x6000)
        //   PUSH1 0x00     (0x6000)
        //   RETURN         (0xF3)
        let bytecode_raw = Bytes::from_hex("60016000556000600060F3").unwrap();

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x44; 20]);
        deploy_at(&mut host, &destination, bytecode_raw);

        // Pre-condition: slot 0 starts at zero.
        assert_eq!(
            read_slot(&mut host, &destination, revm::primitives::U256::ZERO),
            revm::primitives::U256::ZERO,
        );

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request_with_method(
            http::Method::GET,
            &sender,
            &destination,
            "0",
            vec![],
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        // The SSTORE attempt halts the EVM; we surface it as 400.
        assert_eq!(resp.status(), http::StatusCode::BAD_REQUEST);

        // Top-level commit applies whatever the journal accumulated.
        // Since REVM aborted the call on the SSTORE attempt, slot 0
        // stays at zero.
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        assert_eq!(
            read_slot(&mut host, &destination, revm::primitives::U256::ZERO),
            revm::primitives::U256::ZERO,
        );
    }

    #[test]
    fn test_static_call_revert_surfaces_as_bad_request() {
        // Bytecode that REVERTs unconditionally with a 1-byte payload.
        //   PUSH1 0x42     (0x6042)
        //   PUSH1 0x00     (0x6000)
        //   MSTORE         (0x52)
        //   PUSH1 0x20     (0x6020)
        //   PUSH1 0x00     (0x6000)
        //   REVERT         (0xFD)
        let bytecode_raw = Bytes::from_hex("604260005260206000FD").unwrap();

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x55; 20]);
        deploy_at(&mut host, &destination, bytecode_raw);

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let request = build_serve_request_with_method(
            http::Method::GET,
            &sender,
            &destination,
            "0",
            vec![],
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::BAD_REQUEST);
    }

    // ── EthereumRuntime::read_origin tests ───────────────────────────────

    mod read_origin_tests {
        use super::*;
        use revm_etherlink::{
            helpers::storage::bytes_hash,
            storage::world_state_handler::{AccountInfo, AccountOrigin, StorageAccount},
        };
        use tezosx_interfaces::{
            AliasInfo, Classification, RuntimeId, RuntimeInterface, ALIAS_LOOKUP_COST,
        };

        fn evm_addr(byte: u8) -> (Address, String) {
            let addr = Address::from_slice(&[byte; 20]);
            let addr_str = format!("0x{}", alloy_primitives::hex::encode(addr.0 .0));
            (addr, addr_str)
        }

        // Helper: write an account whose only relevant field is a
        // non-empty `code_hash`, so the back-stop's code-presence check
        // fires. The bytecode body itself is not needed by `read_origin`,
        // which only inspects the hash.
        fn set_account_with_code(host: &mut MockKernelHost, addr: &Address) {
            let bytecode_raw = Bytes::from_static(&[0x60, 0x00]); // PUSH1 0x00
            let code_hash = bytes_hash(&bytecode_raw);
            let mut account = StorageAccount::from_address(addr).unwrap();
            account
                .set_info(
                    host,
                    AccountInfo {
                        code_hash,
                        ..AccountInfo::default()
                    },
                )
                .unwrap();
        }

        // (a) Unclassified account with non-empty code → back-stop fires
        //     → Native. Covers both CREATE contracts and EIP-7702
        //     SET_CODE delegations. A single read is charged.
        #[test]
        fn backstop_fires_on_account_with_code() {
            let mut host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();
            let (addr, addr_str) = evm_addr(0xbb);

            set_account_with_code(&mut host, &addr);

            let budget = 100_000;
            let (class, consumed) =
                runtime.read_origin(&host, &addr_str, budget).unwrap();
            assert_eq!(class, Classification::Native);
            assert_eq!(consumed, ALIAS_LOOKUP_COST);
        }

        // (b) Unclassified account with empty code → Unknown. A positive
        //     nonce (a sign-only EOA never classified) must not be enough
        //     on its own — only code presence promotes to Native.
        #[test]
        fn backstop_with_empty_code_returns_unknown() {
            let mut host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();
            let (addr, addr_str) = evm_addr(0xcc);

            let mut account = StorageAccount::from_address(&addr).unwrap();
            account
                .set_info(
                    &mut host,
                    AccountInfo {
                        nonce: 5,
                        ..AccountInfo::default()
                    },
                )
                .unwrap();

            let budget = 100_000;
            let (class, consumed) =
                runtime.read_origin(&host, &addr_str, budget).unwrap();
            assert_eq!(class, Classification::Unknown);
            assert_eq!(consumed, ALIAS_LOOKUP_COST);
        }

        // (c) Account does not exist → Unknown, single read charged.
        #[test]
        fn no_account_returns_unknown() {
            let host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();
            let (_, addr_str) = evm_addr(0xdd);

            let budget = 100_000;
            let (class, consumed) =
                runtime.read_origin(&host, &addr_str, budget).unwrap();
            assert_eq!(class, Classification::Unknown);
            assert_eq!(consumed, ALIAS_LOOKUP_COST);
        }

        // (d) Native classification in the account record → Native.
        #[test]
        fn recorded_native_origin_returns_native() {
            let mut host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();
            let (addr, addr_str) = evm_addr(0xee);

            let mut account = StorageAccount::from_address(&addr).unwrap();
            account
                .set_info(
                    &mut host,
                    AccountInfo {
                        origin: AccountOrigin::Native,
                        ..AccountInfo::default()
                    },
                )
                .unwrap();

            let budget = 100_000;
            let (class, consumed) =
                runtime.read_origin(&host, &addr_str, budget).unwrap();
            assert_eq!(class, Classification::Native);
            assert_eq!(consumed, ALIAS_LOOKUP_COST);
        }

        // (e) Alias classification in the account record → Alias with
        //     its payload, from the same single read.
        #[test]
        fn recorded_alias_origin_returns_alias_payload() {
            let mut host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();
            let (addr, addr_str) = evm_addr(0xff);

            let alias_info = AliasInfo {
                runtime: RuntimeId::Tezos,
                native_address: b"tz1ABC".to_vec(),
            };
            let mut account = StorageAccount::from_address(&addr).unwrap();
            account
                .set_info(
                    &mut host,
                    AccountInfo {
                        origin: AccountOrigin::Alias(alias_info.clone()),
                        ..AccountInfo::default()
                    },
                )
                .unwrap();

            let budget = 100_000;
            let (class, consumed) =
                runtime.read_origin(&host, &addr_str, budget).unwrap();
            assert_eq!(class, Classification::Alias(alias_info));
            assert_eq!(consumed, ALIAS_LOOKUP_COST);
        }

        // (f) Malformed hex address → Unknown, no charge
        #[test]
        fn malformed_address_returns_unknown_no_charge() {
            let host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();

            let budget = 100_000;
            let (class, consumed) =
                runtime.read_origin(&host, "not-hex", budget).unwrap();
            assert_eq!(class, Classification::Unknown);
            assert_eq!(consumed, 0); // malformed → no charge
        }

        // (g) Wrong-length hex address → Unknown, no charge
        #[test]
        fn wrong_length_hex_returns_unknown_no_charge() {
            let host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();

            let budget = 100_000;
            let (class, consumed) = runtime
                .read_origin(&host, "0x00112233445566778899aabbccddeeff0011", budget)
                .unwrap();
            assert_eq!(class, Classification::Unknown);
            assert_eq!(consumed, 0); // malformed → no charge
        }

        // (h) Insufficient budget for the read → OutOfGas
        #[test]
        fn insufficient_budget_returns_out_of_gas() {
            let host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();
            let (_, addr_str) = evm_addr(0x11);

            // Budget below ALIAS_LOOKUP_COST: fails at the read
            let budget = ALIAS_LOOKUP_COST - 1;
            let err = runtime.read_origin(&host, &addr_str, budget).unwrap_err();
            assert_eq!(err, tezosx_interfaces::TezosXRuntimeError::OutOfGas);
        }

        // (i) Budget exactly ALIAS_LOOKUP_COST → succeeds, consumed == budget
        #[test]
        fn exact_budget_succeeds() {
            let host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();
            let (_, addr_str) = evm_addr(0x22);

            let budget = ALIAS_LOOKUP_COST;
            let (class, consumed) =
                runtime.read_origin(&host, &addr_str, budget).unwrap();
            assert_eq!(class, Classification::Unknown);
            assert_eq!(consumed, budget);
        }
    }

    // ── EthereumRuntime::ensure_alias atomicity (L2-1369) ────────────────

    mod ensure_alias_tests {
        use super::*;
        use primitive_types::U256;
        use revm::state::Bytecode;
        use revm_etherlink::precompiles::constants::ALIAS_FORWARDER_PRECOMPILE_ADDRESS;
        use tezosx_interfaces::{AliasInfo, CrossRuntimeContext, RuntimeId};

        // Native address whose alias we materialize. The bytes are the
        // UTF-8 form of the address string, exactly as ensure_alias hashes.
        fn alias_info() -> AliasInfo {
            AliasInfo {
                runtime: RuntimeId::Tezos,
                native_address: b"tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w".to_vec(),
            }
        }

        fn context() -> CrossRuntimeContext {
            CrossRuntimeContext {
                gas_limit: 30_000_000,
                timestamp: U256::from(1u64),
                block_number: U256::from(1u64),
            }
        }

        // Alias address, computed the same way ensure_alias does.
        fn alias_address(info: &AliasInfo) -> Address {
            let mut hasher = Keccak256::new();
            hasher.update(&info.native_address);
            Address::from_slice(&hasher.finalize()[0..20])
        }

        // A failed init must write no durable state and, once the frame
        // reverts, leave no staged alias, so the retry re-runs full
        // materialization instead of blessing an uninitialized forwarder.
        #[test]
        fn failed_init_defers_and_does_not_brick_alias() {
            let mut host = MockKernelHost::default();
            let runtime = EthereumRuntime::default();
            let registry = UnimplementedRegistry;
            let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);

            let info = alias_info();
            let alias = alias_address(&info);
            let pubkey = [0xab; 32];

            let delegation_code_hash = bytes_hash(
                Bytecode::new_eip7702(ALIAS_FORWARDER_PRECOMPILE_ADDRESS)
                    .original_byte_slice(),
            );

            // Mirror the EVM checkpoint taken around the gateway precompile
            // that drives ensure_alias.
            journal.evm.layered_state.checkpoint();

            // Budget 0 is below the EVM intrinsic gas, so init fails
            // before it can run.
            let failed = runtime.ensure_alias(
                &registry,
                &mut host,
                &mut journal,
                info.clone(),
                Some(pubkey.as_slice()),
                context(),
                0,
            );
            // A budget below intrinsic gas is a pre-execution validation
            // failure: it must surface as a catchable BadRequest, never a
            // block-aborting 500.
            assert!(
                matches!(failed, Err(TezosXRuntimeError::BadRequest(_))),
                "init under a zero gas budget must be a catchable BadRequest, got {failed:?}"
            );

            // Nothing is written to durable storage: the alias account has
            // no delegation code_hash and no Origin record.
            let alias_account = StorageAccount::from_address(&alias).unwrap();
            assert_ne!(
                alias_account.info(&mut host).unwrap().code_hash,
                delegation_code_hash,
                "no delegation code_hash may be written when init fails"
            );
            let classification = alias_account
                .info_without_migration(&host)
                .unwrap()
                .map(|info| info.origin)
                .unwrap_or_default();
            assert_eq!(
                classification,
                AccountOrigin::Unclassified,
                "no classification may be written when init fails"
            );

            // Frame revert on the propagated error drops the staged alias.
            journal.evm.layered_state.checkpoint_revert();
            assert!(
                journal
                    .evm
                    .layered_state
                    .pending_alias_origin(&alias)
                    .is_none(),
                "the staged alias must be dropped when its frame reverts"
            );

            // A second attempt re-runs full materialization and fails the
            // same way, rather than short-circuiting through a stale
            // classification.
            let retried = runtime.ensure_alias(
                &registry,
                &mut host,
                &mut journal,
                info,
                Some(pubkey.as_slice()),
                context(),
                0,
            );
            assert!(
                retried.is_err(),
                "a non-materialized alias must remain re-materializable, not bricked"
            );
        }
    }

    // ── CRAC block-observable opcodes (L2-1417) ─────────────────────────

    /// Deploy a probe that runs four block-observable opcodes and stores
    /// each result: `BASEFEE`→slot 0, `GASLIMIT`→slot 1, `GASPRICE`→slot
    /// 2, `PREVRANDAO`→slot 3. Each is `<OPCODE> PUSH1 <slot> SSTORE`.
    fn deploy_observables_probe(host: &mut MockKernelHost, address: &Address) {
        // 48=BASEFEE 45=GASLIMIT 3a=GASPRICE 44=PREVRANDAO,
        // 60 xx=PUSH1 slot, 55=SSTORE.
        let bytecode_raw = Bytes::from_hex("0x48600055456001553a60025544600355").unwrap();
        deploy_at(host, address, bytecode_raw);
    }

    /// An inbound CRAC must expose the originating block's observables to
    /// EVM bytecode — `BASEFEE`, `GASLIMIT` (the block gas limit),
    /// `PREVRANDAO`, and `GASPRICE` — rather than the zeroed placeholders
    /// of the synthesized block. The block is carried on the journal and
    /// inherited by `create_block_constants`; `GASPRICE` resolves to the
    /// block basefee, since Etherlink ignores priority fees and every
    /// transaction's effective price is exactly `base_fee_per_gas`.
    #[test]
    fn test_serve_block_observables_reflect_outer_block() {
        use tezos_ethereum::block::{BlockConstants, BlockFees};

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = UnimplementedRegistry;

        let live_basefee: u64 = 2_000_000_000; // 2 Gwei
        let block_gas_limit: u64 = 42_000_000;
        let prevrandao: u64 = 0xDEAD_BEEF;

        let outer = BlockConstants {
            block_fees: BlockFees::new(
                primitive_types::U256::from(live_basefee),
                primitive_types::U256::from(live_basefee),
                primitive_types::U256::zero(),
            ),
            gas_limit: block_gas_limit,
            prevrandao: Some(primitive_types::H256::from_low_u64_be(prevrandao)),
            ..BlockConstants::test_block_with_no_fees()
        };

        let sender = Address::from_slice(&[0x11; 20]);
        let contract = Address::from_slice(&[0xBB; 20]);
        deploy_observables_probe(&mut host, &contract);

        let mut journal = TezosXJournal::new(
            tezosx_journal::CracId::new(0, 0),
            Default::default(),
            outer,
        );

        // Serve against the journal's own block so the forwarded gas
        // matches the served block's limit.
        let request = build_serve_request(
            &sender,
            &contract,
            "0",
            vec![],
            journal.evm.outer_block(),
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        // A successful serve also proves the basefee preflight gate is
        // opened: the CRAC runs at `gas_price = 0`, which stock EIP-1559
        // validation would reject against the live basefee.
        assert_eq!(
            resp.status(),
            http::StatusCode::OK,
            "CRAC serve() must succeed: {:?}",
            String::from_utf8_lossy(resp.body())
        );

        let commit_block = BlockConstants::test_block_with_no_fees();
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &commit_block,
            &mut journal,
        )
        .unwrap();

        let u256 = revm::primitives::U256::from;
        assert_eq!(
            read_slot(&mut host, &contract, u256(0u64)),
            u256(live_basefee),
            "BASEFEE must reflect the live block basefee"
        );
        assert_eq!(
            read_slot(&mut host, &contract, u256(1u64)),
            u256(block_gas_limit),
            "GASLIMIT must reflect the block gas limit, not the forwarded call gas"
        );
        assert_eq!(
            read_slot(&mut host, &contract, u256(2u64)),
            u256(live_basefee),
            "GASPRICE must reflect the live basefee, not the cross-runtime TxEnv's 0"
        );
        assert_eq!(
            read_slot(&mut host, &contract, u256(3u64)),
            u256(prevrandao),
            "PREVRANDAO must reflect the block's value, not the default 0"
        );
    }
}
