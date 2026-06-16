// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::{
    Address, AddressHash, BinWriter, ByteReprTrait, Operation, OperationInfo,
    TransferTokens, TypedValue,
};
use mir::ast::{PublicKeyHash, Type};
use mir::typechecker::typecheck_value;
use mir::{
    ast::{Entrypoint, Micheline},
    context::CtxTrait,
    gas::OutOfGas,
};
use num_bigint::{BigInt, BigUint};
use num_traits::{ToPrimitive, Zero};
use primitive_types::U256;
use sha3::{Digest, Keccak256};
use std::collections::HashMap;
use std::rc::Rc;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_tezlink::block::AppliedOperation;
use tezos_tezlink::operation_result::{InternalOperationSum, TransferError};
use tezosx_interfaces::{
    gas::convert as convert_gas,
    headers::{format_tez_from_mutez, parse_u64_opt},
    resolve_routing, translate_original_source, AliasInfo, Classification,
    CrossRuntimeContext, Registry, RoutingDecision, RuntimeId, ALIAS_LOOKUP_MILLIGAS,
    ERR_FORBIDDEN_TEZOS_HEADER, X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER, X_TEZOS_CRAC_ID,
    X_TEZOS_GAS_CONSUMED, X_TEZOS_GAS_LIMIT, X_TEZOS_SENDER, X_TEZOS_SOURCE,
    X_TEZOS_SOURCE_RUNTIME, X_TEZOS_STORAGE_COST, X_TEZOS_TIMESTAMP,
};
use tezosx_journal::TezosXJournal;

use crate::account_storage::TezlinkAccount;
use crate::mir_ctx::{
    HasContractAccount, HasCracChainDepth, HasCrossRuntime, HasHost, HasOperationGas,
    HasOriginLookup, HasSourcePublicKey,
};

/// Errors from CRAC-capable operations. The two variants have fundamentally
/// different semantics and must be handled at different levels.
#[derive(Debug)]
pub enum CracError {
    /// Operation-level failure (4xx, bad input, etc.), revert this operation only.
    Operation(TransferError),
    /// The target runtime is broken (5xx), abort the entire block.
    BlockAbort(String),
}

impl std::fmt::Display for CracError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CracError::Operation(e) => write!(f, "{e}"),
            CracError::BlockAbort(msg) => write!(f, "block abort: {msg}"),
        }
    }
}

impl From<TransferError> for CracError {
    fn from(e: TransferError) -> Self {
        match e {
            // Kernel-internal failures (storage corruption / host I/O /
            // encoding bugs in the storage layer) bypass the
            // CracError::Operation channel and signal a block revert
            // directly, matching the Boundary-2 (gateway HTTP) classifier.
            //
            // The same construction sites are reached from both the
            // regular Tezos pipeline and the cross-runtime gateway path;
            // master's blanket Operation wrap silently downgraded the
            // regular-pipeline outcome to op-level. Routing them here as
            // BlockAbort makes both paths agree, and matches how the EVM
            // execution path lifts the same class of failures into its
            // own outer EVMError channel (revm/src/lib.rs:362+,
            // apply.rs:716-721).
            //
            // The cause discarded by .map_err(|_| TransferError::FailedTo*)?
            // at each construction site is a tezos_storage::error::Error
            // (Path / Runtime / Storage / RlpDecoderError / NomReadError /
            // BinWriteError / InvalidLoadValue / ImplicitToOriginated /
            // OriginatedToImplicit / TcError / TryFromBigIntError) — none
            // of these are recoverable user input. A non-existent
            // originated destination is caught upstream by the
            // `dest_account.exists(host)` guard in `transfer` and
            // surfaces as the user-level TransferError::ContractDoesNotExist
            // instead, so FailedToFetchContractCode here can only fire
            // from genuine kernel-internal causes.
            TransferError::FailedToApplyBalanceChanges
            | TransferError::FailedToAllocateDestination
            | TransferError::FailedToFetchDestinationAccount
            | TransferError::FailedToFetchContractCode
            | TransferError::FailedToFetchContractStorage
            | TransferError::FailedToFetchDestinationBalance
            | TransferError::FailedToFetchSenderBalance
            | TransferError::FailedToUpdateContractStorage
            | TransferError::FailedToUpdateDestinationBalance
            | TransferError::FailedToComputeBalanceUpdate(_) => {
                CracError::BlockAbort(format!("internal error during transfer: {e}"))
            }
            // Peer-runtime brokenness surfaced via MIR `VIEW
            // staticcall_evm` (5xx, gas-budget translation overflow,
            // dispatch-request build) — same EVM-runtime breakage
            // would surface as `CracError::BlockAbort` on the
            // gateway-HTTP entrypoint path; the view path must agree.
            TransferError::EnshrinedViewDispatchAbort(_) => {
                CracError::BlockAbort(format!("enshrined view dispatch: {e}"))
            }
            _ => CracError::Operation(e),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EnshrinedContracts {
    TezosXGateway,
    ERC20Wrapper,
}

/// prefix used to do a first quick check to eliminate most non enshrined contracts
const ENSHRINED_PREFIX: [u8; 6] = [2, 90, 121, 0, 0, 0];
// KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw
const GATEWAY_ADDRESS: [u8; 20] = [
    2, 90, 121, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
];
// KT18oDJJKXMKhfE1bSuAPGp92pYcwVKvCChb
const ERC20_WRAPPER_ADDRESS: [u8; 20] = [
    2, 90, 121, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2,
];

// Should be as transparent/cheap as possible for none-native contract (the
// direct path)
impl EnshrinedContracts {
    /// Returns the 22-byte `AddressHash` binary encoding for this contract:
    /// `[0x01][20-byte KT1 hash][0x00]`.
    pub fn address_hash_bytes(&self) -> [u8; 22] {
        let raw: &[u8; 20] = match self {
            Self::TezosXGateway => &GATEWAY_ADDRESS,
            Self::ERC20Wrapper => &ERC20_WRAPPER_ADDRESS,
        };
        let mut bytes = [0u8; 22];
        bytes[0] = 0x01;
        bytes[1..21].copy_from_slice(raw);
        // bytes[21] stays 0x00
        bytes
    }
}

pub fn from_kt1(kt1: &ContractKt1Hash) -> Option<EnshrinedContracts> {
    let bytes = kt1.to_bytes().ok()?;
    // let's escape early if possible
    if !bytes.starts_with(&ENSHRINED_PREFIX) {
        return None;
    }
    if bytes.as_slice() == GATEWAY_ADDRESS {
        Some(EnshrinedContracts::TezosXGateway)
    } else if bytes.as_slice() == ERC20_WRAPPER_ADDRESS {
        Some(EnshrinedContracts::ERC20Wrapper)
    } else {
        None
    }
}

pub fn is_enshrined(kt1: &ContractKt1Hash) -> bool {
    from_kt1(kt1).is_some()
}

/// Extract the inner [`TypedValue`] from an `Rc`.
///
/// [`TypedValue`] wraps recursive positions (e.g. `Pair`, `Option`) in `Rc`
/// to keep the enum sized. This helper moves the value out without copying
/// when the reference count is 1 (the common case for freshly typechecked
/// values), and falls back to cloning if the `Rc` is shared.
fn unwrap_rc(rc: Rc<TypedValue<'_>>) -> TypedValue<'_> {
    Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
}

/// Typecheck a Micheline value against the expected type for the given
/// enshrined contract entrypoint. Returns the typed value, or an error
/// if the entrypoint is unknown or the value doesn't match the type.
fn typecheck_entrypoint_value<'a>(
    contract: EnshrinedContracts,
    entrypoint: &Entrypoint,
    value: &Micheline<'a>,
    ctx: &mut impl CtxTrait<'a>,
) -> Result<TypedValue<'a>, TransferError> {
    let entrypoints = get_enshrined_contract_entrypoint(contract).ok_or_else(|| {
        TransferError::GatewayError("Failed to build entrypoint map".into())
    })?;
    let ty = entrypoints.get(entrypoint).ok_or_else(|| {
        TransferError::GatewayError(format!("Unknown entrypoint: {entrypoint}"))
    })?;
    typecheck_value(value, ctx, ty)
        .map_err(|e| TransferError::GatewayError(format!("Invalid parameters: {e}")))
}

pub(crate) fn execute_enshrined_contract<'a, Host>(
    contract: EnshrinedContracts,
    entrypoint: &Entrypoint,
    value: Micheline<'a>,
    ctx: &mut (impl CtxTrait<'a>
              + HasHost<Host>
              + HasContractAccount
              + HasOperationGas
              + HasSourcePublicKey
              + HasOriginLookup
              + HasCrossRuntime<Host>
              + HasCracChainDepth),
) -> Result<Vec<OperationInfo<'a>>, CracError>
where
    Host: StorageV1,
{
    let typed = typecheck_entrypoint_value(contract, entrypoint, &value, ctx)?;
    match contract {
        EnshrinedContracts::TezosXGateway => {
            if entrypoint.as_str() == Some("call_evm") {
                charge_gateway_base_cost(ctx)?;
                let (dest, method_sig, abi_params, callback) =
                    extract_call_params(typed)?;
                // Per-byte surcharge: calldata (selector+params) going
                // out; response body is charged after it's returned.
                charge_gateway_payload(ctx, 4 + abi_params.len())?;
                ctx.operation_gas()
                    .cast_and_consume_milligas(SELECTOR_COMPUTATION_MILLIGAS)
                    .map_err(TransferError::OutOfGas)?;
                let selector = compute_selector(&method_sig);
                let mut calldata = Vec::with_capacity(4 + abi_params.len());
                calldata.extend_from_slice(&selector);
                calldata.extend_from_slice(&abi_params);
                let request = build_ethereum_request(&dest, &calldata)?;
                let response_body = dispatch_crac_call(ctx, request)?;
                charge_gateway_payload(ctx, response_body.len())?;
                dispatch_callback(ctx, callback, response_body).map_err(Into::into)
            } else if entrypoint.as_str() == Some("call") {
                charge_gateway_base_cost(ctx)?;
                let (request, callback) = extract_http_call_request(typed)?;
                // Per-byte surcharge: outgoing HTTP body.
                charge_gateway_payload(ctx, request.body().len())?;
                let num_user_headers = request.headers().len() as u64;
                ctx.operation_gas()
                    .cast_and_consume_milligas(
                        num_user_headers
                            .saturating_mul(HEADER_VALIDATION_PER_HEADER_MILLIGAS),
                    )
                    .map_err(TransferError::OutOfGas)?;
                let body = dispatch_crac_call(ctx, request)?;
                charge_gateway_payload(ctx, body.len())?;
                dispatch_callback(ctx, callback, body).map_err(Into::into)
            } else if entrypoint.as_str() == Some("collect_result") {
                // %collect_result lets a Michelson adapter deposit a
                // result payload on the current dispatch slot so the
                // kernel can later surface it as the HTTP response body
                // to the EVM gateway, giving EVM callers a synchronous
                // return value.  Unlike the other gateway entrypoints
                // it does not trigger an HTTP round-trip, so
                // `charge_gateway_base_cost` does not apply.
                //
                // Reject any non-zero amount. %collect_result is not a
                // CRAC and has no recipient: allowing tez through it
                // would create a hidden fund side-channel and break EVM
                // sub-call semantics, where the return path carries
                // only bytes. Adapters that need to refund tez must do
                // so via an explicit CRAC (%call / %call_evm).
                if ctx.amount() != 0 {
                    return Err(TransferError::GatewayError(
                        "collect_result: amount must be 0".into(),
                    )
                    .into());
                }
                let TypedValue::Bytes(payload) = typed else {
                    return Err(TransferError::GatewayError(
                        "Expected bytes for collect_result entrypoint".into(),
                    )
                    .into());
                };
                // Pre-charge store + result surfacing + forward. Result
                // surfacing and forward happen after `TezlinkOperationGas`
                // has already reported its consumption, so they must be
                // paid here at deposit time.
                ctx.operation_gas()
                    .cast_and_consume_milligas(collect_result_size_cost(payload.len()))
                    .map_err(TransferError::OutOfGas)?;
                ctx.journal()
                    .michelson
                    .set_dispatch_result(payload)
                    .map_err(|e| {
                        TransferError::GatewayError(format!("collect_result: {e}"))
                    })?;
                Ok(vec![])
            } else {
                Err(TransferError::GatewayError(format!(
                    "Unknown entrypoint: {entrypoint}"
                ))
                .into())
            }
        }
        EnshrinedContracts::ERC20Wrapper => {
            let ep = entrypoint.as_str();
            let method_sig = match ep {
                Some("transfer") => "transfer(address,uint256)",
                Some("approve") => "approve(address,uint256)",
                _ => {
                    return Err(TransferError::GatewayError(format!(
                        "Unknown ERC-20 wrapper entrypoint: {entrypoint}"
                    ))
                    .into())
                }
            };
            let (evm_contract, addr_bytes, amount) =
                extract_erc20_address_uint256_params(typed)?;
            let calldata = abi_encode_address_uint256(method_sig, &addr_bytes, &amount)?;
            charge_gateway_payload(ctx, calldata.len())?;
            let request = build_ethereum_request(&evm_contract, &calldata)?;
            dispatch_crac_call(ctx, request)?;
            Ok(vec![])
        }
    }
}

// ── Gateway gas constants (milligas) ─────────────────────────────────

/// Fixed overhead for all gateway entrypoints (value extraction, HTTP
/// construction, header injection, gas conversion, response parsing).
/// Micheline typechecking is metered separately by the MIR typechecker.
const TEZOSX_GATEWAY_BASE_COST_MILLIGAS: u64 = 100_000;

/// Per-byte surcharge on the flat base cost, charged at Tezos-as-caller
/// gateway entrypoints (`%call`, `%call_evm`, ...) for the
/// boundary work performed when bytes cross the CRAC: outgoing-body
/// marshalling and incoming-response ingest (`Vec::to_vec`, header-map
/// allocation, callback/selector assembly, ...).
///
/// TODO(L2-1165): replace with a benchmarked value.
const TEZOSX_GATEWAY_PER_BYTE_MILLIGAS: u64 = 300;

/// Surcharge when amount > 0 (balance reset after value transfer).
/// Equivalent to SSTORE non-zero→zero (5,000 EVM gas × 100).
const VALUE_TRANSFER_SURCHARGE_MILLIGAS: u64 = 500_000;

/// Per user-supplied header validation in %call: byte-level prefix
/// check against forbidden X-Tezos-* headers plus the subsequent
/// `HeaderMap` insertion (hash + bucket insert + potential rehash).
const HEADER_VALIDATION_PER_HEADER_MILLIGAS: u64 = 10_000;

/// Keccak256 selector computation in %call_evm.
const SELECTOR_COMPUTATION_MILLIGAS: u64 = 2_000;

/// Callback TRANSFER_TOKENS dispatch (~100 Micheline Bytes assembly
/// + ~120 operation allocation).
const CALLBACK_DISPATCH_MILLIGAS: u64 = 220;

/// Size-independent portion of the %collect_result triptych:
/// - `100` mgas — frame-slot store
/// - `260` mgas — result surfacing
/// - `100` mgas — HTTP response-body forward
///
/// Pre-charged at deposit time because result surfacing and forward
/// happen after `TezlinkOperationGas` has already reported its
/// consumption, so they would otherwise ride for free.
const COLLECT_RESULT_SIZE_BASE_MILLIGAS: u64 = 460;

/// Full size-dependent cost of the %collect_result deposit triptych —
/// store into frame slot + result surfacing + forward into HTTP
/// response body: `(100 + size/2) + (260 + size/2) + (100 + size/2)
/// = 460 + 1.5 * size`.
///
/// The third term (response-body forward) is a conservative
/// over-count — the real forward is a `Vec<u8>` move into
/// `Response::builder().body(...)`, not a copy. Kept in as margin
/// until a bench pass validates the constants.
///
/// TODO(L2-1224): replace the three components with benchmarked
/// values from `octez-snoop`. The current split is a back-of-envelope
/// model and may overcharge by a wide factor if the journal-frame
/// path is closer to a memcpy than to a durable write.
fn collect_result_size_cost(payload_len: usize) -> u64 {
    COLLECT_RESULT_SIZE_BASE_MILLIGAS + ((payload_len as u64).saturating_mul(3) >> 1)
}

/// Charge the fixed per-call overhead for gateway entrypoints that
/// cross into another runtime (HTTP request construction, header
/// injection, response parsing).
///
/// Not charged on the synthetic read-only views (`originOf`,
/// `resolveAddress`): they do no HTTP round-trip and no header injection,
/// and the MIR typechecker already meters input validation on the view's
/// parameter type — that's the work the EVM precompile's
/// `ORIGIN_OF_BASE_COST` / `RESOLVE_ADDRESS_BASE_COST` prices on the EVM
/// side (ABI selector matching + decode). Charging a flat constant on
/// the Michelson side would double-count it.
pub(crate) fn charge_gateway_base_cost(
    ctx: &mut impl HasOperationGas,
) -> Result<(), TransferError> {
    ctx.operation_gas()
        .cast_and_consume_milligas(TEZOSX_GATEWAY_BASE_COST_MILLIGAS)
        .map_err(TransferError::OutOfGas)
}

/// Charge `TEZOSX_GATEWAY_PER_BYTE_MILLIGAS * bytes` for a chunk of
/// gateway payload (outgoing body or response body). Complements the
/// flat `charge_gateway_base_cost` with a size-proportional component.
pub(crate) fn charge_gateway_payload(
    ctx: &mut impl HasOperationGas,
    bytes: usize,
) -> Result<(), TransferError> {
    charge_gateway_payload_gas(ctx.operation_gas(), bytes)
}

/// [`charge_gateway_payload`] against a raw [`TezlinkOperationGas`], for
/// call sites that hold the gas accumulator directly rather than through a
/// [`HasOperationGas`] context (e.g. [`classify_and_charge_crac_response`]).
fn charge_gateway_payload_gas(
    operation_gas: &mut crate::gas::TezlinkOperationGas,
    bytes: usize,
) -> Result<(), TransferError> {
    let cost = TEZOSX_GATEWAY_PER_BYTE_MILLIGAS.saturating_mul(bytes as u64);
    if cost == 0 {
        return Ok(());
    }
    operation_gas
        .cast_and_consume_milligas(cost)
        .map_err(TransferError::OutOfGas)
}

/// If `destination` is `Some`, deduct gas and return a `TRANSFER_TOKENS`
/// operation that sends `response_body` as `bytes` to the callback contract.
/// If `None`, return an empty operation list (no callback requested).
fn dispatch_callback<'a>(
    ctx: &mut (impl CtxTrait<'a> + HasOperationGas),
    destination: Option<Address>,
    response_body: Vec<u8>,
) -> Result<Vec<OperationInfo<'a>>, TransferError> {
    let Some(destination) = destination else {
        return Ok(vec![]);
    };
    ctx.operation_gas()
        .cast_and_consume_milligas(CALLBACK_DISPATCH_MILLIGAS)
        .map_err(TransferError::OutOfGas)?;
    let counter = ctx.operation_counter();
    Ok(vec![OperationInfo {
        operation: Operation::TransferTokens(TransferTokens {
            param: TypedValue::Bytes(response_body),
            destination_address: destination,
            // The callback only delivers the response body.
            // Any value transfers from the EVM side happen as
            // separate operations mediated by the EVM alias
            // and the EVM gateway.
            amount: 0,
        }),
        counter,
    }])
}

/// Extract an `Option<Address>` from a typechecked `option (contract bytes)` value.
fn extract_callback(
    typed: TypedValue<'_>,
    entrypoint_name: &str,
) -> Result<Option<Address>, TransferError> {
    match typed {
        TypedValue::Option(Some(rc)) => {
            let TypedValue::Contract(addr) = unwrap_rc(rc) else {
                return Err(TransferError::GatewayError(format!(
                    "{entrypoint_name}: expected contract in callback option"
                )));
            };
            Ok(Some(addr))
        }
        TypedValue::Option(None) => Ok(None),
        _ => Err(TransferError::GatewayError(format!(
            "{entrypoint_name}: expected option for callback"
        ))),
    }
}

/// Tag used by the synthetic CRAC begin event built in
/// `tezosx-tezos-runtime/src/lib.rs::build_(failed_)crac_receipt`.
/// Imported by the receipt builder so both producer and consumer
/// agree on the tag.
pub const SYNTHETIC_CRAC_EVENT_TAG: &str = "cross_runtime_call";

/// Tag used by the synthetic CRAC end event built in
/// `tezosx-tezos-runtime/src/lib.rs::build_(failed_)crac_receipt`.
/// Imported by the receipt builder so both producer and consumer
/// agree on the tag.
pub const SYNTHETIC_CRAC_END_EVENT_TAG: &str = "cross_runtime_call_end";

/// Returns the tag string of a synthetic CRAC frame marker, or `None` if
/// `iop` is not one.
///
/// Returns `Some(SYNTHETIC_CRAC_EVENT_TAG)` for begin markers and
/// `Some(SYNTHETIC_CRAC_END_EVENT_TAG)` for end markers; `None` for
/// everything else.
///
/// User-issued Michelson `EMIT` ops also reify as
/// `InternalOperationSum::Event`, but the kernel stamps the synthetic
/// markers with the null implicit sender (`crate::NULL_PKH`) — user EMITs
/// always carry the executing contract's originated (KT1) sender via
/// `sender_account.contract()`.  A Michelson contract cannot impersonate
/// the null implicit sender.
///
/// The null sender alone is NOT sufficient: bridge deposit events also
/// carry the null sender with tag `"deposit"`, so the tag conjunction is
/// load-bearing — deposit-style null-sender events return `None` because
/// of the tag conjunct.
pub fn synthetic_crac_marker_tag(iop: &InternalOperationSum) -> Option<&str> {
    use tezos_protocol::contract::Contract;
    match iop {
        InternalOperationSum::Event(e) => {
            let sender_is_null = matches!(
                &e.sender,
                Contract::Implicit(pkh) if pkh.to_b58check() == crate::NULL_PKH
            );
            if !sender_is_null {
                return None;
            }
            e.content.tag.as_ref().and_then(|t| match t.as_str() {
                Some(SYNTHETIC_CRAC_EVENT_TAG) => Some(SYNTHETIC_CRAC_EVENT_TAG),
                Some(SYNTHETIC_CRAC_END_EVENT_TAG) => Some(SYNTHETIC_CRAC_END_EVENT_TAG),
                _ => None,
            })
        }
        _ => None,
    }
}

/// Drain re-entrant CRAC receipts that accumulated since the per-list
/// watermarks and splice their internal ops into the caller's flat list.
///
/// After each internal operation that may have triggered a cross-runtime
/// call (e.g. a gateway call that re-entered Michelson), this function
/// drains the CRAC receipts that were pushed since the watermarks —
/// across all three lists (pending, failed, backtracked) — and returns
/// all their internal operation results in execution order, preserving
/// DFS order for nested frames.
///
/// Draining all three lists matters because a re-entrant inner CRAC
/// can land in any of them depending on its outcome and the EVM frame
/// catch behavior:
///   - Applied inner: pushed to `pending_crac_receipts`.
///   - Failed inner caught by an upstream EVM frame: pushed to
///     `failed_crac_receipts`, must still nest under its outer parent
///     in the receipt rather than reach the top-level merge with a
///     smaller seq than the outer (would invert DFS order).
///   - Applied inner whose enclosing EVM frame later reverted but was
///     caught above: migrated to `backtracked_crac_receipts` by
///     `revert_frame`; same nesting requirement.
///
/// Receipts are sorted by their shared sequence number before splicing
/// so cross-list interleavings of inner CRACs at the same depth come
/// out in execution order.
///
/// Frame markers (begin/end events) are preserved: each inner frame
/// is self-bracketed so indexers can identify nested frames without
/// any special treatment at the drain site.  User-issued `EMIT` ops
/// were always preserved and continue to be.
pub(crate) fn drain_reentrant_crac_ops(
    journal: &mut TezosXJournal,
    pending_watermark: usize,
    failed_watermark: usize,
    backtracked_watermark: usize,
) -> Vec<InternalOperationSum> {
    use tezos_tezlink::operation_result::{OperationDataAndMetadata, OperationResultSum};
    let mut receipts: Vec<(u64, AppliedOperation)> = Vec::new();
    receipts.extend(
        journal
            .michelson
            .pending_crac_receipts
            .drain(pending_watermark..),
    );
    receipts.extend(
        journal
            .michelson
            .failed_crac_receipts
            .drain(failed_watermark..),
    );
    receipts.extend(
        journal
            .michelson
            .backtracked_crac_receipts
            .drain(backtracked_watermark..),
    );
    receipts.sort_by_key(|(seq, _)| *seq);

    let mut ops = Vec::new();
    for (_, receipt) in receipts {
        let OperationDataAndMetadata::OperationWithMetadata(batch) =
            receipt.op_and_receipt;
        for op in batch.operations {
            if let OperationResultSum::Transfer(result) = op.receipt {
                ops.extend(result.internal_operation_results);
            }
        }
    }
    ops
}

/// Extract (destination, method_signature, abi_parameters, callback) from a typed
/// Pair(String, Pair(String, Pair(Bytes, Option(Contract(Bytes))))) value.
fn extract_call_params(
    typed: TypedValue<'_>,
) -> Result<(String, String, Vec<u8>, Option<Address>), TransferError> {
    let TypedValue::Pair(dest_rc, inner_rc) = typed else {
        return Err(TransferError::GatewayError(
            "call: expected pair (destination, (method_sig, (abi_params, callback)))"
                .into(),
        ));
    };
    let TypedValue::String(dest) = unwrap_rc(dest_rc) else {
        return Err(TransferError::GatewayError(
            "call: expected string for destination".into(),
        ));
    };
    let TypedValue::Pair(sig_rc, inner2_rc) = unwrap_rc(inner_rc) else {
        return Err(TransferError::GatewayError(
            "call: expected pair (method_sig, (abi_params, callback))".into(),
        ));
    };
    let TypedValue::String(method_sig) = unwrap_rc(sig_rc) else {
        return Err(TransferError::GatewayError(
            "call: expected string for method signature".into(),
        ));
    };
    let TypedValue::Pair(params_rc, callback_rc) = unwrap_rc(inner2_rc) else {
        return Err(TransferError::GatewayError(
            "call: expected pair (abi_params, callback)".into(),
        ));
    };
    let TypedValue::Bytes(abi_params) = unwrap_rc(params_rc) else {
        return Err(TransferError::GatewayError(
            "call: expected bytes for ABI parameters".into(),
        ));
    };
    let callback = extract_callback(unwrap_rc(callback_rc), "call_evm")?;
    Ok((dest, method_sig, abi_params, callback))
}

/// Build an `http::Request<Vec<u8>>` and optional callback from a typed
/// Pair(String, Pair(List(Pair(String, String)), Pair(Bytes, Pair(Nat, Option(Contract(Bytes)))))) value.
///
/// Method mapping: 0 = GET, 1 = POST. Other values default to POST.
fn extract_http_call_request(
    typed: TypedValue<'_>,
) -> Result<(http::Request<Vec<u8>>, Option<Address>), TransferError> {
    let TypedValue::Pair(url_rc, inner_rc) = typed else {
        return Err(TransferError::GatewayError(
            "http_call: expected pair (url, (headers, (body, (method, callback))))"
                .into(),
        ));
    };
    let TypedValue::String(url) = unwrap_rc(url_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected string for URL".into(),
        ));
    };
    let TypedValue::Pair(headers_rc, body_method_rc) = unwrap_rc(inner_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected pair (headers, (body, (method, callback)))".into(),
        ));
    };
    let TypedValue::List(headers_list) = unwrap_rc(headers_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected list for headers".into(),
        ));
    };
    let headers: Vec<(String, String)> = headers_list
        .into_iter()
        .map(|item| {
            let TypedValue::Pair(name_rc, val_rc) = unwrap_rc(item) else {
                return Err(TransferError::GatewayError(
                    "http_call: expected pair (name, value) in headers list".into(),
                ));
            };
            let TypedValue::String(name) = unwrap_rc(name_rc) else {
                return Err(TransferError::GatewayError(
                    "http_call: expected string for header name".into(),
                ));
            };
            let TypedValue::String(val) = unwrap_rc(val_rc) else {
                return Err(TransferError::GatewayError(
                    "http_call: expected string for header value".into(),
                ));
            };
            Ok((name, val))
        })
        .collect::<Result<_, _>>()?;
    let TypedValue::Pair(body_rc, method_callback_rc) = unwrap_rc(body_method_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected pair (body, (method, callback))".into(),
        ));
    };
    let TypedValue::Bytes(body) = unwrap_rc(body_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected bytes for body".into(),
        ));
    };
    let TypedValue::Pair(method_rc, callback_rc) = unwrap_rc(method_callback_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected pair (method, callback)".into(),
        ));
    };
    let TypedValue::Nat(method) = unwrap_rc(method_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected nat for method".into(),
        ));
    };
    let callback = extract_callback(unwrap_rc(callback_rc), "call")?;
    let request = build_http_request(&url, &headers, &body, method)?;
    Ok((request, callback))
}

/// Build an `http::Request<Vec<u8>>` from extracted parameters.
fn build_http_request(
    url: &str,
    headers: &[(String, String)],
    body: &[u8],
    method: BigUint,
) -> Result<http::Request<Vec<u8>>, TransferError> {
    let http_method = match method.to_u64() {
        Some(0) => http::Method::GET,
        Some(1) => http::Method::POST,
        _ => http::Method::POST,
    };

    let mut builder = http::Request::builder().method(http_method).uri(url);
    for (name, value) in headers {
        // Zero-alloc case-insensitive prefix check (avoid the
        // `to_ascii_lowercase()` `String` allocation per header).
        let bytes = name.as_bytes();
        if bytes.len() >= 8 && bytes[..8].eq_ignore_ascii_case(b"x-tezos-") {
            return Err(TransferError::GatewayError(format!(
                "{ERR_FORBIDDEN_TEZOS_HEADER}: {name}"
            )));
        }
        builder = builder.header(name.as_str(), value.as_str());
    }

    builder.body(body.to_vec()).map_err(|e| {
        TransferError::GatewayError(format!(
            "http_call: failed to build HTTP request: {e}"
        ))
    })
}

/// Inject trusted X-Tezos-* context headers into `headers`, overwriting any
/// existing values with the same name.
///
/// `amount_mutez` is in mutez (10^-6 TEZ). It is formatted as a canonical
/// TEZ decimal string in the header (see L2-969).
/// `timestamp` is a Unix timestamp in seconds (must be non-negative).
#[allow(clippy::too_many_arguments)]
fn inject_context_headers_raw(
    headers: &mut http::HeaderMap,
    sender_alias: &str,
    source_alias: &str,
    source_runtime: RuntimeId,
    amount_mutez: u64,
    gas_limit: u64,
    timestamp: u64,
    block_number: u32,
    crac_id: &str,
    crac_depth: u32,
) -> Result<(), TransferError> {
    let parse_value = |v: &str| -> Result<http::HeaderValue, TransferError> {
        v.parse().map_err(|e| {
            TransferError::GatewayError(format!("invalid header value: {e}"))
        })
    };
    headers.insert(X_TEZOS_SENDER, parse_value(sender_alias)?);
    headers.insert(X_TEZOS_SOURCE, parse_value(source_alias)?);
    // Numeric `RuntimeId` tag, same encoding as the `originOf` /
    // `resolveAddress` ABI.
    headers.insert(
        X_TEZOS_SOURCE_RUNTIME,
        parse_value(&u8::from(source_runtime).to_string())?,
    );
    headers.insert(
        X_TEZOS_AMOUNT,
        parse_value(&format_tez_from_mutez(amount_mutez))?,
    );
    headers.insert(X_TEZOS_GAS_LIMIT, parse_value(&format!("{gas_limit}"))?);
    headers.insert(X_TEZOS_TIMESTAMP, parse_value(&format!("{timestamp}"))?);
    headers.insert(
        X_TEZOS_BLOCK_NUMBER,
        parse_value(&format!("{block_number}"))?,
    );
    headers.insert(X_TEZOS_CRAC_ID, parse_value(crac_id)?);
    headers.insert(
        tezosx_interfaces::X_TEZOS_CRAC_DEPTH,
        parse_value(&format!("{crac_depth}"))?,
    );
    Ok(())
}

/// Inject trusted X-Tezos-* context headers derived from `ctx` into `request`.
///
/// The target runtime is derived from the request URI host (e.g. `"ethereum"`,
/// `"tezos"`). Gas is converted from Tezos milligas to the target runtime's
/// units before being written to `X-Tezos-Gas-Limit`.
fn inject_context_headers<'a, Host>(
    mut request: http::Request<Vec<u8>>,
    ctx: &mut (impl CtxTrait<'a>
              + HasHost<Host>
              + HasOperationGas
              + HasSourcePublicKey
              + HasOriginLookup
              + HasCrossRuntime<Host>
              + HasCracChainDepth),
) -> Result<http::Request<Vec<u8>>, TransferError>
where
    Host: StorageV1,
{
    let target_host = request.uri().host().map(str::to_string);
    let target_runtime = target_host
        .as_deref()
        .and_then(RuntimeId::from_host)
        .ok_or_else(|| {
            TransferError::GatewayError(
                "http_call: unknown or missing target runtime in URL host".into(),
            )
        })?;
    let source_public_key = ctx.source_public_key().to_vec();
    let sender = ctx.sender();
    // Propagate the originator end-to-end so `tx.origin` stays invariant
    // across runtime hops. When this execution services an inbound CRAC,
    // forward the carried originator alias (`alias(E_0)`); otherwise (a
    // top-level Michelson tx) fall back to the operation source. The
    // originator alias resolves back through `RoutingDecision::RoundTrip`
    // to the real address on the target side, so an
    // `EVM -> Michelson -> EVM` round-trip preserves the EOA origin
    // instead of collapsing onto this runtime's null source (L2-1363).
    // Mirrors the EVM side's `origin = hdrs.source.unwrap_or(hdrs.sender)`.
    let source = match ctx.crac_origin() {
        Some(Contract::Originated(kt1)) => AddressHash::Kt1(kt1),
        Some(Contract::Implicit(pkh)) => AddressHash::Implicit(pkh),
        None => AddressHash::from(ctx.source()),
    };
    let amount_mutez: u64 = ctx
        .amount()
        .try_into()
        .map_err(|_| TransferError::GatewayError("Negative amount".into()))?;
    let block_number = ctx.level();
    let timestamp = ctx.now();
    let block_number_u32 = block_number
        .to_u32()
        .ok_or_else(|| TransferError::GatewayError("Block number out of range".into()))?;
    let timestamp_u64 = timestamp
        .to_i64()
        .ok_or_else(|| TransferError::GatewayError("Timestamp out of range".into()))
        .and_then(|t| {
            u64::try_from(t)
                .map_err(|_| TransferError::GatewayError("Negative timestamp".into()))
        })?;
    let context = cross_runtime_ctx_from_ctx(ctx)?;

    // Alias gas is accounted in two phases:
    //
    // Phase 1 (upfront): charge ALIAS_LOOKUP_MILLIGAS for the durable
    //   origin read that classifies the source. Always paid.
    //
    // Phase 2 (generation): forward remaining gas (converted to target
    //   runtime units) to the target runtime's ensure_alias, which
    //   consumes gas internally. Idempotent: a second call on the same
    //   source short-circuits and reports zero consumed.

    // --- sender alias ---
    ctx.operation_gas()
        .cast_and_consume_milligas(ALIAS_LOOKUP_MILLIGAS)
        .map_err(TransferError::OutOfGas)?;
    // When sender == source (the common case for implicit account
    // transfers), pass the source pubkey so the alias is created with
    // the credential attached.
    let sender_is_source = sender == source;
    // Each alias resolution also yields the address's *native* runtime —
    // `RoundTrip` resolves to an account native to the target runtime,
    // `Transitive` to the runtime recorded in its alias info, and `Native`
    // to Tezos (this runtime). The source's native runtime is forwarded as
    // `X-Tezos-Source-Runtime` so the EVM side reports a self-consistent
    // `(sourceRuntime, sourceAddress)` tuple.
    let (sender_alias, sender_runtime) = 'sender: {
        if target_runtime == RuntimeId::Tezos {
            // Short-circuit for Tezos target: the alias is the sender's
            // base58-encoded address. This covers both implicit and
            // originated accounts since both fit the same pattern.
            break 'sender (sender.to_base58_check(), RuntimeId::Tezos);
        }
        let alias_info = match read_and_resolve_routing(ctx, &sender, target_runtime)? {
            RoutingDecision::RoundTrip(target) => break 'sender (target, target_runtime),
            RoutingDecision::Transitive(info) => info,
            RoutingDecision::Native => AliasInfo {
                runtime: RuntimeId::Tezos,
                native_address: sender.to_base58_check().into_bytes(),
            },
        };
        let sender_runtime = alias_info.runtime;
        let remaining_milligas = ctx.gas().milligas().ok_or(OutOfGas)? as u64;
        // Convert remaining milligas to target runtime gas: this caps the budget
        // that ensure_alias may spend on alias generation.
        let target_budget =
            convert_gas(RuntimeId::Tezos, target_runtime, remaining_milligas)
                .ok_or(OutOfGas)?;
        let sender_pubkey: Option<&[u8]> = if sender_is_source {
            Some(&source_public_key)
        } else {
            None
        };
        let (sender_alias, sender_resolution) = {
            let (host, journal, registry) = ctx.cross_runtime_split();
            registry
                .ensure_alias(
                    host,
                    journal,
                    alias_info,
                    sender_pubkey,
                    target_runtime,
                    context.clone(),
                    target_budget,
                )
                .map_err(|e| TransferError::GatewayError(e.to_string()))?
        };
        // TODO(L2-1519): handle the sender_resolution storage cost delegated by the callee.
        let sender_target_consumed = target_budget - sender_resolution.gas_remaining;
        let sender_milligas =
            convert_gas(target_runtime, RuntimeId::Tezos, sender_target_consumed)
                .ok_or(OutOfGas)?;
        ctx.operation_gas()
            .cast_and_consume_milligas(sender_milligas)
            .map_err(TransferError::OutOfGas)?;
        (sender_alias, sender_runtime)
    };

    // --- source alias ---
    // Fast path: when sender == source (a user's implicit account calling
    // the gateway directly), reuse the resolved alias and skip the second
    // origin read. This saves one ALIAS_LOOKUP_MILLIGAS charge.
    let (source_alias, source_runtime) = if sender_is_source {
        (sender_alias.clone(), sender_runtime)
    } else {
        ctx.operation_gas()
            .cast_and_consume_milligas(ALIAS_LOOKUP_MILLIGAS)
            .map_err(TransferError::OutOfGas)?;
        'source: {
            if target_runtime == RuntimeId::Tezos {
                // Short-circuit for Tezos target: the alias is the source's
                // base58-encoded address.
                break 'source (source.to_base58_check(), RuntimeId::Tezos);
            }

            let alias_info = match read_and_resolve_routing(ctx, &source, target_runtime)?
            {
                RoutingDecision::RoundTrip(target) => {
                    break 'source (target, target_runtime)
                }
                RoutingDecision::Transitive(info) => info,
                RoutingDecision::Native => AliasInfo {
                    runtime: RuntimeId::Tezos,
                    native_address: source.to_base58_check().into_bytes(),
                },
            };
            let source_runtime = alias_info.runtime;
            let remaining_milligas = ctx.gas().milligas().ok_or(OutOfGas)? as u64;
            let target_budget =
                convert_gas(RuntimeId::Tezos, target_runtime, remaining_milligas)
                    .ok_or(OutOfGas)?;
            let (source_alias, source_resolution) = {
                let (host, journal, registry) = ctx.cross_runtime_split();
                registry
                    .ensure_alias(
                        host,
                        journal,
                        alias_info,
                        Some(&source_public_key),
                        target_runtime,
                        context,
                        target_budget,
                    )
                    .map_err(|e| TransferError::GatewayError(e.to_string()))?
            };
            // TODO(L2-1519): handle the source_resolution storage cost delegated by the callee.
            let source_target_consumed = target_budget - source_resolution.gas_remaining;
            let source_milligas =
                convert_gas(target_runtime, RuntimeId::Tezos, source_target_consumed)
                    .ok_or(OutOfGas)?;
            ctx.operation_gas()
                .cast_and_consume_milligas(source_milligas)
                .map_err(TransferError::OutOfGas)?;
            (source_alias, source_runtime)
        }
    };
    // Convert remaining Tezos milligas to the target runtime's units.
    // Use current remaining gas (not the pre-alias tezos_gas_limit) so the
    // forwarded limit reflects gas already consumed by alias resolution.
    let remaining_after_aliases = ctx.gas().milligas().ok_or(OutOfGas)? as u64;
    let gas_limit =
        convert_gas(RuntimeId::Tezos, target_runtime, remaining_after_aliases)
            .ok_or_else(|| {
                TransferError::GatewayError(
                    "http_call: Tezos gas limit overflows target runtime units".into(),
                )
            })?;
    let crac_depth = ctx.crac_chain_depth().saturating_add(1);
    let crac_id_str = ctx.journal().crac_id().to_string();
    inject_context_headers_raw(
        request.headers_mut(),
        &sender_alias,
        &source_alias,
        source_runtime,
        amount_mutez,
        gas_limit,
        timestamp_u64,
        block_number_u32,
        &crac_id_str,
        crac_depth,
    )?;
    Ok(request)
}

/// Resolve a source address to its target-runtime alias without writing
/// to storage (the read-only counterpart of `inject_context_headers`).
///
/// Returns the resolved alias together with the source address's *native*
/// runtime — the value the state-mutating path forwards as
/// `X-Tezos-Source-Runtime`. `RoundTrip` resolves to an account native to
/// the target runtime, `Transitive` to the runtime recorded in its alias
/// info, and `Native` (or a Tezos target) to Tezos (this runtime).
fn tezosx_resolve_source_alias_readonly(
    ctx: &impl HasOriginLookup,
    registry: &impl Registry,
    source: &AddressHash,
    target_runtime: RuntimeId,
) -> Result<(String, RuntimeId), TransferError> {
    if target_runtime == RuntimeId::Tezos {
        // Short-circuit for Tezos target: the alias is the source's
        // base58check address. This avoids the durable read and alias
        // computation for the common case of a Tezos contract calling
        // the Tezos gateway.
        return Ok((source.to_base58_check(), RuntimeId::Tezos));
    }

    let (native_bytes, source_runtime) =
        match read_and_resolve_routing(ctx, source, target_runtime)? {
            RoutingDecision::RoundTrip(target) => return Ok((target, target_runtime)),
            RoutingDecision::Transitive(info) => (info.native_address, info.runtime),
            RoutingDecision::Native => {
                (source.to_base58_check().into_bytes(), RuntimeId::Tezos)
            }
        };
    // Deterministic fallback: reproduce the alias that the target
    // runtime's `ensure_alias` would compute (and persist on the
    // state-mutating path), but skip the storage writes.
    //
    // Each branch duplicates the deterministic name-derivation
    // step from the corresponding runtime's `ensure_alias` — the
    // canonical implementation — and must stay in sync with it.
    // If either formula changes, this read-only path must change
    // too.
    let alias = registry
        .compute_alias(AliasInfo {
            runtime: target_runtime,
            native_address: native_bytes,
        })
        .map_err(|e| {
            TransferError::GatewayError(format!("Alias computation failed: {e}"))
        })?;
    Ok((alias, source_runtime))
}

/// Extract (evm_contract, address_bytes, value) from a typed
/// Pair(String, Pair(Bytes, Int)) value.
fn extract_erc20_address_uint256_params(
    typed: TypedValue<'_>,
) -> Result<(String, Vec<u8>, BigInt), TransferError> {
    let TypedValue::Pair(contract_rc, inner_rc) = typed else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected pair (contract, (address, amount))".into(),
        ));
    };
    let TypedValue::String(evm_contract) = unwrap_rc(contract_rc) else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected string for EVM contract address".into(),
        ));
    };
    let TypedValue::Pair(addr_rc, amount_rc) = unwrap_rc(inner_rc) else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected pair (address, amount)".into(),
        ));
    };
    let TypedValue::Bytes(addr_bytes) = unwrap_rc(addr_rc) else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected bytes for EVM address".into(),
        ));
    };
    let TypedValue::Int(amount) = unwrap_rc(amount_rc) else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected int for token amount".into(),
        ));
    };
    Ok((evm_contract, addr_bytes, amount))
}

/// ABI-encode a call to an ERC-20 function with signature `method_sig` and
/// arguments `(address, uint256)`. Returns the full calldata including the
/// 4-byte selector.
fn abi_encode_address_uint256(
    method_sig: &str,
    address_bytes: &[u8],
    value: &BigInt,
) -> Result<Vec<u8>, TransferError> {
    if address_bytes.len() > 20 {
        return Err(TransferError::GatewayError(
            "EVM address exceeds 20 bytes".into(),
        ));
    }
    let (sign, value_bytes) = value.to_bytes_be();
    if sign == num_bigint::Sign::Minus {
        return Err(TransferError::GatewayError(
            "Token amount must be non-negative".into(),
        ));
    }
    if value_bytes.len() > 32 {
        return Err(TransferError::GatewayError(
            "Token amount exceeds uint256".into(),
        ));
    }
    let selector = compute_selector(method_sig);
    let mut calldata = Vec::with_capacity(4 + 64);
    calldata.extend_from_slice(&selector);
    // ABI-encode address: left-pad to 32 bytes
    let addr_padding = 32 - address_bytes.len();
    calldata.extend_from_slice(&vec![0u8; addr_padding]);
    calldata.extend_from_slice(address_bytes);
    // ABI-encode uint256: left-pad to 32 bytes
    let val_padding = 32 - value_bytes.len();
    calldata.extend_from_slice(&vec![0u8; val_padding]);
    calldata.extend_from_slice(&value_bytes);
    Ok(calldata)
}

/// Compute the 4-byte Keccak256 function selector from a method signature.
fn compute_selector(method_signature: &str) -> [u8; 4] {
    let hash = Keccak256::digest(method_signature.as_bytes());
    [hash[0], hash[1], hash[2], hash[3]]
}

/// Read the source's classification record and delegate to the shared
/// `resolve_routing` helper, mapping its error variants into the
/// gateway envelope.
fn read_and_resolve_routing(
    ctx: &impl HasOriginLookup,
    address: &AddressHash,
    target_runtime: RuntimeId,
) -> Result<RoutingDecision, TransferError> {
    let origin = ctx
        .read_origin_for_address(address)
        .map_err(|e| TransferError::GatewayError(format!("read origin failed: {e}")))?;
    resolve_routing(origin, target_runtime)
        .map_err(|e| TransferError::GatewayError(e.to_string()))
}

/// Build a `CrossRuntimeContext` from the current execution context.
fn cross_runtime_ctx_from_ctx<'a, Host>(
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host>),
) -> Result<CrossRuntimeContext, TransferError>
where
    Host: StorageV1,
{
    // Remaining milligas is the gas budget for the cross-runtime call, in Tezos
    // milligas. `inject_context_headers` converts to the target runtime's units
    // on the way out.
    let gas_limit = ctx.gas().milligas().ok_or(OutOfGas)? as u64;
    Ok(CrossRuntimeContext {
        gas_limit,
        timestamp: bigint_to_u256(&ctx.now())?,
        block_number: biguint_to_u256(ctx.level())?,
    })
}

/// Build an HTTP POST request targeting the Ethereum runtime at `dest`
/// with `data` as the body.
fn build_ethereum_request(
    dest: &str,
    data: &[u8],
) -> Result<http::Request<Vec<u8>>, TransferError> {
    let url = format!("http://ethereum/{dest}");
    http::Request::builder()
        .method(http::Method::POST)
        .uri(&url)
        .body(data.to_vec())
        .map_err(|e| {
            TransferError::GatewayError(format!("Failed to build HTTP request: {e}"))
        })
}

/// Dispatch a CRAC call: inject context headers, serve the request,
/// classify the response, and debit the gateway balance.
///
/// `serve` is invoked synchronously from inside the gateway entrypoint,
/// which is what makes a CRAC sub-tree execute depth-first within the
/// caller's frame — equivalent to a same-runtime synchronous call (EVM
/// `CALL`) or a same-runtime DFS-expanded `TRANSFER_TOKENS` (Michelson
/// since Florence).
fn dispatch_crac_call<'a, Host>(
    ctx: &mut (impl CtxTrait<'a>
              + HasHost<Host>
              + HasContractAccount
              + HasOperationGas
              + HasSourcePublicKey
              + HasOriginLookup
              + HasCrossRuntime<Host>
              + HasCracChainDepth),
    request: http::Request<Vec<u8>>,
) -> Result<Vec<u8>, CracError>
where
    Host: StorageV1,
{
    if ctx.amount() < 0 {
        return Err(TransferError::GatewayError("Negative amount".into()).into());
    }

    let target_host = request.uri().host().map(str::to_string);
    let request = inject_context_headers(request, ctx)?;

    let response = {
        let (host, journal, registry) = ctx.cross_runtime_split();
        registry.serve(host, journal, request)
    };
    let response_body = classify_and_charge_crac_response(
        response,
        target_host.as_deref(),
        ctx.operation_gas(),
    )?;

    // Debit the gateway: after a successful call, the gateway
    // forwarded the funds to EVM so its balance should be reset to 0.
    // Skip both the durable write and the value-transfer surcharge
    // when the balance is already zero — no state change will occur.
    let account = ctx.contract_account().clone();
    let current = {
        let host = ctx.host();
        account
            .balance(host)
            .map_err(|_| TransferError::FailedToApplyBalanceChanges)?
    };
    if !current.0.is_zero() {
        ctx.operation_gas()
            .cast_and_consume_milligas(VALUE_TRANSFER_SURCHARGE_MILLIGAS)
            .map_err(TransferError::OutOfGas)?;
        let host = ctx.host();
        account
            .set_balance(host, &0u64.into())
            .map_err(|_| TransferError::FailedToApplyBalanceChanges)?;
    }
    Ok(response_body)
}

// ── originOf / resolveAddress gas constants (milligas) ───────────────────

/// Per-hop cost for the alias-derivation path in `resolveAddress`
/// (BLAKE2b-160 + base58check / hex encoding).
///
/// Tezos milligas mirror of the EVM-side `DERIVE_ALIAS_STRING_COST = 1_500`
/// (× `EVM_GAS_TO_MILLIGAS = 100`). Same notional cost on both runtimes so a
/// contract pays the same amount for the same derivation regardless of
/// which side it calls from. Like its EVM peer, conservative against the
/// actual hashing + encoding work — the value mirrors the EVM-side
/// category pin rather than measured derivation cost.
const DERIVE_ALIAS_MILLIGAS: u64 = 150_000;

// ── Resolution constants (Michelson nat encoding) ────────────────────────

/// The target alias is already materialized (its `/origin` record matches).
/// Encodes as nat 0 on the `resolveAddress` return.
const RESOLUTION_RECORDED_NAT: u64 = 0;

/// The target alias was derived but not yet written to storage.
/// Encodes as nat 1 on the `resolveAddress` return.
const RESOLUTION_DERIVED_NAT: u64 = 1;

/// Classify the origin of `addr_str` in `source_runtime` via
/// [`Registry::read_origin`], charging the gas that `read_origin` consumed.
///
/// Passes the available milligas budget (converted to `source_runtime`'s
/// native unit) to `read_origin`. The returned `consumed` value — which
/// covers the primary alias lookup and, for EVM sources, the code-presence
/// back-stop when it fires — is converted back to milligas and charged
/// to `operation_gas`. Callers do not pre-charge anything.
fn classify_origin_for_view<'a, Host, R>(
    host: &Host,
    registry: &R,
    operation_gas: &mut crate::gas::TezlinkOperationGas,
    source_runtime: RuntimeId,
    addr_str: &str,
) -> Result<Classification, mir::interpreter::InterpretError<'a>>
where
    Host: StorageV1,
    R: tezosx_interfaces::Registry,
{
    // Convert available milligas to source_runtime's native unit and pass as budget.
    let remaining_milligas = operation_gas
        .remaining
        .milligas()
        .ok_or(mir::interpreter::InterpretError::OutOfGas)?
        as u64;
    let budget = convert_gas(RuntimeId::Tezos, source_runtime, remaining_milligas)
        .ok_or(mir::interpreter::EnshrinedViewDispatchError::BudgetOverflow)?;

    let (classification, consumed) = registry
        .read_origin(host, source_runtime, addr_str, budget)
        .map_err(|_| mir::interpreter::EnshrinedViewDispatchError::AliasResolution)?;

    // Charge what read_origin consumed (alias lookup + back-stop when it fired).
    // Convert consumed back to milligas before charging.
    if consumed > 0 {
        let consumed_milligas =
            convert_gas(source_runtime, RuntimeId::Tezos, consumed)
                .ok_or(mir::interpreter::EnshrinedViewDispatchError::BudgetOverflow)?;
        operation_gas
            .cast_and_consume_milligas(consumed_milligas)
            .map_err(|_| mir::interpreter::InterpretError::OutOfGas)?;
    }

    Ok(classification)
}

/// Derive the target alias for a Native or foreign-target Alias source
/// and encode the Michelson `option (pair nat string)` result.
///
/// Charges `DERIVE_ALIAS_MILLIGAS` for the derivation and (transparently
/// via `classify_origin_for_view`) the inverse alias lookup on the
/// destination side. The `Recorded` upgrade fires when the destination
/// already has an Alias record pointing back to `source_runtime` with
/// the same `basis`; otherwise the result is `Derived`.
fn derive_alias_for_view<'a, Host, R>(
    host: &Host,
    registry: &R,
    operation_gas: &mut crate::gas::TezlinkOperationGas,
    source_runtime: RuntimeId,
    target_runtime: RuntimeId,
    basis: Vec<u8>,
) -> Result<TypedValue<'a>, mir::interpreter::InterpretError<'a>>
where
    Host: StorageV1,
    R: tezosx_interfaces::Registry,
{
    operation_gas
        .cast_and_consume_milligas(DERIVE_ALIAS_MILLIGAS)
        .map_err(|_| mir::interpreter::InterpretError::OutOfGas)?;
    let derived = registry
        .compute_alias(tezosx_interfaces::AliasInfo {
            runtime: target_runtime,
            native_address: basis.clone(),
        })
        .map_err(|_| mir::interpreter::EnshrinedViewDispatchError::AliasResolution)?;

    let inverse_class = classify_origin_for_view(
        host,
        registry,
        operation_gas,
        target_runtime,
        &derived,
    )?;

    let resolution_nat: u64 = match inverse_class {
        Classification::Alias(info_back)
            if info_back.runtime == source_runtime
                && info_back.native_address == basis =>
        {
            RESOLUTION_RECORDED_NAT
        }
        _ => RESOLUTION_DERIVED_NAT,
    };

    Ok(TypedValue::new_option(Some(TypedValue::new_pair(
        TypedValue::Nat(resolution_nat.into()),
        TypedValue::String(derived),
    ))))
}

/// Kernel-side logic for the `originOf` synthetic Michelson view on the
/// TezosXGateway.
///
/// Input: `(addr_str, source_runtime_nat)` — the caller's parsed Michelson
/// pair. Gas is consumed from `operation_gas`.
///
/// Returns a [`TypedValue`] of type `or unit (or nat (pair nat string))`:
/// - Unknown → `Left Unit`
/// - Native  → `Right (Left <source_runtime as nat>)`
/// - Alias   → `Right (Right (Pair (<home_runtime as nat>, <native_str>)))`
///
/// On an invalid runtime ID (`source_runtime` does not map to a known
/// [`RuntimeId`]), returns `Err(InterpretError::FailedWith(...))` with
/// the Michelson payload `(Pair "INVALID_RUNTIME_ID" received_nat)`.
pub fn dispatch_origin_of_get<'a, Host, R>(
    host: &Host,
    operation_gas: &mut crate::gas::TezlinkOperationGas,
    registry: &R,
    addr_str: &str,
    source_runtime_nat: &num_bigint::BigUint,
) -> Result<TypedValue<'a>, mir::interpreter::InterpretError<'a>>
where
    Host: StorageV1,
    R: tezosx_interfaces::Registry,
{
    // ── Runtime ID validation ────────────────────────────────────────────
    let source_runtime = runtime_id_from_nat(source_runtime_nat)?;

    // ── Origin lookup (back-stop included inside classify_origin_for_view) ──
    let classification = classify_origin_for_view(
        host,
        registry,
        operation_gas,
        source_runtime,
        addr_str,
    )?;

    // ── Encode result as `or unit (or nat (pair nat string))` ────────────
    encode_origin_of(classification, source_runtime)
}

/// Kernel-side logic for the `resolveAddress` synthetic Michelson view.
///
/// Input: `(addr_str, source_runtime_nat, target_runtime_nat)`.
///
/// Returns a [`TypedValue`] of type `option (pair nat string)`:
/// - Unknown source  → `None`
/// - Resolved target → `Some (Pair <resolution_nat> <translated_addr>)`
///   where `resolution_nat` is 0 (Recorded) or 1 (Derived).
pub fn dispatch_resolve_address_get<'a, Host, R>(
    host: &Host,
    operation_gas: &mut crate::gas::TezlinkOperationGas,
    registry: &R,
    addr_str: &str,
    source_runtime_nat: &num_bigint::BigUint,
    target_runtime_nat: &num_bigint::BigUint,
) -> Result<TypedValue<'a>, mir::interpreter::InterpretError<'a>>
where
    Host: StorageV1,
    R: tezosx_interfaces::Registry,
{
    // Reusing `EnshrinedViewDispatchError::AliasResolution` for all three
    // registry call sites (source read, compute, destination read). A more
    // granular variant would help traces, but each of these is a kernel-side
    // infra error caused by the same class of problem (registry can't dispatch
    // or storage layer failure), and the contract-facing semantics are
    // identical (operation reverts with 400). Splitting can land later if
    // observability needs it.

    // ── Runtime ID validation ────────────────────────────────────────────
    let source_runtime = runtime_id_from_nat(source_runtime_nat)?;
    let target_runtime = runtime_id_from_nat(target_runtime_nat)?;

    // ── Same-source short-circuit ────────────────────────────────────────
    // No storage reads needed.  A malformed address still returns None.
    if source_runtime == target_runtime {
        let valid = registry
            .address_from_string(addr_str, source_runtime)
            .is_ok();
        if !valid {
            return Ok(TypedValue::new_option(None));
        }
        return Ok(TypedValue::new_option(Some(TypedValue::new_pair(
            TypedValue::Nat(RESOLUTION_RECORDED_NAT.into()),
            TypedValue::String(addr_str.to_owned()),
        ))));
    }

    // ── Origin lookup (back-stop included inside classify_origin_for_view) ──
    let classification = classify_origin_for_view(
        host,
        registry,
        operation_gas,
        source_runtime,
        addr_str,
    )?;

    // ── Routing ─────────────────────────────────────────────────────────
    // Exhaustive over all three `Classification` arms so the compiler
    // enforces coverage rather than a catch-all + inner `unreachable!`.
    let result = match classification {
        Classification::Unknown => {
            // Unknown — cannot translate.
            TypedValue::new_option(None)
        }
        Classification::Alias(info) if info.runtime == target_runtime => {
            // Direct recorded lookup — target address is already in the record.
            // Strict UTF-8 per `AliasInfo` invariant; failure is data corruption.
            let native_str = info.into_native_address_string().map_err(|_| {
                mir::interpreter::EnshrinedViewDispatchError::AliasResolution
            })?;
            TypedValue::new_option(Some(TypedValue::new_pair(
                TypedValue::Nat(RESOLUTION_RECORDED_NAT.into()),
                TypedValue::String(native_str),
            )))
        }
        Classification::Native => {
            let basis: Vec<u8> = if source_runtime == RuntimeId::Ethereum {
                // Canonicalize EVM hex to lowercase so alias derivation is
                // casing-insensitive; mirrors the EVM peer in
                // `revm/src/precompiles/runtime_gateway.rs` and the
                // journal's native-source path.
                addr_str.to_lowercase().into_bytes()
            } else {
                addr_str.as_bytes().to_vec()
            };
            derive_alias_for_view(
                host,
                registry,
                operation_gas,
                source_runtime,
                target_runtime,
                basis,
            )?
        }
        Classification::Alias(info) => {
            // Foreign-target Alias: source's Alias points to a runtime that
            // is not the requested target. Vacuous in the 2-runtime MVP —
            // the only Alias the source can have points to `target_runtime`,
            // which the direct-lookup arm above already handled. Kept so
            // the derivation surface is correct in 3+ runtime futures.
            derive_alias_for_view(
                host,
                registry,
                operation_gas,
                source_runtime,
                target_runtime,
                info.native_address,
            )?
        }
    };

    Ok(result)
}

/// Convert a Michelson nat to a [`RuntimeId`], returning
/// `Err(InterpretError::FailedWith(...))` with a structured Michelson
/// payload if the nat does not map to a known runtime.
///
/// The FAILWITH payload is `(Pair "INVALID_RUNTIME_ID" received_nat)`,
/// causing the caller's operation to revert with that value on-stack —
/// standard Michelson FAILWITH semantics.
fn runtime_id_from_nat<'a>(
    nat: &num_bigint::BigUint,
) -> Result<RuntimeId, mir::interpreter::InterpretError<'a>> {
    use num_traits::ToPrimitive as _;
    let raw = nat
        .to_u64()
        .and_then(|n| u8::try_from(n).ok())
        .ok_or_else(|| make_invalid_runtime_id_error(nat))?;
    RuntimeId::try_from(raw).map_err(|_| make_invalid_runtime_id_error(nat))
}

/// Build the `InterpretError::FailedWith` payload for an unrecognised
/// runtime ID nat.  The Michelson type of the payload is
/// `pair string nat`, consistent with how the EVM precompile's
/// `InvalidRuntimeId(uint8)` error is displayed to callers.
fn make_invalid_runtime_id_error<'a>(
    received: &num_bigint::BigUint,
) -> mir::interpreter::InterpretError<'a> {
    mir::interpreter::InterpretError::FailedWith(
        mir::ast::Type::new_pair(mir::ast::Type::String, mir::ast::Type::Nat),
        TypedValue::new_pair(
            TypedValue::String("INVALID_RUNTIME_ID".to_owned()),
            TypedValue::Nat(received.clone()),
        ),
    )
}

/// Encode an origin classification as the Michelson type
/// `or unit (or nat (pair nat string))`.
///
/// Fallible on the alias arm: `AliasInfo::into_native_address_string`
/// enforces the UTF-8 invariant; failure surfaces as
/// [`EnshrinedViewDispatchError::AliasResolution`].
fn encode_origin_of<'a>(
    classification: Classification,
    source_runtime: RuntimeId,
) -> Result<TypedValue<'a>, mir::interpreter::InterpretError<'a>> {
    use mir::ast::or::Or;
    Ok(match classification {
        Classification::Unknown => {
            // Unknown → Left Unit
            TypedValue::new_or(Or::Left(TypedValue::Unit))
        }
        Classification::Native => {
            // Native → Right (Left <source_runtime as nat>)
            let runtime_nat = TypedValue::Nat((u8::from(source_runtime) as u64).into());
            TypedValue::new_or(Or::Right(TypedValue::new_or(Or::Left(runtime_nat))))
        }
        Classification::Alias(info) => {
            // Alias → Right (Right (Pair (<home_runtime as nat>, <native_str>)))
            let home_nat = TypedValue::Nat((u8::from(info.runtime) as u64).into());
            let native_str =
                TypedValue::String(info.into_native_address_string().map_err(|_| {
                    mir::interpreter::EnshrinedViewDispatchError::AliasResolution
                })?);
            TypedValue::new_or(Or::Right(TypedValue::new_or(Or::Right(
                TypedValue::new_pair(home_nat, native_str),
            ))))
        }
    })
}

/// HTTP `429 TOO_MANY_REQUESTS` is the cross-runtime out-of-gas
/// sentinel: the EVM runtime's `build_response` maps
/// `HaltReason::OutOfGas` (and its own `TezosXRuntimeError::OutOfGas`)
/// to `429` *specifically* so the dispatching Michelson side can tell
/// gas exhaustion apart from the other 4xx outcomes it would otherwise
/// fold into a generic operation failure or `Ok(None)` (L2-1457). Both
/// the state-mutating call path ([`classify_and_charge_crac_response`])
/// and the read-only view path ([`dispatch_staticcall_evm_get`]) must
/// surface it as an out-of-gas failure so a forwarded-gas exhaustion is
/// never confused with an absent view or a plain contract rejection.
fn is_cross_runtime_oog(status: http::StatusCode) -> bool {
    status == http::StatusCode::TOO_MANY_REQUESTS
}

/// Read-only counterpart to [`dispatch_crac_call`], shaped for the
/// view-side bridge that backs the gateway's `staticcall_evm`
/// synthetic view: build a `GET http://ethereum/<destination>`,
/// inject the same `X-Tezos-*` context headers `dispatch_crac_call`
/// would, fire `registry.serve`, charge `X-Tezos-Gas-Consumed` back
/// to `operation_gas`, and classify the response.
///
/// `X-Tezos-Sender` (→ the callee's `msg.sender`) is always the
/// immediate Michelson caller's alias (`calling_kt1`), resolved with one
/// durable origin read.
///
/// `X-Tezos-Source` (→ the callee's `tx.origin`) and its
/// `X-Tezos-Source-Runtime` are the transitive top-level originator and
/// its native runtime. When this view services an inbound cross-runtime
/// call, the EVM gateway that entered it already captured the
/// originator's complete native `(runtime, address)` identity on the
/// shared journal ([`TezosXJournal::original_source`]); we forward that
/// directly. A nested `EVM → callMichelsonView → VIEW → staticcall_evm →
/// EVM` round-trip therefore preserves the outer EVM originator without
/// consulting any durable alias/origin record — correct even when that
/// originator's alias was never materialized. For a top-level Michelson
/// view (no captured originator) the source is the operation source,
/// resolved here as its own durable origin read.
///
/// Metering: the sender always costs one `ALIAS_LOOKUP_MILLIGAS`. The
/// source costs a second one only in the top-level-view case, where it is
/// resolved here; reading the captured originator from the shared journal
/// performs no durable read and is free.
///
/// - **2xx** → `Ok(Some(body))`.
/// - **429** → `Err(InterpretError::OutOfGas)`: the cross-runtime
///   out-of-gas sentinel (see [`is_cross_runtime_oog`]) must fail
///   closed rather than be confused with an absent view, otherwise an
///   EVM target that exhausts the forwarded view gas drives the
///   Michelson caller down the `IF_NONE` branch.
/// - **other 4xx** → `Ok(None)`, mirroring the standard MIR view
///   dispatch's "view not found" outcome so the Michelson caller can
///   `IF_NONE`.
/// - **5xx / 1xx / 3xx** →
///   `Err(EnshrinedViewDispatchError::UnclassifiableResponse)`. The
///   EVM runtime's `serve()` only ever emits 200/400/429/500-class
///   statuses today, so 1xx/3xx are unreachable; the typed variant
///   surfaces the status code if that ever changes.
///
/// Internal failures (alias resolution, gas-unit overflow,
/// request-build) surface as typed
/// [`EnshrinedViewDispatchError`](mir::interpreter::EnshrinedViewDispatchError)
/// variants; the caller (`view.rs::classify_interpret_error`) routes
/// them to `TezosXRuntimeError::Custom` (→ 500) so they don't
/// masquerade as caller-side `BadRequest`s.
///
/// The inner EVM gas limit is derived from `operation_gas.remaining`
/// at call time, so it scales with the outer view's remaining
/// budget rather than a hardcoded magic.
#[allow(clippy::too_many_arguments)]
pub fn dispatch_staticcall_evm_get<'a, Host, R, C>(
    host: &mut Host,
    operation_gas: &mut crate::gas::TezlinkOperationGas,
    registry: &R,
    journal: &mut TezosXJournal,
    context: &C,
    calling_kt1: &ContractKt1Hash,
    source: &PublicKeyHash,
    crac_id: &str,
    timestamp: &str,
    block_number: &str,
    crac_chain_depth: u32,
    destination: &str,
    calldata: &[u8],
) -> Result<Option<Vec<u8>>, mir::interpreter::InterpretError<'a>>
where
    Host: StorageV1,
    R: Registry,
    C: crate::context::Context,
{
    // Minimal `HasOriginLookup` adapter for the read-only alias
    // resolution path: needs an immutable host plus the context, no
    // operation gas / no source public key. Defined locally so the
    // helper has no extra preconditions on its callers.
    struct ViewOriginLookup<'a, H, C> {
        host: &'a H,
        context: &'a C,
    }
    impl<H: StorageV1, C: crate::context::Context> HasOriginLookup
        for ViewOriginLookup<'_, H, C>
    {
        fn read_origin_for_address(
            &self,
            address: &AddressHash,
        ) -> Result<Option<tezosx_interfaces::Origin>, tezos_storage::error::Error>
        {
            self.context.read_origin_for_address(self.host, address)
        }
    }

    // The captured top-level originator, shared by both runtimes (set by
    // the EVM gateway that entered this view, if any). Read it up front so
    // it does not collide with the mutable journal borrow taken to dispatch
    // the request below.
    let original_source = journal.original_source().cloned();

    // `X-Tezos-Sender` → the inner EVM callee's `msg.sender`: always the
    // immediate Michelson caller's alias. This is a durable origin read,
    // metered like the state-mutating `inject_context_headers` meters its
    // reads (`ALIAS_LOOKUP_MILLIGAS`); the read-only path skips only the
    // alias *generation* (write) cost. The target is always Ethereum here,
    // so the read always happens (no Tezos short-circuit to skip).
    let lookup = ViewOriginLookup {
        host: &*host,
        context,
    };
    operation_gas
        .cast_and_consume_milligas(ALIAS_LOOKUP_MILLIGAS)
        .map_err(|_| mir::interpreter::InterpretError::OutOfGas)?;
    let (sender_addr_hex, _sender_runtime) = tezosx_resolve_source_alias_readonly(
        &lookup,
        registry,
        &AddressHash::Kt1(calling_kt1.clone()),
        RuntimeId::Ethereum,
    )
    .map_err(|_| mir::interpreter::EnshrinedViewDispatchError::AliasResolution)?;

    // `X-Tezos-Source` → the inner EVM callee's `tx.origin`: the transitive
    // top-level originator. When this view services an inbound
    // cross-runtime call, the entering EVM gateway captured the
    // originator's complete native `(runtime, address)` identity on the
    // shared journal, so forward it directly — no durable read, and
    // correct even when the originator's durable alias was never
    // materialized. Otherwise (a top-level Michelson view) the originator
    // is the operation source, resolved here as its own durable origin read.
    let (source_addr_hex, source_runtime) = match original_source {
        // Translate the captured originator's native address into its EVM
        // representation (identity when Ethereum-native, a pure
        // `compute_alias` otherwise) — no durable read, matching the
        // free-of-charge metering below.
        Some(src) => (
            translate_original_source(registry, &src, RuntimeId::Ethereum).map_err(
                |_| mir::interpreter::EnshrinedViewDispatchError::AliasResolution,
            )?,
            src.runtime(),
        ),
        None => {
            operation_gas
                .cast_and_consume_milligas(ALIAS_LOOKUP_MILLIGAS)
                .map_err(|_| mir::interpreter::InterpretError::OutOfGas)?;
            tezosx_resolve_source_alias_readonly(
                &lookup,
                registry,
                &AddressHash::Implicit(source.clone()),
                RuntimeId::Ethereum,
            )
            .map_err(|_| mir::interpreter::EnshrinedViewDispatchError::AliasResolution)?
        }
    };

    // Inner EVM gas budget derived from the view's remaining
    // milligas. Out-of-gas surfaces as `OutOfGas`; a conversion
    // failure (unrepresentable in EVM units) surfaces as
    // `BudgetOverflow`. Neither is silently zeroed — silent zero
    // would mask exhaustion as a "view target said no" 4xx.
    let remaining_milligas = operation_gas
        .remaining
        .milligas()
        .ok_or(mir::interpreter::InterpretError::OutOfGas)?
        as u64;
    let evm_gas_limit = tezosx_interfaces::gas::convert(
        RuntimeId::Tezos,
        RuntimeId::Ethereum,
        remaining_milligas,
    )
    .ok_or(mir::interpreter::EnshrinedViewDispatchError::BudgetOverflow)?;

    // Parse the URI up-front so a caller-supplied bad destination
    // surfaces as `InvalidDestination` (→ 400) rather than getting
    // lumped into the kernel-side `DispatchSetup` (→ 500). Michelson
    // `string` allows characters that `http::Uri` rejects
    // (e.g. spaces), so this path is reachable from caller input.
    let url = format!("http://ethereum/{destination}");
    let uri: http::Uri = url.parse().map_err(|_| {
        mir::interpreter::EnshrinedViewDispatchError::InvalidDestination {
            destination: destination.to_string(),
        }
    })?;
    let crac_depth = crac_chain_depth.saturating_add(1);
    let request = http::Request::builder()
        .method(http::Method::GET)
        .uri(uri)
        .header(tezosx_interfaces::X_TEZOS_SENDER, sender_addr_hex.as_str())
        // `source_addr_hex` is the captured top-level originator when one
        // exists, else the operation source (see its derivation above).
        // This keeps the immediate caller (`X-Tezos-Sender`) distinct from
        // the transitive originator (`X-Tezos-Source`) so `tx.origin`
        // survives a nested cross-runtime round-trip.
        .header(tezosx_interfaces::X_TEZOS_SOURCE, source_addr_hex.as_str())
        // Native runtime of `X-Tezos-Source`, so the EVM side reports a
        // self-consistent `(sourceRuntime, sourceAddress)` tuple instead
        // of defaulting absent → Tezos, matching the state-mutating
        // header injection.
        .header(
            tezosx_interfaces::X_TEZOS_SOURCE_RUNTIME,
            u8::from(source_runtime).to_string(),
        )
        .header(tezosx_interfaces::X_TEZOS_AMOUNT, "0")
        .header(
            tezosx_interfaces::X_TEZOS_GAS_LIMIT,
            evm_gas_limit.to_string(),
        )
        .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, timestamp)
        .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, block_number)
        .header(tezosx_interfaces::X_TEZOS_CRAC_ID, crac_id)
        .header(
            tezosx_interfaces::X_TEZOS_CRAC_DEPTH,
            crac_depth.to_string(),
        )
        .body(calldata.to_vec())
        .map_err(|_| mir::interpreter::EnshrinedViewDispatchError::DispatchSetup)?;

    let response = registry.serve(host, journal, request);
    let status = response.status().as_u16();

    // Charge the inner EVM execution cost back to the operation gas —
    // `X-Tezos-Gas-Consumed` is mandatory on 2xx and best-effort on
    // 4xx; mirrors `classify_and_charge_crac_response`.
    if let Ok(consumed_milligas) = extract_gas_consumed(&response, Some("ethereum")) {
        operation_gas
            .cast_and_consume_milligas(consumed_milligas)
            .map_err(|_| mir::interpreter::InterpretError::OutOfGas)?;
    }

    if (200..300).contains(&status) {
        Ok(Some(response.into_body()))
    } else if is_cross_runtime_oog(response.status()) {
        Err(mir::interpreter::InterpretError::OutOfGas)
    } else if (400..500).contains(&status) {
        Ok(None)
    } else {
        Err(
            mir::interpreter::EnshrinedViewDispatchError::UnclassifiableResponse {
                status,
            }
            .into(),
        )
    }
}

fn bigint_to_u256(value: &num_bigint::BigInt) -> Result<U256, TransferError> {
    let (_, bytes) = value.to_bytes_le();
    if bytes.len() > 32 {
        return Err(TransferError::GatewayError(
            "Failed to convert to U256".into(),
        ));
    }
    Ok(U256::from_little_endian(&bytes))
}

fn biguint_to_u256(value: num_bigint::BigUint) -> Result<U256, TransferError> {
    let bytes = value.to_bytes_le();
    if bytes.len() > 32 {
        return Err(TransferError::GatewayError(
            "Failed to convert to U256".into(),
        ));
    }
    Ok(U256::from_little_endian(&bytes))
}

/// Classify a CRAC HTTP response and charge gas.
///
/// Gas is charged on a best-effort basis for all statuses, on block
/// aborts the block is reverted anyway, so a missing header is harmless.
/// On 2xx the header is mandatory.
///
/// `429` is the cross-runtime out-of-gas sentinel (see
/// [`is_cross_runtime_oog`]) and maps to a catchable
/// `CracError::Operation(TransferError::OutOfGas)` rather than the
/// generic client-error envelope, so a forwarded-gas exhaustion stays
/// a typed out-of-gas failure (L2-1457). Every other 4xx is a generic
/// operation-level failure; 5xx aborts the block.
fn classify_and_charge_crac_response(
    response: http::Response<Vec<u8>>,
    target_host: Option<&str>,
    operation_gas: &mut crate::gas::TezlinkOperationGas,
) -> Result<Vec<u8>, CracError> {
    let callee_gas = extract_gas_consumed(&response, target_host).ok();

    if let Some(milligas) = callee_gas {
        operation_gas
            .cast_and_consume_milligas(milligas)
            .map_err(TransferError::OutOfGas)?;
    }

    if response.status().is_success() {
        if callee_gas.is_none() {
            return Err(TransferError::GatewayError(
                "http_call: missing or invalid X-Tezos-Gas-Consumed header in response"
                    .into(),
            )
            .into());
        }
        // TODO(L2-1519): handle the storage cost delegated by the callee.
        let storage_cost = parse_u64_opt(response.headers(), X_TEZOS_STORAGE_COST)
            .map_err(|e| {
                CracError::Operation(TransferError::GatewayError(e.to_string()))
            })?;
        if let Some(v) = storage_cost {
            return Err(CracError::BlockAbort(format!(
                "{X_TEZOS_STORAGE_COST} not yet supported on the Michelson caller (got {v})"
            )));
        }
        Ok(response.into_body())
    } else if is_cross_runtime_oog(response.status()) {
        Err(CracError::Operation(TransferError::OutOfGas(OutOfGas)))
    } else if response.status().is_client_error() {
        // Charge for the attacker-controlled 4xx body the way it is
        // persisted in the failed CRAC receipt: `from_utf8_lossy` can
        // expand invalid bytes, so charge for the decoded length, not the
        // raw body length. Gas then bounds the persisted metadata just
        // like any FAILWITH payload.
        let status = response.status();
        let body = String::from_utf8_lossy(response.body()).into_owned();
        charge_gateway_payload_gas(operation_gas, body.len())?;
        Err(CracError::Operation(TransferError::GatewayError(format!(
            "Cross-runtime call failed with status {status}: {body}"
        ))))
    } else {
        Err(CracError::BlockAbort(format!(
            "Cross-runtime call returned status {}: {}",
            response.status(),
            String::from_utf8_lossy(response.body())
        )))
    }
}

/// Extract the gas consumed from an HTTP response's `X-Tezos-Gas-Consumed`
/// header and convert it from the target runtime's units to Tezos milligas.
fn extract_gas_consumed(
    response: &http::Response<Vec<u8>>,
    target_host: Option<&str>,
) -> Result<u64, TransferError> {
    let target_runtime = target_host.and_then(RuntimeId::from_host).ok_or_else(|| {
        TransferError::GatewayError(
            "http_call: unknown or missing target runtime in URL host".into(),
        )
    })?;
    let consumed_in_target_units = response
        .headers()
        .get(X_TEZOS_GAS_CONSUMED)
        .and_then(|v| v.to_str().ok())
        .and_then(|s| s.parse::<u64>().ok())
        .ok_or_else(|| {
            TransferError::GatewayError(
                "http_call: missing or invalid X-Tezos-Gas-Consumed header in response"
                    .into(),
            )
        })?;
    convert_gas(target_runtime, RuntimeId::Tezos, consumed_in_target_units).ok_or_else(
        || {
            TransferError::GatewayError(
                "http_call: gas consumed overflows Tezos milligas".into(),
            )
        },
    )
}

pub(crate) fn get_enshrined_contract_entrypoint(
    contract: EnshrinedContracts,
) -> Option<HashMap<Entrypoint, Type>> {
    match contract {
        EnshrinedContracts::TezosXGateway => {
            let mut entrypoints = HashMap::new();
            // %call_evm: pair string (pair string (pair bytes (option (contract bytes))))
            //   (destination, (method_signature, (abi_parameters, callback)))
            entrypoints.insert(
                Entrypoint::try_from("call_evm").ok()?,
                Type::new_pair(
                    Type::String,
                    Type::new_pair(
                        Type::String,
                        Type::new_pair(
                            Type::Bytes,
                            Type::new_option(Type::new_contract(Type::Bytes)),
                        ),
                    ),
                ),
            );
            // %call: pair string (pair (list (pair string string)) (pair bytes (pair nat (option (contract bytes)))))
            //   (url, (headers, (body, (method, callback))))
            entrypoints.insert(
                Entrypoint::try_from("call").ok()?,
                Type::new_pair(
                    Type::String,
                    Type::new_pair(
                        Type::new_list(Type::new_pair(Type::String, Type::String)),
                        Type::new_pair(
                            Type::Bytes,
                            Type::new_pair(
                                Type::Nat,
                                Type::new_option(Type::new_contract(Type::Bytes)),
                            ),
                        ),
                    ),
                ),
            );
            // %collect_result: bytes
            //   Raw bytes payload to return to the EVM caller via the
            //   current dispatch slot; deposited by
            //   `set_dispatch_result` in the entrypoint handler and
            //   surfaced by `serve` as the HTTP response body.
            entrypoints.insert(Entrypoint::try_from("collect_result").ok()?, Type::Bytes);
            Some(entrypoints)
        }
        EnshrinedContracts::ERC20Wrapper => {
            let mut entrypoints = HashMap::new();
            // %transfer: pair string (pair bytes int)
            //   (evm_contract, (recipient_address, amount))
            entrypoints.insert(
                Entrypoint::try_from("transfer").ok()?,
                Type::new_pair(Type::String, Type::new_pair(Type::Bytes, Type::Int)),
            );
            // %approve: pair string (pair bytes int)
            //   (evm_contract, (spender_address, amount))
            entrypoints.insert(
                Entrypoint::try_from("approve").ok()?,
                Type::new_pair(Type::String, Type::new_pair(Type::Bytes, Type::Int)),
            );
            Some(entrypoints)
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use mir::ast::{AddressHash, Micheline};
    use mir::gas::Gas;
    use mir::lexer::Prim;
    use num_bigint::BigInt;
    use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_tezlink::operation_result::{ContentResult, InternalOperationSum};
    use tezosx_interfaces::{Origin, RuntimeId};
    use tezosx_journal::{DispatchSlotError, OriginalSource, TezosXJournal};

    use super::*;
    use crate::mir_ctx::mock::MockCtx;
    use crate::test_utils::MockRegistry;

    const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";
    const ERC20_WRAPPER_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVKvCChb";

    use tezosx_journal::CracId;

    /// Build a CRAC event internal operation (test helper).
    fn make_crac_event(
        gateway_kt1: &ContractKt1Hash,
        crac_id: &CracId,
    ) -> InternalOperationSum {
        use mir::ast::annotations::NO_ANNS;
        use mir::lexer;
        use tezos_protocol::contract::Contract;
        use tezos_tezlink::operation_result::{
            EventContent, EventSuccess, InternalContentWithMetadata,
        };

        let ty = Micheline::App(lexer::Prim::string, &[], NO_ANNS)
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let payload = Micheline::from(crac_id.to_string())
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        InternalOperationSum::Event(InternalContentWithMetadata {
            content: EventContent {
                tag: Some(mir::ast::Entrypoint::from_string_unchecked(
                    "cross_runtime_call".into(),
                )),
                payload: Some(payload.into()),
                ty: ty.into(),
            },
            sender: Contract::Originated(gateway_kt1.clone()),
            nonce: 0,
            result: ContentResult::Applied(EventSuccess {
                consumed_milligas: tezos_data_encoding::types::Narith(0u64.into()),
            }),
        })
    }

    #[test]
    fn test_gateway() {
        let contract = ContractKt1Hash::try_from_bytes(&GATEWAY_ADDRESS).unwrap();
        assert![is_enshrined(&contract)];
        assert![contract.to_base58_check().as_str() == GATEWAY_KT1];
        assert![from_kt1(&contract) == Some(EnshrinedContracts::TezosXGateway)];
    }

    #[test]
    fn test_erc20_wrapper() {
        let contract = ContractKt1Hash::try_from_bytes(&ERC20_WRAPPER_ADDRESS).unwrap();
        assert![is_enshrined(&contract)];
        assert![contract.to_base58_check().as_str() == ERC20_WRAPPER_KT1];
        assert![from_kt1(&contract) == Some(EnshrinedContracts::ERC20Wrapper)];
    }

    #[test]
    fn test_compute_selector() {
        // keccak256("transfer(address,uint256)") starts with 0xa9059cbb
        let selector = compute_selector("transfer(address,uint256)");
        assert_eq!(selector, [0xa9, 0x05, 0x9c, 0xbb]);
    }

    #[test]
    fn test_dispatch_crac_call_passes_calldata() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let method_sig = "transfer(address,uint256)";
        let abi_params = vec![0xAA, 0xBB, 0xCC, 0xDD];
        let amount = 500i64;

        let selector = compute_selector(method_sig);
        let mut calldata = Vec::with_capacity(4 + abi_params.len());
        calldata.extend_from_slice(&selector);
        calldata.extend_from_slice(&abi_params);

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);
        let result = dispatch_crac_call(
            &mut ctx,
            build_ethereum_request(dest, &calldata).unwrap(),
        );
        assert!(result.is_ok());

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 1);
        assert_eq!(
            serve_calls[0].uri().to_string(),
            format!("http://ethereum/{dest}")
        );
        assert_eq!(serve_calls[0].body(), &calldata);
        assert_eq!(
            serve_calls[0].headers().get(X_TEZOS_SENDER).unwrap(),
            "KT1_mock_alias"
        );
        assert_eq!(
            serve_calls[0].headers().get(X_TEZOS_SOURCE).unwrap(),
            "KT1_mock_alias"
        );
        assert_eq!(
            serve_calls[0].headers().get(X_TEZOS_AMOUNT).unwrap(),
            "0.0005"
        );
    }

    /// L2-1363: when servicing an inbound CRAC, the gateway forwards the
    /// carried originator alias (`crac_origin`) as the outbound source —
    /// not this runtime's null operation source — so `tx.origin` stays
    /// invariant across an `EVM -> Michelson -> EVM` round-trip. Here the
    /// mock has no classification record, so the originator resolves
    /// `Native` and we assert the source-side `ensure_alias` call carries
    /// the originator KT1's own address (rather than `MockCtx::source()`,
    /// the null `tz1KqTp...`). In production that originator alias is an
    /// EVM alias and resolves back to the EOA via
    /// `RoutingDecision::RoundTrip` (see
    /// `routing_returns_round_trip_for_matching_alias` and the
    /// TezosX-context classification tests). Reverting the fix forwards
    /// the null operation source, failing the assertion.
    #[test]
    fn test_dispatch_crac_call_forwards_crac_origin_as_source() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        // Immediate caller (sender), distinct from the originator.
        let sender = AddressHash::Kt1(ContractKt1Hash::from([0xAA; 20]));
        // Originator alias carried on the inbound CRAC (`alias(E_0)`): a KT1.
        let crac_origin_kt1 = ContractKt1Hash::from([0x42; 20]);

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, sender, 0);
        ctx.crac_origin = Some(tezos_protocol::contract::Contract::Originated(
            crac_origin_kt1.clone(),
        ));

        let dest = "0x1234567890123456789012345678901234567890";
        dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap()).unwrap();

        let expected_source = AddressHash::Kt1(crac_origin_kt1.clone()).to_base58_check();
        let native_addrs: Vec<String> = registry
            .ensure_alias_calls
            .borrow()
            .iter()
            .map(|(info, _)| String::from_utf8(info.native_address.clone()).unwrap())
            .collect();
        assert!(
            native_addrs.contains(&expected_source),
            "source alias must be derived from crac_origin {expected_source}, got {native_addrs:?}"
        );
        assert!(
            !native_addrs.iter().any(|a| a.starts_with("tz1KqTp")),
            "the null operation source must not be forwarded when crac_origin is set: {native_addrs:?}"
        );
    }

    #[test]
    fn test_tezosx_transfer_creates_alias_when_absent() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        // tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb as AddressHash
        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 1000u64;

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx =
            MockCtx::new(&mut host, &mut journal, &registry, source, amount as i64);
        let result =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result.is_ok());

        // Verify ensure_alias was called for both sender and source
        let alias_calls = registry.ensure_alias_calls.borrow();
        assert_eq!(alias_calls.len(), 2);
        assert_eq!(alias_calls[0].1, RuntimeId::Ethereum);
        assert_eq!(alias_calls[1].1, RuntimeId::Ethereum);

        // Verify serve was called with correct URL and headers
        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 1);
        assert_eq!(
            serve_calls[0].uri().to_string(),
            format!("http://ethereum/{dest}")
        );
        assert_eq!(serve_calls[0].method(), http::Method::POST);
    }

    #[test]
    fn test_tezosx_transfer_calls_ensure_alias_per_transfer() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 1000i64;

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);

        // First transfer creates aliases (sender + source)
        let result1 =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result1.is_ok());

        // Second transfer calls ensure_alias again. Dedup is the
        // responsibility of ensure_alias itself (Branch 1: returns
        // early when the alias account is already classified) — it
        // is no longer the resolver's concern.
        let result2 =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result2.is_ok());

        let alias_calls = registry.ensure_alias_calls.borrow();
        assert_eq!(alias_calls.len(), 4);

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 2);
    }

    #[test]
    fn test_alias_generation_consumes_gas() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 500i64;

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);

        let gas_before = ctx.operation_gas().remaining.milligas().unwrap();
        let result =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result.is_ok());
        let gas_after = ctx.operation_gas().remaining.milligas().unwrap();

        // Gas should have been consumed for alias generation (2 aliases).
        // Each new alias costs EVM_ALIAS_GENERATION_MILLIGAS.
        assert!(gas_before > gas_after);
        let consumed = u64::from(gas_before - gas_after);
        // At minimum, 2 alias resolutions were charged
        assert!(
            consumed >= 2 * ALIAS_LOOKUP_MILLIGAS,
            "Expected at least {} milligas consumed for 2 alias generations, got {}",
            2 * ALIAS_LOOKUP_MILLIGAS,
            consumed
        );
    }

    #[test]
    fn test_alias_lookup_cost_caps_per_transfer() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 500i64;

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);

        let result1 =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result1.is_ok());
        let gas_after_first = ctx.operation_gas().remaining.milligas().unwrap();

        // Second call: ensure_alias is invoked again but its idempotent
        // Branch 1 returns gas_remaining unchanged. The fixed cost
        // bounds the per-transfer overhead.
        let result2 =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result2.is_ok());
        let gas_after_second = ctx.operation_gas().remaining.milligas().unwrap();

        let second_call_cost = u64::from(gas_after_first - gas_after_second);
        let expected_max = 2 * ALIAS_LOOKUP_MILLIGAS + VALUE_TRANSFER_SURCHARGE_MILLIGAS;
        assert!(
            second_call_cost <= expected_max,
            "Second call consumed {second_call_cost} milligas, expected at most \
             {expected_max} (two alias lookups plus value transfer surcharge)",
        );
    }

    /// Helper to build a Micheline call value:
    /// Pair(String(url), Pair(Seq(headers), Pair(Bytes(body), Int(method))))
    pub(crate) fn build_http_call_micheline<'a>(
        arena: &'a typed_arena::Arena<Micheline<'a>>,
        url: &str,
        headers: &[(&str, &str)],
        body: &[u8],
        method: i64,
    ) -> Micheline<'a> {
        let mut gas = Gas::default();
        let header_pairs: Vec<Micheline<'a>> = headers
            .iter()
            .map(|(name, value)| {
                Micheline::prim2(
                    arena,
                    Prim::Pair,
                    name.to_string().into(),
                    value.to_string().into(),
                    &mut gas,
                )
                .unwrap()
            })
            .collect();
        let headers_seq = Micheline::Seq(arena.alloc_extend(header_pairs));
        let method_callback = Micheline::prim2(
            arena,
            Prim::Pair,
            num_bigint::BigInt::from(method).into(),
            Micheline::prim0(Prim::None, &mut gas).unwrap(),
            &mut gas,
        )
        .unwrap();
        let body_method_callback = Micheline::prim2(
            arena,
            Prim::Pair,
            body.to_vec().into(),
            method_callback,
            &mut gas,
        )
        .unwrap();
        let inner_pair = Micheline::prim2(
            arena,
            Prim::Pair,
            headers_seq,
            body_method_callback,
            &mut gas,
        )
        .unwrap();
        Micheline::prim2(
            arena,
            Prim::Pair,
            url.to_string().into(),
            inner_pair,
            &mut gas,
        )
        .unwrap()
    }

    /// Typecheck a Micheline call value into a TypedValue.
    ///
    /// Owns its own journal/registry — typechecking only needs the
    /// trait bounds, not the values, so they're scoped to this call
    /// and dropped before the result returns.
    fn typecheck_call<'a>(
        value: &Micheline<'a>,
        host: &mut MockKernelHost,
    ) -> Result<TypedValue<'a>, TransferError> {
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let mut ctx = MockCtx::new(host, &mut journal, &registry, source, 0);
        typecheck_entrypoint_value(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("call").unwrap(),
            value,
            &mut ctx,
        )
    }

    #[test]
    fn test_http_call_parses_valid_parameters() {
        let arena = typed_arena::Arena::new();
        let value = build_http_call_micheline(
            &arena,
            "http://michelson/KT1abc/transfer",
            &[("Content-Type", "application/micheline")],
            &[0x01, 0x02],
            1,
        );
        let mut host = MockKernelHost::default();
        let typed = typecheck_call(&value, &mut host).unwrap();
        let (request, _) = extract_http_call_request(typed).unwrap();
        assert_eq!(request.uri(), "http://michelson/KT1abc/transfer");
        assert_eq!(request.method(), http::Method::POST);
        assert_eq!(
            request.headers().get("Content-Type").unwrap(),
            "application/micheline"
        );
        assert_eq!(request.body(), &vec![0x01, 0x02]);
    }

    #[test]
    fn test_http_call_empty_headers() {
        let arena = typed_arena::Arena::new();
        let value =
            build_http_call_micheline(&arena, "http://michelson/KT1abc", &[], &[], 0);
        let mut host = MockKernelHost::default();
        let typed = typecheck_call(&value, &mut host).unwrap();
        let (request, _) = extract_http_call_request(typed).unwrap();
        assert_eq!(request.uri(), "http://michelson/KT1abc");
        assert_eq!(request.method(), http::Method::GET);
        assert!(request.headers().is_empty());
        assert!(request.body().is_empty());
    }

    #[test]
    fn test_http_call_multiple_headers() {
        let arena = typed_arena::Arena::new();
        let value = build_http_call_micheline(
            &arena,
            "http://michelson/KT1abc",
            &[
                ("Content-Type", "application/micheline"),
                ("X-Custom", "some-value"),
            ],
            &[0xDE, 0xAD],
            42,
        );
        let mut host = MockKernelHost::default();
        let typed = typecheck_call(&value, &mut host).unwrap();
        let (request, _) = extract_http_call_request(typed).unwrap();
        // Unknown method defaults to POST
        assert_eq!(request.method(), http::Method::POST);
        assert_eq!(
            request.headers().get("Content-Type").unwrap(),
            "application/micheline"
        );
        assert_eq!(request.headers().get("X-Custom").unwrap(), "some-value");
        assert_eq!(request.body(), &vec![0xDE, 0xAD]);
    }

    #[test]
    fn test_http_call_malformed_parameters() {
        let mut host = MockKernelHost::default();
        let result = typecheck_call(
            &Micheline::String("not a valid http_call".to_string()),
            &mut host,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_inject_context_headers_raw_sets_values() {
        let mut headers = http::HeaderMap::new();
        let crac_id = CracId::new(0, 5).to_string();
        inject_context_headers_raw(
            &mut headers,
            "sender_alias",
            "source_alias",
            RuntimeId::Tezos,
            42u64, // 42 mutez = 0.000042 TEZ
            1000,
            1700000000u64,
            5,
            &crac_id,
            0,
        )
        .unwrap();
        assert_eq!(headers.get("X-Tezos-Sender").unwrap(), "sender_alias");
        assert_eq!(headers.get("X-Tezos-Source").unwrap(), "source_alias");
        assert_eq!(
            headers.get("X-Tezos-Source-Runtime").unwrap(),
            u8::from(RuntimeId::Tezos).to_string().as_str()
        );
        assert_eq!(headers.get("X-Tezos-Amount").unwrap(), "0.000042");
        assert_eq!(headers.get("X-Tezos-Gas-Limit").unwrap(), "1000");
        assert_eq!(headers.get("X-Tezos-Timestamp").unwrap(), "1700000000");
        assert_eq!(headers.get("X-Tezos-Block-Number").unwrap(), "5");
        assert_eq!(headers.get("X-Tezos-Cross-Runtime-Call-Id").unwrap(), "0-5");
    }

    /// Non-zero `crac_depth` materialises on the outgoing header.
    /// Receiving runtimes parse it into `TransactionOrigin::call_depth`
    /// (EVM) or `OperationCtx::crac_chain_depth` (Michelson); seeing
    /// the value round-trip through the header is the wire-level
    /// guarantee.
    #[test]
    fn test_inject_context_headers_raw_writes_nonzero_crac_depth() {
        let mut headers = http::HeaderMap::new();
        let crac_id = CracId::new(0, 0).to_string();
        inject_context_headers_raw(
            &mut headers,
            "sender_alias",
            "source_alias",
            RuntimeId::Tezos,
            0u64,
            0,
            0,
            0,
            &crac_id,
            7,
        )
        .unwrap();
        assert_eq!(
            headers.get("X-Tezos-Cross-Runtime-Call-Depth").unwrap(),
            "7"
        );
    }

    #[test]
    fn test_inject_context_headers_raw_overwrites_existing() {
        let mut headers = http::HeaderMap::new();
        headers.insert(
            http::header::HeaderName::from_static("x-tezos-sender"),
            "old-value".parse().unwrap(),
        );
        let crac_id = CracId::new(0, 0).to_string();
        inject_context_headers_raw(
            &mut headers,
            "new_alias",
            "source_alias",
            RuntimeId::Tezos,
            0u64,
            0,
            0u64,
            0,
            &crac_id,
            0,
        )
        .unwrap();
        assert_eq!(headers.get("X-Tezos-Sender").unwrap(), "new_alias");
    }

    #[test]
    fn test_http_call_rejects_x_tezos_headers() {
        let arena = typed_arena::Arena::new();
        let value = build_http_call_micheline(
            &arena,
            "http://michelson/KT1abc",
            &[("X-Tezos-Sender", "attacker")],
            &[],
            0,
        );
        let mut host = MockKernelHost::default();
        let typed = typecheck_call(&value, &mut host).unwrap();
        let result = extract_http_call_request(typed);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("X-Tezos-"),
            "error should mention the header name: {err}"
        );
    }

    #[test]
    fn test_http_call_rejects_x_tezos_headers_case_insensitive() {
        let arena = typed_arena::Arena::new();
        let value = build_http_call_micheline(
            &arena,
            "http://michelson/KT1abc",
            &[("x-tezos-amount", "999")],
            &[],
            0,
        );
        let mut host = MockKernelHost::default();
        let typed = typecheck_call(&value, &mut host).unwrap();
        let result = extract_http_call_request(typed);
        assert!(result.is_err());
    }

    // --- Cross-runtime call: amount edge cases ---

    #[test]
    fn test_cross_runtime_call_zero_amount() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 0i64;

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);
        let result =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result.is_ok());

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 1);
        assert_eq!(serve_calls[0].headers().get(X_TEZOS_AMOUNT).unwrap(), "0");
    }

    #[test]
    fn test_cross_runtime_call_negative_amount_rejected() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = -1i64;

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);
        let result =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Negative amount"),
            "error should mention negative amount: {err}"
        );
    }

    // ── CRAC ID and event tests ─────────────────────────────────────────

    /// Outgoing CRAC via the generic %call entrypoint: the gateway
    /// dispatches the call to the EVM runtime.  CRAC events are emitted
    /// by the incoming receipt builder, not by the gateway itself.
    #[test]
    fn test_outgoing_crac_via_call_entrypoint() {
        let mut host = MockKernelHost::default();
        let generated_alias = "KT1_mock_alias".to_string();
        let registry = MockRegistry::new(generated_alias);

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 100_000_000i64; // 100 tez in mutez

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);

        let entrypoint = Entrypoint::try_from("call").unwrap();
        let arena = typed_arena::Arena::new();
        let value = build_http_call_micheline(
            &arena,
            &format!("http://ethereum/{dest}"),
            &[],
            &[],
            1,
        );

        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &entrypoint,
            value,
            &mut ctx,
        );
        assert!(
            result.is_ok(),
            "execute_enshrined_contract failed: {result:?}"
        );

        // Gateway does not emit CRAC events itself; they are produced
        // by the incoming receipt builder on the receiving runtime.
        let internal_ops = result.unwrap();
        assert!(
            internal_ops.is_empty(),
            "gateway should not emit internal ops"
        );
    }

    /// The legacy %default (simple transfer) entrypoint was removed:
    /// dispatching it must fail with an unknown-entrypoint error.
    #[test]
    fn test_default_entrypoint_is_rejected() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx =
            MockCtx::new(&mut host, &mut journal, &registry, source, 100_000_000);

        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::default(),
            Micheline::String("0x1234567890123456789012345678901234567890".into()),
            &mut ctx,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Unknown entrypoint"),
            "error should mention unknown entrypoint: {err}"
        );
    }

    /// Outgoing CRAC via "call_evm" entrypoint: same behavior — the
    /// gateway dispatches the call without emitting events.
    #[test]
    fn test_outgoing_crac_via_call_evm_entrypoint() {
        let mut host = MockKernelHost::default();
        let generated_alias = "KT1_mock_alias".to_string();
        let registry = MockRegistry::new(generated_alias);

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let amount = 50_000_000i64;

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);
        let mut gas = Gas::default();

        let arena = typed_arena::Arena::new();
        let dest = "0x1234567890123456789012345678901234567890";
        let method_sig = "transfer(address,uint256)";
        let abi_params: Vec<u8> = vec![0xAA; 64];

        let value = Micheline::prim2(
            &arena,
            Prim::Pair,
            Micheline::String(dest.to_string()),
            Micheline::prim2(
                &arena,
                Prim::Pair,
                Micheline::String(method_sig.to_string()),
                Micheline::prim2(
                    &arena,
                    Prim::Pair,
                    Micheline::Bytes(abi_params),
                    Micheline::prim0(Prim::None, &mut gas).unwrap(),
                    &mut gas,
                )
                .unwrap(),
                &mut gas,
            )
            .unwrap(),
            &mut gas,
        )
        .unwrap();

        let entrypoint = Entrypoint::try_from("call_evm").unwrap();
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &entrypoint,
            value,
            &mut ctx,
        );
        assert!(result.is_ok(), "call_evm entrypoint failed: {result:?}");

        let internal_ops = result.unwrap();
        assert!(
            internal_ops.is_empty(),
            "gateway should not emit internal ops"
        );
    }

    #[test]
    fn test_cross_runtime_call_non_success_response() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias")
            .with_serve_response(500, b"internal server error".to_vec());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 100i64;
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);
        let result =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("500"),
            "error should contain status code: {err}"
        );
    }

    #[test]
    fn test_cross_runtime_call_400_response() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias")
            .with_serve_response(400, b"bad request".to_vec());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        let result =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("bad request"),
            "error should contain response body: {err}"
        );
    }

    #[test]
    fn test_cross_runtime_call_429_is_out_of_gas() {
        let response = http::Response::builder()
            .status(429)
            .body(b"OOG".to_vec())
            .unwrap();
        let mut operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(1_000_000).unwrap();
        let err = classify_and_charge_crac_response(
            response,
            Some("ethereum"),
            &mut operation_gas,
        )
        .unwrap_err();
        assert!(
            matches!(err, CracError::Operation(TransferError::OutOfGas(OutOfGas))),
            "429 must map to a typed OutOfGas, got {err:?}"
        );
    }

    // The 4xx path charges the per-byte payload surcharge for the
    // response body, symmetrically with the success path, so the
    // attacker-controlled body that lands in the failed CRAC receipt is
    // metered (and thus gas-bounded) rather than copied in for free.
    #[test]
    fn test_cross_runtime_4xx_body_is_charged() {
        let body_len = 1_000;
        let response = http::Response::builder()
            .status(400)
            .body(vec![b'A'; body_len])
            .unwrap();
        // No callee-gas header, so the only charge is the body surcharge:
        // body_len * TEZOSX_GATEWAY_PER_BYTE_MILLIGAS.
        let start = 10_000_000;
        let mut operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(start).unwrap();
        let _ = classify_and_charge_crac_response(
            response,
            Some("ethereum"),
            &mut operation_gas,
        )
        .unwrap_err();
        let expected_charge = (body_len as u64) * TEZOSX_GATEWAY_PER_BYTE_MILLIGAS;
        let remaining = operation_gas.remaining.milligas().unwrap() as u64;
        assert_eq!(
            remaining,
            start - expected_charge,
            "4xx body must be charged at the gateway per-byte rate"
        );
    }

    // `from_utf8_lossy` expands each invalid byte to U+FFFD (3 bytes), and
    // it is the decoded string — not the raw body — that is persisted in
    // the receipt. The charge must therefore track the decoded length so
    // an all-invalid-bytes body cannot be persisted under-metered.
    #[test]
    fn test_cross_runtime_4xx_charges_decoded_length() {
        let body = vec![0xFF_u8; 100]; // every byte is invalid UTF-8
        let response = http::Response::builder()
            .status(400)
            .body(body.clone())
            .unwrap();
        let start = 10_000_000;
        let mut operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(start).unwrap();
        let err = classify_and_charge_crac_response(
            response,
            Some("ethereum"),
            &mut operation_gas,
        )
        .unwrap_err();
        let CracError::Operation(TransferError::GatewayError(msg)) = err else {
            panic!("4xx must map to a GatewayError, got {err:?}");
        };
        let decoded_len = String::from_utf8_lossy(&body).len();
        assert!(decoded_len > body.len(), "lossy must expand invalid bytes");
        let expected_charge = (decoded_len as u64) * TEZOSX_GATEWAY_PER_BYTE_MILLIGAS;
        let remaining = operation_gas.remaining.milligas().unwrap() as u64;
        assert_eq!(
            remaining,
            start - expected_charge,
            "charge must track the decoded (persisted) length, not the raw body"
        );
        // The decoded body is what lands in the persisted error string.
        assert!(
            msg.len() >= decoded_len,
            "decoded body must be present in the error: {msg}"
        );
    }

    // The body surcharge on the 4xx path can itself exhaust gas; that must
    // surface as a catchable operation-level out-of-gas, never abort the
    // block, and never persist the oversized body.
    #[test]
    fn test_cross_runtime_4xx_body_charge_can_oog() {
        let response = http::Response::builder()
            .status(400)
            .body(vec![b'A'; 1_000_000])
            .unwrap();
        let mut operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(1_000).unwrap();
        let err = classify_and_charge_crac_response(
            response,
            Some("ethereum"),
            &mut operation_gas,
        )
        .unwrap_err();
        assert!(
            matches!(err, CracError::Operation(TransferError::OutOfGas(OutOfGas))),
            "4xx body charge exhaustion must map to a catchable OutOfGas, got {err:?}"
        );
    }

    // Drive `dispatch_staticcall_evm_get` against a mock EVM peer that
    // returns `status`. Alias resolution falls back to the deterministic
    // native path (empty host → no stored origin), so the test reaches
    // the response classifier with a generous gas budget.
    fn run_staticcall_evm_get(
        status: u16,
    ) -> Result<Option<Vec<u8>>, mir::interpreter::InterpretError<'static>> {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string())
            .with_serve_response(status, b"body".to_vec());
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let context = crate::context::TezlinkContext::init_context();
        let calling_kt1 = ContractKt1Hash::from([0u8; 20]);
        let source =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let mut operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(100_000_000).unwrap();
        dispatch_staticcall_evm_get(
            &mut host,
            &mut operation_gas,
            &registry,
            &mut journal,
            &context,
            &calling_kt1,
            &source,
            "1",
            "0",
            "1",
            0,
            "0x1234567890123456789012345678901234567890",
            &[],
        )
    }

    // The view path is the issue's core: an EVM out-of-gas (429) must fail
    // closed with `OutOfGas` so the Michelson caller cannot mistake gas
    // exhaustion for an absent view via `IF_NONE` (L2-1457).
    #[test]
    fn test_staticcall_evm_429_is_out_of_gas() {
        let result = run_staticcall_evm_get(429);
        assert!(
            matches!(result, Err(mir::interpreter::InterpretError::OutOfGas)),
            "429 must surface as InterpretError::OutOfGas, got {result:?}"
        );
    }

    // A genuine 4xx ("view not found") must still collapse to `Ok(None)`,
    // so the OOG carve-out above does not regress absence detection.
    #[test]
    fn test_staticcall_evm_400_is_none() {
        let result = run_staticcall_evm_get(400);
        assert_eq!(result, Ok(None));
    }

    /// Drive `dispatch_staticcall_evm_get` as `calling_kt1`, optionally
    /// seeding the shared journal with a captured originator
    /// (`original_source`, as the entering EVM gateway would). Runs against
    /// an injective-alias registry (sender- and operation-source-derived
    /// aliases stay distinguishable) whose default `serve` replies 200 with
    /// `X-Tezos-Gas-Consumed: 0` and records the request. Returns the
    /// dispatch result, the milligas consumed, and the injected
    /// `X-Tezos-Sender` / `X-Tezos-Source` / `X-Tezos-Source-Runtime`.
    #[allow(clippy::type_complexity)]
    fn run_staticcall_evm_get_as(
        calling_kt1: &ContractKt1Hash,
        source: &PublicKeyHash,
        original_source: Option<OriginalSource>,
    ) -> (
        Result<Option<Vec<u8>>, mir::interpreter::InterpretError<'static>>,
        u64,
        String,
        String,
        String,
    ) {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("unused").with_injective_aliases();
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        if let Some(os) = original_source {
            journal.set_original_source(os);
        }
        let context = crate::context::TezlinkContext::init_context();
        let mut operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(100_000_000).unwrap();
        let gas_before = operation_gas.remaining.milligas().unwrap();

        let result = dispatch_staticcall_evm_get(
            &mut host,
            &mut operation_gas,
            &registry,
            &mut journal,
            &context,
            calling_kt1,
            source,
            "1",
            "0",
            "1",
            0,
            "0x1234567890123456789012345678901234567890",
            &[],
        );
        let consumed =
            u64::from(gas_before - operation_gas.remaining.milligas().unwrap());

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 1);
        let headers = serve_calls[0].headers();
        let header = |name| {
            headers
                .get(name)
                .expect("header must be injected")
                .to_str()
                .expect("header must be ascii")
                .to_owned()
        };
        (
            result,
            consumed,
            header(X_TEZOS_SENDER),
            header(X_TEZOS_SOURCE),
            header(X_TEZOS_SOURCE_RUNTIME),
        )
    }

    /// When the `staticcall_evm` view services an inbound CRAC, the nested
    /// EVM GET carries the *captured top-level originator* — read verbatim
    /// from the shared journal, where the entering EVM gateway stored its
    /// complete native `(runtime, address)` identity — as `X-Tezos-Source`
    /// (→ the inner callee's `tx.origin`), with its native runtime in
    /// `X-Tezos-Source-Runtime`, while keeping the immediate Michelson
    /// caller as `X-Tezos-Sender` (→ the inner callee's `msg.sender`).
    ///
    /// The originator is taken straight from the journal, so NO durable
    /// origin record is consulted: the result is correct even when the
    /// originator's Michelson alias was never materialized (the empty
    /// `MockKernelHost` here has no records). The test pins that the source
    /// is the captured originator's EVM alias verbatim — not the sender,
    /// and not an alias re-derived from a Michelson alias. Only the sender
    /// costs a durable read; the journal-sourced originator is free.
    #[test]
    fn test_staticcall_evm_forwards_captured_originator_record_independent() {
        let calling_kt1 = ContractKt1Hash::from([0xAA; 20]);
        // The entering EVM gateway captured an Ethereum-native originator
        // E_0: its EVM address is the handle the view forwards as
        // `X-Tezos-Source` (the view targets Ethereum).
        let e0_hex = "0x4242424242424242424242424242424242424242";
        let original_source =
            OriginalSource::new(RuntimeId::Ethereum, e0_hex.to_string());
        // With a captured originator the operation source is irrelevant to
        // `X-Tezos-Source`; any valid PKH works here.
        let op_source =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();

        let (result, consumed, sender, source, source_runtime) =
            run_staticcall_evm_get_as(
                &calling_kt1,
                &op_source,
                Some(original_source.clone()),
            );

        assert_eq!(result, Ok(Some(vec![])));
        // Only the sender is a durable origin read; the captured
        // originator comes from the journal at no read cost. The mock
        // reports zero inner gas, so one ALIAS_LOOKUP_MILLIGAS is the
        // whole bill.
        assert_eq!(
            consumed, ALIAS_LOOKUP_MILLIGAS,
            "expected one alias-lookup charge (sender only; originator is journal-sourced)"
        );
        assert_eq!(sender, AddressHash::Kt1(calling_kt1).to_base58_check());
        // Source is the captured originator's native (here EVM) address
        // verbatim — not a re-resolution through a durable record.
        assert_eq!(source, original_source.original_address());
        assert_ne!(
            sender, source,
            "tx.origin (captured originator) must not collapse onto msg.sender (sender)"
        );
        // The captured originator is Ethereum-native, so its runtime tag
        // is Ethereum — preserved across the boundary rather than defaulted
        // to the immediate Tezos sender's runtime.
        assert_eq!(
            source_runtime,
            u8::from(RuntimeId::Ethereum).to_string(),
            "X-Tezos-Source-Runtime must tag the captured originator's native runtime"
        );
    }

    /// With no captured originator on the journal (a top-level Michelson
    /// view), `X-Tezos-Source` resolves the *operation source*
    /// independently of the immediate caller: the inner callee's
    /// `tx.origin` is the account that signed the outer operation, not
    /// `msg.sender`. The source is a distinct durable origin read, so two
    /// alias-lookup charges are billed (sender + operation source). The
    /// test pins that the source does not collapse onto the sender and
    /// that both lookups are charged.
    #[test]
    fn test_staticcall_evm_uses_operation_source_without_captured_originator() {
        let calling_kt1 = ContractKt1Hash::from([0xAA; 20]);
        let op_source =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();

        let (result, consumed, sender, source, source_runtime) =
            run_staticcall_evm_get_as(&calling_kt1, &op_source, None);

        assert_eq!(result, Ok(Some(vec![])));
        // Two distinct origin reads (sender + operation source), one
        // ALIAS_LOOKUP_MILLIGAS each.
        assert_eq!(
            consumed,
            2 * ALIAS_LOOKUP_MILLIGAS,
            "expected two alias-lookup charges (sender + operation source)"
        );
        assert_eq!(sender, AddressHash::Kt1(calling_kt1).to_base58_check());
        assert_eq!(source, AddressHash::Implicit(op_source).to_base58_check());
        assert_ne!(
            sender, source,
            "tx.origin (operation source) must not collapse onto msg.sender (sender)"
        );
        // The operation source is a native implicit account, so its
        // runtime tag is Tezos — and the header must be emitted explicitly.
        assert_eq!(
            source_runtime,
            u8::from(RuntimeId::Tezos).to_string(),
            "X-Tezos-Source-Runtime must be present and tag the source's runtime"
        );
    }

    // --- address_hash_bytes encoding ---

    #[test]
    fn test_address_hash_bytes_gateway() {
        let bytes = EnshrinedContracts::TezosXGateway.address_hash_bytes();
        assert_eq!(bytes[0], 0x01);
        assert_eq!(&bytes[1..21], &GATEWAY_ADDRESS);
        assert_eq!(bytes[21], 0x00);
    }

    #[test]
    fn test_address_hash_bytes_erc20_wrapper() {
        let bytes = EnshrinedContracts::ERC20Wrapper.address_hash_bytes();
        assert_eq!(bytes[0], 0x01);
        assert_eq!(&bytes[1..21], &ERC20_WRAPPER_ADDRESS);
        assert_eq!(bytes[21], 0x00);
    }

    // --- bigint_to_u256 / biguint_to_u256 edge cases ---

    #[test]
    fn test_bigint_to_u256_zero() {
        let result = bigint_to_u256(&BigInt::from(0));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::zero());
    }

    #[test]
    fn test_bigint_to_u256_positive() {
        let result = bigint_to_u256(&BigInt::from(42));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::from(42));
    }

    #[test]
    fn test_bigint_to_u256_large_value() {
        // U256::MAX = 2^256 - 1
        let max_u256_bytes = [0xFFu8; 32];
        let value =
            num_bigint::BigInt::from_bytes_le(num_bigint::Sign::Plus, &max_u256_bytes);
        let result = bigint_to_u256(&value);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::MAX);
    }

    #[test]
    fn test_bigint_to_u256_overflow() {
        // Value larger than 32 bytes overflows
        let mut bytes = vec![0xFF; 33];
        bytes[32] = 0x01;
        let value = num_bigint::BigInt::from_bytes_le(num_bigint::Sign::Plus, &bytes);
        let result = bigint_to_u256(&value);
        assert!(result.is_err());
    }

    #[test]
    fn test_biguint_to_u256_zero() {
        let result = biguint_to_u256(num_bigint::BigUint::from(0u32));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::zero());
    }

    #[test]
    fn test_biguint_to_u256_overflow() {
        let mut bytes = vec![0xFF; 33];
        bytes[32] = 0x01;
        let value = num_bigint::BigUint::from_bytes_le(&bytes);
        let result = biguint_to_u256(value);
        assert!(result.is_err());
    }

    // --- inject_context_headers_raw edge cases ---

    #[test]
    fn test_inject_context_headers_raw_zero_amount() {
        let mut headers = http::HeaderMap::new();
        let crac_id = CracId::new(0, 0).to_string();
        inject_context_headers_raw(
            &mut headers,
            "sender",
            "source",
            RuntimeId::Tezos,
            0u64,
            0,
            0u64,
            0,
            &crac_id,
            0,
        )
        .unwrap();
        assert_eq!(headers.get("X-Tezos-Amount").unwrap(), "0");
        assert_eq!(headers.get("X-Tezos-Gas-Limit").unwrap(), "0");
        assert_eq!(headers.get("X-Tezos-Timestamp").unwrap(), "0");
        assert_eq!(headers.get("X-Tezos-Block-Number").unwrap(), "0");
    }

    #[test]
    fn test_inject_context_headers_raw_max_values() {
        let mut headers = http::HeaderMap::new();
        let crac_id = CracId::new(0, 0).to_string();
        inject_context_headers_raw(
            &mut headers,
            "sender",
            "source",
            RuntimeId::Tezos,
            u64::MAX,
            u64::MAX,
            u64::MAX,
            u32::MAX,
            &crac_id,
            0,
        )
        .unwrap();
        // u64::MAX mutez is a very large TEZ amount
        let amount = headers.get("X-Tezos-Amount").unwrap().to_str().unwrap();
        assert!(!amount.is_empty());
        assert_eq!(
            headers.get("X-Tezos-Gas-Limit").unwrap(),
            &u64::MAX.to_string()
        );
        assert_eq!(
            headers.get("X-Tezos-Timestamp").unwrap(),
            &u64::MAX.to_string()
        );
        assert_eq!(
            headers.get("X-Tezos-Block-Number").unwrap(),
            &u32::MAX.to_string()
        );
    }

    // --- ERC20 wrapper edge cases ---

    #[test]
    fn test_abi_encode_address_uint256_zero_amount() {
        let addr = vec![0x11; 20];
        let amount = BigInt::from(0);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_ok());
        let calldata = result.unwrap();
        // 4 bytes selector + 32 bytes address + 32 bytes value
        assert_eq!(calldata.len(), 68);
        // The uint256 part should be all zeros
        assert!(calldata[36..68].iter().all(|&b| b == 0));
    }

    #[test]
    fn test_abi_encode_address_uint256_negative_amount() {
        let addr = vec![0x11; 20];
        let amount = BigInt::from(-1);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("non-negative"),
            "error should mention non-negative: {err}"
        );
    }

    #[test]
    fn test_abi_encode_address_exceeding_20_bytes() {
        let addr = vec![0x11; 21]; // 21 bytes — too long
        let amount = BigInt::from(100);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("20 bytes"),
            "error should mention 20 bytes: {err}"
        );
    }

    #[test]
    fn test_abi_encode_amount_exceeding_uint256() {
        let addr = vec![0x11; 20];
        // 33 bytes — exceeds uint256 (32 bytes)
        let bytes = vec![0xFF; 33];
        let amount = num_bigint::BigInt::from_bytes_be(num_bigint::Sign::Plus, &bytes);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("uint256"),
            "error should mention uint256: {err}"
        );
    }

    #[test]
    fn test_abi_encode_address_short_address() {
        // Address shorter than 20 bytes should be left-padded
        let addr = vec![0xAB; 4]; // Only 4 bytes
        let amount = BigInt::from(1);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_ok());
        let calldata = result.unwrap();
        // First 4 bytes are selector, next 32 bytes are left-padded address
        // 28 zero bytes followed by 4 bytes of 0xAB
        assert!(calldata[4..32].iter().all(|&b| b == 0));
        assert!(calldata[32..36].iter().all(|&b| b == 0xAB));
    }

    // --- Non-enshrined contract detection ---

    #[test]
    fn test_non_enshrined_contract() {
        let non_enshrined = ContractKt1Hash::try_from_bytes(&[0x01; 20]).unwrap();
        assert!(!is_enshrined(&non_enshrined));
        assert_eq!(from_kt1(&non_enshrined), None);
    }

    #[test]
    fn test_enshrined_prefix_mismatch() {
        // Starts with ENSHRINED_PREFIX but doesn't match either contract
        let mut bytes = ENSHRINED_PREFIX.to_vec();
        bytes.extend_from_slice(&[0x99; 14]);
        let contract = ContractKt1Hash::try_from_bytes(&bytes).unwrap();
        assert!(!is_enshrined(&contract));
        assert_eq!(from_kt1(&contract), None);
    }

    // --- Gateway entrypoint edge cases ---

    #[test]
    fn test_gateway_unknown_entrypoint() {
        let entrypoints =
            get_enshrined_contract_entrypoint(EnshrinedContracts::TezosXGateway).unwrap();
        let unknown = Entrypoint::try_from("nonexistent").unwrap();
        assert!(!entrypoints.contains_key(&unknown));
    }

    #[test]
    fn test_erc20_wrapper_entrypoints() {
        let entrypoints =
            get_enshrined_contract_entrypoint(EnshrinedContracts::ERC20Wrapper).unwrap();
        assert!(entrypoints.contains_key(&Entrypoint::try_from("transfer").unwrap()));
        assert!(entrypoints.contains_key(&Entrypoint::try_from("approve").unwrap()));
        assert!(!entrypoints.contains_key(&Entrypoint::default()));
    }

    // --- HTTP call method mapping ---

    #[test]
    fn test_http_call_method_0_is_get() {
        let arena = typed_arena::Arena::new();
        let value =
            build_http_call_micheline(&arena, "http://michelson/KT1abc", &[], &[], 0);
        let mut host = MockKernelHost::default();
        let typed = typecheck_call(&value, &mut host).unwrap();
        let (request, _) = extract_http_call_request(typed).unwrap();
        assert_eq!(request.method(), http::Method::GET);
    }

    #[test]
    fn test_http_call_method_1_is_post() {
        let arena = typed_arena::Arena::new();
        let value =
            build_http_call_micheline(&arena, "http://michelson/KT1abc", &[], &[], 1);
        let mut host = MockKernelHost::default();
        let typed = typecheck_call(&value, &mut host).unwrap();
        let (request, _) = extract_http_call_request(typed).unwrap();
        assert_eq!(request.method(), http::Method::POST);
    }

    #[test]
    fn test_http_call_unknown_method_defaults_to_post() {
        let arena = typed_arena::Arena::new();
        let value =
            build_http_call_micheline(&arena, "http://michelson/KT1abc", &[], &[], 99);
        let mut host = MockKernelHost::default();
        let typed = typecheck_call(&value, &mut host).unwrap();
        let (request, _) = extract_http_call_request(typed).unwrap();
        assert_eq!(request.method(), http::Method::POST);
    }

    // --- Amount header formatting in cross-runtime call ---

    #[test]
    fn test_cross_runtime_call_large_amount_header() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        // 1_000_000 mutez = 1 TEZ
        let amount = 1_000_000i64;

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);
        let result =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result.is_ok());

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls[0].headers().get(X_TEZOS_AMOUNT).unwrap(), "1");
    }

    #[test]
    fn test_cross_runtime_call_fractional_amount_header() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        // 1 mutez = 0.000001 TEZ
        let amount = 1i64;

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, amount);
        let result =
            dispatch_crac_call(&mut ctx, build_ethereum_request(dest, &[]).unwrap());
        assert!(result.is_ok());

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(
            serve_calls[0].headers().get(X_TEZOS_AMOUNT).unwrap(),
            "0.000001"
        );
    }

    // --- Typecheck entrypoint value errors ---

    #[test]
    fn test_typecheck_unknown_entrypoint_returns_error() {
        let mut host = MockKernelHost::default();
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        let value = Micheline::String("hello".to_string());
        let result = typecheck_entrypoint_value(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("nonexistent").unwrap(),
            &value,
            &mut ctx,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Unknown entrypoint"),
            "error should mention unknown entrypoint: {err}"
        );
    }

    #[test]
    fn test_typecheck_default_entrypoint_is_unknown() {
        let mut host = MockKernelHost::default();
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        // The legacy %default (simple transfer) entrypoint was removed:
        // it no longer typechecks against any parameter.
        let value = Micheline::String("0xabc".into());
        let result = typecheck_entrypoint_value(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::default(),
            &value,
            &mut ctx,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Unknown entrypoint"),
            "error should mention unknown entrypoint: {err}"
        );
    }

    // --- %collect_result entrypoint ---

    #[test]
    fn test_gateway_has_collect_result_entrypoint() {
        let entrypoints =
            get_enshrined_contract_entrypoint(EnshrinedContracts::TezosXGateway).unwrap();
        let ep = Entrypoint::try_from("collect_result").unwrap();
        assert!(entrypoints.contains_key(&ep));
        assert_eq!(entrypoints[&ep], Type::Bytes);
    }

    #[test]
    fn test_collect_result_typechecks_bytes() {
        let mut host = MockKernelHost::default();
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        let value = Micheline::Bytes(vec![0xDE, 0xAD, 0xBE, 0xEF]);
        let result = typecheck_entrypoint_value(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            &value,
            &mut ctx,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_collect_result_rejects_non_bytes() {
        let mut host = MockKernelHost::default();
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        let value = Micheline::String("not bytes".to_string());
        let result = typecheck_entrypoint_value(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            &value,
            &mut ctx,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_collect_result_execute_succeeds() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        // %collect_result writes to the dispatch slot opened by `serve`.
        // Simulate that here.
        journal.michelson.push_dispatch_slot();
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        let value = Micheline::Bytes(vec![0xCA, 0xFE]);
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            value,
            &mut ctx,
        );
        assert!(result.is_ok());
        // No serve calls are made: %collect_result only deposits bytes.
        assert!(registry.serve_calls.borrow().is_empty());
        // The payload is now observable on the dispatch slot.
        assert_eq!(
            journal.michelson.take_dispatch_result(),
            Ok(Some(vec![0xCA, 0xFE]))
        );
    }

    #[test]
    fn test_collect_result_execute_empty_bytes() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        // %collect_result writes to the dispatch slot opened by `serve`.
        // Simulate that here.
        journal.michelson.push_dispatch_slot();
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        let value = Micheline::Bytes(vec![]);
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            value,
            &mut ctx,
        );
        assert!(result.is_ok());
        assert_eq!(journal.michelson.take_dispatch_result(), Ok(Some(vec![])));
    }

    #[test]
    fn test_collect_result_execute_without_frame_fails() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();

        // No `push_dispatch_slot` — dispatching %collect_result outside
        // a `serve` invocation must surface the journal invariant as a
        // gateway error so the operation reverts cleanly.
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        let value = Micheline::Bytes(vec![0xCA, 0xFE]);
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            value,
            &mut ctx,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("no active dispatch slot"),
            "error should mention missing slot: {err}"
        );
        assert!(registry.serve_calls.borrow().is_empty());
    }

    #[test]
    fn test_collect_result_execute_already_set_fails() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        // %collect_result writes to the dispatch slot opened by `serve`.
        // Simulate that here.
        journal.michelson.push_dispatch_slot();
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        // First dispatch deposits the payload.
        let first = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0xCA, 0xFE]),
            &mut ctx,
        );
        assert!(first.is_ok());
        // Second dispatch on the same slot must fail: the slot is
        // write-once per dispatch, and a retry signals a misbehaving
        // adapter.
        let second = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0xBE, 0xEF]),
            &mut ctx,
        );
        assert!(second.is_err());
        let err = second.unwrap_err();
        assert!(
            err.to_string().contains("dispatch result already set"),
            "error should mention already-set slot: {err}"
        );
        // The original payload is preserved — a failing retry doesn't
        // clobber what the slot already holds.
        assert_eq!(
            journal.michelson.take_dispatch_result(),
            Ok(Some(vec![0xCA, 0xFE]))
        );
        assert!(registry.serve_calls.borrow().is_empty());
    }

    #[test]
    fn test_collect_result_rejects_nonzero_amount() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();

        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        // Dispatch slot is set up; the rejection must happen even when
        // a slot exists, so it isn't masked by the no-slot check.
        journal.michelson.push_dispatch_slot();
        // Non-zero amount: %collect_result has no recipient and is not
        // a CRAC, so any positive amount must fail.
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 1);
        let value = Micheline::Bytes(vec![0xCA, 0xFE]);
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            value,
            &mut ctx,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("collect_result: amount must be 0"),
            "error should mention non-zero amount rejection: {err}"
        );
        // The dispatch slot stays empty — a rejected deposit must not
        // leak any payload into the response body.
        assert_eq!(journal.michelson.take_dispatch_result(), Ok(None));
        assert!(registry.serve_calls.borrow().is_empty());
    }

    // --- %collect_result gas charges ---

    /// MIR's typechecker charges `tc_cost::VALUE_STEP = 100` mgas when
    /// typechecking any leaf Micheline value. The %collect_result gas
    /// tests pre-fund this so that budgets isolate the handler's own
    /// charges from the typecheck cost.
    const TYPECHECK_VALUE_STEP_MILLIGAS: u64 = 100;

    // 256-byte payload, no other charges: typecheck (100) + size base
    // (460) + 1.5 * 256 (384) = 944 mgas total consumed.
    #[test]
    fn test_collect_result_charges_size_dependent() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        journal.michelson.push_dispatch_slot();
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        ctx.operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(100_000).unwrap();
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0u8; 256]),
            &mut ctx,
        );
        assert!(result.is_ok());
        assert_eq!(ctx.operation_gas().total_milligas_consumed() as u64, 944);
    }

    // Empty payload: budget exactly covers typecheck (100) + size
    // base (460). Remaining gas must be zero after a successful deposit.
    #[test]
    fn test_collect_result_charges_exact_budget_zero_bytes() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        journal.michelson.push_dispatch_slot();
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        ctx.operation_gas = crate::gas::TezlinkOperationGas::start_milligas(
            TYPECHECK_VALUE_STEP_MILLIGAS + COLLECT_RESULT_SIZE_BASE_MILLIGAS,
        )
        .unwrap();
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![]),
            &mut ctx,
        );
        assert!(result.is_ok());
        assert_eq!(ctx.operation_gas().remaining.milligas().unwrap(), 0);
    }

    // Budget covers the typecheck step but nothing more: the handler's
    // size charge trips OutOfGas before `set_dispatch_result`, so the
    // dispatch slot stays empty.
    #[test]
    fn test_collect_result_out_of_gas_on_size_charge() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        journal.michelson.push_dispatch_slot();
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        ctx.operation_gas = crate::gas::TezlinkOperationGas::start_milligas(
            TYPECHECK_VALUE_STEP_MILLIGAS,
        )
        .unwrap();
        // Empty payload: the size term contributes nothing, so only the
        // base charge (460 mgas) fires — which alone exceeds the budget.
        let err = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![]),
            &mut ctx,
        )
        .unwrap_err();
        assert!(matches!(
            err,
            CracError::Operation(TransferError::OutOfGas(OutOfGas))
        ));
        assert_eq!(journal.michelson.take_dispatch_result(), Ok(None));
    }

    // Direct probe of the cost function. Pins the size formula at the
    // boundaries: zero collapses to the base, a mid value matches the
    // documented `460 + 1.5 * size`, and `usize::MAX` must not overflow
    // — `saturating_mul(3)` caps the multiply at `u64::MAX`, the shift
    // halves it, and the final `+ 460` stays inside `u64`.
    #[test]
    fn test_collect_result_size_cost_edge_cases() {
        assert_eq!(
            collect_result_size_cost(0),
            COLLECT_RESULT_SIZE_BASE_MILLIGAS
        );
        // 256-byte payload: 460 + (256 * 3) / 2 = 460 + 384 = 844.
        assert_eq!(collect_result_size_cost(256), 844);
        // Must not panic; result fits in u64.
        let max = collect_result_size_cost(usize::MAX);
        assert!(max >= COLLECT_RESULT_SIZE_BASE_MILLIGAS);
    }

    // The size charge fires before `set_dispatch_result`. If the write
    // then fails (here: no active slot), the gas stays consumed — the
    // kernel did the validation work and the caller pays for it.
    #[test]
    fn test_collect_result_charges_gas_when_no_frame() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        // No `push_dispatch_slot` — `set_dispatch_result` fails.
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        ctx.operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(100_000).unwrap();
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0u8; 256]),
            &mut ctx,
        );
        assert!(result.is_err());
        // typecheck (100) + size base (460) + 1.5 * 256 (384) = 944.
        assert_eq!(ctx.operation_gas().total_milligas_consumed() as u64, 944);
        assert_eq!(
            journal.michelson.take_dispatch_result(),
            Err(DispatchSlotError::NoSlot)
        );
    }

    // The size charge also fires on the failing second deposit when
    // the dispatch slot is already set: gas is consumed before the
    // `AlreadySet` branch of `set_dispatch_result` is reached.
    #[test]
    fn test_collect_result_charges_gas_when_already_set() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        journal.michelson.push_dispatch_slot();
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        ctx.operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(100_000).unwrap();
        // First deposit succeeds. 2-byte payload:
        // typecheck (100) + size base (460) + 1.5 * 2 (3) = 563.
        let first = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0xCA, 0xFE]),
            &mut ctx,
        );
        assert!(first.is_ok());
        let after_first = ctx.operation_gas().total_milligas_consumed() as u64;
        assert_eq!(after_first, 563);
        // Second deposit fails on `AlreadySet` but its size charge is
        // consumed anyway: another 944 mgas for the 256-byte payload.
        let second = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0u8; 256]),
            &mut ctx,
        );
        assert!(second.is_err());
        assert_eq!(
            ctx.operation_gas().total_milligas_consumed() as u64,
            after_first + 944
        );
        // Original payload is preserved.
        assert_eq!(
            journal.michelson.take_dispatch_result(),
            Ok(Some(vec![0xCA, 0xFE]))
        );
    }

    fn make_test_address() -> Address {
        Address {
            hash: AddressHash::from_bytes(&[
                0x01, 0x29, 0x58, 0x93, 0x60, 0xad, 0xf1, 0x56, 0x94, 0xac, 0x33, 0x0d,
                0xe5, 0x9f, 0x46, 0x44, 0x15, 0xb5, 0xf7, 0xea, 0x69, 0x00,
            ])
            .unwrap(),
            entrypoint: Entrypoint::default(),
        }
    }

    #[test]
    fn test_dispatch_callback() {
        let mut host = MockKernelHost::default();
        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        let destination = make_test_address();
        let body = vec![0xDE, 0xAD];
        let ops =
            dispatch_callback(&mut ctx, Some(destination.clone()), body.clone()).unwrap();
        assert_eq!(ops.len(), 1);
        let op = &ops[0];
        assert_eq!(
            op.counter, 1,
            "counter should come from operation_counter(), expecting 1 operation but got {}", op.counter
        );
        match &op.operation {
            Operation::TransferTokens(tt) => {
                assert_eq!(tt.param, TypedValue::Bytes(body));
                assert_eq!(tt.destination_address, destination);
                assert_eq!(tt.amount, 0);
            }
            other => panic!("expected TransferTokens, got {other:?}"),
        }
    }

    #[test]
    fn test_dispatch_callback_out_of_gas() {
        let mut host = MockKernelHost::default();
        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        // Drain almost all gas so there isn't enough for the callback
        let remaining = ctx.operation_gas().remaining.milligas().unwrap();
        let to_consume = remaining - 1;
        ctx.operation_gas().remaining.consume(to_consume).unwrap();

        let destination = make_test_address();
        let result = dispatch_callback(&mut ctx, Some(destination), vec![]);
        assert!(matches!(result, Err(TransferError::OutOfGas(OutOfGas))));
    }

    #[test]
    fn test_dispatch_callback_counter_increments() {
        let mut host = MockKernelHost::default();
        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let mut ctx = MockCtx::new(&mut host, &mut journal, &registry, source, 0);
        // Simulate prior internal operations having consumed counters
        let _ = ctx.operation_counter(); // 1
        let _ = ctx.operation_counter(); // 2
        let destination = make_test_address();
        let ops = dispatch_callback(&mut ctx, Some(destination), vec![]).unwrap();
        assert_eq!(
            ops[0].counter, 3,
            "counter should follow previously consumed values"
        );
    }

    #[test]
    fn test_extract_callback_some() {
        let addr = make_test_address();
        let typed = TypedValue::new_option(Some(TypedValue::Contract(addr.clone())));
        let result = extract_callback(typed, "test").unwrap();
        assert_eq!(result, Some(addr));
    }

    #[test]
    fn test_extract_callback_none() {
        let typed = TypedValue::Option(None);
        let result = extract_callback(typed, "test").unwrap();
        assert_eq!(result, None);
    }

    #[test]
    fn test_extract_callback_wrong_type() {
        let typed = TypedValue::String("not an option".into());
        let result = extract_callback(typed, "test");
        assert!(result.is_err());
    }

    /// Multiple gateway calls within the same tx share the same CRAC ID
    /// because the CRAC-ID is determined by the top-level transaction.
    #[test]
    fn test_same_tx_gateway_calls_share_crac_id() {
        let mut host = MockKernelHost::default();
        let generated_alias = "KT1_mock_alias".to_string();
        let registry = MockRegistry::new(generated_alias);

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();

        let crac_id = CracId::new(1, 0);
        let mut journal = TezosXJournal::new(
            crac_id,
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let entrypoint = Entrypoint::try_from("call").unwrap();
        let arena = typed_arena::Arena::new();

        // Two gateway calls in the same tx
        let mut ctx1 = MockCtx::new(
            &mut host,
            &mut journal,
            &registry,
            source.clone(),
            10_000_000,
        );
        let dest_a = "0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
        execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &entrypoint,
            build_http_call_micheline(
                &arena,
                &format!("http://ethereum/{dest_a}"),
                &[],
                &[],
                1,
            ),
            &mut ctx1,
        )
        .unwrap();

        let mut ctx2 =
            MockCtx::new(&mut host, &mut journal, &registry, source, 10_000_000);
        let dest_b = "0xBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";
        execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &entrypoint,
            build_http_call_micheline(
                &arena,
                &format!("http://ethereum/{dest_b}"),
                &[],
                &[],
                1,
            ),
            &mut ctx2,
        )
        .unwrap();

        // Both calls share the same journal CRAC-ID
        assert_eq!(
            *journal.crac_id(),
            crac_id,
            "journal CRAC-ID must remain unchanged across gateway calls"
        );
    }

    /// Verify that the journal carries the CRAC ID set at construction.
    #[test]
    fn test_journal_carries_crac_id() {
        let id = CracId::new(1, 5);
        let journal = TezosXJournal::new(
            id,
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        assert_eq!(*journal.crac_id(), id);
    }

    /// RFC Example 4: Backtracking.
    /// When a CRAC fails, applied events should be transformed to backtracked.
    #[test]
    fn test_crac_event_backtracking() {
        let gateway_kt1 = ContractKt1Hash::try_from_bytes(&GATEWAY_ADDRESS).unwrap();
        let crac_id = CracId::new(0, 10);
        let mut event = make_crac_event(&gateway_kt1, &crac_id);

        // Initially applied
        assert!(event.is_applied());

        // Transform to backtracked (simulates failure in a later internal op)
        event.transform_result_backtrack();

        // Should no longer be applied
        assert!(!event.is_applied());
    }

    /// Verify CRAC event payload encodes the CRAC ID as "runtime_id-tx_index".
    #[test]
    fn test_crac_event_payload_format() {
        let gateway_kt1 = ContractKt1Hash::try_from_bytes(&GATEWAY_ADDRESS).unwrap();
        let crac_id = CracId::new(0, 3);
        let event = make_crac_event(&gateway_kt1, &crac_id);

        let InternalOperationSum::Event(ref e) = event else {
            panic!("expected Event");
        };

        // The payload is Micheline-encoded. Decode it to verify the string.
        let payload_bytes = e.content.payload.as_ref().unwrap();
        let parser = mir::parser::Parser::new();
        let decoded =
            Micheline::decode_raw(&parser.arena, &payload_bytes.0, &mut Gas::default())
                .unwrap()
                .unwrap();

        match decoded {
            Micheline::String(s) => {
                assert_eq!(
                    s, "0-3",
                    "payload should be CRAC ID in runtime_id-tx_index format"
                );
            }
            other => panic!("expected Micheline::String, got {other:?}"),
        }
    }

    // --- From<TransferError> for CracError routing ---
    //
    // These tests pin the routing decision that unifies the regular Tezos
    // pipeline and the gateway HTTP path: kernel-internal failures must
    // route to BlockAbort (block discarded on both paths), and user-facing
    // failures must route to Operation (op-level revert, block continues).

    /// Every infra-class TransferError variant must route to BlockAbort.
    /// If a new infra variant is added, add it here too.
    #[test]
    fn test_infra_variants_route_to_block_abort() {
        let infra: Vec<TransferError> = vec![
            TransferError::FailedToApplyBalanceChanges,
            TransferError::FailedToAllocateDestination,
            TransferError::FailedToFetchDestinationAccount,
            TransferError::FailedToFetchContractCode,
            TransferError::FailedToFetchContractStorage,
            TransferError::FailedToFetchDestinationBalance,
            TransferError::FailedToFetchSenderBalance,
            TransferError::FailedToUpdateContractStorage,
            TransferError::FailedToUpdateDestinationBalance,
            TransferError::FailedToComputeBalanceUpdate(String::new()),
        ];
        for variant in infra {
            let label = format!("{variant:?}");
            match CracError::from(variant) {
                CracError::BlockAbort(_) => {}
                CracError::Operation(_) => {
                    panic!("{label} must route to BlockAbort, not Operation")
                }
            }
        }
    }

    /// User-facing TransferError variants must route to Operation so the
    /// regular pipeline produces an op-level Failed receipt and the
    /// gateway path returns 4xx (catchable revert).
    #[test]
    fn test_user_facing_variants_route_to_operation() {
        use tezos_protocol::contract::Contract;
        let kt1 =
            ContractKt1Hash::from_base58_check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton")
                .unwrap();
        let user_facing: Vec<TransferError> = vec![
            TransferError::OutOfGas(OutOfGas),
            TransferError::EmptyImplicitTransfer,
            TransferError::NonSmartContractExecutionCall,
            TransferError::MirAddressUnsupportedError,
            TransferError::ContractDoesNotExist(Contract::Originated(kt1)),
        ];
        for variant in user_facing {
            let label = format!("{variant:?}");
            match CracError::from(variant) {
                CracError::Operation(_) => {}
                CracError::BlockAbort(_) => {
                    panic!("{label} must route to Operation, not BlockAbort")
                }
            }
        }
    }

    /// Stub that returns a fixed origin classification regardless of address.
    /// Used to drive `read_and_resolve_routing` without instantiating a full ctx.
    struct OriginLookupStub(Option<Origin>);

    impl HasOriginLookup for OriginLookupStub {
        fn read_origin_for_address(
            &self,
            _address: &AddressHash,
        ) -> Result<Option<Origin>, tezos_storage::error::Error> {
            Ok(self.0.clone())
        }
    }

    fn alias_origin(runtime: RuntimeId, native_address: &[u8]) -> Origin {
        Origin::Alias(AliasInfo {
            runtime,
            native_address: native_address.to_vec(),
        })
    }

    fn some_address() -> AddressHash {
        AddressHash::from_base58_check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap()
    }

    #[test]
    fn routing_returns_round_trip_for_matching_alias() {
        let stub = OriginLookupStub(Some(alias_origin(
            RuntimeId::Ethereum,
            b"0xabcdef0123456789abcdef0123456789abcdef01",
        )));
        match read_and_resolve_routing(&stub, &some_address(), RuntimeId::Ethereum)
            .unwrap()
        {
            RoutingDecision::RoundTrip(target) => {
                assert_eq!(target, "0xabcdef0123456789abcdef0123456789abcdef01")
            }
            other => panic!(
                "expected RoundTrip, got {:?}",
                std::mem::discriminant(&other)
            ),
        }
    }

    #[test]
    fn routing_returns_native_for_native_origin() {
        let stub = OriginLookupStub(Some(Origin::Native));
        assert!(matches!(
            read_and_resolve_routing(&stub, &some_address(), RuntimeId::Ethereum)
                .unwrap(),
            RoutingDecision::Native,
        ));
    }

    #[test]
    fn routing_returns_native_for_unrecorded_source() {
        let stub = OriginLookupStub(None);
        assert!(matches!(
            read_and_resolve_routing(&stub, &some_address(), RuntimeId::Ethereum)
                .unwrap(),
            RoutingDecision::Native,
        ));
    }

    #[test]
    fn routing_returns_transitive_for_mismatched_runtime() {
        // Path-independence: the recorded info is the basis for
        // derivation toward a third target. Unreachable in two
        // runtime mode.
        let stub = OriginLookupStub(Some(alias_origin(RuntimeId::Tezos, b"tz1abcdef")));
        match read_and_resolve_routing(&stub, &some_address(), RuntimeId::Ethereum)
            .unwrap()
        {
            RoutingDecision::Transitive(info) => {
                assert_eq!(info.runtime, RuntimeId::Tezos);
                assert_eq!(info.native_address, b"tz1abcdef".to_vec());
            }
            other => panic!(
                "expected Transitive, got {:?}",
                std::mem::discriminant(&other)
            ),
        }
    }

    #[test]
    fn test_tezosx_resolve_source_alias_readonly() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let mut journal = TezosXJournal::new(
            CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let ctx = MockCtx::new(
            &mut host,
            &mut journal,
            &registry,
            source.clone(),
            10_000_000,
        );
        let (_alias, runtime) = tezosx_resolve_source_alias_readonly(
            &ctx,
            &registry,
            &source,
            RuntimeId::Tezos,
        )
        .unwrap();
        // Tezos target short-circuits to the source's own (Tezos) runtime.
        assert_eq!(runtime, RuntimeId::Tezos);
        // The registry is read from but not written to: the handler only
        // needs to query the alias for routing resolution.
        assert!(registry.serve_calls.borrow().is_empty());
    }

    #[test]
    fn resolve_source_alias_readonly_round_trip_reports_target_runtime() {
        // An EVM-origin alias resolves back to its native EVM address
        // (`RoundTrip`), so the reported runtime is the target (Ethereum)
        // rather than defaulting `X-Tezos-Source-Runtime` to Tezos.
        let stub = OriginLookupStub(Some(alias_origin(
            RuntimeId::Ethereum,
            b"0xabcdef0123456789abcdef0123456789abcdef01",
        )));
        let registry = MockRegistry::new("unused");
        let (alias, runtime) = tezosx_resolve_source_alias_readonly(
            &stub,
            &registry,
            &some_address(),
            RuntimeId::Ethereum,
        )
        .unwrap();
        assert_eq!(alias, "0xabcdef0123456789abcdef0123456789abcdef01");
        assert_eq!(runtime, RuntimeId::Ethereum);
    }

    #[test]
    fn resolve_source_alias_readonly_native_reports_tezos() {
        // A Tezos-native source resolves to a Tezos runtime tag.
        let stub = OriginLookupStub(Some(Origin::Native));
        let registry = MockRegistry::new("KT1_alias");
        let (_alias, runtime) = tezosx_resolve_source_alias_readonly(
            &stub,
            &registry,
            &some_address(),
            RuntimeId::Ethereum,
        )
        .unwrap();
        assert_eq!(runtime, RuntimeId::Tezos);
    }

    // ── originOf / resolveAddress synthetic views ─────────────────────────────

    use tezosx_interfaces::testing::StubRegistry;

    fn make_gas(milligas: u64) -> crate::gas::TezlinkOperationGas {
        crate::gas::TezlinkOperationGas::start_milligas(milligas).unwrap()
    }

    /// Decode an `originOf` result (type `or unit (or nat (pair nat string))`)
    /// into a human-readable tuple for test assertions.
    /// Returns `(kind, home_runtime_u64, native_str)` where:
    ///   kind 0 = Unknown, 1 = Native, 2 = Alias.
    fn decode_origin_of(tv: &TypedValue<'_>) -> (u64, u64, String) {
        use mir::ast::or::Or;
        let TypedValue::Or(outer) = tv else {
            panic!("expected Or, got: {tv:?}")
        };
        match outer {
            Or::Left(_) => (0, 0, String::new()), // Unknown
            Or::Right(right) => {
                let TypedValue::Or(inner) = right.as_ref() else {
                    panic!("expected inner Or")
                };
                match inner {
                    Or::Left(nat) => {
                        let TypedValue::Nat(n) = nat.as_ref() else {
                            panic!("expected Nat")
                        };
                        let runtime_u64 =
                            if n.is_zero() { 0 } else { n.to_u64_digits()[0] };
                        (1, runtime_u64, String::new()) // Native
                    }
                    Or::Right(pair) => {
                        let TypedValue::Pair(home_rc, addr_rc) = pair.as_ref() else {
                            panic!("expected Pair")
                        };
                        let TypedValue::Nat(n) = home_rc.as_ref() else {
                            panic!("expected Nat")
                        };
                        let TypedValue::String(s) = addr_rc.as_ref() else {
                            panic!("expected String")
                        };
                        let home = if n.is_zero() { 0 } else { n.to_u64_digits()[0] };
                        (2, home, s.clone()) // Alias
                    }
                }
            }
        }
    }

    /// Decode a `resolveAddress` result (type `option (pair nat string)`)
    /// into `None` or `Some((resolution_nat, translated_str))`.
    fn decode_resolve_address(tv: &TypedValue<'_>) -> Option<(u64, String)> {
        let TypedValue::Option(inner) = tv else {
            panic!("expected Option, got: {tv:?}")
        };
        let pair_rc = inner.as_ref()?;
        let TypedValue::Pair(nat_rc, str_rc) = pair_rc.as_ref() else {
            panic!("expected Pair inside Option")
        };
        let TypedValue::Nat(n) = nat_rc.as_ref() else {
            panic!("expected Nat")
        };
        let TypedValue::String(s) = str_rc.as_ref() else {
            panic!("expected String")
        };
        let resolution = if n.is_zero() { 0 } else { n.to_u64_digits()[0] };
        Some((resolution, s.clone()))
    }

    // ── originOf tests ─────────────────────────────────────────────────────

    #[test]
    fn origin_of_tezos_native() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Native);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
            &num_bigint::BigUint::from(0u64), // Tezos = 0
        );
        let tv = result.expect("should succeed");
        // Expected: Right (Left (Nat 0)) — Native(Tezos)
        let (kind, home, _) = decode_origin_of(&tv);
        assert_eq!(kind, 1, "expected Native");
        assert_eq!(home, 0, "home runtime should be Tezos (0)");
    }

    #[test]
    fn origin_of_tezos_alias_to_evm() {
        let host = MockKernelHost::default();
        let evm_addr = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
        let alias_info = tezosx_interfaces::AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: evm_addr.as_bytes().to_vec(),
        };
        let registry =
            StubRegistry::with_classification(Classification::Alias(alias_info));
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
            &num_bigint::BigUint::from(0u64),
        );
        let tv = result.expect("should succeed");
        // Expected: Right (Right (Pair(Nat(1), String(evm_addr)))) — Alias(Ethereum, addr)
        let (kind, home, native_str) = decode_origin_of(&tv);
        assert_eq!(kind, 2, "expected Alias");
        assert_eq!(home, 1, "home runtime should be Ethereum (1)");
        assert_eq!(native_str, evm_addr);
    }

    #[test]
    fn origin_of_evm_native() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Native);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            &num_bigint::BigUint::from(1u64), // Ethereum = 1
        );
        let tv = result.expect("should succeed");
        // Expected: Right (Left (Nat 1)) — Native(Ethereum)
        let (kind, home, _) = decode_origin_of(&tv);
        assert_eq!(kind, 1, "expected Native");
        assert_eq!(home, 1, "home runtime should be Ethereum (1)");
    }

    #[test]
    fn origin_of_evm_unknown_no_backstop() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            &num_bigint::BigUint::from(1u64),
        );
        let tv = result.expect("should succeed");
        // Expected: Left Unit — Unknown
        let (kind, _, _) = decode_origin_of(&tv);
        assert_eq!(kind, 0, "expected Unknown");
    }

    #[test]
    fn origin_of_evm_with_backstop_native() {
        let host = MockKernelHost::default();
        // When the code-presence back-stop fires on an unclassified EVM
        // address, `read_origin` classifies the address as Native. The
        // actual gas accounting (ALIAS_LOOKUP_COST) is tested in
        // `tezosx-ethereum-runtime` against the real EVM impl.
        // Here we verify that `dispatch_origin_of_get` correctly encodes
        // the Native result when `read_origin` returns Native.
        let registry = StubRegistry::with_classification(Classification::Native);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            &num_bigint::BigUint::from(1u64),
        );
        let tv = result.expect("should succeed");
        let (kind, home, _) = decode_origin_of(&tv);
        assert_eq!(kind, 1, "expected Native");
        assert_eq!(home, 1, "home runtime should be Ethereum (1)");
    }

    #[test]
    fn origin_of_evm_alias_to_tezos() {
        let host = MockKernelHost::default();
        let tezos_addr = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
        let alias_info = tezosx_interfaces::AliasInfo {
            runtime: RuntimeId::Tezos,
            native_address: tezos_addr.as_bytes().to_vec(),
        };
        let registry =
            StubRegistry::with_classification(Classification::Alias(alias_info));
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            &num_bigint::BigUint::from(1u64),
        );
        let tv = result.expect("should succeed");
        let (kind, home, native_str) = decode_origin_of(&tv);
        assert_eq!(kind, 2, "expected Alias");
        assert_eq!(home, 0, "home runtime should be Tezos (0)");
        assert_eq!(native_str, tezos_addr);
    }

    #[test]
    fn origin_of_invalid_runtime_id_returns_failwith() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "tz1abc",
            &num_bigint::BigUint::from(9u64), // 9 is not a valid RuntimeId
        );
        match result {
            Err(mir::interpreter::InterpretError::FailedWith(_, ref tv)) => {
                let TypedValue::Pair(msg, received) = tv else {
                    panic!("expected Pair payload, got: {tv:?}")
                };
                assert!(
                    matches!(msg.as_ref(), TypedValue::String(s) if s == "INVALID_RUNTIME_ID"),
                    "expected INVALID_RUNTIME_ID message"
                );
                assert!(
                    matches!(received.as_ref(), TypedValue::Nat(n) if *n == num_bigint::BigUint::from(9u64)),
                    "expected received nat = 9"
                );
            }
            other => panic!("expected FailedWith INVALID_RUNTIME_ID, got: {other:?}"),
        }
    }

    // ── resolveAddress tests ───────────────────────────────────────────────

    #[test]
    fn resolve_address_same_source_valid() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let mut gas = make_gas(10_000_000);
        let addr = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
        let result = dispatch_resolve_address_get(
            &host,
            &mut gas,
            &registry,
            addr,
            &num_bigint::BigUint::from(0u64), // source = Tezos
            &num_bigint::BigUint::from(0u64), // target = Tezos (same)
        );
        let tv = result.expect("should succeed");
        // Expected: Some(Pair(Nat(0 = Recorded), String(addr)))
        let resolved = decode_resolve_address(&tv);
        assert!(resolved.is_some(), "expected Some");
        let (res, translated) = resolved.unwrap();
        assert_eq!(res, 0, "expected Recorded");
        assert_eq!(translated, addr);
    }

    #[test]
    fn resolve_address_same_source_malformed() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_resolve_address_get(
            &host,
            &mut gas,
            &registry,
            "not-an-address",
            &num_bigint::BigUint::from(0u64),
            &num_bigint::BigUint::from(0u64),
        );
        let tv = result.expect("should succeed");
        // Expected: None (malformed)
        assert!(
            decode_resolve_address(&tv).is_none(),
            "expected None for malformed address"
        );
    }

    #[test]
    fn resolve_address_alias_direct_to_target() {
        let host = MockKernelHost::default();
        let evm_addr = "0xcccccccccccccccccccccccccccccccccccccccc";
        let alias_info = tezosx_interfaces::AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: evm_addr.as_bytes().to_vec(),
        };
        let registry =
            StubRegistry::with_classification(Classification::Alias(alias_info));
        let mut gas = make_gas(10_000_000);
        let result = dispatch_resolve_address_get(
            &host,
            &mut gas,
            &registry,
            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
            &num_bigint::BigUint::from(0u64), // source = Tezos
            &num_bigint::BigUint::from(1u64), // target = Ethereum
        );
        let tv = result.expect("should succeed");
        let resolved = decode_resolve_address(&tv);
        assert!(resolved.is_some(), "expected Some");
        let (res, translated) = resolved.unwrap();
        assert_eq!(res, 0, "expected Recorded");
        assert_eq!(translated, evm_addr);
    }

    #[test]
    fn resolve_address_native_derives_recorded() {
        let host = MockKernelHost::default();
        let derived = "0xdddddddddddddddddddddddddddddddddddddddd";
        let tezos_addr = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
        // Destination check returns an alias pointing back to source → Recorded.
        let dest_class = Classification::Alias(tezosx_interfaces::AliasInfo {
            runtime: RuntimeId::Tezos,
            native_address: tezos_addr.as_bytes().to_vec(),
        });
        let registry = StubRegistry::with_alias_and_expected_runtime(
            Classification::Native,
            derived,
            Some(dest_class),
            RuntimeId::Ethereum,
        );
        let mut gas = make_gas(10_000_000);
        let result = dispatch_resolve_address_get(
            &host,
            &mut gas,
            &registry,
            tezos_addr,
            &num_bigint::BigUint::from(0u64), // source = Tezos
            &num_bigint::BigUint::from(1u64), // target = Ethereum
        );
        let tv = result.expect("should succeed");
        let resolved = decode_resolve_address(&tv);
        assert!(resolved.is_some(), "expected Some");
        let (res, translated) = resolved.unwrap();
        assert_eq!(res, 0, "expected Recorded");
        assert_eq!(translated, derived);
    }

    #[test]
    fn resolve_address_native_derives_unrecorded() {
        let host = MockKernelHost::default();
        let derived = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee";
        let tezos_addr = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
        // Destination check returns Unknown → Derived (1).
        let registry = StubRegistry::with_alias_and_expected_runtime(
            Classification::Native,
            derived,
            Some(Classification::Unknown),
            RuntimeId::Ethereum,
        );
        let mut gas = make_gas(10_000_000);
        let result = dispatch_resolve_address_get(
            &host,
            &mut gas,
            &registry,
            tezos_addr,
            &num_bigint::BigUint::from(0u64), // source = Tezos
            &num_bigint::BigUint::from(1u64), // target = Ethereum
        );
        let tv = result.expect("should succeed");
        let resolved = decode_resolve_address(&tv);
        assert!(resolved.is_some(), "expected Some");
        let (res, translated) = resolved.unwrap();
        assert_eq!(res, 1, "expected Derived");
        assert_eq!(translated, derived);
    }

    #[test]
    fn resolve_address_unknown_returns_none() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_resolve_address_get(
            &host,
            &mut gas,
            &registry,
            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
            &num_bigint::BigUint::from(0u64),
            &num_bigint::BigUint::from(1u64),
        );
        let tv = result.expect("should succeed");
        assert!(
            decode_resolve_address(&tv).is_none(),
            "expected None for unknown address"
        );
    }

    // ── missing row tests ─────────────────────────────────────────────────

    #[test]
    fn origin_of_tezos_unknown_returns_unknown() {
        // Tezos source, no `/origin` record → Unknown (Left Unit).
        // No code-presence back-stop for Tezos sources (back-stop only applies to EVM).
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
            &num_bigint::BigUint::from(0u64), // source = Tezos
        );
        let tv = result.expect("should succeed");
        let (kind, _, _) = decode_origin_of(&tv);
        assert_eq!(kind, 0, "expected Unknown for Tezos with no origin record");
    }

    #[test]
    fn origin_of_malformed_addr_tezos_returns_unknown() {
        // Tezos source, malformed address (neither tz1*/KT1* nor 0x*) → Unknown.
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "not_a_valid_address_at_all",
            &num_bigint::BigUint::from(0u64), // source = Tezos
        );
        let tv = result.expect("should succeed");
        let (kind, _, _) = decode_origin_of(&tv);
        assert_eq!(
            kind, 0,
            "expected Unknown for malformed address (Tezos source)"
        );
    }

    #[test]
    fn origin_of_malformed_addr_evm_returns_unknown() {
        // EVM source, malformed address → Unknown.
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_origin_of_get(
            &host,
            &mut gas,
            &registry,
            "not_a_valid_address_at_all",
            &num_bigint::BigUint::from(1u64), // source = Ethereum
        );
        let tv = result.expect("should succeed");
        let (kind, _, _) = decode_origin_of(&tv);
        assert_eq!(
            kind, 0,
            "expected Unknown for malformed address (EVM source)"
        );
    }

    #[test]
    fn resolve_address_evm_unknown_with_bytecode_falls_into_derivation() {
        // EVM source, no `/origin` record, but non-empty bytecode → back-stop fires,
        // effective origin becomes Native, derivation runs.
        // No inverse alias on the destination side → Derived (1).
        // The actual gas accounting of the back-stop is tested in tezosx-ethereum-runtime.
        let host = MockKernelHost::default();
        let derived = "0xffffffffffffffffffffffffffffffffffffffff";
        // source_classification = Native (back-stop result), destination_classification = Unknown.
        let registry = StubRegistry::with_alias_and_expected_runtime(
            Classification::Native,
            derived,
            Some(Classification::Unknown),
            RuntimeId::Tezos,
        );
        let mut gas = make_gas(10_000_000);
        let result = dispatch_resolve_address_get(
            &host,
            &mut gas,
            &registry,
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            &num_bigint::BigUint::from(1u64), // source = Ethereum
            &num_bigint::BigUint::from(0u64), // target = Tezos
        );
        let tv = result.expect("should succeed");
        let resolved = decode_resolve_address(&tv);
        assert!(
            resolved.is_some(),
            "expected Some — backstop treats as Native"
        );
        let (res, translated) = resolved.unwrap();
        assert_eq!(res, RESOLUTION_DERIVED_NAT, "expected Derived (1)");
        assert_eq!(translated, derived);
    }

    #[test]
    fn resolve_address_native_evm_lowercases_basis_for_derivation() {
        // Mixed-case EVM hex input must be canonicalized to lowercase
        // before being passed as the basis to `compute_alias`, otherwise
        // a Michelson caller passing `0xDDdd...` and a Solidity caller
        // passing the same address would derive different KT1 aliases.
        // The StubRegistry asserts the basis passed to `compute_alias`.
        let host = MockKernelHost::default();
        let mixed_case = "0xDDddDDddDDddDDddDDddDDddDDddDDddDDddDDdd";
        let lowercase = "0xdddddddddddddddddddddddddddddddddddddddd";
        let derived = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
        let registry = StubRegistry::with_alias_and_expected_runtime(
            Classification::Native,
            derived,
            Some(Classification::Unknown),
            RuntimeId::Tezos,
        )
        .expecting_native_address(lowercase.as_bytes().to_vec());
        let mut gas = make_gas(10_000_000);
        let result = dispatch_resolve_address_get(
            &host,
            &mut gas,
            &registry,
            mixed_case,
            &num_bigint::BigUint::from(1u64), // source = Ethereum
            &num_bigint::BigUint::from(0u64), // target = Tezos
        );
        let tv = result.expect("should succeed");
        let resolved = decode_resolve_address(&tv);
        let (res, translated) = resolved.expect("expected Some");
        assert_eq!(res, RESOLUTION_DERIVED_NAT, "expected Derived (1)");
        assert_eq!(translated, derived);
    }

    #[test]
    fn resolve_address_invalid_runtime_id() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let mut gas = make_gas(10_000_000);
        let result = dispatch_resolve_address_get(
            &host,
            &mut gas,
            &registry,
            "tz1abc",
            &num_bigint::BigUint::from(99u64), // invalid
            &num_bigint::BigUint::from(1u64),
        );
        match result {
            Err(mir::interpreter::InterpretError::FailedWith(_, ref tv)) => {
                let TypedValue::Pair(msg, received) = tv else {
                    panic!("expected Pair payload, got: {tv:?}")
                };
                assert!(
                    matches!(msg.as_ref(), TypedValue::String(s) if s == "INVALID_RUNTIME_ID"),
                    "expected INVALID_RUNTIME_ID message"
                );
                assert!(
                    matches!(received.as_ref(), TypedValue::Nat(n) if *n == num_bigint::BigUint::from(99u64)),
                    "expected received nat = 99"
                );
            }
            other => panic!("expected FailedWith INVALID_RUNTIME_ID, got: {other:?}"),
        }
    }

    #[test]
    fn test_classify_and_charge_crac_response_rejects_storage_cost_header() {
        let response = http::Response::builder()
            .status(http::status::StatusCode::OK)
            .header(X_TEZOS_GAS_CONSUMED, "1000")
            .header(X_TEZOS_STORAGE_COST, "42")
            .body(vec![])
            .unwrap();
        let mut gas = crate::gas::TezlinkOperationGas::default();
        let result = classify_and_charge_crac_response(response, Some("tezos"), &mut gas);
        assert!(
            matches!(
                result,
                Err(CracError::BlockAbort(ref msg))
                 if msg.contains("X-Tezos-Storage-Cost")
                    && msg.contains("not yet supported")
                    && msg.contains("42")
            ),
            "expected GatewayError with storage-cost not-yet-supported message, got: {result:?}"
        );
    }

    // ── Synthetic CRAC marker predicate tests ──────────────────────────

    fn make_marker_event(tag: &str, null_sender: bool) -> InternalOperationSum {
        use mir::ast::annotations::NO_ANNS;
        use mir::lexer;
        use tezos_protocol::contract::Contract;
        use tezos_smart_rollup::types::PublicKeyHash;
        use tezos_tezlink::operation_result::{
            EventContent, EventSuccess, InternalContentWithMetadata,
        };

        let ty = Micheline::App(lexer::Prim::string, &[], NO_ANNS)
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let payload = Micheline::from("1-0".to_string())
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        let sender = if null_sender {
            Contract::Implicit(PublicKeyHash::from_b58check(crate::NULL_PKH).unwrap())
        } else {
            Contract::Originated(
                ContractKt1Hash::from_base58_check(
                    "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT",
                )
                .unwrap(),
            )
        };

        InternalOperationSum::Event(InternalContentWithMetadata {
            content: EventContent {
                tag: Some(mir::ast::Entrypoint::from_string_unchecked(tag.into())),
                payload: Some(payload.into()),
                ty: ty.into(),
            },
            sender,
            nonce: 0,
            result: ContentResult::Applied(EventSuccess {
                consumed_milligas: tezos_data_encoding::types::Narith(0u64.into()),
            }),
        })
    }

    #[test]
    fn synthetic_crac_marker_tag_begin_null_sender() {
        let iop = make_marker_event(SYNTHETIC_CRAC_EVENT_TAG, true);
        assert_eq!(
            synthetic_crac_marker_tag(&iop),
            Some(SYNTHETIC_CRAC_EVENT_TAG),
            "null sender + tag 'cross_runtime_call' must return Some(SYNTHETIC_CRAC_EVENT_TAG)"
        );
    }

    #[test]
    fn synthetic_crac_marker_tag_end_null_sender() {
        let iop = make_marker_event(SYNTHETIC_CRAC_END_EVENT_TAG, true);
        assert_eq!(
            synthetic_crac_marker_tag(&iop),
            Some(SYNTHETIC_CRAC_END_EVENT_TAG),
            "null sender + tag 'cross_runtime_call_end' must return Some(SYNTHETIC_CRAC_END_EVENT_TAG)"
        );
    }

    #[test]
    fn synthetic_crac_marker_tag_deposit_null_sender_returns_none() {
        // Tag conjunction: null sender with tag "deposit" (bridge event)
        // must return None — the tag conjunct is load-bearing.
        let iop = make_marker_event("deposit", true);
        assert_eq!(
            synthetic_crac_marker_tag(&iop),
            None,
            "null sender + tag 'deposit' must return None (tag conjunction is load-bearing)"
        );
    }

    #[test]
    fn synthetic_crac_marker_tag_begin_kt1_sender_returns_none() {
        // User EMIT with tag "cross_runtime_call" but non-null sender must return None.
        let iop = make_marker_event(SYNTHETIC_CRAC_EVENT_TAG, false);
        assert_eq!(
            synthetic_crac_marker_tag(&iop),
            None,
            "KT1 sender + tag 'cross_runtime_call' must return None (sender must be null)"
        );
    }

    #[test]
    fn synthetic_crac_marker_tag_end_kt1_sender_returns_none() {
        // Forged user EMIT with tag "cross_runtime_call_end" from a KT1 must return None.
        let iop = make_marker_event(SYNTHETIC_CRAC_END_EVENT_TAG, false);
        assert_eq!(
            synthetic_crac_marker_tag(&iop),
            None,
            "KT1 sender + tag 'cross_runtime_call_end' must return None (sender must be null)"
        );
    }
}
