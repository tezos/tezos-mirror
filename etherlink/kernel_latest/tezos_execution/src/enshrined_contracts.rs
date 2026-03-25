// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::Type;
use mir::ast::{
    Address, AddressHash, BinWriter, ByteReprTrait, Operation, OperationInfo,
    TransferTokens, TypedValue,
};
use mir::typechecker::typecheck_value;
use mir::{
    ast::{Entrypoint, Micheline},
    context::CtxTrait,
};
use num_bigint::{BigInt, BigUint};
use num_traits::{ToPrimitive, Zero};
use primitive_types::U256;
use sha3::{Digest, Keccak256};
use std::collections::HashMap;
use std::rc::Rc;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_tezlink::block::AppliedOperation;
use tezos_tezlink::operation_result::{InternalOperationSum, TransferError};
use tezosx_interfaces::{
    gas::convert as convert_gas, headers::format_tez_from_mutez, resolve_routing,
    AliasInfo, CrossRuntimeContext, Registry, RoutingDecision, RuntimeId,
    ERR_FORBIDDEN_TEZOS_HEADER, X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER, X_TEZOS_CRAC_ID,
    X_TEZOS_GAS_CONSUMED, X_TEZOS_GAS_LIMIT, X_TEZOS_SENDER, X_TEZOS_SOURCE,
    X_TEZOS_TIMESTAMP,
};
use tezosx_journal::TezosXJournal;

use crate::account_storage::TezlinkAccount;
use crate::mir_ctx::{
    HasContractAccount, HasHost, HasOperationGas, HasOriginLookup, HasSourcePublicKey,
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
            | TransferError::FailedToComputeBalanceUpdate => {
                CracError::BlockAbort(format!("internal error during transfer: {e}"))
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
              + HasOriginLookup),
    registry: &impl Registry,
    journal: &mut TezosXJournal,
) -> Result<Vec<OperationInfo<'a>>, CracError>
where
    Host: StorageV1,
{
    let typed = typecheck_entrypoint_value(contract, entrypoint, &value, ctx)?;
    match contract {
        EnshrinedContracts::TezosXGateway => {
            if entrypoint.is_default() {
                charge_gateway_base_cost(ctx)?;
                let TypedValue::String(dest) = typed else {
                    return Err(TransferError::GatewayError(
                        "Expected string for default entrypoint".into(),
                    )
                    .into());
                };
                let request = build_ethereum_request(&dest, &[])?;
                dispatch_crac_call(registry, journal, ctx, request)?;
                Ok(vec![])
            } else if entrypoint.as_str() == "call_evm" {
                charge_gateway_base_cost(ctx)?;
                let (dest, method_sig, abi_params, callback) =
                    extract_call_params(typed)?;
                // Per-byte surcharge: calldata (selector+params) going
                // out; response body is charged after it's returned.
                charge_gateway_payload(ctx, 4 + abi_params.len())?;
                ctx.operation_gas()
                    .cast_and_consume_milligas(SELECTOR_COMPUTATION_MILLIGAS)
                    .map_err(|_| TransferError::OutOfGas)?;
                let selector = compute_selector(&method_sig);
                let mut calldata = Vec::with_capacity(4 + abi_params.len());
                calldata.extend_from_slice(&selector);
                calldata.extend_from_slice(&abi_params);
                let request = build_ethereum_request(&dest, &calldata)?;
                let response_body = dispatch_crac_call(registry, journal, ctx, request)?;
                charge_gateway_payload(ctx, response_body.len())?;
                dispatch_callback(ctx, callback, response_body).map_err(Into::into)
            } else if entrypoint.as_str() == "call" {
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
                    .map_err(|_| TransferError::OutOfGas)?;
                let body = dispatch_crac_call(registry, journal, ctx, request)?;
                charge_gateway_payload(ctx, body.len())?;
                dispatch_callback(ctx, callback, body).map_err(Into::into)
            } else if entrypoint.as_str() == "collect_result" {
                // %collect_result lets a Michelson adapter deposit a
                // result payload on the current CRAC frame so the kernel
                // can later surface it as the HTTP response body to the
                // EVM gateway, giving EVM callers a synchronous return
                // value. Unlike the other gateway entrypoints it does not
                // trigger an HTTP round-trip, so `charge_gateway_base_cost`
                // does not apply.
                //
                // Reject any non-zero amount. %collect_result is not a
                // CRAC and has no recipient: allowing tez through it
                // would create a hidden fund side-channel and break EVM
                // sub-call semantics, where the return path carries
                // only bytes. Adapters that need to refund tez must do
                // so via an explicit CRAC (%transfer / %call_evm).
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
                    .map_err(|_| TransferError::OutOfGas)?;
                journal.michelson.set_frame_result(payload).map_err(|e| {
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
                "transfer" => "transfer(address,uint256)",
                "approve" => "approve(address,uint256)",
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
            dispatch_crac_call(registry, journal, ctx, request)?;
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

/// Durable storage read for alias lookup, equivalent to cold SLOAD
/// (2,100 EVM gas × 100).
pub(crate) const ALIAS_LOOKUP_MILLIGAS: u64 = 210_000;

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
fn charge_gateway_base_cost(ctx: &mut impl HasOperationGas) -> Result<(), TransferError> {
    ctx.operation_gas()
        .cast_and_consume_milligas(TEZOSX_GATEWAY_BASE_COST_MILLIGAS)
        .map_err(|_| TransferError::OutOfGas)
}

/// Charge `TEZOSX_GATEWAY_PER_BYTE_MILLIGAS * bytes` for a chunk of
/// gateway payload (outgoing body or response body). Complements the
/// flat `charge_gateway_base_cost` with a size-proportional component.
fn charge_gateway_payload(
    ctx: &mut impl HasOperationGas,
    bytes: usize,
) -> Result<(), TransferError> {
    let cost = TEZOSX_GATEWAY_PER_BYTE_MILLIGAS.saturating_mul(bytes as u64);
    if cost == 0 {
        return Ok(());
    }
    ctx.operation_gas()
        .cast_and_consume_milligas(cost)
        .map_err(|_| TransferError::OutOfGas)
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
        .map_err(|_| TransferError::OutOfGas)?;
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

/// Tag used by the synthetic CRAC-ID event built in
/// `tezosx-tezos-runtime/src/lib.rs::build_(failed_)crac_receipt`.
/// Imported by the receipt builder so both producer and consumer
/// agree on the tag.
pub const SYNTHETIC_CRAC_EVENT_TAG: &str = "crac";

/// Returns `true` if `iop` is the synthetic CRAC-ID event prepended to a
/// CRAC receipt by `build_crac_receipt` / `build_failed_crac_receipt`.
/// User-issued Michelson `EMIT` ops also reify as
/// `InternalOperationSum::Event` but the kernel stamps the synthetic
/// event with the null implicit sender (`crate::NULL_PKH`, set by
/// `build_crac_receipt`) on top of the canonical `"crac"` tag — both
/// must match.  A Michelson contract cannot impersonate the null
/// implicit sender, since user EMITs always carry the executing
/// contract's originated (KT1) sender via `sender_account.contract()`.
pub fn is_synthetic_crac_event(iop: &InternalOperationSum) -> bool {
    use tezos_protocol::contract::Contract;
    match iop {
        InternalOperationSum::Event(e) => {
            let sender_is_null = matches!(
                &e.sender,
                Contract::Implicit(pkh) if pkh.to_b58check() == crate::NULL_PKH
            );
            sender_is_null
                && e.content
                    .tag
                    .as_ref()
                    .is_some_and(|t| t.as_str() == SYNTHETIC_CRAC_EVENT_TAG)
        }
        _ => false,
    }
}

/// Drain re-entrant CRAC receipts that accumulated since the per-list
/// watermarks.
///
/// After each internal operation that may have triggered a cross-runtime
/// call (e.g. a gateway call that re-entered Michelson), this function
/// drains the CRAC receipts that were pushed since the watermarks —
/// across all three lists (pending, failed, backtracked) — and splices
/// their internal operation results into the parent op's flat list,
/// preserving DFS execution order (RFC Example 8).
///
/// Draining all three lists matters because a re-entrant inner CRAC
/// can land in any of them depending on its outcome and the EVM frame
/// catch behavior:
///   - Applied inner: pushed to `pending_crac_receipts`.
///   - Failed inner caught by an upstream EVM frame: pushed to
///     `failed_crac_receipts`, must still nest under its outer parent
///     in the receipt rather than reach the top-level merge with a
///     smaller seq than the outer (would invert DFS order — L2-1300).
///   - Applied inner whose enclosing EVM frame later reverted but was
///     caught above: migrated to `backtracked_crac_receipts` by
///     `revert_frame` (L2-1304); same nesting requirement.
///
/// Receipts are sorted by their shared sequence number before splicing
/// so cross-list interleavings of inner CRACs at the same depth come
/// out in execution order.
///
/// Synthetic CRAC-ID events are dropped from each spliced receipt
/// (only the outermost CRAC carries one per RFC principle 6); user-
/// issued `EMIT` ops are preserved.
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
                ops.extend(
                    result
                        .internal_operation_results
                        .into_iter()
                        .filter(|iop| !is_synthetic_crac_event(iop)),
                );
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
    amount_mutez: u64,
    gas_limit: u64,
    timestamp: u64,
    block_number: u32,
    crac_id: &str,
) -> Result<(), TransferError> {
    let parse_value = |v: &str| -> Result<http::HeaderValue, TransferError> {
        v.parse().map_err(|e| {
            TransferError::GatewayError(format!("invalid header value: {e}"))
        })
    };
    headers.insert(X_TEZOS_SENDER, parse_value(sender_alias)?);
    headers.insert(X_TEZOS_SOURCE, parse_value(source_alias)?);
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
              + HasOriginLookup),
    journal: &mut TezosXJournal,
    registry: &impl Registry,
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
    let source = AddressHash::from(ctx.source());
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
        .map_err(|_| TransferError::OutOfGas)?;
    // When sender == source (the common case for implicit account
    // transfers), pass the source pubkey so the alias is created with
    // the credential attached.
    let sender_is_source = sender == source;
    let sender_alias = 'sender: {
        let alias_info = match read_and_resolve_routing(ctx, &sender, target_runtime)? {
            RoutingDecision::RoundTrip(target) => break 'sender target,
            RoutingDecision::Transitive(info) => info,
            RoutingDecision::Native => AliasInfo {
                runtime: RuntimeId::Tezos,
                native_address: sender.to_base58_check().into_bytes(),
            },
        };
        let remaining_milligas =
            ctx.gas().milligas().ok_or(TransferError::OutOfGas)? as u64;
        // Convert remaining milligas to target runtime gas: this caps the budget
        // that ensure_alias may spend on alias generation.
        let target_budget =
            convert_gas(RuntimeId::Tezos, target_runtime, remaining_milligas)
                .ok_or(TransferError::OutOfGas)?;
        let sender_pubkey: Option<&[u8]> = if sender_is_source {
            Some(&source_public_key)
        } else {
            None
        };
        let (sender_alias, target_remaining) = registry
            .ensure_alias(
                ctx.host(),
                journal,
                alias_info,
                sender_pubkey,
                target_runtime,
                context.clone(),
                target_budget,
            )
            .map_err(|e| TransferError::GatewayError(e.to_string()))?;
        let sender_target_consumed = target_budget - target_remaining;
        let sender_milligas =
            convert_gas(target_runtime, RuntimeId::Tezos, sender_target_consumed)
                .ok_or(TransferError::OutOfGas)?;
        ctx.operation_gas()
            .cast_and_consume_milligas(sender_milligas)
            .map_err(|_| TransferError::OutOfGas)?;
        sender_alias
    };

    // --- source alias ---
    // Fast path: when sender == source (a user's implicit account calling
    // the gateway directly), reuse the resolved alias and skip the second
    // origin read. This saves one ALIAS_LOOKUP_MILLIGAS charge.
    let source_alias = if sender_is_source {
        sender_alias.clone()
    } else {
        ctx.operation_gas()
            .cast_and_consume_milligas(ALIAS_LOOKUP_MILLIGAS)
            .map_err(|_| TransferError::OutOfGas)?;
        'source: {
            let alias_info = match read_and_resolve_routing(ctx, &source, target_runtime)?
            {
                RoutingDecision::RoundTrip(target) => break 'source target,
                RoutingDecision::Transitive(info) => info,
                RoutingDecision::Native => AliasInfo {
                    runtime: RuntimeId::Tezos,
                    native_address: source.to_base58_check().into_bytes(),
                },
            };
            let remaining_milligas =
                ctx.gas().milligas().ok_or(TransferError::OutOfGas)? as u64;
            let target_budget =
                convert_gas(RuntimeId::Tezos, target_runtime, remaining_milligas)
                    .ok_or(TransferError::OutOfGas)?;
            let (source_alias, target_remaining) = registry
                .ensure_alias(
                    ctx.host(),
                    journal,
                    alias_info,
                    Some(&source_public_key),
                    target_runtime,
                    context,
                    target_budget,
                )
                .map_err(|e| TransferError::GatewayError(e.to_string()))?;
            let source_target_consumed = target_budget - target_remaining;
            let source_milligas =
                convert_gas(target_runtime, RuntimeId::Tezos, source_target_consumed)
                    .ok_or(TransferError::OutOfGas)?;
            ctx.operation_gas()
                .cast_and_consume_milligas(source_milligas)
                .map_err(|_| TransferError::OutOfGas)?;
            source_alias
        }
    };
    // Convert remaining Tezos milligas to the target runtime's units.
    // Use current remaining gas (not the pre-alias tezos_gas_limit) so the
    // forwarded limit reflects gas already consumed by alias resolution.
    let remaining_after_aliases =
        ctx.gas().milligas().ok_or(TransferError::OutOfGas)? as u64;
    let gas_limit =
        convert_gas(RuntimeId::Tezos, target_runtime, remaining_after_aliases)
            .ok_or_else(|| {
                TransferError::GatewayError(
                    "http_call: Tezos gas limit overflows target runtime units".into(),
                )
            })?;
    inject_context_headers_raw(
        request.headers_mut(),
        &sender_alias,
        &source_alias,
        amount_mutez,
        gas_limit,
        timestamp_u64,
        block_number_u32,
        &journal.crac_id().to_string(),
    )?;
    Ok(request)
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
    let gas_limit = ctx.gas().milligas().ok_or(TransferError::OutOfGas)? as u64;
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
fn dispatch_crac_call<'a, Host>(
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    ctx: &mut (impl CtxTrait<'a>
              + HasHost<Host>
              + HasContractAccount
              + HasOperationGas
              + HasSourcePublicKey
              + HasOriginLookup),
    request: http::Request<Vec<u8>>,
) -> Result<Vec<u8>, CracError>
where
    Host: StorageV1,
{
    if ctx.amount() < 0 {
        return Err(TransferError::GatewayError("Negative amount".into()).into());
    }

    let target_host = request.uri().host().map(str::to_string);
    let request = inject_context_headers(request, ctx, journal, registry)?;

    let response = registry.serve(ctx.host(), journal, request);
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
            .map_err(|_| TransferError::OutOfGas)?;
        let host = ctx.host();
        account
            .set_balance(host, &0u64.into())
            .map_err(|_| TransferError::FailedToApplyBalanceChanges)?;
    }
    Ok(response_body)
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
fn classify_and_charge_crac_response(
    response: http::Response<Vec<u8>>,
    target_host: Option<&str>,
    operation_gas: &mut crate::gas::TezlinkOperationGas,
) -> Result<Vec<u8>, CracError> {
    let callee_gas = extract_gas_consumed(&response, target_host).ok();

    if let Some(milligas) = callee_gas {
        operation_gas
            .cast_and_consume_milligas(milligas)
            .map_err(|_| TransferError::OutOfGas)?;
    }

    if response.status().is_success() {
        if callee_gas.is_none() {
            return Err(TransferError::GatewayError(
                "http_call: missing or invalid X-Tezos-Gas-Consumed header in response"
                    .into(),
            )
            .into());
        }
        Ok(response.into_body())
    } else if response.status().is_client_error() {
        Err(CracError::Operation(TransferError::GatewayError(format!(
            "Cross-runtime call failed with status {}: {}",
            response.status(),
            String::from_utf8_lossy(response.body())
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
            // default %default: string (destination address for simple transfers)
            entrypoints.insert(Entrypoint::default(), Type::String);
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
                        Type::List(Rc::new(Type::new_pair(Type::String, Type::String))),
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
            //   current CRAC frame.  Actual storage in the frame is
            //   handled in a follow-up issue; for now we accept and
            //   discard the value.
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
mod tests {
    use mir::ast::{AddressHash, Micheline};
    use mir::lexer::Prim;
    use num_bigint::BigInt;
    use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_tezlink::operation_result::{ContentResult, InternalOperationSum};
    use tezosx_interfaces::{Origin, RuntimeId};
    use tezosx_journal::TezosXJournal;

    use super::*;
    use crate::mir_ctx::mock::MockCtx;
    use crate::test_utils::{MockRegistry, MockRegistryWithStatus};

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
            .encode()
            .unwrap();
        let payload = Micheline::from(crac_id.to_string()).encode().unwrap();

        InternalOperationSum::Event(InternalContentWithMetadata {
            content: EventContent {
                tag: Some(mir::ast::Entrypoint::from_string_unchecked("crac".into())),
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result = dispatch_crac_call(
            &registry,
            &mut journal,
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount as i64);
        let result = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount);

        // First transfer creates aliases (sender + source)
        let result1 = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
        assert!(result1.is_ok());

        // Second transfer calls ensure_alias again. Dedup is the
        // responsibility of ensure_alias itself (Branch 1: returns
        // early when the alias account is already classified) — it
        // is no longer the resolver's concern.
        let result2 = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
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

        let mut journal = TezosXJournal::default();
        let mut ctx = MockCtx::new(&mut host, source, amount);

        let gas_before = ctx.operation_gas().remaining.milligas().unwrap();
        let result = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
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

        let mut journal = TezosXJournal::default();
        let mut ctx = MockCtx::new(&mut host, source, amount);

        let result1 = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
        assert!(result1.is_ok());
        let gas_after_first = ctx.operation_gas().remaining.milligas().unwrap();

        // Second call: ensure_alias is invoked again but its idempotent
        // Branch 1 returns gas_remaining unchanged. The fixed cost
        // bounds the per-transfer overhead.
        let result2 = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
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
    fn build_http_call_micheline<'a>(
        arena: &'a typed_arena::Arena<Micheline<'a>>,
        url: &str,
        headers: &[(&str, &str)],
        body: &[u8],
        method: i64,
    ) -> Micheline<'a> {
        let header_pairs: Vec<Micheline<'a>> = headers
            .iter()
            .map(|(name, value)| {
                Micheline::prim2(
                    arena,
                    Prim::Pair,
                    name.to_string().into(),
                    value.to_string().into(),
                )
            })
            .collect();
        let headers_seq = Micheline::Seq(arena.alloc_extend(header_pairs));
        let method_callback = Micheline::prim2(
            arena,
            Prim::Pair,
            num_bigint::BigInt::from(method).into(),
            Micheline::prim0(Prim::None),
        );
        let body_method_callback =
            Micheline::prim2(arena, Prim::Pair, body.to_vec().into(), method_callback);
        let inner_pair =
            Micheline::prim2(arena, Prim::Pair, headers_seq, body_method_callback);
        Micheline::prim2(arena, Prim::Pair, url.to_string().into(), inner_pair)
    }

    /// Typecheck a Micheline call value into a TypedValue.
    fn typecheck_call<'a>(
        value: &Micheline<'a>,
        host: &'a mut MockKernelHost,
    ) -> Result<TypedValue<'a>, TransferError> {
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut ctx = MockCtx::new(host, source, 0);
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
            42u64, // 42 mutez = 0.000042 TEZ
            1000,
            1700000000u64,
            5,
            &crac_id,
        )
        .unwrap();
        assert_eq!(headers.get("X-Tezos-Sender").unwrap(), "sender_alias");
        assert_eq!(headers.get("X-Tezos-Source").unwrap(), "source_alias");
        assert_eq!(headers.get("X-Tezos-Amount").unwrap(), "0.000042");
        assert_eq!(headers.get("X-Tezos-Gas-Limit").unwrap(), "1000");
        assert_eq!(headers.get("X-Tezos-Timestamp").unwrap(), "1700000000");
        assert_eq!(headers.get("X-Tezos-Block-Number").unwrap(), "5");
        assert_eq!(headers.get("X-Tezos-Crac-Id").unwrap(), "0-5");
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
            0u64,
            0,
            0u64,
            0,
            &crac_id,
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Negative amount"),
            "error should mention negative amount: {err}"
        );
    }

    // ── CRAC ID and event tests ─────────────────────────────────────────

    /// Outgoing CRAC via default entrypoint: the gateway dispatches the
    /// call to the EVM runtime.  CRAC events are emitted by the incoming
    /// receipt builder, not by the gateway itself.
    #[test]
    fn test_outgoing_crac_via_default_entrypoint() {
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount);

        let entrypoint = Entrypoint::default();
        let value = Micheline::String(dest.to_string());

        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &entrypoint,
            value,
            &mut ctx,
            &registry,
            &mut journal,
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount);

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
                    Micheline::prim0(Prim::None),
                ),
            ),
        );

        let entrypoint = Entrypoint::try_from("call_evm").unwrap();
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &entrypoint,
            value,
            &mut ctx,
            &registry,
            &mut journal,
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
        let registry = MockRegistryWithStatus::new(
            "KT1_mock_alias".to_string(),
            500,
            b"internal server error".to_vec(),
        );

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 100i64;
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
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
        let registry = MockRegistryWithStatus::new(
            "KT1_mock_alias".to_string(),
            400,
            b"bad request".to_vec(),
        );

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, 0);
        let result = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("bad request"),
            "error should contain response body: {err}"
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
            0u64,
            0,
            0u64,
            0,
            &crac_id,
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
            u64::MAX,
            u64::MAX,
            u64::MAX,
            u32::MAX,
            &crac_id,
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result = dispatch_crac_call(
            &registry,
            &mut journal,
            &mut ctx,
            build_ethereum_request(dest, &[]).unwrap(),
        );
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
        let mut ctx = MockCtx::new(&mut host, source, 0);
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
    fn test_typecheck_wrong_type_for_default_entrypoint() {
        let mut host = MockKernelHost::default();
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut ctx = MockCtx::new(&mut host, source, 0);
        // Default entrypoint expects a string, not bytes
        let value = Micheline::Bytes(vec![0x01, 0x02]);
        let result = typecheck_entrypoint_value(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::default(),
            &value,
            &mut ctx,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Invalid parameters"),
            "error should mention invalid parameters: {err}"
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
        let mut ctx = MockCtx::new(&mut host, source, 0);
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
        let mut ctx = MockCtx::new(&mut host, source, 0);
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        // %collect_result writes to the current CRAC frame's slot, which
        // the calling EVM frame would have created.  Simulate that here.
        journal.michelson.push_external_checkpoint();
        let mut ctx = MockCtx::new(&mut host, source, 0);
        let value = Micheline::Bytes(vec![0xCA, 0xFE]);
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            value,
            &mut ctx,
            &registry,
            &mut journal,
        );
        assert!(result.is_ok());
        // No serve calls are made: %collect_result only deposits bytes.
        assert!(registry.serve_calls.borrow().is_empty());
        // The payload is now observable on the current frame.
        assert_eq!(journal.michelson.frame_result(), Some(&[0xCA, 0xFE][..]));
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        // %collect_result writes to the current CRAC frame's slot, which
        // the calling EVM frame would have created.  Simulate that here.
        journal.michelson.push_external_checkpoint();
        let mut ctx = MockCtx::new(&mut host, source, 0);
        let value = Micheline::Bytes(vec![]);
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            value,
            &mut ctx,
            &registry,
            &mut journal,
        );
        assert!(result.is_ok());
        assert_eq!(journal.michelson.frame_result(), Some(&[][..]));
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

        // No `push_external_checkpoint` — dispatching %collect_result
        // outside a CRAC frame must surface the journal invariant as a
        // gateway error so the operation reverts cleanly.
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, 0);
        let value = Micheline::Bytes(vec![0xCA, 0xFE]);
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            value,
            &mut ctx,
            &registry,
            &mut journal,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("no active external checkpoint"),
            "error should mention missing frame: {err}"
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        // %collect_result writes to the current CRAC frame's slot, which
        // the calling EVM frame would have created.  Simulate that here.
        journal.michelson.push_external_checkpoint();
        let mut ctx = MockCtx::new(&mut host, source, 0);
        // First dispatch deposits the payload.
        let first = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0xCA, 0xFE]),
            &mut ctx,
            &registry,
            &mut journal,
        );
        assert!(first.is_ok());
        // Second dispatch on the same frame must fail: the slot is
        // write-once per frame, and a retry signals a misbehaving adapter.
        let second = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0xBE, 0xEF]),
            &mut ctx,
            &registry,
            &mut journal,
        );
        assert!(second.is_err());
        let err = second.unwrap_err();
        assert!(
            err.to_string().contains("frame result already set"),
            "error should mention already-set slot: {err}"
        );
        // The original payload is preserved — a failing retry doesn't
        // clobber what the frame already holds.
        assert_eq!(journal.michelson.frame_result(), Some(&[0xCA, 0xFE][..]));
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

        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        // Frame is set up; the rejection must happen even when a frame
        // exists, so it isn't masked by the no-frame check.
        journal.michelson.push_external_checkpoint();
        // Non-zero amount: %collect_result has no recipient and is not
        // a CRAC, so any positive amount must fail.
        let mut ctx = MockCtx::new(&mut host, source, 1);
        let value = Micheline::Bytes(vec![0xCA, 0xFE]);
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            value,
            &mut ctx,
            &registry,
            &mut journal,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("collect_result: amount must be 0"),
            "error should mention non-zero amount rejection: {err}"
        );
        // The frame's result slot stays empty — a rejected deposit must
        // not leak any payload into the response body.
        assert_eq!(journal.michelson.frame_result(), None);
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
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        journal.michelson.push_external_checkpoint();
        let mut ctx = MockCtx::new(&mut host, source, 0);
        ctx.operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(100_000).unwrap();
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0u8; 256]),
            &mut ctx,
            &registry,
            &mut journal,
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
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        journal.michelson.push_external_checkpoint();
        let mut ctx = MockCtx::new(&mut host, source, 0);
        ctx.operation_gas = crate::gas::TezlinkOperationGas::start_milligas(
            TYPECHECK_VALUE_STEP_MILLIGAS + COLLECT_RESULT_SIZE_BASE_MILLIGAS,
        )
        .unwrap();
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![]),
            &mut ctx,
            &registry,
            &mut journal,
        );
        assert!(result.is_ok());
        assert_eq!(ctx.operation_gas().remaining.milligas().unwrap(), 0);
    }

    // Budget covers the typecheck step but nothing more: the handler's
    // size charge trips OutOfGas before `set_frame_result`, so the
    // frame's result slot stays empty.
    #[test]
    fn test_collect_result_out_of_gas_on_size_charge() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        journal.michelson.push_external_checkpoint();
        let mut ctx = MockCtx::new(&mut host, source, 0);
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
            &registry,
            &mut journal,
        )
        .unwrap_err();
        assert!(matches!(err, CracError::Operation(TransferError::OutOfGas)));
        assert!(journal.michelson.frame_result().is_none());
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

    // The size charge fires before `set_frame_result`. If the write
    // then fails (here: no active frame), the gas stays consumed —
    // the kernel did the validation work and the caller pays for it.
    #[test]
    fn test_collect_result_charges_gas_when_no_frame() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        // No `push_external_checkpoint` — `set_frame_result` fails.
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        let mut ctx = MockCtx::new(&mut host, source, 0);
        ctx.operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(100_000).unwrap();
        let result = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0u8; 256]),
            &mut ctx,
            &registry,
            &mut journal,
        );
        assert!(result.is_err());
        // typecheck (100) + size base (460) + 1.5 * 256 (384) = 944.
        assert_eq!(ctx.operation_gas().total_milligas_consumed() as u64, 944);
        assert!(journal.michelson.frame_result().is_none());
    }

    // The size charge also fires on the failing second deposit when
    // the frame slot is already set: gas is consumed before the
    // `AlreadySet` branch of `set_frame_result` is reached.
    #[test]
    fn test_collect_result_charges_gas_when_already_set() {
        let mut host = MockKernelHost::default();
        let registry = MockRegistry::new("KT1_mock_alias".to_string());
        let source = AddressHash::Kt1(ContractKt1Hash::from([0u8; 20]));
        let mut journal = TezosXJournal::new(CracId::new(1, 0));
        journal.michelson.push_external_checkpoint();
        let mut ctx = MockCtx::new(&mut host, source, 0);
        ctx.operation_gas =
            crate::gas::TezlinkOperationGas::start_milligas(100_000).unwrap();
        // First deposit succeeds. 2-byte payload:
        // typecheck (100) + size base (460) + 1.5 * 2 (3) = 563.
        let first = execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("collect_result").unwrap(),
            Micheline::Bytes(vec![0xCA, 0xFE]),
            &mut ctx,
            &registry,
            &mut journal,
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
            &registry,
            &mut journal,
        );
        assert!(second.is_err());
        assert_eq!(
            ctx.operation_gas().total_milligas_consumed() as u64,
            after_first + 944
        );
        // Original payload is preserved.
        assert_eq!(journal.michelson.frame_result(), Some(&[0xCA, 0xFE][..]));
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
        let mut ctx = MockCtx::new(&mut host, source, 0);
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
        let mut ctx = MockCtx::new(&mut host, source, 0);
        // Drain almost all gas so there isn't enough for the callback
        let remaining = ctx.operation_gas().remaining.milligas().unwrap();
        let to_consume = remaining - 1;
        ctx.operation_gas().remaining.consume(to_consume).unwrap();

        let destination = make_test_address();
        let result = dispatch_callback(&mut ctx, Some(destination), vec![]);
        assert!(matches!(result, Err(TransferError::OutOfGas)));
    }

    #[test]
    fn test_dispatch_callback_counter_increments() {
        let mut host = MockKernelHost::default();
        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let mut ctx = MockCtx::new(&mut host, source, 0);
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
        let mut journal = TezosXJournal::new(crac_id);
        let entrypoint = Entrypoint::default();

        // Two gateway calls in the same tx
        let mut ctx1 = MockCtx::new(&mut host, source.clone(), 10_000_000);
        let dest_a = "0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
        execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &entrypoint,
            Micheline::String(dest_a.to_string()),
            &mut ctx1,
            &registry,
            &mut journal,
        )
        .unwrap();

        let mut ctx2 = MockCtx::new(&mut host, source, 10_000_000);
        let dest_b = "0xBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";
        execute_enshrined_contract(
            EnshrinedContracts::TezosXGateway,
            &entrypoint,
            Micheline::String(dest_b.to_string()),
            &mut ctx2,
            &registry,
            &mut journal,
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
        let journal = TezosXJournal::new(id);
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
        let decoded = Micheline::decode_raw(&parser.arena, &payload_bytes.0).unwrap();

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
            TransferError::FailedToComputeBalanceUpdate,
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
            TransferError::OutOfGas,
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
}
