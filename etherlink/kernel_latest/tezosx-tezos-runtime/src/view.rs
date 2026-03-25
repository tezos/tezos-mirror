// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Execution of Michelson views reachable through the cross-runtime HTTP
//! protocol.
//!
//! A view call is an HTTP GET on `http://tezos/<kt1>/<view_name>`. The
//! request body carries the view's input as Micheline bytes (empty body →
//! `Unit`). The Micheline-encoded result is deposited on the topmost
//! external checkpoint of the CRAC journal (same `frame_result` slot the
//! Michelson `%collect_result` entry fills for entrypoint calls); the
//! outer `serve()` peeks that slot to build the HTTP response body.
//!
//! Views are read-only: no value transfer is accepted, no CRAC receipt is
//! produced, and MIR rejects side-effectful instructions inside view code
//! at typechecking time.

use std::collections::BTreeMap;
use std::rc::Rc;

use mir::ast::big_map::BigMapId;
use mir::ast::{AddressHash, IntoMicheline, Micheline, Type, TypedValue};
use mir::context::{CtxTrait, TypecheckingCtx};
use mir::interpreter::InterpretError;
use mir::parser::Parser;
use mir::typechecker::{typecheck_value, typecheck_view, TcError};
use tezos_crypto_rs::hash::OperationHash;
use tezos_data_encoding::types::{Narith, Zarith};
use tezos_execution::account_storage::{
    TezlinkAccount, TezlinkOriginatedAccount, TezosOriginatedAccount,
};
use tezos_execution::context::Context;
use tezos_execution::mir_ctx::{Ctx, ExecCtx, OperationCtx, TcCtx};
use tezos_execution::{OriginationNonce, TezlinkOperationGas};
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::TezosXRuntimeError;
use tezosx_journal::TezosXJournal;

use crate::context::TezosRuntimeContext;
use crate::{headers, url, NULL_PKH, TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH};

/// Classify a MIR typechecker error into the right HTTP-surface error.
///
/// The invariant here is that **every** error must map to a catchable
/// 4XX status — a 5XX would abort the whole block, and view
/// typechecking iterates over types declared by the target contract,
/// so an attacker could otherwise deploy a contract with deeply-nested
/// storage/view types and force an uncatchable failure during
/// `parse_ty`.
///
/// `OutOfGas` is the one variant that needs specific routing
/// (→ [`TezosXRuntimeError::OutOfGas`], catchable 429). Every other
/// `TcError` is a deterministic, caller-visible failure (malformed
/// input, type mismatch, unknown entrypoint, ...) and maps to
/// [`TezosXRuntimeError::BadRequest`] (→ 400).
fn classify_tc_error(e: TcError) -> TezosXRuntimeError {
    match e {
        TcError::OutOfGas => TezosXRuntimeError::OutOfGas,
        other => TezosXRuntimeError::BadRequest(format!("{other:?}")),
    }
}

/// Classify a MIR interpreter error. Same invariant as
/// [`classify_tc_error`]: every error must map to a catchable 4XX.
/// `OutOfGas` (including the variant nested under `TcError`) routes to
/// [`TezosXRuntimeError::OutOfGas`]; everything else defaults to
/// [`TezosXRuntimeError::BadRequest`].
fn classify_interpret_error(e: InterpretError) -> TezosXRuntimeError {
    match e {
        InterpretError::OutOfGas => TezosXRuntimeError::OutOfGas,
        InterpretError::TcError(TcError::OutOfGas) => TezosXRuntimeError::OutOfGas,
        other => TezosXRuntimeError::BadRequest(format!("{other:?}")),
    }
}

/// Execute a cross-runtime view call.
///
/// The URL must have the shape `http://tezos/<kt1>/<view_name>`. Views are
/// only defined on originated (KT1) contracts; calls targeting an implicit
/// account are rejected. `X-Tezos-Amount` must be `0` — views cannot
/// receive value.
///
/// The Micheline-encoded result is deposited on the topmost external
/// checkpoint of the CRAC [`TezosXJournal`] via
/// [`MichelsonJournal::set_frame_result`] — exactly the slot that the
/// entrypoint path fills via the Michelson `%collect_result` entry.
/// [`TezosRuntime::serve`] peeks that slot to build the HTTP body, so
/// view and entrypoint results travel through the same mechanism. This
/// also means the view observes the same journal state that an
/// entrypoint call would (storage writes are revertible through the
/// journal) rather than reading out of the operation-start snapshot.
///
/// `consumed_milligas` is updated with the gas consumed during
/// typechecking and interpretation, so the caller can report it back
/// in `X-Tezos-Gas-Consumed`.
pub fn execute_view_call<Host>(
    chain_id: &tezos_crypto_rs::hash::ChainId,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
    consumed_milligas: &mut u64,
) -> Result<(), TezosXRuntimeError>
where
    Host: StorageV1,
{
    let parsed = url::parse_tezos_view_url(request.uri())?;
    let hdrs = headers::parse_request_headers(request.headers())?;

    // Views do not accept value transfer.
    if hdrs.amount != Narith(0u64.into()) {
        return Err(TezosXRuntimeError::BadRequest(
            "views cannot receive value (X-Tezos-Amount must be 0)".into(),
        ));
    }

    let destination_kt1 = parsed.destination;
    let view_name = parsed.view_name;

    let context =
        TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;
    let dest_account = context.originated_from_kt1(&destination_kt1).map_err(|e| {
        TezosXRuntimeError::NotFound(format!(
            "destination contract {destination_kt1:?} not found: {e:?}"
        ))
    })?;

    // Read the destination's balance once (before `tc_ctx` takes a
    // mutable borrow on `host`) and feed it straight into
    // `ExecCtx.balance` so MIR's `BALANCE` opcode sees the contract
    // balance. `lookup_view_storage_balance` below re-reads balance
    // internally; the extra storage read is the price of keeping this
    // ctx initialized in one shot rather than patching a placeholder.
    let balance_narith = dest_account.balance(host).map_err(|e| {
        TezosXRuntimeError::Custom(format!("failed to read destination balance: {e:?}"))
    })?;
    let balance: i64 = balance_narith.0.try_into().map_err(|_| {
        TezosXRuntimeError::BadRequest("destination balance overflows i64".into())
    })?;

    let source_pkh = PublicKeyHash::from_b58check(NULL_PKH).map_err(|e| {
        TezosXRuntimeError::ConversionError(format!("failed to parse null address: {e}"))
    })?;
    let source_account =
        context
            .implicit_from_public_key_hash(&source_pkh)
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "failed to fetch source account: {e:?}"
                ))
            })?;

    // Any error here comes from an invalid `X-Tezos-Gas-Limit` header
    // (value above `MAX_LIMIT` or not a valid u32), i.e. a caller
    // mistake — surface it as a catchable 400 rather than a 500
    // `Custom`, and propagate the original error's message through its
    // `Display` impl.
    let mut gas = TezlinkOperationGas::start_milligas(hdrs.gas_limit)
        .map_err(|e| TezosXRuntimeError::BadRequest(e.to_string()))?;
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
    // Views have no notion of origination or of a running batch: no
    // internal operations are produced (typechecked away by `in_view`)
    // and no new contract can be originated, so both the operation
    // hash feeding the origination nonce and the per-operation counter
    // are left at default — they are never observed by view code.
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

    // Arena holding all Micheline allocations for typechecking and
    // interpretation. Its lifetime is bounded by this function's scope,
    // which matches the typechecked view code's `'a`.
    let parser = Parser::new();

    let body = request.into_body();
    let input_mich = if body.is_empty() {
        Micheline::from(())
    } else {
        Micheline::decode_raw(&parser.arena, &body).map_err(|e| {
            TezosXRuntimeError::BadRequest(format!(
                "failed to decode view input as Micheline: {e:?}"
            ))
        })?
    };

    // ExecCtx carries what MIR's VIEW semantics expect: self = dest,
    // sender = caller alias, amount = 0 (checked above), balance =
    // destination balance just read.
    let exec_ctx = ExecCtx {
        sender: AddressHash::Kt1(hdrs.sender.clone()),
        amount: 0,
        self_address: AddressHash::Kt1(destination_kt1.clone()),
        balance,
        contract_account: TezlinkOriginatedAccount {
            path: dest_account.path().clone(),
            kt1: dest_account.kt1().clone(),
        },
    };

    let mut mir_ctx = Ctx {
        tc_ctx: &mut tc_ctx,
        exec_ctx,
        operation_ctx: &mut operation_ctx,
    };

    // Wrap the MIR-consuming work in an IIFE so that `consumed_milligas`
    // is always refreshed from the gas counter after it runs — success or
    // failure. Without this, an early `?` (OOG, BadRequest, ...) would
    // leave `consumed_milligas` at the `u64::MAX` sentinel set by
    // [`TezosRuntime::serve`], burning the caller's entire gas budget on
    // what is otherwise a catchable revert.
    let mir_result: Result<Vec<u8>, TezosXRuntimeError> = (|| {
        let (view, storage_ty_mich, storage_bytes, _) = mir_ctx
            .lookup_view_storage_balance(&destination_kt1, &view_name, &parser.arena)
            .ok_or_else(|| {
                TezosXRuntimeError::NotFound(format!(
                    "view {view_name:?} not found on contract {destination_kt1:?}"
                ))
            })?;

        // Type parsing runs on types declared by the target contract — an
        // attacker-controllable surface. Route OOG here as OutOfGas so
        // malformed type declarations cannot abort the block.
        let storage_ty = storage_ty_mich
            .parse_ty(mir_ctx.gas())
            .map_err(classify_tc_error)?;
        let input_ty = view
            .input_type
            .parse_ty(mir_ctx.gas())
            .map_err(classify_tc_error)?;
        let output_ty = view
            .output_type
            .parse_ty(mir_ctx.gas())
            .map_err(classify_tc_error)?;

        // Typecheck the contract's stored storage against its declared type.
        // Use the module-level `typecheck_value` that takes a `&Type` so
        // that MIR doesn't re-parse `storage_ty` from a Micheline roundtrip
        // on every view call (saves 3 `parse_ty` over the hot path).
        let storage_mich =
            Micheline::decode_raw(&parser.arena, &storage_bytes).map_err(|e| {
                TezosXRuntimeError::Custom(format!("failed to decode storage: {e:?}"))
            })?;
        let storage = typecheck_value(&storage_mich, &mut mir_ctx, &storage_ty)
            .map_err(classify_tc_error)?;

        // Typecheck the caller-supplied input against the view's declared input type.
        let input = typecheck_value(&input_mich, &mut mir_ctx, &input_ty)
            .map_err(classify_tc_error)?;

        // Typecheck the view body. `typecheck_view` sets the `in_view` flag
        // that bars side-effectful instructions. Reaching the non-Seq
        // branch would mean the target contract's on-chain view is
        // structurally corrupt (L1 origination would have rejected it), so
        // we surface it as a server-side `Custom` error rather than blaming
        // the caller with a 400.
        let instrs = match view.code {
            Micheline::Seq(s) => s,
            _ => {
                return Err(TezosXRuntimeError::Custom(format!(
                    "view {view_name:?} on {destination_kt1:?} has non-Seq code; \
                     contract storage is corrupt"
                )));
            }
        };
        let code = typecheck_view(
            instrs,
            mir_ctx.gas(),
            Type::Pair(Rc::new((input_ty, storage_ty))),
            output_ty,
        )
        .map_err(classify_tc_error)?;

        let mut stk = mir::stk![TypedValue::new_pair(input, storage)];
        for instr in code.as_ref() {
            instr
                .interpret(&mut mir_ctx, &parser.arena, &mut stk)
                .map_err(classify_interpret_error)?;
        }

        let result_rc = stk.pop().ok_or_else(|| {
            TezosXRuntimeError::Custom("view execution left an empty stack".into())
        })?;
        let result = Rc::try_unwrap(result_rc).unwrap_or_else(|rc| (*rc).clone());

        result
            .into_micheline_optimized_legacy(&parser.arena)
            .encode()
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!("failed to encode view result: {e}"))
            })
    })();

    // Always report consumed gas, regardless of MIR success/failure:
    // `mir_ctx`'s mutable borrow ended when the IIFE returned, so the
    // gas counter is readable again here.
    *consumed_milligas = mir_ctx
        .tc_ctx
        .operation_gas
        .total_milligas_consumed()
        .into();

    let encoded = mir_result?;

    // Deposit the encoded view result on the topmost external
    // checkpoint so `TezosRuntime::serve` surfaces it through the
    // same `frame_result` slot that the entrypoint path uses. A
    // `NoFrame` error would only happen if `serve` were called
    // without REVM pushing a checkpoint first (current callers
    // always do); `AlreadySet` is impossible because the view path
    // writes the slot exactly once per frame and Michelson view
    // code cannot emit a `%collect_result` operation (typechecker
    // forbids side-effectful instructions with `in_view = true`).
    journal.michelson.set_frame_result(encoded).map_err(|e| {
        TezosXRuntimeError::Custom(format!(
            "failed to deposit view result into journal: {e}"
        ))
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_tc_error_out_of_gas() {
        assert!(matches!(
            classify_tc_error(TcError::OutOfGas),
            TezosXRuntimeError::OutOfGas
        ));
    }

    #[test]
    fn classify_tc_error_other_is_bad_request() {
        assert!(matches!(
            classify_tc_error(TcError::FailNotInTail),
            TezosXRuntimeError::BadRequest(_)
        ));
    }

    #[test]
    fn classify_interpret_error_out_of_gas() {
        assert!(matches!(
            classify_interpret_error(InterpretError::OutOfGas),
            TezosXRuntimeError::OutOfGas
        ));
    }

    #[test]
    fn classify_interpret_error_nested_tc_out_of_gas() {
        assert!(matches!(
            classify_interpret_error(InterpretError::TcError(TcError::OutOfGas)),
            TezosXRuntimeError::OutOfGas
        ));
    }

    #[test]
    fn classify_interpret_error_other_is_bad_request() {
        assert!(matches!(
            classify_interpret_error(InterpretError::MutezOverflow),
            TezosXRuntimeError::BadRequest(_)
        ));
    }
}
