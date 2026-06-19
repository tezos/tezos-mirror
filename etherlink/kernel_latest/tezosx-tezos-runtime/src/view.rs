// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Execution of Michelson views reachable through the cross-runtime HTTP
//! protocol.
//!
//! A view call is an HTTP GET on `http://tezos/<kt1>/<view_name>`. The
//! request body carries the view's input as Micheline bytes (empty body →
//! `Unit`). The Micheline-encoded result is deposited on the dispatch
//! slot of the CRAC journal (same slot the Michelson `%collect_result`
//! entry fills for entrypoint calls); the outer `serve()` takes that
//! slot to build the HTTP response body.
//!
//! Views are read-only: no value transfer is accepted, no CRAC receipt is
//! produced, and MIR rejects side-effectful instructions inside view code
//! at typechecking time.

use std::collections::BTreeMap;
use std::rc::Rc;

use mir::ast::big_map::BigMapId;
use mir::ast::{AddressHash, IntoMicheline, Micheline, Type, TypedValue};
use mir::context::{CtxTrait, TypecheckingCtx};
use mir::gas::OutOfGas;
use mir::interpreter::{EnshrinedViewDispatchError, InterpretError};
use mir::parser::Parser;
use mir::typechecker::{
    typecheck_value, typecheck_view, AllowForgedLazyStorageId, TcError,
};
use tezos_crypto_rs::hash::OperationHash;
use tezos_data_encoding::types::{Narith, Zarith};
use tezos_execution::account_storage::{
    TezlinkAccount, TezlinkOriginatedAccount, TezosOriginatedAccount,
};
use tezos_execution::context::Context;
use tezos_execution::mir_ctx::{Ctx, ExecCtx, InterpretContext, OperationCtx, TcCtx};
use tezos_execution::{OriginationNonce, TezlinkOperationGas};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::TezosXRuntimeError;
use tezosx_journal::TezosXJournal;

use crate::context::TezosRuntimeContext;
use crate::{
    headers, url, ExecuteRequestOutcome, RequestFailure, NULL_PKH, TEZOS_ACCOUNTS_ROOT,
};

/// Resource-exhaustion `TcError`s that must route to
/// [`TezosXRuntimeError::OutOfGas`]: direct budget exhaustion, a
/// cost-arithmetic overflow ("the cost is unrepresentably large",
/// catchable like OOG), and the comparison-cost path.
/// `CompareError::Incomparable` is a deterministic type failure and is
/// deliberately excluded.
///
/// Single-sourced so [`classify_tc_error`] and the nested-`TcError` arm
/// of [`classify_interpret_error`] cannot drift apart — the realistic
/// failure mode, since the two used to duplicate this set by hand. The
/// non-OOG side intentionally keeps a wildcard rather than enumerating
/// `TcError`'s ~40 deterministic variants (which would duplicate the
/// enum and add churn on every MIR change); a new *resource-exhaustion*
/// variant is the only thing that needs adding here, and that is a
/// reviewed, deliberate act.
fn tc_error_is_oog(e: &TcError) -> bool {
    use mir::gas::CompareError;
    matches!(
        e,
        TcError::OutOfGas(_)
            | TcError::CostOverflow(_)
            | TcError::CompareError(CompareError::Cost(_))
            | TcError::CompareError(CompareError::OutOfGas(_))
    )
}

/// Classify a MIR typechecker error into the right HTTP-surface error.
///
/// The invariant here is that **every** error must map to a catchable
/// 4XX status — a 5XX would abort the whole block, and view
/// typechecking iterates over types declared by the target contract,
/// so an attacker could otherwise deploy a contract with deeply-nested
/// storage/view types and force an uncatchable failure during
/// `parse_ty`.
///
/// `OutOfGas` (and the other resource-exhaustion shapes, see
/// [`tc_error_is_oog`]) needs specific routing
/// (→ [`TezosXRuntimeError::OutOfGas`], catchable 429). Every other
/// `TcError` is a deterministic, caller-visible failure (malformed
/// input, type mismatch, unknown entrypoint, ...) and maps to
/// [`TezosXRuntimeError::BadRequest`] (→ 400).
fn classify_tc_error(e: TcError) -> TezosXRuntimeError {
    if tc_error_is_oog(&e) {
        TezosXRuntimeError::OutOfGas
    } else {
        // Bounded like `classify_interpret_error`: a `TcError` can embed a
        // `Type` synthesised during typechecking whose render is far larger
        // than its node count (gas-bounded to `O(gas)`, since the
        // typechecker's `DUP` walks the unfolded type, but still large).
        // This arm feeds the catchable `BadRequest` body on the view path.
        TezosXRuntimeError::BadRequest(mir::bounded_fmt::debug_bounded(
            &e,
            mir::bounded_fmt::MAX_INTERPRET_ERROR_RENDER_BYTES,
        ))
    }
}

/// Classify a MIR interpreter error. Same invariant as
/// [`classify_tc_error`]: every error must map to a catchable 4XX
/// **except** kernel-side failures surfaced via
/// [`InterpretError::EnshrinedViewDispatch`] (alias resolution,
/// gas-conversion overflow, request-build, unclassifiable peer
/// response), which route to [`TezosXRuntimeError::Custom`] (→ 500).
/// The `InvalidDestination` arm of that enum is caller-controllable
/// (Michelson `string` allows characters that `http::Uri` rejects),
/// so it routes to `BadRequest` instead. `OutOfGas` (including the
/// variant nested under `TcError`) routes to
/// [`TezosXRuntimeError::OutOfGas`]; everything else defaults to
/// [`TezosXRuntimeError::BadRequest`].
fn classify_interpret_error(e: InterpretError) -> TezosXRuntimeError {
    use mir::gas::CompareError;
    // Match by reference: `InterpretError` implements `Drop` (a deep
    // `FailedWith` payload is flattened iteratively when the error is
    // finally dropped — see L2-1446 + !22025), so Rust forbids partial-move
    // destructuring. The arms borrow the variant fields and observe the
    // original payload; `e` falls out of scope at function end and its
    // `Drop` runs then, after any `format!("{other:?}")` above has already
    // serialised the value into the response body.
    match &e {
        // All flavours of resource exhaustion → OutOfGas: direct
        // interpreter budget exhaustion, a cost helper overflowing, the
        // comparison-cost path, and the nested-`TcError` forms (routed
        // through the shared `tc_error_is_oog` so this cannot drift from
        // `classify_tc_error`). `CompareError::Incomparable` (a
        // deterministic type failure) falls through to BadRequest.
        InterpretError::OutOfGas
        | InterpretError::CostOverflow(_)
        | InterpretError::CompareError(CompareError::Cost(_))
        | InterpretError::CompareError(CompareError::OutOfGas(_)) => {
            TezosXRuntimeError::OutOfGas
        }
        InterpretError::TcError(tc) if tc_error_is_oog(tc) => {
            TezosXRuntimeError::OutOfGas
        }
        InterpretError::EnshrinedViewDispatch(
            err @ EnshrinedViewDispatchError::InvalidDestination { .. },
        ) => TezosXRuntimeError::BadRequest(err.to_string()),
        InterpretError::EnshrinedViewDispatch(err) => {
            TezosXRuntimeError::Custom(err.to_string())
        }
        other => TezosXRuntimeError::BadRequest(mir::bounded_fmt::debug_bounded(
            other,
            mir::bounded_fmt::MAX_INTERPRET_ERROR_RENDER_BYTES,
        )),
    }
}

/// Execute a cross-runtime view call.
///
/// The URL must have the shape `http://tezos/<kt1>/<view_name>`. Views are
/// only defined on originated (KT1) contracts; calls targeting an implicit
/// account are rejected. `X-Tezos-Amount` must be `0` — views cannot
/// receive value.
///
/// The Micheline-encoded result is deposited on the CRAC
/// [`TezosXJournal`]'s dispatch slot via
/// [`MichelsonJournal::set_dispatch_result`] — exactly the slot that
/// the entrypoint path fills via the Michelson `%collect_result`
/// entry. [`TezosRuntime::serve`] takes that slot to build the HTTP
/// body, so view and entrypoint results travel through the same
/// mechanism. This also means the view observes the same journal
/// state that an entrypoint call would (storage writes are revertible
/// through the journal) rather than reading out of the
/// operation-start snapshot.
///
/// The returned [`ExecuteRequestOutcome`] carries the gas consumed during
/// typechecking and interpretation, and [`RequestFailure`] does the same
/// on failure, so the caller can report it to the gateway.
pub(crate) fn execute_view_call<Host>(
    chain_id: &tezos_crypto_rs::hash::ChainId,
    registry: &impl tezosx_interfaces::Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
) -> Result<ExecuteRequestOutcome, RequestFailure>
where
    Host: StorageV1,
{
    let parsed = url::parse_tezos_view_url(request.uri())?;
    let hdrs = headers::parse_request_headers(request.headers())?;

    // Views do not accept value transfer.
    if hdrs.amount != Narith(0u64.into()) {
        return Err(TezosXRuntimeError::BadRequest(
            "views cannot receive value (X-Tezos-Amount must be 0)".into(),
        )
        .into());
    }

    let destination_kt1 = parsed.destination;
    let view_name = parsed.view_name;

    let context = TezosRuntimeContext::from_root(&TEZOS_ACCOUNTS_ROOT)?;
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
        interpret_context: InterpretContext::new(),
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
        // Views cannot emit operations, so no internal-operation
        // replay can occur; the set is left empty.
        applied_counters: std::collections::BTreeSet::new(),
        level: &hdrs.block_number,
        now: &hdrs.timestamp,
        chain_id,
        source_public_key: &[],
        crac_chain_depth: hdrs.crac_depth,
        crac_origin: hdrs.crac_origin_contract.clone(),
        delegated_storage_cost: 0,
    };

    // Arena holding all Micheline allocations for typechecking and
    // interpretation. Its lifetime is bounded by this function's scope,
    // which matches the typechecked view code's `'a`.
    let parser = Parser::new();

    let body = request.into_body();
    let input_mich = if body.is_empty() {
        Micheline::from(())
    } else {
        Micheline::decode_raw(&parser.arena, &body, tc_ctx.gas())
            .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
            .map_err(|e| {
                TezosXRuntimeError::BadRequest(format!(
                    "failed to decode view input as Micheline: {e:?}"
                ))
            })?
    };

    // ExecCtx carries what MIR's VIEW semantics expect: self = dest,
    // sender = caller alias, amount = 0 (checked above), balance =
    // destination balance just read.
    let sender_address = match &hdrs.sender {
        Contract::Originated(kt1) => AddressHash::Kt1(kt1.clone()),
        Contract::Implicit(pkh) => AddressHash::Implicit(pkh.clone()),
    };
    let exec_ctx = ExecCtx {
        sender: sender_address,
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
        journal: &mut *journal,
        registry,
    };

    // Wrap the metered work so its gas can be read once after it runs. On
    // failure the metered figure is attached to the RequestFailure below,
    // so a fault after metering reports its real gas rather than unset.
    let mir_result: Result<Vec<u8>, TezosXRuntimeError> = (|| {
        let (view, storage_ty_mich, storage_bytes, _) = mir_ctx
            .lookup_view_storage_balance(&destination_kt1, &view_name, &parser.arena)
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "view lookup on contract {destination_kt1:?} failed: {e}"
                ))
            })?
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
            Micheline::decode_raw(&parser.arena, &storage_bytes, mir_ctx.gas())
                .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
                .map_err(|e| {
                    TezosXRuntimeError::Custom(format!("failed to decode storage: {e:?}"))
                })?;
        // The storage is the target contract's own committed state, so it may
        // legitimately reference big_maps it owns by id.
        let storage = typecheck_value(
            &storage_mich,
            &mut mir_ctx,
            &storage_ty,
            AllowForgedLazyStorageId::Yes,
        )
        .map_err(classify_tc_error)?;

        // Typecheck the caller-supplied input against the view's declared input
        // type. The input comes from outside the contract's own state, so
        // forged lazy-storage ids are rejected.
        let input = typecheck_value(
            &input_mich,
            &mut mir_ctx,
            &input_ty,
            AllowForgedLazyStorageId::No,
        )
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
            Type::new_pair(input_ty, storage_ty),
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
            .into_micheline_optimized_legacy(&parser.arena, mir_ctx.gas())
            .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
            .encode(mir_ctx.gas())
            .map_err(|OutOfGas| TezosXRuntimeError::OutOfGas)?
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!("failed to encode view result: {e}"))
            })
    })();

    // Release the inner `&mut` borrow on `staticcall_bridge` (and
    // through it on `journal`) before reading gas back from `tc_ctx`
    // and before the outer caller writes to `journal`.
    drop(mir_ctx);
    let consumed_milligas: u64 = tc_ctx.operation_gas.total_milligas_consumed().into();

    let encoded = mir_result.map_err(|error| RequestFailure {
        error,
        consumed_milligas: Some(consumed_milligas),
    })?;

    // Deposit the encoded view result on the dispatch slot that
    // `TezosRuntime::serve` opened for this call — the same slot the
    // entrypoint path fills via `%collect_result`. `NoSlot` is
    // impossible because `serve` always pushes the slot at entry;
    // `AlreadySet` is impossible because the view path writes the
    // slot exactly once per dispatch and Michelson view code cannot
    // emit a `%collect_result` operation (typechecker forbids
    // side-effectful instructions with `in_view = true`).
    journal
        .michelson
        .set_dispatch_result(encoded)
        .map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "failed to deposit view result into journal: {e}"
            ))
        })?;
    Ok(ExecuteRequestOutcome {
        delegated_storage_cost: 0,
        consumed_milligas,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_tc_error_out_of_gas() {
        assert!(matches!(
            classify_tc_error(TcError::OutOfGas(mir::gas::OutOfGas)),
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
            classify_interpret_error(InterpretError::TcError(TcError::OutOfGas(
                mir::gas::OutOfGas
            ))),
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

    #[test]
    fn classify_tc_error_cost_overflow_and_compare_cost_are_oog() {
        use mir::gas::{CompareError, CostOverflow};
        assert!(matches!(
            classify_tc_error(TcError::CostOverflow(CostOverflow)),
            TezosXRuntimeError::OutOfGas
        ));
        assert!(matches!(
            classify_tc_error(TcError::CompareError(CompareError::Cost(CostOverflow))),
            TezosXRuntimeError::OutOfGas
        ));
        // A COMPARE that exhausts the budget mid-walk surfaces as
        // CompareError::OutOfGas and must route to OutOfGas, not BadRequest.
        assert!(matches!(
            classify_tc_error(TcError::CompareError(CompareError::OutOfGas(
                mir::gas::OutOfGas
            ))),
            TezosXRuntimeError::OutOfGas
        ));
    }

    #[test]
    fn classify_tc_error_incomparable_is_bad_request() {
        use mir::gas::CompareError;
        assert!(matches!(
            classify_tc_error(TcError::CompareError(CompareError::Incomparable)),
            TezosXRuntimeError::BadRequest(_)
        ));
    }

    #[test]
    fn classify_interpret_error_cost_overflow_and_nested_tc_are_oog() {
        use mir::gas::{CompareError, CostOverflow};
        assert!(matches!(
            classify_interpret_error(InterpretError::CostOverflow(CostOverflow)),
            TezosXRuntimeError::OutOfGas
        ));
        assert!(matches!(
            classify_interpret_error(InterpretError::CompareError(CompareError::Cost(
                CostOverflow
            ))),
            TezosXRuntimeError::OutOfGas
        ));
        assert!(matches!(
            classify_interpret_error(InterpretError::CompareError(
                CompareError::OutOfGas(mir::gas::OutOfGas)
            )),
            TezosXRuntimeError::OutOfGas
        ));
        // Nested TcError OOG goes through the shared `tc_error_is_oog`.
        assert!(matches!(
            classify_interpret_error(InterpretError::TcError(TcError::CostOverflow(
                CostOverflow
            ))),
            TezosXRuntimeError::OutOfGas
        ));
    }

    #[test]
    fn classify_interpret_error_nested_tc_non_oog_is_bad_request() {
        assert!(matches!(
            classify_interpret_error(InterpretError::TcError(TcError::FailNotInTail)),
            TezosXRuntimeError::BadRequest(_)
        ));
    }

    #[test]
    fn classify_interpret_error_internal_error_is_bad_request() {
        use mir::interpreter::InterpretInvariant;
        // `InterpretError::InternalError` is the interpreter-side parallel
        // of `TcError::InternalError`: it surfaces invariant violations
        // unreachable for typechecked input. It must route to BadRequest
        // (catchable revert), matching how `TcError::InternalError`
        // already does — never silently to OutOfGas.
        for inv in [
            InterpretInvariant::EmptyValueStackPop,
            InterpretInvariant::TypeMismatchOnPop { expected: "V::Foo" },
            InterpretInvariant::TypeMismatchOnTop { expected: "V::Foo" },
            InterpretInvariant::TypeMismatch { expected: "V::Foo" },
            InterpretInvariant::UnreachableState,
            InterpretInvariant::ExpectedPairResult,
            InterpretInvariant::ExpectedListOperation,
            InterpretInvariant::ExpectedOperationElement,
        ] {
            assert!(matches!(
                classify_interpret_error(InterpretError::InternalError(inv)),
                TezosXRuntimeError::BadRequest(_)
            ));
        }
    }

    /// `EnshrinedViewDispatchError::InvalidDestination` carries caller-
    /// supplied data (the destination URL the Michelson program passed)
    /// — caller-controllable malformed input must route to
    /// [`TezosXRuntimeError::BadRequest`] (4xx, catchable revert) so the
    /// caller can recover. The other four `EnshrinedViewDispatchError`
    /// variants (`AliasResolution`, `BudgetOverflow`, `DispatchSetup`,
    /// `UnclassifiableResponse`) represent kernel-side brokenness
    /// reachable from a MIR `VIEW staticcall_evm` and must route to
    /// [`TezosXRuntimeError::Custom`], which the gateway maps to a 5xx
    /// blueprint-abort. Pin every routing so a future arm reorder or
    /// guard typo cannot silently flip them; the inline `match` over
    /// every variant is exhaustive, so a future 6th variant fails to
    /// compile until its expected routing is decided here.
    #[test]
    fn classify_interpret_error_enshrined_view_dispatch_routing() {
        use EnshrinedViewDispatchError::*;
        let cases = [
            InvalidDestination {
                destination: "not-a-url".to_owned(),
            },
            AliasResolution,
            BudgetOverflow,
            DispatchSetup,
            UnclassifiableResponse { status: 599 },
        ];
        for case in cases {
            // Exhaustive match: a future variant must add its routing
            // decision here AND to `cases` above, or this test fails to
            // compile.
            let expected_is_bad_request = match &case {
                InvalidDestination { .. } => true,
                AliasResolution
                | BudgetOverflow
                | DispatchSetup
                | UnclassifiableResponse { .. } => false,
            };
            let actual = classify_interpret_error(InterpretError::EnshrinedViewDispatch(
                case.clone(),
            ));
            if expected_is_bad_request {
                assert!(
                    matches!(actual, TezosXRuntimeError::BadRequest(_)),
                    "expected BadRequest for {case:?}, got {actual:?}",
                );
            } else {
                assert!(
                    matches!(actual, TezosXRuntimeError::Custom(_)),
                    "expected Custom for {case:?}, got {actual:?}",
                );
            }
        }
    }
}
