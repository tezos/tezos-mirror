// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
// SPDX-License-Identifier: MIT

use account_storage::Code;
use account_storage::Manager;
use account_storage::TezlinkAccount;
use enshrined_contracts::charge_internal_receipt_bodies;
use enshrined_contracts::charge_persisted_error;
use enshrined_contracts::get_enshrined_contract_entrypoint;
use enshrined_contracts::CracError;
use mir::ast::BinWriter;
use mir::ast::{AddressHash, Entrypoint, OperationInfo, TransferTokens, TypedValue};
use mir::context::TypecheckingCtx;
use mir::{
    ast::{big_map::BigMapId, IntoMicheline, Micheline},
    context::CtxTrait,
    gas::{Gas, OutOfGas},
    parser::Parser,
    typechecker::{AllowForgedLazyStorageId, TypecheckViews},
};
use num_bigint::{BigInt, BigUint};
use num_traits::ops::checked::CheckedSub;
use num_traits::{ToPrimitive, Zero};
use primitive_types::U256;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use tezos_crypto_rs::hash::OperationHash;
use tezos_crypto_rs::{hash::ContractKt1Hash, PublicKeyWithHash};
use tezos_data_encoding::types::Narith;
use tezos_ethereum::wei::michelson_gas_to_mutez;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::safe_storage::SafeStorage;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKey;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_storage::error::Error as StorageError;
use tezos_tezlink::lazy_storage_diff::LazyStorageDiffList;
use tezos_tezlink::operation::{
    ManagerOperationContentConv, ManagerOperationField as _, Operation,
    OriginationContent, Script,
};
use tezos_tezlink::operation_result::{
    produce_skipped_receipt, BacktrackedResult, ContentResult,
    InternalContentWithMetadata, InternalOperationSum, OperationKind, OperationResult,
    OperationWithMetadata, Originated, OriginationSuccess, TransferTarget,
};
use tezos_tezlink::{
    operation::{OperationContent, Parameters, RevealContent, TransferContent},
    operation_result::{
        ApplyOperationError, Balance, BalanceTooLow, BalanceUpdate, EventContent,
        EventSuccess, OperationError, OperationResultSum, OriginationError, RevealError,
        RevealSuccess, TransferError, TransferSuccess, UpdateOrigin,
    },
};
use tezosx_interfaces::{Origin, Registry};
use tezosx_journal::TezosXJournal;

use crate::account_storage::{
    OriginatedContractInfo, StorageSpace, TezosImplicitAccount,
    TezosImplicitAccountTrait, TezosOriginatedAccount,
};
pub use crate::address::OriginationNonce;
use crate::gas::Cost;
pub use crate::gas::TezlinkOperationGas;
use crate::mir_ctx::{
    clear_temporary_big_maps, convert_big_map_diff, BlockCtx, Ctx, ExecCtx,
    InterpretContext, OperationCtx, TcCtx,
};

/// Result of applying a single operation within a batch.
///
/// Wraps the serializable [`OperationWithMetadata`] together with
/// the amount of gas that was consumed during the execution of the operation.
/// This struct is internal, it is never serialized nor deserialized.
#[derive(Debug, PartialEq)]
pub struct ProcessedOperation {
    pub operation_with_metadata: OperationWithMetadata,
    /// Total gas consumed by this operation (including internal operations),
    /// in milligas.
    ///
    /// - For Applied/BackTracked/Failed: total gas consumed since the start
    ///   of the operation, computed from the gas tracker's initial limit and
    ///   remaining gas. This includes gas from all internal sub-operations.
    ///   For BackTracked operations, this reflects gas actually burned during
    ///   execution, even though the operation's effects were reverted (because
    ///   a later operation in the batch failed).
    /// - For Skipped: 0.
    pub operation_consumed_milligas: u32,
}

impl ProcessedOperation {
    /// Total milligas consumed across a batch of processed operations.
    /// In practice, individual operation gas is bounded by
    /// `hard_gas_limit_per_operation` (3_000_000 gas = 3_000_000_000 milligas
    /// in Tezos X, raised from the L1 default of 1_040_000 gas to match the
    /// EVM per-transaction cap of 30M gas), so neither per-operation values
    /// nor batch totals will overflow `u64`.
    pub fn total_consumed_milligas(ops: &[ProcessedOperation]) -> u64 {
        ops.iter()
            .map(|p| u64::from(p.operation_consumed_milligas))
            .sum()
    }

    /// Extract the operation-with-metadata from each processed operation,
    /// discarding the per-operation consumed milligas.
    pub fn into_receipts(ops: Vec<Self>) -> Vec<OperationWithMetadata> {
        ops.into_iter().map(|p| p.operation_with_metadata).collect()
    }
}

/// Fee pricing parameters for computing refunds inside
/// `validate_and_apply_operation`.
pub struct FeeRefundConfig {
    pub da_fees: u64,
    pub base_fee_per_gas: U256,
    pub michelson_to_evm_gas_multiplier: u64,
}

/// Compute the fee refund: surplus of declared fees beyond actual costs.
///
/// fee_refund = total_operation_fees - da_fees - consumed_gas_fees
/// where consumed_gas_fees = michelson_gas_to_mutez(base_fee, multiplier, consumed_gas)
/// and consumed_gas = ceil(total_consumed_milligas / 1000).
fn compute_fee_refund(
    total_operation_fees: u64,
    processed_operations: &[ProcessedOperation],
    config: &FeeRefundConfig,
) -> u64 {
    let consumed_milligas =
        ProcessedOperation::total_consumed_milligas(processed_operations);
    // Safe: consumed_milligas <= gas_limit * 1000, so ceil never exceeds gas_limit.
    let consumed_gas = consumed_milligas.div_ceil(1000);
    let consumed_gas_fees = michelson_gas_to_mutez(
        config.base_fee_per_gas,
        config.michelson_to_evm_gas_multiplier,
        consumed_gas,
    );

    // saturating_sub ensures the refund is non-negative and smaller than the fees
    // paid by the source account at the beginning of the application.
    total_operation_fees.saturating_sub(config.da_fees.saturating_add(consumed_gas_fees))
}

extern crate alloc;

/// Base58Check of the null implicit account
/// `tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU`.
///
/// Used as the kernel-stamped `SOURCE` field for cross-runtime requests
/// (entrypoint calls and view calls) and as the `sender` of the
/// synthetic CRAC-ID event prepended to CRAC receipts.  Michelson
/// requires `SOURCE` to be an implicit account (tz1/tz2/tz3); CRACs
/// have no natural implicit source, so this placeholder fills the
/// role.  User-issued Michelson `EMIT` ops cannot carry this sender
/// because `execute_internal_operations` stamps them with the
/// executing contract's originated (KT1) address.
pub const NULL_PKH: &str = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU";

pub mod account_storage;
mod address;
pub mod context;
pub mod enshrined_contracts;
mod gas;
pub mod mir_ctx;
pub mod storage_fees;
mod validate;

/// Where an internal operation entered the current frame's tree.
enum InternalOpOrigin {
    /// Operation directly emitted by the current frame.
    Own {
        /// Storage cost delegated to the operation.
        delegated_storage_cost: u64,
    },
    /// Operation surfaced by the CRAC drain mechanism.
    Crac,
}

/// An internal operation accumulated during the execution of a manager
/// operation, paired with the origin that produced it within the current
/// frame's tree.
struct TaggedInternalOp {
    op: InternalOperationSum,
    origin: InternalOpOrigin,
}

impl TaggedInternalOp {
    fn own(op: InternalOperationSum, delegated_storage_cost: u64) -> Self {
        Self {
            op,
            origin: InternalOpOrigin::Own {
                delegated_storage_cost,
            },
        }
    }

    fn from_crac(op: InternalOperationSum) -> Self {
        Self {
            op,
            origin: InternalOpOrigin::Crac,
        }
    }

    #[inline]
    fn is_own(&self) -> bool {
        matches!(self.origin, InternalOpOrigin::Own { .. })
    }
}

/// Drop the origin tags and return the underlying internal operations
/// in their original order.
fn untag_internals(tagged: Vec<TaggedInternalOp>) -> Vec<InternalOperationSum> {
    tagged.into_iter().map(|t| t.op).collect()
}

/// Build the final [`ContentResult`] for the parent operation,
/// cascading any exec failure into the internals (mutated in place).
///
/// - Parent `Err` → parent becomes `Failed`; every Applied
///   internal is demoted to `BackTracked`.
/// - Parent `Ok` but an internal failed → parent demoted to
///   `BackTracked`; every Applied internal demoted to `BackTracked`.
/// - Otherwise → parent stays `Applied`; internals untouched.
fn finalize_statuses<M: OperationKind>(
    result: Result<M::Success, ApplyOperationError>,
    internal_operation_results: &mut [TaggedInternalOp],
) -> ContentResult<M> {
    match result {
        Ok(success) => {
            // A top-level Applied stays Applied iff the last internal
            // op is Applied — internals fail linearly, so the last one
            // carries the cascade signal.
            let all_internal_succeeded = internal_operation_results
                .last()
                .is_none_or(|t| t.op.is_applied());
            if all_internal_succeeded {
                ContentResult::Applied(success)
            } else {
                internal_operation_results
                    .iter_mut()
                    .for_each(|t| t.op.transform_result_backtrack());
                ContentResult::BackTracked(BacktrackedResult {
                    errors: None,
                    result: success,
                })
            }
        }
        Err(operation_error) => {
            internal_operation_results
                .iter_mut()
                .for_each(|t| t.op.transform_result_backtrack());
            ContentResult::Failed(vec![operation_error].into())
        }
    }
}

/// Charge the payer for the storage that `content` and each Own
/// internal allocated.
///
/// Entries tagged [`InternalOpOrigin::Crac`] pass through unchanged
/// — their storage cost belongs to a foreign-runtime caller.
///
/// On a per-entry burn failure, the receipt entries
/// (clone-then-rebuild) carry the pre-burn internals — no stray
/// balance_updates survive. Host-side debits applied by `burn_tez`
/// are rolled back out-of-band by the caller's `SafeStorage`
/// transaction.
fn burn_pass<Host, M, A>(
    host: &mut Host,
    payer: &A,
    storage_limit_remaining: &mut BigUint,
    content: &mut ContentResult<M>,
    internal_operation_results: Vec<TaggedInternalOp>,
) -> (Vec<InternalOperationSum>, Result<(), ApplyOperationError>)
where
    Host: StorageV1,
    M: storage_fees::OperationStorageFees,
    A: TezlinkAccount,
{
    let parent_outcome = storage_fees::burn_content_storage_fees::<_, M>(
        host,
        payer,
        storage_limit_remaining,
        content,
    );
    if parent_outcome.is_err() {
        return (untag_internals(internal_operation_results), parent_outcome);
    }

    let burned: Result<Vec<InternalOperationSum>, ApplyOperationError> =
        internal_operation_results
            .iter()
            .map(|tagged_op| {
                let mut op = tagged_op.op.clone();
                match &tagged_op.origin {
                    InternalOpOrigin::Own {
                        delegated_storage_cost,
                    } => {
                        storage_fees::burn_internal_op_storage_fees(
                            host,
                            payer,
                            storage_limit_remaining,
                            &mut op,
                        )?;
                        storage_fees::add_delegated_storage_fee_balance_updates(
                            payer,
                            &mut op,
                            *delegated_storage_cost,
                        )?;
                    }
                    InternalOpOrigin::Crac => {}
                }
                Ok(op)
            })
            .collect();

    match burned {
        Ok(burned) => (burned, Ok(())),
        Err(e) => (untag_internals(internal_operation_results), Err(e)),
    }
}

/// Build the wire [`OperationResult`] for a manager operation:
/// cascade statuses, burn storage fees, then assemble the receipt
/// (never mutated afterwards).
///
/// A burn-level failure demotes the parent (and every Applied
/// internal) to `BackTracked`, with the burn errors attached to
/// the parent.
#[allow(clippy::too_many_arguments)]
fn finalize_and_burn<Host, M, A>(
    host: &mut Host,
    payer: &A,
    storage_limit: &Narith,
    balance_updates: Vec<BalanceUpdate>,
    delegated_storage_cost: u64,
    top_level_delegated_delta: u64,
    result: Result<M::Success, ApplyOperationError>,
    mut internal_operation_results: Vec<TaggedInternalOp>,
) -> OperationResult<M>
where
    Host: StorageV1,
    M: storage_fees::OperationStorageFees,
    A: TezlinkAccount,
{
    let mut content = finalize_statuses::<M>(result, &mut internal_operation_results);
    let mut storage_limit_remaining: BigUint = storage_limit.0.clone();
    let (mut internals, burn_outcome) = burn_pass(
        host,
        payer,
        &mut storage_limit_remaining,
        &mut content,
        internal_operation_results,
    );
    let burn_outcome = burn_outcome.and_then(|()| {
        storage_fees::burn_storage_cost(
            host,
            payer,
            &mut storage_limit_remaining,
            delegated_storage_cost,
        )?;
        storage_fees::add_delegated_storage_fee_balance_updates_to_content(
            payer,
            &mut content,
            top_level_delegated_delta,
        )
    });
    if let Err(errors) = burn_outcome {
        log!(Debug, "Storage burn failed: {errors:?}");
        content.backtrack_if_applied_with_errors(errors.into());
        internals
            .iter_mut()
            .for_each(InternalOperationSum::transform_result_backtrack);
    }
    OperationResult {
        balance_updates,
        result: content,
        internal_operation_results: internals,
    }
}

/// Cost of a single durable-store write of `payload_bytes`, in
/// milligas. Public counterpart of [`consume_storage_write_milligas`]
/// for callers outside `tezos_execution` that maintain their own gas
/// counter.
pub fn storage_write_cost_milligas(count: u64, payload_bytes: u64) -> u64 {
    gas::STORAGE_WRITE_BASE_MILLIGAS
        .saturating_mul(count)
        .saturating_add(
            gas::STORAGE_WRITE_PER_BYTE_MILLIGAS.saturating_mul(payload_bytes),
        )
}

/// Charge gas for a sequence of `count` durable-store writes whose
/// combined payload is `payload_bytes`.
pub(crate) fn consume_storage_write_milligas(
    operation_gas: &mut TezlinkOperationGas,
    count: u64,
    payload_bytes: u64,
) -> Result<(), mir::gas::OutOfGas> {
    let cost = storage_write_cost_milligas(count, payload_bytes);
    operation_gas.cast_and_consume_milligas(cost)
}

/// Cost of a single durable-store read of `payload_bytes`, in
/// milligas. Public counterpart of [`consume_storage_read_milligas`]
/// for callers outside `tezos_execution` that maintain their own gas
/// counter.
pub fn storage_read_cost_milligas(count: u64, payload_bytes: u64) -> u64 {
    gas::STORAGE_READ_BASE_MILLIGAS
        .saturating_mul(count)
        .saturating_add(gas::STORAGE_READ_PER_BYTE_MILLIGAS.saturating_mul(payload_bytes))
}

/// Charge gas for a sequence of `count` durable-store reads whose
/// combined payload is `payload_bytes`.
pub(crate) fn consume_storage_read_milligas(
    operation_gas: &mut TezlinkOperationGas,
    count: u64,
    payload_bytes: u64,
) -> Result<(), mir::gas::OutOfGas> {
    let cost = storage_read_cost_milligas(count, payload_bytes);
    operation_gas.cast_and_consume_milligas(cost)
}

/// Counters are often being read and updated (code size, paid bytes, etc.). When
/// charging gas for these operations, we use an upper-bound of their size.
const COUNTER_SIZE: u64 = 32;

fn reveal<Host>(
    tc_ctx: &mut TcCtx<'_, Host>,
    source_account: &TezosImplicitAccount,
    public_key: &PublicKey,
) -> Result<RevealSuccess, RevealError>
where
    Host: StorageV1,
{
    log!(Debug, "Applying a reveal operation");
    let manager = source_account
        .manager(tc_ctx.host)
        .map_err(|_| RevealError::UnretrievableManager)?;

    let expected_hash = match manager {
        Manager::Revealed(pk) => return Err(RevealError::PreviouslyRevealedKey(pk)),
        Manager::NotRevealed(pkh) => pkh,
    };

    // Ensure that the source of the operation is equal to the retrieved hash.
    if &expected_hash != source_account.pkh() {
        return Err(RevealError::InconsistentHash(expected_hash));
    }

    // Check the public key
    let pkh_from_pk = public_key.pk_hash();
    if expected_hash != pkh_from_pk {
        return Err(RevealError::InconsistentPublicKey(expected_hash));
    }

    // Set the public key as the manager
    source_account
        .set_manager_public_key(tc_ctx.host, public_key)
        .map_err(|_| RevealError::FailedToWriteManager)?;

    log!(Debug, "Reveal operation succeed");

    Ok(RevealSuccess {
        consumed_milligas: tc_ctx
            .operation_gas
            .get_and_reset_milligas_consumed()
            .map_err(|_| RevealError::OutOfGas)?,
    })
}

fn contract_from_address(address: AddressHash) -> Result<Contract, TransferError> {
    match address {
        AddressHash::Kt1(kt1) => Ok(Contract::Originated(kt1)),
        AddressHash::Implicit(pkh) => Ok(Contract::Implicit(pkh)),
        AddressHash::Sr1(_) => Err(TransferError::MirAddressUnsupportedError),
    }
}

fn transfer_tez<Host>(
    host: &mut Host,
    giver_account: &impl TezlinkAccount,
    amount: &Narith,
    receiver_account: &impl TezlinkAccount,
) -> Result<TransferSuccess, TransferError>
where
    Host: StorageV1,
{
    let balance_updates =
        compute_balance_updates(giver_account, receiver_account, amount)
            .map_err(|e| TransferError::FailedToComputeBalanceUpdate(e.to_string()))?;

    apply_balance_changes(host, giver_account, receiver_account, &amount.0)?;
    Ok(TransferSuccess {
        storage: None,
        lazy_storage_diff: None,
        balance_updates,
        ticket_receipt: vec![],
        originated_contracts: vec![],
        consumed_milligas: 0_u64.into(),
        storage_size: 0_u64.into(),
        paid_storage_size_diff: 0_u64.into(),
        allocated_destination_contract: false,
        address_registry_diff: vec![],
    })
}

/// Credit the receiver without debiting any sender.
/// Used for cross-runtime calls where the sender's balance was already
/// debited on the EVM side.
///
/// Assumption: this path is enabled from a foreign calling runtime,
/// and the sender was already debited in that runtime.
fn credit_destination_without_debiting_sender(
    host: &mut impl StorageV1,
    amount: &Narith,
    receiver_account: &impl TezlinkAccount,
) -> Result<TransferSuccess, TransferError> {
    if amount.eq(&0_u64.into()) {
        return Ok(TransferSuccess {
            storage: None,
            lazy_storage_diff: None,
            balance_updates: vec![],
            ticket_receipt: vec![],
            originated_contracts: vec![],
            consumed_milligas: 0_u64.into(),
            storage_size: 0_u64.into(),
            paid_storage_size_diff: 0_u64.into(),
            allocated_destination_contract: false,
            address_registry_diff: vec![],
        });
    }
    let receiver_balance = receiver_account
        .balance(host)
        .map_err(|_| TransferError::FailedToFetchDestinationBalance)?
        .0;
    let new_receiver_balance = (&receiver_balance + &amount.0).into();
    receiver_account
        .set_balance(host, &new_receiver_balance)
        .map_err(|_| TransferError::FailedToUpdateDestinationBalance)?;
    let receiver_delta = BigInt::from_biguint(num_bigint::Sign::Plus, amount.into());
    Ok(TransferSuccess {
        storage: None,
        lazy_storage_diff: None,
        // TODO: L2-882 design CRAC receipts
        balance_updates: vec![BalanceUpdate {
            balance: Balance::Account(receiver_account.contract()),
            changes: receiver_delta.try_into().map_err(
                |e: num_bigint::TryFromBigIntError<num_bigint::BigInt>| {
                    TransferError::FailedToComputeBalanceUpdate(e.to_string())
                },
            )?,
            update_origin: UpdateOrigin::BlockApplication,
        }],
        ticket_receipt: vec![],
        originated_contracts: vec![],
        consumed_milligas: 0_u64.into(),
        storage_size: 0_u64.into(),
        paid_storage_size_diff: 0_u64.into(),
        allocated_destination_contract: false,
        address_registry_diff: vec![],
    })
}

fn burn_tez(
    host: &mut impl StorageV1,
    account: &impl TezlinkAccount,
    amount: &num_bigint::BigUint,
) -> Result<Narith, TransferError> {
    let balance = account
        .balance(host)
        .map_err(|_| TransferError::FailedToFetchSenderBalance)?;
    let new_balance = match balance.0.checked_sub(amount) {
        None => {
            return Err(TransferError::BalanceTooLow(BalanceTooLow {
                contract: account.contract(),
                balance: balance.clone(),
                amount: amount.into(),
            }));
        }
        Some(new_balance) => new_balance.into(),
    };
    account
        .set_balance(host, &new_balance)
        .map_err(|_| TransferError::FailedToApplyBalanceChanges)?;
    Ok(new_balance)
}

/// How a queued internal operation should be turned into a receipt.
///
/// The decision is the same for every operation kind, so it is taken
/// once per operation — before the per-kind `match` — rather than
/// re-derived inside each arm:
/// - [`Disposition::Skip`]: a previous internal operation in the same
///   top-level operation already failed, so this one is not executed
///   and is reported as `Skipped`.
/// - [`Disposition::Replay`]: this operation shares its MIR counter
///   with an already-applied one (a `DUP`ed `operation` value); it is
///   rejected as an internal-operation replay (matching L1's
///   `Internal_operation_replay`) instead of being applied again.
/// - [`Disposition::Apply`]: execute the operation normally.
#[derive(Clone, Copy)]
enum Disposition {
    Skip,
    Replay,
    Apply,
}

#[allow(clippy::too_many_arguments)]
fn execute_internal_operations<'a, Host>(
    tc_ctx: &mut TcCtx<'a, Host>,
    operation_ctx: &mut OperationCtx<'a, TezosImplicitAccount>,
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    internal_operations: impl Iterator<Item = OperationInfo<'a>>,
    sender_account: &crate::account_storage::TezlinkOriginatedAccount,
    parser: &'a Parser<'a>,
    all_internal_receipts: &mut Vec<TaggedInternalOp>,
    nonce_counter: &mut u16,
) -> Result<(), ApplyOperationError>
where
    Host: StorageV1,
{
    let mut failed = None;
    for (index, OperationInfo { operation, counter }) in
        internal_operations.into_iter().enumerate()
    {
        tc_ctx
            .operation_gas
            .consume(Cost::manager_operation())
            .map_err(TransferError::OutOfGas)?;
        log!(
            Debug,
            "Executing internal operation {operation:?} with counter {counter:?}"
        );
        // Assign operation-local nonces; block-sequential nonces are
        // assigned at block finalization by renumber_nonces().
        let nonce = *nonce_counter;
        *nonce_counter = nonce_counter.saturating_add(1);
        // Internal-operation replay detection. `operation` is
        // `Duplicable`, so a contract may `DUP` an `operation` value and
        // emit both copies. They share the MIR `counter`, the identity
        // L1 uses to enforce internal-operation linearity. Record each
        // counter as it is dequeued; a counter already seen within this
        // top-level operation is a replay, and the duplicated operation
        // is rejected (matching L1 `Internal_operation_replay`) instead
        // of being applied a second time under a fresh display nonce.
        let is_replay = !operation_ctx.applied_counters.insert(counter);
        let receipts_before = all_internal_receipts.len();
        // Watermarks for `drain_reentrant_crac_ops`. Execution is
        // synchronous and stack-nested, so everything pushed to the three
        // CRAC lists between here and the drain below is this op's own
        // re-entrant subtree. Splicing it at the parent site keeps DFS
        // order rather than letting it reach the top-level merge with a
        // smaller seq than its outer parent (would invert DFS — L2-1300).
        let pending_crac_receipts_before = journal.michelson.pending_crac_receipts.len();
        let failed_crac_receipts_before = journal.michelson.failed_crac_receipts.len();
        let backtracked_crac_receipts_before =
            journal.michelson.backtracked_crac_receipts.len();
        // Resolve the disposition once: a prior failure short-circuits
        // every kind to `Skip`, a replayed counter to `Replay`, and a
        // replay also fails the enclosing operation. Only `Apply` needs
        // the per-kind logic below.
        let disposition = if failed.is_some() {
            Disposition::Skip
        } else if is_replay {
            failed = Some(index);
            Disposition::Replay
        } else {
            Disposition::Apply
        };
        let (internal_receipt, delegated_storage_cost_delta) = match operation {
            mir::ast::Operation::TransferTokens(TransferTokens {
                param,
                destination_address,
                amount,
            }) => {
                let amount = Narith(amount.try_into().map_err(
                    |err: num_bigint::TryFromBigIntError<()>| {
                        TransferError::MirAmountToNarithError(err.to_string())
                    },
                )?);
                let dest_contract = contract_from_address(destination_address.hash)?;
                let value =
                    param.into_micheline_optimized_legacy(&parser.arena, tc_ctx.gas())?;
                let encoded_value = value.encode(tc_ctx.gas())?.map_err(|e| {
                    TransferError::MichelineSerializationError(e.to_string())
                })?;
                let content = TransferContent {
                    amount,
                    destination: dest_contract,
                    parameters: Parameters {
                        entrypoint: destination_address.entrypoint,
                        value: encoded_value,
                    },
                };
                let (result, delegated_storage_cost_delta) = match disposition {
                    Disposition::Skip => (ContentResult::Skipped, 0),
                    Disposition::Replay => (
                        ContentResult::Failed(
                            ApplyOperationError::InternalOperationReplay.into(),
                        ),
                        0,
                    ),
                    Disposition::Apply => {
                        // Internal-operation parameters were produced and
                        // validated by the emitting contract during its own
                        // execution, so they may legitimately reference
                        // big_maps it owns.
                        let allow_forged_lazy_storage_id = AllowForgedLazyStorageId::Yes;
                        let receipt = transfer(
                            tc_ctx,
                            operation_ctx,
                            registry,
                            journal,
                            sender_account,
                            &content.amount,
                            &content.destination,
                            &content.parameters.entrypoint,
                            value,
                            parser,
                            all_internal_receipts,
                            false,
                            allow_forged_lazy_storage_id,
                            nonce_counter,
                        );
                        let delegated_storage_cost_delta = match &receipt {
                            Ok((_, delta)) => *delta,
                            Err(_) => 0,
                        };
                        let result = match receipt {
                            Ok((success, _)) => {
                                let sub_ops_succeeded = all_internal_receipts
                                    .get(receipts_before..)
                                    .and_then(|s| s.last())
                                    .is_none_or(|t| t.op.is_applied());
                                if sub_ops_succeeded {
                                    ContentResult::Applied(success.into())
                                } else {
                                    failed = Some(index);
                                    ContentResult::BackTracked(BacktrackedResult {
                                        errors: None,
                                        result: success.into(),
                                    })
                                }
                            }
                            Err(CracError::BlockAbort(msg)) => {
                                return Err(ApplyOperationError::BlockAbort(msg));
                            }
                            Err(CracError::Operation(err)) => {
                                failed = Some(index);
                                log!(Error, "Internal transfer failed: {err:?}");
                                ContentResult::Failed(
                                    ApplyOperationError::from(err).into(),
                                )
                            }
                        };
                        (result, delegated_storage_cost_delta)
                    }
                };
                (
                    InternalOperationSum::Transfer(InternalContentWithMetadata {
                        content,
                        sender: sender_account.contract(),
                        nonce,
                        result,
                    }),
                    delegated_storage_cost_delta,
                )
            }
            mir::ast::Operation::CreateContract(mir::ast::CreateContract {
                delegate,
                amount,
                storage,
                code: _,
                micheline_code,
                address,
            }) => {
                let amount = Narith(amount.try_into().unwrap_or(BigUint::ZERO));
                let encode_err = |e: tezos_data_encoding::enc::BinError| {
                    ApplyOperationError::Origination(
                        OriginationError::MichelineSerializationError(e.to_string()),
                    )
                };
                let script = Script {
                    code: micheline_code.encode(tc_ctx.gas())?.map_err(encode_err)?,
                    storage: storage
                        .clone()
                        .into_micheline_optimized_legacy(&parser.arena, tc_ctx.gas())?
                        .encode(tc_ctx.gas())?
                        .map_err(encode_err)?,
                };
                let result = match disposition {
                    Disposition::Skip => ContentResult::Skipped,
                    Disposition::Replay => ContentResult::Failed(
                        ApplyOperationError::InternalOperationReplay.into(),
                    ),
                    Disposition::Apply => {
                        let receipt = originate_contract(
                            tc_ctx,
                            address,
                            sender_account,
                            &amount,
                            Some(&script.code),
                            storage,
                            &Origin::Native,
                        );
                        match receipt {
                            Ok(success) => ContentResult::Applied(success),
                            Err(err) => {
                                failed = Some(index);
                                log!(Error, "Internal origination failed: {err:?}");
                                ContentResult::Failed(
                                    ApplyOperationError::from(err).into(),
                                )
                            }
                        }
                    }
                };
                (
                    InternalOperationSum::Origination(InternalContentWithMetadata {
                        content: OriginationContent {
                            balance: amount,
                            delegate,
                            script,
                        },
                        sender: sender_account.contract(),
                        nonce,
                        result,
                    }),
                    0,
                )
            }
            mir::ast::Operation::SetDelegate(set_delegate) => {
                return Err(ApplyOperationError::UnSupportedSetDelegate(format!(
                    "{set_delegate:?}"
                )));
            }

            mir::ast::Operation::Emit(mir::ast::Emit { tag, value, arg_ty }) => {
                let tag: Option<Entrypoint> = tag
                    .map(|t| {
                        t.try_into().map_err(|e: mir::ast::ByteReprError| {
                            ApplyOperationError::UnSupportedEmit(format!(
                                "Invalid emit tag: {e:?}"
                            ))
                        })
                    })
                    .transpose()?;
                let emit_err = |e: tezos_data_encoding::enc::BinError| {
                    ApplyOperationError::EmitMichelineSerializationError(e.to_string())
                };
                let payload = Some(
                    value
                        .into_micheline_optimized_legacy(&parser.arena, tc_ctx.gas())?
                        .encode(tc_ctx.gas())?
                        .map_err(emit_err)?
                        .into(),
                );
                let ty = match arg_ty {
                    mir::ast::Or::Left(typ) => typ
                        .into_micheline_optimized_legacy(&parser.arena, tc_ctx.gas())?
                        .encode(tc_ctx.gas())?
                        .map_err(emit_err)?,
                    mir::ast::Or::Right(mic) => {
                        mic.encode(tc_ctx.gas())?.map_err(emit_err)?
                    }
                }
                .into();
                let result = match disposition {
                    Disposition::Skip => ContentResult::Skipped,
                    Disposition::Replay => ContentResult::Failed(
                        ApplyOperationError::InternalOperationReplay.into(),
                    ),
                    Disposition::Apply => {
                        // Same semantics as OCaml:
                        // Gas.consumed ~since:ctxt_before_op ~until:ctxt
                        let consumed_milligas = tc_ctx
                            .operation_gas
                            .get_and_reset_milligas_consumed()
                            .map_err(TransferError::OutOfGas)?;
                        ContentResult::Applied(EventSuccess { consumed_milligas })
                    }
                };
                (
                    InternalOperationSum::Event(InternalContentWithMetadata {
                        content: EventContent { tag, payload, ty },
                        sender: sender_account.contract(),
                        nonce,
                        result,
                    }),
                    0,
                )
            }
        };
        log!(Debug, "Internal operation executed successfully");
        // Insert the parent receipt BEFORE its children so the flat
        // list follows DFS order: parent op, then its sub-ops.
        // `receipts_before` was captured before `transfer()` added the
        // child receipts, so inserting at that index puts the parent
        // receipt in the correct position.
        all_internal_receipts.insert(
            receipts_before,
            TaggedInternalOp::own(internal_receipt, delegated_storage_cost_delta),
        );
        // Drain the re-entrant CRAC ops accumulated during this op's
        // execution (a gateway leg that re-entered the Michelson runtime)
        // and splice the synthetic frame in at `receipts_before + 1` —
        // right after the parent (just inserted at `receipts_before`) and
        // ahead of its inline children (e.g. the %on_result callback). The
        // leg runs before the callback that consumes its payload, so the
        // frame must precede it. Empty drain → no-op (`frame_at == len`).
        let reentrant_ops = crate::enshrined_contracts::drain_reentrant_crac_ops(
            journal,
            pending_crac_receipts_before,
            failed_crac_receipts_before,
            backtracked_crac_receipts_before,
        );
        let frame_at = receipts_before + 1;
        all_internal_receipts.splice(
            frame_at..frame_at,
            reentrant_ops.into_iter().map(TaggedInternalOp::from_crac),
        );
    }
    Ok(())
}

/// Handles manager transfer operations for both implicit and originated contracts but with a MIR context.
///
/// When `skip_sender_debit` is true, only the receiver is credited without
/// debiting the sender. This is used for cross-runtime calls (e.g. EVM gateway)
/// where the sender's balance was already debited by the calling runtime.
#[allow(clippy::too_many_arguments)]
fn transfer<'a, Host>(
    tc_ctx: &mut TcCtx<'a, Host>,
    operation_ctx: &mut OperationCtx<'a, TezosImplicitAccount>,
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    sender_account: &impl TezlinkAccount,
    amount: &Narith,
    dest_contract: &Contract,
    entrypoint: &Entrypoint,
    param: Micheline<'a>,
    parser: &'a Parser<'a>,
    all_internal_receipts: &mut Vec<TaggedInternalOp>,
    skip_sender_debit: bool,
    // Whether a forged big_map id (a bare lazy-storage reference) is allowed in
    // `param`. See [`mir::typechecker::AllowForgedLazyStorageId`].
    allow_forged_lazy_storage_id: AllowForgedLazyStorageId,
    nonce_counter: &mut u16,
) -> Result<(TransferSuccess, u64), CracError>
where
    Host: StorageV1,
{
    match dest_contract {
        Contract::Implicit(pkh) => {
            tc_ctx
                .operation_gas
                .consume(Cost::transaction())
                .map_err(TransferError::OutOfGas)?;

            // Safety coupling: `Operation::touches_only_accounts` (tezos crate)
            // treats implicit-destination transfers as account-only and lets the
            // caller narrow the per-operation SafeStorage snapshot to the accounts
            // root. That relies on this guard rejecting any implicit transfer that
            // could execute code or touch another root (non-default entrypoint /
            // non-Unit parameter). Do not relax it without revisiting that
            // classifier.
            if param != Micheline::from(()) || !entrypoint.is_default() {
                return Err(TransferError::NonSmartContractExecutionCall.into());
            }
            // Transfers of 0 tez to an implicit contract are rejected.
            if amount.eq(&0_u64.into()) {
                return Err(TransferError::EmptyImplicitTransfer.into());
            };

            let dest_account = context::implicit_from_public_key_hash(pkh)
                .map_err(|_| TransferError::FailedToFetchDestinationAccount)?;
            let already_allocated = dest_account
                .allocate(tc_ctx.host)
                .map_err(|_| TransferError::FailedToAllocateDestination)?;
            let receipt = if skip_sender_debit {
                credit_destination_without_debiting_sender(
                    tc_ctx.host,
                    amount,
                    &dest_account,
                )?
            } else {
                transfer_tez(tc_ctx.host, sender_account, amount, &dest_account)?
            };
            Ok((
                TransferSuccess {
                    allocated_destination_contract: !already_allocated,
                    consumed_milligas: tc_ctx
                        .operation_gas
                        .get_and_reset_milligas_consumed()
                        .map_err(TransferError::OutOfGas)?,
                    ..receipt
                },
                0,
            ))
        }
        Contract::Originated(kt1) => {
            let dest_account = context::originated_from_kt1(kt1)
                .map_err(|_| TransferError::FailedToFetchDestinationAccount)?;
            // Reject transfers to a never-originated KT1 before any state
            // write. Without this check, `transfer_tez` would create a
            // balance entry under the bogus contract path and the failure
            // would only surface at `code()` as RuntimeError::PathNotFound,
            // wrapped as FailedToFetchContractCode → CracError::BlockAbort
            // — a user-controllable block-abort handle. Mirrors Tezos's
            // `Contract.Non_existing_contract`.
            if !dest_account
                .exists(tc_ctx.host)
                .map_err(|_| TransferError::FailedToFetchDestinationAccount)?
            {
                return Err(TransferError::ContractDoesNotExist(Contract::Originated(
                    kt1.clone(),
                ))
                .into());
            }
            let receipt = if skip_sender_debit {
                credit_destination_without_debiting_sender(
                    tc_ctx.host,
                    amount,
                    &dest_account,
                )?
            } else {
                transfer_tez(tc_ctx.host, sender_account, amount, &dest_account)?
            };
            let code = dest_account
                .code(tc_ctx.host)
                .map_err(|_| TransferError::FailedToFetchContractCode)?;
            let (code_read_count, code_size) = match &code {
                Code::Code(code_bytes) => (1, code_bytes.len() as u64),
                Code::Enshrined(_) => (0, 0),
            };
            consume_storage_read_milligas(
                tc_ctx.operation_gas,
                code_read_count,
                code_size,
            )
            .map_err(TransferError::OutOfGas)?;
            let storage = dest_account
                .storage(tc_ctx.host)
                .map_err(|_| TransferError::FailedToFetchContractStorage)?;
            consume_storage_read_milligas(tc_ctx.operation_gas, 1, storage.len() as u64)
                .map_err(TransferError::OutOfGas)?;
            let exec_ctx =
                ExecCtx::create(tc_ctx.host, sender_account, &dest_account, amount)?;

            let delegated_storage_cost_before = operation_ctx.delegated_storage_cost;
            let (internal_operations, new_storage): (Vec<OperationInfo<'a>>, Vec<u8>) = {
                let mut ctx = Ctx {
                    tc_ctx: &mut *tc_ctx,
                    exec_ctx,
                    operation_ctx: &mut *operation_ctx,
                    journal: &mut *journal,
                    registry,
                };
                let result = match code {
                    Code::Code(code_bytes) => {
                        let (iter, new_storage) = execute_smart_contract_originated(
                            code_bytes,
                            storage,
                            entrypoint,
                            param,
                            parser,
                            &mut ctx,
                            allow_forged_lazy_storage_id,
                        )?;
                        (iter.collect::<Vec<_>>(), new_storage)
                    }
                    Code::Enshrined(contract) => {
                        let ops = enshrined_contracts::execute_enshrined_contract(
                            contract, entrypoint, param, &mut ctx,
                        )?;
                        (ops, vec![])
                    }
                };
                drop(ctx);
                result
            };
            let delegated_storage_cost =
                operation_ctx.delegated_storage_cost - delegated_storage_cost_before;

            // `interpret_context` holds this operation's lazy-storage size
            // delta, accumulated by the `LazyStorage` hooks during execution.
            // Like `big_map_diff`, it is a per-operation value carried on the
            // batch-lifetime `TcCtx` and drained at each operation boundary;
            // the drain also resets it, so every frame (this transfer, and the
            // origination path) starts empty and no separate reset is needed.
            // TODO(L2-1481): make this draining structurally safe so a hook
            // firing without a matching drain cannot be silently mispriced.
            let lazy_storage_size_diff =
                tc_ctx.interpret_context.take_lazy_storage_size_diff();

            // Read the contract's current accounting record and compute the
            // post-update record, folding the storage-size delta and the
            // lazy-storage delta into `used_bytes`. Then charge gas for the
            // durable-storage operations this transfer performs — the storage
            // blob write plus the single read-modify-write of the aggregated
            // `/info` record, each at the exact number of bytes moved. The
            // record is only persisted (further below) once its cost has been
            // charged. Enshrined contracts have synthetic storage that is not
            // accounted: they read, charge, and write nothing here.
            let (storage_space, info_to_write) =
                if enshrined_contracts::is_enshrined(dest_account.kt1()) {
                    (
                        StorageSpace {
                            used_bytes: 0.into(),
                            allocated_bytes: 0.into(),
                        },
                        None,
                    )
                } else {
                    let (prev_info, info_bytes_read) = dest_account
                        .read_info_with_len(tc_ctx.host)
                        .map_err(|_| TransferError::FailedToUpdateContractStorage)?;
                    let (new_info, space) = prev_info.with_storage_size(
                        new_storage.len() as u64,
                        lazy_storage_size_diff,
                    );
                    let info_bytes_written = new_info
                        .encoded_len()
                        .map_err(|_| TransferError::FailedToUpdateContractStorage)?;

                    consume_storage_write_milligas(
                        tc_ctx.operation_gas,
                        1,
                        new_storage.len() as u64,
                    )
                    .map_err(TransferError::OutOfGas)?;
                    consume_storage_read_milligas(
                        tc_ctx.operation_gas,
                        1,
                        info_bytes_read,
                    )
                    .map_err(TransferError::OutOfGas)?;
                    consume_storage_write_milligas(
                        tc_ctx.operation_gas,
                        1,
                        info_bytes_written,
                    )
                    .map_err(TransferError::OutOfGas)?;

                    (space, Some(new_info))
                };

            // In L1, the receipt of an operation only shows its own gas
            // consumption, i.e. it does not include that of its internal
            // operations.
            let consumed_milligas = tc_ctx
                .operation_gas
                .get_and_reset_milligas_consumed()
                .map_err(TransferError::OutOfGas)?;
            let big_map_diff_order = tc_ctx.interpret_context.take_big_map_diff_order();
            let lazy_storage_diff = convert_big_map_diff(
                std::mem::take(&mut tc_ctx.big_map_diff),
                big_map_diff_order,
            );

            // Persist the storage blob and the accounting record now that
            // their gas has been charged (no-op for enshrined contracts).
            if let Some(info) = &info_to_write {
                dest_account
                    .write_storage_and_info(tc_ctx.host, &new_storage, info)
                    .map_err(|_| TransferError::FailedToUpdateContractStorage)?;
            }
            let StorageSpace {
                used_bytes,
                allocated_bytes: paid_storage_size_diff,
            } = storage_space;

            match execute_internal_operations(
                tc_ctx,
                operation_ctx,
                registry,
                journal,
                internal_operations.into_iter(),
                &dest_account,
                parser,
                all_internal_receipts,
                nonce_counter,
            ) {
                Ok(()) => {}
                Err(ApplyOperationError::BlockAbort(msg)) => {
                    return Err(CracError::BlockAbort(msg))
                }
                Err(other) => {
                    return Err(CracError::Operation(
                        TransferError::FailedToExecuteInternalOperation(
                            other.to_string(),
                        ),
                    ))
                }
            }
            log!(Debug, "Transfer operation succeeded");
            Ok((
                TransferSuccess {
                    storage: if new_storage.is_empty() {
                        None
                    } else {
                        Some(new_storage.into())
                    },
                    lazy_storage_diff,
                    consumed_milligas,
                    storage_size: used_bytes,
                    paid_storage_size_diff,
                    ..receipt
                },
                delegated_storage_cost,
            ))
        }
    }
}

/// Typecheck originated-contract `code`, returning its exposed entrypoints
/// (annotation → type) together with its declared storage type. `None` if
/// `code` is not a well-formed, typeable script (or gas is exhausted).
fn typecheck_originated_script(
    code: &[u8],
    gas: &mut Gas,
) -> Option<(HashMap<Entrypoint, mir::ast::Type>, mir::ast::Type)> {
    let parser = Parser::new();
    let micheline = Micheline::decode_raw(&parser.arena, code, gas).ok()?.ok()?;
    let typechecked = micheline
        .split_script()
        .ok()?
        .typecheck_script(gas, true, false)
        .ok()?;
    // `typechecked` is only used afterwards via `.annotations`, so the
    // (recursive, non-trivial) storage type can be moved out instead of cloned.
    let storage_ty = typechecked.storage;
    let entrypoints = typechecked
        .annotations
        .into_iter()
        .filter_map(|(field_annotation, (_, ty))| {
            mir::ast::Entrypoint::try_from(field_annotation)
                .ok()
                .map(|entrypoint| (entrypoint, ty.clone()))
        })
        .collect();
    Some((entrypoints, storage_ty))
}

fn get_originated_contract_entrypoint(
    code: Vec<u8>,
    gas: &mut Gas,
) -> Option<HashMap<Entrypoint, mir::ast::Type>> {
    typecheck_originated_script(&code, gas).map(|(entrypoints, _)| entrypoints)
}

pub fn get_contract_entrypoint(
    host: &impl StorageV1,
    address: &AddressHash,
    gas: &mut Gas,
) -> Option<HashMap<mir::ast::Entrypoint, mir::ast::Type>> {
    let contract = contract_from_address(address.clone()).ok()?;
    let contract_account = context::originated_from_contract(&contract).ok()?;
    let code = contract_account.code(host).ok()?;
    match code {
        Code::Code(code) => get_originated_contract_entrypoint(code, gas),
        Code::Enshrined(contract) => get_enshrined_contract_entrypoint(contract),
    }
}

/// Errors returned by [`upgrade_alias_implementation`].
#[derive(Debug, PartialEq, Eq)]
pub enum AliasUpgradeError {
    /// `new_code` is not a well-formed, typeable Michelson script.
    InvalidScript,
    /// The currently-installed implementation no longer typechecks. Should
    /// never happen; indicates a corrupted slot.
    CurrentImplementationInvalid,
    /// `new_code` removes (or renames) an entrypoint exposed by the current
    /// implementation. Downstream Michelson contracts may hardcode entrypoint
    /// annotations, so an exposed entrypoint may never disappear.
    EntrypointRemoved(String),
    /// `new_code` changes the type of an already-exposed entrypoint.
    EntrypointTypeChanged(String),
    /// `new_code` changes the contract's storage type. Every alias keeps its
    /// own storage typed against the shared code; a storage-type change would
    /// brick every alias at its next execution.
    StorageTypeChanged,
    /// Durable-storage failure while reading or writing the slot.
    Storage(StorageError),
}

impl From<StorageError> for AliasUpgradeError {
    fn from(e: StorageError) -> Self {
        Self::Storage(e)
    }
}

/// Atomically upgrade the single shared Michelson alias implementation (see
/// [`account_storage::ALIAS_IMPLEMENTATION_PATH`]). A single durable-storage
/// write here changes the behaviour of **every** existing and future alias at
/// once — O(1) in the number of aliases.
///
/// `new_code` must be a typeable Michelson script. To protect downstream
/// callers, the upgrade enforces two invariants against the currently-installed
/// implementation:
///
/// - **Entrypoint monotonicity:** every entrypoint exposed today must still be
///   present, with the same type, in `new_code` (new entrypoints may be added).
///   This protects callers that hardcode entrypoint annotations.
/// - **Storage-type invariance:** the storage type must not change. Each alias
///   keeps its own `/data/storage` typed against the shared code, so a
///   storage-type change would brick every alias at its next execution.
///
/// Note on widening the parameter while keeping the implicit `default`
/// entrypoint: in Michelson the `default` entrypoint's type is the *whole* root
/// parameter unless a branch is annotated `%default`. So to add an entrypoint
/// via an `or` while preserving `default`, the old default branch must be
/// annotated `%default` in `new_code` — otherwise `default`'s type changes and
/// the upgrade is (correctly) rejected.
///
/// This is the upgrade primitive only.
// TODO: https://linear.app/tezos/issue/L2-1530
// Wire this to the Tezos X governance trigger that authorises an upgrade.
// Until then it has no caller outside tests; the mechanism by which a
// governance decision reaches the kernel is out of scope for L2-1528. When a
// metered caller is wired, revisit gas accounting and distinguishing
// out-of-gas from a genuinely invalid script.
pub fn upgrade_alias_implementation<Host: StorageV1>(
    host: &mut Host,
    new_code: &[u8],
    gas: &mut Gas,
) -> Result<(), AliasUpgradeError> {
    // `new_code` (untrusted) must be a well-formed, typeable script.
    let (new_entrypoints, new_storage_ty) = typecheck_originated_script(new_code, gas)
        .ok_or(AliasUpgradeError::InvalidScript)?;

    // Check invariants against the currently-installed implementation. An
    // absent slot (not seeded yet) means there is nothing to preserve.
    if let Some(current_code) = account_storage::read_alias_implementation(host)? {
        // The current implementation is already installed and trusted; typecheck
        // it with a fresh budget (it is small and known-good) so `new_code`'s
        // gas consumption cannot starve this check and misreport it as corrupt.
        let (current_entrypoints, current_storage_ty) =
            typecheck_originated_script(&current_code, &mut Gas::default())
                .ok_or(AliasUpgradeError::CurrentImplementationInvalid)?;

        if new_storage_ty != current_storage_ty {
            return Err(AliasUpgradeError::StorageTypeChanged);
        }

        for (entrypoint, ty) in current_entrypoints.iter() {
            match new_entrypoints.get(entrypoint) {
                Some(new_ty) if new_ty == ty => {}
                Some(_) => {
                    return Err(AliasUpgradeError::EntrypointTypeChanged(format!(
                        "{entrypoint}"
                    )))
                }
                None => {
                    return Err(AliasUpgradeError::EntrypointRemoved(format!(
                        "{entrypoint}"
                    )))
                }
            }
        }
    }

    // One write upgrades every alias.
    account_storage::write_alias_implementation(host, new_code)?;
    Ok(())
}

/// Look up the synthetic views declared by an enshrined contract.
///
/// Returns `Some(views)` (possibly empty) for enshrined contracts,
/// `None` for unknown addresses or for originated contracts — the
/// latter already expose their views inside their on-chain Michelson
/// code, so off-chain tooling discovers them through `/script`
/// directly. See
/// [`crate::mir_ctx::enshrined_synthetic_views`] for the source of
/// truth.
pub fn get_enshrined_contract_views(
    host: &impl StorageV1,
    address: &AddressHash,
) -> Option<Vec<(&'static str, mir::ast::Type, mir::ast::Type)>> {
    let contract = contract_from_address(address.clone()).ok()?;
    let contract_account = context::originated_from_contract(&contract).ok()?;
    let code = contract_account.code(host).ok()?;
    match code {
        Code::Code(_) => None,
        Code::Enshrined(contract) => {
            Some(crate::mir_ctx::enshrined_synthetic_views(contract))
        }
    }
}

// Handles manager transfer operations.
#[allow(clippy::too_many_arguments)]
fn transfer_external<'a, Host>(
    tc_ctx: &mut TcCtx<'a, Host>,
    operation_ctx: &mut OperationCtx<'a, TezosImplicitAccount>,
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    amount: &Narith,
    dest: &Contract,
    parameters: &Parameters,
    all_internal_receipts: &mut Vec<TaggedInternalOp>,
    parser: &'a Parser<'a>,
    nonce_counter: &mut u16,
) -> Result<(TransferTarget, u64), CracError>
where
    Host: StorageV1,
{
    log!(Debug,
        "Applying an external transfer operation from {} to {dest:?} of {amount:?} mutez with parameters {parameters:?}",
        operation_ctx.source.contract()
    );
    let entrypoint = &parameters.entrypoint;
    let value = Micheline::decode_raw(&parser.arena, &parameters.value, tc_ctx.gas())
        .map_err(|oog: OutOfGas| CracError::Operation(oog.into()))?
        .map_err(|e| CracError::Operation(TransferError::from(e)))?;

    transfer(
        tc_ctx,
        operation_ctx,
        registry,
        journal,
        operation_ctx.source,
        amount,
        dest,
        entrypoint,
        value,
        parser,
        all_internal_receipts,
        false, // external operations always debit the sender
        // external operations are not allowed to forge big_map ids
        AllowForgedLazyStorageId::No,
        nonce_counter,
    )
    .map(|(success, delegated)| (success.into(), delegated))
}

fn gw<E: ToString>(e: E) -> TransferError {
    TransferError::GatewayError(e.to_string())
}

/// Result of a cross-runtime transfer, carrying both the transfer
/// outcome and any internal operation receipts produced during
/// Michelson execution (e.g. sub-transfers, originations, events).
pub struct CrossRuntimeTransferResult {
    pub target: TransferTarget,
    pub internal_receipts: Vec<InternalOperationSum>,
    /// Standalone storage cost — in mutez — of the internal
    /// operations the sub-execution emitted directly. Excludes
    /// operations surfaced from inner CRAC sub-executions (their
    /// cost is reported upward by those sub-executions through
    /// their own `X-Tezos-Storage-Cost` header, accumulated on the
    /// caller frame's [`OperationCtx::delegated_storage_cost`]).
    pub own_storage_cost: BigUint,
}

/// Error from [`cross_runtime_transfer`], carrying both the transfer
/// error and any partial internal operation receipts that were
/// collected before the failure (RFC Example 4: backtracked / failed /
/// skipped statuses).
pub struct CracTransferError {
    pub error: CracError,
    pub internal_receipts: Vec<InternalOperationSum>,
    /// `true` when the target's own call succeeded but one of its
    /// internal operations subsequently failed (a "deep failure").
    /// In that case the real error is already present as a `Failed`
    /// entry in `internal_receipts`, so the `alias→target` synthetic
    /// op should be marked `BackTracked` rather than `Failed`.
    /// `false` for every other error path (decode/checkpoint errors,
    /// direct target failure, structural failures).
    pub deep_failure: bool,
}

impl From<CracError> for CracTransferError {
    fn from(error: CracError) -> Self {
        Self {
            error,
            internal_receipts: vec![],
            deep_failure: false,
        }
    }
}

impl From<TransferError> for CracTransferError {
    fn from(error: TransferError) -> Self {
        Self::from(CracError::Operation(error))
    }
}

/// Execute a cross-runtime transfer with automatic rollback on failure.
///
/// Snapshots the world state before executing the transfer.  On success
/// the snapshot is discarded; on failure it is restored, undoing any
/// balance credits or storage writes that occurred before the error
/// (e.g. Michelson `FAILWITH`).
// TODO: L2-888 replace the low level revert mechanism by more general one
#[allow(clippy::too_many_arguments)]
pub fn cross_runtime_transfer<'a, Host>(
    tc_ctx: &mut TcCtx<'a, Host>,
    operation_ctx: &mut OperationCtx<'a, TezosImplicitAccount>,
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    sender: &impl TezlinkAccount,
    amount: &Narith,
    dest: &Contract,
    parameters: &Parameters,
    parser: &'a Parser<'a>,
    nonce_counter: &mut u16,
) -> Result<CrossRuntimeTransferResult, CracTransferError>
where
    Host: StorageV1,
{
    let entrypoint = &parameters.entrypoint;
    let value = Micheline::decode_raw(&parser.arena, &parameters.value, tc_ctx.gas())
        .map_err(|oog: OutOfGas| CracTransferError::from(TransferError::from(oog)))?
        .map_err(|e| CracTransferError::from(TransferError::from(e)))?;

    let world_state =
        tezos_smart_rollup_host::path::OwnedPath::from(&context::TEZOS_ACCOUNTS_ROOT);
    let checkpoint_index = journal
        .michelson
        .checkpoint(tc_ctx.host, &world_state)
        .map_err(|e| CracTransferError::from(gw(e)))?;

    let mut internal_receipts = Vec::new();
    match transfer(
        tc_ctx,
        operation_ctx,
        registry,
        journal,
        sender,
        amount,
        dest,
        entrypoint,
        value,
        parser,
        &mut internal_receipts,
        true, // skip_sender_debit: the calling runtime already debited the sender
        // big maps cannot be sent across runtimes
        AllowForgedLazyStorageId::No,
        nonce_counter,
    ) {
        Ok((success, _)) => {
            // transfer() returns Ok even when internal operations FAILWITH,
            // because execute_internal_operations records failures only in
            // receipts. We must inspect the receipts to detect this case,
            // matching the pattern used by [`finalize_statuses`] in the
            // normal Tezos operation path.
            let all_internal_succeeded =
                internal_receipts.last().is_none_or(|t| t.op.is_applied());
            if all_internal_succeeded {
                // promote: discard the snapshot, keep changes
                journal
                    .michelson
                    .checkpoint_commit(tc_ctx.host, checkpoint_index)
                    .map_err(|e| CracTransferError::from(gw(e)))?;

                let target: TransferTarget = success.into();
                let own_storage_cost =
                    storage_fees::compute_storage_fees::<TransferContent>(&target)?;
                let own_storage_cost: BigUint = internal_receipts
                    .iter()
                    .filter(|t| t.is_own())
                    .try_fold(own_storage_cost, |acc, t| {
                        let fee = storage_fees::compute_internal_op_storage_fees(&t.op)?;
                        Ok::<BigUint, TransferError>(acc + fee)
                    })?;

                Ok(CrossRuntimeTransferResult {
                    target,
                    internal_receipts: untag_internals(internal_receipts),
                    own_storage_cost,
                })
            } else {
                // revert: restore the snapshot
                journal
                    .michelson
                    .checkpoint_revert(tc_ctx.host, checkpoint_index)
                    .map_err(|e| CracTransferError::from(gw(e)))?;
                internal_receipts
                    .iter_mut()
                    .for_each(|t| t.op.transform_result_backtrack());
                // Meter the persisted error bodies of internal-op failures
                // before recording the failed CRAC receipt.  Each
                // `ContentResult::Failed` entry carries the FAILWITH body;
                // charging here closes the bypass by which an attacker
                // whose KT1 issues an internal TRANSFER_TOKENS to a
                // FAILWITHing contract would otherwise route a large body
                // into the receipt unmetered.  On OOG the oversized body is
                // replaced with `OutOfGas` while the entry is preserved so
                // the failed CRAC receipt always carries its internal-op list.
                let mut internal_receipts = untag_internals(internal_receipts);
                charge_internal_receipt_bodies(
                    tc_ctx.operation_gas,
                    &mut internal_receipts,
                );
                // Return the internal receipts so the failed CRAC
                // receipt can include backtracked/failed/skipped ops.
                // deep_failure=true: the target's own call returned Ok
                // but one of its internal ops recorded a Failed receipt.
                // The real error is already present in internal_receipts;
                // the alias→target synthetic op should be BackTracked.
                Err(CracTransferError {
                    error: CracError::Operation(
                        TransferError::FailedToExecuteInternalOperation(
                            "internal operation failed during cross-runtime call".into(),
                        ),
                    ),
                    internal_receipts,
                    deep_failure: true,
                })
            }
        }
        Err(e) => {
            journal
                .michelson
                .checkpoint_revert(tc_ctx.host, checkpoint_index)
                .map_err(|e| CracTransferError::from(gw(e)))?;
            internal_receipts
                .iter_mut()
                .for_each(|t| t.op.transform_result_backtrack());
            // Meter the persisted error bodies of any internal-op failures
            // accumulated before the top-level error, then meter the
            // top-level error itself.  Only `CracError::Operation` errors
            // are stored in the BSON sink; `CracError::BlockAbort` aborts
            // the block so no receipt is produced and no per-byte charge
            // applies.
            let mut internal_receipts = untag_internals(internal_receipts);
            charge_internal_receipt_bodies(tc_ctx.operation_gas, &mut internal_receipts);
            let metered_error = match e {
                CracError::Operation(te) => {
                    charge_persisted_error(tc_ctx.operation_gas, te)
                }
                CracError::BlockAbort(_) => e,
            };
            Err(CracTransferError {
                error: metered_error,
                internal_receipts,
                deep_failure: false,
            })
        }
    }
}

/// Maximum size, in bytes, of an originated Michelson script (code + storage).
/// Mirrors L1's `max_operation_data_length`; bounds `decode_raw` at origination
/// time (defense-in-depth on top of the per-type cap, L2-1706).
pub const MICHELSON_MAXIMUM_SCRIPT_SIZE: usize = 32 * 1024;

/// Render a typechecking error, capped at
/// [`mir::bounded_fmt::MAX_INTERPRET_ERROR_RENDER_BYTES`]. Origination
/// typechecks attacker-supplied code, which can synthesise a
/// structurally-shared `Type` DAG whose render is far larger than its node
/// count; the per-type parse cap does not bound instruction-synthesised
/// types. Building such a type is gas-bounded (the typechecker's `DUP` walks
/// the unfolded type in its `Duplicable` check), so the render is `O(gas)`
/// rather than unbounded, but that ceiling is still large, so the error text
/// is capped here as on the transfer and view sinks.
fn bounded_tc_error(e: &mir::typechecker::TcError) -> String {
    mir::bounded_fmt::display_bounded(
        e,
        mir::bounded_fmt::MAX_INTERPRET_ERROR_RENDER_BYTES,
    )
}

/// This function typechecks both fields of a &Script: the code and the storage.
/// It returns the typechecked storage.
pub fn typecheck_code_and_storage<'a, Host: StorageV1>(
    ctx: &mut TcCtx<'a, Host>,
    parser: &'a Parser<'a>,
    script: &Script,
) -> Result<TypedValue<'a>, OriginationError> {
    let script_size = script.code.len() + script.storage.len();
    if script_size > MICHELSON_MAXIMUM_SCRIPT_SIZE {
        return Err(OriginationError::ScriptTooLarge(format!(
            "{script_size} bytes exceeds the maximum of {MICHELSON_MAXIMUM_SCRIPT_SIZE}"
        )));
    }
    let contract_micheline =
        Micheline::decode_raw(&parser.arena, &script.code, ctx.gas())
            .map_err(OriginationError::from)?
            .map_err(|e| OriginationError::MichelineSerializationError(e.to_string()))?;
    let allow_lazy_storage_in_storage = true;
    let contract_typechecked = contract_micheline
        .split_script()
        .map_err(|e| {
            OriginationError::MirTypecheckingError(format!(
                "Splitting script : {}",
                bounded_tc_error(&e)
            ))
        })?
        .typecheck_script(ctx.gas(), allow_lazy_storage_in_storage, true)
        .map_err(|e| {
            OriginationError::MirTypecheckingError(format!(
                "Script : {}",
                bounded_tc_error(&e)
            ))
        })?;
    let storage_micheline =
        Micheline::decode_raw(&parser.arena, &script.storage, ctx.gas())
            .map_err(OriginationError::from)?
            .map_err(|e| OriginationError::MichelineSerializationError(e.to_string()))?;
    // The initial storage comes from the (external) origination operation, so
    // it must not reference a `big_map` by a forged id — a contract cannot own
    // a lazy-storage id before it exists. Internal `CREATE_CONTRACT`
    // originations do not go through here: they carry an already-typechecked
    // `TypedValue` storage straight to `originate_contract`, which legitimately
    // references the emitting contract's big_maps. Mirrors L1's
    // `allow_forged_lazy_storage_id_in_storage:false` at origination.
    contract_typechecked
        // Origination: validate storage-value lambda child views (L2-1635).
        .typecheck_storage_with_views(
            ctx,
            &storage_micheline,
            TypecheckViews::Enabled,
            AllowForgedLazyStorageId::No,
        )
        .map_err(|e| {
            OriginationError::MirTypecheckingError(format!(
                "Storage : {}",
                bounded_tc_error(&e)
            ))
        })
}

fn handle_storage_with_big_maps<'a, Host: StorageV1>(
    ctx: &mut TcCtx<'a, Host>,
    mut storage: TypedValue<'a>,
) -> Result<(Vec<u8>, Option<LazyStorageDiffList>), OriginationError> {
    let parser = Parser::new();

    let mut big_maps = vec![];
    storage.view_big_maps_mut(&mut big_maps);

    // Dump big_map allocation, starting with empty big_maps
    mir::ast::big_map::dump_big_map_updates(ctx, &[], &mut big_maps, false)
        .map_err(|err| OriginationError::MirBigMapAllocation(err.to_string()))?;
    // Drain the diff before the fallible encode below: otherwise an
    // encode failure leaves `ctx.big_map_diff` half-populated and the
    // next operation in the same batch inherits stale entries.
    let big_map_diff_order = ctx.interpret_context.take_big_map_diff_order();
    let lazy_storage_diff =
        convert_big_map_diff(std::mem::take(&mut ctx.big_map_diff), big_map_diff_order);
    let storage = storage
        .into_micheline_optimized_legacy(&parser.arena, ctx.gas())?
        .encode(ctx.gas())?
        .map_err(|e| OriginationError::MichelineSerializationError(e.to_string()))?;
    Ok((storage, lazy_storage_diff))
}

/// Originate a contract: install its code and initial storage, run
/// the initial-balance transfer from the sender, record the
/// classification of the new contract under `origin`, and return the
/// success body that the post-execution burn pass will charge. User-
/// issued and internal MIR originations pass [`Origin::Native`]; the
/// alias-forwarder materialization path passes [`Origin::Alias`].
pub fn originate_contract<'a, Host>(
    ctx: &mut TcCtx<'a, Host>,
    contract: ContractKt1Hash,
    sender_account: &impl TezlinkAccount,
    initial_balance: &Narith,
    script_code: Option<&[u8]>,
    script_storage: TypedValue<'a>,
    origin: &Origin,
) -> Result<OriginationSuccess, OriginationError>
where
    Host: StorageV1,
{
    // If the origination is internal the big map are handled by the first transfer
    // The big_maps vector will be filled only if the origination is "external"

    let (new_storage, lazy_storage_diff) =
        handle_storage_with_big_maps(ctx, script_storage)?;

    // Drain the lazy-storage size delta accumulated by the big-map hooks in
    // `handle_storage_with_big_maps`, on the same footing as the transfer path
    // (and as `big_map_diff`): every frame drains its per-operation accumulator
    // so the next one starts empty. The delta is the contribution of the
    // big-maps present in the initial storage; it is folded into the contract's
    // initial `used_bytes` by `init` below.
    // TODO(L2-1481): make this draining structurally safe so a hook firing
    // without a matching drain cannot be silently mispriced.
    let lazy_storage_size_diff = ctx.interpret_context.take_lazy_storage_size_diff();

    // Set the storage of the contract
    let smart_contract = context::originated_from_kt1(&contract)
        .map_err(|_| OriginationError::FailedToFetchOriginated)?;

    // Charge gas for the durable writes `smart_contract.init` performs: the
    // code and storage blobs (their own size), plus a single write of the
    // aggregated `/info` accounting record, charged at the exact number of
    // bytes written. `init` builds the record from the blob lengths without
    // reading it back — origination always targets a fresh KT1 — so no read is
    // charged. The charge precedes the write inside `init`.
    //
    // `init` writes the code and storage blobs as two separate durable
    // writes (`store_write_all` each), so a code-bearing origination is
    // billed two write bases over the combined blob payload — one base per
    // physical write, matching the transfer path. A code-less origination
    // (`script_code = None`, Tezos X alias) writes no `/data/code`, so only
    // the storage blob is written and a single base is billed.
    let code_size = script_code.map_or(0, |c| c.len()) as u64;
    let storage_size = new_storage.len() as u64;
    let blob_writes = if script_code.is_some() { 2 } else { 1 };
    let init_payload_bytes = code_size.saturating_add(storage_size);
    let info_bytes_written = OriginatedContractInfo::for_new_contract(
        code_size,
        storage_size,
        lazy_storage_size_diff.clone(),
    )
    .encoded_len()
    .map_err(|_| OriginationError::CantInitContract)?;
    consume_storage_write_milligas(ctx.operation_gas, blob_writes, init_payload_bytes)
        .map_err(OriginationError::OutOfGas)?;
    consume_storage_write_milligas(ctx.operation_gas, 1, info_bytes_written)
        .map_err(OriginationError::OutOfGas)?;

    let StorageSpace {
        used_bytes: total_size,
        allocated_bytes: paid_storage_size_diff,
    } = smart_contract
        .init(ctx.host, script_code, &new_storage, lazy_storage_size_diff)
        .map_err(|_| OriginationError::CantInitContract)?;

    // There's this line in the origination `assert (Compare.Z.(total_size >= Z.zero)) ;`
    //
    // A code-less Tezos X alias (`script_code = None`) resolves to the shared
    // implementation and carries no own code, so its total size is just its
    // storage — and re-materializing an already-materialized alias sees a zero
    // storage delta. Neither is an "empty contract" in the sense this guard
    // rejects, so it only applies to contracts that ship their own code.
    if script_code.is_some() {
        match BigUint::try_from(total_size.0.clone()) {
            Ok(b) if !b.is_zero() => b,
            _ => return Err(OriginationError::CantOriginateEmptyContract),
        };
    }

    // Compute the initial_balance setup of the smart contract as a balance update for the origination.
    let balance_updates =
        compute_balance_updates(sender_account, &smart_contract, initial_balance)
            .map_err(|e| OriginationError::FailedToComputeBalanceUpdate(e.to_string()))?;

    // `apply_balance_changes` writes and reads the giver and receiver balances,
    // two reads and writes each for an amount, so 4 reads and 4 writes in total.
    consume_storage_write_milligas(ctx.operation_gas, 4, COUNTER_SIZE)
        .map_err(OriginationError::OutOfGas)?;
    consume_storage_read_milligas(ctx.operation_gas, 4, COUNTER_SIZE)
        .map_err(OriginationError::OutOfGas)?;

    // Apply the initial-balance transfer (sender → smart_contract).
    apply_balance_changes(
        ctx.host,
        sender_account,
        &smart_contract,
        &initial_balance.0,
    )
    .map_err(|_| OriginationError::FailedToApplyBalanceUpdate)?;

    // Record the classification of the new contract.
    context::record_origin(ctx.host, &contract, origin)
        .map_err(|_| OriginationError::CantInitContract)?;

    let origination_success = OriginationSuccess {
        balance_updates,
        originated_contracts: vec![Originated { contract }],
        consumed_milligas: ctx.operation_gas.get_and_reset_milligas_consumed()?,
        storage_size: total_size,
        paid_storage_size_diff,
        lazy_storage_diff,
    };
    Ok(origination_success)
}

/// Prepares balance updates in the format expected by the Tezos operation.
fn compute_balance_updates(
    giver: &impl TezlinkAccount,
    receiver: &impl TezlinkAccount,
    amount: &Narith,
) -> Result<Vec<BalanceUpdate>, num_bigint::TryFromBigIntError<num_bigint::BigInt>> {
    if amount.eq(&0_u64.into()) {
        return Ok(vec![]);
    };

    let giver_delta = BigInt::from_biguint(num_bigint::Sign::Minus, amount.into());
    let receiver_delta = BigInt::from_biguint(num_bigint::Sign::Plus, amount.into());

    let giver_update = BalanceUpdate {
        balance: Balance::Account(giver.contract()),
        changes: giver_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    let receiver_update = BalanceUpdate {
        balance: Balance::Account(receiver.contract()),
        changes: receiver_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    Ok(vec![giver_update, receiver_update])
}

/// Applies balance changes by updating both source and destination accounts.
fn apply_balance_changes<Host>(
    host: &mut Host,
    giver_account: &impl TezlinkAccount,
    receiver_account: &impl TezlinkAccount,
    amount: &num_bigint::BigUint,
) -> Result<(), TransferError>
where
    Host: StorageV1,
{
    let giver_balance = giver_account
        .balance(host)
        .map_err(|_| TransferError::FailedToFetchSenderBalance)?;
    let new_giver_balance = match giver_balance.0.checked_sub(amount) {
        None => {
            return Err(TransferError::BalanceTooLow(BalanceTooLow {
                contract: giver_account.contract(),
                balance: giver_balance,
                amount: amount.into(),
            }));
        }
        Some(new_source_balance) => new_source_balance.into(),
    };
    giver_account
        .set_balance(host, &new_giver_balance)
        .map_err(|_| TransferError::FailedToApplyBalanceChanges)?;
    let receiver_balance = receiver_account
        .balance(host)
        .map_err(|_| TransferError::FailedToFetchDestinationBalance)?
        .0;
    let new_receiver_balance = (&receiver_balance + amount).into();
    receiver_account
        .set_balance(host, &new_receiver_balance)
        .map_err(|_| TransferError::FailedToUpdateDestinationBalance)?;

    log!(Debug,
        "Transfer: OK - the new balance of the giver is {new_giver_balance:?} and the new balance of the receiver is {new_receiver_balance:?}"
    );

    Ok(())
}

/// Executes the entrypoint logic of an originated smart contract and returns the new storage.
fn execute_smart_contract_originated<'a>(
    code: Vec<u8>,
    storage: Vec<u8>,
    entrypoint: &Entrypoint,
    value: Micheline<'a>,
    parser: &'a Parser<'a>,
    ctx: &mut impl CtxTrait<'a>,
    allow_forged_lazy_storage_id: AllowForgedLazyStorageId,
) -> Result<(impl Iterator<Item = OperationInfo<'a>>, Vec<u8>), TransferError> {
    // Parse and typecheck the contract
    let contract_micheline = Micheline::decode_raw(&parser.arena, &code, ctx.gas())??;
    let contract_typechecked =
        contract_micheline
            .split_script()?
            .typecheck_script(ctx.gas(), true, false)?;
    let storage_micheline = Micheline::decode_raw(&parser.arena, &storage, ctx.gas())??;

    // Execute the contract
    let (internal_operations, new_storage) = contract_typechecked.interpret(
        ctx,
        &parser.arena,
        value,
        entrypoint,
        &storage_micheline,
        allow_forged_lazy_storage_id,
    )?;

    // Encode the new storage
    let new_storage = new_storage
        .into_micheline_optimized_legacy(&parser.arena, ctx.gas())?
        .encode(ctx.gas())?
        .map_err(|e| TransferError::MichelineSerializationError(e.to_string()))?;

    Ok((internal_operations, new_storage))
}

pub fn get_required_da_fees(
    operation: &Operation,
    da_fee_per_byte_mutez: u64,
) -> anyhow::Result<u64> {
    let op_raw_size = operation
        .to_bytes()
        .map(|bytes| bytes.len())
        .map_err(|_| anyhow::anyhow!("Cannot map operation to its raw size"))?;
    // TODO: See: L2-939.
    // It is needed for the fees to be updated but without
    // a proper gas system 'à-la-Ethereum' we can't use for `FeeUpdates`
    // for now.
    Ok(da_fee_per_byte_mutez.saturating_mul(op_raw_size as u64))
}

#[allow(clippy::too_many_arguments)]
pub fn validate_and_apply_operation<Host>(
    host: &mut Host,
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    hash: OperationHash,
    operation: Operation,
    block_ctx: &BlockCtx,
    skip_signature_check: bool,
    required_fees: Option<u64>,
    fee_refund_config: Option<FeeRefundConfig>,
    safe_roots: &[tezos_smart_rollup_host::path::OwnedPath],
) -> Result<Vec<ProcessedOperation>, OperationError>
where
    Host: StorageV1,
{
    // Sum declared fees (mutez) from all operation contents before
    // `operation` is moved into validation.
    let fee_refund_config = if let Some(config) = fee_refund_config {
        let total_fees: u64 = operation
            .content
            .iter()
            .map(|c| &c.fee().0)
            .sum::<BigUint>()
            .to_u64()
            .ok_or_else(|| {
                OperationError::BlockAbort("total fees do not fit in u64".into())
            })?;
        Some((config, total_fees))
    } else {
        None
    };

    let mut safe_host = SafeStorage {
        host,
        world_states: safe_roots.to_vec(),
    };

    safe_host.start()?;

    log!(Debug, "Verifying that the batch is valid");

    let validation_info = match validate::execute_validation(
        &mut safe_host,
        operation,
        skip_signature_check,
        required_fees,
    ) {
        Ok(validation_info) => validation_info,
        Err(validity_err) => {
            log!(Debug, "Reverting the changes because the batch is invalid.");
            safe_host.revert()?;
            return Err(OperationError::Validation(validity_err));
        }
    };

    log!(Debug, "Batch is valid!");

    safe_host.promote()?;
    // Skip trace promotion (and its store_has probe) when the tracer is off:
    // no trace data was written, so there is nothing to move out of /tmp.
    if block_ctx.tracing_enabled {
        safe_host.promote_trace()?;
    }
    if block_ctx.http_trace_enabled {
        safe_host.promote_http_trace()?;
    }
    safe_host.start()?;

    // Each operation uses 0-based nonces; block-sequential nonces are
    // assigned at block finalization by renumber_nonces().
    let mut nonce_counter: u16 = 0;
    let mut origination_nonce = OriginationNonce::initial(hash);
    // We use `mut` here because apply_batch does not handle fee refund,
    // so we append the refund balance updates to processed_ops afterwards.
    let (mut processed_ops, applied) = apply_batch(
        &mut safe_host,
        registry,
        journal,
        &mut origination_nonce,
        validation_info,
        block_ctx,
        &mut nonce_counter,
    )
    .map_err(OperationError::BlockAbort)?;

    if applied {
        log!(
            Debug,
            "Committing the changes because the batch was successfully applied."
        );
        safe_host.promote()?;
        if block_ctx.tracing_enabled {
            safe_host.promote_trace()?;
        }
        if block_ctx.http_trace_enabled {
            safe_host.promote_http_trace()?;
        }
    } else {
        log!(
            Debug,
            "Reverting the changes because some operation failed."
        );
        log!(Debug, "Processed operations: {processed_ops:#?}");
        safe_host.revert()?;
        // Clear the in-memory EVM journal: safe_host.revert() only rolls
        // back Tezos durable storage but cannot affect the in-memory REVM
        // JournalInner. Without this, commit_evm_journal_from_external()
        // would persist EVM state changes from a backtracked operation.
        journal.evm.clear();
        // The captured cross-runtime originator now lives on the shared
        // journal (not `evm`), so drop it here too — a backtracked
        // operation's originator must not leak into the next one.
        journal.reset_original_source();
    }

    // Apply fee refund after all transactional work is done.
    // This runs outside the SafeStorage transaction (after promote/revert)
    // so that the refund applies in both cases: applied and failed operations.
    // Uses safe_host.host directly since the transactional phase is complete.
    if let Some((config, total_fees)) = fee_refund_config {
        let fee_refund = compute_fee_refund(total_fees, &processed_ops, &config);
        apply_fee_refund(safe_host.host, &mut processed_ops, fee_refund)
            .map_err(|e| OperationError::BlockAbort(format!("Fee refund: {e}")))?;
    }

    Ok(processed_ops)
}

/// Credit the source with a fee refund and record balance updates in the receipt.
///
/// Returns Ok(()) with no effect when `fee_refund == 0`.
fn apply_fee_refund<Host>(
    host: &mut Host,
    processed_operations: &mut [ProcessedOperation],
    fee_refund: u64,
) -> Result<(), anyhow::Error>
where
    Host: StorageV1,
{
    if fee_refund == 0 {
        return Ok(());
    }

    // Validate conversions before any storage mutation.
    let source_delta: i64 = i64::try_from(fee_refund)?;
    let block_fees_delta: i64 = -source_delta;

    // Safe: validate_and_apply_operation rejects empty batches (ValidityError::EmptyBatch).
    let first_op = processed_operations
        .first_mut()
        .ok_or_else(|| anyhow::anyhow!("Fee refund: empty processed_operations"))?;

    let source_pkh = first_op.operation_with_metadata.content.source()?.clone();

    // Credit source account with the refund.
    let source_account = context::implicit_from_public_key_hash(&source_pkh)?;
    source_account.add_balance(host, fee_refund)?;

    log!(
        Debug,
        "Fee refund: {} mutez credited to {}",
        fee_refund,
        source_pkh
    );

    // Record balance updates in the receipt (same ordering as
    // compute_fees_balance_updates: Account first, BlockFees second).
    let contract = Contract::Implicit(source_pkh);
    let receipt = &mut first_op.operation_with_metadata.receipt;
    receipt.push_balance_update(BalanceUpdate {
        balance: Balance::Account(contract),
        changes: source_delta,
        update_origin: UpdateOrigin::BlockApplication,
    });
    receipt.push_balance_update(BalanceUpdate {
        balance: Balance::BlockFees,
        changes: block_fees_delta,
        update_origin: UpdateOrigin::BlockApplication,
    });

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn apply_batch<Host>(
    host: &mut Host,
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    origination_nonce: &mut OriginationNonce,
    validation_info: validate::ValidatedBatch<TezosImplicitAccount>,
    block_ctx: &BlockCtx,
    nonce_counter: &mut u16,
) -> Result<(Vec<ProcessedOperation>, bool), String>
where
    Host: StorageV1,
{
    let validate::ValidatedBatch {
        source_account,
        source_public_key,
        validated_operations,
    } = validation_info;
    let mut first_failure: Option<usize> = None;
    let mut processed_ops = Vec::with_capacity(validated_operations.len());
    let mut next_temporary_id = BigMapId { value: (-1).into() };
    for (index, validated_operation) in validated_operations.into_iter().enumerate() {
        log!(
            Debug,
            "Applying operation #{index} in the batch with counter {:?}.",
            validated_operation.content.counter
        );
        let processed = if first_failure.is_some() {
            log!(
                Debug,
                "Skipping this operation because we already failed on {first_failure:?}."
            );
            ProcessedOperation {
                operation_with_metadata: produce_skipped_receipt(
                    validated_operation.content,
                    validated_operation.balance_updates,
                ),
                // Skipped operations were not executed, so consumed no gas.
                operation_consumed_milligas: 0,
            }
        } else {
            apply_operation(
                host,
                registry,
                journal,
                origination_nonce,
                &source_account,
                &source_public_key,
                validated_operation,
                &mut next_temporary_id,
                block_ctx,
                nonce_counter,
            )?
        };

        if first_failure.is_none()
            && !processed.operation_with_metadata.receipt.is_applied()
        {
            first_failure = Some(index);
        }

        processed_ops.push(processed);
    }

    // Clear all the temporaries big_map after the application of the batch
    let cleared = clear_temporary_big_maps(host, &mut next_temporary_id);

    if let Err(lazy_storage_err) = cleared {
        log!(
            Error,
            "Cleaning the temporary big_map in the storage failed: {lazy_storage_err}"
        )
    }

    if let Some(failure_idx) = first_failure {
        processed_ops[..failure_idx]
            .iter_mut()
            .for_each(|processed| {
                OperationResultSum::transform_result_backtrack(
                    &mut processed.operation_with_metadata.receipt,
                )
            });
        return Ok((processed_ops, false));
    }

    Ok((processed_ops, true))
}

fn log_on_operation_failure<T, E: std::fmt::Debug>(
    operation_name: &str,
    result: &Result<T, E>,
) {
    if let Err(err) = result {
        log!(Error, "{operation_name} failed because of: {err:?}");
    }
}

#[allow(clippy::too_many_arguments)]
fn apply_operation<Host>(
    host: &mut Host,
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    origination_nonce: &mut OriginationNonce,
    source_account: &TezosImplicitAccount,
    source_public_key: &[u8],
    validated_operation: validate::ValidatedOperation,
    next_temporary_id: &mut BigMapId,
    block_ctx: &BlockCtx,
    nonce_counter: &mut u16,
) -> Result<ProcessedOperation, String>
where
    Host: StorageV1,
{
    let mut internal_operations_receipts: Vec<TaggedInternalOp> = Vec::new();
    let mut gas = validated_operation.gas;
    let mut tc_ctx = TcCtx {
        host,
        operation_gas: &mut gas,
        big_map_diff: BTreeMap::new(),
        interpret_context: InterpretContext::new(),
        next_temporary_id,
    };
    let parser = Parser::new();
    // Block-cumulative internal-operation counter: the block's prior count
    // (`internal_operations_base`) plus what this batch already produced
    // (`nonce_counter`). The MIR cap fires at L1's 65,535 limit against this.
    let mut counter = block_ctx
        .internal_operations_base
        .saturating_add(u128::from(*nonce_counter));
    let storage_limit = &validated_operation.content.storage_limit;
    let balance_updates = validated_operation.balance_updates;
    let receipt = match &validated_operation.content.operation {
        OperationContent::Reveal(RevealContent { pk, .. }) => {
            let reveal_result = reveal(&mut tc_ctx, source_account, pk);
            log_on_operation_failure("Reveal", &reveal_result);
            OperationResultSum::Reveal(finalize_and_burn::<_, RevealContent, _>(
                tc_ctx.host,
                source_account,
                storage_limit,
                balance_updates,
                0,
                0,
                reveal_result.map_err(Into::into),
                internal_operations_receipts,
            ))
        }
        OperationContent::Transfer(TransferContent {
            amount,
            destination,
            parameters,
        }) => {
            let mut operation_ctx = OperationCtx {
                source: source_account,
                counter: &mut counter,
                applied_counters: BTreeSet::new(),
                origination_nonce,
                level: block_ctx.level,
                now: block_ctx.now,
                chain_id: block_ctx.chain_id,
                source_public_key,
                crac_chain_depth: 0,
                crac_origin: None,
                delegated_storage_cost: 0,
            };
            // Watermarks for `drain_reentrant_crac_ops`: a top-level
            // manager op whose destination is the gateway enters EVM
            // directly (no intermediate Michelson contract), so
            // `execute_internal_operations` never runs and its drain
            // never fires.  Capture watermarks here so that any nested
            // EVM→Michelson CRAC receipts are folded into
            // `internal_operations_receipts` after the call returns,
            // mirroring what `execute_internal_operations` does per
            // internal operation.
            let pending_crac_receipts_before =
                journal.michelson.pending_crac_receipts.len();
            let failed_crac_receipts_before =
                journal.michelson.failed_crac_receipts.len();
            let backtracked_crac_receipts_before =
                journal.michelson.backtracked_crac_receipts.len();
            let (transfer_result, top_level_delegated_delta) = match transfer_external(
                &mut tc_ctx,
                &mut operation_ctx,
                registry,
                journal,
                amount,
                destination,
                parameters,
                &mut internal_operations_receipts,
                &parser,
                nonce_counter,
            ) {
                Ok((target, delta)) => (Ok(target), delta),
                Err(e) => (Err(e), 0),
            };
            // Drain any re-entrant CRAC ops that accumulated during the
            // top-level gateway call, mirroring `execute_internal_operations`.
            let reentrant_ops = crate::enshrined_contracts::drain_reentrant_crac_ops(
                journal,
                pending_crac_receipts_before,
                failed_crac_receipts_before,
                backtracked_crac_receipts_before,
            );
            internal_operations_receipts
                .extend(reentrant_ops.into_iter().map(TaggedInternalOp::from_crac));
            let transfer_result = match transfer_result {
                Ok(v) => Ok(v),
                Err(CracError::BlockAbort(msg)) => {
                    log!(
                        Error,
                        "Block abort was triggered by a Michelson transfer: {msg}"
                    );
                    return Err(msg);
                }
                Err(CracError::Operation(e)) => {
                    log!(Error, "Transfer failed because of: {e:?}");
                    Err(e)
                }
            };
            OperationResultSum::Transfer(finalize_and_burn::<_, TransferContent, _>(
                tc_ctx.host,
                source_account,
                storage_limit,
                balance_updates,
                operation_ctx.delegated_storage_cost,
                top_level_delegated_delta,
                transfer_result.map_err(Into::into),
                internal_operations_receipts,
            ))
        }
        OperationContent::Origination(OriginationContent {
            ref balance,
            delegate: _,
            ref script,
        }) => {
            let address = origination_nonce.generate_kt1();
            let typechecked_storage =
                typecheck_code_and_storage(&mut tc_ctx, &parser, script);
            let origination_result = match typechecked_storage {
                Ok(storage) => originate_contract(
                    &mut tc_ctx,
                    address,
                    source_account,
                    balance,
                    Some(&script.code),
                    storage,
                    &Origin::Native,
                ),
                Err(err) => Err(err),
            };
            log_on_operation_failure("Origination", &origination_result);
            OperationResultSum::Origination(
                finalize_and_burn::<_, OriginationContent, _>(
                    tc_ctx.host,
                    source_account,
                    storage_limit,
                    balance_updates,
                    0,
                    0,
                    origination_result.map_err(|e| e.into()),
                    internal_operations_receipts,
                ),
            )
        }
    };

    // Read the total gas consumed since the start of the operation, immune
    // to baseline resets performed by per-segment measurements (e.g. via
    // `get_and_reset_milligas_consumed` while attributing gas to internal
    // sub-operations). Skipped operations never reach this point: they are
    // built directly in `apply_batch` with `operation_consumed_milligas: 0`.
    let operation_consumed_milligas = tc_ctx.operation_gas.total_milligas_consumed();
    Ok(ProcessedOperation {
        operation_with_metadata: OperationWithMetadata {
            content: validated_operation.content.into_manager_operation_content(),
            receipt,
        },
        operation_consumed_milligas,
    })
}

/// Shared test utilities for gateway testing
#[cfg(test)]
pub(crate) mod test_utils {
    pub use tezosx_interfaces::testing::MockRegistry;
}

#[cfg(test)]
mod tests {
    use crate::account_storage::TezosImplicitAccount;
    use crate::account_storage::{
        self, Code, TezosImplicitAccountTrait, TezosOriginatedAccount,
    };
    use crate::context;
    use crate::{
        account_storage::TezlinkOriginatedAccount, address::OriginationNonce,
        mir_ctx::BlockCtx,
    };
    use mir::ast::big_map::BigMapId;
    use mir::ast::{Address, Entrypoint, IntoMicheline, Micheline, Type, TypedValue};
    use mir::context::TypecheckingCtx;
    use mir::gas::{self, Gas};
    use mir::parser::Parser;
    use mir::typechecker::{typecheck_value, AllowForgedLazyStorageId};
    use num_traits::ops::checked::CheckedSub;
    use num_traits::ToPrimitive;
    use pretty_assertions::assert_eq;
    use std::collections::BTreeMap;
    use std::fs::read_to_string;
    use tezos_crypto_rs::hash::{
        ContractKt1Hash, HashTrait, OperationHash, SecretKeyEd25519,
    };
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::types::{Narith, Zarith};
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup::types::{PublicKey, PublicKeyHash};
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_tezlink::{
        block::TezBlock,
        operation::{
            sign_operation, ManagerOperation, ManagerOperationContent,
            ManagerOperationContentConv, Operation, OperationContent, OriginationContent,
            Parameters, RevealContent, Script, TransferContent,
        },
        operation_result::{
            ApplyOperationError, ApplyOperationErrors, BacktrackedResult, Balance,
            BalanceTooLow, BalanceUpdate, ContentResult, CounterError,
            InternalContentWithMetadata, InternalOperationSum, OperationKind,
            OperationResult, OperationResultSum, OperationWithMetadata, Originated,
            OriginationError, OriginationSuccess, RevealError, RevealSuccess,
            TransferError, TransferSuccess, TransferTarget, UpdateOrigin, ValidityError,
        },
    };
    use tezosx_interfaces::RuntimeId;
    use tezosx_journal::TezosXJournal;
    use typed_arena::Arena;

    use crate::enshrined_contracts::{CracError, PERSISTED_ERROR_PER_BYTE_MILLIGAS};
    use crate::gas::TezlinkOperationGas;
    use crate::make_default_ctx;
    use crate::storage_fees::{COST_PER_BYTES, ORIGINATION_SIZE};
    use crate::storage_read_cost_milligas;
    use crate::{
        account_storage::{Manager, TezlinkAccount},
        burn_pass, cross_runtime_transfer, validate_and_apply_operation,
        CracTransferError, FeeRefundConfig, OperationError, ProcessedOperation,
        TaggedInternalOp,
    };
    use tezos_smart_rollup::types::Timestamp;
    use tezos_smart_rollup_host::path::OwnedPath;
    use tezos_tezlink::enc_wrappers::BlockNumber;

    /// Test-only SafeStorage root matching the production Michelson accounts
    /// root, used to build the account/big-map paths in tests.
    fn test_root() -> OwnedPath {
        OwnedPath::from(&context::TEZOS_ACCOUNTS_ROOT)
    }

    /// Test-only SafeStorage roots matching [`test_root`], so the inner
    /// SafeStorage wrap inside `validate_and_apply_operation` covers the
    /// test account subtree.
    fn test_safe_roots() -> Vec<OwnedPath> {
        vec![test_root()]
    }
    use crate::{get_required_da_fees, TcCtx};
    use primitive_types::U256;
    use tezosx_interfaces::testing::NotWiredRegistry;

    macro_rules! block_ctx {
        () => {
            BlockCtx {
                level: &0u32.into(),
                now: &0i64.into(),
                chain_id: &HashTrait::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
                internal_operations_base: 0,
                tracing_enabled: false,
                http_trace_enabled: false,
            }
        };
    }

    #[derive(Clone)]
    struct Bootstrap {
        pkh: PublicKeyHash,
        pk: PublicKey,
        sk: SecretKeyEd25519,
    }

    fn bootstrap1() -> Bootstrap {
        Bootstrap {
            pkh: PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                .unwrap(),
            pk: PublicKey::from_b58check(
                "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
            )
            .unwrap(),
            sk: SecretKeyEd25519::from_base58_check(
                "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh",
            )
            .unwrap(),
        }
    }

    fn bootstrap2() -> Bootstrap {
        Bootstrap {
            pkh: PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN")
                .unwrap(),
            pk: PublicKey::from_b58check(
                "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9",
            )
            .unwrap(),
            sk: SecretKeyEd25519::from_base58_check(
                "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo",
            )
            .unwrap(),
        }
    }

    const CONTRACT_1: &str = "KT1EFxv88KpjxzGNu1ozh9Vta4BaV3psNknp";
    const CONTRACT_2: &str = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
    const CONTRACT_3: &str = "KT1AEfeckNbdEYwaMKkytBwPJPycz7jdSGea";

    static SCRIPT: &str = r#"
        parameter string;
        storage string;
        code {
            CAR ;
            NIL operation ;
            PAIR
        }"#;

    static UNIT_SCRIPT: &str = r#"
        parameter unit;
        storage unit;
        code {
            CAR ;
            NIL operation ;
            PAIR
        }"#;

    static FAILING_SCRIPT: &str = r#"
        parameter unit;
        storage unit;
        code {
            DROP;
            PUSH string "This contract always fails";
            FAILWITH
        }"#;

    // Contract whose storage is `pair (big_map string string) unit`.
    // Used by big-map origination billing tests.
    static BIG_MAP_UNIT_SCRIPT: &str = r#"
        parameter unit;
        storage (pair (big_map string string) unit);
        code {
            CDR;
            NIL operation;
            PAIR
        }"#;

    /// The entrypoints RPC (`get_contract_entrypoint`) goes through
    /// `code()`, so a code-less alias transparently exposes the entrypoints of
    /// the shared implementation — here the `default : unit` of `UNIT_SCRIPT`.
    #[test]
    fn alias_entrypoints_resolve_via_shared_implementation() {
        use crate::account_storage::write_alias_implementation;
        use crate::context::code::origin_path;
        use crate::get_contract_entrypoint;
        use mir::ast::{AddressHash, Entrypoint, Type};
        use tezosx_interfaces::{AliasInfo, Origin, RuntimeId};

        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();

        // Seed the shared implementation with a real Michelson script.
        let code = parser
            .parse_top_level(UNIT_SCRIPT)
            .unwrap()
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        write_alias_implementation(&mut host, &code).unwrap();

        // A code-less KT1 classified as an alias.
        let kt1 = ContractKt1Hash::from_b58check(CONTRACT_1).unwrap();
        let account = context::originated_from_kt1(&kt1).unwrap();
        let origin = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: b"0xabc".to_vec(),
        });
        let mut buf = vec![];
        origin.bin_write(&mut buf).unwrap();
        host.store_write_all(&origin_path(&account).unwrap(), &buf)
            .unwrap();

        let entrypoints =
            get_contract_entrypoint(&host, &AddressHash::Kt1(kt1), &mut Gas::default())
                .expect("alias entrypoints resolve to the shared implementation");
        assert_eq!(entrypoints.get(&Entrypoint::default()), Some(&Type::Unit));
    }

    use crate::{upgrade_alias_implementation, AliasUpgradeError};

    /// Encode a Michelson script source into the Micheline bytes the alias
    /// implementation slot holds.
    fn encode_script(src: &str) -> Vec<u8> {
        mir::parser::Parser::new()
            .parse_top_level(src)
            .unwrap()
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap()
    }

    // `default : unit`.
    const UPGRADE_BASE_SCRIPT: &str =
        "parameter unit; storage unit; code { CDR; NIL operation; PAIR }";
    // `default : unit` plus an added `foo : string` — a valid superset.
    const UPGRADE_SUPERSET_SCRIPT: &str = "parameter (or (unit %default) (string \
         %foo)); storage unit; code { CDR; NIL operation; PAIR }";

    #[test]
    fn upgrade_accepts_entrypoint_superset() {
        let mut host = MockKernelHost::default();
        account_storage::write_alias_implementation(
            &mut host,
            &encode_script(UPGRADE_BASE_SCRIPT),
        )
        .unwrap();

        let superset = encode_script(UPGRADE_SUPERSET_SCRIPT);
        upgrade_alias_implementation(&mut host, &superset, &mut Gas::default())
            .expect("adding an entrypoint while keeping `default` must be accepted");
        assert_eq!(
            account_storage::read_alias_implementation(&host).unwrap(),
            Some(superset)
        );
    }

    #[test]
    fn upgrade_rejects_dropped_entrypoint() {
        let mut host = MockKernelHost::default();
        // Start from the superset (default + foo)...
        account_storage::write_alias_implementation(
            &mut host,
            &encode_script(UPGRADE_SUPERSET_SCRIPT),
        )
        .unwrap();

        // ...then try to drop `foo` by going back to the bare default script.
        let dropped = encode_script(UPGRADE_BASE_SCRIPT);
        let err = upgrade_alias_implementation(&mut host, &dropped, &mut Gas::default())
            .expect_err("dropping an exposed entrypoint must be rejected");
        assert!(matches!(err, AliasUpgradeError::EntrypointRemoved(_)));
        // The slot is left untouched.
        assert_eq!(
            account_storage::read_alias_implementation(&host).unwrap(),
            Some(encode_script(UPGRADE_SUPERSET_SCRIPT))
        );
    }

    #[test]
    fn upgrade_rejects_entrypoint_type_change() {
        let mut host = MockKernelHost::default();
        account_storage::write_alias_implementation(
            &mut host,
            &encode_script(UPGRADE_BASE_SCRIPT),
        )
        .unwrap();

        // `default : string` changes the type of the existing `default` entrypoint.
        let retyped = encode_script(
            "parameter string; storage unit; code { CDR; NIL operation; PAIR }",
        );
        let err = upgrade_alias_implementation(&mut host, &retyped, &mut Gas::default())
            .expect_err("changing an entrypoint's type must be rejected");
        assert!(matches!(err, AliasUpgradeError::EntrypointTypeChanged(_)));
    }

    #[test]
    fn upgrade_rejects_storage_type_change() {
        let mut host = MockKernelHost::default();
        // Base storage is `string` (like the forwarder).
        account_storage::write_alias_implementation(
            &mut host,
            &encode_script(UPGRADE_SUPERSET_SCRIPT),
        )
        .unwrap();

        // Same entrypoints, but `storage nat` — would brick every alias (whose
        // stored storage is a `string`) at execution.
        let retyped_storage = encode_script(
            "parameter (or (unit %default) (string %foo)); storage nat; code { CDR; \
             NIL operation; PAIR }",
        );
        let err = upgrade_alias_implementation(
            &mut host,
            &retyped_storage,
            &mut Gas::default(),
        )
        .expect_err("changing the storage type must be rejected");
        assert!(matches!(err, AliasUpgradeError::StorageTypeChanged));
    }

    #[test]
    fn upgrade_rejects_widening_without_default_annotation() {
        // Documents the `%default` requirement: widening the parameter via an
        // `or` *without* annotating the old branch `%default` changes the
        // implicit `default` entrypoint's type (it becomes the whole `or`), so
        // the upgrade is correctly rejected. The safe path is to annotate
        // `(unit %default)` — exercised by `upgrade_accepts_entrypoint_superset`.
        let mut host = MockKernelHost::default();
        account_storage::write_alias_implementation(
            &mut host,
            &encode_script(UPGRADE_BASE_SCRIPT),
        )
        .unwrap();

        let widened_no_default = encode_script(
            "parameter (or (unit %a) (string %b)); storage unit; code { CDR; NIL \
             operation; PAIR }",
        );
        let err = upgrade_alias_implementation(
            &mut host,
            &widened_no_default,
            &mut Gas::default(),
        )
        .expect_err("widening without %default changes `default`'s type");
        assert!(matches!(err, AliasUpgradeError::EntrypointTypeChanged(_)));
    }

    #[test]
    fn upgrade_rejects_invalid_script() {
        let mut host = MockKernelHost::default();
        account_storage::write_alias_implementation(
            &mut host,
            &encode_script(UPGRADE_BASE_SCRIPT),
        )
        .unwrap();

        let err = upgrade_alias_implementation(
            &mut host,
            b"not a michelson script",
            &mut Gas::default(),
        )
        .expect_err("a non-typeable script must be rejected");
        assert!(matches!(err, AliasUpgradeError::InvalidScript));
        // The core safety property: the slot is left untouched on rejection.
        assert_eq!(
            account_storage::read_alias_implementation(&host).unwrap(),
            Some(encode_script(UPGRADE_BASE_SCRIPT))
        );
    }

    #[test]
    fn upgrade_rejects_corrupt_current_implementation() {
        // A non-typeable slot means the current implementation is corrupt.
        let mut host = MockKernelHost::default();
        account_storage::write_alias_implementation(&mut host, b"garbage").unwrap();

        let err = upgrade_alias_implementation(
            &mut host,
            &encode_script(UPGRADE_BASE_SCRIPT),
            &mut Gas::default(),
        )
        .expect_err("a corrupt current implementation must be rejected");
        assert!(matches!(
            err,
            AliasUpgradeError::CurrentImplementationInvalid
        ));
    }

    #[test]
    fn upgrade_into_empty_slot_is_accepted() {
        // No current implementation: nothing to preserve.
        let mut host = MockKernelHost::default();
        let code = encode_script(UPGRADE_BASE_SCRIPT);
        upgrade_alias_implementation(&mut host, &code, &mut Gas::default())
            .expect("seeding an empty slot has no monotonicity constraint");
        assert_eq!(
            account_storage::read_alias_implementation(&host).unwrap(),
            Some(code)
        );
    }

    static SCRIPT_EMITING_INTERNAL_TRANSFER: &str = r#"
        parameter (list address);
        storage unit;
        code {
            CAR;
            MAP {
                CONTRACT unit;
                IF_NONE { PUSH string "Invalid contract address"; FAILWITH } {};
                PUSH mutez 10;
                PUSH unit Unit;
                TRANSFER_TOKENS
            };
            PUSH unit Unit;
            SWAP;
            PAIR
        }"#;

    /// Build the whole CREATE_CONTRACT { ... } block.
    fn make_create_contract_block(
        param_ty: &str,
        storage_ty: &str,
        code_body: &str, // inside `code { ... }`
    ) -> String {
        // Michelson `{ ... }` need to be doubled to escape Rust’s format! braces
        format!(
            "
                parameter {param_ty} ;
                storage {storage_ty} ;
                code {{ {code_body} }}
            "
        )
    }

    /// Embed a whole contract creation block into a surrounding
    /// script, with delegate hard-coded as `NONE key_hash`.
    /// Also, the created script must have a storage of type `unit`.
    fn make_script_emitting_internal_origination(create_block: &str) -> String {
        format!(
            r#"
            parameter unit;
            storage (option address);
            code {{
                DROP;
                UNIT;                   # starting storage for contract
                AMOUNT;                 # starting balance for the child
                NONE key_hash;          # delegate is always None
                CREATE_CONTRACT {{ {create_block} }};
                DIP {{ SOME ; NIL operation }} ; CONS ; PAIR
            }}"#
        )
    }

    /// Call CREATE_CONTRACT three times:
    /// Result of first call is dropped, result of other calls are swapped.
    fn make_script_emitting_two_internal_originations(
        create_block_1: &str,
        create_block_2: &str,
        create_block_3: &str,
    ) -> String {
        format!(
            r#"
            parameter unit;
            storage (option (pair address address address));
            code {{
                DROP;
                UNIT;                   # starting storage for contract
                PUSH mutez 0;           # starting balance for the child
                NONE key_hash;          # delegate is always None
                CREATE_CONTRACT {{ {create_block_1} }}; DROP;

                PUSH nat 1;             # starting storage for contract
                PUSH mutez 0;           # starting balance for the child
                NONE key_hash;          # delegate is always None
                CREATE_CONTRACT {{ {create_block_2} }};
                DIP {{ PAIR ; NIL operation }} ; CONS ;

                PUSH bytes 0x;          # starting storage for contract
                PUSH mutez 0;           # starting balance for the child
                NONE key_hash;          # delegate is always None
                CREATE_CONTRACT {{ {create_block_3} }};
                DIP {{ SWAP; DIP {{ PAIR ; SOME }} }} ; CONS ; PAIR
            }}"#
        )
    }

    fn make_operation(
        fee: u64,
        first_counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        content: Vec<OperationContent>,
    ) -> Operation {
        let branch = TezBlock::genesis_block_hash();
        let content = content
            .into_iter()
            .enumerate()
            .map(|(i, c)| -> ManagerOperationContent {
                ManagerOperation {
                    source: source.pkh.clone(),
                    fee: fee.into(),
                    counter: (first_counter + i as u64).into(),
                    operation: c,
                    gas_limit: gas_limit.into(),
                    storage_limit: storage_limit.into(),
                }
                .into_manager_operation_content()
            })
            .collect::<Vec<ManagerOperationContent>>();

        let signature = sign_operation(&source.sk, &branch, &content).unwrap();

        Operation {
            branch,
            content,
            signature,
        }
    }

    fn make_reveal_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source.clone(),
            vec![OperationContent::Reveal(RevealContent {
                pk: source.pk,
                proof: None,
            })],
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn make_transfer_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        amount: Narith,
        destination: Contract,
        parameters: Parameters,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source,
            vec![OperationContent::Transfer(TransferContent {
                amount,
                destination,
                parameters,
            })],
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn make_origination_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        balance: u64,
        script: Script,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source,
            vec![OperationContent::Origination(OriginationContent {
                balance: balance.into(),
                script,
                delegate: None,
            })],
        )
    }

    // This function setups an account that will pass the validity checks
    fn init_account(
        host: &mut impl StorageV1,
        src: &PublicKeyHash,
        amount: u64,
    ) -> TezosImplicitAccount {
        // Setting the account in TezosImplicitAccount
        let contract = Contract::from_b58check(&src.to_b58check())
            .expect("Contract b58 conversion should have succeed");

        let account = context::implicit_from_contract(&contract)
            .expect("Account creation should have succeed");

        // Allocate the account
        account
            .allocate(host)
            .expect("Account allocation should have succeed");

        // Setting the balance to pass the validity check
        account
            .set_balance(host, &amount.into())
            .expect("Set balance should have succeed");

        account
    }

    fn reveal_account(host: &mut impl StorageV1, source: &Bootstrap) {
        let account = context::implicit_from_public_key_hash(&source.pkh)
            .expect("Account creation should have succeed");
        account.set_manager_public_key(host, &source.pk).unwrap()
    }

    // This function sets up an account that will pass the validity checks
    fn init_contract(
        host: &mut impl StorageV1,
        src: &ContractKt1Hash,
        script: &str,
        storage_micheline: &Micheline,
        balance: &Narith,
    ) -> TezlinkOriginatedAccount {
        // Setting the account in TezosImplicitAccount
        let contract = Contract::Originated(src.clone());

        let account = context::originated_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let parser = mir::parser::Parser::new();
        let script_micheline = parser.parse_top_level(script).unwrap();

        account
            .init(
                host,
                Some(
                    &script_micheline
                        .encode(&mut Gas::default())
                        .unwrap()
                        .unwrap(),
                ),
                &storage_micheline
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
                0.into(),
            )
            .expect("Account initialisation should have succeeded");

        account
            .set_balance(host, balance)
            .expect("Set balance should have succeeded");

        account
    }

    fn zip_operations(
        operation: Operation,
        receipt: Vec<OperationResultSum>,
    ) -> Vec<OperationWithMetadata> {
        operation
            .content
            .into_iter()
            .zip(receipt)
            .map(|(c, r)| OperationWithMetadata {
                content: c,
                receipt: r,
            })
            .collect::<Vec<OperationWithMetadata>>()
    }

    // Test an operation on an account that has no entry in `/context/contracts/index`
    // This should fail as an EmptyImplicitContract
    #[test]
    fn apply_operation_empty_account() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        // We need to have something written in the durable storage
        // to avoid getting an error when initializing the safe_storage
        let other = bootstrap2();

        let src_account = init_account(&mut host, &other.pkh, 50);

        let operation = make_reveal_operation(15, 1, 1000, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        );

        let expected_error =
            OperationError::Validation(ValidityError::EmptyImplicitContract);

        assert_eq!(
            src_account.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented"
        );

        assert_eq!(result, Err(expected_error));
    }

    // Test that increasing the fees makes the operation fails
    #[test]
    fn apply_operation_cant_pay_fees() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let src_account = init_account(&mut host, &source.pkh, 50);

        // Fees are too high for source's balance
        let operation = make_reveal_operation(100, 1, 1000, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        );

        let expected_error =
            OperationError::Validation(ValidityError::CantPayFees(100_u64.into()));

        assert_eq!(
            src_account.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented"
        );

        assert_eq!(result, Err(expected_error));
    }

    // Test that a wrong counter should make the operation fails
    #[test]
    fn apply_operation_invalid_counter() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let src_account = init_account(&mut host, &source.pkh, 50);

        // Counter is incoherent for source's counter
        let operation = make_reveal_operation(15, 15, 1000, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        );

        let expected_error =
            OperationError::Validation(ValidityError::CounterInTheFuture(CounterError {
                expected: 1_u64.into(),
                found: 15_u64.into(),
            }));

        assert_eq!(
            src_account.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented"
        );

        assert_eq!(result, Err(expected_error));
    }

    // At this point, tests are focused on the content of the operation. We should not revert with ValidityError anymore.
    // Test a reveal operation on an already revealed account
    #[test]
    fn apply_reveal_operation_on_already_revealed_account() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let account = init_account(&mut host, &source.pkh, 50);

        // Setting the manager key of this account to its public_key, this account
        // will be considered as revealed and the reveal operation should fail
        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        account
            .set_manager_public_key(&mut host, &pk)
            .expect("Setting manager field should have succeed");

        // Applying the operation
        let operation = make_reveal_operation(15, 1, 1000, 5, source.clone());
        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        // Reveal operation should fail
        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Reveal(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(source.pkh)),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Failed(
                    vec![RevealError::PreviouslyRevealedKey(pk).into()].into(),
                ),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(
            account.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );

        assert_eq!(receipt, expected_receipt);
    }

    // The live RLP `TezosImplicitAccount` derives an unrevealed account's
    // manager from the account's own public-key hash (it stores only a
    // *revealed* public key, never a separate manager pkh). It therefore
    // cannot represent the standalone layout's "inconsistent manager" state:
    // `set_manager_pk_hash_internal` is a no-op, so `force_set_manager_public_key_hash`
    // leaves the manager consistent and the reveal succeeds. This pins that
    // invariant (the `InconsistentHash` branch of `reveal` is unreachable for
    // the live account) rather than the standalone-only failure path.
    #[test]
    fn reveal_manager_is_always_consistent_on_live_account() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let account = init_account(&mut host, &source.pkh, 50);

        // Attempting to plant an inconsistent manager pkh is a no-op on the
        // live account: the manager stays `NotRevealed(source.pkh)`.
        let inconsistent_pkh =
            PublicKeyHash::from_b58check("tz1UEQcU7M43yUECMpKGJcxCVwHRaP819qhN")
                .expect("PublicKeyHash b58 conversion should have succeed");

        account
            .force_set_manager_public_key_hash(&mut host, &inconsistent_pkh)
            .expect("Setting manager field should have succeed");

        assert_eq!(
            account.manager(&host).unwrap(),
            Manager::NotRevealed(source.pkh.clone()),
            "live account manager is derived from its own pkh, stays consistent"
        );

        let operation = make_reveal_operation(15, 1, 1000, 5, source.clone());

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Reveal(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(source.pkh.clone())),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(RevealSuccess {
                    consumed_milligas: 148496_u64.into(),
                }),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(receipt, expected_receipt);
        assert_eq!(
            account.manager(&host).unwrap(),
            Manager::Revealed(source.pk),
            "reveal records the source's public key as the manager"
        );
        assert_eq!(
            account.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    // Test an invalid operation where the provided public key is inconsistent for the source
    #[test]
    fn apply_reveal_operation_with_an_inconsistent_public_key() {
        let mut host = MockKernelHost::default();

        // Wrong public key for source
        let pk = PublicKey::from_b58check(
            "edpkuT1qccDweCHnvgjLuNUHERpZmEaFZfbWvTzj2BxmTgQBZjaDFD",
        )
        .expect("Public key creation should have succeed");

        let source = Bootstrap { pk, ..bootstrap1() };

        // Even if we don't use it we need to init the account
        let account = init_account(&mut host, &source.pkh, 50);

        let operation = make_reveal_operation(15, 1, 1000, 5, source.clone());

        let result = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        );

        let expected_error = OperationError::Validation(ValidityError::InvalidSignature);

        assert_eq!(
            account.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented"
        );

        assert_eq!(result, Err(expected_error));
    }

    // Test a valid reveal operation, the manager should go from NotRevealed to Revealed
    #[test]
    fn apply_reveal_operation() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let account = init_account(&mut host, &source.pkh, 50);

        let manager = account
            .manager(&host)
            .expect("Read manager should have succeed");

        assert_eq!(manager, Manager::NotRevealed(source.pkh.clone()));

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 1000, 5, source.clone());

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Reveal(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(source.pkh)),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(RevealSuccess {
                    consumed_milligas: 148496_u64.into(),
                }),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(receipt, expected_receipt);

        let manager = account
            .manager(&host)
            .expect("Read manager should have succeed");

        assert_eq!(manager, Manager::Revealed(pk));

        assert_eq!(
            account.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    // Test an invalid transfer operation, source has not enough balance to fulfill the Transfer
    #[test]
    fn apply_transfer_with_not_enough_balance() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let dest = bootstrap2();

        // Setup accounts with 50 mutez in their balance
        let source_account = init_account(&mut host, &source.pkh, 50);
        reveal_account(&mut host, &source);

        let destination_account = init_account(&mut host, &dest.pkh, 50);

        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            source.clone(),
            100_u64.into(),
            Contract::Implicit(dest.pkh),
            Parameters::default(),
        );

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(source.pkh.clone())),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Failed(
                    vec![TransferError::BalanceTooLow(BalanceTooLow {
                        contract: Contract::Implicit(source.pkh),
                        balance: 35_u64.into(),
                        amount: 100_u64.into(),
                    })
                    .into()]
                    .into(),
                ),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(receipt, expected_receipt);

        // Verify that source only paid the fees and the destination balance is unchanged
        assert_eq!(source_account.balance(&host).unwrap(), 35.into());
        assert_eq!(destination_account.balance(&host).unwrap(), 50_u64.into());

        assert_eq!(
            source_account.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    // Bootstrap 1 successfully transfer 30 mutez to Bootstrap 2
    #[test]
    fn apply_successful_transfer() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        let dst = bootstrap2();

        // Setup accounts with 50 mutez in their balance and reveal the source
        let source = init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let destination = init_account(&mut host, &dst.pkh, 50);

        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dst.pkh.clone()),
            Parameters::default(),
        );

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
                        storage: None,
                        lazy_storage_diff: None,
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(src.pkh)),
                                changes: -30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(dst.pkh)),
                                changes: 30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_milligas: 2148527_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: false,
                        address_registry_diff: vec![],
                    },
                )),
                internal_operation_results: vec![],
            }),
        }];
        assert_eq!(receipt, expected_receipt);

        // Verify that source and destination balances changed
        assert_eq!(source.balance(&host).unwrap(), 5_u64.into());
        assert_eq!(destination.balance(&host).unwrap(), 80_u64.into());

        // Verify that the source's counter has been incremented
        assert_eq!(
            source.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    // Bootstrap 1 successfully transfers 30 mutez to itself
    #[test]
    fn apply_successful_self_transfer() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        let dest = src.clone();

        // Setup account with 50 mutez in its balance
        let source = init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let operation = make_transfer_operation(
            15,
            1,
            21000,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh.clone()),
            Parameters::default(),
        );

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
                        storage: None,
                        lazy_storage_diff: None,
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(src.pkh)),
                                changes: -30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(dest.pkh)),
                                changes: 30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_milligas: 2148527_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: false,
                        address_registry_diff: vec![],
                    },
                )),
                internal_operation_results: vec![],
            }),
        }];

        // Verify that balance was only debited for fees
        assert_eq!(source.balance(&host).unwrap(), 35_u64.into());

        assert_eq!(receipt, expected_receipt);

        // Verify that the source's counter has been incremented
        assert_eq!(
            source.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    #[test]
    fn apply_transfer_to_originated_faucet_with_success_receipt() {
        let mut host = MockKernelHost::default();
        let (requester_balance, faucet_balance, fees) = (50, 1000, 15);
        let src = bootstrap1();
        let desthash =
            ContractKt1Hash::from_base58_check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton")
                .expect("ContractKt1Hash b58 conversion should have succeeded");
        // Setup accounts with 50 mutez in their balance
        let requester = init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);
        let (code, storage) = (
            r#"
                        parameter (mutez %fund);
                        storage unit;
                        code
                        {
                            UNPAIR;
                            SENDER;
                            CONTRACT unit;
                            IF_NONE { FAILWITH } {};
                            SWAP;
                            UNIT;
                            TRANSFER_TOKENS;
                            NIL operation;
                            SWAP;
                            CONS;
                            PAIR
                        }
            "#,
            &Micheline::from(()),
        );
        let faucet = init_contract(&mut host, &desthash, code, storage, &1000.into());
        let requested_amount = 100;
        let operation = make_transfer_operation(
            fees,
            1,
            22080,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(desthash.clone()),
            Parameters {
                entrypoint: Entrypoint::try_from("fund")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(requested_amount as i128)
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );
        let res = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect("validate_and_apply_operation should not have failed with a kernel error")
        .remove(0)
        .operation_with_metadata;
        assert_eq!(
            res,
            OperationWithMetadata {
                content: operation.content[0].clone(),
                receipt: OperationResultSum::Transfer(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: 0 - fees as i64,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: fees as i64,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess {
                            storage: Some(
                                storage
                                    .encode(&mut Gas::default())
                                    .unwrap()
                                    .unwrap()
                                    .into()
                            ),
                            lazy_storage_diff: None,
                            balance_updates: vec![],
                            ticket_receipt: vec![],
                            originated_contracts: vec![],
                            consumed_milligas: 1158974_u64.into(),
                            storage_size: 69_u64.into(), // code (67) + unit (2)
                            paid_storage_size_diff: 0_u64.into(), // unit unchanged
                            allocated_destination_contract: false,
                            address_registry_diff: vec![],
                        }
                    )),
                    internal_operation_results: vec![InternalOperationSum::Transfer(
                        InternalContentWithMetadata {
                            content: TransferContent {
                                amount: requested_amount.into(),
                                destination: Contract::Implicit(src.pkh.clone()),
                                parameters: Parameters {
                                    entrypoint: Entrypoint::default(),
                                    value: Micheline::from(())
                                        .encode(&mut Gas::default())
                                        .unwrap()
                                        .unwrap(),
                                },
                            },
                            sender: Contract::Originated(desthash.clone()),
                            nonce: 0,
                            result: ContentResult::Applied(TransferTarget::ToContrat(
                                TransferSuccess {
                                    storage: None,
                                    lazy_storage_diff: None,
                                    balance_updates: vec![
                                        BalanceUpdate {
                                            balance: Balance::Account(
                                                Contract::Originated(desthash.clone())
                                            ),
                                            changes: 0 - (requested_amount as i64),
                                            update_origin: UpdateOrigin::BlockApplication,
                                        },
                                        BalanceUpdate {
                                            balance: Balance::Account(
                                                Contract::Implicit(src.pkh.clone())
                                            ),
                                            changes: requested_amount as i64,
                                            update_origin: UpdateOrigin::BlockApplication,
                                        },
                                    ],
                                    ticket_receipt: vec![],
                                    originated_contracts: vec![],
                                    consumed_milligas: 2_100_250_u64.into(),
                                    storage_size: 0_u64.into(),
                                    paid_storage_size_diff: 0_u64.into(),
                                    allocated_destination_contract: false,
                                    address_registry_diff: vec![],
                                }
                            )),
                        }
                    )],
                })
            }
        );
        assert_eq!(
            faucet.balance(&host).unwrap(),
            (faucet_balance - requested_amount).into()
        );
        assert_eq!(
            requester.balance(&host).unwrap(),
            (requester_balance + requested_amount - fees).into()
        ); // The faucet should have transferred 100 mutez to the source

        assert_eq!(
            requester.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    /// Pin the existence-check guard introduced before `transfer_tez` in
    /// the `Contract::Originated` branch of `transfer`. A Transaction to a
    /// never-originated KT1 must produce a typed user-level
    /// `ContractDoesNotExist` failure, not surface as
    /// `FailedToFetchContractCode` (which is now routed to BlockAbort and
    /// would discard the whole block).
    ///
    /// Regression for L2-1290: without this guard, a single transfer to
    /// any unknown KT1 would be a user-controllable block-abort handle.
    #[test]
    fn apply_transfer_to_nonexistent_originated_contract() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        // Never-originated KT1: no `init_contract` call, no code blob, no
        // balance entry exists at this path.
        let desthash =
            ContractKt1Hash::from_base58_check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton")
                .expect("ContractKt1Hash b58 conversion should have succeeded");

        let requester = init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(desthash.clone()),
            Parameters::default(),
        );

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipts = ProcessedOperation::into_receipts(processed);

        assert_eq!(receipts.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Failed(ApplyOperationErrors { errors }),
                    ..
                }) if errors.len() == 1 && matches!(
                    &errors[0],
                    ApplyOperationError::Transfer(
                        TransferError::ContractDoesNotExist(Contract::Originated(kt1))
                    ) if kt1 == &desthash
                )
            ),
            "Expected Failed Transfer with ContractDoesNotExist({desthash}), got {:?}",
            receipts[0].receipt
        );

        // No balance entry should have been written under the bogus
        // contract path. The guard runs before `transfer_tez`, so the
        // never-originated KT1 must remain absent from durable storage.
        let dest_account = context::originated_from_kt1(&desthash)
            .expect("originated_from_kt1 should have succeeded");
        let balance_path = context::account::balance_path(&dest_account)
            .expect("balance_path should have succeeded");
        assert_eq!(
            host.store_has(&balance_path).unwrap(),
            None,
            "no balance entry should exist for a never-originated KT1"
        );

        // Source paid the manager fee but not the transfer amount.
        assert_eq!(requester.balance(&host).unwrap(), (50_u64 - 15).into());
        assert_eq!(
            requester.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    #[test]
    fn apply_transfer_with_execution() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        let dest = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");

        let initial_storage = Micheline::from("initial");

        // Source funded with 54 mutez: 15 fee + 30 transfer + 4
        // storage burn (4 × COST_PER_BYTES) + 5 remaining balance.
        let source = init_account(&mut host, &src.pkh, 54);
        reveal_account(&mut host, &src);
        let destination =
            init_contract(&mut host, &dest, SCRIPT, &initial_storage, &50_u64.into());

        let storage_value = Micheline::from("Hello world")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(dest),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: storage_value.clone(),
            },
        );

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let storage = Some(storage_value.clone());

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
                        storage: storage.map(Into::into),
                        lazy_storage_diff: None,
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -(4 * COST_PER_BYTES as i64),
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::StorageFees,
                                changes: (4 * COST_PER_BYTES as i64),
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            // Transfer payload (sender → destination).
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(
                                    Contract::from_b58check(CONTRACT_1).unwrap(),
                                ),
                                changes: 30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_milligas: 1153093_u64.into(),
                        storage_size: 44_u64.into(), // code (33) + "Hello world" (11)
                        paid_storage_size_diff: 4_u64.into(), // "Hello world" (11) − "initial" (7)
                        allocated_destination_contract: false,
                        address_registry_diff: vec![],
                    },
                )),
                internal_operation_results: vec![],
            }),
        }];

        // Verify that source and destination balances changed:
        // 54 - 15 fee - 30 transfer - 4 burn (4 × COST_PER_BYTES) = 5.
        assert_eq!(source.balance(&host).unwrap(), 5_u64.into());
        assert_eq!(destination.balance(&host).unwrap(), 80_u64.into());

        assert_eq!(receipt, expected_receipt);

        assert_eq!(
            source.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );

        assert_eq!(
            destination.storage(&host).unwrap(),
            storage_value,
            "Storage has not been updated"
        )
    }

    /// After a transfer that grew the destination's storage,
    /// `paid_bytes` matches `used_bytes` and both equal the post-op
    /// `code_size + storage_size`. The watermark moved forward inside
    /// `set_storage` during execution.
    #[test]
    fn apply_transfer_bumps_paid_bytes_after_storage_growth() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();
        let dest = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");

        let initial_storage = Micheline::from("initial");
        // Source funded with 1050 mutez: 15 fee + 30 transfer + 1000
        // storage burn (4 × COST_PER_BYTES) + 5 remaining balance.
        init_account(&mut host, &src.pkh, 1050);
        reveal_account(&mut host, &src);
        let destination =
            init_contract(&mut host, &dest, SCRIPT, &initial_storage, &50_u64.into());

        let storage_value = Micheline::from("Hello world")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let new_storage_size = storage_value.len() as u64;
        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(dest),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: storage_value,
            },
        );

        let _ = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        // SCRIPT replaces storage by the parameter; post-op `used_bytes`
        // is `code_size + |encoded("Hello world")|`. Read `code_size`
        // from durable storage — the invariant under test is exactly
        // `used == code_size + storage_size`.
        let code_size = destination.code_size(&host).unwrap();
        let expected_used = Zarith(code_size.0 + new_storage_size);

        assert_eq!(destination.used_bytes(&host).unwrap(), expected_used);
        assert_eq!(destination.paid_bytes(&host).unwrap(), expected_used);
    }

    #[test]
    fn apply_transfer_burn_failure_backtracks_with_error() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        let dest = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");

        let initial_storage = Micheline::from("initial");
        // Fund the source with 48 mutez: 15 fee + 30 transfer + 3
        // left.  The storage burn (4 × COST_PER_BYTES = 4 mutez)
        // does not fit; the operation must Backtrack.
        let source = init_account(&mut host, &src.pkh, 48);
        reveal_account(&mut host, &src);
        let destination = init_contract(
            &mut host,
            &dest,
            SCRIPT,
            &initial_storage.clone(),
            &50_u64.into(),
        );

        let storage_value = Micheline::from("Hello world")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(dest),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: storage_value.clone(),
            },
        );

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect("kernel error not expected — burn failure routes through Backtracked");
        let receipts = ProcessedOperation::into_receipts(processed);

        let expected_receipts = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::BackTracked(BacktrackedResult {
                    errors: Some(ApplyOperationErrors {
                        errors: vec![ApplyOperationError::CannotPayStorageFee(
                            BalanceTooLow {
                                contract: source.contract(),
                                balance: 3_u64.into(), // 48 - 15 fee - 30 transfer
                                amount: (4 * COST_PER_BYTES).into(), // 4
                            },
                        )],
                    }),
                    result: TransferTarget::ToContrat(TransferSuccess {
                        storage: Some(storage_value.into()),
                        lazy_storage_diff: None,
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(
                                    Contract::from_b58check(CONTRACT_1).unwrap(),
                                ),
                                changes: 30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_milligas: 1153093_u64.into(),
                        storage_size: 44_u64.into(), // code (33) + "Hello world" (11)
                        paid_storage_size_diff: 4_u64.into(), // "Hello world" (11) − "initial" (7)
                        allocated_destination_contract: false,
                        address_registry_diff: vec![],
                    }),
                }),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(receipts, expected_receipts);

        // SafeStorage rollback: source paid the fee but neither the
        // transfer amount nor the burn. Destination storage untouched.
        assert_eq!(source.balance(&host).unwrap(), (48_u64 - 15).into());
        assert_eq!(
            destination.storage(&host).unwrap(),
            initial_storage
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
            "destination storage must roll back to pre-op value",
        );
    }

    #[test]
    fn apply_transfer_to_unallocated_implicit_burns_slot() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        let dest = bootstrap2();
        // Source funded for: 15 fee + 30 transfer + 257 slot burn
        // (ORIGINATION_SIZE × COST_PER_BYTES) + 5 mutez change.
        let source = init_account(&mut host, &src.pkh, 307);
        reveal_account(&mut host, &src);
        // Note: dest is NOT pre-allocated (no init_account call).

        let operation = make_transfer_operation(
            15,
            1,
            21010,
            300,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh.clone()),
            Parameters::default(),
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation.clone(),
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect("validate_and_apply_operation should not return a kernel error"),
        );

        let expected_receipts = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
                        storage: None,
                        lazy_storage_diff: None,
                        balance_updates: vec![
                            // Transfer pair (source → dest implicit)
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    dest.pkh.clone(),
                                )),
                                changes: 30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            // Slot burn pair (ORIGINATION_SIZE × COST_PER_BYTES = 257)
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -257,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::StorageFees,
                                changes: 257,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_milligas: 2_148_528_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: true,
                        address_registry_diff: vec![],
                    },
                )),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(receipts, expected_receipts);

        let dest_account =
            context::implicit_from_public_key_hash(&dest.pkh).expect("dest account");
        assert_eq!(dest_account.balance(&host).unwrap(), 30_u64.into());
        // Source: 307 - 15 fee - 30 transfer - 257 slot burn = 5.
        assert_eq!(source.balance(&host).unwrap(), 5_u64.into());
    }

    #[test]
    fn apply_transfer_to_unallocated_implicit_cannot_pay_slot() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        let dest = bootstrap2();
        // 50 mutez covers fee (15) + transfer (30) but NOT the slot
        // burn (257).
        let source = init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let operation = make_transfer_operation(
            15,
            1,
            21010,
            300,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh.clone()),
            Parameters::default(),
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation.clone(),
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect("validate_and_apply_operation should not return a kernel error"),
        );

        let expected_receipts = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::BackTracked(BacktrackedResult {
                    errors: Some(ApplyOperationErrors {
                        errors: vec![ApplyOperationError::CannotPayStorageFee(
                            BalanceTooLow {
                                contract: Contract::Implicit(src.pkh.clone()),
                                // 50 - 15 fee - 30 transfer = 5
                                balance: 5_u64.into(),
                                // ORIGINATION_SIZE × COST_PER_BYTES
                                amount: 257_u64.into(),
                            },
                        )],
                    }),
                    result: TransferTarget::ToContrat(TransferSuccess {
                        storage: None,
                        lazy_storage_diff: None,
                        balance_updates: vec![
                            // Transfer pair (source → fresh implicit dest).
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    dest.pkh.clone(),
                                )),
                                changes: 30,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_milligas: 2_148_528_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: true,
                        address_registry_diff: vec![],
                    }),
                }),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(receipts, expected_receipts);

        // SafeStorage rolled back the allocation: the destination
        // account is gone again, and the source paid only the fee.
        assert_eq!(source.balance(&host).unwrap(), (50_u64 - 15).into());
        let dest_account =
            context::implicit_from_public_key_hash(&dest.pkh).expect("dest account");
        assert!(
            !dest_account.allocated(&host).unwrap(),
            "implicit allocation must be rolled back"
        );
    }

    #[test]
    fn apply_transfer_with_failed_execution() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        let dest = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");

        let initial_storage = Micheline::from(());

        let source = init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let destination = init_contract(
            &mut host,
            &dest,
            FAILING_SCRIPT,
            &initial_storage,
            &50_u64.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            1040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(dest),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src.pkh)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result:  ContentResult::Failed(vec![
                ApplyOperationError::Transfer(
            TransferError::MichelsonContractInterpretError(
                "runtime failure while running the script: failed with: String(\"This contract always fails\") of type String".into()
            )
        )].into()),
            internal_operation_results: vec![],
            })}];

        // Verify that source and destination balances changed
        // Transfer should be free as it got reverted + 15 for fees, 5 should be left
        assert_eq!(source.balance(&host).unwrap(), 35_u64.into());
        assert_eq!(destination.balance(&host).unwrap(), 50_u64.into());

        assert_eq!(receipt, expected_receipt);

        assert_eq!(
            source.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );

        assert_eq!(
            destination.storage(&host).unwrap(),
            initial_storage
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
            "Storage should not have been updated"
        )
    }

    #[test]
    fn apply_transfer_with_argument_to_implicit_fails() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        let dest = bootstrap2();

        init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(0)
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh)),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Failed(
                    vec![ApplyOperationError::Transfer(
                        TransferError::NonSmartContractExecutionCall,
                    )]
                    .into(),
                ),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(receipt, expected_receipt);
    }

    #[test]
    fn apply_transfer_with_non_default_entrypoint_to_implicit_fails() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        let dest = bootstrap2();

        init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let operation = make_transfer_operation(
            15,
            1,
            21000,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh),
            Parameters {
                entrypoint: mir::ast::Entrypoint::try_from("non_default")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh)),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Failed(
                    vec![ApplyOperationError::Transfer(
                        TransferError::NonSmartContractExecutionCall,
                    )]
                    .into(),
                ),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(receipt, expected_receipt);
    }

    #[test]
    fn apply_three_valid_operations() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();
        let dest = bootstrap2();

        // src & dest each credited with 50ꜩ
        let src_acc = init_account(&mut host, &src.pkh, 50);
        let dest_acc = init_account(&mut host, &dest.pkh, 50);

        // op‑1: reveal
        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: src.pk.clone(),
            proof: None,
        });

        println!("Balance: {:?}", src_acc.balance(&host).unwrap());

        // op‑2: transfer 10ꜩ to dest
        let transfer_content_1 = OperationContent::Transfer(TransferContent {
            amount: 10.into(),
            destination: Contract::Implicit(dest.pkh.clone()),
            parameters: Parameters::default(),
        });

        // op‑3: transfer 20ꜩ to dest
        let transfer_content_2 = OperationContent::Transfer(TransferContent {
            amount: 20.into(),
            destination: Contract::Implicit(dest.pkh.clone()),
            parameters: Parameters::default(),
        });

        let batch = make_operation(
            5,
            1,
            21000,
            0,
            src.clone(),
            vec![reveal_content, transfer_content_1, transfer_content_2],
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                batch.clone(),
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .unwrap(),
        );

        let expected_receipts = zip_operations(
            batch,
            vec![
                OperationResultSum::Reveal(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: 5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::Applied(RevealSuccess {
                        consumed_milligas: 148612_u64.into(),
                    }),
                    internal_operation_results: vec![],
                }),
                OperationResultSum::Transfer(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: 5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess {
                            storage: None,
                            lazy_storage_diff: None,
                            balance_updates: vec![
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        src.pkh.clone(),
                                    )),
                                    changes: -10,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        dest.pkh.clone(),
                                    )),
                                    changes: 10,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                            ],
                            ticket_receipt: vec![],
                            originated_contracts: vec![],
                            consumed_milligas: 2100040_u64.into(),
                            storage_size: 0_u64.into(),
                            paid_storage_size_diff: 0_u64.into(),
                            allocated_destination_contract: false,
                            address_registry_diff: vec![],
                        },
                    )),
                    internal_operation_results: vec![],
                }),
                OperationResultSum::Transfer(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: 5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess {
                            storage: None,
                            lazy_storage_diff: None,
                            balance_updates: vec![
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        src.pkh,
                                    )),
                                    changes: -20,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        dest.pkh,
                                    )),
                                    changes: 20,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                            ],
                            ticket_receipt: vec![],
                            originated_contracts: vec![],
                            consumed_milligas: 2100040_u64.into(),
                            storage_size: 0_u64.into(),
                            paid_storage_size_diff: 0_u64.into(),
                            allocated_destination_contract: false,
                            address_registry_diff: vec![],
                        },
                    )),
                    internal_operation_results: vec![],
                }),
            ],
        );

        assert_eq!(receipts, expected_receipts);

        // counter updated, balances moved
        // initial_balance: 50 tez, fee amount: (3*5)tez, transfer amount: (10 + 20)tez
        assert_eq!(
            src_acc.balance(&host).unwrap(),
            5u64.into(),
            "Source account should have 5ꜩ left after fees and transfers."
        );
        assert_eq!(
            dest_acc.balance(&host).unwrap(),
            80u64.into(),
            "Destination account should have 80ꜩ after transfers."
        );

        assert_eq!(
            src_acc.counter(&host).unwrap(),
            3.into(),
            "Counter should have been incremented three times."
        );
    }

    #[test]
    fn apply_valid_then_invalid_operation_is_atomic() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();
        let dest = bootstrap2();

        // src & dest each credited with 50ꜩ
        let src_acc = init_account(&mut host, &src.pkh, 50);
        let _dst_acc = init_account(&mut host, &dest.pkh, 50);

        // op‑1: reveal
        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: src.pk.clone(),
            proof: None,
        });

        // op‑2: transfer 10ꜩ to dest
        let transfer_content = OperationContent::Transfer(TransferContent {
            amount: 10.into(),
            destination: Contract::Implicit(dest.pkh.clone()),
            parameters: Parameters::default(),
        });

        let batch = make_operation(
            100,
            1,
            22000,
            0,
            src.clone(),
            vec![reveal_content, transfer_content],
        );

        let receipts = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            batch,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        );

        let expected_error =
            OperationError::Validation(ValidityError::CantPayFees(100_u64.into()));

        assert_eq!(receipts, Err(expected_error));

        assert_eq!(
            context::implicit_from_public_key_hash(&src.pkh)
                .unwrap()
                .balance(&host)
                .unwrap(),
            50u64.into()
        );

        assert_eq!(
            context::implicit_from_public_key_hash(&src.pkh)
                .unwrap()
                .manager(&host)
                .unwrap(),
            Manager::NotRevealed(src.pkh.clone())
        );

        assert_eq!(
            src_acc.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented."
        );
    }

    #[test]
    fn apply_smart_contract_failure_reverts_batch() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();
        // Funded for: 3 fees of 10 (= 30) + 2 transfer amounts of 1
        // (= 2) + storage burn for the second op (4 × COST_PER_BYTES = 1000) +
        // 18 mutez change. The batch reverts via FAILWITH; only the
        // fees survive, so final balance is 1050 - 30 = 1020.
        let src_acc = init_account(&mut host, &src.pkh, 1050);

        let fail_dest = ContractKt1Hash::from_base58_check(CONTRACT_1).unwrap();
        let succ_dest = ContractKt1Hash::from_base58_check(CONTRACT_2).unwrap();

        init_contract(
            &mut host,
            &fail_dest,
            FAILING_SCRIPT,
            &Micheline::from(()),
            &0_u64.into(),
        );
        let succ_account = init_contract(
            &mut host,
            &succ_dest,
            SCRIPT,
            &Micheline::from("initial"),
            &0_u64.into(),
        );

        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: src.pk.clone(),
            proof: None,
        });

        let succ_transfer = OperationContent::Transfer(TransferContent {
            amount: 1.into(),
            destination: Contract::Originated(succ_dest.clone()),
            parameters: Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from("Hello world")
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        });

        let fail_transfer = OperationContent::Transfer(TransferContent {
            amount: 1.into(),
            destination: Contract::Originated(fail_dest.clone()),
            parameters: Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        });

        let batch = make_operation(
            10_u64,
            1,
            21010,
            100,
            src.clone(),
            vec![reveal_content, succ_transfer, fail_transfer],
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                batch,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .unwrap(),
        );

        println!("{receipts:?}");

        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Reveal(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "First receipt should be BackTracked Reveal"
        );

        assert!(
            matches!(
                &receipts[1].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "Second receipt should be BackTracked Transfer"
        );

        assert!(
            matches!(
                &receipts[2].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Failed(_),
                    ..
                })
            ),
            "Third receipt should be Failed Transfer"
        );

        // Storage must have reverted
        assert!(
            succ_account.storage(&host).unwrap()
                == Micheline::from("initial")
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
        );

        assert_eq!(
            src_acc.counter(&host).unwrap(),
            3.into(),
            "Counter should have been incremented three times."
        );

        // Initial balance: 1050 tez, paid in fees: (3*10) tez,
        // transfer + storage burn reverted via SafeStorage.
        assert_eq!(
            src_acc.balance(&host).unwrap(),
            1020.into(),
            "Fees should have been paid for failed operation"
        );
    }

    #[test]
    fn origination_of_a_smart_contract() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 1000000_u64);
        reveal_account(&mut host, &src);

        let src_account = context::implicit_from_public_key_hash(&src.pkh)
            .expect("Should have succeeded to create an account");

        // Retrieve initial balance for the end of the test
        let initial_balance = src_account
            .balance(&host)
            .expect("Should have found a balance");

        let fee = 15u64;
        let smart_contract_balance = 30u64;
        /*
        octez-client convert script "
                  parameter string;
                  storage string;
                  code { CAR; NIL operation; PAIR }
                " from Michelson to binary
         */
        let code =
            hex::decode("02000000170500036805010368050202000000080316053d036d0342")
                .unwrap();

        // octez-client -E https://rpc.tzkt.io/mainnet convert data  '"hello"' from Michelson to binary
        let storage = hex::decode("010000000568656c6c6f").unwrap();
        let operation = make_origination_operation(
            fee,
            1,
            5000,
            500,
            src.clone(),
            smart_contract_balance,
            Script {
                code: code.clone(),
                storage: storage.clone(),
            },
        );

        let origination_storage_fee: u64 =
            ((code.len() as u64) + (storage.len() as u64)) * COST_PER_BYTES;

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation.clone(),
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        let mut origination_nonce = OriginationNonce::default();
        let expected_kt1 = origination_nonce.generate_kt1();

        let expected_receipt = OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Origination(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 15,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(OriginationSuccess {
                    balance_updates: vec![
                        // Variable burn pair (storage_bus): 38 × COST_PER_BYTES.
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -38,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 38,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        // Slot burn pair (origination_bus): ORIGINATION_SIZE × COST_PER_BYTES.
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -257,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 257,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        // Prior: initial-balance transfer (sender → contract).
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -30,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Originated(
                                expected_kt1.clone(),
                            )),
                            changes: 30,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    originated_contracts: vec![Originated {
                        contract: expected_kt1.clone(),
                    }],
                    consumed_milligas: 2_352_383_u64.into(),
                    storage_size: 38u64.into(),
                    paid_storage_size_diff: 38u64.into(),
                    lazy_storage_diff: None,
                }),
                internal_operation_results: vec![],
            }),
        };

        assert_eq!(receipt, vec![expected_receipt]);

        // Now check that everything in the durable storage is updated

        // Balance of the source
        let current_balance = src_account
            .balance(&host)
            .expect("Should have found a balance for the source");

        let expected_balance = initial_balance
            .0
            .checked_sub(&fee.into())
            .expect("Should have been able to debit the fees")
            .checked_sub(&smart_contract_balance.into())
            .expect("Should have been able to debit the smart contract balance")
            .checked_sub(&(ORIGINATION_SIZE * COST_PER_BYTES).into())
            .expect("Should have been able to debit the origination cost")
            .checked_sub(&origination_storage_fee.into())
            .expect("Should have been able to debit the storage fees");

        assert_eq!(
            current_balance.0, expected_balance,
            "Source current balance doesn't match the expected one"
        );

        let smart_contract_account =
            context::originated_from_contract(&Contract::Originated(expected_kt1))
                .expect("Should have been able to create an account from the KT1");

        // Balance of the smart contract
        let current_kt1_balance = smart_contract_account
            .balance(&host)
            .expect("Should have found a balance for the smart contract");

        assert_eq!(
            current_kt1_balance,
            smart_contract_balance.into(),
            "Smart contract current balance doesn't match the expected one"
        );

        // Verify code and storage
        let smart_contract_code = smart_contract_account
            .code(&host)
            .expect("Should have found a code for the KT1");
        assert_eq!(
            smart_contract_code,
            Code::Code(code),
            "Current code for smart contract is not the same as the one originated"
        );
        let smart_contract_storage = smart_contract_account
            .storage(&host)
            .expect("Should have found a code for the KT1");
        assert_eq!(
            smart_contract_storage, storage,
            "Current storage for smart contract is not the same as the one originated"
        );
    }

    #[test]
    fn test_internal_receipts_failure_backtrack_all() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();

        // Initialize accounts with higher balances for the test
        init_account(&mut host, &src.pkh, 100);

        // Create a script that emits internal operations to multiple targets
        let contract_chapo_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &contract_chapo_hash,
            SCRIPT_EMITING_INTERNAL_TRANSFER,
            &Micheline::from(()),
            &100_u64.into(),
        );

        // Create a failing contract
        let internal_fail_contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_2)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &internal_fail_contract_hash,
            FAILING_SCRIPT,
            &Micheline::from(()),
            &100_u64.into(),
        );

        // Create a successful contract
        let internal_success_contract_hash =
            ContractKt1Hash::from_base58_check(CONTRACT_3)
                .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &internal_success_contract_hash,
            UNIT_SCRIPT,
            &Micheline::from(()),
            &100_u64.into(),
        );

        let success_micheline_address = Micheline::Bytes(
            Contract::Originated(internal_success_contract_hash.clone())
                .to_bytes()
                .unwrap(),
        );
        let fail_micheline_address = Micheline::Bytes(
            Contract::Originated(internal_fail_contract_hash.clone())
                .to_bytes()
                .unwrap(),
        );
        let addrs: Vec<Micheline> = vec![
            success_micheline_address.clone(),
            fail_micheline_address,
            success_micheline_address,
        ];
        let param_value = Micheline::Seq(&addrs);
        let operation = make_operation(
            10,
            1,
            21150,
            0,
            src.clone(),
            vec![
                OperationContent::Reveal(RevealContent {
                    pk: src.pk.clone(),
                    proof: None,
                }),
                OperationContent::Transfer(TransferContent {
                    amount: 0.into(),
                    destination: Contract::Originated(contract_chapo_hash),
                    parameters: Parameters {
                        entrypoint: mir::ast::Entrypoint::default(),
                        value: param_value.encode(&mut Gas::default()).unwrap().unwrap(),
                    },
                }),
            ],
        );
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );
        assert_eq!(
            receipts.len(),
            2,
            "There should be two receipts: one for reveal and one for transfer"
        );
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Reveal(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "First receipt should be a BackTracked Reveal but is {:?}",
            receipts[0].receipt
        );
        assert!(
            matches!(
                &receipts[1].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "Second receipt should be a BackTracked Transfer but is {:?}",
            receipts[1].receipt
        );

        // Check Internal Operations
        if let OperationResultSum::Transfer(OperationResult {
            internal_operation_results,
            ..
        }) = &receipts[1].receipt
        {
            assert_eq!(
                internal_operation_results.len(),
                3,
                "There should be three internal operations"
            );

            // Check the first internal operation
            if let InternalOperationSum::Transfer(InternalContentWithMetadata {
                content: TransferContent { destination, .. },
                result:
                    ContentResult::BackTracked(BacktrackedResult {
                        errors: None,
                        result:
                            TransferTarget::ToContrat(TransferSuccess {
                                balance_updates, ..
                            }),
                    }),
                ..
            }) = &internal_operation_results[0]
            {
                assert_eq!(
                    destination,
                    &Contract::Originated(internal_success_contract_hash.clone()),
                    "First internal operation should target the successful contract"
                );
                assert_eq!(
                    balance_updates.len(),
                    2,
                    "Balance updates should have two entries"
                );
            } else {
                panic!(
                    "First internal operation is not a transfer its {:?}",
                    internal_operation_results[0]
                );
            }

            // Check the second internal operation
            if let InternalOperationSum::Transfer(InternalContentWithMetadata {
                content: TransferContent { destination, .. },
                result: ContentResult::Failed(_),
                ..
            }) = &internal_operation_results[1]
            {
                assert_eq!(
                    destination,
                    &Contract::Originated(internal_fail_contract_hash),
                    "Second internal operation should target the failing contract"
                );
            } else {
                panic!("Second internal operation is not a transfer or does not match expected structure");
            }

            // Check the third internal operation
            if let InternalOperationSum::Transfer(InternalContentWithMetadata {
                content: TransferContent { destination, .. },
                result: ContentResult::Skipped,
                ..
            }) = &internal_operation_results[2]
            {
                assert_eq!(
                    destination,
                    &Contract::Originated(internal_success_contract_hash),
                    "Third internal operation should target the successful contract"
                );
            } else {
                panic!("Third internal operation is not a transfer or does not match expected structure");
            }
        } else {
            panic!("Second receipt is not a Transfer with Internal Operations");
        }
    }

    /// L2-1637: a contract `DUP`s an `operation` value and emits both
    /// copies. `operation` is `Duplicable`, so this typechecks, but the
    /// two copies share one MIR counter — the identity L1 uses to
    /// enforce internal-operation linearity. The kernel must reject the
    /// second copy as an internal-operation replay instead of applying
    /// it a second time under a fresh display nonce; otherwise the
    /// carried side effect (here a plain transfer, but equally a
    /// ticket-carrying transfer) would run twice while L1 backtracks.
    #[test]
    fn test_internal_transfer_replay_is_rejected() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 100000);
        reveal_account(&mut host, &src);

        // Parent: build one internal transfer, DUP the resulting
        // `operation`, and emit both copies in the returned list.
        static DUP_TRANSFER_SCRIPT: &str = r#"
            parameter address;
            storage unit;
            code {
                CAR;
                CONTRACT unit;
                IF_NONE { PUSH string "Invalid contract address"; FAILWITH } {};
                PUSH mutez 0;
                PUSH unit Unit;
                TRANSFER_TOKENS;
                DUP;
                NIL operation;
                SWAP; CONS;
                SWAP; CONS;
                PUSH unit Unit;
                SWAP;
                PAIR
            }"#;

        let parent_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &parent_hash,
            DUP_TRANSFER_SCRIPT,
            &Micheline::from(()),
            &100_u64.into(),
        );

        let receiver_hash = ContractKt1Hash::from_base58_check(CONTRACT_2)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &receiver_hash,
            UNIT_SCRIPT,
            &Micheline::from(()),
            &100_u64.into(),
        );

        let receiver_address =
            Micheline::Bytes(Contract::Originated(receiver_hash).to_bytes().unwrap());

        let operation = make_transfer_operation(
            10,
            1,
            30000,
            0,
            src.clone(),
            0.into(),
            Contract::Originated(parent_hash),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: receiver_address
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts.len(), 1, "There should be one transfer receipt");
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "The duplicated internal operation must make the whole operation backtrack, but receipt is {:?}",
            receipts[0].receipt
        );

        let internal_receipts = get_internal_receipts(&receipts[0].receipt);
        assert_eq!(
            internal_receipts.len(),
            2,
            "Both copies of the duplicated operation must appear in the receipt"
        );
        // The first copy applied, then was demoted to BackTracked when
        // the operation failed on the replay.
        assert!(
            matches!(
                &internal_receipts[0],
                InternalOperationSum::Transfer(InternalContentWithMetadata {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "First copy should be BackTracked but is {:?}",
            internal_receipts[0]
        );
        // The second copy is rejected as an internal-operation replay.
        match &internal_receipts[1] {
            InternalOperationSum::Transfer(InternalContentWithMetadata {
                result: ContentResult::Failed(errors),
                ..
            }) => assert_eq!(
                errors.errors,
                vec![ApplyOperationError::InternalOperationReplay],
                "Second copy must be rejected as an internal-operation replay"
            ),
            other => {
                panic!("Second copy should be a Failed transfer but is {other:?}")
            }
        }
    }

    /// L2-1637, origination variant: a contract `DUP`s an internal
    /// `CREATE_CONTRACT` `operation` and emits both copies. Both carry
    /// the same MIR counter (and the same pre-computed KT1), so the
    /// second must be rejected as a replay rather than originating the
    /// same contract twice.
    #[test]
    fn test_internal_origination_replay_is_rejected() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 100000);
        reveal_account(&mut host, &src);

        // Parent: one internal origination, DUP the `operation`, emit
        // both copies; store the (single) child address.
        static DUP_ORIGINATION_SCRIPT: &str = r#"
            parameter unit;
            storage (option address);
            code {
                DROP;
                UNIT;
                PUSH mutez 0;
                NONE key_hash;
                CREATE_CONTRACT { parameter unit; storage unit; code { CDR; NIL operation; PAIR } };
                DIP { SOME };
                DUP;
                NIL operation;
                SWAP; CONS;
                SWAP; CONS;
                PAIR
            }"#;

        let parent_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &parent_hash,
            DUP_ORIGINATION_SCRIPT,
            &Micheline::prim0(mir::lexer::Prim::None, &mut Gas::default()).unwrap(),
            &1000000_u64.into(),
        );

        let operation = make_transfer_operation(
            10,
            1,
            50000,
            10000,
            src.clone(),
            0.into(),
            Contract::Originated(parent_hash),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts.len(), 1, "There should be one transfer receipt");
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "The duplicated internal origination must make the whole operation backtrack, but receipt is {:?}",
            receipts[0].receipt
        );

        let internal_receipts = get_internal_receipts(&receipts[0].receipt);
        assert_eq!(
            internal_receipts.len(),
            2,
            "Both copies of the duplicated origination must appear in the receipt"
        );
        assert!(
            matches!(
                &internal_receipts[0],
                InternalOperationSum::Origination(InternalContentWithMetadata {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "First copy should be BackTracked but is {:?}",
            internal_receipts[0]
        );
        match &internal_receipts[1] {
            InternalOperationSum::Origination(InternalContentWithMetadata {
                result: ContentResult::Failed(errors),
                ..
            }) => assert_eq!(
                errors.errors,
                vec![ApplyOperationError::InternalOperationReplay],
                "Second copy must be rejected as an internal-operation replay"
            ),
            other => {
                panic!("Second copy should be a Failed origination but is {other:?}")
            }
        }
    }

    /// L2-1637, event variant: a contract `DUP`s an internal `EMIT`
    /// `operation` and returns both copies. They share one MIR counter,
    /// so the second must be rejected as a replay rather than emitting
    /// the same event twice — exercising the `Emit` arm's replay path.
    #[test]
    fn test_internal_emit_replay_is_rejected() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 100000);
        reveal_account(&mut host, &src);

        // Parent: build one internal EMIT operation, DUP the resulting
        // `operation`, and return both copies in the operation list.
        static DUP_EMIT_SCRIPT: &str = r#"
            parameter unit;
            storage unit;
            code {
                DROP;
                PUSH nat 42;
                EMIT %foo nat;
                DUP;
                NIL operation;
                SWAP; CONS;
                SWAP; CONS;
                PUSH unit Unit;
                SWAP;
                PAIR
            }"#;

        let parent_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &parent_hash,
            DUP_EMIT_SCRIPT,
            &Micheline::from(()),
            &100_u64.into(),
        );

        let operation = make_transfer_operation(
            10,
            1,
            30000,
            0,
            src.clone(),
            0.into(),
            Contract::Originated(parent_hash),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts.len(), 1, "There should be one transfer receipt");
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "The duplicated internal event must make the whole operation backtrack, but receipt is {:?}",
            receipts[0].receipt
        );

        let internal_receipts = get_internal_receipts(&receipts[0].receipt);
        assert_eq!(
            internal_receipts.len(),
            2,
            "Both copies of the duplicated event must appear in the receipt"
        );
        // The first copy applied, then was demoted to BackTracked when
        // the operation failed on the replay.
        assert!(
            matches!(
                &internal_receipts[0],
                InternalOperationSum::Event(InternalContentWithMetadata {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "First copy should be BackTracked but is {:?}",
            internal_receipts[0]
        );
        // The second copy is rejected as an internal-operation replay.
        match &internal_receipts[1] {
            InternalOperationSum::Event(InternalContentWithMetadata {
                result: ContentResult::Failed(errors),
                ..
            }) => assert_eq!(
                errors.errors,
                vec![ApplyOperationError::InternalOperationReplay],
                "Second copy must be rejected as an internal-operation replay"
            ),
            other => {
                panic!("Second copy should be a Failed event but is {other:?}")
            }
        }
    }

    #[test]
    fn test_smart_contract_amount_instruction() {
        // Write AMOUNT in the storage
        const SCRIPT: &str = "
            parameter unit;
            storage mutez;
            code {
                DROP;
                AMOUNT;
                NIL operation;
                PAIR
            }
        ";
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_3)
            .expect("ContractKt1Hash b58 conversion should have succeed");

        let initial_amount = 0;
        let transfer_amount = 30;
        let src_contract = init_contract(
            &mut host,
            &contract_hash,
            SCRIPT,
            &Micheline::from(initial_amount),
            &0_u64.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            src.clone(),
            transfer_amount.into(),
            Contract::Originated(contract_hash.clone()),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let _processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            operation.hash().unwrap(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(
            src_contract.storage(&host).unwrap(),
            Micheline::from(i128::from(transfer_amount))
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
            "Storage should contain the amount sent"
        );
    }

    #[test]
    fn test_smart_contract_balance_instruction() {
        // Write BALANCE in the storage
        const SCRIPT: &str = "
            parameter unit;
            storage mutez;
            code {
                DROP;
                BALANCE;
                NIL operation;
                PAIR
            }
        ";
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        // Fund source for fee + transfer + storage burn for
        // recording the contract balance into its storage.
        init_account(&mut host, &src.pkh, 5_000);
        reveal_account(&mut host, &src);

        let contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_3)
            .expect("ContractKt1Hash b58 conversion should have succeed");

        let transfer_amount = 30;
        let initial_balance = 200;
        let src_contract = init_contract(
            &mut host,
            &contract_hash,
            SCRIPT,
            &Micheline::from(0),
            &initial_balance.into(),
        );
        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            src.clone(),
            transfer_amount.into(),
            Contract::Originated(contract_hash.clone()),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let _processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            operation.hash().unwrap(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(
            src_contract.storage(&host).unwrap(),
            Micheline::from(i128::from(initial_balance + transfer_amount))
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
            "Storage should contain the balance of the contract"
        );
    }

    #[test]
    fn test_smart_contract_self_address_instruction() {
        // Write SELF_ADDRESS in the storage
        const SCRIPT_ADDR: &str = "
            parameter unit;
            storage address;
            code {
                DROP;
                SELF_ADDRESS;
                NIL operation;
                PAIR
            }
        ";
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_3)
            .expect("ContractKt1Hash b58 conversion should have succeed");

        let micheline_address = Micheline::Bytes(
            Contract::Originated(contract_hash.clone())
                .to_bytes()
                .unwrap(),
        );
        let src_contract = init_contract(
            &mut host,
            &contract_hash,
            SCRIPT_ADDR,
            &micheline_address,
            &0_u64.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(contract_hash.clone()),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let _processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            operation.hash().unwrap(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(
            src_contract.storage(&host).unwrap(),
            micheline_address
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
            "Storage should contain the self address of the contract"
        );
    }

    fn get_internal_receipts(op: &OperationResultSum) -> &Vec<InternalOperationSum> {
        if let OperationResultSum::Transfer(OperationResult {
            internal_operation_results,
            ..
        }) = op
        {
            internal_operation_results
        } else {
            panic!("Expected a Transfer operation result")
        }
    }

    #[test]
    fn test_apply_origination_slot_burn_failure_backtracks() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();

        // 283 = 15 fee + 30 initial balance + 38 variable burn
        // (code 28 bytes + storage 10 bytes = 38 bytes × COST_PER_BYTES)
        // + 200 leftover, insufficient for the 257 slot burn that follows.
        let funded = 283_u64;
        init_account(&mut host, &src.pkh, funded);
        reveal_account(&mut host, &src);

        let fee = 15u64;
        let smart_contract_balance = 30u64;
        /*
        octez-client convert script "
                  parameter string;
                  storage string;
                  code { CAR; NIL operation; PAIR }
                " from Michelson to binary
         */
        let code =
            hex::decode("02000000170500036805010368050202000000080316053d036d0342")
                .unwrap();

        // octez-client -E https://rpc.tzkt.io/mainnet convert data  '"hello"' from Michelson to binary
        let storage = hex::decode("010000000568656c6c6f").unwrap();
        let operation = make_origination_operation(
            fee,
            1,
            5000,
            500,
            src.clone(),
            smart_contract_balance,
            Script { code, storage },
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation.clone(),
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "kernel error not expected — burn failure routes through Backtracked",
            ),
        );

        let mut origination_nonce = OriginationNonce::default();
        let expected_kt1 = origination_nonce.generate_kt1();

        let expected_receipts = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Origination(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -(fee as i64),
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: fee as i64,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::BackTracked(BacktrackedResult {
                    errors: Some(ApplyOperationErrors {
                        errors: vec![ApplyOperationError::CannotPayStorageFee(
                            BalanceTooLow {
                                contract: Contract::Implicit(src.pkh.clone()),
                                // 283 - 15 fee - 30 transfer - 38 variable burn = 200
                                balance: 200_u64.into(),
                                // ORIGINATION_SIZE × COST_PER_BYTES
                                amount: 257_u64.into(),
                            },
                        )],
                    }),
                    result: OriginationSuccess {
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -(smart_contract_balance as i64),
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Originated(
                                    expected_kt1.clone(),
                                )),
                                changes: smart_contract_balance as i64,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        originated_contracts: vec![Originated {
                            contract: expected_kt1.clone(),
                        }],
                        consumed_milligas: 2_352_383_u64.into(),
                        storage_size: 38_u64.into(),
                        paid_storage_size_diff: 38_u64.into(),
                        lazy_storage_diff: None,
                    },
                }),
                internal_operation_results: vec![],
            }),
        }];

        assert_eq!(receipts, expected_receipts);

        // Source paid only the fee; SafeStorage rolled back the
        // initial-balance transfer, the variable burn, and the
        // pre-burn smart-contract setup.
        let src_account =
            context::implicit_from_public_key_hash(&src.pkh).expect("source account");
        assert_eq!(src_account.balance(&host).unwrap(), (funded - fee).into());

        // SafeStorage rolled back the origination: the smart contract is
        // nowhere to be found in durable storage.
        let originated_account = context::originated_from_kt1(&expected_kt1)
            .expect("originated account handle");
        assert!(
            !originated_account.exists(&host).unwrap(),
            "originated contract must be rolled back"
        );

        // Initial-balance transfer rolled back too.
        assert_eq!(
            originated_account.balance(&host).unwrap(),
            0_u64.into(),
            "originated contract balance must be 0 (initial transfer rolled back)"
        );
    }

    #[test]
    fn test_internal_origination_of_a_smart_contract() {
        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();
        let src = bootstrap1();
        let init_src_balance = 100000;
        let expected_init_contract_balance = 1000000;
        init_account(&mut host, &src.pkh, 100000);
        reveal_account(&mut host, &src);
        let mut gas = Gas::default();

        let originated_code = "CDR;
                        NIL operation;
                        PAIR;";
        let originated_script =
            make_create_contract_block("unit", "unit", originated_code);
        let parsed_script = parser
            .parse_top_level(&originated_script)
            .expect("Should have parsed the script");
        let init_script = make_script_emitting_internal_origination(&originated_script);

        // Create a script that emits internal operations to multiple targets
        let contract_chapo_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &contract_chapo_hash,
            &init_script,
            &Micheline::prim0(mir::lexer::Prim::None, &mut gas).unwrap(),
            &1000000_u64.into(),
        );

        let operation = make_operation(
            10,
            1,
            22100,
            500,
            src.clone(),
            vec![OperationContent::Transfer(TransferContent {
                amount: 1000.into(),
                destination: Contract::Originated(contract_chapo_hash.clone()),
                parameters: Parameters {
                    entrypoint: mir::ast::Entrypoint::default(),
                    value: Micheline::from(())
                        .encode(&mut Gas::default())
                        .unwrap()
                        .unwrap(),
                },
            })],
        );
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation.clone(),
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );
        assert_eq!(
            receipts.len(),
            1,
            "There should be one receipt for the transfer operation"
        );
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "First receipt should be an Applied Transfer but is {:?}",
            receipts[0]
        );
        let internal_receipts = get_internal_receipts(&receipts[0].receipt);

        assert_eq!(
            internal_receipts.len(),
            1,
            "There should be one internal operation"
        );
        let expected_address =
            OriginationNonce::initial(OperationHash::default()).generate_kt1();

        assert_eq!(
            internal_receipts[0],
            InternalOperationSum::Origination(InternalContentWithMetadata {
                content: OriginationContent {
                    balance: 1000.into(),
                    delegate: None,
                    script: Script {
                        code: parsed_script.encode(&mut Gas::default()).unwrap().unwrap(),
                        storage: Micheline::from(())
                            .encode(&mut Gas::default())
                            .unwrap()
                            .unwrap(),
                    },
                },
                result: ContentResult::Applied(OriginationSuccess {
                    balance_updates: vec![
                        // Variable burn pair (storage_bus): 30 × COST_PER_BYTES.
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -30,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 30,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        // Slot burn pair (origination_bus): ORIGINATION_SIZE × COST_PER_BYTES.
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -257,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 257,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        // Prior: initial-balance transfer (parent → child).
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Originated(
                                contract_chapo_hash.clone()
                            )),
                            changes: -1000,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Originated(
                                expected_address.clone()
                            )),
                            changes: 1000,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    originated_contracts: vec![Originated {
                        contract: expected_address.clone(),
                    }],
                    consumed_milligas: 2_302_203_u64.into(),
                    storage_size: 30_u64.into(),
                    paid_storage_size_diff: 30_u64.into(),
                    lazy_storage_diff: None,
                }),
                sender: Contract::Originated(contract_chapo_hash.clone()),
                nonce: 0
            }),
            "Internal origination should match the expected structure"
        );

        // Check Balances of everything is correct
        let src_account = context::implicit_from_public_key_hash(&src.pkh)
            .expect("Should have succeeded to create an account");
        let init_contract_account =
            context::originated_from_contract(&Contract::Originated(contract_chapo_hash))
                .expect("Should have succeeded to create an account");
        let originated_account =
            context::originated_from_contract(&Contract::Originated(expected_address))
                .expect("Should have succeeded to create an account");
        let expected_src_balance = init_src_balance
            - 10 // fee for the operation
            - 30 // origination cost paid by the contract (30 bytes × COST_PER_BYTES)
            - 257 // storage cost paid by the source (ORIGINATION_SIZE × COST_PER_BYTES)
            - 27 // post-execution storage burn for the transfer-to-chapo
                 // (paid_storage_size_diff = 27 bytes × COST_PER_BYTES)
            - 1000; // amount sent to the originated contract
        assert_eq!(
            src_account.balance(&host).unwrap(),
            expected_src_balance.into(),
            "Source balance should be correct"
        );
        assert_eq!(
            init_contract_account.balance(&host).unwrap(),
            expected_init_contract_balance.into(),
            "Init contract balance should be correct"
        );
        assert_eq!(
            originated_account.balance(&host).unwrap(),
            1000u64.into(),
            "Originated contract balance should be correct"
        );
    }

    #[test]
    fn internal_operation_cap_is_block_cumulative() {
        // L2-1644: the 65535 internal-operation cap (L1's
        // Too_many_internal_operations) is cumulative across the block,
        // threaded via BlockCtx::internal_operations_base. An operation whose
        // internal op would allocate nonce 65535 fails IN ISOLATION (the block
        // is not aborted), like L1 backtracking the offending operation.
        let run = |base: u128| {
            let mut host = MockKernelHost::default();
            let src = bootstrap1();
            init_account(&mut host, &src.pkh, 100000);
            reveal_account(&mut host, &src);
            let mut gas = Gas::default();
            let originated_script =
                make_create_contract_block("unit", "unit", "CDR; NIL operation; PAIR;");
            let init_script =
                make_script_emitting_internal_origination(&originated_script);
            let contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_1).unwrap();
            init_contract(
                &mut host,
                &contract_hash,
                &init_script,
                &Micheline::prim0(mir::lexer::Prim::None, &mut gas).unwrap(),
                &1000000_u64.into(),
            );
            let operation = make_operation(
                10,
                1,
                22100,
                500,
                src.clone(),
                vec![OperationContent::Transfer(TransferContent {
                    amount: 1000.into(),
                    destination: Contract::Originated(contract_hash),
                    parameters: Parameters {
                        entrypoint: mir::ast::Entrypoint::default(),
                        value: Micheline::from(())
                            .encode(&mut Gas::default())
                            .unwrap()
                            .unwrap(),
                    },
                })],
            );
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &BlockCtx {
                    level: &0u32.into(),
                    now: &0i64.into(),
                    chain_id: &HashTrait::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
                    internal_operations_base: base,
                    tracing_enabled: false,
                    http_trace_enabled: false,
                },
                false,
                None,
                None,
                &test_safe_roots(),
            )
        };
        let applied = |result| {
            matches!(
                &ProcessedOperation::into_receipts(result)[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            )
        };
        // Just below the cap: the internal origination takes nonce 65534 and
        // is applied.
        assert!(applied(run(65_534).expect("no kernel error")));
        // At the cap: producing the internal op would allocate nonce 65535, so
        // the operation fails in isolation and the block is not aborted.
        assert!(!applied(run(65_535).expect("block must not abort")));
    }

    /// Regression for L2-1642: the KT1 returned by CREATE_CONTRACT, stored
    /// in `originated_contracts`, and written to durable storage must be
    /// derived from origination index 0 for the first origination of an
    /// operation, matching Tezos L1 (use-then-increment). The pre-fix kernel
    /// incremented before deriving, producing the first KT1 from index 1,
    /// which disagreed with the L1-canonical address computed from the
    /// (renumbered) receipt nonce. For a single internal origination the
    /// per-operation local nonce is 0, which is exactly what `renumber_nonces`
    /// (kernel/src/apply.rs) assigns at block finalization for the first
    /// internal op, so this also pins the post-renumber value.
    #[test]
    fn test_create_contract_kt1_uses_l1_canonical_index_zero() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 100000);
        reveal_account(&mut host, &src);
        let mut gas = Gas::default();

        // A contract whose code performs exactly one CREATE_CONTRACT.
        let originated_code = "CDR; NIL operation; PAIR;";
        let originated_script =
            make_create_contract_block("unit", "unit", originated_code);
        let init_script = make_script_emitting_internal_origination(&originated_script);

        let contract_chapo_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &contract_chapo_hash,
            &init_script,
            &Micheline::prim0(mir::lexer::Prim::None, &mut gas).unwrap(),
            &1000000_u64.into(),
        );

        // Operation hash of the call that triggers the internal CREATE_CONTRACT.
        let op_hash = OperationHash::default();

        let operation = make_operation(
            10,
            1,
            22100,
            500,
            src.clone(),
            vec![OperationContent::Transfer(TransferContent {
                amount: 1000.into(),
                destination: Contract::Originated(contract_chapo_hash),
                parameters: Parameters {
                    entrypoint: mir::ast::Entrypoint::default(),
                    value: Micheline::from(())
                        .encode(&mut Gas::default())
                        .unwrap()
                        .unwrap(),
                },
            })],
        );
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                op_hash.clone(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        let internal_receipts = get_internal_receipts(&receipts[0].receipt);
        assert_eq!(
            internal_receipts.len(),
            1,
            "There should be exactly one internal origination"
        );

        let (originated_kt1, nonce) = match &internal_receipts[0] {
            InternalOperationSum::Origination(InternalContentWithMetadata {
                result:
                    ContentResult::Applied(OriginationSuccess {
                        originated_contracts,
                        ..
                    }),
                nonce,
                ..
            }) => {
                assert_eq!(
                    originated_contracts.len(),
                    1,
                    "There should be exactly one originated contract"
                );
                (originated_contracts[0].contract.clone(), *nonce)
            }
            other => panic!("Expected an Applied internal origination, got {other:?}"),
        };

        // The internal origination is the first (and only) internal op: its
        // local nonce is 0, and renumber_nonces preserves 0 for the first
        // internal op of the first operation in a block.
        assert_eq!(nonce, 0, "First internal-operation nonce must be 0");

        // L1-canonical: derived from index 0 (== the receipt nonce), not the
        // pre-fix index 1 (L2-1642).
        let l1_canonical = mir::interpreter::compute_contract_address(&op_hash, 0);
        let pre_fix_buggy = mir::interpreter::compute_contract_address(&op_hash, 1);
        assert_eq!(
            originated_kt1, l1_canonical,
            "Originated KT1 must use the L1-canonical index 0"
        );
        assert_eq!(
            originated_kt1,
            mir::interpreter::compute_contract_address(&op_hash, nonce as u32),
            "Originated KT1 must agree with the (renumbered) receipt nonce"
        );
        assert_ne!(
            originated_kt1, pre_fix_buggy,
            "Originated KT1 must NOT use the pre-fix index 1 (L2-1642)"
        );

        // The contract is actually stored at the L1-canonical KT1.
        let originated_account = context::originated_from_kt1(&l1_canonical)
            .expect("originated account handle");
        assert!(
            originated_account.exists(&host).unwrap(),
            "Contract must be stored at the L1-canonical KT1"
        );
    }

    /// In this test, the CREATE_CONTRACT instruction is called three
    /// times.  The result of the first call is dropped.  The results
    /// of the two other calls are swapped.  The point is to check
    /// that the addresses of the originated contracts are not mixed
    /// up.
    #[test]
    fn test_internal_originations_generated_addresses() {
        let mut host = MockKernelHost::default();
        let mut gas = Gas::default();
        let parser = mir::parser::Parser::new();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 1000000);
        reveal_account(&mut host, &src);

        let originated_code = "CDR;
                        NIL operation;
                        PAIR;";
        let originated_script_1 =
            make_create_contract_block("unit", "unit", originated_code);
        let originated_script_2 =
            make_create_contract_block("nat", "nat", originated_code);
        let parsed_script_2 = parser
            .parse_top_level(&originated_script_2)
            .expect("Should have parsed the script");
        let originated_script_3 =
            make_create_contract_block("bytes", "bytes", originated_code);
        let parsed_script_3 = parser
            .parse_top_level(&originated_script_3)
            .expect("Should have parsed the script");
        let init_script = make_script_emitting_two_internal_originations(
            &originated_script_1,
            &originated_script_2,
            &originated_script_3,
        );

        // Create a script that emits internal operations to multiple targets
        let contract_chapo_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &contract_chapo_hash,
            &init_script,
            &Micheline::prim0(mir::lexer::Prim::None, &mut gas).unwrap(),
            &0.into(),
        );

        let operation = make_operation(
            10,
            1,
            22300,
            2000,
            src.clone(),
            vec![OperationContent::Transfer(TransferContent {
                amount: 0.into(),
                destination: Contract::Originated(contract_chapo_hash.clone()),
                parameters: Parameters {
                    entrypoint: mir::ast::Entrypoint::default(),
                    value: Micheline::from(())
                        .encode(&mut Gas::default())
                        .unwrap()
                        .unwrap(),
                },
            })],
        );
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation.clone(),
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );
        assert_eq!(
            receipts.len(),
            1,
            "There should be one receipt for the transfer operation"
        );
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "First receipt should be an Applied Transfer but is {:?}",
            receipts[0].receipt
        );
        let internal_receipts = get_internal_receipts(&receipts[0].receipt);

        assert_eq!(
            internal_receipts.len(),
            2,
            "There should be two internal operations"
        );
        let (_expected_address_1, expected_address_2, expected_address_3) = {
            let mut nonce = OriginationNonce::initial(OperationHash::default());
            let expected_address_1 = nonce.generate_kt1();
            let expected_address_2 = nonce.generate_kt1();
            let expected_address_3 = nonce.generate_kt1();
            (expected_address_1, expected_address_2, expected_address_3)
        };

        assert_eq!(
            internal_receipts[0],
            InternalOperationSum::Origination(InternalContentWithMetadata {
                content: OriginationContent {
                    balance: 0.into(),
                    delegate: None,
                    script: Script {
                        code: parsed_script_3
                            .encode(&mut Gas::default())
                            .unwrap()
                            .unwrap(),
                        storage: Micheline::from(vec![])
                            .encode(&mut Gas::default())
                            .unwrap()
                            .unwrap(),
                    },
                },
                result: ContentResult::Applied(OriginationSuccess {
                    balance_updates: vec![
                        // Variable burn pair: 33 × COST_PER_BYTES.
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -33,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 33,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        // Slot burn pair: ORIGINATION_SIZE × COST_PER_BYTES.
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -257,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 257,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    originated_contracts: vec![Originated {
                        contract: expected_address_3,
                    },],
                    consumed_milligas: 2_302_215_u64.into(),
                    storage_size: 33_u64.into(),
                    paid_storage_size_diff: 33_u64.into(),
                    lazy_storage_diff: None,
                }),
                sender: Contract::Originated(contract_chapo_hash.clone()),
                nonce: 0
            }),
            "Internal origination should match the expected structure"
        );

        assert_eq!(
            internal_receipts[1],
            InternalOperationSum::Origination(InternalContentWithMetadata {
                content: OriginationContent {
                    balance: 0.into(),
                    delegate: None,
                    script: Script {
                        code: parsed_script_2
                            .encode(&mut Gas::default())
                            .unwrap()
                            .unwrap(),
                        storage: Micheline::from(num_bigint::BigUint::from(1u32))
                            .encode(&mut Gas::default())
                            .unwrap()
                            .unwrap(),
                    },
                },
                result: ContentResult::Applied(OriginationSuccess {
                    balance_updates: vec![
                        // Variable burn pair: 30 × COST_PER_BYTES.
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -30,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 30,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        // Slot burn pair: ORIGINATION_SIZE × COST_PER_BYTES.
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -257,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 257,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    originated_contracts: vec![Originated {
                        contract: expected_address_2,
                    }],
                    consumed_milligas: 2_302_303_u64.into(),
                    storage_size: 30_u64.into(),
                    paid_storage_size_diff: 30_u64.into(),
                    lazy_storage_diff: None,
                }),
                sender: Contract::Originated(contract_chapo_hash),
                nonce: 1
            }),
            "Internal origination should match the expected structure"
        );
    }

    fn backtrack_result<M: OperationKind>(result: M::Success) -> BacktrackedResult<M> {
        BacktrackedResult {
            errors: None,
            result,
        }
    }

    #[test]
    fn test_try_apply_three_origination_batch() {
        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();

        let src = bootstrap1();

        // src & dest each credited with 400000ꜩ
        let src_acc = init_account(&mut host, &src.pkh, 400000);

        // op‑1: reveal
        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: src.pk.clone(),
            proof: None,
        });

        println!("Balance: {:?}", src_acc.balance(&host).unwrap());

        // op‑2 orgination: create a contract with 15ꜩ balance successfully
        let origination_content_1 = OperationContent::Origination(OriginationContent {
            balance: 15.into(),
            delegate: None,
            script: Script {
                code: parser
                    .parse_top_level(UNIT_SCRIPT)
                    .unwrap()
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
                storage: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        });

        // op‑3 orgination: create a contract with 20ꜩ balance successfully
        let origination_content_2 = OperationContent::Origination(OriginationContent {
            balance: 20.into(),
            delegate: None,
            script: Script {
                code: parser
                    .parse_top_level(UNIT_SCRIPT)
                    .unwrap()
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
                storage: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        });

        // op‑4 orgination: create a contract with 999999ꜩ balance fails
        let origination_content_3 = OperationContent::Origination(OriginationContent {
            balance: 999999.into(),
            delegate: None,
            script: Script {
                code: parser
                    .parse_top_level(UNIT_SCRIPT)
                    .unwrap()
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
                storage: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        });

        // op-5 transfer: self-transfer 1ꜩ is skipped
        let transfer_content = OperationContent::Transfer(TransferContent {
            amount: 1.into(),
            destination: src_acc.contract(),
            parameters: Parameters::default(),
        });

        let batch = make_operation(
            5,
            1,
            21040,
            10_000,
            src.clone(),
            vec![
                reveal_content,
                origination_content_1,
                origination_content_2,
                origination_content_3,
                transfer_content,
            ],
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                batch.clone(),
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .unwrap(),
        );

        let mut orignation_nonce = OriginationNonce::initial(OperationHash::default());
        let expected_kt1_1 = orignation_nonce.generate_kt1();
        let expected_kt1_2 = orignation_nonce.generate_kt1();
        let expected_receipts = zip_operations(
            batch,
            vec![
                OperationResultSum::Reveal(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: 5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::BackTracked(backtrack_result(RevealSuccess {
                        consumed_milligas: 148786_u64.into(),
                    })),
                    internal_operation_results: vec![],
                }),
                OperationResultSum::Origination(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: 5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::BackTracked(backtrack_result(
                        OriginationSuccess {
                            balance_updates: vec![
                                // Variable burn pair: 30 × COST_PER_BYTES.
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        src.pkh.clone(),
                                    )),
                                    changes: -30,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::StorageFees,
                                    changes: 30,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                // Slot burn pair: ORIGINATION_SIZE × COST_PER_BYTES.
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        src.pkh.clone(),
                                    )),
                                    changes: -257,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::StorageFees,
                                    changes: 257,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        src.pkh.clone(),
                                    )),
                                    changes: -15,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Originated(
                                        expected_kt1_1.clone(),
                                    )),
                                    changes: 15,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                            ],
                            originated_contracts: vec![Originated {
                                contract: expected_kt1_1.clone(),
                            }],
                            consumed_milligas: 2_303_578_u64.into(),
                            storage_size: 30.into(),
                            paid_storage_size_diff: 30.into(),
                            lazy_storage_diff: None,
                        },
                    )),
                    internal_operation_results: vec![],
                }),
                OperationResultSum::Origination(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: 5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::BackTracked(backtrack_result(
                        OriginationSuccess {
                            balance_updates: vec![
                                // Variable burn pair: 30 × COST_PER_BYTES.
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        src.pkh.clone(),
                                    )),
                                    changes: -30,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::StorageFees,
                                    changes: 30,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                // Slot burn pair: ORIGINATION_SIZE × COST_PER_BYTES.
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        src.pkh.clone(),
                                    )),
                                    changes: -257,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::StorageFees,
                                    changes: 257,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        src.pkh.clone(),
                                    )),
                                    changes: -20,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Originated(
                                        expected_kt1_2.clone(),
                                    )),
                                    changes: 20,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                            ],
                            originated_contracts: vec![Originated {
                                contract: expected_kt1_2.clone(),
                            }],
                            consumed_milligas: 2_303_578_u64.into(),
                            storage_size: 30.into(),
                            paid_storage_size_diff: 30.into(),
                            lazy_storage_diff: None,
                        },
                    )),
                    internal_operation_results: vec![],
                }),
                OperationResultSum::Origination(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: 5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::Failed(
                        ApplyOperationError::Origination(
                            OriginationError::FailedToApplyBalanceUpdate,
                        )
                        .into(),
                    ),
                    internal_operation_results: vec![],
                }),
                OperationResultSum::Transfer(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: 5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::Skipped,
                    internal_operation_results: vec![],
                }),
            ],
        );
        assert_eq!(
            receipts, expected_receipts,
            "Receipts do not match the expected ones"
        );
        // Check the balances
        assert_eq!(
            src_acc.balance(&host).unwrap(),
            399975.into(),
            "Source account balance should be 399980ꜩ after the operations"
        );

        // Check the counters
        assert_eq!(
            src_acc.counter(&host).unwrap(),
            5.into(),
            "Source account counter should be 4 after the operations"
        );

        // Check the originated contracts
        let expected_contracts = [expected_kt1_1, expected_kt1_2];
        for (i, expected_kt1) in expected_contracts.iter().enumerate() {
            let account = context::originated_from_contract(&Contract::Originated(
                expected_kt1.clone(),
            ))
            .unwrap();
            assert!(
                account.code(&host).is_err(),
                "Account {i} for KT1{expected_kt1} should not exist"
            );
        }
    }

    #[test]
    fn test_origination_contract_typecheck_storage() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        init_account(&mut host, &src.pkh, 50000);
        reveal_account(&mut host, &src);

        let balance = 10.into();

        let code = mir::parser::Parser::new()
            .parse_top_level(UNIT_SCRIPT)
            .expect("Should have succeeded to parse the script")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let storage = Micheline::from(42)
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let origination_content = OriginationContent {
            balance,
            delegate: None,
            script: Script { code, storage },
        };
        let operation = make_operation(
            10,
            1,
            1000,
            0,
            src.clone(),
            vec![OperationContent::Origination(origination_content)],
        );
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts.len(), 1, "There should be one receipt");
        assert!(matches!(
            &receipts[0].receipt,
            OperationResultSum::Origination(OperationResult {
            result: ContentResult::Failed(ApplyOperationErrors { errors }),
            ..
            }) if errors.len() == 1 && matches!(
            &errors[0],
            ApplyOperationError::Origination(
                OriginationError::MirTypecheckingError(_)
            )
            )
        ), "Expected Failed Origination operation result with MirTypecheckingError, got {:?}", receipts[0]);
    }

    // L2-1706: an oversized script is rejected at origination with
    // `ScriptTooLarge`. The gate runs before decoding, so raw bytes suffice.
    #[test]
    fn test_origination_script_too_large() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        init_account(&mut host, &src.pkh, 50000);
        reveal_account(&mut host, &src);

        let code = vec![1u8; crate::MICHELSON_MAXIMUM_SCRIPT_SIZE + 1];
        let storage = vec![];
        let origination_content = OriginationContent {
            balance: 10.into(),
            delegate: None,
            script: Script { code, storage },
        };
        let operation = make_operation(
            10,
            1,
            1000,
            0,
            src.clone(),
            vec![OperationContent::Origination(origination_content)],
        );
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Origination(OperationResult {
                result: ContentResult::Failed(ApplyOperationErrors { errors }),
                ..
                }) if errors.len() == 1 && matches!(
                &errors[0],
                ApplyOperationError::Origination(
                    OriginationError::ScriptTooLarge(_)
                )
                )
            ),
            "Expected Failed Origination operation result with ScriptTooLarge, got {:?}",
            receipts[0]
        );
    }

    // L2-1706 regression: a deeply nested `or` parameter type exceeds the
    // type-size cap while staying under the script-size cap (~8 kB), so it is
    // rejected with `MirTypecheckingError` (`TypeTooLarge`) instead of OOM.
    #[test]
    fn test_origination_deeply_nested_or_parameter_rejected() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        init_account(&mut host, &src.pkh, 50000);
        reveal_account(&mut host, &src);

        // 1001 `or` levels => 2003 type nodes > 2001 cap, but only ~8 kB.
        let mut param = String::from("unit");
        for _ in 0..1001 {
            param = format!("(or unit {param})");
        }
        let script_str = format!(
            "parameter {param}; storage unit; code {{ CDR; NIL operation; PAIR }}"
        );
        let code = mir::parser::Parser::new()
            .parse_top_level(&script_str)
            .expect("Should have parsed the deeply nested script")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        assert!(
            code.len() < crate::MICHELSON_MAXIMUM_SCRIPT_SIZE,
            "script must stay under the byte cap to exercise the type-size cap",
        );
        let storage = Micheline::from(0)
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let origination_content = OriginationContent {
            balance: 10.into(),
            delegate: None,
            script: Script { code, storage },
        };
        let operation = make_operation(
            10,
            1,
            100000,
            0,
            src.clone(),
            vec![OperationContent::Origination(origination_content)],
        );
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts.len(), 1, "There should be one receipt");
        assert!(matches!(
            &receipts[0].receipt,
            OperationResultSum::Origination(OperationResult {
            result: ContentResult::Failed(ApplyOperationErrors { errors }),
            ..
            }) if errors.len() == 1 && matches!(
            &errors[0],
            ApplyOperationError::Origination(
                OriginationError::MirTypecheckingError(msg)
            ) if msg.contains("type too large")
            )
        ), "Expected Failed Origination with MirTypecheckingError(type too large), got {:?}", receipts[0]);
    }

    #[test]
    // Tests that empty transfers (external or internal) to implicit accounts
    // fail, and empty transfers (external or internal) to smart contracts
    // succeed.
    fn test_empty_transfers() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        let dst = bootstrap2();
        let kt1_addr =
            ContractKt1Hash::from_base58_check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton")
                .expect("ContractKt1Hash b58 conversion should have succeeded");
        // Setup accounts with 50 mutez in their balance
        init_account(&mut host, &src.pkh, 1000);
        reveal_account(&mut host, &src);
        let (code, storage) = (
            r#"
                        parameter (or (unit %default) (address %call));
                        storage unit;
                        code
                        { UNPAIR;
                          IF_LEFT
                            { DROP; NIL operation; PAIR }
                            { CONTRACT unit;
                              { IF_NONE { { UNIT ; FAILWITH } } {} } ;
                              PUSH mutez 0;
                              UNIT;
                              TRANSFER_TOKENS;
                              NIL operation;
                              SWAP;
                              CONS;
                              PAIR } }
            "#,
            &Micheline::from(()),
        );
        init_contract(&mut host, &kt1_addr, code, storage, &0.into());

        // An empty external transfer to an implicit account fails.
        let operation = make_transfer_operation(
            15,
            1,
            21000,
            5,
            src.clone(),
            0.into(),
            Contract::Implicit(dst.pkh),
            Parameters::default(),
        );
        let receipts1 = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts1.len(), 1, "There should be one receipt");
        assert!(matches!(
            &receipts1[0].receipt,
            OperationResultSum::Transfer(OperationResult {
                result: ContentResult::Failed(ApplyOperationErrors { errors }),
                ..
            }) if errors.len() == 1 && matches!(
                &errors[0],
                ApplyOperationError::Transfer(
                    TransferError::EmptyImplicitTransfer
                )
            )
        ), "Expected Failed Transfer operation result with EmptyImplicitTransfer, got {:?}", receipts1[0]);

        // An empty external transfer to a smart contract succeeds.
        let operation = make_transfer_operation(
            15,
            2,
            21110,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(kt1_addr.clone()),
            Parameters {
                entrypoint: Entrypoint::try_from("default")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );
        let receipts2 = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts2.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts2[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess { .. }
                    )),
                    ..
                })
            ),
            "Expected Successful Transfer operation result, got {:?}",
            receipts2[0].receipt
        );

        // An empty internal transfer to an implicit account fails.
        let operation = make_transfer_operation(
            15,
            3,
            23000,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(kt1_addr.clone()),
            Parameters {
                entrypoint: Entrypoint::try_from("call")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(src.clone().pkh.to_b58check())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );
        let receipts3 = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts3.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts3[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(BacktrackedResult { result: TransferTarget::ToContrat(TransferSuccess { .. }), .. }),
                    internal_operation_results,
                    ..
                }) if internal_operation_results.len() == 1 && matches!(
                    &internal_operation_results[0],
                    InternalOperationSum::Transfer(InternalContentWithMetadata {result: ContentResult::Failed(ApplyOperationErrors { errors }), ..})
                        if errors.len() == 1 && matches!(
                            &errors[0],
                            ApplyOperationError::Transfer(
                                TransferError::EmptyImplicitTransfer
                            )
                        )
                )
            ),
            "Expected Failed Transfer operation result with EmptyImplicitTransfer, got {:?}",
            receipts3[0].receipt
        );

        // An empty internal transfer to a smart contract succeeds.
        let operation = make_transfer_operation(
            15,
            4,
            4000,
            5,
            src,
            0.into(),
            Contract::Originated(kt1_addr.clone()),
            Parameters {
                entrypoint: Entrypoint::try_from("call")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(kt1_addr.to_b58check())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );
        let receipts4 = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts4.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts4[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess{ .. })),
                    internal_operation_results,
                    ..
                }) if internal_operation_results.len() == 1 && matches!(
                    &internal_operation_results[0],
                    InternalOperationSum::Transfer(InternalContentWithMetadata {result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess{ .. })), ..})
                )
            ),
            "Expected Successful Transfer operation result, got {:?}",
            receipts4[0].receipt
        );
    }

    #[test]
    fn test_view_instruction() {
        let mut host = MockKernelHost::default();
        let mut gas = Gas::default();
        let src = bootstrap1();
        let mut orignation_nonce = OriginationNonce::initial(OperationHash::default());
        let view_addr = orignation_nonce.generate_kt1();
        let caller_addr = orignation_nonce.generate_kt1();

        let arena = Arena::new();

        init_account(&mut host, &src.pkh, 1000);
        reveal_account(&mut host, &src);
        let (code_view, storage_view) = (
            r#"
                parameter unit ;
                storage int ;
                code { CDR ; NIL operation ; PAIR } ;
                view "get_value" unit int { CDR ; }
            "#,
            &Micheline::from(5),
        );
        init_contract(&mut host, &view_addr, code_view, storage_view, &0.into());

        let mich_addr = TypedValue::Address(Address {
            hash: mir::ast::AddressHash::Kt1(view_addr),
            entrypoint: Entrypoint::default(),
        })
        .into_micheline_optimized_legacy(&arena, &mut gas)
        .unwrap();

        let (code_caller, storage_caller) = (
            r#"
                parameter unit ;
                storage address ;
                code { CDR ;
                    DUP ;
                    UNIT ;
                    VIEW "get_value" int ;
                    IF_NONE { PUSH string "View failed" ; FAILWITH }
                            { PUSH int 5 ; ASSERT_CMPEQ ; NIL operation ; PAIR } }
            "#,
            &mich_addr,
        );

        init_contract(
            &mut host,
            &caller_addr,
            code_caller,
            storage_caller,
            &0.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            21000,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(caller_addr),
            Parameters::default(),
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess { .. }
                    )),
                    ..
                })
            ),
            "Expected Successful Transfer operation result, got {:?}",
            receipts[0].receipt
        );
    }

    #[test]
    fn test_view_balance() {
        let mut host = MockKernelHost::default();
        let mut gas = Gas::default();
        let src = bootstrap1();
        let mut orignation_nonce = OriginationNonce::initial(OperationHash::default());
        let view_addr = orignation_nonce.generate_kt1();
        let caller_addr = orignation_nonce.generate_kt1();

        let arena = Arena::new();

        init_account(&mut host, &src.pkh, 1000);
        reveal_account(&mut host, &src);

        // Contract with a view that returns its BALANCE
        let view_balance = 500;
        init_contract(
            &mut host,
            &view_addr,
            r#"
                parameter unit ;
                storage unit ;
                code { CDR ; NIL operation ; PAIR } ;
                view "get_balance" unit mutez { DROP ; BALANCE }
            "#,
            &Micheline::from(()),
            &view_balance.into(),
        );

        let mich_addr = TypedValue::Address(Address {
            hash: mir::ast::AddressHash::Kt1(view_addr),
            entrypoint: Entrypoint::default(),
        })
        .into_micheline_optimized_legacy(&arena, &mut gas)
        .unwrap();

        // Caller invokes the view and asserts the result equals 500
        init_contract(
            &mut host,
            &caller_addr,
            r#"
                parameter unit ;
                storage address ;
                code { CDR ;
                    DUP ;
                    UNIT ;
                    VIEW "get_balance" mutez ;
                    IF_NONE { PUSH string "View failed" ; FAILWITH }
                            { PUSH mutez 500 ; ASSERT_CMPEQ ; NIL operation ; PAIR } }
            "#,
            &mich_addr,
            &0.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            21000,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(caller_addr),
            Parameters::default(),
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert_eq!(receipts.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess { .. }
                    )),
                    ..
                })
            ),
            "Expected Successful Transfer operation result, got {:?}",
            receipts[0].receipt
        );
    }

    const SCRIPTS_FOLDER: &str = "../../../michelson_test_scripts/big_maps/";

    fn read_script(file: &str) -> String {
        read_to_string(format!("{SCRIPTS_FOLDER}{file}"))
            .unwrap_or_else(|_| panic!("Contract source code not found for {file}"))
    }

    fn big_map_was_removed<Host: StorageV1>(ctx: &mut TcCtx<'_, Host>, id: BigMapId) {
        let types = ctx
            .big_map_get_type(&id)
            .expect("Get big_map type should not panic");
        assert_eq!(types, None, "Temporary big_map was not correctly removed");
    }

    struct BigMapTransfer {
        sender: TezlinkOriginatedAccount,
        receiver: TezlinkOriginatedAccount,
        receipts: Vec<OperationWithMetadata>,
    }

    fn transfer_big_map<Host>(
        ctx: &mut TcCtx<'_, Host>,
        tz1: &Bootstrap,
        script_sender: &str,
        init_sender: &str,
        script_receiver: &str,
        init_receiver: &str,
    ) -> BigMapTransfer
    where
        Host: StorageV1,
    {
        let sender_addr = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");
        let receiver_addr = ContractKt1Hash::from_base58_check(CONTRACT_2)
            .expect("ContractKt1Hash b58 conversion should have succeeded");
        init_account(ctx.host, &tz1.pkh, 10_000_000);
        reveal_account(ctx.host, tz1);

        let parser = Parser::new();

        let sender_contract = init_contract(
            ctx.host,
            &sender_addr,
            script_sender,
            &parser
                .parse(init_sender)
                .expect("Failed to parse sender storage"),
            &0.into(),
        );

        let receiver_contract = init_contract(
            ctx.host,
            &receiver_addr,
            script_receiver,
            &parser
                .parse(init_receiver)
                .expect("Failed to parse receiver storage"),
            &0.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            21040,
            60_000,
            tz1.clone(),
            10.into(),
            Contract::Originated(sender_addr.clone()),
            Parameters {
                entrypoint: Entrypoint::default(),
                value: Micheline::from(CONTRACT_2)
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                ctx.host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        BigMapTransfer {
            sender: sender_contract,
            receiver: receiver_contract,
            receipts,
        }
    }

    fn test_transfer_big_map<'a>(
        script_receiver: &str,
        init_receiver: &str,
        script_sender: &str,
        init_sender: &str,
        expected_sender_big_map: Option<BTreeMap<TypedValue<'a>, TypedValue<'a>>>,
        expected_receiver_big_map: Option<BTreeMap<TypedValue<'a>, TypedValue<'a>>>,
    ) {
        let mut host = MockKernelHost::default();
        make_default_ctx!(ctx, &mut host);
        let tz1 = bootstrap1();

        let result = transfer_big_map(
            &mut ctx,
            &tz1,
            script_sender,
            init_sender,
            script_receiver,
            init_receiver,
        );

        big_map_was_removed(&mut ctx, (-1).into());

        for r in result.receipts {
            assert!(r.receipt.is_applied())
        }

        let parser = Parser::new();
        let sender_contract = result.sender;
        let receiver_contract = result.receiver;

        if let Some(expected_sender_big_map) = expected_sender_big_map {
            let storage = sender_contract.storage(ctx.host).unwrap();
            let mich_storage =
                Micheline::decode_raw(&parser.arena, &storage, &mut Gas::default())
                    .unwrap()
                    .expect("Coudln't decode storage.");
            let big_map_id = typecheck_value(
                &mich_storage,
                &mut ctx,
                &Type::Int,
                AllowForgedLazyStorageId::No,
            )
            .expect("Storage has unexpected type");
            match &big_map_id {
                TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                    &mut ctx,
                    &parser.arena,
                    &id.clone().into(),
                    Type::String,
                    Type::Bytes,
                    expected_sender_big_map,
                ),
                _ => panic!("ID should've been integer"),
            };
        }

        if let Some(expected_receiver_big_map) = expected_receiver_big_map {
            let storage = receiver_contract.storage(ctx.host).unwrap();
            let mich_storage =
                Micheline::decode_raw(&parser.arena, &storage, &mut Gas::default())
                    .unwrap()
                    .expect("Coudln't decode storage.");
            match mich_storage {
                Micheline::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                    &mut ctx,
                    &Arena::new(),
                    &id.into(),
                    Type::String,
                    Type::Bytes,
                    expected_receiver_big_map,
                ),
                _ => panic!("ID should've been integer"),
            };
        }
    }

    // Receiver in {drop, store, store_updated} and Sender in {fresh, stored, stored_updated}
    #[test]
    fn big_map_transfer_receiver_drop_sender_fresh() {
        let script_receiver = read_script("receiver_drop.tz");
        let script_sender = read_script("sender_fresh.tz");
        test_transfer_big_map(
            &script_receiver,
            "Unit",
            &script_sender,
            "Unit",
            None,
            None,
        );
    }

    #[test]
    fn big_map_transfer_receiver_drop_sender_stored() {
        let script_receiver = read_script("receiver_drop.tz");
        let script_sender = read_script("sender_stored.tz");
        test_transfer_big_map(
            &script_receiver,
            "Unit",
            &script_sender,
            "{Elt \"d\" 0x; }",
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
            None,
        );
    }

    #[test]
    fn big_map_transfer_receiver_drop_sender_stored_updated() {
        let script_receiver = read_script("receiver_drop.tz");
        let script_sender = read_script("sender_stored_updated.tz");
        test_transfer_big_map(
            &script_receiver,
            "Unit",
            &script_sender,
            "{Elt \"b\" 0x; Elt \"d\" 0x; }",
            Some(BTreeMap::from([
                (TypedValue::String("d".into()), TypedValue::Bytes("".into())),
                (TypedValue::String("b".into()), TypedValue::Bytes("".into())),
            ])),
            None,
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_sender_fresh() {
        let script_receiver = read_script("receiver_store.tz");
        let script_sender = read_script("sender_fresh.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "Unit",
            None,
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_sender_stored() {
        let script_receiver = read_script("receiver_store.tz");
        let script_sender = read_script("sender_stored.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "{Elt \"d\" 0x; }",
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_sender_stored_updated() {
        let script_receiver = read_script("receiver_store.tz");
        let script_sender = read_script("sender_stored_updated.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "{Elt \"b\" 0x; Elt \"d\" 0x; }",
            Some(BTreeMap::from([
                (TypedValue::String("d".into()), TypedValue::Bytes("".into())),
                (TypedValue::String("b".into()), TypedValue::Bytes("".into())),
            ])),
            Some(BTreeMap::from([
                (TypedValue::String("d".into()), TypedValue::Bytes("".into())),
                (
                    TypedValue::String("a".into()),
                    TypedValue::Bytes(vec![0u8, 16u8]),
                ),
            ])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_updated_sender_fresh() {
        let script_receiver = read_script("receiver_store_updated.tz");
        let script_sender = read_script("sender_fresh.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "Unit",
            None,
            Some(BTreeMap::from([(
                TypedValue::String("c".into()),
                TypedValue::Bytes(vec![(16 + 1), (2 * 16 + 4)]),
            )])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_updated_sender_stored() {
        let script_receiver = read_script("receiver_store_updated.tz");
        let script_sender = read_script("sender_stored.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "{Elt \"d\" 0x; }",
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
            Some(BTreeMap::from([(
                TypedValue::String("c".into()),
                TypedValue::Bytes(vec![(16 + 1), (2 * 16 + 4)]),
            )])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_updated_sender_stored_updated() {
        let script_receiver = read_script("receiver_store_updated.tz");
        let script_sender = read_script("sender_stored_updated.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "{Elt \"b\" 0x; Elt \"d\" 0x; }",
            Some(BTreeMap::from([
                (TypedValue::String("d".into()), TypedValue::Bytes("".into())),
                (TypedValue::String("b".into()), TypedValue::Bytes("".into())),
            ])),
            Some(BTreeMap::from([
                (
                    TypedValue::String("c".into()),
                    TypedValue::Bytes(vec![(16 + 1), (2 * 16 + 4)]),
                ),
                (
                    TypedValue::String("a".into()),
                    TypedValue::Bytes(vec![0u8, 16u8]),
                ),
            ])),
        );
    }

    #[test]
    fn big_map_transfer_with_creation() {
        let mut host = MockKernelHost::default();
        make_default_ctx!(ctx, &mut host);
        let tz1 = bootstrap1();

        let originator_addr = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");

        let script_originator = read_script("originator.tz");

        init_account(ctx.host, &tz1.pkh, 100_000_000);
        reveal_account(ctx.host, &tz1);
        let parser = Parser::new();

        let originator_contract = init_contract(
            ctx.host,
            &originator_addr,
            &script_originator,
            &parser
                .parse("{Elt \"b\" 0x; Elt \"d\" 0x; }")
                .expect("Could not parse initial storage"),
            &0.into(),
        );

        let operation = make_transfer_operation(
            0,
            1,
            21040,
            10_000,
            tz1.clone(),
            0.into(),
            Contract::Originated(originator_addr),
            Parameters::default(),
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                ctx.host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        assert!(receipts.len() == 1);

        let receipt = receipts.first().unwrap();

        let internal_operation_results = match &receipt.receipt {
            OperationResultSum::Transfer(OperationResult {
                internal_operation_results,
                ..
            }) => internal_operation_results.as_slice(),
            _ => panic!("Failed to read originated contracts from receipt"),
        };

        let mut contracts: Vec<Originated> = vec![];
        for ir in internal_operation_results {
            let originated_contracts = match &ir {
                InternalOperationSum::Origination(InternalContentWithMetadata {
                    result:
                        ContentResult::Applied(OriginationSuccess {
                            originated_contracts,
                            ..
                        }),
                    ..
                }) => originated_contracts,
                _ => &vec![],
            };
            contracts.extend_from_slice(originated_contracts);
        }

        assert!(contracts.len() == 3);

        let Originated {
            contract: created_addr_0,
        } = &contracts[0];
        let created_acount_0 = context::originated_from_kt1(created_addr_0)
            .expect("Failed to retrieve generated account");

        let Originated {
            contract: created_addr_1,
        } = &contracts[1];
        let created_acount_1 = context::originated_from_kt1(created_addr_1)
            .expect("Failed to retrieve generated account");

        let Originated {
            contract: created_addr_2,
        } = &contracts[2];
        let created_acount_2 = context::originated_from_kt1(created_addr_2)
            .expect("Failed to retrieve generated account");

        let storage_originator = originator_contract
            .storage(ctx.host)
            .expect("Failed to fetch storage for originator");
        let mich_storage_originator = Micheline::decode_raw(
            &parser.arena,
            &storage_originator,
            &mut Gas::default(),
        )
        .unwrap()
        .expect("Couldn't decode storage.");
        let big_map_id_originator = typecheck_value(
            &mich_storage_originator,
            &mut ctx,
            &Type::Int,
            AllowForgedLazyStorageId::No,
        )
        .expect("Storage has unexpected type");
        match &big_map_id_originator {
            TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                &mut ctx,
                &parser.arena,
                &id.clone().into(),
                Type::String,
                Type::Bytes,
                BTreeMap::from([
                    (TypedValue::String("d".into()), TypedValue::Bytes(vec![])),
                    (TypedValue::String("b".into()), TypedValue::Bytes(vec![])),
                ]),
            ),
            _ => panic!("ID should've been integer"),
        }

        println!("Originator OK");

        let storage_0 = created_acount_0
            .storage(ctx.host)
            .expect("Failed to fetch storage for created account #0");
        let mich_storage_0 =
            Micheline::decode_raw(&parser.arena, &storage_0, &mut Gas::default())
                .unwrap()
                .expect("Couldn't decode storage.");
        let big_map_id_0 = typecheck_value(
            &mich_storage_0,
            &mut ctx,
            &Type::Int,
            AllowForgedLazyStorageId::No,
        )
        .expect("Storage has unexpected type");
        match &big_map_id_0 {
            TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                &mut ctx,
                &parser.arena,
                &id.clone().into(),
                Type::String,
                Type::Bytes,
                BTreeMap::from([(
                    TypedValue::String("d".into()),
                    TypedValue::Bytes("".into()),
                )]),
            ),
            _ => panic!("ID should've been integer"),
        }

        println!("Created_0 OK");

        let storage_1 = created_acount_1
            .storage(ctx.host)
            .expect("Failed to fetch storage for created account #1");
        let mich_storage_1 =
            Micheline::decode_raw(&parser.arena, &storage_1, &mut Gas::default())
                .unwrap()
                .expect("Coudln't decode storage.");
        let big_map_id_1 = typecheck_value(
            &mich_storage_1,
            &mut ctx,
            &Type::Int,
            AllowForgedLazyStorageId::No,
        )
        .expect("Storage has unexpected type");
        match &big_map_id_1 {
            TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                &mut ctx,
                &parser.arena,
                &id.clone().into(),
                Type::String,
                Type::Bytes,
                BTreeMap::from([
                    (TypedValue::String("d".into()), TypedValue::Bytes(vec![])),
                    (TypedValue::String("b".into()), TypedValue::Bytes(vec![])),
                ]),
            ),
            _ => panic!("ID should've been integer"),
        }

        println!("Created_1 OK");

        let storage_2 = created_acount_2
            .storage(ctx.host)
            .expect("Failed to fetch storage for created account #2");
        let mich_storage_2 =
            Micheline::decode_raw(&parser.arena, &storage_2, &mut Gas::default())
                .unwrap()
                .expect("Coudln't decode storage.");
        let big_map_id_2 = typecheck_value(
            &mich_storage_2,
            &mut ctx,
            &Type::Int,
            AllowForgedLazyStorageId::No,
        )
        .expect("Storage has unexpected type");
        match &big_map_id_2 {
            TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                &mut ctx,
                &parser.arena,
                &id.clone().into(),
                Type::String,
                Type::Bytes,
                BTreeMap::from([
                    (TypedValue::String("d".into()), TypedValue::Bytes(vec![])),
                    (
                        TypedValue::String("a".into()),
                        TypedValue::Bytes(vec![0u8, 16u8]),
                    ),
                ]),
            ),
            _ => panic!("ID should've been integer"),
        }

        println!("Created_2 OK");
    }

    // Regression test for the forged-`big_map`-id vulnerability: an external
    // operation must not be able to reference another contract's `big_map` by
    // passing its bare id as a parameter. Before the fix, a writer contract
    // could overwrite a victim's `big_map` this way (and a reader could read
    // it). The parameter is now rejected and the victim's storage is left
    // untouched. Mirrors L1's `allow_forged_lazy_storage_id:false` gate on
    // operation parameters.
    #[test]
    fn forged_big_map_id_in_external_parameter_is_rejected() {
        use mir::ast::big_map::LazyStorage;

        let mut host = MockKernelHost::default();
        make_default_ctx!(ctx, &mut host);

        let tz1 = bootstrap1();
        init_account(ctx.host, &tz1.pkh, 10_000_000);
        reveal_account(ctx.host, &tz1);

        // Victim: a contract owning a `big_map nat string` initialised with
        // `0 -> "genesis-42"`. We allocate the big_map directly in the lazy
        // storage (as origination would) and point the victim's storage at
        // its bare id.
        let victim_id = ctx
            .big_map_new(&Type::Nat, &Type::String, false)
            .expect("big_map allocation should succeed");
        ctx.big_map_update(
            &victim_id,
            TypedValue::nat(0),
            Some(TypedValue::String("genesis-42".into())),
        )
        .expect("big_map update should succeed");

        let victim_addr = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");
        let _victim = init_contract(
            ctx.host,
            &victim_addr,
            "parameter unit ; storage (big_map nat string) ; \
             code { CDR ; NIL operation ; PAIR }",
            &Micheline::Int(victim_id.value.0.clone()),
            &0.into(),
        );

        // Attacker: a writer taking a `big_map nat string` parameter, writing
        // "Hacked-by-mi" at key 0 and storing it.
        let parser = Parser::new();
        let writer_addr = ContractKt1Hash::from_base58_check(CONTRACT_2)
            .expect("ContractKt1Hash b58 conversion should have succeeded");
        let _writer = init_contract(
            ctx.host,
            &writer_addr,
            "parameter (big_map nat string) ; storage (big_map nat string) ; \
             code { CAR ; PUSH string \"Hacked-by-mi\" ; SOME ; PUSH nat 0 ; \
             UPDATE ; NIL operation ; PAIR }",
            &parser.parse("{}").expect("Failed to parse writer storage"),
            &0.into(),
        );

        // Forge the victim's `big_map` id as the writer's parameter.
        let forged_param = Micheline::Int(victim_id.value.0.clone());
        let operation = make_transfer_operation(
            15,
            1,
            21040,
            60_000,
            tz1.clone(),
            10.into(),
            Contract::Originated(writer_addr.clone()),
            Parameters {
                entrypoint: Entrypoint::default(),
                value: forged_param.encode(&mut Gas::default()).unwrap().unwrap(),
            },
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                ctx.host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        // The forged-id parameter must be rejected: the operation is not
        // applied.
        assert!(
            receipts.iter().all(|r| !r.receipt.is_applied()),
            "forged big_map id parameter should have been rejected, got: {receipts:?}"
        );

        // And the victim's `big_map` must be left untouched.
        crate::mir_ctx::tests::assert_big_map_eq(
            &mut ctx,
            &Arena::new(),
            &victim_id,
            Type::Nat,
            Type::String,
            BTreeMap::from([(
                TypedValue::nat(0),
                TypedValue::String("genesis-42".into()),
            )]),
        );
    }

    // Regression test for the origination variant of the forged-`big_map`-id
    // vulnerability: a contract's initial storage supplied in an external
    // origination must not reference another contract's `big_map` by a forged
    // id. Doing so would let the freshly-originated contract usurp a big_map it
    // does not own and read or overwrite it through its own entrypoints. The
    // origination is now rejected. Mirrors L1's
    // `allow_forged_lazy_storage_id_in_storage:false` at origination.
    #[test]
    fn forged_big_map_id_in_origination_storage_is_rejected() {
        use mir::ast::big_map::LazyStorage;

        let mut host = MockKernelHost::default();
        make_default_ctx!(ctx, &mut host);

        let tz1 = bootstrap1();
        init_account(ctx.host, &tz1.pkh, 10_000_000);
        reveal_account(ctx.host, &tz1);

        // Victim big_map { 0 -> "genesis-42" } owned by an existing contract.
        let victim_id = ctx
            .big_map_new(&Type::Nat, &Type::String, false)
            .expect("big_map allocation should succeed");
        ctx.big_map_update(
            &victim_id,
            TypedValue::nat(0),
            Some(TypedValue::String("genesis-42".into())),
        )
        .expect("big_map update should succeed");
        let victim_addr = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");
        let _victim = init_contract(
            ctx.host,
            &victim_addr,
            "parameter unit ; storage (big_map nat string) ; \
             code { CDR ; NIL operation ; PAIR }",
            &Micheline::Int(victim_id.value.0.clone()),
            &0.into(),
        );

        // Originate a contract whose initial storage is the victim's forged
        // big_map id.
        let parser = Parser::new();
        let code = parser
            .parse_top_level(
                "parameter unit ; storage (big_map nat string) ; code { CDR ; \
                 NIL operation ; PAIR }",
            )
            .expect("Failed to parse origination code")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let storage = parser
            .parse(&format!("{victim_id}"))
            .expect("Failed to parse origination storage")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let operation = make_origination_operation(
            15,
            1,
            21040,
            60_000,
            tz1.clone(),
            0,
            Script { code, storage },
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                ctx.host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        // The origination must be rejected.
        assert!(
            receipts.iter().all(|r| !r.receipt.is_applied()),
            "forged big_map id in origination storage should have been \
             rejected, got: {receipts:?}"
        );

        // And the failure must carry the storage typecheck error, i.e. the
        // forged big_map id is what got it refused (not some unrelated error).
        let rendered = format!("{receipts:?}");
        assert!(
            rendered.contains("unexpected forged lazy storage id"),
            "origination failure should carry the storage typecheck error, \
             got: {rendered}"
        );

        // And the victim's big_map must be left untouched.
        crate::mir_ctx::tests::assert_big_map_eq(
            &mut ctx,
            &Arena::new(),
            &victim_id,
            Type::Nat,
            Type::String,
            BTreeMap::from([(
                TypedValue::nat(0),
                TypedValue::String("genesis-42".into()),
            )]),
        );
    }

    #[test]
    fn verify_temp_big_map_content_is_cleaned() {
        let mut host = MockKernelHost::default();
        make_default_ctx!(ctx, &mut host);
        let tz1 = bootstrap1();
        let parser = Parser::new();

        // A contract that receives a big_map and checks if
        // there's a "d" key in the big_map received
        let script_receiver = r#"
                parameter (big_map string bytes) ;
                storage bool ;
                code { CAR; PUSH string "d"; MEM; NIL operation; PAIR }
            "#;
        let script_sender = read_script("sender_stored.tz");

        // Transfer the big_map in the storage of the sender ({ Elt "d" 0x })
        // And the receiver checks if the big_map received contains the key "d"
        // The storage of the receiver contract should change to 'True' after this
        // transfer.
        let result = transfer_big_map(
            &mut ctx,
            &tz1,
            &script_sender,
            "{Elt \"d\" 0x; }",
            script_receiver,
            "False",
        );

        let receiver = result.receiver;

        for r in result.receipts {
            assert!(r.receipt.is_applied())
        }

        // Checks that the storage of the receiver is true
        let storage = receiver
            .storage(ctx.host)
            .expect("Get storage should succeed");
        let storage = Micheline::decode_raw(&parser.arena, &storage, &mut Gas::default())
            .unwrap()
            .expect("Micheline should be decodable");
        let typed_storage = typecheck_value(
            &storage,
            &mut ctx,
            &Type::Bool,
            AllowForgedLazyStorageId::No,
        )
        .expect("Typecheck value should succeed");
        assert_eq!(typed_storage, TypedValue::Bool(true));

        // We redeploy a second contract sender, that will send a different big_map
        // to the receiver contract ({Elt "a" 0x})
        let second_sender_contract =
            ContractKt1Hash::from_base58_check(CONTRACT_3).unwrap();

        let _ = init_contract(
            ctx.host,
            &second_sender_contract,
            &script_sender,
            &parser
                .parse("{Elt \"a\" 0x; }")
                .expect("Failed to parse receiver storage"),
            &0.into(),
        );

        let operation = make_transfer_operation(
            0,
            2,
            21040,
            60_000,
            tz1.clone(),
            0.into(),
            Contract::Originated(second_sender_contract),
            Parameters {
                entrypoint: Entrypoint::default(),
                value: Micheline::from(receiver.kt1().to_base58_check())
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        // After that call, the storage of the receiver should be 'False'
        // as the big_map passed in argument doesn't have the key "d"
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                ctx.host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            ),
        );

        for r in receipts {
            assert!(r.receipt.is_applied())
        }

        // Checks that the storage of the receiver contract is 'False'
        let storage = receiver
            .storage(ctx.host)
            .expect("Get storage should succeed");
        let storage = Micheline::decode_raw(&parser.arena, &storage, &mut Gas::default())
            .unwrap()
            .expect("Micheline should be decodable");
        let typed_storage = typecheck_value(
            &storage,
            &mut ctx,
            &Type::Bool,
            AllowForgedLazyStorageId::No,
        )
        .expect("Typecheck value should succeed");
        assert_eq!(typed_storage, TypedValue::Bool(false));
    }

    /// Build the gateway's generic %call argument for a plain value
    /// transfer: `Pair "http://ethereum/<dest>" (Pair {} (Pair 0x (Pair 1 None)))`
    /// (POST, no headers, empty body, no callback).
    fn gateway_call_transfer_param<'a>(
        arena: &'a Arena<Micheline<'a>>,
        eth_destination: &str,
    ) -> Micheline<'a> {
        use crate::enshrined_contracts::tests::build_http_call_micheline;
        build_http_call_micheline(
            arena,
            &format!("http://ethereum/{eth_destination}"),
            &[],
            &[],
            1,
        )
    }

    /// Test a successful transfer from a tz1 address to an Ethereum address via the gateway.
    /// This tests the happy path: source has enough balance, gateway executes, bridge succeeds.
    #[test]
    fn apply_transfer_to_gateway_happy_path() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        // Gateway contract address: KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw
        let gateway_kt1 =
            ContractKt1Hash::from_base58_check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw")
                .expect("Gateway KT1 address should be valid");

        // Set up source with enough balance (100 mutez: 15 for fees + 50 for transfer + some extra)
        let source_account = init_account(&mut host, &src.pkh, 100);
        reveal_account(&mut host, &src);

        // Create Micheline parameters for the generic %call entrypoint:
        // a POST to the destination Ethereum address with an empty body.
        // The amount is taken from the operation's amount field via ctx.amount()
        let eth_destination = "0x1234567890123456789012345678901234567890";
        let arena = Arena::new();
        let params_micheline = gateway_call_transfer_param(&arena, eth_destination);

        let operation = make_transfer_operation(
            15,      // fee
            1,       // counter
            100_000, // gas_limit
            100,     // storage_limit
            src.clone(),
            50_u64.into(), // amount to transfer
            Contract::Originated(gateway_kt1.clone()),
            Parameters {
                entrypoint: Entrypoint::try_from("call").unwrap(),
                value: params_micheline
                    .encode(&mut Gas::default())
                    .unwrap()
                    .unwrap(),
            },
        );

        let registry = crate::test_utils::MockRegistry::new("KT1_mock_alias".to_string());
        let mut journal = TezosXJournal::new(
            tezosx_journal::CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let processed = validate_and_apply_operation(
            &mut host,
            &registry,
            &mut journal,
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        // Verify the operation succeeded
        assert_eq!(receipt.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipt[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "Expected Applied Transfer result, got {:?}",
            receipt[0].receipt
        );

        // Verify that source balance decreased by fee (15) + transfer amount (50)
        // Initial: 100, Final: 100 - 15 - 50 = 35
        assert_eq!(
            source_account.balance(&host).unwrap(),
            35_u64.into(),
            "Source balance should have decreased by fee + transfer amount"
        );

        // Verify that serve was called
        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 1, "Serve should have been called once");

        // Verify that the gateway balance is 0 (funds were forwarded, not locked)
        let gateway_account = context::originated_from_kt1(&gateway_kt1)
            .expect("Gateway account should exist");
        assert_eq!(
            gateway_account.balance(&host).unwrap(),
            0_u64.into(),
            "Gateway balance should be 0 after successful serve call"
        );
    }

    // DA fee enforcement: delayed inbox operations (da_fee_check = None) skip the
    // DA fee check entirely, so zero-fee operations are accepted.
    #[test]
    fn da_fee_delayed_inbox_zero_fees_accepted() {
        let mut host = MockKernelHost::default();
        let source = bootstrap1();
        let dest = bootstrap2();
        let _src_account = init_account(&mut host, &source.pkh, 1_000_000);
        let _dst_account = init_account(&mut host, &dest.pkh, 0);
        reveal_account(&mut host, &source);

        // Transfer with fee = 0 mutez.
        let operation = make_transfer_operation(
            0,     // fee
            1,     // counter
            21000, // gas_limit
            0,     // storage_limit
            source,
            10_u64.into(), // amount
            Contract::Implicit(dest.pkh),
            Parameters::default(),
        );

        // None = delayed inbox: DA fee check is skipped.
        let result = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        );

        assert!(
            result.is_ok(),
            "Delayed inbox operation with zero fees should succeed"
        );
    }

    // DA fee enforcement: batch with reveal + transfer whose combined fee covers DA cost.
    #[test]
    fn da_fee_batch_sufficient_fees_accepted() {
        let mut host = MockKernelHost::default();
        let source = bootstrap1();
        let dest = bootstrap2();
        let src_account = init_account(&mut host, &source.pkh, 1_000_000);
        let dst_account = init_account(&mut host, &dest.pkh, 0);

        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: source.pk.clone(),
            proof: None,
        });

        let transfer_content = OperationContent::Transfer(TransferContent {
            amount: 10_u64.into(),
            destination: Contract::Implicit(dest.pkh),
            parameters: Parameters::default(),
        });

        // Single batch fee of 10_000 mutez shared across both operations.
        let batch = make_operation(
            10_000, // fee
            1,      // counter
            22000,  // gas_limit
            0,      // storage_limit
            source,
            vec![reveal_content, transfer_content],
        );

        // DA fee = 4 mutez/byte: 10_000 mutez covers up to 2500 bytes.
        let da_fee_per_byte_mutez = 4;
        let required_da_fees =
            get_required_da_fees(&batch, da_fee_per_byte_mutez).unwrap();
        let result = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            batch,
            &block_ctx!(),
            false,
            Some(required_da_fees),
            None,
            &test_safe_roots(),
        );

        assert!(result.is_ok(), "Batch with sufficient fees should succeed");

        // Initial: 1_000_000, fee: 10_000 per op × 2 ops = 20_000, transfer: 10
        assert_eq!(
            src_account.balance(&host).unwrap(),
            979_990_u64.into(),
            "Source balance should be 1_000_000 - 20_000 (fees) - 10 (amount)"
        );
        assert_eq!(
            dst_account.balance(&host).unwrap(),
            10_u64.into(),
            "Destination should have received 10 mutez"
        );
    }

    // DA fee enforcement: operation with sufficient fees is accepted.
    #[test]
    fn da_fee_sufficient_fees_accepted() {
        let mut host = MockKernelHost::default();
        let source = bootstrap1();
        let dest = bootstrap2();
        let src_account = init_account(&mut host, &source.pkh, 1_000_000);
        let dst_account = init_account(&mut host, &dest.pkh, 0);
        reveal_account(&mut host, &source);

        // Transfer with fee = 10_000 mutez, well above DA cost for any reasonable op size.
        let operation = make_transfer_operation(
            10_000, // fee
            1,      // counter
            21000,  // gas_limit
            0,      // storage_limit
            source,
            10_u64.into(), // amount
            Contract::Implicit(dest.pkh),
            Parameters::default(),
        );

        // DA fee = 4 mutez/byte: 10_000 mutez covers up to 2500 bytes.
        let da_fee_per_byte_mutez = 4;
        let required_da_fees =
            get_required_da_fees(&operation, da_fee_per_byte_mutez).unwrap();
        let result = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            Some(required_da_fees),
            None,
            &test_safe_roots(),
        );

        assert!(
            result.is_ok(),
            "Operation with sufficient fees should succeed"
        );

        // Initial: 1_000_000, fee: 10_000, transfer: 10
        assert_eq!(
            src_account.balance(&host).unwrap(),
            989_990_u64.into(),
            "Source balance should be 1_000_000 - 10_000 (fee) - 10 (amount)"
        );
        assert_eq!(
            dst_account.balance(&host).unwrap(),
            10_u64.into(),
            "Destination should have received 10 mutez"
        );
    }

    // DA fee enforcement: operation fees below DA cost are rejected.
    #[test]
    fn da_fee_insufficient_fees_rejected() {
        let mut host = MockKernelHost::default();
        let source = bootstrap1();
        let _src_account = init_account(&mut host, &source.pkh, 1_000_000);
        reveal_account(&mut host, &source);

        // Transfer with fee = 1 mutez, well below DA cost.
        let operation = make_transfer_operation(
            1,     // fee
            1,     // counter
            21000, // gas_limit
            0,     // storage_limit
            source,
            10_u64.into(), // amount
            Contract::Implicit(bootstrap2().pkh),
            Parameters::default(),
        );

        // DA fee = 4 mutez/byte: for any operation > 0 bytes, 1 mutez is insufficient.
        let da_fee_per_byte_mutez = 4;
        let required_da_fees =
            get_required_da_fees(&operation, da_fee_per_byte_mutez).unwrap();
        let result = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            Some(required_da_fees),
            None,
            &test_safe_roots(),
        );

        assert_eq!(
            result,
            Err(OperationError::Validation(ValidityError::InsufficientFee))
        );
    }

    /// Calling the gateway with the generic %call entrypoint (plain tez
    /// transfer to an EVM address) produces an Applied receipt whose storage
    /// field is None.
    ///
    /// Enshrined contracts always return empty storage bytes; the receipt must
    /// encode this as absent storage rather than Some([]).
    #[test]
    fn gateway_tez_transfer_receipt_storage_is_none() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        let gateway_kt1 =
            ContractKt1Hash::from_base58_check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw")
                .expect("Gateway KT1 address should be valid");
        init_account(&mut host, &src.pkh, 100);
        reveal_account(&mut host, &src);

        let arena = Arena::new();
        let operation = make_transfer_operation(
            15,
            1,
            100_000,
            100,
            src,
            50_u64.into(),
            Contract::Originated(gateway_kt1),
            Parameters {
                entrypoint: Entrypoint::try_from("call").unwrap(),
                value: gateway_call_transfer_param(
                    &arena,
                    "0x1111111111111111111111111111111111111111",
                )
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
            },
        );

        let registry = crate::test_utils::MockRegistry::new("KT1_mock_alias".to_string());
        let mut journal = TezosXJournal::new(
            tezosx_journal::CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &registry,
                &mut journal,
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect("validate_and_apply_operation should not fail"),
        );

        assert_eq!(receipts.len(), 1);
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess { storage: None, .. }
                    )),
                    ..
                })
            ),
            "Expected Applied transfer with storage=None, got {:?}",
            receipts[0].receipt
        );
    }

    /// Calling the gateway with the call entrypoint (EVM contract call with
    /// value) produces an Applied receipt whose storage field is None.
    #[test]
    fn gateway_contract_call_receipt_storage_is_none() {
        let mut host = MockKernelHost::default();
        let mut gas = Gas::default();
        let src = bootstrap1();
        let gateway_kt1 =
            ContractKt1Hash::from_base58_check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw")
                .expect("Gateway KT1 address should be valid");
        init_account(&mut host, &src.pkh, 100);
        reveal_account(&mut host, &src);

        // Encode Pair(dest, Pair(method_sig, abi_params)) for the call entrypoint
        let arena = Arena::new();
        let call_value = Micheline::prim2(
            &arena,
            mir::lexer::Prim::Pair,
            Micheline::String("0x2222222222222222222222222222222222222222".to_string()),
            Micheline::prim2(
                &arena,
                mir::lexer::Prim::Pair,
                Micheline::String("store(uint256)".to_string()),
                Micheline::prim2(
                    &arena,
                    mir::lexer::Prim::Pair,
                    Micheline::Bytes(vec![0u8; 32]),
                    Micheline::prim0(mir::lexer::Prim::None, &mut gas).unwrap(),
                    &mut gas,
                )
                .unwrap(),
                &mut gas,
            )
            .unwrap(),
            &mut gas,
        )
        .unwrap();

        let operation = make_transfer_operation(
            15,
            1,
            100_000,
            100,
            src,
            50_u64.into(),
            Contract::Originated(gateway_kt1),
            Parameters {
                entrypoint: Entrypoint::try_from("call_evm").unwrap(),
                value: call_value.encode(&mut Gas::default()).unwrap().unwrap(),
            },
        );

        let registry = crate::test_utils::MockRegistry::new("KT1_mock_alias".to_string());
        let mut journal = TezosXJournal::new(
            tezosx_journal::CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &registry,
                &mut journal,
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect("validate_and_apply_operation should not fail"),
        );

        assert_eq!(receipts.len(), 1);
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess { storage: None, .. }
                    )),
                    ..
                })
            ),
            "Expected Applied transfer with storage=None, got {:?}",
            receipts[0].receipt
        );
    }

    /// A gateway call whose EVM execution reverts propagates as a Failed
    /// receipt (not Applied), confirming that the revert path does not
    /// produce an Applied result with a spurious storage field.
    #[test]
    fn gateway_evm_revert_yields_failed_receipt() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        let gateway_kt1 =
            ContractKt1Hash::from_base58_check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw")
                .expect("Gateway KT1 address should be valid");
        init_account(&mut host, &src.pkh, 100);
        reveal_account(&mut host, &src);

        let registry = crate::test_utils::MockRegistry::new("KT1_mock_revert")
            .with_serve_response(400, b"reverted".to_vec());

        let operation = make_transfer_operation(
            15,
            1,
            100_000,
            100,
            src,
            50_u64.into(),
            Contract::Originated(gateway_kt1),
            Parameters {
                entrypoint: Entrypoint::default(),
                value: Micheline::String(
                    "0x3333333333333333333333333333333333333333".to_string(),
                )
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
            },
        );

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &registry,
                &mut journal,
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect("validate_and_apply_operation should not fail at the protocol level"),
        );

        assert_eq!(receipts.len(), 1);
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Failed(_),
                    ..
                })
            ),
            "Expected Failed transfer when EVM reverts, got {:?}",
            receipts[0].receipt
        );
    }

    // --- Tezos alias forwarder tests ---

    /// Test that sending funds to a Tezos alias KT1 triggers the forwarding
    /// Michelson contract, which calls the TezosXGateway enshrined contract,
    /// which routes funds cross-runtime via registry.serve().
    #[test]
    fn tezos_alias_forwarder_forwards_on_transfer() {
        let mut host = MockKernelHost::default();

        let evm_address = "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef";

        // Deploy the forwarder contract under the Michelson accounts root
        // (which is where validate_and_apply_operation reads from)
        let forwarder_script = r#"
            parameter unit ;
            storage string ;
            code { CDR ;
                   DUP ;
                   PUSH string "http://ethereum/" ;
                   CONCAT ;
                   NONE (contract bytes) ;
                   PUSH nat 1 ;
                   PAIR ;
                   PUSH bytes 0x ;
                   PAIR ;
                   NIL (pair string string) ;
                   PAIR ;
                   SWAP ;
                   PAIR ;
                   PUSH address "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw" ;
                   CONTRACT %call (pair string (pair (list (pair string string)) (pair bytes (pair nat (option (contract bytes)))))) ;
                   IF_NONE { PUSH string "gateway" ; FAILWITH } {} ;
                   SWAP ;
                   BALANCE ;
                   SWAP ;
                   TRANSFER_TOKENS ;
                   NIL operation ;
                   SWAP ;
                   CONS ;
                   PAIR }
        "#;

        // Use an arbitrary KT1 for the alias
        let alias_kt1 =
            ContractKt1Hash::from_base58_check("KT1QbKzQAyJtzprfvUJZv8VGqwQNch2o89di")
                .unwrap();

        let storage_micheline = Micheline::String(evm_address.to_string());
        let _alias_account = init_contract(
            &mut host,
            &alias_kt1,
            forwarder_script,
            &storage_micheline,
            &0_u64.into(),
        );

        let alias_contract = Contract::Originated(alias_kt1.clone());

        // Set up a source account with funds
        let src = bootstrap1();
        let _src_account = init_account(&mut host, &src.pkh, 1_000_000);
        reveal_account(&mut host, &src);

        // Transfer 50 mutez to the alias KT1
        let transfer_amount = 50_u64;
        let operation = make_transfer_operation(
            15,      // fee
            1,       // counter
            500_000, // gas_limit (needs enough for Michelson + gateway)
            500,     // storage_limit
            src.clone(),
            transfer_amount.into(),
            alias_contract,
            Parameters::default(), // unit parameter
        );

        // Use MockRegistry that tracks serve calls
        let registry = crate::test_utils::MockRegistry::new("KT1_mock_alias".to_string());
        let processed = validate_and_apply_operation(
            &mut host,
            &registry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            None,
            None,
            &test_safe_roots(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        let receipt = ProcessedOperation::into_receipts(processed);

        // Verify the operation succeeded
        assert_eq!(receipt.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipt[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "Expected Applied Transfer result, got {:?}",
            receipt[0].receipt
        );

        // Verify serve was called — this means the forwarder called the gateway
        // which routed the transfer cross-runtime
        let serve_calls = registry.serve_calls.borrow();
        assert!(
            !serve_calls.is_empty(),
            "serve() should have been called by the gateway (forwarding contract -> gateway -> serve)"
        );

        // Verify the serve call targeted the ethereum runtime
        let request = &serve_calls[0];
        assert_eq!(
            request.uri().host(),
            Some("ethereum"),
            "Cross-runtime call should target the ethereum runtime"
        );

        // Verify the gateway balance is 0 (funds were forwarded, not locked)
        let gateway_kt1 =
            ContractKt1Hash::from_base58_check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw")
                .unwrap();
        let gateway_account = context::originated_from_kt1(&gateway_kt1)
            .expect("Gateway account should exist");
        assert_eq!(
            gateway_account.balance(&host).unwrap(),
            0_u64.into(),
            "Gateway balance should be 0 after forwarding"
        );
    }

    /// Tests that `operation_consumed_milligas` correctly captures gas from
    /// internal sub-operations (multiple `get_and_reset_milligas_consumed`
    /// calls within a single manager operation).
    ///
    /// Uses `SCRIPT_EMITING_INTERNAL_TRANSFER` which MAP-iterates over a
    /// list of addresses and emits one `TRANSFER_TOKENS` per address.
    /// Each internal transfer triggers a separate gas measurement cycle,
    /// so `total_milligas_consumed` (initial_limit − remaining) is the
    /// only correct way to compute the total.
    ///
    /// Five scenarios:
    /// 1. **Success**: all internal targets run `UNIT_SCRIPT` → Applied
    /// 2. **FAILWITH**: one target runs `FAILING_SCRIPT` → BackTracked
    /// 3. **Out-of-gas**: gas limit too low → Failed, consumes entire limit
    /// 4. **FAILWITH direct**: transfer to `FAILING_SCRIPT` → Failed, partial gas
    /// 5. **Skipped**: batch where first op fails → second op Skipped, 0 gas
    #[test]
    fn gas_tracking_with_internal_operations() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 100_000);
        reveal_account(&mut host, &src);

        // Chapo contract: emits internal transfers to each address in param
        let chapo = ContractKt1Hash::from_base58_check(CONTRACT_1).unwrap();
        init_contract(
            &mut host,
            &chapo,
            SCRIPT_EMITING_INTERNAL_TRANSFER,
            &Micheline::from(()),
            &10_000_u64.into(),
        );

        // OK target: accepts unit, does nothing
        let ok_target = ContractKt1Hash::from_base58_check(CONTRACT_2).unwrap();
        init_contract(
            &mut host,
            &ok_target,
            UNIT_SCRIPT,
            &Micheline::from(()),
            &0_u64.into(),
        );

        // Fail target: always FAILWITHs
        let fail_target = ContractKt1Hash::from_base58_check(CONTRACT_3).unwrap();
        init_contract(
            &mut host,
            &fail_target,
            FAILING_SCRIPT,
            &Micheline::from(()),
            &0_u64.into(),
        );

        let ok_addr =
            Micheline::Bytes(Contract::Originated(ok_target.clone()).to_bytes().unwrap());
        let fail_addr = Micheline::Bytes(
            Contract::Originated(fail_target.clone())
                .to_bytes()
                .unwrap(),
        );

        // Gas limit that comfortably covers all scenarios except OOG.
        const AMPLE_GAS: u64 = 21_150;

        // Helper: build, validate and apply an operation with given content
        // and gas limit. Reads the counter from durable storage each time
        // so it stays in sync even if an operation is rejected during
        // validation.
        let run = |host: &mut MockKernelHost,
                   content: Vec<OperationContent>,
                   gas_limit: u64|
         -> Vec<ProcessedOperation> {
            let counter = context::implicit_from_public_key_hash(&src.pkh)
                .unwrap()
                .counter(host)
                .unwrap()
                .0
                .to_u64()
                .unwrap()
                + 1;
            let op = make_operation(5, counter, gas_limit, 0, src.clone(), content);
            validate_and_apply_operation(
                host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                op,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect("should not fail with a kernel error")
        };

        // Helper: build a direct transfer to a contract with unit parameter.
        let direct_transfer = |target: &ContractKt1Hash| -> OperationContent {
            OperationContent::Transfer(TransferContent {
                amount: 0.into(),
                destination: Contract::Originated(target.clone()),
                parameters: Parameters {
                    entrypoint: mir::ast::Entrypoint::default(),
                    value: Micheline::from(())
                        .encode(&mut Gas::default())
                        .unwrap()
                        .unwrap(),
                },
            })
        };

        // Helper: build a single transfer-to-chapo content with the given
        // target addresses as parameter.
        let chapo_transfer = |addrs: &[Micheline]| -> Vec<OperationContent> {
            vec![OperationContent::Transfer(TransferContent {
                amount: 0.into(),
                destination: Contract::Originated(chapo.clone()),
                parameters: Parameters {
                    entrypoint: mir::ast::Entrypoint::default(),
                    value: Micheline::Seq(addrs)
                        .encode(&mut Gas::default())
                        .unwrap()
                        .unwrap(),
                },
            })]
        };

        // ── Scenario 1: Success ──────────────────────────────────────
        let addrs_ok = vec![ok_addr.clone(), ok_addr.clone(), ok_addr.clone()];
        let success = run(&mut host, chapo_transfer(&addrs_ok), AMPLE_GAS);
        assert_eq!(success.len(), 1);
        assert!(
            matches!(
                &success[0].operation_with_metadata.receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "Expected Applied, got {:?}",
            success[0].operation_with_metadata.receipt
        );
        let gas_success = success[0].operation_consumed_milligas;
        assert!(
            gas_success > 0,
            "Applied op with internal transfers should consume non-zero gas"
        );

        // ── Scenario 2: FAILWITH (middle internal target) ────────────
        let addrs_fail = vec![ok_addr.clone(), fail_addr.clone(), ok_addr.clone()];
        let failed = run(&mut host, chapo_transfer(&addrs_fail), AMPLE_GAS);
        assert_eq!(failed.len(), 1);
        assert!(
            matches!(
                &failed[0].operation_with_metadata.receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "Expected BackTracked (internal FAILWITH), got {:?}",
            failed[0].operation_with_metadata.receipt
        );
        let gas_fail = failed[0].operation_consumed_milligas;
        assert!(
            gas_fail > 0,
            "BackTracked op should still report non-zero consumed gas"
        );
        assert!(
            gas_fail < gas_success,
            "BackTracked gas ({gas_fail}) should be less than success gas ({gas_success}): \
             the 3rd internal transfer was skipped"
        );

        // ── Scenario 3: Out-of-gas ──────────────────────────────────
        // 200 gas = 200_000 milligas. Validation consumes
        // manager_operation (100k) + check_signature (~65k) ≈ 165k,
        // leaving ~35k milligas — not enough to interpret the
        // chapo script.
        let oog_gas_limit: u64 = 200;
        let oog = run(&mut host, chapo_transfer(&addrs_ok), oog_gas_limit);
        assert_eq!(oog.len(), 1);
        assert!(
            matches!(
                &oog[0].operation_with_metadata.receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Failed(ApplyOperationErrors { errors }),
                    ..
                }) if errors.iter().any(|e| match e {
                    ApplyOperationError::Transfer(TransferError::OutOfGas(gas::OutOfGas)) => true,
                    ApplyOperationError::Transfer(
                        TransferError::FailedToExecuteInternalOperation(msg)
                    ) => msg.contains("Gas exhaustion"),
                    _ => false,
                })
            ),
            "Expected Failed with gas exhaustion error, got {:?}",
            oog[0].operation_with_metadata.receipt
        );
        let gas_oog = oog[0].operation_consumed_milligas;
        assert_eq!(
            u64::from(gas_oog),
            oog_gas_limit * 1000,
            "Out-of-gas op should consume its entire gas limit"
        );

        // ── Scenario 4: Failed (FAILWITH) with partial gas ──────────
        // Direct transfer to a contract that FAILWITHs immediately.
        // Unlike scenario 3, the script fails before exhausting gas,
        // so consumed gas is strictly less than the limit.
        let fail_direct = run(&mut host, vec![direct_transfer(&fail_target)], AMPLE_GAS);
        assert_eq!(fail_direct.len(), 1);
        assert!(
            matches!(
                &fail_direct[0].operation_with_metadata.receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Failed(_),
                    ..
                })
            ),
            "Expected Failed (FAILWITH), got {:?}",
            fail_direct[0].operation_with_metadata.receipt
        );
        let gas_fail_direct = fail_direct[0].operation_consumed_milligas;
        assert!(
            gas_fail_direct > 0,
            "Failed (FAILWITH) op should consume non-zero gas"
        );
        assert!(
            u64::from(gas_fail_direct) < AMPLE_GAS * 1000,
            "Failed (FAILWITH) gas ({gas_fail_direct}) should be less than \
             gas limit ({}) — script fails before exhausting gas",
            AMPLE_GAS * 1000
        );

        // ── Scenario 5: Skipped ─────────────────────────────────────
        // Batch of two operations: first fails (FAILWITH), second is
        // skipped. Skipped operations consume 0 gas.
        let skipped = run(
            &mut host,
            vec![
                direct_transfer(&fail_target), // Op 1 → Failed
                direct_transfer(&ok_target),   // Op 2 → Skipped
            ],
            AMPLE_GAS,
        );
        assert_eq!(skipped.len(), 2, "Batch should produce 2 receipts");
        assert!(
            matches!(
                &skipped[0].operation_with_metadata.receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Failed(_),
                    ..
                })
            ),
            "First op should be Failed, got {:?}",
            skipped[0].operation_with_metadata.receipt
        );
        assert!(
            matches!(
                &skipped[1].operation_with_metadata.receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Skipped,
                    ..
                })
            ),
            "Second op should be Skipped, got {:?}",
            skipped[1].operation_with_metadata.receipt
        );
        assert!(
            skipped[0].operation_consumed_milligas > 0,
            "Failed op should consume non-zero gas"
        );
        assert_eq!(
            skipped[1].operation_consumed_milligas, 0,
            "Skipped op should consume 0 gas"
        );
    }

    // --- cross_runtime_transfer metering tests ---

    /// Build a Michelson script that pushes `n` copies of the character 'x' as
    /// a string then FAILWITHs with it, so the attacker-controlled error body
    /// is exactly `n` bytes.
    fn push_failwith_script(n: usize) -> String {
        let payload = "x".repeat(n);
        format!(
            r#"parameter unit;
               storage unit;
               code {{ DROP; PUSH string "{payload}"; FAILWITH }}"#
        )
    }

    /// Drive a `PUSH string "<n bytes>"; FAILWITH` script through
    /// `cross_runtime_transfer` and return the `CracTransferError`.
    ///
    /// `milligas_limit` is the operation-level budget placed in the
    /// `TezlinkOperationGas` that `cross_runtime_transfer` charges against.
    fn run_failwith_crac(
        n: usize,
        milligas_limit: u64,
    ) -> (CracTransferError, u64 /* consumed milligas */) {
        let mut host = MockKernelHost::default();

        // Sender: an implicit account (tz1).
        let src = bootstrap1();
        let sender_account = init_account(&mut host, &src.pkh, 100_000);

        // Destination: a KT1 contract with `PUSH string; FAILWITH`.
        let dest_kt1 = ContractKt1Hash::from_base58_check(CONTRACT_1).unwrap();
        init_contract(
            &mut host,
            &dest_kt1,
            &push_failwith_script(n),
            &Micheline::from(()),
            &0_u64.into(),
        );
        let dest = Contract::Originated(dest_kt1);

        let parameters = Parameters {
            entrypoint: mir::ast::Entrypoint::default(),
            value: Micheline::from(())
                .encode(&mut Gas::default())
                .unwrap()
                .unwrap(),
        };

        let parser = Parser::new();
        let mut operation_gas = TezlinkOperationGas::start_milligas(milligas_limit)
            .expect("milligas within limit");
        let mut tc_ctx = TcCtx {
            host: &mut host,
            operation_gas: &mut operation_gas,
            big_map_diff: std::collections::BTreeMap::new(),
            interpret_context: crate::mir_ctx::InterpretContext::new(),
            next_temporary_id: &mut mir::ast::big_map::BigMapId { value: (-1).into() },
        };
        let mut origination_nonce = OriginationNonce::default();
        let mut counter = 0u128;
        let level = BlockNumber { block_number: 0 };
        let now = Timestamp::from(0);
        let chain_id = tezos_crypto_rs::hash::ChainId::from([0, 0, 0, 0]);
        let mut operation_ctx = crate::mir_ctx::OperationCtx {
            source: &sender_account,
            origination_nonce: &mut origination_nonce,
            counter: &mut counter,
            level: &level,
            now: &now,
            chain_id: &chain_id,
            source_public_key: &[],
            crac_chain_depth: 0,
            crac_origin: None,
            delegated_storage_cost: 0,
            applied_counters: std::collections::BTreeSet::new(),
        };
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let mut nonce_counter: u16 = 0;

        let result = cross_runtime_transfer(
            &mut tc_ctx,
            &mut operation_ctx,
            &NotWiredRegistry,
            &mut journal,
            &sender_account,
            &Narith::from(0u64),
            &dest,
            &parameters,
            &parser,
            &mut nonce_counter,
        );
        let err = match result {
            Ok(_) => panic!("FAILWITH script must return an error"),
            Err(e) => e,
        };

        let consumed = u64::from(operation_gas.total_milligas_consumed());
        (err, consumed)
    }

    /// Gas consumed by `cross_runtime_transfer` scales with the FAILWITH
    /// payload length.  The exact delta matches the sum of three
    /// contributions that grow with `n`:
    ///
    /// 1. Storage code read (`STORAGE_READ_PER_BYTE_MILLIGAS` per code byte)
    /// 2. Micheline decode (`interpret_cost::micheline_decoding_bytes` per code byte)
    /// 3. `charge_persisted_error` (`PERSISTED_ERROR_PER_BYTE_MILLIGAS` per
    ///    byte of the persisted Debug body, charged on the one persisted copy
    ///    of the error — synthetic alias(E_1)→target failed-transfer internal
    ///    op)
    ///
    /// The two scripts differ by exactly `N_LONG - N_SHORT = 190` bytes in
    /// both binary encoding and rendered error length.
    #[test]
    fn cross_runtime_transfer_meters_failwith_body() {
        // Ample budget: 10 M milligas — well above what either script needs.
        const BUDGET: u64 = 10_000_000;
        const N_SHORT: usize = 10;
        const N_LONG: usize = 200;
        let (err_short, gas_short) = run_failwith_crac(N_SHORT, BUDGET);
        let (err_long, gas_long) = run_failwith_crac(N_LONG, BUDGET);

        // Both should fail with an Operation-level error (MichelsonContractInterpretError).
        assert!(
            matches!(
                err_short.error,
                CracError::Operation(TransferError::MichelsonContractInterpretError(_))
            ),
            "expected interpret error (short), got: {:?}",
            err_short.error
        );
        assert!(
            matches!(
                err_long.error,
                CracError::Operation(TransferError::MichelsonContractInterpretError(_))
            ),
            "expected interpret error (long), got: {:?}",
            err_long.error
        );

        // Exact per-byte delta: encode both scripts to get their binary sizes,
        // then compute each contribution independently.
        let parser = mir::parser::Parser::new();
        let short_script_str = push_failwith_script(N_SHORT);
        let long_script_str = push_failwith_script(N_LONG);
        let short_code = parser
            .parse_top_level(&short_script_str)
            .unwrap()
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let long_code = parser
            .parse_top_level(&long_script_str)
            .unwrap()
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        // Storage read gas (charged per byte of code binary on load).
        let storage_read_delta = storage_read_cost_milligas(1, long_code.len() as u64)
            - storage_read_cost_milligas(1, short_code.len() as u64);
        // Micheline decode gas (charged per byte of code binary on decode).
        let decode_delta =
            mir::gas::interpret_cost::micheline_decoding_bytes(long_code.len()).unwrap()
                as u64
                - mir::gas::interpret_cost::micheline_decoding_bytes(short_code.len())
                    .unwrap() as u64;
        // charge_persisted_error gas (charged per byte of the Debug-rendered error).
        let short_rendered = match &err_short.error {
            CracError::Operation(te) => format!("{te:?}").len() as u64,
            other => panic!("expected Operation error (short), got: {other:?}"),
        };
        let long_rendered = match &err_long.error {
            CracError::Operation(te) => format!("{te:?}").len() as u64,
            other => panic!("expected Operation error (long), got: {other:?}"),
        };
        // The error is persisted once (synthetic alias(E_1)→target
        // failed-transfer entry only), charged per byte over the *wrapped*
        // length ("Transfer(" + bare_debug + ")", i.e. bare + 10). The
        // constant +10 wrapper cancels in the delta, so the per-byte charge
        // applies to the rendered length delta.
        let metering_delta =
            PERSISTED_ERROR_PER_BYTE_MILLIGAS * (long_rendered - short_rendered);

        assert_eq!(
            gas_long - gas_short,
            storage_read_delta + decode_delta + metering_delta,
            "exact per-byte delta: storage_read({storage_read_delta}) \
             + decode({decode_delta}) + metering({metering_delta})"
        );
    }

    /// When the gas budget is too tight to meter the FAILWITH body,
    /// `cross_runtime_transfer` returns `OutOfGas` AND still records the
    /// failed CRAC receipt (small — no bloated body), preserving the
    /// invariant that a failed CRAC receipt is always produced.  The
    /// `internal_receipts` field on the error is still present (not dropped)
    /// even on OOG.
    #[test]
    fn cross_runtime_transfer_oog_on_large_failwith_body() {
        // A body that will overflow a tight budget.  We first run with an
        // ample budget to find the actual gas cost, then re-run with a budget
        // below that threshold.
        const N: usize = 500;
        const AMPLE: u64 = 10_000_000;
        let (_, gas_ample) = run_failwith_crac(N, AMPLE);

        // Budget just below what the ample run consumed → must OOG.
        let tight_budget = gas_ample - 1;
        let (err_tight, _gas_tight) = run_failwith_crac(N, tight_budget);

        assert!(
            matches!(
                err_tight.error,
                CracError::Operation(TransferError::OutOfGas(_))
            ),
            "expected OutOfGas on tight budget, got: {:?}",
            err_tight.error
        );
        // Top-level FAILWITH has no internal ops, so internal_receipts is
        // empty — but it must still be present as a field (not dropped).
        assert!(
            err_tight.internal_receipts.is_empty(),
            "internal_receipts must be present (not dropped) even on OOG; \
             got: {:?}",
            err_tight.internal_receipts
        );
    }

    /// Drive a CRAC where the top-level contract (`SCRIPT_EMITING_INTERNAL_TRANSFER`)
    /// issues an internal `TRANSFER_TOKENS` to a `push_failwith_script(n)` contract.
    /// The outer call succeeds but the inner FAILWITHs, so
    /// `cross_runtime_transfer` takes the internal-revert branch and returns
    /// `Err(CracTransferError { error: FailedToExecuteInternalOperation, internal_receipts: [Failed(...)] })`.
    ///
    /// Returns `(CracTransferError, consumed_milligas)`.
    fn run_internal_failwith_crac(
        n: usize,
        milligas_limit: u64,
    ) -> (CracTransferError, u64 /* consumed milligas */) {
        let mut host = MockKernelHost::default();

        // Sender: implicit account.
        let src = bootstrap1();
        let sender_account = init_account(&mut host, &src.pkh, 100_000);

        // Outer contract: issues TRANSFER_TOKENS to a list of addresses.
        // Balance 100 mutez so it can forward 10 mutez per internal op.
        let outer_kt1 = ContractKt1Hash::from_base58_check(CONTRACT_1).unwrap();
        init_contract(
            &mut host,
            &outer_kt1,
            SCRIPT_EMITING_INTERNAL_TRANSFER,
            &Micheline::from(()),
            &100_u64.into(),
        );

        // Inner contract: FAILWITHs with `n` bytes.
        let inner_kt1 = ContractKt1Hash::from_base58_check(CONTRACT_2).unwrap();
        init_contract(
            &mut host,
            &inner_kt1,
            &push_failwith_script(n),
            &Micheline::from(()),
            &0_u64.into(),
        );

        // Parameter for the outer contract: a single-element list containing the
        // inner contract's address (binary Micheline representation).
        let inner_addr_bytes =
            Contract::Originated(inner_kt1.clone()).to_bytes().unwrap();
        let inner_micheline_addr = Micheline::Bytes(inner_addr_bytes);
        let addrs = vec![inner_micheline_addr];
        let param_micheline = Micheline::Seq(&addrs);
        let param_bytes = param_micheline
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        let parameters = Parameters {
            entrypoint: mir::ast::Entrypoint::default(),
            value: param_bytes,
        };

        let parser = mir::parser::Parser::new();
        let mut operation_gas = TezlinkOperationGas::start_milligas(milligas_limit)
            .expect("milligas within limit");
        let mut tc_ctx = TcCtx {
            host: &mut host,
            operation_gas: &mut operation_gas,
            big_map_diff: std::collections::BTreeMap::new(),
            interpret_context: crate::mir_ctx::InterpretContext::new(),
            next_temporary_id: &mut mir::ast::big_map::BigMapId { value: (-1).into() },
        };
        let mut origination_nonce = OriginationNonce::default();
        let mut counter = 0u128;
        let level = BlockNumber { block_number: 0 };
        let now = Timestamp::from(0);
        let chain_id = tezos_crypto_rs::hash::ChainId::from([0, 0, 0, 0]);
        let mut operation_ctx = crate::mir_ctx::OperationCtx {
            source: &sender_account,
            origination_nonce: &mut origination_nonce,
            counter: &mut counter,
            level: &level,
            now: &now,
            chain_id: &chain_id,
            source_public_key: &[],
            crac_chain_depth: 0,
            crac_origin: None,
            delegated_storage_cost: 0,
            applied_counters: std::collections::BTreeSet::new(),
        };
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let mut nonce_counter: u16 = 0;

        let dest = Contract::Originated(outer_kt1);
        let result = cross_runtime_transfer(
            &mut tc_ctx,
            &mut operation_ctx,
            &NotWiredRegistry,
            &mut journal,
            &sender_account,
            &Narith::from(0u64),
            &dest,
            &parameters,
            &parser,
            &mut nonce_counter,
        );
        let err = match result {
            Ok(_) => {
                panic!("cross_runtime_transfer must fail when an internal op FAILWITHs")
            }
            Err(e) => e,
        };

        let consumed = u64::from(operation_gas.total_milligas_consumed());
        (err, consumed)
    }

    /// Gas consumed by `cross_runtime_transfer` scales with the FAILWITH
    /// payload of an **internal** op — closing the bypass by which an
    /// attacker-controlled `TRANSFER_TOKENS` target could route a large body
    /// into the persisted receipt without paying gas.
    ///
    /// (a) Consumed gas scales with `n` (internal body is metered).
    /// (b) Under a tight budget the call OOGs AND the `internal_receipts`
    ///     entry is still present but carries a small (bounded) body.
    #[test]
    fn cross_runtime_transfer_meters_internal_failwith_body() {
        const BUDGET: u64 = 10_000_000;
        const N_SHORT: usize = 10;
        const N_LONG: usize = 200;

        let (err_short, gas_short) = run_internal_failwith_crac(N_SHORT, BUDGET);
        let (err_long, gas_long) = run_internal_failwith_crac(N_LONG, BUDGET);

        // (a) The internal-revert branch is taken: top-level error is
        // `FailedToExecuteInternalOperation` and internal_receipts are present.
        assert!(
            matches!(
                err_short.error,
                CracError::Operation(TransferError::FailedToExecuteInternalOperation(_))
            ),
            "expected FailedToExecuteInternalOperation (short), got: {:?}",
            err_short.error
        );
        assert!(
            matches!(
                err_long.error,
                CracError::Operation(TransferError::FailedToExecuteInternalOperation(_))
            ),
            "expected FailedToExecuteInternalOperation (long), got: {:?}",
            err_long.error
        );

        // The internal receipt carries the `Failed` entry.
        assert_eq!(
            err_short.internal_receipts.len(),
            1,
            "expected one internal receipt (short)"
        );
        assert_eq!(
            err_long.internal_receipts.len(),
            1,
            "expected one internal receipt (long)"
        );

        // Gas scales with the internal FAILWITH body length — metering is applied.
        assert!(
            gas_long > gas_short,
            "longer internal FAILWITH body should cost more gas: \
             {gas_long} vs {gas_short}"
        );

        // (b) Under a tight budget the transfer OOGs.
        // Find the exact cost of the long run then use budget-1.
        let tight_budget = gas_long - 1;
        let (err_oog, _) = run_internal_failwith_crac(N_LONG, tight_budget);

        assert!(
            matches!(
                err_oog.error,
                CracError::Operation(TransferError::FailedToExecuteInternalOperation(_))
                    | CracError::Operation(TransferError::OutOfGas(_))
            ),
            "expected operation-level error on OOG (internal-revert path), got: {:?}",
            err_oog.error
        );

        // The internal receipt entry must still be present on OOG: the failed
        // CRAC receipt is always produced even when the body is bounded.
        assert_eq!(
            err_oog.internal_receipts.len(),
            1,
            "internal_receipts must not be dropped on OOG; got: {:?}",
            err_oog.internal_receipts
        );

        // The internal-receipt body must be bounded (not the full n-byte payload):
        // when OOG the oversized `Failed(errors)` is replaced with
        // `Failed(OutOfGas)`.
        let oog_body_len = match &err_oog.internal_receipts[0] {
            InternalOperationSum::Transfer(r) => match &r.result {
                ContentResult::Failed(errors) => errors
                    .errors
                    .iter()
                    .map(|e| format!("{e:?}").len())
                    .sum::<usize>(),
                other => {
                    panic!("expected Failed internal receipt on OOG, got: {other:?}")
                }
            },
            other => panic!("expected Transfer internal receipt, got: {other:?}"),
        };
        // A bounded body must be much smaller than the N_LONG-byte payload.
        let long_payload_debug_len = format!(
            "{:?}",
            ApplyOperationError::Transfer(
                TransferError::MichelsonContractInterpretError("x".repeat(N_LONG))
            )
        )
        .len();
        assert!(
            oog_body_len < long_payload_debug_len,
            "OOG internal receipt body ({oog_body_len} bytes) must be smaller \
             than the full payload ({long_payload_debug_len} bytes)"
        );
    }

    // --- Fee refund test helpers ---

    /// Balance update: fee debited from source account.
    fn bu_fee_debit(pkh: &PublicKeyHash, fee: u64) -> BalanceUpdate {
        BalanceUpdate {
            balance: Balance::Account(Contract::Implicit(pkh.clone())),
            changes: -(fee as i64),
            update_origin: UpdateOrigin::BlockApplication,
        }
    }

    /// Balance update: fee credited to block fees accumulator.
    fn bu_fee_credit(fee: u64) -> BalanceUpdate {
        BalanceUpdate {
            balance: Balance::BlockFees,
            changes: fee as i64,
            update_origin: UpdateOrigin::BlockApplication,
        }
    }

    /// Balance update: refund credited back to source account.
    fn bu_refund_credit(pkh: &PublicKeyHash, amount: u64) -> BalanceUpdate {
        BalanceUpdate {
            balance: Balance::Account(Contract::Implicit(pkh.clone())),
            changes: amount as i64,
            update_origin: UpdateOrigin::BlockApplication,
        }
    }

    /// Balance update: refund debited from block fees accumulator.
    fn bu_refund_debit(amount: u64) -> BalanceUpdate {
        BalanceUpdate {
            balance: Balance::BlockFees,
            changes: -(amount as i64),
            update_origin: UpdateOrigin::BlockApplication,
        }
    }

    /// Standard base_fee_per_gas for fee refund tests.
    const TEST_BASE_FEE_PER_GAS: u64 = 10u64.pow(9);
    /// Standard Michelson-to-EVM gas multiplier for fee refund tests.
    const TEST_GAS_MULTIPLIER: u64 = 10;

    /// Result of a fee refund test setup.
    struct RefundTestCtx {
        host: MockKernelHost,
        src: Bootstrap,
        source: TezosImplicitAccount,
        destination: TezosImplicitAccount,
        receipt: Vec<OperationWithMetadata>,
        total_consumed_milligas: u64,
    }

    impl RefundTestCtx {
        /// Extract balance_updates from the first receipt (must be a Transfer).
        fn balance_updates(&self) -> &[BalanceUpdate] {
            match &self.receipt[0].receipt {
                OperationResultSum::Transfer(op_result) => &op_result.balance_updates,
                other => panic!("Expected Transfer receipt, got {other:?}"),
            }
        }

        /// Compute the expected refund: fee - da_fees - michelson_gas_to_mutez(consumed).
        fn expected_refund(&self, fee: u64, da_fees: u64) -> u64 {
            let consumed_gas = self.total_consumed_milligas.div_ceil(1000);
            let consumed_gas_fees = (U256::from(TEST_BASE_FEE_PER_GAS)
                * U256::from(TEST_GAS_MULTIPLIER)
                * U256::from(consumed_gas)
                / U256::exp10(12))
            .low_u64();
            fee.saturating_sub(da_fees.saturating_add(consumed_gas_fees))
        }
    }

    /// Set up accounts, build a transfer operation, run it through
    /// `validate_and_apply_operation`, and return the context for assertions.
    ///
    /// `da_fees`: if `Some`, a `FeeRefundConfig` is passed; if `None`, no config.
    fn run_refund_transfer(
        fee: u64,
        amount: u64,
        initial_src_balance: u64,
        da_fees: Option<u64>,
    ) -> RefundTestCtx {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();
        let dst = bootstrap2();

        let source = init_account(&mut host, &src.pkh, initial_src_balance);
        reveal_account(&mut host, &src);
        let destination = init_account(&mut host, &dst.pkh, 50);

        let operation = make_transfer_operation(
            fee,
            1,
            21040,
            5,
            src.clone(),
            amount.into(),
            Contract::Implicit(dst.pkh.clone()),
            Parameters::default(),
        );

        let refund_config = da_fees.map(|da| FeeRefundConfig {
            da_fees: da,
            base_fee_per_gas: U256::from(TEST_BASE_FEE_PER_GAS),
            michelson_to_evm_gas_multiplier: TEST_GAS_MULTIPLIER,
        });

        let processed = validate_and_apply_operation(
            &mut host,
            &NotWiredRegistry,
            &mut TezosXJournal::mock(RuntimeId::Ethereum),
            OperationHash::default(),
            operation,
            &block_ctx!(),
            false,
            None,
            refund_config,
            &test_safe_roots(),
        )
        .expect("validate_and_apply_operation should succeed");

        let total_consumed_milligas =
            ProcessedOperation::total_consumed_milligas(&processed);
        let receipt = ProcessedOperation::into_receipts(processed);

        RefundTestCtx {
            host,
            src,
            source,
            destination,
            receipt,
            total_consumed_milligas,
        }
    }

    // Transfer with fee refund enabled: surplus fees are credited back to source.
    #[test]
    fn apply_transfer_with_fee_refund() {
        let fee: u64 = 1000;
        let amount: u64 = 30;
        let initial_balance: u64 = 2000;
        let da_fees: u64 = 50;

        let ctx = run_refund_transfer(fee, amount, initial_balance, Some(da_fees));
        let bus = ctx.balance_updates();

        // With refund: 4 entries (fee debit/credit + refund credit/debit).
        assert_eq!(bus.len(), 4, "Expected 4 balance_updates, got {:?}", bus);

        assert_eq!(bus[0], bu_fee_debit(&ctx.src.pkh, fee));
        assert_eq!(bus[1], bu_fee_credit(fee));

        let expected_refund = ctx.expected_refund(fee, da_fees);
        assert!(expected_refund > 0, "Test expects a positive refund");

        assert_eq!(bus[2], bu_refund_credit(&ctx.src.pkh, expected_refund));
        assert_eq!(bus[3], bu_refund_debit(expected_refund));

        // Token conservation: all fee-level balance updates sum to zero.
        let total: i64 = bus.iter().map(|bu| bu.changes).sum();
        assert_eq!(total, 0, "Fee-level balance updates must sum to zero");

        // Verify final balances.
        assert_eq!(
            ctx.source.balance(&ctx.host).unwrap(),
            (initial_balance - fee + expected_refund - amount).into(),
            "Source balance should reflect fee, refund, and transfer"
        );
        assert_eq!(
            ctx.destination.balance(&ctx.host).unwrap(),
            (50 + amount).into(),
            "Destination balance should increase by transfer amount"
        );
    }

    // Transfer with fee refund where costs exceed fee: saturating_sub yields refund = 0.
    #[test]
    fn apply_transfer_with_zero_refund() {
        // Use a very small fee so that da_fees + consumed_gas_fees >= fee,
        // making the refund saturate to 0.
        let fee: u64 = 31;
        let amount: u64 = 5;
        let da_fees: u64 = 10;

        let ctx = run_refund_transfer(fee, amount, 100, Some(da_fees));
        let bus = ctx.balance_updates();

        // With zero refund: only 2 entries (fee debit/credit), no refund entries.
        assert_eq!(bus.len(), 2, "Expected 2 balance_updates, got {:?}", bus);

        // Source balance = initial - fee - amount (no refund).
        assert_eq!(
            ctx.source.balance(&ctx.host).unwrap(),
            (100 - fee - amount).into(),
        );
    }

    // Transfer without fee refund config (None): no refund entries.
    #[test]
    fn apply_transfer_without_fee_refund_config() {
        let fee: u64 = 1000;
        let amount: u64 = 30;

        let ctx = run_refund_transfer(fee, amount, 2000, None);
        let bus = ctx.balance_updates();

        // Without refund config: only 2 entries (fee debit/credit).
        assert_eq!(bus.len(), 2, "Expected 2 balance_updates, got {:?}", bus);

        // Source balance = initial - fee - amount.
        assert_eq!(
            ctx.source.balance(&ctx.host).unwrap(),
            (2000 - fee - amount).into(),
        );
    }

    // Failed transfer with fee refund: refund still applies after revert.
    // The operation fails (BalanceTooLow) but the source still gets refunded
    // because fee refund runs after promote/revert, outside the transaction.
    #[test]
    fn apply_failed_transfer_with_fee_refund() {
        let fee: u64 = 1000;
        let initial_balance: u64 = 2000;
        let da_fees: u64 = 50;
        // amount > balance after fee deduction → BalanceTooLow
        let amount: u64 = initial_balance;

        let ctx = run_refund_transfer(fee, amount, initial_balance, Some(da_fees));

        // The operation should have failed.
        assert!(
            matches!(
                &ctx.receipt[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Failed(_),
                    ..
                })
            ),
            "Expected a Failed transfer result"
        );

        let bus = ctx.balance_updates();

        // Even though the transfer failed, fee refund still produces 4 entries.
        assert_eq!(bus.len(), 4, "Expected 4 balance_updates, got {:?}", bus);

        assert_eq!(bus[0], bu_fee_debit(&ctx.src.pkh, fee));
        assert_eq!(bus[1], bu_fee_credit(fee));

        let expected_refund = ctx.expected_refund(fee, da_fees);
        assert!(expected_refund > 0, "Test expects a positive refund");

        // IMPORTANT: Failed operations MUST consume gas, even though the transfer reverted.
        // This validates that the refund calculation properly accounts for gas consumption.
        assert!(
            ctx.total_consumed_milligas > 0,
            "Failed operation MUST consume gas; got {} milligas",
            ctx.total_consumed_milligas
        );

        assert_eq!(bus[2], bu_refund_credit(&ctx.src.pkh, expected_refund));
        assert_eq!(bus[3], bu_refund_debit(expected_refund));

        // Token conservation.
        let total: i64 = bus.iter().map(|bu| bu.changes).sum();
        assert_eq!(total, 0, "Fee-level balance updates must sum to zero");

        // Source paid fees but got refund; transfer was reverted so amount not deducted.
        assert_eq!(
            ctx.source.balance(&ctx.host).unwrap(),
            (initial_balance - fee + expected_refund).into(),
            "Source should pay fee minus refund, transfer amount reverted"
        );
        // Destination unchanged: transfer was reverted.
        assert_eq!(
            ctx.destination.balance(&ctx.host).unwrap(),
            50_u64.into(),
            "Destination should be unchanged after failed transfer"
        );
    }

    fn origination_success_from_receipts(
        receipts: &[OperationWithMetadata],
        index: usize,
    ) -> &OriginationSuccess {
        match &receipts[index].receipt {
            OperationResultSum::Origination(OperationResult {
                result: ContentResult::Applied(success),
                ..
            }) => success,
            other => {
                panic!("Expected Applied Origination at index {index}, got: {other:?}")
            }
        }
    }

    /// Origination receipt exposes the contribution of the big-maps
    /// present in the initial storage.
    #[test]
    fn test_origination_bills_initial_big_map() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 10_000_000);
        reveal_account(&mut host, &src);

        let parser = mir::parser::Parser::new();
        let code = parser
            .parse_top_level(BIG_MAP_UNIT_SCRIPT)
            .expect("Failed to parse BIG_MAP_UNIT_SCRIPT")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let storage = parser
            .parse(r#"Pair { Elt "k" "v" } Unit"#)
            .expect("Failed to parse initial storage")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        let operation = make_origination_operation(
            15,
            1,
            100_000,
            10_000,
            src.clone(),
            0,
            Script {
                code: code.clone(),
                storage,
            },
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect("validate_and_apply_operation must not return a kernel error"),
        );

        assert_eq!(receipts.len(), 1, "Expected one receipt");
        let success = origination_success_from_receipts(&receipts, 0);

        let empty_storage = parser
            .parse(r#"Pair {} Unit"#)
            .expect("Failed to parse empty storage")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        let mut host2 = MockKernelHost::default();
        let src2 = bootstrap2();
        init_account(&mut host2, &src2.pkh, 10_000_000);
        reveal_account(&mut host2, &src2);

        let operation2 = make_origination_operation(
            15,
            1,
            100_000,
            10_000,
            src2.clone(),
            0,
            Script {
                code,
                storage: empty_storage,
            },
        );

        let receipts2 = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host2,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                operation2,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect("baseline origination must not fail"),
        );

        assert_eq!(receipts2.len(), 1, "Expected one baseline receipt");
        let baseline = origination_success_from_receipts(&receipts2, 0);

        // The 33-byte slot forfait is present in both originations and
        // cancels in the delta; only the per-key forfait + value size remain.
        let expected_delta: u64 = 65 + 6;
        let delta = success.storage_size.0.clone() - baseline.storage_size.0.clone();
        assert_eq!(delta, expected_delta.into());

        assert_eq!(success.paid_storage_size_diff, success.storage_size);
    }

    /// Two back-to-back originations on the same `TcCtx` must each
    /// reflect only their own big-map content — the accumulator must
    /// be drained between them, not shared.
    #[test]
    fn test_origination_anti_contamination() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 20_000_000);
        reveal_account(&mut host, &src);

        let parser = mir::parser::Parser::new();
        let code = parser
            .parse_top_level(BIG_MAP_UNIT_SCRIPT)
            .expect("Failed to parse BIG_MAP_UNIT_SCRIPT")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let storage_a = parser
            .parse(r#"Pair { Elt "a" "x" } Unit"#)
            .expect("Failed to parse storage A")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let storage_b = parser
            .parse(r#"Pair { Elt "b" "yy" } Unit"#)
            .expect("Failed to parse storage B")
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        let batch = make_operation(
            15,
            1,
            200_000,
            20_000,
            src.clone(),
            vec![
                OperationContent::Origination(OriginationContent {
                    balance: 0.into(),
                    delegate: None,
                    script: Script {
                        code: code.clone(),
                        storage: storage_a,
                    },
                }),
                OperationContent::Origination(OriginationContent {
                    balance: 0.into(),
                    delegate: None,
                    script: Script {
                        code,
                        storage: storage_b,
                    },
                }),
            ],
        );

        let receipts = ProcessedOperation::into_receipts(
            validate_and_apply_operation(
                &mut host,
                &NotWiredRegistry,
                &mut TezosXJournal::mock(RuntimeId::Ethereum),
                OperationHash::default(),
                batch,
                &block_ctx!(),
                false,
                None,
                None,
                &test_safe_roots(),
            )
            .expect("validate_and_apply_operation must not return a kernel error"),
        );

        assert_eq!(receipts.len(), 2);
        let success_a = origination_success_from_receipts(&receipts, 0);
        let success_b = origination_success_from_receipts(&receipts, 1);

        assert_eq!(success_a.paid_storage_size_diff, success_a.storage_size);
        assert_eq!(success_b.paid_storage_size_diff, success_b.storage_size);

        // A and B differ only by `enc("yy") - enc("x") = 1`. Any other
        // value would mean the accumulator leaked between originations.
        let size_a = success_a.storage_size.0.clone();
        let size_b = success_b.storage_size.0.clone();
        assert_eq!(size_b - size_a, 1u64.into());
    }

    const PAYER_PKH: &str = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";

    /// [`burn_pass`]: when the parent's storage burn succeeds but the
    /// internal's burn exhausts the shared budget, the call returns
    /// `OperationQuotaExceeded` and the *pre-burn* internals are
    /// surfaced — no half-applied storage-fees entry survives.
    #[test]
    fn burn_pass_internal_overshoot_returns_quota_exceeded() {
        let mut host = MockKernelHost::default();
        let pkh = PublicKeyHash::from_b58check(PAYER_PKH).unwrap();
        let payer = init_account(&mut host, &pkh, 10_000_000);
        let internal = InternalOperationSum::Transfer(InternalContentWithMetadata {
            sender: payer.contract(),
            nonce: 0,
            content: TransferContent {
                amount: 0_u64.into(),
                destination: payer.contract(),
                parameters: Parameters::default(),
            },
            result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                paid_storage_size_diff: 5_u64.into(),
                ..Default::default()
            })),
        });
        let mut content: ContentResult<TransferContent> =
            ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                paid_storage_size_diff: 4_u64.into(),
                ..Default::default()
            }));
        let mut storage_limit_remaining = num_bigint::BigUint::from(8_u64);
        let (internals, outcome) = burn_pass::<_, TransferContent, _>(
            &mut host,
            &payer,
            &mut storage_limit_remaining,
            &mut content,
            vec![TaggedInternalOp::own(internal, 0)],
        );
        assert_eq!(outcome, Err(ApplyOperationError::OperationQuotaExceeded));

        // The returned internal must be the pre-burn one: no
        // storage-fee balance_updates survived the clone-then-rebuild
        // rollback.
        let InternalOperationSum::Transfer(inner) = &internals[0] else {
            panic!("expected internal Transfer");
        };
        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &inner.result
        else {
            panic!("expected Applied internal target");
        };
        assert!(
            success.balance_updates.is_empty(),
            "internal must carry no storage-fee balance_updates on rollback",
        );
    }

    /// [`burn_pass`]: two internals compete for a shared budget that
    /// fits the first slot burn but not the second. The
    /// clone-then-rebuild pass discards the first internal's
    /// half-applied burn so no storage-fees / payer-debit entry
    /// survives on any internal.
    #[test]
    fn burn_pass_two_internals_share_budget_with_rollback() {
        let mut host = MockKernelHost::default();
        let pkh = PublicKeyHash::from_b58check(PAYER_PKH).unwrap();
        let payer = init_account(&mut host, &pkh, 10_000_000);
        let make_internal = || {
            TaggedInternalOp::own(
                InternalOperationSum::Transfer(InternalContentWithMetadata {
                    sender: payer.contract(),
                    nonce: 0,
                    content: TransferContent {
                        amount: 0_u64.into(),
                        destination: payer.contract(),
                        parameters: Parameters::default(),
                    },
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess {
                            allocated_destination_contract: true,
                            ..Default::default()
                        },
                    )),
                }),
                0,
            )
        };
        let mut content: ContentResult<TransferContent> =
            ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess::default()));
        // Budget fits one slot (257) but not two (514): remainder
        // after internal 0 is `2 * ORIGINATION_SIZE - 1 - 257 == 256`,
        // and internal 1 needs 257.
        let mut storage_limit_remaining =
            num_bigint::BigUint::from(2 * ORIGINATION_SIZE - 1);
        let (internals, outcome) = burn_pass::<_, TransferContent, _>(
            &mut host,
            &payer,
            &mut storage_limit_remaining,
            &mut content,
            vec![make_internal(), make_internal()],
        );
        assert_eq!(outcome, Err(ApplyOperationError::OperationQuotaExceeded));

        assert_eq!(internals.len(), 2);
        let payer_contract = payer.contract();
        for internal in &internals {
            let InternalOperationSum::Transfer(inner) = internal else {
                panic!("expected internal Transfer");
            };
            let ContentResult::Applied(TransferTarget::ToContrat(success)) =
                &inner.result
            else {
                panic!("expected Applied internal target");
            };
            assert!(
                success.balance_updates.iter().all(|bu| {
                    !matches!(bu.balance, Balance::StorageFees)
                        && !matches!(&bu.balance, Balance::Account(c) if c == &payer_contract)
                }),
                "no storage-fees / payer-debit entry must survive on a rolled-back internal",
            );
        }
    }

    /// [`burn_pass`]: an internal op tagged `Crac` is left untouched —
    /// no `balance_updates` are appended on it, and its
    /// `paid_storage_size_diff` does not consume the shared
    /// `storage_limit`. An `Own` entry sitting next to it is burned
    /// as usual. The vec order is preserved.
    #[test]
    fn burn_pass_skips_crac_tagged_entries() {
        let mut host = MockKernelHost::default();
        let pkh = PublicKeyHash::from_b58check(PAYER_PKH).unwrap();
        let payer = init_account(&mut host, &pkh, 100_000);

        let own_op = InternalOperationSum::Transfer(InternalContentWithMetadata {
            sender: payer.contract(),
            nonce: 0,
            content: TransferContent {
                amount: 0_u64.into(),
                destination: payer.contract(),
                parameters: Parameters::default(),
            },
            result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                paid_storage_size_diff: 4_u64.into(),
                ..Default::default()
            })),
        });
        let crac_op = InternalOperationSum::Transfer(InternalContentWithMetadata {
            sender: payer.contract(),
            nonce: 1,
            content: TransferContent {
                amount: 0_u64.into(),
                destination: payer.contract(),
                parameters: Parameters::default(),
            },
            result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                // Non-zero diff is the worst case: a buggy burn would
                // try to charge 7 bytes and consume from the budget.
                paid_storage_size_diff: 7_u64.into(),
                ..Default::default()
            })),
        });
        let tagged = vec![
            TaggedInternalOp::own(own_op, 0),
            TaggedInternalOp::from_crac(crac_op),
        ];
        let mut content: ContentResult<TransferContent> =
            ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess::default()));

        // Storage limit deliberately tight: 4 bytes is just enough for
        // the Own internal alone. A buggy burn that also charged the
        // Crac internal (7 bytes) would overshoot and return an error.
        let mut storage_limit_remaining = num_bigint::BigUint::from(4_u64);
        let (internals, outcome) = burn_pass::<_, TransferContent, _>(
            &mut host,
            &payer,
            &mut storage_limit_remaining,
            &mut content,
            tagged,
        );
        outcome.expect("Own internal must be burned, Crac internal must be skipped");

        // (a) The Own internal got its `Debited / Credited(StorageFees)`
        // pair, charging 4 × COST_PER_BYTES against the payer's balance.
        let own_burn = 4 * COST_PER_BYTES;
        assert_eq!(payer.balance(&host).unwrap(), (100_000 - own_burn).into());

        assert_eq!(internals.len(), 2);

        // (b) The Own internal's body carries the storage-fee pair.
        let InternalOperationSum::Transfer(own_after) = &internals[0] else {
            panic!("expected Own internal at index 0");
        };
        let ContentResult::Applied(TransferTarget::ToContrat(own_success)) =
            &own_after.result
        else {
            panic!("expected Applied Own");
        };
        assert_eq!(own_success.balance_updates.len(), 2);

        // (c) The Crac internal is intact: empty balance_updates.
        let InternalOperationSum::Transfer(crac_after) = &internals[1] else {
            panic!("expected Crac internal at index 1");
        };
        let ContentResult::Applied(TransferTarget::ToContrat(crac_success)) =
            &crac_after.result
        else {
            panic!("expected Applied Crac");
        };
        assert!(
            crac_success.balance_updates.is_empty(),
            "Crac internal must not have storage-fee balance_updates"
        );

        // (d) The vec order is preserved (Own first, Crac second).
        assert_eq!(own_after.nonce, 0);
        assert_eq!(crac_after.nonce, 1);
    }
}
