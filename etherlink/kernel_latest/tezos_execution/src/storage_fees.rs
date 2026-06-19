// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Storage-fee accounting: charges the cost of the storage
//! allocated by a manager operation to its source and records
//! the matching balance updates on the receipt.

use num_bigint::{BigInt, BigUint, TryFromBigIntError};
use num_traits::{ops::checked::CheckedSub, Zero};
use tezos_data_encoding::types::Zarith;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_tezlink::operation::{OriginationContent, RevealContent, TransferContent};
use tezos_tezlink::operation_result::{
    ApplyOperationError, Balance, BalanceUpdate, ContentResult, EventContent,
    EventSuccess, InternalContentWithMetadata, InternalOperationSum, OperationKind,
    OriginationSuccess, RevealSuccess, TransferError, TransferTarget, UpdateOrigin,
};

use crate::account_storage::TezlinkAccount;
use crate::burn_tez;

pub const COST_PER_BYTES: u64 = 1;
// Values from src/proto_023_PtSeouLo/lib_parameters/default_parameters.ml.
pub const ORIGINATION_SIZE: u64 = 257;
pub const HARD_STORAGE_LIMIT_PER_OPERATION: u64 = 60_000;

/// A storage fee in mutez.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct StorageFee(BigUint);

impl From<StorageFee> for BigUint {
    fn from(val: StorageFee) -> Self {
        val.0
    }
}

/// Burns the cost of `nb_consumed_bytes` bytes from the payer and
/// returns the cost. Subtracts `consumed_bytes` from
/// `storage_limit_remaining`, refusing to go negative.
fn burn_storage_fee<Host: StorageV1>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    storage_limit_remaining: &mut BigUint,
    nb_consumed_bytes: &Zarith,
) -> Result<StorageFee, ApplyOperationError> {
    let nb_consumed_bytes = BigUint::try_from(&nb_consumed_bytes.0).map_err(
        |e: num_bigint::TryFromBigIntError<()>| {
            ApplyOperationError::Transfer(TransferError::StorageFeesConversion(
                e.to_string(),
            ))
        },
    )?;

    *storage_limit_remaining = storage_limit_remaining
        .checked_sub(&nb_consumed_bytes)
        .ok_or(ApplyOperationError::OperationQuotaExceeded)?;

    let to_burn = BigUint::from(COST_PER_BYTES) * nb_consumed_bytes;
    if to_burn.is_zero() {
        return Ok(StorageFee::default());
    }
    burn_tez(host, payer, &to_burn).map_err(|err| match err {
        TransferError::BalanceTooLow(btl) => {
            ApplyOperationError::CannotPayStorageFee(btl)
        }
        other => ApplyOperationError::Transfer(other),
    })?;
    Ok(StorageFee(to_burn))
}

/// Prepares balance updates when accounting storage fees in the format expected by the Tezos operation.
fn compute_storage_balance_updates(
    source_contract: Contract,
    fee: BigUint,
) -> Result<Vec<BalanceUpdate>, TryFromBigIntError<BigInt>> {
    if fee.is_zero() {
        return Ok(vec![]);
    };
    let source_delta = BigInt::from_biguint(num_bigint::Sign::Minus, fee.clone());
    let block_fees = BigInt::from_biguint(num_bigint::Sign::Plus, fee);

    let source_update = BalanceUpdate {
        balance: Balance::Account(source_contract),
        changes: source_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    let block_fees = BalanceUpdate {
        balance: Balance::StorageFees,
        changes: block_fees.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    Ok(vec![source_update, block_fees])
}

/// Append the `Debited(payer) / Credited(StorageFees)` pair for a
/// delegated storage cost to a top-level content's
/// `balance_updates`, going through
/// [`OperationStorageFees::extend_success_balance_updates`]. No-op
/// on `mutez_cost == 0`, on non-Applied content, and on kinds whose
/// success carries no `balance_updates` field ([`RevealSuccess`],
/// [`EventSuccess`]).
pub fn add_delegated_storage_fee_balance_updates_to_content<M: OperationStorageFees>(
    payer: &impl TezlinkAccount,
    content: &mut ContentResult<M>,
    mutez_cost: u64,
) -> Result<(), ApplyOperationError> {
    if mutez_cost == 0 {
        return Ok(());
    }
    if let ContentResult::Applied(success) = content {
        let pair =
            compute_storage_balance_updates(payer.contract(), BigUint::from(mutez_cost))
                .map_err(|e| {
                    ApplyOperationError::Transfer(
                        TransferError::FailedToComputeBalanceUpdate(e.to_string()),
                    )
                })?;
        M::extend_success_balance_updates(success, pair);
    }
    Ok(())
}

/// Append the `Debited(payer) / Credited(StorageFees)` pair for a
/// delegated storage cost to the operation's `balance_updates`,
/// dispatching on its [`InternalOperationSum`] variant. No-op on
/// `mutez_cost == 0`, on non-Applied content, and on variants
/// whose success carries no `balance_updates` field
/// ([`EventSuccess`]).
pub fn add_delegated_storage_fee_balance_updates(
    payer: &impl TezlinkAccount,
    op: &mut InternalOperationSum,
    mutez_cost: u64,
) -> Result<(), ApplyOperationError> {
    match op {
        InternalOperationSum::Transfer(meta) => {
            add_delegated_storage_fee_balance_updates_to_content(
                payer,
                &mut meta.result,
                mutez_cost,
            )
        }
        InternalOperationSum::Origination(meta) => {
            add_delegated_storage_fee_balance_updates_to_content(
                payer,
                &mut meta.result,
                mutez_cost,
            )
        }
        InternalOperationSum::Event(meta) => {
            add_delegated_storage_fee_balance_updates_to_content(
                payer,
                &mut meta.result,
                mutez_cost,
            )
        }
    }
}

/// Returns the mutez cost of allocating `nb_consumed_bytes`:
/// `nb_consumed_bytes × COST_PER_BYTES`. Pure: no payer debit, no
/// `storage_limit` consumption, no host access. Errors on a
/// negative-sign `Zarith` (cannot happen on a receipt's
/// `paid_storage_size_diff`).
fn storage_fee_for_bytes(nb_consumed_bytes: &Zarith) -> Result<BigUint, TransferError> {
    let bytes = BigUint::try_from(&nb_consumed_bytes.0)
        .map_err(|e| TransferError::StorageFeesConversion(e.to_string()))?;
    Ok(BigUint::from(COST_PER_BYTES) * bytes)
}

/// Charge `mutez_cost` against the payer.
///
/// - Deducts `mutez_cost / COST_PER_BYTES` from `storage_limit_remaining`;
///   overshoot raises `OperationQuotaExceeded`.
/// - Burns the mutez against the payer's balance; insufficient balance raises
///   `CannotPayStorageFee`.
///
/// No-op on `mutez_cost == 0`.
pub fn burn_storage_cost<Host: StorageV1>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    storage_limit_remaining: &mut BigUint,
    mutez_cost: u64,
) -> Result<(), ApplyOperationError> {
    if mutez_cost == 0 {
        return Ok(());
    }

    let nb_consumed_bytes = BigUint::from(mutez_cost / COST_PER_BYTES);

    *storage_limit_remaining = storage_limit_remaining
        .checked_sub(&nb_consumed_bytes)
        .ok_or(ApplyOperationError::OperationQuotaExceeded)?;

    let to_burn = BigUint::from(mutez_cost);
    burn_tez(host, payer, &to_burn).map_err(|err| match err {
        TransferError::BalanceTooLow(btl) => {
            ApplyOperationError::CannotPayStorageFee(btl)
        }
        other => ApplyOperationError::Transfer(other),
    })?;
    Ok(())
}

/// Charge the payer for the storage allocated by `success`: walk the
/// per-kind byte-burn sequence (debiting the payer and decrementing
/// `storage_limit_remaining` at each step), then render the
/// resulting balance-update pairs on `success.balance_updates` in
/// the per-kind display order.
pub fn burn_storage_fees<Host, M, A>(
    host: &mut Host,
    payer: &A,
    storage_limit_remaining: &mut BigUint,
    success: &mut M::Success,
) -> Result<(), ApplyOperationError>
where
    Host: StorageV1,
    M: OperationStorageFees,
    A: TezlinkAccount,
{
    let storage_fees = M::build_storage_fees(success, |bytes| {
        burn_storage_fee(host, payer, storage_limit_remaining, bytes)
    })?;
    M::write_balance_updates(success, storage_fees, |fee| {
        compute_storage_balance_updates(payer.contract(), fee.into()).map_err(|e| {
            ApplyOperationError::Transfer(TransferError::FailedToComputeBalanceUpdate(
                e.to_string(),
            ))
        })
    })?;
    Ok(())
}

/// Charge the payer for the storage allocated by `content`, when its
/// status is `Applied`. No-op on every other status: a non-Applied
/// content result records work that did not allocate anything (Failed,
/// Skipped) or that has been rolled back (BackTracked).
pub fn burn_content_storage_fees<Host: StorageV1, M: OperationStorageFees>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    storage_limit_remaining: &mut BigUint,
    content: &mut ContentResult<M>,
) -> Result<(), ApplyOperationError> {
    match content {
        ContentResult::Applied(success) => {
            burn_storage_fees::<_, M, _>(host, payer, storage_limit_remaining, success)
        }
        _ => Ok(()),
    }
}

fn burn_internal_meta_storage_fees<Host: StorageV1, M: OperationStorageFees>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    storage_limit_remaining: &mut BigUint,
    meta: &mut InternalContentWithMetadata<M>,
) -> Result<(), ApplyOperationError> {
    burn_content_storage_fees(host, payer, storage_limit_remaining, &mut meta.result)
}

/// Charge the payer for the storage allocated by a single internal
/// operation, dispatching on its [`InternalOperationSum`] variant.
pub fn burn_internal_op_storage_fees<Host: StorageV1>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    storage_limit_remaining: &mut BigUint,
    op: &mut InternalOperationSum,
) -> Result<(), ApplyOperationError> {
    match op {
        InternalOperationSum::Transfer(meta) => {
            burn_internal_meta_storage_fees(host, payer, storage_limit_remaining, meta)
        }
        InternalOperationSum::Origination(meta) => {
            burn_internal_meta_storage_fees(host, payer, storage_limit_remaining, meta)
        }
        InternalOperationSum::Event(meta) => {
            burn_internal_meta_storage_fees(host, payer, storage_limit_remaining, meta)
        }
    }
}

pub fn compute_storage_fees<M: OperationStorageFees>(
    success: &M::Success,
) -> Result<BigUint, TransferError> {
    let mut total = BigUint::ZERO;
    let _: M::StorageFees = M::build_storage_fees(success, |bytes| {
        let fee = storage_fee_for_bytes(bytes)?;
        total += &fee;
        Ok::<StorageFee, TransferError>(StorageFee(fee))
    })?;
    Ok(total)
}

fn compute_content_storage_fees<M: OperationStorageFees>(
    content: &ContentResult<M>,
) -> Result<BigUint, TransferError> {
    match content {
        ContentResult::Applied(success) => compute_storage_fees::<M>(success),
        _ => Ok(BigUint::ZERO),
    }
}

fn compute_internal_meta_storage_fees<M: OperationStorageFees>(
    meta: &InternalContentWithMetadata<M>,
) -> Result<BigUint, TransferError> {
    compute_content_storage_fees(&meta.result)
}

/// Sum the per-kind storage-fee components of a single internal
/// operation, dispatching on its [`InternalOperationSum`] variant.
pub fn compute_internal_op_storage_fees(
    op: &InternalOperationSum,
) -> Result<BigUint, TransferError> {
    match op {
        InternalOperationSum::Transfer(meta) => compute_internal_meta_storage_fees(meta),
        InternalOperationSum::Origination(meta) => {
            compute_internal_meta_storage_fees(meta)
        }
        InternalOperationSum::Event(meta) => compute_internal_meta_storage_fees(meta),
    }
}

/// Per-`OperationKind` storage-fee structure. Each implementation
/// describes the storage-fee components of a successful operation of
/// that kind: which components exist (and their byte counts), the
/// order in which they are enumerated, and the order in which the
/// corresponding balance updates are spliced into the success's
/// pre-existing `balance_updates`.
///
/// The receiver (`success`) is reached only when the operation's
/// [`ContentResult`] is `Applied`; non-Applied content results are
/// skipped at the dispatch layer, so implementations never see them.
pub trait OperationStorageFees: OperationKind {
    /// The kind-specific structured costs.
    type StorageFees;

    /// Enumerate the per-kind storage-fee components of `success`,
    /// in a kind-specific order. For each component, call
    /// `build_storage_fee` with its byte count to obtain a
    /// [`StorageFee`], then pack the assembled fees into
    /// [`Self::StorageFees`] and return it. The first error from
    /// `build_storage_fee` short-circuits the walk.
    fn build_storage_fees<F, E>(
        success: &Self::Success,
        build_storage_fee: F,
    ) -> Result<Self::StorageFees, E>
    where
        F: FnMut(&Zarith) -> Result<StorageFee, E>;

    /// Render `storage_fees` onto `success.balance_updates`. Each
    /// [`StorageFee`] is turned into balance updates via
    /// `fee_to_updates`, the success's pre-existing
    /// `balance_updates` are spliced at the kind-specific position,
    /// and the assembled list is written back to
    /// `success.balance_updates`.
    fn write_balance_updates<F, E>(
        success: &mut Self::Success,
        storage_fees: Self::StorageFees,
        fee_to_updates: F,
    ) -> Result<(), E>
    where
        F: Fn(StorageFee) -> Result<Vec<BalanceUpdate>, E>;

    /// Append `pair` to `success.balance_updates` when the kind's
    /// success carries one. No-op for kinds whose success has no
    /// such field ([`RevealSuccess`], [`EventSuccess`]).
    fn extend_success_balance_updates(
        success: &mut Self::Success,
        pair: Vec<BalanceUpdate>,
    );
}

pub struct TransferStorageFees {
    pub content: StorageFee,
    /// `None` iff the transfer did not freshly allocate its
    /// destination contract.
    pub slot: Option<StorageFee>,
}

impl OperationStorageFees for TransferContent {
    type StorageFees = TransferStorageFees;

    fn build_storage_fees<F, E>(
        success: &TransferTarget,
        mut build_storage_fee: F,
    ) -> Result<TransferStorageFees, E>
    where
        F: FnMut(&Zarith) -> Result<StorageFee, E>,
    {
        let TransferTarget::ToContrat(success) = success;
        let content = build_storage_fee(&success.paid_storage_size_diff)?;
        let slot = if success.allocated_destination_contract {
            Some(build_storage_fee(&ORIGINATION_SIZE.into())?)
        } else {
            None
        };
        Ok(TransferStorageFees { content, slot })
    }

    fn write_balance_updates<F, E>(
        success: &mut TransferTarget,
        storage_fees: TransferStorageFees,
        fee_to_updates: F,
    ) -> Result<(), E>
    where
        F: Fn(StorageFee) -> Result<Vec<BalanceUpdate>, E>,
    {
        let TransferTarget::ToContrat(success) = success;
        let mut updates = fee_to_updates(storage_fees.content)?;
        updates.extend(success.balance_updates.iter().cloned());
        if let Some(slot) = storage_fees.slot {
            updates.extend(fee_to_updates(slot)?);
        }
        success.balance_updates = updates;
        Ok(())
    }

    fn extend_success_balance_updates(
        success: &mut TransferTarget,
        pair: Vec<BalanceUpdate>,
    ) {
        let TransferTarget::ToContrat(success) = success;
        success.balance_updates.extend(pair);
    }
}

pub struct OriginationStorageFees {
    pub content: StorageFee,
    pub slot: StorageFee,
}

impl OperationStorageFees for OriginationContent {
    type StorageFees = OriginationStorageFees;

    fn build_storage_fees<F, E>(
        success: &OriginationSuccess,
        mut build_storage_fee: F,
    ) -> Result<OriginationStorageFees, E>
    where
        F: FnMut(&Zarith) -> Result<StorageFee, E>,
    {
        let content = build_storage_fee(&success.paid_storage_size_diff)?;
        let slot = build_storage_fee(&ORIGINATION_SIZE.into())?;
        Ok(OriginationStorageFees { content, slot })
    }

    fn write_balance_updates<F, E>(
        success: &mut OriginationSuccess,
        storage_fees: OriginationStorageFees,
        fee_to_updates: F,
    ) -> Result<(), E>
    where
        F: Fn(StorageFee) -> Result<Vec<BalanceUpdate>, E>,
    {
        let mut updates = fee_to_updates(storage_fees.content)?;
        updates.extend(fee_to_updates(storage_fees.slot)?);
        updates.extend(success.balance_updates.iter().cloned());
        success.balance_updates = updates;
        Ok(())
    }

    fn extend_success_balance_updates(
        success: &mut OriginationSuccess,
        pair: Vec<BalanceUpdate>,
    ) {
        success.balance_updates.extend(pair);
    }
}

impl OperationStorageFees for RevealContent {
    type StorageFees = ();

    fn build_storage_fees<F, E>(_: &RevealSuccess, _: F) -> Result<(), E>
    where
        F: FnMut(&Zarith) -> Result<StorageFee, E>,
    {
        Ok(())
    }

    fn write_balance_updates<F, E>(_: &mut RevealSuccess, _: (), _: F) -> Result<(), E>
    where
        F: Fn(StorageFee) -> Result<Vec<BalanceUpdate>, E>,
    {
        Ok(())
    }

    fn extend_success_balance_updates(_: &mut RevealSuccess, _: Vec<BalanceUpdate>) {}
}

impl OperationStorageFees for EventContent {
    type StorageFees = ();

    fn build_storage_fees<F, E>(_: &EventSuccess, _: F) -> Result<(), E>
    where
        F: FnMut(&Zarith) -> Result<StorageFee, E>,
    {
        Ok(())
    }

    fn write_balance_updates<F, E>(_: &mut EventSuccess, _: (), _: F) -> Result<(), E>
    where
        F: Fn(StorageFee) -> Result<Vec<BalanceUpdate>, E>,
    {
        Ok(())
    }

    fn extend_success_balance_updates(_: &mut EventSuccess, _: Vec<BalanceUpdate>) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_crypto_rs::hash::ContractKt1Hash;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup::types::PublicKeyHash;
    use tezos_tezlink::operation::{
        OriginationContent, Parameters, Script, TransferContent,
    };
    use tezos_tezlink::operation_result::{
        ApplyOperationErrors, Balance, BalanceTooLow, BalanceUpdate,
        InternalContentWithMetadata, Originated, RevealSuccess, TransferSuccess,
        UpdateOrigin,
    };

    use crate::account_storage::TezosImplicitAccount;
    use crate::account_storage::TezosImplicitAccountTrait;
    use crate::context;

    const SOURCE_PKH: &str = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
    const ORIGINATED_KT1: &str = "KT1EFxv88KpjxzGNu1ozh9Vta4BaV3psNknp";
    const SLOT_BURN: u64 = ORIGINATION_SIZE * COST_PER_BYTES;

    fn origination_success(paid_storage_size_diff: u64) -> OriginationSuccess {
        OriginationSuccess {
            balance_updates: vec![],
            originated_contracts: vec![Originated {
                contract: ContractKt1Hash::from_base58_check(ORIGINATED_KT1).unwrap(),
            }],
            consumed_milligas: 0_u64.into(),
            storage_size: paid_storage_size_diff.into(),
            paid_storage_size_diff: paid_storage_size_diff.into(),
            lazy_storage_diff: None,
        }
    }

    fn applied_origination(
        success: OriginationSuccess,
    ) -> ContentResult<OriginationContent> {
        ContentResult::Applied(success)
    }

    fn init_payer(host: &mut MockKernelHost, balance: u64) -> TezosImplicitAccount {
        let pkh = PublicKeyHash::from_b58check(SOURCE_PKH).unwrap();
        let account = context::implicit_from_public_key_hash(&pkh).unwrap();
        account.allocate(host).unwrap();
        account.set_balance(host, &balance.into()).unwrap();
        account
    }

    /// Test wrapper around [`burn_content_storage_fees`] that builds
    /// the `storage_limit_remaining` accumulator from a `u64`.
    fn burn_content<M: OperationStorageFees>(
        host: &mut MockKernelHost,
        payer: &impl TezlinkAccount,
        storage_limit: u64,
        content: &mut ContentResult<M>,
    ) -> Result<(), ApplyOperationError> {
        let mut storage_limit_remaining = BigUint::from(storage_limit);
        burn_content_storage_fees(host, payer, &mut storage_limit_remaining, content)
    }

    /// Test wrapper that exercises the byte-level primitive
    /// [`burn_bytes`] and converts the resulting [`StorageFee`] into
    /// the legacy `Vec<BalanceUpdate>` shape these tests assert on.
    fn burn_storage_fee<Host: StorageV1>(
        host: &mut Host,
        payer: &impl TezlinkAccount,
        storage_limit_remaining: &mut BigUint,
        nb_consumed_bytes: &Zarith,
    ) -> Result<Vec<BalanceUpdate>, ApplyOperationError> {
        let fee = super::burn_storage_fee(
            host,
            payer,
            storage_limit_remaining,
            nb_consumed_bytes,
        )?;
        compute_storage_balance_updates(payer.contract(), fee.into()).map_err(|e| {
            ApplyOperationError::Transfer(TransferError::FailedToComputeBalanceUpdate(
                e.to_string(),
            ))
        })
    }

    /// Test wrapper around [`burn_internal_op_storage_fees`] that
    /// builds the `storage_limit_remaining` accumulator from a `u64`.
    fn burn_internal_op(
        host: &mut MockKernelHost,
        payer: &impl TezlinkAccount,
        storage_limit: u64,
        op: &mut InternalOperationSum,
    ) -> Result<(), ApplyOperationError> {
        let mut storage_limit_remaining = BigUint::from(storage_limit);
        burn_internal_op_storage_fees(host, payer, &mut storage_limit_remaining, op)
    }

    /// On a zero-byte input, `burn_storage_fee` returns no balance
    /// updates and does not touch the payer's balance.
    #[test]
    fn burn_storage_fee_zero_bytes_is_noop() {
        let mut host = MockKernelHost::default();
        let initial_balance = 50_u64;
        let payer = init_payer(&mut host, initial_balance);

        let updates =
            burn_storage_fee(&mut host, &payer, &mut BigUint::from(u64::MAX), &0.into())
                .expect("zero-byte burn cannot fail");

        assert!(updates.is_empty(), "zero-byte burn must emit no updates");
        assert_eq!(
            payer.balance(&host).expect("read balance"),
            initial_balance.into(),
            "payer balance must be unchanged",
        );
    }

    /// A non-zero burn debits `nb_consumed_bytes × COST_PER_BYTES` from the
    /// payer and emits the matching `Debited(payer) /
    /// Credited(StorageFees)` pair.
    #[test]
    fn burn_storage_fee_emits_balance_pair() {
        let mut host = MockKernelHost::default();
        let initial_balance = 100_000_u64;
        let nb_consumed_bytes = 10_u64;
        let payer = init_payer(&mut host, initial_balance);

        let updates = burn_storage_fee(
            &mut host,
            &payer,
            &mut BigUint::from(u64::MAX),
            &nb_consumed_bytes.into(),
        )
        .expect("non-zero burn against funded payer must succeed");

        let expected_to_burn = nb_consumed_bytes * COST_PER_BYTES;
        assert_eq!(updates.len(), 2, "expected exactly two balance updates");
        assert_eq!(updates[0].balance, Balance::Account(payer.contract()));
        assert_eq!(updates[0].changes, -(expected_to_burn as i64));
        assert_eq!(updates[1].balance, Balance::StorageFees);
        assert_eq!(updates[1].changes, expected_to_burn as i64);
        assert_eq!(
            payer.balance(&host).expect("read balance"),
            (initial_balance - expected_to_burn).into(),
            "payer balance must drop by the burn amount",
        );
    }

    /// When the payer's balance does not cover the burn, the helper
    /// returns a single [`ApplyOperationError::CannotPayStorageFee`]
    /// carrying the underlying `BalanceTooLow` with the payer's exact
    /// balance and the burn amount.
    #[test]
    fn burn_storage_fee_insolvent_returns_cannot_pay() {
        let mut host = MockKernelHost::default();
        // Cost = 10 × COST_PER_BYTES; balance is 5, strictly below.
        let initial_balance = 5_u64;
        let nb_consumed_bytes = 10_u64;
        let payer = init_payer(&mut host, initial_balance);

        let err = burn_storage_fee(
            &mut host,
            &payer,
            &mut BigUint::from(u64::MAX),
            &nb_consumed_bytes.into(),
        )
        .expect_err("burn must fail when payer is insolvent");

        match err {
            ApplyOperationError::CannotPayStorageFee(b) => {
                assert_eq!(b.contract, payer.contract());
                assert_eq!(b.balance, initial_balance.into());
                assert_eq!(b.amount, (nb_consumed_bytes * COST_PER_BYTES).into());
            }
            other => panic!("expected CannotPayStorageFee(_), got {other:?}"),
        }
    }

    fn applied_transfer(success: TransferSuccess) -> ContentResult<TransferContent> {
        ContentResult::Applied(TransferTarget::ToContrat(success))
    }

    /// A successful Reveal allocates no storage and leaves the payer
    /// balance untouched.
    #[test]
    fn applied_reveal_is_a_noop() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000);
        let mut content: ContentResult<RevealContent> =
            ContentResult::Applied(RevealSuccess {
                consumed_milligas: 0_u64.into(),
            });
        burn_content(&mut host, &payer, u64::MAX, &mut content).unwrap();
        assert_eq!(payer.balance(&host).unwrap(), 1_000_u64.into());
    }

    /// A Transfer with `paid_storage_size_diff > 0` debits the payer
    /// `bytes × COST_PER_BYTES` mutez and appends the
    /// `Debited(payer) / Credited(StorageFees)` pair to the
    /// receipt's inner `balance_updates`.
    #[test]
    fn transfer_with_storage_growth_fires_variable_burn() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 100_000);
        let mut content = applied_transfer(TransferSuccess {
            paid_storage_size_diff: 10_u64.into(),
            ..Default::default()
        });

        burn_content(&mut host, &payer, u64::MAX, &mut content).unwrap();

        let burn = 10 * COST_PER_BYTES;
        assert_eq!(
            payer.balance(&host).unwrap(),
            (100_000 - burn).into(),
            "payer must drop by the burn amount"
        );
        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &content else {
            panic!("expected Applied target");
        };

        assert_eq!(
            success.balance_updates,
            vec![
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(burn as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (burn as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ]
        );
    }

    /// A Transfer with `paid_storage_size_diff == 0` short-circuits:
    /// no balance update is added and the payer is not touched.
    #[test]
    fn transfer_with_no_storage_growth_is_a_noop() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000);
        let mut content = applied_transfer(TransferSuccess::default());
        burn_content(&mut host, &payer, u64::MAX, &mut content).unwrap();
        assert_eq!(payer.balance(&host).unwrap(), 1_000_u64.into());
        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &content else {
            panic!("expected Applied target");
        };
        assert!(success.balance_updates.is_empty());
    }

    /// A non-Applied top-level (here Failed) does not trigger any
    /// burn — the body is not a successful operation.
    #[test]
    fn failed_top_level_does_not_burn() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000);
        let mut content: ContentResult<TransferContent> =
            ContentResult::Failed(ApplyOperationErrors { errors: vec![] });
        burn_content(&mut host, &payer, u64::MAX, &mut content).unwrap();
        assert_eq!(payer.balance(&host).unwrap(), 1_000_u64.into());
    }

    /// Insolvent payer for the top-level burn: returns a single
    /// [`ApplyOperationError::CannotPayStorageFee`] carrying the
    /// underlying `BalanceTooLow`.
    #[test]
    fn insolvent_payer_returns_cannot_pay() {
        let mut host = MockKernelHost::default();
        // Burn cost = 10 × COST_PER_BYTES; balance is 5, strictly below.
        let payer = init_payer(&mut host, 5);
        let mut content = applied_transfer(TransferSuccess {
            paid_storage_size_diff: 10_u64.into(),
            ..Default::default()
        });
        let err = burn_content(&mut host, &payer, u64::MAX, &mut content)
            .expect_err("burn must fail when payer is insolvent");

        assert_eq!(
            err,
            ApplyOperationError::CannotPayStorageFee(BalanceTooLow {
                contract: payer.contract(),
                balance: 5_u64.into(),
                amount: (10 * COST_PER_BYTES).into(),
            }),
        );
    }

    /// An `Applied` internal Transfer with storage growth fires its
    /// own burn against the same payer; the burn pair lands on the
    /// internal op's success body, not the top-level.
    #[test]
    fn applied_internal_transfer_fires_its_own_burn() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 100_000);
        let mut op = InternalOperationSum::Transfer(InternalContentWithMetadata {
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

        burn_internal_op(&mut host, &payer, u64::MAX, &mut op).unwrap();

        let burn = 4 * COST_PER_BYTES;
        assert_eq!(payer.balance(&host).unwrap(), (100_000 - burn).into());

        let InternalOperationSum::Transfer(inner) = &op else {
            panic!("expected internal Transfer");
        };
        let ContentResult::Applied(TransferTarget::ToContrat(s)) = &inner.result else {
            panic!("expected Applied internal target");
        };

        assert_eq!(
            s.balance_updates,
            vec![
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(burn as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (burn as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ]
        );
    }

    /// An Origination with `paid_storage_size_diff > 0` fires the
    /// double burn: variable on `paid_storage_size_diff` then the
    /// fixed slot burn. The receipt's `balance_updates` is extended
    /// by both pairs (4 entries total).
    #[test]
    fn origination_with_storage_growth_fires_double_burn() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        let mut content = applied_origination(origination_success(38));

        burn_content(&mut host, &payer, u64::MAX, &mut content).unwrap();

        let variable = 38 * COST_PER_BYTES;
        let total = variable + SLOT_BURN;
        assert_eq!(payer.balance(&host).unwrap(), (1_000_000 - total).into());
        let ContentResult::Applied(success) = &content else {
            panic!("expected Applied origination");
        };

        assert_eq!(
            success.balance_updates,
            vec![
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(variable as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (variable as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ]
        );
    }

    /// An Origination with `paid_storage_size_diff == 0` short-circuits
    /// the variable burn (no balance update added), but the fixed slot
    /// burn still fires.
    #[test]
    fn origination_with_no_growth_only_fires_slot_burn() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        let mut content = applied_origination(origination_success(0));

        burn_content(&mut host, &payer, u64::MAX, &mut content).unwrap();

        assert_eq!(
            payer.balance(&host).unwrap(),
            (1_000_000 - SLOT_BURN).into()
        );
        let ContentResult::Applied(success) = &content else {
            panic!("expected Applied origination");
        };

        assert_eq!(
            success.balance_updates,
            vec![
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ]
        );
    }

    /// Insolvent payer on the slot burn (after the variable burn
    /// succeeded): the L1-shaped trace is returned and the success
    /// body's `balance_updates` is left in its pre-burn state — no
    /// half-applied burn pair survives in the receipt. The variable
    /// burn was applied to the payer's balance; SafeStorage revert
    /// at the caller level is what undoes that state effect.
    #[test]
    fn origination_insolvent_on_slot_burn_returns_cannot_pay() {
        let mut host = MockKernelHost::default();
        // Cover variable burn (38 × COST_PER_BYTES = 38) but not the
        // slot burn (SLOT_BURN = 257). 100 - 38 = 62 remaining < 257.
        let payer = init_payer(&mut host, 100);
        let mut content = applied_origination(origination_success(38));

        let err = burn_content(&mut host, &payer, u64::MAX, &mut content)
            .expect_err("slot burn must fail when payer is insolvent");

        assert_eq!(
            err,
            ApplyOperationError::CannotPayStorageFee(BalanceTooLow {
                contract: payer.contract(),
                balance: 62_u64.into(),
                amount: SLOT_BURN.into(),
            }),
        );

        let ContentResult::Applied(success) = &content else {
            panic!("expected Applied origination");
        };

        assert!(
            success.balance_updates.is_empty(),
            "atomic-assignment: the variable burn pair must not land on \
             the success body when the slot burn fails (got {:?})",
            success.balance_updates
        );
    }

    /// An `Applied` internal Origination fires the double burn
    /// against the same payer; the burn pairs land on the internal
    /// op's success body.
    #[test]
    fn applied_internal_origination_fires_double_burn() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        let mut op = InternalOperationSum::Origination(InternalContentWithMetadata {
            sender: payer.contract(),
            nonce: 0,
            content: OriginationContent {
                balance: 0_u64.into(),
                delegate: None,
                script: Script {
                    code: vec![],
                    storage: vec![],
                },
            },
            result: ContentResult::Applied(origination_success(10)),
        });

        burn_internal_op(&mut host, &payer, u64::MAX, &mut op).unwrap();

        let variable = 10 * COST_PER_BYTES;
        let total = variable + SLOT_BURN;
        assert_eq!(payer.balance(&host).unwrap(), (1_000_000 - total).into());

        let InternalOperationSum::Origination(inner) = &op else {
            panic!("expected internal Origination");
        };
        let ContentResult::Applied(success) = &inner.result else {
            panic!("expected Applied internal origination");
        };

        assert_eq!(
            success.balance_updates,
            vec![
                // Variable burn (paid_storage_size_diff = 10).
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(variable as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (variable as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                // Slot burn.
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ]
        );
    }

    /// A Transfer that freshly allocated an implicit destination
    /// (`allocated_destination_contract: true`) without storage
    /// growth fires only the slot burn — variable burn short-circuits
    /// on the zero `paid_storage_size_diff`.
    #[test]
    fn transfer_with_allocation_only_fires_slot_burn() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        let mut content = applied_transfer(TransferSuccess {
            allocated_destination_contract: true,
            ..Default::default()
        });

        burn_content(&mut host, &payer, u64::MAX, &mut content).unwrap();

        assert_eq!(
            payer.balance(&host).unwrap(),
            (1_000_000 - SLOT_BURN).into()
        );
        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &content else {
            panic!("expected Applied target");
        };

        assert_eq!(
            success.balance_updates,
            vec![
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ]
        );
    }

    /// A Transfer that grew storage AND freshly allocated an
    /// implicit destination fires both burns. Edge case — rare in
    /// practice — but the burn pass must compose correctly.
    #[test]
    fn transfer_with_growth_and_allocation_fires_both_burns() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        let mut content = applied_transfer(TransferSuccess {
            paid_storage_size_diff: 10_u64.into(),
            allocated_destination_contract: true,
            ..Default::default()
        });

        burn_content(&mut host, &payer, u64::MAX, &mut content).unwrap();

        let variable = 10 * COST_PER_BYTES;
        let total = variable + SLOT_BURN;
        assert_eq!(payer.balance(&host).unwrap(), (1_000_000 - total).into());
        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &content else {
            panic!("expected Applied target");
        };

        assert_eq!(
            success.balance_updates,
            vec![
                // Variable burn (paid_storage_size_diff = 10).
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(variable as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (variable as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                // Slot burn (allocated_destination_contract = true).
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ]
        );
    }

    /// Insolvent payer on the slot burn (after the variable burn
    /// succeeded): the L1-shaped trace is returned and the success
    /// body's `balance_updates` is left in its pre-burn state — no
    /// half-applied burn pair survives in the receipt.
    #[test]
    fn transfer_insolvent_on_slot_burn_returns_cannot_pay() {
        let mut host = MockKernelHost::default();
        // Cover variable burn (10 × COST_PER_BYTES = 10) but not the
        // slot burn (SLOT_BURN = 257). 100 - 10 = 90 remaining < 257.
        let payer = init_payer(&mut host, 100);
        let mut content = applied_transfer(TransferSuccess {
            paid_storage_size_diff: 10_u64.into(),
            allocated_destination_contract: true,
            ..Default::default()
        });

        let err = burn_content(&mut host, &payer, u64::MAX, &mut content)
            .expect_err("slot burn must fail when payer is insolvent");

        assert_eq!(
            err,
            ApplyOperationError::CannotPayStorageFee(BalanceTooLow {
                contract: payer.contract(),
                balance: 90_u64.into(),
                amount: SLOT_BURN.into(),
            }),
        );

        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &content else {
            panic!("expected Applied target");
        };

        assert!(
            success.balance_updates.is_empty(),
            "atomic-assignment: the variable burn pair must not land on \
             the success body when the slot burn fails (got {:?})",
            success.balance_updates
        );
    }

    /// An `Applied` internal Transfer with
    /// `allocated_destination_contract: true` fires its slot burn
    /// against the same payer; the pair lands on the internal op's
    /// success body.
    #[test]
    fn applied_internal_allocation_fires_slot_burn() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        let mut op = InternalOperationSum::Transfer(InternalContentWithMetadata {
            sender: payer.contract(),
            nonce: 0,
            content: TransferContent {
                amount: 0_u64.into(),
                destination: payer.contract(),
                parameters: Parameters::default(),
            },
            result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                allocated_destination_contract: true,
                ..Default::default()
            })),
        });

        burn_internal_op(&mut host, &payer, u64::MAX, &mut op).unwrap();

        assert_eq!(
            payer.balance(&host).unwrap(),
            (1_000_000 - SLOT_BURN).into()
        );
        let InternalOperationSum::Transfer(inner) = &op else {
            panic!("expected internal Transfer");
        };
        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &inner.result
        else {
            panic!("expected Applied internal target");
        };

        assert_eq!(
            success.balance_updates,
            vec![
                BalanceUpdate {
                    balance: Balance::Account(payer.contract()),
                    changes: -(SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: (SLOT_BURN as i64),
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ]
        );
    }

    /// A top-level Transfer whose `paid_storage_size_diff` exceeds
    /// the operation's declared `storage_limit` returns the
    /// single-element trace `[OperationQuotaExceeded]` and does not
    /// touch the payer's balance (decrement-before-burn).
    #[test]
    fn transfer_overshoot_returns_quota_exceeded() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        let mut content = applied_transfer(TransferSuccess {
            paid_storage_size_diff: 10_u64.into(),
            ..Default::default()
        });

        let trace = burn_content(&mut host, &payer, 9, &mut content)
            .expect_err("burn must fail when consumed exceeds storage_limit");

        assert_eq!(trace, ApplyOperationError::OperationQuotaExceeded);
        assert_eq!(
            payer.balance(&host).unwrap(),
            1_000_000_u64.into(),
            "payer balance must be untouched on quota failure",
        );
    }

    /// An Origination whose variable burn fits exactly within
    /// `storage_limit` but whose slot burn pushes the running total
    /// over the cap returns `[OperationQuotaExceeded]`. The variable
    /// burn already debited the payer; SafeStorage rollback at the
    /// caller is what reverts that — the trace itself is single-element.
    #[test]
    fn origination_overshoot_on_slot_burn_returns_quota_exceeded() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 10_000_000);
        let mut content = applied_origination(origination_success(38));

        let trace = burn_content(
            &mut host,
            &payer,
            38_u64 + ORIGINATION_SIZE - 1,
            &mut content,
        )
        .expect_err("slot burn must fail when budget cannot cover both burns");

        assert_eq!(trace, ApplyOperationError::OperationQuotaExceeded);
    }

    /// `consumed_bytes == storage_limit_remaining` succeeds; a
    /// subsequent non-zero burn against the now-zero remainder fails.
    /// Boundary test: full-budget consumption is allowed, one byte
    /// beyond is not.
    #[test]
    fn burn_at_exact_storage_limit_succeeds() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        let mut storage_limit_remaining = BigUint::from(10_u64);

        let updates = burn_storage_fee(
            &mut host,
            &payer,
            &mut storage_limit_remaining,
            &10_u64.into(),
        )
        .expect("exact-equality burn must succeed");
        assert_eq!(updates.len(), 2);
        assert!(storage_limit_remaining.is_zero());

        let zero_updates = burn_storage_fee(
            &mut host,
            &payer,
            &mut storage_limit_remaining,
            &0_u64.into(),
        )
        .expect("zero-byte burn against an exhausted budget is still a no-op");
        assert!(zero_updates.is_empty());

        let trace = burn_storage_fee(
            &mut host,
            &payer,
            &mut storage_limit_remaining,
            &1_u64.into(),
        )
        .expect_err("any positive burn against a zero remainder must fail");
        assert_eq!(trace, ApplyOperationError::OperationQuotaExceeded);
    }

    /// Transfer with growth + allocation: sum is
    /// `(paid_storage_size_diff + ORIGINATION_SIZE) × COST_PER_BYTES`.
    #[test]
    fn compute_transfer_growth_and_alloc() {
        let content = applied_transfer(TransferSuccess {
            paid_storage_size_diff: 10_u64.into(),
            allocated_destination_contract: true,
            ..Default::default()
        });
        let expected = BigUint::from((10_u64 + ORIGINATION_SIZE) * COST_PER_BYTES);
        assert_eq!(compute_content_storage_fees(&content).unwrap(), expected);
    }

    /// Transfer with no growth and no allocation: zero.
    #[test]
    fn compute_transfer_no_growth_no_alloc_is_zero() {
        let content = applied_transfer(TransferSuccess::default());
        assert_eq!(
            compute_content_storage_fees(&content).unwrap(),
            BigUint::ZERO
        );
    }

    /// Origination always pays the slot, plus content when grown.
    #[test]
    fn compute_origination_content_plus_slot() {
        let content = applied_origination(origination_success(38));
        let expected = BigUint::from((38_u64 + ORIGINATION_SIZE) * COST_PER_BYTES);
        assert_eq!(compute_content_storage_fees(&content).unwrap(), expected);
    }

    /// Reveal allocates nothing — compute returns zero.
    #[test]
    fn compute_reveal_is_zero() {
        let content: ContentResult<RevealContent> =
            ContentResult::Applied(RevealSuccess {
                consumed_milligas: 0_u64.into(),
            });
        assert_eq!(
            compute_content_storage_fees(&content).unwrap(),
            BigUint::ZERO
        );
    }

    /// Non-Applied content (Failed, BackTracked, Skipped) yields
    /// zero — there is no Applied allocation to bill.
    #[test]
    fn compute_non_applied_is_zero() {
        let content: ContentResult<TransferContent> =
            ContentResult::Failed(ApplyOperationErrors { errors: vec![] });
        assert_eq!(
            compute_content_storage_fees(&content).unwrap(),
            BigUint::ZERO
        );
    }

    /// Internal-op dispatcher hits the per-kind summation through
    /// the [`InternalOperationSum`] variants.
    #[test]
    fn compute_internal_op_dispatches_per_kind() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 0);
        let op = InternalOperationSum::Origination(InternalContentWithMetadata {
            sender: payer.contract(),
            nonce: 0,
            content: OriginationContent {
                balance: 0_u64.into(),
                delegate: None,
                script: Script {
                    code: vec![],
                    storage: vec![],
                },
            },
            result: ContentResult::Applied(origination_success(7)),
        });
        let expected = BigUint::from((7_u64 + ORIGINATION_SIZE) * COST_PER_BYTES);
        assert_eq!(compute_internal_op_storage_fees(&op).unwrap(), expected);
    }

    #[test]
    fn burn_storage_cost_zero_is_noop() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        let mut storage_limit_remaining = BigUint::from(100u64);
        burn_storage_cost(&mut host, &payer, &mut storage_limit_remaining, 0).unwrap();
        assert_eq!(storage_limit_remaining, BigUint::from(100u64));
        assert_eq!(payer.balance(&host).unwrap(), 1_000_000_u64.into());
    }

    #[test]
    fn burn_storage_cost_within_limit_debits_payer() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        // Pick V = 10 × COST_PER_BYTES to land exactly on 10 bytes.
        let mutez_cost: u64 = 10 * COST_PER_BYTES;
        let mut storage_limit_remaining = BigUint::from(100u64);
        burn_storage_cost(&mut host, &payer, &mut storage_limit_remaining, mutez_cost)
            .expect("absorption must succeed when limit and balance suffice");
        assert_eq!(storage_limit_remaining, BigUint::from(90u64));
        assert_eq!(
            payer.balance(&host).unwrap(),
            (1_000_000_u64 - mutez_cost).into()
        );
    }

    #[test]
    fn burn_storage_cost_overshoot_returns_quota_exceeded() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000_000);
        // 10 bytes worth of cost, but the budget only allows 9.
        let mutez_cost: u64 = 10 * COST_PER_BYTES;
        let mut storage_limit_remaining = BigUint::from(9u64);
        let err = burn_storage_cost(
            &mut host,
            &payer,
            &mut storage_limit_remaining,
            mutez_cost,
        )
        .expect_err("overshoot must surface OperationQuotaExceeded");
        assert!(matches!(err, ApplyOperationError::OperationQuotaExceeded));
        assert_eq!(
            payer.balance(&host).unwrap(),
            1_000_000_u64.into(),
            "payer balance must be untouched when the limit check rejects the burn"
        );
    }

    #[test]
    fn burn_storage_cost_insolvent_returns_cannot_pay() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 5);
        let mutez_cost: u64 = 10 * COST_PER_BYTES;
        let mut storage_limit_remaining = BigUint::from(100u64);
        let err = burn_storage_cost(
            &mut host,
            &payer,
            &mut storage_limit_remaining,
            mutez_cost,
        )
        .expect_err("insufficient balance must surface CannotPayStorageFee");
        assert!(matches!(err, ApplyOperationError::CannotPayStorageFee(_)));
    }
}
