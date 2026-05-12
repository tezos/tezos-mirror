// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Storage-fee accounting: charges the cost of the storage
//! allocated by a manager operation to its source and records
//! the matching balance updates on the receipt.

use num_bigint::{BigInt, BigUint, TryFromBigIntError};
use num_traits::Zero;
use tezos_data_encoding::types::Zarith;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_tezlink::operation_result::{
    ApplyOperationError, Balance, BalanceUpdate, TransferError, UpdateOrigin,
};

use crate::account_storage::TezlinkAccount;
use crate::burn_tez;

// Values from src/proto_023_PtSeouLo/lib_parameters/default_parameters.ml.
pub const COST_PER_BYTES: u64 = 250;

/// Burns the cost of `nb_consumed_bytes` bytes from the payer and
/// returns the associated balance updates.
#[allow(dead_code)]
fn burn_storage_fee<Host: StorageV1>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    nb_consumed_bytes: &Zarith,
) -> Result<Vec<BalanceUpdate>, ApplyOperationError> {
    let to_burn = BigUint::from(COST_PER_BYTES)
        * BigUint::try_from(&nb_consumed_bytes.0).map_err(|_| {
            ApplyOperationError::Transfer(TransferError::FailedToComputeBalanceUpdate)
        })?;
    if to_burn.is_zero() {
        return Ok(vec![]);
    }
    burn_tez(host, payer, &to_burn).map_err(|err| match err {
        TransferError::BalanceTooLow(btl) => {
            ApplyOperationError::CannotPayStorageFee(btl)
        }
        other => ApplyOperationError::Transfer(other),
    })?;
    compute_storage_balance_updates(payer.contract(), to_burn).map_err(|_| {
        ApplyOperationError::Transfer(TransferError::FailedToComputeBalanceUpdate)
    })
}

/// Prepares balance updates when accounting storage fees in the format expected by the Tezos operation.
pub fn compute_storage_balance_updates(
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

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup::types::PublicKeyHash;
    use tezos_tezlink::operation_result::Balance;

    use crate::account_storage::{TezlinkImplicitAccount, TezosImplicitAccount};
    use crate::context::{self, Context};

    const SOURCE_PKH: &str = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";

    fn init_payer(host: &mut MockKernelHost, balance: u64) -> TezlinkImplicitAccount {
        let pkh = PublicKeyHash::from_b58check(SOURCE_PKH).unwrap();
        let context = context::TezlinkContext::init_context();
        let account = context.implicit_from_public_key_hash(&pkh).unwrap();
        account.allocate(host).unwrap();
        account.set_balance(host, &balance.into()).unwrap();
        account
    }

    /// On a zero-byte input, `burn_storage_fee` returns no balance
    /// updates and does not touch the payer's balance.
    #[test]
    fn burn_storage_fee_zero_bytes_is_noop() {
        let mut host = MockKernelHost::default();
        let initial_balance = 50_u64;
        let payer = init_payer(&mut host, initial_balance);

        let updates = burn_storage_fee(&mut host, &payer, &0.into())
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

        let updates = burn_storage_fee(&mut host, &payer, &nb_consumed_bytes.into())
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
        let initial_balance = 100_u64;
        let nb_consumed_bytes = 10_u64;
        let payer = init_payer(&mut host, initial_balance);

        let err = burn_storage_fee(&mut host, &payer, &nb_consumed_bytes.into())
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
}
