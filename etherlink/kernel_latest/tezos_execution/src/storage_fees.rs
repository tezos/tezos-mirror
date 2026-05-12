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
    ApplyOperationError, Balance, BalanceUpdate, ContentResult, InternalOperationSum,
    OperationResultSum, OriginationSuccess, TransferError, TransferSuccess,
    TransferTarget, UpdateOrigin,
};

use crate::account_storage::TezlinkAccount;
use crate::burn_tez;

// Values from src/proto_023_PtSeouLo/lib_parameters/default_parameters.ml.
pub const COST_PER_BYTES: u64 = 250;
pub const ORIGINATION_SIZE: u64 = 257;

/// Burns the cost of `nb_consumed_bytes` bytes from the payer and
/// returns the associated balance updates.
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

/// Charge the source of a manager operation for the storage
/// allocated by the operation and its successful internal
/// operations.
pub fn burn_manager_storage_fees<Host: StorageV1>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    receipt: &mut OperationResultSum,
) -> Result<(), ApplyOperationError> {
    match receipt {
        OperationResultSum::Reveal(op) => {
            if matches!(op.result, ContentResult::Applied(_)) {
                burn_internals(host, payer, &mut op.internal_operation_results)?;
            }
            Ok(())
        }
        OperationResultSum::Transfer(op) => {
            if let ContentResult::Applied(TransferTarget::ToContrat(success)) =
                &mut op.result
            {
                let updates = burn_for_transfer(host, payer, success)?;
                success.balance_updates = updates;
                burn_internals(host, payer, &mut op.internal_operation_results)?;
            }
            Ok(())
        }
        OperationResultSum::Origination(op) => {
            if let ContentResult::Applied(success) = &mut op.result {
                let updates = burn_for_origination(host, payer, success)?;
                success.balance_updates = updates;
                burn_internals(host, payer, &mut op.internal_operation_results)?;
            }
            Ok(())
        }
    }
}

/// Charge the source of a manager operation for the storage that
/// each of its successful internal operations allocated.
fn burn_internals<Host: StorageV1>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    internals: &mut Vec<InternalOperationSum>,
) -> Result<(), ApplyOperationError> {
    let burned: Result<Vec<InternalOperationSum>, ApplyOperationError> = internals
        .iter()
        .cloned()
        .map(|mut internal| {
            match &mut internal {
                InternalOperationSum::Transfer(inner) => {
                    if let ContentResult::Applied(TransferTarget::ToContrat(success)) =
                        &mut inner.result
                    {
                        let updates = burn_for_transfer(host, payer, success)?;
                        success.balance_updates = updates;
                    }
                }
                InternalOperationSum::Origination(inner) => {
                    if let ContentResult::Applied(success) = &mut inner.result {
                        let updates = burn_for_origination(host, payer, success)?;
                        success.balance_updates = updates;
                    }
                }
                InternalOperationSum::Event(_) => {}
            }
            Ok(internal)
        })
        .collect();
    *internals = burned?;
    Ok(())
}

/// Charge the source for the storage allocated by a successful
/// transfer to a contract. If the charging fails, the receipt is
/// not modified.
fn burn_for_transfer<Host: StorageV1>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    success: &TransferSuccess,
) -> Result<Vec<BalanceUpdate>, ApplyOperationError> {
    let mut updates = burn_storage_fee(host, payer, &success.paid_storage_size_diff)?;
    updates.extend(success.balance_updates.iter().cloned());
    Ok(updates)
}

/// Charge the source for the storage allocated by a successful
/// origination: the initial content of the contract and the
/// contract slot itself.
fn burn_for_origination<Host: StorageV1>(
    host: &mut Host,
    payer: &impl TezlinkAccount,
    success: &OriginationSuccess,
) -> Result<Vec<BalanceUpdate>, ApplyOperationError> {
    let mut updates = burn_storage_fee(host, payer, &success.paid_storage_size_diff)?;
    updates.extend(burn_storage_fee(host, payer, &ORIGINATION_SIZE.into())?);
    updates.extend(success.balance_updates.iter().cloned());
    Ok(updates)
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
        InternalContentWithMetadata, OperationResult, Originated, RevealSuccess,
        TransferSuccess, UpdateOrigin,
    };

    use crate::account_storage::{TezlinkImplicitAccount, TezosImplicitAccount};
    use crate::context::{self, Context};

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

    fn applied_origination(success: OriginationSuccess) -> OperationResultSum {
        OperationResultSum::Origination(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Applied(success),
            internal_operation_results: vec![],
        })
    }

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

    fn applied_transfer(success: TransferSuccess) -> OperationResultSum {
        OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Applied(TransferTarget::ToContrat(success)),
            internal_operation_results: vec![],
        })
    }

    /// A Reveal receipt with no internals leaves the payer balance
    /// untouched.
    #[test]
    fn reveal_with_no_internals_is_a_noop() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 1_000);
        let mut receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Applied(RevealSuccess {
                consumed_milligas: 0_u64.into(),
            }),
            internal_operation_results: vec![],
        });
        burn_manager_storage_fees(&mut host, &payer, &mut receipt).unwrap();
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
        let mut receipt = applied_transfer(TransferSuccess {
            paid_storage_size_diff: 10_u64.into(),
            ..Default::default()
        });

        burn_manager_storage_fees(&mut host, &payer, &mut receipt).unwrap();

        let burn = 10 * COST_PER_BYTES;
        assert_eq!(
            payer.balance(&host).unwrap(),
            (100_000 - burn).into(),
            "payer must drop by the burn amount"
        );
        let OperationResultSum::Transfer(op) = &receipt else {
            panic!("expected Transfer receipt");
        };
        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &op.result
        else {
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
        let mut receipt = applied_transfer(TransferSuccess::default());
        burn_manager_storage_fees(&mut host, &payer, &mut receipt).unwrap();
        assert_eq!(payer.balance(&host).unwrap(), 1_000_u64.into());
        let OperationResultSum::Transfer(op) = &receipt else {
            panic!("expected Transfer receipt");
        };
        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &op.result
        else {
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
        let mut receipt = OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(ApplyOperationErrors { errors: vec![] }),
            internal_operation_results: vec![],
        });
        burn_manager_storage_fees(&mut host, &payer, &mut receipt).unwrap();
        assert_eq!(payer.balance(&host).unwrap(), 1_000_u64.into());
    }

    /// Insolvent payer for the top-level burn: returns a single
    /// [`ApplyOperationError::CannotPayStorageFee`] carrying the
    /// underlying `BalanceTooLow`.
    #[test]
    fn insolvent_payer_returns_cannot_pay() {
        let mut host = MockKernelHost::default();
        let payer = init_payer(&mut host, 100);
        let mut receipt = applied_transfer(TransferSuccess {
            paid_storage_size_diff: 10_u64.into(),
            ..Default::default()
        });
        let err = burn_manager_storage_fees(&mut host, &payer, &mut receipt)
            .expect_err("burn must fail when payer is insolvent");

        assert_eq!(
            err,
            ApplyOperationError::CannotPayStorageFee(BalanceTooLow {
                contract: payer.contract(),
                balance: 100_u64.into(),
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
        let internal = InternalOperationSum::Transfer(InternalContentWithMetadata {
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
        let mut receipt = OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Applied(TransferTarget::ToContrat(
                TransferSuccess::default(),
            )),
            internal_operation_results: vec![internal],
        });

        burn_manager_storage_fees(&mut host, &payer, &mut receipt).unwrap();

        let burn = 4 * COST_PER_BYTES;
        assert_eq!(payer.balance(&host).unwrap(), (100_000 - burn).into());
        let OperationResultSum::Transfer(op) = &receipt else {
            panic!("expected Transfer receipt");
        };
        let ContentResult::Applied(TransferTarget::ToContrat(success)) = &op.result
        else {
            panic!("expected Applied target");
        };
        assert!(success.balance_updates.is_empty());

        let InternalOperationSum::Transfer(inner) = &op.internal_operation_results[0]
        else {
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
        let mut receipt = applied_origination(origination_success(38));

        burn_manager_storage_fees(&mut host, &payer, &mut receipt).unwrap();

        let variable = 38 * COST_PER_BYTES;
        let total = variable + SLOT_BURN;
        assert_eq!(payer.balance(&host).unwrap(), (1_000_000 - total).into());
        let OperationResultSum::Origination(op) = &receipt else {
            panic!("expected Origination receipt");
        };
        let ContentResult::Applied(success) = &op.result else {
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
        let mut receipt = applied_origination(origination_success(0));

        burn_manager_storage_fees(&mut host, &payer, &mut receipt).unwrap();

        assert_eq!(
            payer.balance(&host).unwrap(),
            (1_000_000 - SLOT_BURN).into()
        );
        let OperationResultSum::Origination(op) = &receipt else {
            panic!("expected Origination receipt");
        };
        let ContentResult::Applied(success) = &op.result else {
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
        // Cover variable burn (38 × COST_PER_BYTES = 9_500) but not the slot burn.
        // 10_000 - 9_500 = 500 remaining
        let payer = init_payer(&mut host, 10_000);
        let mut receipt = applied_origination(origination_success(38));

        let err = burn_manager_storage_fees(&mut host, &payer, &mut receipt)
            .expect_err("slot burn must fail when payer is insolvent");

        assert_eq!(
            err,
            ApplyOperationError::CannotPayStorageFee(BalanceTooLow {
                contract: payer.contract(),
                balance: 500_u64.into(),
                amount: SLOT_BURN.into(),
            }),
        );

        let OperationResultSum::Origination(op) = &receipt else {
            panic!("expected Origination receipt");
        };
        let ContentResult::Applied(success) = &op.result else {
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
        let internal = InternalOperationSum::Origination(InternalContentWithMetadata {
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
        let mut receipt = OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Applied(TransferTarget::ToContrat(
                TransferSuccess::default(),
            )),
            internal_operation_results: vec![internal],
        });

        burn_manager_storage_fees(&mut host, &payer, &mut receipt).unwrap();

        let variable = 10 * COST_PER_BYTES;
        let total = variable + SLOT_BURN;
        assert_eq!(payer.balance(&host).unwrap(), (1_000_000 - total).into());
        let OperationResultSum::Transfer(op) = &receipt else {
            panic!("expected Transfer receipt");
        };
        let ContentResult::Applied(TransferTarget::ToContrat(top_success)) = &op.result
        else {
            panic!("expected Applied top");
        };
        assert!(
            top_success.balance_updates.is_empty(),
            "top has no growth and no allocation, must not burn"
        );

        let InternalOperationSum::Origination(inner) = &op.internal_operation_results[0]
        else {
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
}
