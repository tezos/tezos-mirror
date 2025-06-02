// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use account_storage::{Manager, TezlinkImplicitAccount};
use tezos_crypto_rs::{base58::FromBase58CheckError, PublicKeyWithHash};
use tezos_data_encoding::types::Narith;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
use tezos_tezlink::{
    operation::{ManagerOperation, Operation, OperationContent},
    operation_result::{
        produce_operation_result, ApplyOperationError, OperationError,
        OperationResultSum, Reveal, RevealSuccess, ValidityError,
    },
};
use thiserror::Error;

extern crate alloc;
pub mod account_storage;
pub mod context;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ApplyKernelError {
    #[error("Apply operation failed on a storage manipulation {0}")]
    StorageError(tezos_storage::error::Error),
    #[error("Apply operation failed because of a b58 conversion {0}")]
    Base58Error(String),
}

// 'FromBase58CheckError' doesn't implement PartialEq and Eq
// Use the String representation instead
impl From<FromBase58CheckError> for ApplyKernelError {
    fn from(err: FromBase58CheckError) -> Self {
        Self::Base58Error(err.to_string())
    }
}

impl From<tezos_storage::error::Error> for ApplyKernelError {
    fn from(value: tezos_storage::error::Error) -> Self {
        Self::StorageError(value)
    }
}

fn is_valid_tezlink_operation<Host: Runtime>(
    host: &Host,
    account: &TezlinkImplicitAccount,
    fee: &Narith,
    expected_counter: &Narith,
) -> Result<Result<(), ValidityError>, ApplyKernelError> {
    // Account must exist in the durable storage
    if !account.allocated(host)? {
        return Ok(Err(ValidityError::EmptyImplicitContract));
    }

    // The manager account must be solvent to pay the announced fees.
    // TODO: account balance should not be stored as U256
    let balance = account.balance(host)?;

    if balance.0 < fee.0 {
        return Ok(Err(ValidityError::CantPayFees(balance)));
    }

    let previous_counter = account.counter(host)?;

    // The provided counter value must be the successor of the manager's counter.
    let succ_counter = Narith(&previous_counter.0 + 1_u64);

    if &succ_counter != expected_counter {
        return Ok(Err(ValidityError::InvalidCounter(previous_counter)));
    }

    Ok(Ok(()))
}

fn reveal<Host: Runtime>(
    host: &mut Host,
    provided_hash: &PublicKeyHash,
    account: &mut TezlinkImplicitAccount,
    public_key: &PublicKey,
) -> Result<Result<RevealSuccess, OperationError>, ApplyKernelError> {
    log!(host, Debug, "Applying a reveal operation");
    let manager = account.manager(host)?;

    let expected_hash = match manager {
        Manager::Revealed(pk) => {
            return Ok(Err(ApplyOperationError::PreviouslyRevealedKey(pk).into()))
        }
        Manager::NotRevealed(pkh) => pkh,
    };

    // Ensure that the source of the operation is equal to the retrieved hash.
    if &expected_hash != provided_hash {
        return Ok(Err(
            ApplyOperationError::InconsistentHash(expected_hash).into()
        ));
    }

    // Check the public key
    let pkh_from_pk = public_key.pk_hash();
    if expected_hash != pkh_from_pk {
        return Ok(Err(ApplyOperationError::InconsistentPublicKey(
            expected_hash,
        )
        .into()));
    }

    // Set the public key as the manager
    account.set_manager_public_key(host, public_key)?;

    log!(host, Debug, "Reveal operation succeed");

    Ok(Ok(RevealSuccess {
        consumed_gas: 0_u64.into(),
    }))
}

pub fn apply_operation<Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    operation: &Operation,
) -> Result<OperationResultSum, ApplyKernelError> {
    let ManagerOperation {
        source,
        fee,
        counter,
        gas_limit: _,
        storage_limit: _,
        operation: content,
    } = &operation.content;

    log!(
        host,
        Debug,
        "Going to run a Tezos Manager Operation from {}",
        source
    );

    let contract = Contract::from_b58check(&source.to_b58check())?;
    let mut account =
        account_storage::TezlinkImplicitAccount::from_contract(context, &contract)?;

    log!(host, Debug, "Verifying that the operation is valid");

    let validity_result = is_valid_tezlink_operation(host, &account, fee, counter)?;

    if let Err(validity_err) = validity_result {
        log!(host, Debug, "Operation is invalid, exiting apply_operation");
        // TODO: Don't force the receipt to a reveal receipt
        let receipt = produce_operation_result::<Reveal>(Err(
            OperationError::Validation(validity_err),
        ));
        return Ok(OperationResultSum::Reveal(receipt));
    }

    log!(host, Debug, "Operation is valid");

    let receipt = match content {
        OperationContent::Reveal { pk } => {
            let reveal_result = reveal(host, source, &mut account, pk)?;
            let manager_result = produce_operation_result(reveal_result);
            OperationResultSum::Reveal(manager_result)
        }
    };

    Ok(receipt)
}

#[cfg(test)]
mod tests {
    use tezos_crypto_rs::hash::UnknownSignature;
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};
    use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
    use tezos_tezlink::{
        block::TezBlock,
        operation::{ManagerOperation, Operation, OperationContent},
        operation_result::{
            ApplyOperationError, ContentResult, OperationResult, OperationResultSum,
            RevealSuccess,
        },
    };

    use crate::{
        account_storage::{Manager, TezlinkImplicitAccount},
        apply_operation, context, OperationError, ValidityError,
    };

    fn make_reveal_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: PublicKeyHash,
        pk: PublicKey,
    ) -> Operation {
        let branch = TezBlock::genesis_block_hash();
        // No need a real signature for now
        let signature = UnknownSignature::from_base58_check("sigSPESPpW4p44JK181SmFCFgZLVvau7wsJVN85bv5ciigMu7WSRnxs9H2NydN5ecxKHJBQTudFPrUccktoi29zHYsuzpzBX").unwrap();
        Operation {
            branch,
            content: ManagerOperation {
                source,
                fee: fee.into(),
                counter: counter.into(),
                operation: OperationContent::Reveal { pk },
                gas_limit: gas_limit.into(),
                storage_limit: storage_limit.into(),
            },
            signature,
        }
    }

    // This function setups an account that will pass the validity checks
    fn init_account(
        host: &mut impl Runtime,
        src: &PublicKeyHash,
    ) -> TezlinkImplicitAccount {
        // Setting the account in TezlinkImplicitAccount
        let contract = Contract::from_b58check(&src.to_b58check())
            .expect("Contract b58 conversion should have succeed");

        let context = context::Context::init_context();

        // Allocate the account
        TezlinkImplicitAccount::allocate(host, &context, &contract)
            .expect("Account initialization should have succeed");

        let mut account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        // Setting the balance to pass the validity check
        account
            .set_balance(host, &50_u64.into())
            .expect("Set balance should have succeed");

        account
    }

    // Test an operation on an account that has no entry in `/context/contracts/index`
    // This should fail as an EmptyImplicitContract
    #[test]
    fn apply_operation_empty_account() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash b58 conversion should have succeed");

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, src, pk);

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), &operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![OperationError::Validation(
                ValidityError::EmptyImplicitContract,
            )]),
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test that increasing the fees makes the operation fails
    #[test]
    fn apply_operation_cant_pay_fees() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash b58 conversion should have succeed");

        let _ = init_account(&mut host, &src);

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        // Fees are too high for source's balance
        let operation = make_reveal_operation(100, 1, 4, 5, src, pk);

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), &operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![OperationError::Validation(
                ValidityError::CantPayFees(50_u64.into()),
            )]),
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test that a wrong counter should make the operation fails
    #[test]
    fn apply_operation_invalid_counter() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash b58 conversion should have succeed");

        let _ = init_account(&mut host, &src);

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        // Counter is incoherent for source's counter
        let operation = make_reveal_operation(15, 15, 4, 5, src, pk);

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), &operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![OperationError::Validation(
                ValidityError::InvalidCounter(0_u64.into()),
            )]),
        });
        assert_eq!(receipt, expected_receipt);
    }

    // At this point, tests are focused on the content of the operation. We should not revert with ValidityError anymore.
    // Test a reveal operation on an already revealed account
    #[test]
    fn apply_reveal_operation_on_already_revealed_account() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash b58 conversion should have succeed");

        let mut account = init_account(&mut host, &src);

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
        let operation = make_reveal_operation(15, 1, 4, 5, src, pk.clone());
        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), &operation)
                .expect("apply_operation should not have failed with a kernel error");

        // Reveal operation should fail
        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![OperationError::Apply(
                ApplyOperationError::PreviouslyRevealedKey(pk),
            )]),
        });
        assert_eq!(receipt, expected_receipt);
    }

    // Test an invalid reveal operation where the manager is inconsistent for source
    // (where source is different of the manager field)
    #[test]
    fn apply_reveal_operation_with_an_inconsistent_manager() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash b58 conversion should have succeed");

        let mut account = init_account(&mut host, &src);

        // Set the an inconsistent manager with the source
        let inconsistent_pkh =
            PublicKeyHash::from_b58check("tz1UEQcU7M43yUECMpKGJcxCVwHRaP819qhN")
                .expect("PublicKeyHash b58 conversion should have succeed");

        account
            .set_manager_public_key_hash(&mut host, &inconsistent_pkh)
            .expect("Setting manager field should have succeed");

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, src, pk);

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), &operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![OperationError::Apply(
                ApplyOperationError::InconsistentHash(inconsistent_pkh),
            )]),
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test an invalid operation where the provided public key is inconsistent for the source
    #[test]
    fn apply_reveal_operation_with_an_inconsistent_public_key() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash b58 conversion should have succeed");

        // Even if we don't use it we need to init the account
        let _ = init_account(&mut host, &src);

        // Wrong public key for source
        let pk = PublicKey::from_b58check(
            "edpkuT1qccDweCHnvgjLuNUHERpZmEaFZfbWvTzj2BxmTgQBZjaDFD",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, src.clone(), pk);

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), &operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![OperationError::Apply(
                ApplyOperationError::InconsistentPublicKey(src),
            )]),
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test a valid reveal operation, the manager should go from NotRevealed to Revealed
    #[test]
    fn apply_reveal_operation() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash b58 conversion should have succeed");

        let account = init_account(&mut host, &src);

        let manager = account
            .manager(&host)
            .expect("Read manager should have succeed");

        assert_eq!(manager, Manager::NotRevealed(src.clone()));

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, src, pk.clone());

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), &operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Applied(RevealSuccess {
                consumed_gas: 0_u64.into(),
            }),
        });

        assert_eq!(receipt, expected_receipt);

        let manager = account
            .manager(&host)
            .expect("Read manager should have succeed");

        assert_eq!(manager, Manager::Revealed(pk));
    }
}
