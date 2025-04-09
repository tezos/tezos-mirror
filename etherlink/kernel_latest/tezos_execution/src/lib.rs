// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use account_storage::TezlinkImplicitAccount;
use tezos_crypto_rs::base58::FromBase58CheckError;
use tezos_data_encoding::types::Narith;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::Contract;
use tezos_tezlink::{
    operation::{ManagerOperation, Operation},
    operation_result::{
        produce_operation_result, OperationError, OperationResultSum, Reveal,
        RevealSuccess, ValidityError,
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
        operation: _,
    } = &operation.content;

    log!(
        host,
        Debug,
        "Going to run a Tezos Manager Operation from {}",
        source
    );

    let contract = Contract::from_b58check(&source.to_b58check())?;
    let account =
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

    // TODO: Don't force the receipt to a reveal receipt
    let dummy_result = produce_operation_result::<Reveal>(Ok(RevealSuccess {
        consumed_gas: 0_u64.into(),
    }));

    Ok(OperationResultSum::Reveal(dummy_result))
}

#[cfg(test)]
mod tests {
    use tezos_crypto_rs::hash::UnknownSignature;
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};
    use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
    use tezos_tezlink::{
        block::TezBlock,
        operation::{ManagerOperation, Operation, OperationContent},
        operation_result::{ContentResult, OperationResult, OperationResultSum},
    };

    use crate::{
        account_storage::TezlinkImplicitAccount, apply_operation, context,
        OperationError, ValidityError,
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
}
