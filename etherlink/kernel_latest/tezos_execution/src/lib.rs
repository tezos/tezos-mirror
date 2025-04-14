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
