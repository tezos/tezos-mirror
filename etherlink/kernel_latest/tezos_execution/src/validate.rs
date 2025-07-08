// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::types::Narith;
use tezos_evm_runtime::runtime::Runtime;
use tezos_tezlink::{
    operation::{ManagerOperation, OperationContent},
    operation_result::{CounterError, ValidityError},
};

use crate::{
    account_storage::{TezlinkAccount, TezlinkImplicitAccount},
    ApplyKernelError,
};

impl TezlinkImplicitAccount {
    /// Inspired from `contract_storage.ml` in the Tezos protocol
    /// This function verifies that the counter given in argument is the
    /// successor of the stored counter. If not, it returns the appropriate
    /// error.
    pub fn check_counter_increment(
        &self,
        host: &impl Runtime,
        counter: &Narith,
    ) -> Result<Result<(), ValidityError>, tezos_storage::error::Error> {
        let contract_counter = self.counter(host)?;
        // The provided counter value must be the successor of the manager's counter.
        let expected_counter = Narith(&contract_counter.0 + 1_u64);

        if &expected_counter == counter {
            Ok(Ok(()))
        } else if expected_counter.0 > counter.0 {
            let error = CounterError {
                expected: expected_counter,
                found: counter.clone(),
            };
            Ok(Err(ValidityError::CounterInThePast(error)))
        } else {
            let error = CounterError {
                expected: expected_counter,
                found: counter.clone(),
            };
            Ok(Err(ValidityError::CounterInTheFuture(error)))
        }
    }
}

pub fn is_valid_tezlink_operation<Host: Runtime>(
    host: &Host,
    account: &TezlinkImplicitAccount,
    operation: &ManagerOperation<OperationContent>,
) -> Result<Result<(), ValidityError>, ApplyKernelError> {
    // Account must exist in the durable storage
    if !account.allocated(host)? {
        return Ok(Err(ValidityError::EmptyImplicitContract));
    }

    if let Err(err) = account.check_counter_increment(host, &operation.counter)? {
        // Counter verification failed, return the error
        return Ok(Err(err));
    }

    // The manager account must be solvent to pay the announced fees.
    // TODO: account balance should not be stored as U256
    let balance = account.balance(host)?;

    if balance.0 < operation.fee.0 {
        return Ok(Err(ValidityError::CantPayFees(balance)));
    }

    Ok(Ok(()))
}
