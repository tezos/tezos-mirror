// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::types::Narith;
use tezos_evm_runtime::runtime::Runtime;
use tezos_tezlink::{
    operation::{ManagerOperation, OperationContent},
    operation_result::{CounterError, ValidityError},
};

use crate::{account_storage::TezlinkImplicitAccount, ApplyKernelError};

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

// Inspired from `check_gas_limit` in `src/proto_alpha/lib_protocol/gas_limit_repr.ml`
fn check_gas_limit(
    hard_gas_limit_per_operation: &Narith,
    gas_limit: &Narith,
) -> Result<(), ValidityError> {
    if gas_limit.0 <= hard_gas_limit_per_operation.0 {
        Ok(())
    } else {
        Err(ValidityError::GasLimitTooHigh)
    }
}

// Inspired from `check_storage_limit` in `src/proto_alpha/lib_protocol/validate.ml`
fn check_storage_limit(
    hard_storage_limit_per_operation: &Narith,
    storage_limit: &Narith,
) -> Result<(), ValidityError> {
    if storage_limit.0 <= hard_storage_limit_per_operation.0 {
        Ok(())
    } else {
        Err(ValidityError::StorageLimitTooHigh)
    }
}

pub fn is_valid_tezlink_operation<Host: Runtime>(
    host: &Host,
    account: &TezlinkImplicitAccount,
    operation: &ManagerOperation<OperationContent>,
) -> Result<Result<Narith, ValidityError>, ApplyKernelError> {
    // Account must exist in the durable storage
    if !account.allocated(host)? {
        return Ok(Err(ValidityError::EmptyImplicitContract));
    }

    if let Err(err) = account.check_counter_increment(host, &operation.counter)? {
        // Counter verification failed, return the error
        return Ok(Err(err));
    }

    // TODO: hard gas limit per operation is a Tezos constant, for now we took the one from ghostnet
    if let Err(err) = check_gas_limit(&1040000_u64.into(), &operation.gas_limit) {
        // Gas limit verification failed, return the error
        return Ok(Err(err));
    }

    // TODO: hard storage limit per operation is a Tezos constant, for now we took the one from ghostnet
    if let Err(err) = check_storage_limit(&60000_u64.into(), &operation.storage_limit) {
        // Storage limit verification failed, return the error
        return Ok(Err(err));
    }

    // The manager account must be solvent to pay the announced fees.
    let new_balance = match account.simulate_spending(host, &operation.fee)? {
        Some(new_balance) => new_balance,
        None => {
            return Ok(Err(ValidityError::CantPayFees(operation.fee.clone())));
        }
    };

    Ok(Ok(new_balance))
}
