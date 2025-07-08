// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::types::Narith;
use tezos_evm_runtime::runtime::Runtime;
use tezos_tezlink::{
    operation::{ManagerOperation, OperationContent},
    operation_result::ValidityError,
};

use crate::{
    account_storage::{TezlinkAccount, TezlinkImplicitAccount},
    ApplyKernelError,
};

pub fn is_valid_tezlink_operation<Host: Runtime>(
    host: &Host,
    account: &TezlinkImplicitAccount,
    operation: &ManagerOperation<OperationContent>,
) -> Result<Result<(), ValidityError>, ApplyKernelError> {
    // Account must exist in the durable storage
    if !account.allocated(host)? {
        return Ok(Err(ValidityError::EmptyImplicitContract));
    }

    // The manager account must be solvent to pay the announced fees.
    // TODO: account balance should not be stored as U256
    let balance = account.balance(host)?;

    if balance.0 < operation.fee.0 {
        return Ok(Err(ValidityError::CantPayFees(balance)));
    }

    let previous_counter = account.counter(host)?;

    // The provided counter value must be the successor of the manager's counter.
    let succ_counter = Narith(&previous_counter.0 + 1_u64);

    if succ_counter != operation.counter {
        return Ok(Err(ValidityError::InvalidCounter(previous_counter)));
    }

    Ok(Ok(()))
}
