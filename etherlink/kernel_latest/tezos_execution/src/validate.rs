// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::types::Narith;
use tezos_evm_logging::log;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::PublicKey;
use tezos_tezlink::{
    operation::{
        verify_signature, ManagerOperation, Operation, OperationContent, RevealContent,
    },
    operation_result::{CounterError, ValidityError},
};

use crate::{
    account_storage::{Manager, TezlinkImplicitAccount},
    context::Context,
    BalanceUpdate,
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
    ) -> Result<(), ValidityError> {
        let contract_counter = self
            .counter(host)
            .map_err(|_| ValidityError::FailedToFetchCounter)?;
        // The provided counter value must be the successor of the manager's counter.
        let expected_counter = Narith(&contract_counter.0 + 1_u64);

        if &expected_counter == counter {
            Ok(())
        } else if expected_counter.0 > counter.0 {
            let error = CounterError {
                expected: expected_counter,
                found: counter.clone(),
            };
            log!(
                host,
                tezos_evm_logging::Level::Debug,
                "Invalid operation: Source counter is in the past"
            );
            Err(ValidityError::CounterInThePast(error))
        } else {
            let error = CounterError {
                expected: expected_counter,
                found: counter.clone(),
            };
            log!(
                host,
                tezos_evm_logging::Level::Debug,
                "Invalid operation: Source counter is in the future"
            );
            Err(ValidityError::CounterInTheFuture(error))
        }
    }

    pub fn get_manager_key(
        &self,
        host: &impl Runtime,
    ) -> Result<Result<PublicKey, ValidityError>, tezos_storage::error::Error> {
        let manager = self.manager(host).ok();
        match manager {
            None => Ok(Err(ValidityError::MissingManagerContract)),
            Some(Manager::NotRevealed(public_key_hash)) => {
                Ok(Err(ValidityError::UnrevealedManagerKey(public_key_hash)))
            }
            Some(Manager::Revealed(public_key)) => Ok(Ok(public_key)),
        }
    }
}

/// In order to validate an operation, we need to check its signature,
/// which requires obtaining the public key of the source.
/// In all cases except Reveal, we obtain the public key from the context.
/// However, the purpose of Reveal is to register the public key in the context,
/// making it a special case. In this case, we obtain the public key
/// from the operation's payload.
fn get_revealed_key<Host: Runtime>(
    host: &Host,
    account: &TezlinkImplicitAccount,
    content: &OperationContent,
) -> Result<PublicKey, ValidityError> {
    match content {
        OperationContent::Reveal(RevealContent { pk, proof: _ }) => Ok(pk.clone()),
        _ => account
            .get_manager_key(host)
            .map_err(|_| ValidityError::FailedToFetchManagerKey)?,
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

pub struct ValidationInfo {
    pub new_source_balance: Narith,
    pub source_account: TezlinkImplicitAccount,
    pub balance_updates: Vec<BalanceUpdate>,
}

pub fn validate_operation<Host: Runtime>(
    host: &Host,
    context: &Context,
    operation: &Operation,
) -> Result<ValidationInfo, ValidityError> {
    let branch = &operation.branch;
    let content: ManagerOperation<OperationContent> = operation.content.clone().into();
    let signature = &operation.signature;
    let account = TezlinkImplicitAccount::from_public_key_hash(context, &content.source)
        .map_err(|_| ValidityError::FailedToFetchAccount)?;

    // Account must exist in the durable storage
    if !account
        .allocated(host)
        .map_err(|_| ValidityError::FailedToFetchBalance)?
    {
        return Err(ValidityError::EmptyImplicitContract);
    }

    account.check_counter_increment(host, &content.counter)?;

    let pk = get_revealed_key(host, &account, &content.operation)?;

    // TODO: hard gas limit per operation is a Tezos constant, for now we took the one from ghostnet
    check_gas_limit(&1040000_u64.into(), &content.gas_limit)?;

    // TODO: hard storage limit per operation is a Tezos constant, for now we took the one from ghostnet
    check_storage_limit(&60000_u64.into(), &content.storage_limit)?;

    // The manager account must be solvent to pay the announced fees.
    let new_balance = match account.simulate_spending(host, &content.fee) {
        Ok(Some(new_balance)) => new_balance,
        Ok(None) => {
            log!(
                host,
                tezos_evm_logging::Level::Debug,
                "Invalid operation: Can't pay the fees"
            );
            return Err(ValidityError::CantPayFees(content.fee));
        }
        Err(_) => return Err(ValidityError::FailedToFetchBalance),
    };

    match verify_signature(&pk, branch, &operation.content, signature.clone()) {
        Ok(true) => (),
        _ => return Err(ValidityError::InvalidSignature),
    }

    let (src_delta, block_fees) =
        crate::compute_fees_balance_updates(&content.source, &content.fee)
            .map_err(|_| ValidityError::FailedToComputeFeeBalanceUpdate)?;

    Ok(ValidationInfo {
        new_source_balance: new_balance,
        source_account: account,
        balance_updates: vec![src_delta, block_fees],
    })
}
