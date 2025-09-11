// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::types::Narith;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::PublicKey;
use tezos_tezlink::{
    operation::{ManagerOperation, OperationContent, RevealContent},
    operation_result::{CounterError, ValidityError},
};

use crate::{
    account_storage::{Manager, TezlinkAccount, TezlinkImplicitAccount},
    context::Context,
    BalanceUpdate,
};

impl TezlinkImplicitAccount {
    /// Inspired from `contract_storage.ml` in the Tezos protocol
    /// This function verifies that the counter given in argument is the
    /// successor of the stored counter. If not, it returns the appropriate
    /// error.
    fn check_counter_increment(
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
            log!(
                host,
                Debug,
                "Validation: OK - Operation has the expected counter {:?}.",
                expected_counter
            );
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

    fn get_manager_key(
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
/// from the operation's payload. When processing a batch, the reveal operation is the
/// first operation on it.
fn get_revealed_key<Host: Runtime>(
    host: &Host,
    account: &TezlinkImplicitAccount,
    first_content: &OperationContent,
) -> Result<PublicKey, ValidityError> {
    match first_content {
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

fn validate_source<Host: Runtime>(
    host: &Host,
    context: &Context,
    content: &Vec<ManagerOperation<OperationContent>>,
) -> Result<(PublicKey, TezlinkImplicitAccount), ValidityError> {
    if content.is_empty() {
        return Err(ValidityError::EmptyBatch);
    }

    let source = &content[0].source;

    for c in content {
        if c.source != *source {
            return Err(ValidityError::MultipleSources);
        }
    }

    let account = TezlinkImplicitAccount::from_public_key_hash(context, source)
        .map_err(|_| ValidityError::FailedToFetchAccount)?;

    // Account must exist in the durable storage
    if !account
        .allocated(host)
        .map_err(|_| ValidityError::FailedToFetchBalance)?
    {
        return Err(ValidityError::EmptyImplicitContract);
    }

    let pk = get_revealed_key(host, &account, &content[0].operation)?;

    Ok((pk, account))
}

fn validate_individual_operation<Host: Runtime>(
    host: &Host,
    account: &TezlinkImplicitAccount,
    content: &ManagerOperation<OperationContent>,
) -> Result<(Narith, Vec<BalanceUpdate>), ValidityError> {
    account.check_counter_increment(host, &content.counter)?;

    // TODO: hard gas limit per operation is a Tezos constant, for now we took the one from ghostnet
    let hard_gas_limit = 1040000_u64;
    check_gas_limit(&hard_gas_limit.into(), &content.gas_limit)?;
    log!(
        host,
        Debug,
        "Validation: OK - the gas_limit {:?} does not exceed the {:?} threshold.",
        &content.gas_limit,
        hard_gas_limit
    );

    // TODO: hard storage limit per operation is a Tezos constant, for now we took the one from ghostnet
    let hard_storage_limit = 60000_u64;
    check_storage_limit(&hard_storage_limit.into(), &content.storage_limit)?;
    log!(
        host,
        Debug,
        "Validation: OK - the storage_limit {:?} does not exceed the {:?} threshold.",
        &content.storage_limit,
        hard_storage_limit
    );

    // The manager account must be solvent to pay the announced fees.
    let new_balance = match account.simulate_spending(host, &content.fee) {
        Ok(Some(new_balance)) => new_balance,
        Ok(None) => {
            return Err(ValidityError::CantPayFees(content.fee.clone()));
        }
        Err(_) => return Err(ValidityError::FailedToFetchBalance),
    };
    log!(
        host,
        Debug,
        "Validation: OK - the source can pay {:?} in fees, being left with a new balance of {:?}.",
        &content.fee,
        new_balance
    );

    let (src_delta, block_fees) =
        crate::compute_fees_balance_updates(account, &content.fee)
            .map_err(|_| ValidityError::FailedToComputeFeeBalanceUpdate)?;

    Ok((new_balance, vec![src_delta, block_fees]))
}

pub struct ValidationInfo {
    pub source_account: TezlinkImplicitAccount,
    pub balance_updates: Vec<Vec<BalanceUpdate>>,
    pub validated_operations: Vec<ManagerOperation<OperationContent>>,
}

pub fn execute_validation<Host: Runtime>(
    host: &mut Host,
    context: &Context,
    operation: tezos_tezlink::operation::Operation,
) -> Result<ValidationInfo, ValidityError> {
    let unvalidated_operation = operation
        .content
        .clone()
        .into_iter()
        .map(Into::into)
        .collect();
    let (pk, source_account) = validate_source(host, context, &unvalidated_operation)?;
    let mut balance_updates = vec![];
    for c in &unvalidated_operation {
        let (new_source_balance, op_balance_updates) =
            validate_individual_operation(host, &source_account, c)?;

        source_account
            .set_balance(host, &new_source_balance)
            .map_err(|_| ValidityError::FailedToUpdateBalance)?;

        source_account
            .increment_counter(host)
            .map_err(|_| ValidityError::FailedToIncrementCounter)?;

        balance_updates.push(op_balance_updates);
    }

    match operation.verify_signature(&pk) {
        Ok(true) => Ok(ValidationInfo {
            source_account,
            balance_updates,
            validated_operations: unvalidated_operation,
        }),
        _ => Err(ValidityError::InvalidSignature),
    }
}
