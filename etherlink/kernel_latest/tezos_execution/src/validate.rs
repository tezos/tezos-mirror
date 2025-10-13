// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use num_bigint::{BigInt, Sign, TryFromBigIntError};
use num_traits::ops::checked::CheckedSub;
use tezos_crypto_rs::PublicKeySignatureVerifier;
use tezos_data_encoding::{enc::BinError, types::Narith};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
use tezos_tezlink::{
    operation::{
        serialize_unsigned_operation, ManagerOperation, Operation, OperationContent,
        RevealContent,
    },
    operation_result::{Balance, CounterError, UpdateOrigin, ValidityError},
};

use crate::{
    account_storage::{Manager, TezlinkAccount, TezlinkImplicitAccount},
    context::Context,
    BalanceUpdate,
};

/// Inspired from `contract_storage.ml` in the Tezos protocol
/// This function verifies that the counter given in argument is the
/// successor of the stored counter. If not, it returns the appropriate
/// error.
fn check_counter_increment(
    host: &impl Runtime,
    account_counter: &Narith,
    operation_counter: &Narith,
) -> Result<(), ValidityError> {
    // The provided counter value must be the successor of the manager's counter.
    let expected_counter = Narith(&account_counter.0 + 1_u64);
    if &expected_counter == operation_counter {
        log!(
            host,
            Debug,
            "Validation: OK - Operation has the expected counter {:?}.",
            expected_counter
        );
        return Ok(());
    }
    let error = CounterError {
        expected: expected_counter,
        found: operation_counter.clone(),
    };
    if error.expected > error.found {
        log!(
            host,
            tezos_evm_logging::Level::Debug,
            "Invalid operation: Source counter is in the past"
        );
        Err(ValidityError::CounterInThePast(error))
    } else {
        log!(
            host,
            tezos_evm_logging::Level::Debug,
            "Invalid operation: Source counter is in the future"
        );
        Err(ValidityError::CounterInTheFuture(error))
    }
}

/// Prepares balance updates when accounting fees in the format expected by the Tezos operation.
fn compute_fees_balance_updates(
    source: &PublicKeyHash,
    amount: &Narith,
) -> Result<(BalanceUpdate, BalanceUpdate), TryFromBigIntError<BigInt>> {
    let source_delta = BigInt::from_biguint(Sign::Minus, amount.into());
    let block_fees = BigInt::from_biguint(Sign::Plus, amount.into());

    let source_update = BalanceUpdate {
        balance: Balance::Account(Contract::Implicit(source.clone())),
        changes: source_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    let block_fees = BalanceUpdate {
        balance: Balance::BlockFees,
        changes: block_fees.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    Ok((source_update, block_fees))
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
        _ => {
            let manager = account
                .manager(host)
                .map_err(|_| ValidityError::FailedToFetchManagerKey)?;
            match manager {
                Manager::Revealed(public_key) => Ok(public_key),
                Manager::NotRevealed(public_key_hash) => {
                    Err(ValidityError::UnrevealedManagerKey(public_key_hash))
                }
            }
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

fn validate_source<Host: Runtime>(
    host: &Host,
    context: &Context,
    content: &Vec<ManagerOperation<OperationContent>>,
) -> Result<(PublicKey, TezlinkImplicitAccount), ValidityError> {
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
    content: &ManagerOperation<OperationContent>,
    account_pkh: &PublicKeyHash,
    account_balance: &Narith,
    account_counter: &Narith,
) -> Result<(Narith, Vec<BalanceUpdate>), ValidityError> {
    check_counter_increment(host, account_counter, &content.counter)?;

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
    let new_balance = account_balance
        .0
        .checked_sub(&content.fee.0)
        .ok_or(ValidityError::CantPayFees(content.fee.clone()))?
        .into();

    log!(
        host,
        Debug,
        "Validation: OK - the source can pay {:?} in fees, being left with a new balance of {:?}.",
        &content.fee,
        account_balance
    );

    let (src_delta, block_fees) = compute_fees_balance_updates(account_pkh, &content.fee)
        .map_err(|_| ValidityError::FailedToComputeFeeBalanceUpdate)?;

    Ok((new_balance, vec![src_delta, block_fees]))
}

pub struct ValidatedOperation {
    pub balance_updates: Vec<BalanceUpdate>,
    pub content: ManagerOperation<OperationContent>,
}

pub struct ValidatedBatch {
    pub source_account: TezlinkImplicitAccount,
    pub validated_operations: Vec<ValidatedOperation>,
}

pub fn verify_signature(operation: Operation, pk: &PublicKey) -> Result<bool, BinError> {
    let serialized_unsigned_operation =
        serialize_unsigned_operation(&operation.branch, &operation.content)?;
    let signature = &operation.signature.into();
    // The verify_signature function never returns false. If the verification
    // is incorrect the function will return an Error and it's up to us to
    // transform that into a `false` boolean if we want.
    let check = pk
        .verify_signature(signature, &serialized_unsigned_operation)
        .unwrap_or(false);
    Ok(check)
}

pub fn execute_validation<Host: Runtime>(
    host: &mut Host,
    context: &Context,
    operation: tezos_tezlink::operation::Operation,
) -> Result<ValidatedBatch, ValidityError> {
    if operation.content.is_empty() {
        return Err(ValidityError::EmptyBatch);
    }

    let mut validated_operations = Vec::new();
    let unvalidated_operation: Vec<ManagerOperation<OperationContent>> = operation
        .content
        .clone()
        .into_iter()
        .map(Into::into)
        .collect();

    let (pk, source_account) = validate_source(host, context, &unvalidated_operation)?;
    match verify_signature(operation, &pk) {
        Ok(true) => log!(host, Debug, "Validation: OK - Signature is valid."),
        _ => return Err(ValidityError::InvalidSignature),
    }

    let mut source_balance = source_account
        .balance(host)
        .map_err(|_| ValidityError::FailedToFetchBalance)?;
    let mut source_counter = source_account
        .counter(host)
        .map_err(|_| ValidityError::FailedToFetchCounter)?;

    for content in unvalidated_operation {
        let (source_balance_after_fees, balance_updates) = validate_individual_operation(
            host,
            &content,
            source_account.pkh(),
            &source_balance,
            &source_counter,
        )?;
        source_counter = Narith(&source_counter.0 + 1_u64);
        source_balance = source_balance_after_fees;
        validated_operations.push(ValidatedOperation {
            balance_updates,
            content,
        });
    }

    source_account
        .set_balance(host, &source_balance)
        .map_err(|_| ValidityError::FailedToUpdateBalance)?;

    source_account
        .increment_counter(host, validated_operations.len())
        .map_err(|_| ValidityError::FailedToIncrementCounter)?;

    Ok(ValidatedBatch {
        source_account,
        validated_operations,
    })
}
