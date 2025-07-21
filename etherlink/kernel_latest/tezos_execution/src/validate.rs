// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::{hash::UnknownSignature, PublicKeySignatureVerifier};
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::{enc::BinError, types::Narith};
use tezos_evm_logging::log;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::PublicKey;
use tezos_tezlink::enc_wrappers::BlockHash;
use tezos_tezlink::operation::ManagerOperationContent;
use tezos_tezlink::{
    operation::{ManagerOperation, OperationContent, RevealContent},
    operation_result::{CounterError, ValidityError},
};

use crate::{
    account_storage::{Manager, TezlinkImplicitAccount},
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
            log!(
                host,
                tezos_evm_logging::Level::Debug,
                "Invalid operation: Source counter is in the past"
            );
            Ok(Err(ValidityError::CounterInThePast(error)))
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
            Ok(Err(ValidityError::CounterInTheFuture(error)))
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
) -> Result<Result<PublicKey, ValidityError>, ApplyKernelError> {
    match content {
        OperationContent::Reveal(RevealContent { pk, proof: _ }) => Ok(Ok(pk.clone())),
        _ => Ok(account.get_manager_key(host)?),
    }
}

fn verify_signature(
    pk: &PublicKey,
    branch: &BlockHash,
    operation: &ManagerOperationContent,
    signature: UnknownSignature,
) -> Result<bool, BinError> {
    // Watermark comes from `src/lib_crypto/signature_v2.ml`
    // The watermark for a ManagerOperation is always `Generic_operation`
    // encoded with `0x03`
    let watermark = 3_u8;

    let mut serialized_unsigned_operation = vec![watermark];

    let branch: [u8; 32] = branch.0.to_fixed_bytes();
    tezos_data_encoding::enc::put_bytes(&branch, &mut serialized_unsigned_operation);
    operation.bin_write(&mut serialized_unsigned_operation)?;

    let signature = &signature.into();

    // The verify_signature function never returns false. If the verification
    // is incorrect the function will return an Error and it's up to us to
    // transform that into a `false` boolean if we want.
    let check = pk
        .verify_signature(signature, &serialized_unsigned_operation)
        .unwrap_or(false);

    Ok(check)
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
    branch: &BlockHash,
    operation: ManagerOperation<OperationContent>,
    signature: UnknownSignature,
) -> Result<Result<Narith, ValidityError>, ApplyKernelError> {
    // Account must exist in the durable storage
    if !account.allocated(host)? {
        log!(
            host,
            tezos_evm_logging::Level::Debug,
            "Invalid operation: Source is not allocated"
        );
        return Ok(Err(ValidityError::EmptyImplicitContract));
    }

    if let Err(err) = account.check_counter_increment(host, &operation.counter)? {
        // Counter verification failed, return the error
        return Ok(Err(err));
    }

    let pk = match get_revealed_key(host, account, &operation.operation)? {
        Err(err) => {
            // Retrieve public key failed, return the error
            return Ok(Err(err));
        }
        Ok(pk) => pk,
    };

    // TODO: hard gas limit per operation is a Tezos constant, for now we took the one from ghostnet
    if let Err(err) = check_gas_limit(&1040000_u64.into(), &operation.gas_limit) {
        // Gas limit verification failed, return the error
        log!(
            host,
            tezos_evm_logging::Level::Debug,
            "Invalid operation: Gas limit is too high"
        );
        return Ok(Err(err));
    }

    // TODO: hard storage limit per operation is a Tezos constant, for now we took the one from ghostnet
    if let Err(err) = check_storage_limit(&60000_u64.into(), &operation.storage_limit) {
        // Storage limit verification failed, return the error
        log!(
            host,
            tezos_evm_logging::Level::Debug,
            "Invalid operation: Storage limit is too high"
        );
        return Ok(Err(err));
    }

    // The manager account must be solvent to pay the announced fees.
    let new_balance = match account.simulate_spending(host, &operation.fee)? {
        Some(new_balance) => new_balance,
        None => {
            log!(
                host,
                tezos_evm_logging::Level::Debug,
                "Invalid operation: Can't pay the fees"
            );
            return Ok(Err(ValidityError::CantPayFees(operation.fee)));
        }
    };

    let verify = verify_signature(&pk, branch, &operation.into(), signature)?;

    if verify {
        Ok(Ok(new_balance))
    } else {
        log!(
            host,
            tezos_evm_logging::Level::Debug,
            "Invalid operation: Signature is invalid"
        );
        Ok(Err(ValidityError::InvalidSignature))
    }
}
