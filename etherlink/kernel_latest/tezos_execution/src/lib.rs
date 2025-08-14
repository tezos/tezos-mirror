// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use account_storage::TezlinkAccount;
use account_storage::{Manager, TezlinkImplicitAccount, TezlinkOriginatedAccount};
use address::generate_kt1;
use context::Context;
use mir::ast::{AddressHash, Entrypoint, OperationInfo, TransferTokens};
use mir::{
    ast::{IntoMicheline, Micheline},
    context::Ctx,
    parser::Parser,
};
use num_bigint::{BigInt, BigUint};
use num_traits::ops::checked::CheckedSub;
use tezos_crypto_rs::PublicKeyWithHash;
use tezos_data_encoding::types::Narith;
use tezos_evm_logging::{log, Level::*, Verbosity};
use tezos_evm_runtime::{runtime::Runtime, safe_storage::SafeStorage};
use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
use tezos_tezlink::operation::Operation;
use tezos_tezlink::operation_result::{
    produce_skipped_receipt, ApplyOperationError, ContentResult,
    InternalContentWithMetadata, InternalOperationSum, Originated, OriginationSuccess,
    TransferTarget,
};
use tezos_tezlink::{
    operation::{
        verify_signature, ManagerOperation, OperationContent, Parameter, RevealContent,
        TransferContent,
    },
    operation_result::{
        produce_operation_result, Balance, BalanceTooLow, BalanceUpdate, OperationError,
        OperationResultSum, OriginationError, RevealError, RevealSuccess, TransferError,
        TransferSuccess, UpdateOrigin, ValidityError,
    },
};
use validate::{validate_individual_operation, ValidationInfo};

extern crate alloc;
pub mod account_storage;
mod address;
pub mod context;
mod validate;

fn reveal<Host: Runtime>(
    host: &mut Host,
    provided_hash: &PublicKeyHash,
    account: &mut TezlinkImplicitAccount,
    public_key: &PublicKey,
) -> Result<RevealSuccess, RevealError> {
    log!(host, Debug, "Applying a reveal operation");
    let manager = account
        .manager(host)
        .map_err(|_| RevealError::UnretrievableManager)?;

    let expected_hash = match manager {
        Manager::Revealed(pk) => return Err(RevealError::PreviouslyRevealedKey(pk)),
        Manager::NotRevealed(pkh) => pkh,
    };

    // Ensure that the source of the operation is equal to the retrieved hash.
    if &expected_hash != provided_hash {
        return Err(RevealError::InconsistentHash(expected_hash));
    }

    // Check the public key
    let pkh_from_pk = public_key.pk_hash();
    if expected_hash != pkh_from_pk {
        return Err(RevealError::InconsistentPublicKey(expected_hash));
    }

    // Set the public key as the manager
    account
        .set_manager_public_key(host, public_key)
        .map_err(|_| RevealError::FailedToWriteManager)?;

    log!(host, Debug, "Reveal operation succeed");

    Ok(RevealSuccess {
        consumed_gas: 0_u64.into(),
    })
}

fn contract_from_address(address: AddressHash) -> Result<Contract, TransferError> {
    match address {
        AddressHash::Kt1(kt1) => Ok(Contract::Originated(kt1)),
        AddressHash::Implicit(pkh) => Ok(Contract::Implicit(pkh)),
        AddressHash::Sr1(_) => Err(TransferError::MirAddressUnsupportedError),
    }
}

fn address_from_contract(contract: Contract) -> AddressHash {
    match contract {
        Contract::Originated(kt1) => AddressHash::Kt1(kt1),
        Contract::Implicit(hash) => AddressHash::Implicit(hash),
    }
}

pub fn transfer_tez<Host: Runtime>(
    host: &mut Host,
    src_contract: &Contract,
    src_account: &mut impl TezlinkAccount,
    amount: &Narith,
    dest_contract: &Contract,
    dest_account: &mut impl TezlinkAccount,
) -> Result<TransferSuccess, TransferError> {
    let (src_update, dest_update) =
        compute_balance_updates(src_contract, dest_contract, amount)
            .map_err(|_| TransferError::FailedToComputeBalanceUpdate)?;

    apply_balance_changes(host, src_contract, src_account, dest_account, &amount.0)?;
    Ok(TransferSuccess {
        storage: None,
        lazy_storage_diff: None,
        balance_updates: vec![src_update, dest_update],
        ticket_receipt: vec![],
        originated_contracts: vec![],
        consumed_gas: 0_u64.into(),
        storage_size: 0_u64.into(),
        paid_storage_size_diff: 0_u64.into(),
        allocated_destination_contract: false,
    })
}

#[allow(clippy::too_many_arguments)]
pub fn execute_internal_operations<'a, Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    internal_operations: impl Iterator<Item = OperationInfo<'a>>,
    sender_contract: &Contract,
    sender_account: &mut TezlinkOriginatedAccount,
    parser: &'a Parser<'a>,
    ctx: &mut Ctx<'a>,
    all_internal_receipts: &mut Vec<InternalOperationSum>,
) -> Result<(), ApplyOperationError> {
    let mut failed = None;
    for (index, internal_op) in internal_operations.into_iter().enumerate() {
        log!(
            host,
            Debug,
            "Executing internal operation: {:?}",
            internal_op
        );
        let internal_receipt = match internal_op.operation {
            mir::ast::Operation::TransferTokens(TransferTokens {
                param,
                destination_address,
                amount,
            }) => {
                let amount = Narith(amount.try_into().unwrap_or(BigUint::ZERO));
                let dest_contract = contract_from_address(destination_address.hash)?;
                let value = param.into_micheline_optimized_legacy(&parser.arena);
                let encoded_value = value.encode();
                let content = TransferContent {
                    amount,
                    destination: dest_contract,
                    parameters: Some(Parameter {
                        entrypoint: destination_address.entrypoint,
                        value: encoded_value,
                    }),
                };
                let nonce = ctx.get_operation_counter().try_into().map_err(
                    |err: std::num::TryFromIntError| {
                        ApplyOperationError::InternalOperationNonceOverflow(
                            err.to_string(),
                        )
                    },
                )?;
                if failed.is_some() {
                    InternalOperationSum::Transfer(InternalContentWithMetadata {
                        content,
                        sender: sender_contract.clone(),
                        nonce,
                        result: ContentResult::Skipped,
                    })
                } else {
                    let receipt = transfer(
                        host,
                        context,
                        sender_contract,
                        sender_account,
                        &content.amount,
                        &content.destination,
                        content
                            .parameters
                            .as_ref()
                            .map_or(&Entrypoint::default(), |param| &param.entrypoint),
                        value,
                        parser,
                        ctx,
                        all_internal_receipts,
                    );
                    InternalOperationSum::Transfer(InternalContentWithMetadata {
                        content,
                        sender: sender_contract.clone(),
                        nonce,
                        result: match receipt {
                            Ok(success) => ContentResult::Applied(success.into()),
                            Err(err) => {
                                failed = Some(index);
                                ContentResult::Failed(
                                    ApplyOperationError::from(err).into(),
                                )
                            }
                        },
                    })
                }
            }
            _ => {
                return Err(ApplyOperationError::UnSupportedOperation(format!(
                    "Internal operation {:?} is not supported",
                    internal_op.operation
                )));
            }
        };
        log!(
            host,
            Debug,
            "Internal operation executed successfully: {:?}",
            internal_receipt
        );
        all_internal_receipts.push(internal_receipt);
    }
    if let Some(index) = failed {
        log!(
            host,
            Debug,
            "Internal operation execution failed at index {}",
            index
        );
        all_internal_receipts
            .iter_mut()
            .take(index)
            .for_each(InternalOperationSum::transform_result_backtrack);
    }
    Ok(())
}

/// Handles manager transfer operations for both implicit and originated contracts but with a MIR context.
#[allow(clippy::too_many_arguments)]
pub fn transfer<'a, Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    src_contract: &Contract,
    src_account: &mut impl TezlinkAccount,
    amount: &Narith,
    dest_contract: &Contract,
    entrypoint: &Entrypoint,
    param: Micheline<'a>,
    parser: &'a Parser<'a>,
    ctx: &mut Ctx<'a>,
    all_internal_receipts: &mut Vec<InternalOperationSum>,
) -> Result<TransferSuccess, TransferError> {
    match dest_contract {
        Contract::Implicit(pkh) => {
            if param != Micheline::from(()) || !entrypoint.is_default() {
                return Err(TransferError::NonSmartContractExecutionCall);
            }
            // Allocated is not being used on purpose (see below the comment on the allocated_destination_contract field)
            let _allocated =
                TezlinkImplicitAccount::allocate(host, context, dest_contract)
                    .map_err(|_| TransferError::FailedToAllocateDestination)?;
            let mut dest_account =
                TezlinkImplicitAccount::from_public_key_hash(context, pkh)
                    .map_err(|_| TransferError::FailedToFetchDestinationAccount)?;
            transfer_tez(
                host,
                src_contract,
                src_account,
                amount,
                dest_contract,
                &mut dest_account,
            )
            .map(|success| {
                TransferSuccess {
                    // This boolean is kept at false on purpose to maintain compatibility with TZKT.
                    // When transferring to a non-existent account, we need to allocate it (I/O to durable storage).
                    // This incurs a cost, and TZKT expects balance updates in the operation receipt representing this cost.
                    // So, as long as we don't have balance updates to represent this cost, we keep this boolean false.
                    allocated_destination_contract: false,
                    ..success
                }
            })
        }
        Contract::Originated(_) => {
            let mut dest_account =
                TezlinkOriginatedAccount::from_contract(context, dest_contract)
                    .map_err(|_| TransferError::FailedToFetchDestinationAccount)?;
            let receipt = transfer_tez(
                host,
                src_contract,
                src_account,
                amount,
                dest_contract,
                &mut dest_account,
            )?;
            ctx.sender = address_from_contract(src_contract.clone());
            let code = dest_account
                .code(host)
                .map_err(|_| TransferError::FailedToFetchContractCode)?;
            let storage = dest_account
                .storage(host)
                .map_err(|_| TransferError::FailedToFetchContractStorage)?;
            let (internal_operations, new_storage) =
                execute_smart_contract(code, storage, entrypoint, param, parser, ctx)?;
            dest_account
                .set_storage(host, &new_storage)
                .map_err(|_| TransferError::FailedToUpdateContractStorage)?;
            execute_internal_operations(
                host,
                context,
                internal_operations,
                dest_contract,
                &mut dest_account,
                parser,
                ctx,
                all_internal_receipts,
            )
            .map_err(|err| {
                TransferError::FailedToExecuteInternalOperation(err.to_string())
            })?;
            log!(host, Debug, "Transfer operation succeeded");
            Ok(TransferSuccess {
                storage: Some(new_storage),
                ..receipt
            })
        }
    }
}

// Handles manager transfer operations.
#[allow(clippy::too_many_arguments)]
pub fn transfer_external<Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    src: &PublicKeyHash,
    src_account: &mut TezlinkImplicitAccount,
    amount: &Narith,
    dest: &Contract,
    parameter: &Option<Parameter>,
    all_internal_receipts: &mut Vec<InternalOperationSum>,
) -> Result<TransferTarget, TransferError> {
    log!(
        host,
        Debug,
        "Applying an external transfer operation from {} to {:?} of {:?} mutez with parameters {:?}",
        src,
        dest,
        amount,
        parameter
    );

    let src_contract = Contract::Implicit(src.clone());
    let parser = Parser::new();
    let (entrypoint, value) = match parameter {
        Some(param) => (
            &param.entrypoint,
            Micheline::decode_raw(&parser.arena, &param.value)?,
        ),
        None => (&Entrypoint::default(), Micheline::from(())),
    };
    let mut ctx = Ctx::default();
    ctx.source = address_from_contract(src_contract.clone());

    transfer(
        host,
        context,
        &src_contract,
        src_account,
        amount,
        dest,
        entrypoint,
        value,
        &parser,
        &mut ctx,
        all_internal_receipts,
    )
    .map(Into::into)
}

/// Originate a contract deployed by the public key hash given in parameter. For now
/// the origination is not correctly implemented.
fn originate_contract<Host: Runtime>(
    _host: &mut Host,
    src: &PublicKeyHash,
) -> Result<OriginationSuccess, OriginationError> {
    // Generate a simple KT1 address depending on the source of the operation
    let contract = generate_kt1(src)?;
    let dummy_origination_sucess = OriginationSuccess {
        balance_updates: vec![],
        originated_contracts: vec![Originated { contract }],
        consumed_gas: 0u64.into(),
        storage_size: 0u64.into(),
        paid_storage_size_diff: 0u64.into(),
        lazy_storage_diff: None,
    };
    Ok(dummy_origination_sucess)
}

/// Prepares balance updates when accounting fees in the format expected by the Tezos operation.
fn compute_fees_balance_updates(
    source: &PublicKeyHash,
    amount: &Narith,
) -> Result<
    (BalanceUpdate, BalanceUpdate),
    num_bigint::TryFromBigIntError<num_bigint::BigInt>,
> {
    let source_delta = BigInt::from_biguint(num_bigint::Sign::Minus, amount.into());
    let block_fees = BigInt::from_biguint(num_bigint::Sign::Plus, amount.into());

    let src_update = BalanceUpdate {
        balance: Balance::Account(Contract::Implicit(source.clone())),
        changes: source_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    let block_fees = BalanceUpdate {
        balance: Balance::BlockFees,
        changes: block_fees.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    Ok((src_update, block_fees))
}

/// Prepares balance updates in the format expected by the Tezos operation.
fn compute_balance_updates(
    src: &Contract,
    dest: &Contract,
    amount: &Narith,
) -> Result<
    (BalanceUpdate, BalanceUpdate),
    num_bigint::TryFromBigIntError<num_bigint::BigInt>,
> {
    let src_delta = BigInt::from_biguint(num_bigint::Sign::Minus, amount.into());
    let dest_delta = BigInt::from_biguint(num_bigint::Sign::Plus, amount.into());

    let src_update = BalanceUpdate {
        balance: Balance::Account(src.clone()),
        changes: src_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    let dest_update = BalanceUpdate {
        balance: Balance::Account(dest.clone()),
        changes: dest_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    Ok((src_update, dest_update))
}

/// Applies balance changes by updating both source and destination accounts.
fn apply_balance_changes(
    host: &mut impl Runtime,
    src_contract: &Contract,
    src_account: &mut impl TezlinkAccount,
    dest_account: &mut impl TezlinkAccount,
    amount: &num_bigint::BigUint,
) -> Result<(), TransferError> {
    let src_balance = src_account
        .balance(host)
        .map_err(|_| TransferError::FailedToFetchSenderBalance)?;
    let new_src_balance = match src_balance.0.checked_sub(amount) {
        None => {
            log!(host, Debug, "Balance is too low");
            return Err(TransferError::BalanceTooLow(BalanceTooLow {
                contract: src_contract.clone(),
                balance: src_balance.clone(),
                amount: amount.into(),
            }));
        }
        Some(new_source_balance) => new_source_balance.into(),
    };
    src_account
        .set_balance(host, &new_src_balance)
        .map_err(|_| TransferError::FailedToApplyBalanceChanges)?;
    let dest_balance = dest_account
        .balance(host)
        .map_err(|_| TransferError::FailedToFetchDestinationBalance)?
        .0;
    let new_dest_balance = (&dest_balance + amount).into();
    dest_account
        .set_balance(host, &new_dest_balance)
        .map_err(|_| TransferError::FailedToUpdateDestinationBalance)?;

    log!(
        host,
        Debug,
        "Transfer: OK - the new balance of the source is {:?} and the new balance of the destination is {:?}",
    new_src_balance, new_dest_balance);

    Ok(())
}

/// Executes the entrypoint logic of an originated smart contract and returns the new storage.
fn execute_smart_contract<'a>(
    code: Vec<u8>,
    storage: Vec<u8>,
    entrypoint: &Entrypoint,
    value: Micheline<'a>,
    parser: &'a Parser<'a>,
    ctx: &mut Ctx<'a>,
) -> Result<(impl Iterator<Item = OperationInfo<'a>>, Vec<u8>), TransferError> {
    // Parse and typecheck the contract
    let contract_micheline = Micheline::decode_raw(&parser.arena, &code)?;
    let contract_typechecked = contract_micheline.typecheck_script(ctx)?;
    let storage_micheline = Micheline::decode_raw(&parser.arena, &storage)?;

    // Execute the contract
    let (internal_operations, new_storage) = contract_typechecked.interpret(
        ctx,
        &parser.arena,
        value,
        entrypoint,
        &storage_micheline,
    )?;

    // Encode the new storage
    let new_storage = new_storage
        .into_micheline_optimized_legacy(&parser.arena)
        .encode();

    Ok((internal_operations, new_storage))
}

fn execute_validation<Host: Runtime>(
    host: &mut Host,
    context: &Context,
    operation: Operation,
) -> Result<ValidationInfo, ValidityError> {
    let content = operation
        .content
        .into_iter()
        .map(|op| op.into())
        .collect::<Vec<ManagerOperation<OperationContent>>>();
    let (source, pk, mut source_account) =
        validate::validate_source(host, context, &content)?;
    let mut balance_updates = vec![];

    for c in &content {
        let (new_source_balance, op_balance_updates) =
            validate_individual_operation(host, &source, &source_account, c)?;

        source_account
            .set_balance(host, &new_source_balance)
            .map_err(|_| ValidityError::FailedToUpdateBalance)?;

        source_account
            .increment_counter(host)
            .map_err(|_| ValidityError::FailedToIncrementCounter)?;

        balance_updates.push(op_balance_updates);
    }

    match verify_signature(&pk, &operation.branch, content, operation.signature) {
        Ok((true, validated_operations)) => Ok(ValidationInfo {
            source,
            source_account,
            balance_updates,
            validated_operations,
        }),
        _ => Err(ValidityError::InvalidSignature),
    }
}

pub fn validate_and_apply_operation<Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    operation: Operation,
) -> Result<Vec<OperationResultSum>, OperationError> {
    let mut safe_host = SafeStorage {
        host,
        world_state: context::contracts::root(context).unwrap(),
    };

    safe_host.start()?;

    log!(safe_host, Debug, "Verifying that the batch is valid");

    let validation_info = match execute_validation(&mut safe_host, context, operation) {
        Ok(validation_info) => validation_info,
        Err(validity_err) => {
            log!(
                safe_host,
                Debug,
                "Reverting the changes because the batch is invalid."
            );
            safe_host.revert()?;
            return Err(OperationError::Validation(validity_err));
        }
    };

    log!(safe_host, Debug, "Batch is valid!");

    safe_host.promote()?;
    safe_host.promote_trace()?;
    safe_host.start()?;

    let (receipts, applied) = apply_batch(&mut safe_host, context, validation_info);

    log!(safe_host, Debug, "Receipts: {:#?}", receipts);

    if applied {
        log!(
            safe_host,
            Debug,
            "Committing the changes because the batch was successfully applied."
        );
        safe_host.promote()?;
        safe_host.promote_trace()?;
    } else {
        log!(
            safe_host,
            Debug,
            "Reverting the changes because some operation failed."
        );
        safe_host.revert()?;
    }

    Ok(receipts)
}

fn apply_batch<Host: Runtime>(
    host: &mut Host,
    context: &Context,
    validation_info: ValidationInfo,
) -> (Vec<OperationResultSum>, bool) {
    let ValidationInfo {
        source,
        mut source_account,
        balance_updates,
        validated_operations,
    } = validation_info;
    let mut first_failure: Option<usize> = None;
    let mut receipts = Vec::with_capacity(validated_operations.len());

    for (index, (content, balance_uppdate)) in validated_operations
        .into_iter()
        .zip(balance_updates)
        .enumerate()
    {
        log!(
            host,
            Debug,
            "Applying operation #{} in the batch with counter {:?}.",
            index,
            content.counter
        );
        let receipt = if first_failure.is_some() {
            log!(
                host,
                Debug,
                "Skipping this operation because we already failed on {:?}.",
                first_failure
            );
            produce_skipped_receipt(&content)
        } else {
            apply_operation(
                host,
                context,
                &content,
                &source,
                &mut source_account,
                balance_uppdate,
            )
        };

        if first_failure.is_none() && !receipt.is_applied() {
            first_failure = Some(index);
        }

        receipts.push(receipt);
    }

    if let Some(failure_idx) = first_failure {
        receipts[..failure_idx]
            .iter_mut()
            .for_each(OperationResultSum::transform_result_backtrack);
        return (receipts, false);
    }

    (receipts, true)
}

fn apply_operation<Host: Runtime>(
    host: &mut Host,
    context: &Context,
    content: &ManagerOperation<OperationContent>,
    source: &PublicKeyHash,
    source_account: &mut TezlinkImplicitAccount,
    balance_updates: Vec<BalanceUpdate>,
) -> OperationResultSum {
    let mut internal_operations_receipts = Vec::new();
    match &content.operation {
        OperationContent::Reveal(RevealContent { pk, .. }) => {
            let reveal_result = reveal(host, source, source_account, pk);
            let manager_result = produce_operation_result(
                balance_updates,
                reveal_result.map_err(Into::into),
                internal_operations_receipts,
            );
            OperationResultSum::Reveal(manager_result)
        }
        OperationContent::Transfer(TransferContent {
            amount,
            destination,
            parameters,
        }) => {
            let transfer_result = transfer_external(
                host,
                context,
                source,
                source_account,
                amount,
                destination,
                parameters,
                &mut internal_operations_receipts,
            );
            let manager_result = produce_operation_result(
                balance_updates,
                transfer_result.map_err(Into::into),
                internal_operations_receipts,
            );
            OperationResultSum::Transfer(manager_result)
        }
        OperationContent::Origination(_) => {
            let origination_result = originate_contract(host, source);
            let manager_result = produce_operation_result(
                balance_updates,
                origination_result.map_err(|e| e.into()),
                internal_operations_receipts,
            );
            OperationResultSum::Origination(manager_result)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{account_storage::TezlinkOriginatedAccount, TezlinkImplicitAccount};
    use mir::ast::{Entrypoint, Micheline};
    use pretty_assertions::assert_eq;
    use tezos_crypto_rs::hash::{ContractKt1Hash, SecretKeyEd25519};
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::types::Narith;
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};
    use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
    use tezos_tezlink::{
        block::TezBlock,
        operation::{
            sign_operation, ManagerOperation, ManagerOperationContent, Operation,
            OperationContent, Parameter, RevealContent, TransferContent,
        },
        operation_result::{
            ApplyOperationError, Balance, BalanceTooLow, BalanceUpdate, ContentResult,
            CounterError, InternalContentWithMetadata, InternalOperationSum,
            OperationResult, OperationResultSum, RevealError, RevealSuccess,
            TransferError, TransferSuccess, TransferTarget, UpdateOrigin, ValidityError,
        },
    };

    use crate::{
        account_storage::{Manager, TezlinkAccount},
        context, validate_and_apply_operation, OperationError,
    };

    #[derive(Clone)]
    struct Bootstrap {
        pkh: PublicKeyHash,
        pk: PublicKey,
        sk: SecretKeyEd25519,
    }

    fn bootstrap1() -> Bootstrap {
        Bootstrap {
            pkh: PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                .unwrap(),
            pk: PublicKey::from_b58check(
                "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
            )
            .unwrap(),
            sk: SecretKeyEd25519::from_base58_check(
                "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh",
            )
            .unwrap(),
        }
    }

    fn bootstrap2() -> Bootstrap {
        Bootstrap {
            pkh: PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN")
                .unwrap(),
            pk: PublicKey::from_b58check(
                "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9",
            )
            .unwrap(),
            sk: SecretKeyEd25519::from_base58_check(
                "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo",
            )
            .unwrap(),
        }
    }

    const CONTRACT_1: &str = "KT1EFxv88KpjxzGNu1ozh9Vta4BaV3psNknp";
    const CONTRACT_2: &str = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
    const CONTRACT_3: &str = "KT1AEfeckNbdEYwaMKkytBwPJPycz7jdSGea";

    static SCRIPT: &str = r#"
        parameter string;
        storage string;
        code {
            CAR ;
            NIL operation ;
            PAIR
        }"#;

    static UNIT_SCRIPT: &str = r#"
        parameter unit;
        storage unit;
        code {
            CAR ;
            NIL operation ;
            PAIR
        }"#;

    static FAILING_SCRIPT: &str = r#"
        parameter unit;
        storage unit;
        code {
            DROP;
            PUSH string "This contract always fails";
            FAILWITH
        }"#;

    static SCRIPT_EMITING_INTERNAL_OP: &str = r#"
        parameter (list address);
        storage unit;
        code {
            CAR;
            MAP {
                CONTRACT unit;
                IF_NONE { PUSH string "Invalid contract address"; FAILWITH } {};
                PUSH mutez 10;
                PUSH unit Unit;
                TRANSFER_TOKENS
            };
            PUSH unit Unit;
            SWAP;
            PAIR
        }"#;

    fn make_operation(
        fee: u64,
        first_counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        content: Vec<OperationContent>,
    ) -> Operation {
        let branch = TezBlock::genesis_block_hash().into();
        let content = content
            .into_iter()
            .enumerate()
            .map(|(i, c)| -> ManagerOperationContent {
                ManagerOperation {
                    source: source.pkh.clone(),
                    fee: fee.into(),
                    counter: (first_counter + i as u64).into(),
                    operation: c,
                    gas_limit: gas_limit.into(),
                    storage_limit: storage_limit.into(),
                }
                .into()
            })
            .collect::<Vec<ManagerOperationContent>>();

        let signature = sign_operation(&source.sk, &branch, &content).unwrap();

        Operation {
            branch,
            content,
            signature,
        }
    }

    fn make_reveal_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source.clone(),
            vec![OperationContent::Reveal(RevealContent {
                pk: source.pk,
                proof: None,
            })],
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn make_transfer_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        amount: Narith,
        destination: Contract,
        parameters: Option<Parameter>,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source,
            vec![OperationContent::Transfer(TransferContent {
                amount,
                destination,
                parameters,
            })],
        )
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

    fn reveal_account(host: &mut impl Runtime, source: &Bootstrap) {
        let context = context::Context::init_context();
        let mut account =
            TezlinkImplicitAccount::from_public_key_hash(&context, &source.pkh)
                .expect("Account creation should have succeed");
        account.set_manager_public_key(host, &source.pk).unwrap()
    }

    // This function sets up an account that will pass the validity checks
    fn init_contract(
        host: &mut impl Runtime,
        src: &ContractKt1Hash,
        script: &str,
        initial_storage: &str,
        balance: &Narith,
    ) -> TezlinkOriginatedAccount {
        // Setting the account in TezlinkImplicitAccount
        let contract = Contract::Originated(src.clone());

        let context = context::Context::init_context();

        let mut account = TezlinkOriginatedAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let parser = mir::parser::Parser::new();
        let script_micheline = parser.parse_top_level(script).unwrap();
        let storage_micheline = parser.parse(initial_storage).unwrap();

        account
            .set_code(host, &script_micheline.encode())
            .expect("Set code should have succeeded");

        account
            .set_storage(host, &storage_micheline.encode())
            .expect("Set storage should have succeeded");

        account
            .set_balance(host, balance)
            .expect("Set balance should have succeeded");

        account
    }

    // Test an operation on an account that has no entry in `/context/contracts/index`
    // This should fail as an EmptyImplicitContract
    #[test]
    fn apply_operation_empty_account() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        // We need to have something written in the durable storage
        // to avoid getting an error when initializing the safe_storage
        let other = bootstrap2();

        let src_account = init_account(&mut host, &other.pkh);

        let operation = make_reveal_operation(15, 1, 4, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        );

        let expected_error =
            OperationError::Validation(ValidityError::EmptyImplicitContract);

        assert_eq!(
            src_account.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented"
        );

        assert_eq!(result, Err(expected_error));
    }

    // Test that increasing the fees makes the operation fails
    #[test]
    fn apply_operation_cant_pay_fees() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let src_account = init_account(&mut host, &source.pkh);

        // Fees are too high for source's balance
        let operation = make_reveal_operation(100, 1, 4, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        );

        let expected_error =
            OperationError::Validation(ValidityError::CantPayFees(100_u64.into()));

        assert_eq!(
            src_account.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented"
        );

        assert_eq!(result, Err(expected_error));
    }

    // Test that a wrong counter should make the operation fails
    #[test]
    fn apply_operation_invalid_counter() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let src_account = init_account(&mut host, &source.pkh);

        // Counter is incoherent for source's counter
        let operation = make_reveal_operation(15, 15, 4, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        );

        let expected_error =
            OperationError::Validation(ValidityError::CounterInTheFuture(CounterError {
                expected: 1_u64.into(),
                found: 15_u64.into(),
            }));

        assert_eq!(
            src_account.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented"
        );

        assert_eq!(result, Err(expected_error));
    }

    // At this point, tests are focused on the content of the operation. We should not revert with ValidityError anymore.
    // Test a reveal operation on an already revealed account
    #[test]
    fn apply_reveal_operation_on_already_revealed_account() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let mut account = init_account(&mut host, &source.pkh);

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
        let operation = make_reveal_operation(15, 1, 4, 5, source.clone());
        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        // Reveal operation should fail
        let expected_receipt = vec![OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(source.pkh)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Failed(
                vec![RevealError::PreviouslyRevealedKey(pk).into()].into(),
            ),
            internal_operation_results: vec![],
        })];

        assert_eq!(
            account.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );

        assert_eq!(receipt, expected_receipt);
    }

    // Test an invalid reveal operation where the manager is inconsistent for source
    // (where source is different of the manager field)
    #[test]
    fn apply_reveal_operation_with_an_inconsistent_manager() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let mut account = init_account(&mut host, &source.pkh);

        // Set the an inconsistent manager with the source
        let inconsistent_pkh =
            PublicKeyHash::from_b58check("tz1UEQcU7M43yUECMpKGJcxCVwHRaP819qhN")
                .expect("PublicKeyHash b58 conversion should have succeed");

        account
            .set_manager_public_key_hash(&mut host, &inconsistent_pkh)
            .expect("Setting manager field should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, source.clone());

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(source.pkh)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Failed(
                vec![RevealError::InconsistentHash(inconsistent_pkh).into()].into(),
            ),
            internal_operation_results: vec![],
        })];

        assert_eq!(receipt, expected_receipt);
        assert_eq!(
            account.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    // Test an invalid operation where the provided public key is inconsistent for the source
    #[test]
    fn apply_reveal_operation_with_an_inconsistent_public_key() {
        let mut host = MockKernelHost::default();

        // Wrong public key for source
        let pk = PublicKey::from_b58check(
            "edpkuT1qccDweCHnvgjLuNUHERpZmEaFZfbWvTzj2BxmTgQBZjaDFD",
        )
        .expect("Public key creation should have succeed");

        let source = Bootstrap { pk, ..bootstrap1() };

        // Even if we don't use it we need to init the account
        let account = init_account(&mut host, &source.pkh);

        let operation = make_reveal_operation(15, 1, 4, 5, source.clone());

        let result = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        );

        let expected_error = OperationError::Validation(ValidityError::InvalidSignature);

        assert_eq!(
            account.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented"
        );

        assert_eq!(result, Err(expected_error));
    }

    // Test a valid reveal operation, the manager should go from NotRevealed to Revealed
    #[test]
    fn apply_reveal_operation() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let account = init_account(&mut host, &source.pkh);

        let manager = account
            .manager(&host)
            .expect("Read manager should have succeed");

        assert_eq!(manager, Manager::NotRevealed(source.pkh.clone()));

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, source.clone());

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(source.pkh)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Applied(RevealSuccess {
                consumed_gas: 0_u64.into(),
            }),
            internal_operation_results: vec![],
        })];

        assert_eq!(receipt, expected_receipt);

        let manager = account
            .manager(&host)
            .expect("Read manager should have succeed");

        assert_eq!(manager, Manager::Revealed(pk));

        assert_eq!(
            account.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    // Test an invalid transfer operation, source has not enough balance to fulfill the Transfer
    #[test]
    fn apply_transfer_with_not_enough_balance() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let dest = bootstrap2();

        // Setup accounts with 50 mutez in their balance
        let source_account = init_account(&mut host, &source.pkh);
        reveal_account(&mut host, &source);

        let destination_account = init_account(&mut host, &dest.pkh);

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            source.clone(),
            100_u64.into(),
            Contract::Implicit(dest.pkh),
            None,
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(source.pkh.clone())),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Failed(
                vec![TransferError::BalanceTooLow(BalanceTooLow {
                    contract: Contract::Implicit(source.pkh),
                    balance: 35_u64.into(),
                    amount: 100_u64.into(),
                })
                .into()]
                .into(),
            ),
            internal_operation_results: vec![],
        })];

        assert_eq!(receipt, expected_receipt);

        // Verify that source only paid the fees and the destination balance is unchanged
        assert_eq!(source_account.balance(&host).unwrap(), 35.into());
        assert_eq!(destination_account.balance(&host).unwrap(), 50_u64.into());

        assert_eq!(
            source_account.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    // Bootstrap 1 successfully transfer 30 mutez to Bootstrap 2
    #[test]
    fn apply_successful_transfer() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        let dst = bootstrap2();

        // Setup accounts with 50 mutez in their balance and reveal the source
        let source = init_account(&mut host, &src.pkh);
        reveal_account(&mut host, &src);

        let destination = init_account(&mut host, &dst.pkh);

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dst.pkh.clone()),
            None,
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                storage: None,
                lazy_storage_diff: None,
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh)),
                        changes: -30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(dst.pkh)),
                        changes: 30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                ticket_receipt: vec![],
                originated_contracts: vec![],
                consumed_gas: 0_u64.into(),
                storage_size: 0_u64.into(),
                paid_storage_size_diff: 0_u64.into(),
                allocated_destination_contract: false,
            })),
            internal_operation_results: vec![],
        })];
        assert_eq!(receipt, expected_receipt);

        // Verify that source and destination balances changed
        assert_eq!(source.balance(&host).unwrap(), 5_u64.into());
        assert_eq!(destination.balance(&host).unwrap(), 80_u64.into());

        // Verify that the source's counter has been incremented
        assert_eq!(
            source.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    // Bootstrap 1 successfully transfers 30 mutez to itself
    #[test]
    fn apply_successful_self_transfer() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        let dest = src.clone();

        // Setup account with 50 mutez in its balance
        let source = init_account(&mut host, &src.pkh);
        reveal_account(&mut host, &src);

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh.clone()),
            None,
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                storage: None,
                lazy_storage_diff: None,
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh)),
                        changes: -30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(dest.pkh)),
                        changes: 30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                ticket_receipt: vec![],
                originated_contracts: vec![],
                consumed_gas: 0_u64.into(),
                storage_size: 0_u64.into(),
                paid_storage_size_diff: 0_u64.into(),
                allocated_destination_contract: false,
            })),
            internal_operation_results: vec![],
        })];

        // Verify that balance was only debited for fees
        assert_eq!(source.balance(&host).unwrap(), 35_u64.into());

        assert_eq!(receipt, expected_receipt);

        // Verify that the source's counter has been incremented
        assert_eq!(
            source.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    #[test]
    fn apply_transfer_to_originated_faucet_with_success_receipt() {
        let mut host = MockKernelHost::default();
        let context = context::Context::init_context();
        let (requester_balance, faucet_balance, fees) = (50, 1000, 15);
        let src = bootstrap1();
        let desthash =
            ContractKt1Hash::from_base58_check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton")
                .expect("ContractKt1Hash b58 conversion should have succeeded");
        // Setup accounts with 50 mutez in their balance
        let requester = init_account(&mut host, &src.pkh);
        reveal_account(&mut host, &src);
        let (code, storage) = (
            r#"
                        parameter (mutez %fund);
                        storage unit;
                        code
                        {
                            UNPAIR;
                            SENDER;
                            CONTRACT unit;
                            IF_NONE { FAILWITH } {};
                            SWAP;
                            UNIT;
                            TRANSFER_TOKENS;
                            NIL operation;
                            SWAP;
                            CONS;
                            PAIR
                        }
            "#,
            "Unit",
        );
        let encoded_storage = mir::parser::Parser::new().parse(storage).unwrap().encode();
        let faucet = init_contract(&mut host, &desthash, code, storage, &1000.into());
        let requested_amount = 100;
        let operation = make_transfer_operation(
            fees,
            1,
            4,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(desthash.clone()),
            Some(Parameter {
                entrypoint: Entrypoint::try_from("fund")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(requested_amount as i128).encode(),
            }),
        );
        let res = validate_and_apply_operation(&mut host, &context, operation)
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            )
            .remove(0);
        assert_eq!(
            res,
            OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: 0 - fees as i64,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: fees as i64,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
                        storage: Some(encoded_storage),
                        lazy_storage_diff: None,
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone()
                                )),
                                changes: 0,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Originated(
                                    desthash.clone()
                                ),),
                                changes: 0,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_gas: 0_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: false,
                    }
                )),
                internal_operation_results: vec![InternalOperationSum::Transfer(
                    InternalContentWithMetadata {
                        content: TransferContent {
                            amount: requested_amount.into(),
                            destination: Contract::Implicit(src.pkh.clone()),
                            parameters: Some(Parameter {
                                entrypoint: Entrypoint::default(),
                                value: Micheline::from(()).encode(),
                            }),
                        },

                        sender: Contract::Originated(desthash.clone()),
                        nonce: 1,
                        result: ContentResult::Applied(TransferTarget::ToContrat(
                            TransferSuccess {
                                storage: None,
                                lazy_storage_diff: None,
                                balance_updates: vec![
                                    BalanceUpdate {
                                        balance: Balance::Account(Contract::Originated(
                                            desthash.clone()
                                        )),
                                        changes: 0 - (requested_amount as i64),
                                        update_origin: UpdateOrigin::BlockApplication,
                                    },
                                    BalanceUpdate {
                                        balance: Balance::Account(Contract::Implicit(
                                            src.pkh.clone()
                                        )),
                                        changes: requested_amount as i64,
                                        update_origin: UpdateOrigin::BlockApplication,
                                    },
                                ],
                                ticket_receipt: vec![],
                                originated_contracts: vec![],
                                consumed_gas: 0_u64.into(),
                                storage_size: 0_u64.into(),
                                paid_storage_size_diff: 0_u64.into(),
                                allocated_destination_contract: false,
                            }
                        )),
                    }
                )],
            })
        );
        println!("Result: {res:?}");
        assert_eq!(
            faucet.balance(&host).unwrap(),
            (faucet_balance - requested_amount).into()
        );
        assert_eq!(
            requester.balance(&host).unwrap(),
            (requester_balance + requested_amount - fees).into()
        ); // The faucet should have transferred 100 mutez to the source

        assert_eq!(
            requester.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );
    }

    #[test]
    fn apply_transfer_with_execution() {
        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();

        let src = bootstrap1();

        let dest = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");

        let initial_storage = "\"initial\"";

        let source = init_account(&mut host, &src.pkh);
        reveal_account(&mut host, &src);
        let destination =
            init_contract(&mut host, &dest, SCRIPT, initial_storage, &50_u64.into());

        let storage_value = parser.parse("\"Hello world\"").unwrap().encode();
        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(dest),
            Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: storage_value.clone(),
            }),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let storage = Some(storage_value.clone());

        let expected_receipt = vec![OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                storage,
                lazy_storage_diff: None,
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh)),
                        changes: -30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::Account(
                            Contract::from_b58check(CONTRACT_1).unwrap(),
                        ),
                        changes: 30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                ticket_receipt: vec![],
                originated_contracts: vec![],
                consumed_gas: 0_u64.into(),
                storage_size: 0_u64.into(),
                paid_storage_size_diff: 0_u64.into(),
                allocated_destination_contract: false,
            })),
            internal_operation_results: vec![],
        })];

        // Verify that source and destination balances changed
        // 30 for transfer + 15 for fees, 5 should be left
        assert_eq!(source.balance(&host).unwrap(), 5_u64.into());
        assert_eq!(destination.balance(&host).unwrap(), 80_u64.into());

        assert_eq!(receipt, expected_receipt);

        assert_eq!(
            source.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );

        assert_eq!(
            destination.storage(&host).unwrap(),
            storage_value,
            "Storage has not been updated"
        )
    }

    #[test]
    fn apply_transfer_with_failed_execution() {
        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();

        let src = bootstrap1();

        let dest = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");

        let initial_storage = "Unit";

        let source = init_account(&mut host, &src.pkh);
        reveal_account(&mut host, &src);

        let destination = init_contract(
            &mut host,
            &dest,
            FAILING_SCRIPT,
            initial_storage,
            &50_u64.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(dest),
            Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: parser.parse("Unit").unwrap().encode(),
            }),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src.pkh)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result:  ContentResult::Failed(vec![
                ApplyOperationError::Transfer(
            TransferError::MichelsonContractInterpretError(
                "runtime failure while running the script: failed with: String(\"This contract always fails\") of type String".into()
            )
        )].into()),
            internal_operation_results: vec![],
        })];

        // Verify that source and destination balances changed
        // Transfer should be free as it got reverted + 15 for fees, 5 should be left
        assert_eq!(source.balance(&host).unwrap(), 35_u64.into());
        assert_eq!(destination.balance(&host).unwrap(), 50_u64.into());

        assert_eq!(receipt, expected_receipt);

        assert_eq!(
            source.counter(&host).unwrap(),
            1.into(),
            "Counter should have been incremented"
        );

        assert_eq!(
            destination.storage(&host).unwrap(),
            parser.parse(initial_storage).unwrap().encode(),
            "Storage should not have been updated"
        )
    }

    #[test]
    fn apply_transfer_with_argument_to_implicit_fails() {
        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();

        let src = bootstrap1();

        let dest = bootstrap2();

        init_account(&mut host, &src.pkh);
        reveal_account(&mut host, &src);

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh),
            Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: parser.parse("0").unwrap().encode(),
            }),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src.pkh)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Failed(
                vec![ApplyOperationError::Transfer(
                    TransferError::NonSmartContractExecutionCall,
                )]
                .into(),
            ),
            internal_operation_results: vec![],
        })];

        assert_eq!(receipt, expected_receipt);
    }

    #[test]
    fn apply_transfer_with_non_default_entrypoint_to_implicit_fails() {
        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();

        let src = bootstrap1();

        let dest = bootstrap2();

        init_account(&mut host, &src.pkh);
        reveal_account(&mut host, &src);

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh),
            Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::try_from("non_default")
                    .expect("Entrypoint should be valid"),
                value: parser.parse("0").unwrap().encode(),
            }),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src.pkh)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Failed(
                vec![ApplyOperationError::Transfer(
                    TransferError::NonSmartContractExecutionCall,
                )]
                .into(),
            ),
            internal_operation_results: vec![],
        })];

        assert_eq!(receipt, expected_receipt);
    }

    #[test]
    fn apply_three_valid_operations() {
        let mut host = MockKernelHost::default();
        let ctx = context::Context::init_context();

        let src = bootstrap1();
        let dest = bootstrap2();

        // src & dest each credited with 50ꜩ
        let src_acc = init_account(&mut host, &src.pkh);
        let dest_acc = init_account(&mut host, &dest.pkh);

        // op‑1: reveal
        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: src.pk.clone(),
            proof: None,
        });

        println!("Balance: {:?}", src_acc.balance(&host).unwrap());

        // op‑2: transfer 10ꜩ to dest
        let transfer_content_1 = OperationContent::Transfer(TransferContent {
            amount: 10.into(),
            destination: Contract::Implicit(dest.pkh.clone()),
            parameters: None,
        });

        // op‑3: transfer 20ꜩ to dest
        let transfer_content_2 = OperationContent::Transfer(TransferContent {
            amount: 20.into(),
            destination: Contract::Implicit(dest.pkh.clone()),
            parameters: None,
        });

        let batch = make_operation(
            5,
            1,
            0,
            0,
            src.clone(),
            vec![reveal_content, transfer_content_1, transfer_content_2],
        );

        let receipts = validate_and_apply_operation(&mut host, &ctx, batch).unwrap();

        let expected_receipts = vec![
            OperationResultSum::Reveal(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -5,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 5,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(RevealSuccess {
                    consumed_gas: 0_u64.into(),
                }),
                internal_operation_results: vec![],
            }),
            OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -5,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 5,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
                        storage: None,
                        lazy_storage_diff: None,
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -10,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    dest.pkh.clone(),
                                )),
                                changes: 10,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_gas: 0_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: false,
                    },
                )),
                internal_operation_results: vec![],
            }),
            OperationResultSum::Transfer(OperationResult {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -5,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::BlockFees,
                        changes: 5,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
                        storage: None,
                        lazy_storage_diff: None,
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(src.pkh)),
                                changes: -20,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(dest.pkh)),
                                changes: 20,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_gas: 0_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: false,
                    },
                )),
                internal_operation_results: vec![],
            }),
        ];

        assert_eq!(receipts, expected_receipts);

        // counter updated, balances moved
        // initial_balance: 50 tez, fee amount: (3*5)tez, transfer amount: (10 + 20)tez
        assert_eq!(
            src_acc.balance(&host).unwrap(),
            5u64.into(),
            "Source account should have 5ꜩ left after fees and transfers."
        );
        assert_eq!(
            dest_acc.balance(&host).unwrap(),
            80u64.into(),
            "Destination account should have 80ꜩ after transfers."
        );

        assert_eq!(
            src_acc.counter(&host).unwrap(),
            3.into(),
            "Counter should have been incremented three times."
        );
    }

    #[test]
    fn apply_valid_then_invalid_operation_is_atomic() {
        let mut host = MockKernelHost::default();
        let ctx = context::Context::init_context();

        let src = bootstrap1();
        let dest = bootstrap2();

        // src & dest each credited with 50ꜩ
        let src_acc = init_account(&mut host, &src.pkh);
        let _dst_acc = init_account(&mut host, &dest.pkh);

        // op‑1: reveal
        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: src.pk.clone(),
            proof: None,
        });

        // op‑2: transfer 10ꜩ to dest
        let transfer_content = OperationContent::Transfer(TransferContent {
            amount: 10.into(),
            destination: Contract::Implicit(dest.pkh.clone()),
            parameters: None,
        });

        let batch = make_operation(
            100,
            1,
            0,
            0,
            src.clone(),
            vec![reveal_content, transfer_content],
        );

        let receipts = validate_and_apply_operation(&mut host, &ctx, batch);

        let expected_error =
            OperationError::Validation(ValidityError::CantPayFees(100_u64.into()));

        assert_eq!(receipts, Err(expected_error));

        assert_eq!(
            TezlinkImplicitAccount::from_public_key_hash(&ctx, &src.pkh)
                .unwrap()
                .balance(&host)
                .unwrap(),
            50u64.into()
        );

        assert_eq!(
            TezlinkImplicitAccount::from_public_key_hash(&ctx, &src.pkh)
                .unwrap()
                .manager(&host)
                .unwrap(),
            Manager::NotRevealed(src.pkh.clone())
        );

        assert_eq!(
            src_acc.counter(&host).unwrap(),
            0.into(),
            "Counter should not have been incremented."
        );
    }

    #[test]
    fn apply_smart_contract_failure_reverts_batch() {
        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();

        let src = bootstrap1();
        let src_acc = init_account(&mut host, &src.pkh);

        let fail_dest = ContractKt1Hash::from_base58_check(CONTRACT_1).unwrap();
        let succ_dest = ContractKt1Hash::from_base58_check(CONTRACT_2).unwrap();

        init_contract(&mut host, &fail_dest, FAILING_SCRIPT, "Unit", &0_u64.into());
        let succ_account =
            init_contract(&mut host, &succ_dest, SCRIPT, "\"initial\"", &0_u64.into());

        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: src.pk.clone(),
            proof: None,
        });

        let succ_transfer = OperationContent::Transfer(TransferContent {
            amount: 1.into(),
            destination: Contract::Originated(succ_dest.clone()),
            parameters: Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: parser.parse("\"Hello world\"").unwrap().encode(),
            }),
        });

        let fail_transfer = OperationContent::Transfer(TransferContent {
            amount: 1.into(),
            destination: Contract::Originated(fail_dest.clone()),
            parameters: Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: parser.parse("Unit").unwrap().encode(),
            }),
        });

        let batch = make_operation(
            10_u64,
            1,
            0,
            0,
            src.clone(),
            vec![reveal_content, succ_transfer, fail_transfer],
        );

        let receipts = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            batch,
        )
        .unwrap();

        println!("{receipts:?}");

        assert!(
            matches!(
                &receipts[0],
                OperationResultSum::Reveal(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "First receipt should be BackTracked Reveal"
        );

        assert!(
            matches!(
                &receipts[1],
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "Second receipt should be BackTracked Transfer"
        );

        assert!(
            matches!(
                &receipts[2],
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Failed(_),
                    ..
                })
            ),
            "Third receipt should be Failed Transfer"
        );

        // Storage must have reverted
        assert!(
            succ_account.storage(&host).unwrap()
                == parser.parse("\"initial\"").unwrap().encode()
        );

        assert_eq!(
            src_acc.counter(&host).unwrap(),
            3.into(),
            "Counter should have been incremented three times."
        );

        // Initial balance: 50 tez, paid in fees: (3*10)tez, transfer reverted
        assert_eq!(
            src_acc.balance(&host).unwrap(),
            20.into(),
            "Fees should have been paid for failed operation"
        )
    }

    #[test]
    fn test_internal_receipts_failure_backtrack_all() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();

        // Initialize accounts with higher balances for the test
        let mut src_acc = init_account(&mut host, &src.pkh);
        src_acc.set_balance(&mut host, &100_u64.into()).unwrap();

        // Create a script that emits internal operations to multiple targets
        let contract_chapo_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &contract_chapo_hash,
            SCRIPT_EMITING_INTERNAL_OP,
            "Unit",
            &100_u64.into(),
        );

        // Create a failing contract
        let internal_fail_contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_2)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &internal_fail_contract_hash,
            FAILING_SCRIPT,
            "Unit",
            &100_u64.into(),
        );

        // Create a successful contract
        let internal_success_contract_hash =
            ContractKt1Hash::from_base58_check(CONTRACT_3)
                .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &internal_success_contract_hash,
            UNIT_SCRIPT,
            "Unit",
            &100_u64.into(),
        );

        let success_micheline_address = Micheline::Bytes(
            Contract::Originated(internal_success_contract_hash.clone())
                .to_bytes()
                .unwrap(),
        );
        let fail_micheline_address = Micheline::Bytes(
            Contract::Originated(internal_fail_contract_hash.clone())
                .to_bytes()
                .unwrap(),
        );
        let addrs: Vec<Micheline> = vec![
            success_micheline_address.clone(),
            fail_micheline_address,
            success_micheline_address,
        ];
        let param_value = Micheline::Seq(&addrs);
        let operation = make_operation(
            10,
            1,
            0,
            0,
            src.clone(),
            vec![
                OperationContent::Reveal(RevealContent {
                    pk: src.pk.clone(),
                    proof: None,
                }),
                OperationContent::Transfer(TransferContent {
                    amount: 0.into(),
                    destination: Contract::Originated(contract_chapo_hash),
                    parameters: Some(Parameter {
                        entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                        value: param_value.encode(),
                    }),
                }),
            ],
        );
        let context = context::Context::init_context();
        let receipts = validate_and_apply_operation(&mut host, &context, operation)
            .expect(
                "validate_and_apply_operation should not have failed with a kernel error",
            );
        assert_eq!(
            receipts.len(),
            2,
            "There should be two receipts: one for reveal and one for transfer"
        );
        assert!(
            matches!(
                &receipts[0],
                OperationResultSum::Reveal(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "First receipt should be a BackTracked Reveal but is {:?}",
            receipts[0]
        );
        assert!(
            matches!(
                &receipts[1],
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "Second receipt should be a BackTracked Transfer but is {:?}",
            receipts[1]
        );

        // Check Internal Operations
        if let OperationResultSum::Transfer(OperationResult {
            internal_operation_results,
            ..
        }) = &receipts[1]
        {
            assert_eq!(
                internal_operation_results.len(),
                3,
                "There should be three internal operations"
            );

            // Check the first internal operation
            if let InternalOperationSum::Transfer(InternalContentWithMetadata {
                content: TransferContent { destination, .. },
                result:
                    ContentResult::BackTracked(TransferTarget::ToContrat(TransferSuccess {
                        balance_updates,
                        ..
                    })),
                ..
            }) = &internal_operation_results[0]
            {
                assert_eq!(
                    destination,
                    &Contract::Originated(internal_success_contract_hash.clone()),
                    "First internal operation should target the successful contract"
                );
                assert_eq!(
                    balance_updates.len(),
                    2,
                    "Balance updates should have two entries"
                );
            } else {
                panic!(
                    "First internal operation is not a transfer its {:?}",
                    internal_operation_results[0]
                );
            }

            // Check the second internal operation
            if let InternalOperationSum::Transfer(InternalContentWithMetadata {
                content: TransferContent { destination, .. },
                result: ContentResult::Failed(_),
                ..
            }) = &internal_operation_results[1]
            {
                assert_eq!(
                    destination,
                    &Contract::Originated(internal_fail_contract_hash),
                    "Second internal operation should target the failing contract"
                );
            } else {
                panic!("Second internal operation is not a transfer or does not match expected structure");
            }

            // Check the third internal operation
            if let InternalOperationSum::Transfer(InternalContentWithMetadata {
                content: TransferContent { destination, .. },
                result: ContentResult::Skipped,
                ..
            }) = &internal_operation_results[2]
            {
                assert_eq!(
                    destination,
                    &Contract::Originated(internal_success_contract_hash),
                    "Third internal operation should target the successful contract"
                );
            } else {
                panic!("Third internal operation is not a transfer or does not match expected structure");
            }
        } else {
            panic!("Second receipt is not a Transfer with Internal Operations");
        }
    }
}
