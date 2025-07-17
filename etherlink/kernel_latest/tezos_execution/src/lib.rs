// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use account_storage::TezlinkAccount;
use account_storage::{Manager, TezlinkImplicitAccount, TezlinkOriginatedAccount};
use mir::{
    ast::{IntoMicheline, Micheline},
    context::Ctx,
    parser::Parser,
};
use num_bigint::BigInt;
use num_traits::ops::checked::CheckedSub;
use tezos_crypto_rs::{base58::FromBase58CheckError, PublicKeyWithHash};
use tezos_data_encoding::enc::BinError;
use tezos_data_encoding::types::Narith;
use tezos_evm_logging::{log, Level::*, Verbosity};
use tezos_evm_runtime::{runtime::Runtime, safe_storage::SafeStorage};
use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
use tezos_tezlink::operation::Operation;
use tezos_tezlink::{
    operation::{
        ManagerOperation, OperationContent, Parameter, RevealContent, TransferContent,
    },
    operation_result::{
        is_applied, produce_operation_result, Balance, BalanceTooLow, BalanceUpdate,
        OperationError, OperationResultSum, Reveal, RevealError, RevealSuccess,
        TransferError, TransferSuccess, TransferTarget, UpdateOrigin,
    },
};
use thiserror::Error;

extern crate alloc;
pub mod account_storage;
pub mod context;
mod validate;

type ExecutionResult<A> = Result<Result<A, OperationError>, ApplyKernelError>;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ApplyKernelError {
    #[error("Host failed with a runtime error {0}")]
    HostRuntimeError(#[from] tezos_smart_rollup_host::runtime::RuntimeError),
    #[error("Apply operation failed on a storage manipulation {0}")]
    StorageError(tezos_storage::error::Error),
    #[error("Apply operation failed because of a b58 conversion {0}")]
    Base58Error(String),
    #[error("Apply operation failed because of a big integer conversion error {0}")]
    BigIntError(num_bigint::TryFromBigIntError<num_bigint::BigInt>),
    #[error("Serialization failed because of {0}")]
    BinaryError(String),
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

impl From<BinError> for ApplyKernelError {
    fn from(value: BinError) -> Self {
        Self::BinaryError(format!("{:?}", value))
    }
}

fn reveal<Host: Runtime>(
    host: &mut Host,
    provided_hash: &PublicKeyHash,
    account: &mut TezlinkImplicitAccount,
    public_key: &PublicKey,
) -> ExecutionResult<RevealSuccess> {
    log!(host, Debug, "Applying a reveal operation");
    let manager = account.manager(host)?;

    let expected_hash = match manager {
        Manager::Revealed(pk) => {
            return Ok(Err(RevealError::PreviouslyRevealedKey(pk).into()))
        }
        Manager::NotRevealed(pkh) => pkh,
    };

    // Ensure that the source of the operation is equal to the retrieved hash.
    if &expected_hash != provided_hash {
        return Ok(Err(RevealError::InconsistentHash(expected_hash).into()));
    }

    // Check the public key
    let pkh_from_pk = public_key.pk_hash();
    if expected_hash != pkh_from_pk {
        return Ok(Err(RevealError::InconsistentPublicKey(expected_hash).into()));
    }

    // Set the public key as the manager
    account.set_manager_public_key(host, public_key)?;
    account.increment_counter(host)?;

    log!(host, Debug, "Reveal operation succeed");

    Ok(Ok(RevealSuccess {
        consumed_gas: 0_u64.into(),
    }))
}

/// Handles manager transfer operations for both implicit and originated contracts.
pub fn transfer<Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    src: &PublicKeyHash,
    amount: &Narith,
    dest: &Contract,
    parameter: Option<Parameter>,
) -> ExecutionResult<TransferTarget> {
    log!(
        host,
        Debug,
        "Applying a transfer operation from {} to {:?} of {:?} mutez",
        src,
        dest,
        amount
    );

    let src_contract = Contract::Implicit(src.clone());
    let (src_update, dest_update) = compute_balance_updates(&src_contract, dest, amount)
        .map_err(ApplyKernelError::BigIntError)?;

    // Check source balance
    let mut src_account = TezlinkImplicitAccount::from_public_key_hash(context, src)?;
    let current_src_balance = src_account.balance(host)?.0;

    let new_source_balance = match current_src_balance.checked_sub(&amount.0) {
        None => {
            log!(host, Debug, "Balance is too low");
            let error = TransferError::BalanceTooLow(BalanceTooLow {
                contract: src_contract.clone(),
                balance: current_src_balance.into(),
                amount: amount.clone(),
            });
            return Ok(Err(error.into()));
        }
        Some(new_source_balance) => new_source_balance,
    };

    // Delegate to appropriate handler
    match dest {
        Contract::Implicit(dest_key_hash) => {
            if parameter.is_some() {
                return Ok(Err(TransferError::NonSmartContractExecutionCall.into()));
            }
            let allocated = TezlinkImplicitAccount::allocate(host, context, dest)?;
            let mut dest_account =
                TezlinkImplicitAccount::from_public_key_hash(context, dest_key_hash)?;
            apply_balance_changes(
                host,
                &mut src_account,
                new_source_balance.clone(),
                &mut dest_account,
                &amount.0,
            )?;

            Ok(Ok(TransferTarget::ToContrat(TransferSuccess {
                storage: None,
                lazy_storage_diff: None,
                balance_updates: vec![src_update, dest_update],
                ticket_receipt: vec![],
                originated_contracts: vec![],
                consumed_gas: 0_u64.into(),
                storage_size: 0_u64.into(),
                paid_storage_size_diff: 0_u64.into(),
                allocated_destination_contract: allocated,
            })))
        }

        Contract::Originated(_) => {
            let mut dest_contract =
                TezlinkOriginatedAccount::from_contract(context, dest)?;
            apply_balance_changes(
                host,
                &mut src_account,
                new_source_balance.clone(),
                &mut dest_contract,
                &amount.0,
            )?;

            let code = dest_contract.code(host)?;
            let storage = dest_contract.storage(host)?;

            let new_storage = execute_smart_contract(code, storage, &parameter);

            match new_storage {
                Ok(new_storage) => {
                    let _ = dest_contract.set_storage(host, &new_storage);
                    Ok(Ok(TransferTarget::ToContrat(TransferSuccess {
                        storage: Some(new_storage),
                        lazy_storage_diff: None,
                        balance_updates: vec![src_update, dest_update],
                        ticket_receipt: vec![],
                        originated_contracts: vec![],
                        consumed_gas: 0_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: false,
                    })))
                }

                Err(err) => Ok(Err(err.into())),
            }
        }
    }
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
    src_account: &mut impl TezlinkAccount,
    new_src_balance: num_bigint::BigUint,
    dest_account: &mut impl TezlinkAccount,
    amount: &num_bigint::BigUint,
) -> Result<(), ApplyKernelError> {
    src_account.set_balance(host, &new_src_balance.into())?;
    let dest_balance = dest_account.balance(host)?.0;
    let new_dest_balance = &dest_balance + amount;
    dest_account.set_balance(host, &new_dest_balance.into())?;
    Ok(())
}

/// Executes the entrypoint logic of an originated smart contract and returns the new storage and consumed gas.
fn execute_smart_contract(
    code: Vec<u8>,
    storage: Vec<u8>,
    parameter: &Option<Parameter>,
) -> Result<Vec<u8>, TransferError> {
    let parser = Parser::new();
    let contract_micheline = Micheline::decode_raw(&parser.arena, &code)?;

    let (entrypoint, value) = match parameter {
        Some(param) => (
            Some(param.entrypoint.clone()),
            Micheline::decode_raw(&parser.arena, &param.value)?,
        ),
        None => (None, Micheline::from(())),
    };

    let mut ctx = Ctx::default();
    let contract_typechecked = contract_micheline.typecheck_script(&mut ctx)?;

    let storage = Micheline::decode_raw(&parser.arena, &storage)?;

    let (_, new_storage) = contract_typechecked.interpret(
        &mut ctx,
        &parser.arena,
        value,
        entrypoint,
        storage,
    )?;

    let new_storage = new_storage
        .into_micheline_optimized_legacy(&parser.arena)
        .encode();
    Ok(new_storage)
}

pub fn apply_operation<Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    operation: Operation,
) -> Result<OperationResultSum, ApplyKernelError> {
    let manager_operation: ManagerOperation<OperationContent> =
        operation.content.clone().into();

    let source = &manager_operation.source;

    let mut safe_host = SafeStorage {
        host,
        world_state: context::contracts::root(context).unwrap(),
    };

    log!(
        safe_host,
        Debug,
        "Going to run a Tezos Manager Operation from {}",
        source
    );

    safe_host.start()?;

    let mut account = TezlinkImplicitAccount::from_public_key_hash(context, source)?;

    log!(safe_host, Debug, "Verifying that the operation is valid");

    let validity_result = validate::is_valid_tezlink_operation(
        &safe_host,
        &account,
        &operation.branch,
        operation.content.into(),
        operation.signature,
    )?;

    let new_balance = match validity_result {
        Ok(new_balance) => new_balance,
        Err(validity_err) => {
            log!(
                safe_host,
                Debug,
                "Operation is invalid, exiting apply_operation"
            );
            // TODO: Don't force the receipt to a reveal receipt
            let receipt = produce_operation_result::<Reveal>(
                vec![],
                Err(OperationError::Validation(validity_err)),
            );
            safe_host.revert()?;
            return Ok(OperationResultSum::Reveal(receipt));
        }
    };

    log!(safe_host, Debug, "Operation is valid");

    log!(safe_host, Debug, "Updates balance to pay fees");
    account.set_balance(&mut safe_host, &new_balance)?;

    let (src_delta, block_fees) =
        compute_fees_balance_updates(source, &manager_operation.fee)
            .map_err(ApplyKernelError::BigIntError)?;

    safe_host.promote()?;
    safe_host.promote_trace()?;
    safe_host.start()?;

    let receipt = match manager_operation.operation {
        OperationContent::Reveal(RevealContent { pk, proof: _ }) => {
            let reveal_result = reveal(&mut safe_host, source, &mut account, &pk)?;
            let manager_result =
                produce_operation_result(vec![src_delta, block_fees], reveal_result);
            OperationResultSum::Reveal(manager_result)
        }
        OperationContent::Transfer(TransferContent {
            amount,
            destination,
            parameters,
        }) => {
            let transfer_result = transfer(
                &mut safe_host,
                context,
                source,
                &amount,
                &destination,
                parameters,
            )?;
            let manager_result =
                produce_operation_result(vec![src_delta, block_fees], transfer_result);
            OperationResultSum::Transfer(manager_result)
        }
    };

    if is_applied(&receipt) {
        safe_host.promote()?;
        safe_host.promote_trace()?;
    } else {
        safe_host.revert()?;
    }

    Ok(receipt)
}

#[cfg(test)]
mod tests {
    use crate::{TezlinkImplicitAccount, TezlinkOriginatedAccount};
    use tezos_crypto_rs::hash::{ContractKt1Hash, SecretKeyEd25519};
    use tezos_data_encoding::types::Narith;
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};
    use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
    use tezos_tezlink::{
        block::TezBlock,
        operation::{
            sign_operation, ManagerOperation, Operation, OperationContent, Parameter,
            RevealContent, TransferContent,
        },
        operation_result::{
            ApplyOperationError, Balance, BalanceTooLow, BalanceUpdate, ContentResult,
            CounterError, OperationResult, OperationResultSum, RevealError,
            RevealSuccess, TransferError, TransferSuccess, TransferTarget, UpdateOrigin,
            ValidityError,
        },
    };

    use crate::{
        account_storage::{Manager, TezlinkAccount},
        apply_operation, context, OperationError,
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

    static SCRIPT: &str = r#"
        parameter string;
        storage string;
        code {
            CAR ;
            NIL operation ;
            PAIR
        }"#;

    fn make_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        content: OperationContent,
    ) -> Operation {
        let branch = TezBlock::genesis_block_hash().into();
        let manager_op = ManagerOperation {
            source: source.pkh,
            fee: fee.into(),
            counter: counter.into(),
            operation: content,
            gas_limit: gas_limit.into(),
            storage_limit: storage_limit.into(),
        }
        .into();

        let signature = sign_operation(&source.sk, &branch, &manager_op).unwrap();

        Operation {
            branch,
            content: manager_op,
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
            OperationContent::Reveal(RevealContent {
                pk: source.pk,
                proof: None,
            }),
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
            OperationContent::Transfer(TransferContent {
                amount,
                destination,
                parameters,
            }),
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

        init_account(&mut host, &other.pkh);

        let operation = make_reveal_operation(15, 1, 4, 5, source);

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(
                vec![OperationError::Validation(
                    ValidityError::EmptyImplicitContract,
                )]
                .into(),
            ),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test that increasing the fees makes the operation fails
    #[test]
    fn apply_operation_cant_pay_fees() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let _ = init_account(&mut host, &source.pkh);

        // Fees are too high for source's balance
        let operation = make_reveal_operation(100, 1, 4, 5, source);

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(
                vec![OperationError::Validation(ValidityError::CantPayFees(
                    100_u64.into(),
                ))]
                .into(),
            ),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test that a wrong counter should make the operation fails
    #[test]
    fn apply_operation_invalid_counter() {
        let mut host = MockKernelHost::default();

        let source = bootstrap1();

        let _ = init_account(&mut host, &source.pkh);

        // Counter is incoherent for source's counter
        let operation = make_reveal_operation(15, 15, 4, 5, source);

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(
                vec![OperationError::Validation(
                    ValidityError::CounterInTheFuture(CounterError {
                        expected: 1_u64.into(),
                        found: 15_u64.into(),
                    }),
                )]
                .into(),
            ),
            internal_operation_results: vec![],
        });
        assert_eq!(receipt, expected_receipt);
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
        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        // Reveal operation should fail
        let expected_receipt = OperationResultSum::Reveal(OperationResult {
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
                vec![OperationError::Apply(
                    RevealError::PreviouslyRevealedKey(pk).into(),
                )]
                .into(),
            ),
            internal_operation_results: vec![],
        });
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

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
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
                vec![OperationError::Apply(
                    RevealError::InconsistentHash(inconsistent_pkh).into(),
                )]
                .into(),
            ),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);
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
        let _ = init_account(&mut host, &source.pkh);

        let operation = make_reveal_operation(15, 1, 4, 5, source.clone());

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(
                vec![OperationError::Validation(ValidityError::InvalidSignature)].into(),
            ),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);
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

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
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
        });

        assert_eq!(receipt, expected_receipt);

        let manager = account
            .manager(&host)
            .expect("Read manager should have succeed");

        assert_eq!(manager, Manager::Revealed(pk));
    }

    // Test an invalid transfer operation, source has not enough balance to fullfil the Transfer
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

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Transfer(OperationResult {
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
                vec![OperationError::Apply(
                    TransferError::BalanceTooLow(BalanceTooLow {
                        contract: Contract::Implicit(source.pkh),
                        balance: 35_u64.into(),
                        amount: 100_u64.into(),
                    })
                    .into(),
                )]
                .into(),
            ),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);

        // Verify that source only paid the fees and the destination balance is unchanged
        assert_eq!(source_account.balance(&host).unwrap(), 35.into());
        assert_eq!(destination_account.balance(&host).unwrap(), 50_u64.into());
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

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Transfer(OperationResult {
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
                allocated_destination_contract: true,
            })),
            internal_operation_results: vec![],
        });
        assert_eq!(receipt, expected_receipt);

        // Verify that source and destination balances changed
        assert_eq!(source.balance(&host).unwrap(), 5_u64.into());
        assert_eq!(destination.balance(&host).unwrap(), 80_u64.into());
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

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Transfer(OperationResult {
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
                allocated_destination_contract: true,
            })),
            internal_operation_results: vec![],
        });

        // Verify that balance was only debited for fees
        assert_eq!(source.balance(&host).unwrap(), 35_u64.into());

        assert_eq!(receipt, expected_receipt);
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

        let receipt =
            apply_operation(&mut host, &context::Context::init_context(), operation)
                .expect("apply_operation should not have failed with a kernel error");

        let storage = Some(storage_value);

        let expected_receipt = OperationResultSum::Transfer(OperationResult {
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
        });

        // Verify that source and destination balances changed
        // 30 for transfer + 15 for fees, 5 should be left
        assert_eq!(source.balance(&host).unwrap(), 5_u64.into());
        assert_eq!(destination.balance(&host).unwrap(), 80_u64.into());

        assert_eq!(receipt, expected_receipt);
    }
}
