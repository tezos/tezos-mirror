// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use account_storage::TezlinkAccount;
use account_storage::{Manager, TezlinkImplicitAccount, TezlinkOriginatedAccount};
use context::Context;
use mir::ast::{AddressHash, Entrypoint, OperationInfo, PublicKeyHash, TransferTokens};
use mir::{
    ast::{IntoMicheline, Micheline},
    context::CtxTrait,
    gas::Gas,
    parser::Parser,
};
use num_bigint::{BigInt, BigUint};
use num_traits::ops::checked::CheckedMul;
use num_traits::ops::checked::CheckedSub;
use std::collections::HashMap;
use tezos_crypto_rs::{
    hash::{ChainId, ContractKt1Hash},
    PublicKeyWithHash,
};
use tezos_data_encoding::types::Narith;
use tezos_evm_logging::{log, Level::*, Verbosity};
use tezos_evm_runtime::{runtime::Runtime, safe_storage::SafeStorage};
use tezos_smart_rollup::types::{Contract, PublicKey, Timestamp};
use tezos_tezlink::enc_wrappers::{BlockNumber, OperationHash};
use tezos_tezlink::operation::{Operation, OriginationContent, Script};
use tezos_tezlink::operation_result::{
    produce_skipped_receipt, ApplyOperationError, ContentResult,
    InternalContentWithMetadata, InternalOperationSum, Originated, OriginationSuccess,
    TransferTarget,
};
use tezos_tezlink::{
    operation::{
        ManagerOperation, OperationContent, Parameter, RevealContent, TransferContent,
    },
    operation_result::{
        produce_operation_result, Balance, BalanceTooLow, BalanceUpdate, OperationError,
        OperationResultSum, OriginationError, RevealError, RevealSuccess, TransferError,
        TransferSuccess, UpdateOrigin,
    },
};

use crate::address::OriginationNonce;

extern crate alloc;
pub mod account_storage;
mod address;
pub mod context;
mod validate;

fn reveal<Host: Runtime>(
    host: &mut Host,
    source_account: &TezlinkImplicitAccount,
    public_key: &PublicKey,
) -> Result<RevealSuccess, RevealError> {
    log!(host, Debug, "Applying a reveal operation");
    let manager = source_account
        .manager(host)
        .map_err(|_| RevealError::UnretrievableManager)?;

    let expected_hash = match manager {
        Manager::Revealed(pk) => return Err(RevealError::PreviouslyRevealedKey(pk)),
        Manager::NotRevealed(pkh) => pkh,
    };

    // Ensure that the source of the operation is equal to the retrieved hash.
    if &expected_hash != source_account.pkh() {
        return Err(RevealError::InconsistentHash(expected_hash));
    }

    // Check the public key
    let pkh_from_pk = public_key.pk_hash();
    if expected_hash != pkh_from_pk {
        return Err(RevealError::InconsistentPublicKey(expected_hash));
    }

    // Set the public key as the manager
    source_account
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

fn transfer_tez<Host: Runtime>(
    host: &mut Host,
    giver_account: &impl TezlinkAccount,
    amount: &Narith,
    receiver_account: &impl TezlinkAccount,
) -> Result<TransferSuccess, TransferError> {
    let balance_updates =
        compute_balance_updates(giver_account, receiver_account, amount)
            .map_err(|_| TransferError::FailedToComputeBalanceUpdate)?;

    apply_balance_changes(host, giver_account, receiver_account, &amount.0)?;
    Ok(TransferSuccess {
        storage: None,
        lazy_storage_diff: None,
        balance_updates,
        ticket_receipt: vec![],
        originated_contracts: vec![],
        consumed_gas: 0_u64.into(),
        storage_size: 0_u64.into(),
        paid_storage_size_diff: 0_u64.into(),
        allocated_destination_contract: false,
    })
}

fn burn_tez(
    host: &mut impl Runtime,
    account: &impl TezlinkAccount,
    amount: &num_bigint::BigUint,
) -> Result<Narith, TransferError> {
    let balance = account
        .balance(host)
        .map_err(|_| TransferError::FailedToFetchSenderBalance)?;
    let new_balance = match balance.0.checked_sub(amount) {
        None => {
            log!(host, Debug, "Balance is too low");
            return Err(TransferError::BalanceTooLow(BalanceTooLow {
                contract: account.contract(),
                balance: balance.clone(),
                amount: amount.into(),
            }));
        }
        Some(new_balance) => new_balance.into(),
    };
    account
        .set_balance(host, &new_balance)
        .map_err(|_| TransferError::FailedToApplyBalanceChanges)?;
    Ok(new_balance)
}

#[allow(clippy::too_many_arguments)]
fn execute_internal_operations<'a, Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    internal_operations: impl Iterator<Item = OperationInfo<'a>>,
    sender_account: &TezlinkOriginatedAccount,
    parser: &'a Parser<'a>,
    source_account: &TezlinkImplicitAccount,
    gas: &mut Gas,
    operation_counter: &mut u128,
    all_internal_receipts: &mut Vec<InternalOperationSum>,
    origination_nonce: &mut OriginationNonce,
    level: &BlockNumber,
    now: &Timestamp,
    chain_id: &ChainId,
) -> Result<(), ApplyOperationError> {
    let mut failed = None;
    for (index, OperationInfo { operation, counter }) in
        internal_operations.into_iter().enumerate()
    {
        log!(
            host,
            Debug,
            "Executing internal operation {:?} with counter {:?}",
            operation,
            counter
        );
        let nonce = counter
            .try_into()
            .map_err(|err: std::num::TryFromIntError| {
                ApplyOperationError::InternalOperationNonceOverflow(err.to_string())
            })?;
        let internal_receipt = match operation {
            mir::ast::Operation::TransferTokens(TransferTokens {
                param,
                destination_address,
                amount,
            }) => {
                let amount = Narith(amount.try_into().map_err(
                    |err: num_bigint::TryFromBigIntError<()>| {
                        TransferError::MirAmountToNarithError(err.to_string())
                    },
                )?);
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
                if failed.is_some() {
                    InternalOperationSum::Transfer(InternalContentWithMetadata {
                        content,
                        sender: sender_account.contract(),
                        nonce,
                        result: ContentResult::Skipped,
                    })
                } else {
                    let receipt = transfer(
                        host,
                        context,
                        sender_account,
                        &content.amount,
                        &content.destination,
                        content
                            .parameters
                            .as_ref()
                            .map_or(&Entrypoint::default(), |param| &param.entrypoint),
                        value,
                        parser,
                        source_account,
                        gas,
                        operation_counter,
                        all_internal_receipts,
                        origination_nonce,
                        level,
                        now,
                        chain_id,
                    );
                    InternalOperationSum::Transfer(InternalContentWithMetadata {
                        content,
                        sender: sender_account.contract(),
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
            mir::ast::Operation::CreateContract(mir::ast::CreateContract {
                delegate,
                amount,
                storage,
                code: _,
                micheline_code,
                address,
            }) => {
                let amount = Narith(amount.try_into().unwrap_or(BigUint::ZERO));
                let script = Script {
                    code: micheline_code.encode(),
                    storage: storage
                        .into_micheline_optimized_legacy(&parser.arena)
                        .encode(),
                };
                if failed.is_some() {
                    InternalOperationSum::Origination(InternalContentWithMetadata {
                        content: OriginationContent {
                            balance: amount,
                            delegate,
                            script,
                        },
                        sender: sender_account.contract(),
                        nonce,
                        result: ContentResult::Skipped,
                    })
                } else {
                    let receipt = originate_contract(
                        host,
                        context,
                        address,
                        source_account,
                        sender_account,
                        &amount,
                        &script,
                        false, // We skip typechecking for internal operations since CREATE_CONTRACT already typechecks the script and the initial storage.
                    );
                    InternalOperationSum::Origination(InternalContentWithMetadata {
                        content: OriginationContent {
                            balance: amount,
                            delegate,
                            script,
                        },
                        sender: sender_account.contract(),
                        nonce,
                        result: match receipt {
                            Ok(success) => ContentResult::Applied(success),
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
            mir::ast::Operation::SetDelegate(set_delegate) => {
                return Err(ApplyOperationError::UnSupportedSetDelegate(format!(
                    "{set_delegate:?}"
                )));
            }
            mir::ast::Operation::Emit(emit) => {
                return Err(ApplyOperationError::UnSupportedEmit(format!("{emit:?}")));
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

struct Ctx<'a, Host, Context, Gas, OpCounter, OrigNonce> {
    host: Host,
    context: Context,
    sender: AddressHash,
    amount: i64,
    self_address: AddressHash,
    balance: i64,
    level: BlockNumber,
    now: Timestamp,
    chain_id: ChainId,

    source: PublicKeyHash,
    gas: Gas,
    operation_counter: OpCounter,
    origination_nonce: OrigNonce,

    lazy_storage: mir::ast::big_map::InMemoryLazyStorage<'a>,
}

impl<'a, Host: Runtime> CtxTrait<'a>
    for Ctx<'a, &mut Host, &Context, &mut mir::gas::Gas, &mut u128, &mut OriginationNonce>
{
    type BigMapStorage = mir::ast::big_map::InMemoryLazyStorage<'a>;

    fn sender(&self) -> AddressHash {
        self.sender.clone()
    }

    fn source(&self) -> PublicKeyHash {
        self.source.clone()
    }

    fn amount(&self) -> i64 {
        self.amount
    }

    fn self_address(&self) -> AddressHash {
        self.self_address.clone()
    }

    fn balance(&self) -> i64 {
        self.balance
    }

    fn gas(&mut self) -> &mut mir::gas::Gas {
        self.gas
    }

    fn level(&self) -> BigUint {
        self.level.block_number.into()
    }

    fn min_block_time(&self) -> BigUint {
        1u32.into()
    }

    fn chain_id(&self) -> mir::ast::ChainId {
        self.chain_id.clone()
    }

    fn voting_power(&self, _: &PublicKeyHash) -> BigUint {
        0u32.into()
    }

    fn now(&self) -> num_bigint::BigInt {
        i64::from(self.now).into()
    }

    fn total_voting_power(&self) -> BigUint {
        1u32.into()
    }

    fn operation_group_hash(&self) -> [u8; 32] {
        self.origination_nonce.operation.0 .0
    }

    fn big_map_storage(&mut self) -> &mut mir::ast::big_map::InMemoryLazyStorage<'a> {
        &mut self.lazy_storage
    }

    fn origination_counter(&mut self) -> u32 {
        let c: &mut u32 = &mut self.origination_nonce.index;
        *c += 1;
        *c
    }

    fn operation_counter(&mut self) -> u128 {
        let c: &mut u128 = self.operation_counter;
        *c += 1;
        *c
    }

    fn lookup_contract(
        &self,
        address: &AddressHash,
    ) -> Option<std::collections::HashMap<mir::ast::Entrypoint, mir::ast::Type>> {
        get_contract_entrypoint(self.host, self.context, address)
    }
}

/// Handles manager transfer operations for both implicit and originated contracts but with a MIR context.
#[allow(clippy::too_many_arguments)]
fn transfer<'a, Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    sender_account: &impl TezlinkAccount,
    amount: &Narith,
    dest_contract: &Contract,
    entrypoint: &Entrypoint,
    param: Micheline<'a>,
    parser: &'a Parser<'a>,
    source_account: &TezlinkImplicitAccount,
    gas: &mut Gas,
    operation_counter: &mut u128,
    all_internal_receipts: &mut Vec<InternalOperationSum>,
    origination_nonce: &mut OriginationNonce,
    level: &BlockNumber,
    now: &Timestamp,
    chain_id: &ChainId,
) -> Result<TransferSuccess, TransferError> {
    match dest_contract {
        Contract::Implicit(pkh) => {
            if param != Micheline::from(()) || !entrypoint.is_default() {
                return Err(TransferError::NonSmartContractExecutionCall);
            }
            // Transfers of 0 tez to an implicit contract are rejected.
            if amount.eq(&0_u64.into()) {
                return Err(TransferError::EmptyImplicitTransfer);
            };

            let dest_account = TezlinkImplicitAccount::from_public_key_hash(context, pkh)
                .map_err(|_| TransferError::FailedToFetchDestinationAccount)?;
            // Allocated is not being used on purpose (see below the comment on the allocated_destination_contract field)
            let _allocated = dest_account
                .allocate(host)
                .map_err(|_| TransferError::FailedToAllocateDestination)?;
            transfer_tez(host, sender_account, amount, &dest_account).map(|success| {
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
        Contract::Originated(kt1) => {
            let dest_account = TezlinkOriginatedAccount::from_kt1(context, kt1)
                .map_err(|_| TransferError::FailedToFetchDestinationAccount)?;
            let receipt = transfer_tez(host, sender_account, amount, &dest_account)?;
            let sender = address_from_contract(sender_account.contract());
            let amount = amount.0.clone().try_into().map_err(
                |err: num_bigint::TryFromBigIntError<num_bigint::BigUint>| {
                    TransferError::MirAmountToNarithError(err.to_string())
                },
            )?;
            let self_address = address_from_contract(dest_contract.clone());
            let balance = dest_account
                .balance(host)
                .map_err(|_| TransferError::FailedToFetchSenderBalance)?;
            let balance = balance.0.try_into().map_err(
                |err: num_bigint::TryFromBigIntError<num_bigint::BigUint>| {
                    TransferError::MirAmountToNarithError(err.to_string())
                },
            )?;
            let code = dest_account
                .code(host)
                .map_err(|_| TransferError::FailedToFetchContractCode)?;
            let storage = dest_account
                .storage(host)
                .map_err(|_| TransferError::FailedToFetchContractStorage)?;
            let mut ctx = Ctx {
                host,
                context,
                gas,
                source: source_account.pkh().clone(),
                sender,
                amount,
                self_address,
                balance,
                operation_counter,
                origination_nonce,
                level: *level,
                now: *now,
                chain_id: chain_id.clone(),
                lazy_storage: mir::ast::big_map::InMemoryLazyStorage::new(),
            };
            let (internal_operations, new_storage) = execute_smart_contract(
                code, storage, entrypoint, param, parser, &mut ctx,
            )?;
            let host = ctx.host;
            let gas = ctx.gas;
            dest_account
                .set_storage(host, &new_storage)
                .map_err(|_| TransferError::FailedToUpdateContractStorage)?;
            execute_internal_operations(
                host,
                context,
                internal_operations,
                &dest_account,
                parser,
                source_account,
                gas,
                ctx.operation_counter,
                all_internal_receipts,
                ctx.origination_nonce,
                level,
                now,
                chain_id,
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

fn get_contract_entrypoint(
    host: &impl Runtime,
    context: &context::Context,
    address: &AddressHash,
) -> Option<HashMap<mir::ast::Entrypoint, mir::ast::Type>> {
    let contract = contract_from_address(address.clone()).ok()?;
    let contract_account =
        TezlinkOriginatedAccount::from_contract(context, &contract).ok()?;
    let code = contract_account.code(host).ok()?;
    let parser = Parser::new();
    let micheline = Micheline::decode_raw(&parser.arena, &code).ok()?;
    // Note: to typecheck a script, MIR only uses the ctx argument to
    // consume gas, so it's not a problem to use default values here
    // except the gas field which should be propagated here.
    // TODO (Linear issue L2-383): handle gas consumption here.
    let typechecked = micheline
        .typecheck_script(&mut mir::context::Ctx::default())
        .ok()?;
    let entrypoints_annotations = typechecked.annotations;
    // Cast  the entry_points_annotations to the expected type
    let entrypoints = entrypoints_annotations
        .into_iter()
        .filter_map(|(field_annotation, (_, ty))| {
            mir::ast::Entrypoint::try_from(field_annotation)
                .ok()
                .map(|entrypoint| (entrypoint, ty.clone()))
        })
        .collect();
    Some(entrypoints)
}

// Handles manager transfer operations.
#[allow(clippy::too_many_arguments)]
fn transfer_external<Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    source_account: &TezlinkImplicitAccount,
    amount: &Narith,
    dest: &Contract,
    parameter: &Option<Parameter>,
    all_internal_receipts: &mut Vec<InternalOperationSum>,
    origination_nonce: &mut OriginationNonce,
    level: &BlockNumber,
    now: &Timestamp,
    chain_id: &ChainId,
) -> Result<TransferTarget, TransferError> {
    log!(
        host,
        Debug,
        "Applying an external transfer operation from {} to {:?} of {:?} mutez with parameters {:?}",
        source_account.pkh(),
        dest,
        amount,
        parameter
    );

    let parser = Parser::new();
    let (entrypoint, value) = match parameter {
        Some(param) => (
            &param.entrypoint,
            Micheline::decode_raw(&parser.arena, &param.value)?,
        ),
        None => (&Entrypoint::default(), Micheline::from(())),
    };

    transfer(
        host,
        context,
        source_account,
        amount,
        dest,
        entrypoint,
        value,
        &parser,
        source_account,
        &mut mir::gas::Gas::default(),
        &mut 0,
        all_internal_receipts,
        origination_nonce,
        level,
        now,
        chain_id,
    )
    .map(Into::into)
}

// Values from src/proto_023_PtSeouLo/lib_parameters/default_parameters.ml.
const ORIGINATION_SIZE: u64 = 257;
const COST_PER_BYTES: u64 = 250;
const ORIGINATION_COST: u64 = ORIGINATION_SIZE * COST_PER_BYTES;

/// Originate a contract deployed by the public key hash given in parameter. For now
/// the origination is not correctly implemented.
#[allow(clippy::too_many_arguments)]
fn originate_contract<Host: Runtime>(
    host: &mut Host,
    context: &Context,
    contract: ContractKt1Hash,
    source_account: &TezlinkImplicitAccount,
    sender_account: &impl TezlinkAccount,
    initial_balance: &Narith,
    script: &Script,
    typecheck: bool,
) -> Result<OriginationSuccess, OriginationError> {
    if typecheck {
        let parser = Parser::new();
        let mut ctx = mir::context::Ctx::default();
        let contract_micheline = Micheline::decode_raw(&parser.arena, &script.code)
            .map_err(|e| OriginationError::MichelineDecodeError(e.to_string()))?;
        let contract_typechecked =
            contract_micheline.typecheck_script(&mut ctx).map_err(|e| {
                OriginationError::MirTypecheckingError(format!("Script : {e}"))
            })?;
        let storage_micheline = Micheline::decode_raw(&parser.arena, &script.storage)
            .map_err(|e| OriginationError::MichelineDecodeError(e.to_string()))?;
        contract_typechecked
            .typecheck_storage(&mut ctx, &storage_micheline)
            .map_err(|e| {
                OriginationError::MirTypecheckingError(format!("Storage : {e}"))
            })?;
    }

    // TODO: Handle lazy_storage diff, a lot of the origination is concerned

    // Set the storage of the contract
    let smart_contract = TezlinkOriginatedAccount::from_kt1(context, &contract)
        .map_err(|_| OriginationError::FailedToFetchOriginated)?;

    let total_size = smart_contract
        .init(host, &script.code, &script.storage)
        .map_err(|_| OriginationError::CantInitContract)?;

    // There's this line in the origination `assert (Compare.Z.(total_size >= Z.zero)) ;`
    // This error is unreachable because of the simulation, but as we don't have simulation
    // yet it's possible.
    if total_size.eq(&0u64.into()) {
        return Err(OriginationError::CantOriginateEmptyContract);
    }

    // Compute the initial_balance setup of the smart contract as a balance update for the origination.
    let mut balance_updates =
        compute_balance_updates(sender_account, &smart_contract, initial_balance)
            .map_err(|_| OriginationError::FailedToComputeBalanceUpdate)?;

    // Balance updates for the impacts of origination on storage space.
    // storage_fees = total_size * COST_PER_BYTES
    let storage_fees = BigUint::from(total_size.clone())
        .checked_mul(&BigUint::from(COST_PER_BYTES))
        .ok_or(OriginationError::FailedToComputeBalanceUpdate)?;
    let storage_fees_balance_updates =
        compute_storage_balance_updates(source_account, storage_fees.clone())
            .map_err(|_| OriginationError::FailedToComputeBalanceUpdate)?;
    balance_updates.extend(storage_fees_balance_updates);

    // Balance updates for the base origination cost.
    let origination_fees_balance_updates =
        compute_storage_balance_updates(source_account, ORIGINATION_COST.into())
            .map_err(|_| OriginationError::FailedToComputeBalanceUpdate)?;
    balance_updates.extend(origination_fees_balance_updates);

    // Apply the balance change, accordingly to the balance updates computed
    apply_balance_changes(host, sender_account, &smart_contract, &initial_balance.0)
        .map_err(|_| OriginationError::FailedToApplyBalanceUpdate)?;

    let _ = burn_tez(host, source_account, &(ORIGINATION_COST + storage_fees))
        .map_err(|_| OriginationError::FailedToApplyBalanceUpdate)?;

    let dummy_origination_sucess = OriginationSuccess {
        balance_updates,
        originated_contracts: vec![Originated { contract }],
        consumed_gas: 0u64.into(),
        // TODO https://linear.app/tezos/issue/L2-325/fix-storage-size-and-paid-diff-at-origination
        // These are probably not the right values for storage_size and
        // paid_storage_size_diff, but having something different than 0
        // participates in having the TzKT front-end not crash when originating.
        storage_size: total_size.clone().into(),
        paid_storage_size_diff: total_size.into(),
        lazy_storage_diff: None,
    };
    Ok(dummy_origination_sucess)
}

/// Prepares balance updates when accounting fees in the format expected by the Tezos operation.
fn compute_fees_balance_updates(
    source: &TezlinkImplicitAccount,
    amount: &Narith,
) -> Result<
    (BalanceUpdate, BalanceUpdate),
    num_bigint::TryFromBigIntError<num_bigint::BigInt>,
> {
    let source_delta = BigInt::from_biguint(num_bigint::Sign::Minus, amount.into());
    let block_fees = BigInt::from_biguint(num_bigint::Sign::Plus, amount.into());

    let source_update = BalanceUpdate {
        balance: Balance::Account(source.contract()),
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

/// Prepares balance updates in the format expected by the Tezos operation.
fn compute_balance_updates(
    giver: &impl TezlinkAccount,
    receiver: &impl TezlinkAccount,
    amount: &Narith,
) -> Result<Vec<BalanceUpdate>, num_bigint::TryFromBigIntError<num_bigint::BigInt>> {
    if amount.eq(&0_u64.into()) {
        return Ok(vec![]);
    };

    let giver_delta = BigInt::from_biguint(num_bigint::Sign::Minus, amount.into());
    let receiver_delta = BigInt::from_biguint(num_bigint::Sign::Plus, amount.into());

    let giver_update = BalanceUpdate {
        balance: Balance::Account(giver.contract()),
        changes: giver_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    let receiver_update = BalanceUpdate {
        balance: Balance::Account(receiver.contract()),
        changes: receiver_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    Ok(vec![giver_update, receiver_update])
}

/// Prepares balance updates when accounting storage fees in the format expected by the Tezos operation.
fn compute_storage_balance_updates(
    source: &TezlinkImplicitAccount,
    fee: BigUint,
) -> Result<Vec<BalanceUpdate>, num_bigint::TryFromBigIntError<num_bigint::BigInt>> {
    let source_delta = BigInt::from_biguint(num_bigint::Sign::Minus, fee.clone());
    let block_fees = BigInt::from_biguint(num_bigint::Sign::Plus, fee);

    let source_update = BalanceUpdate {
        balance: Balance::Account(source.contract()),
        changes: source_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    let block_fees = BalanceUpdate {
        balance: Balance::StorageFees,
        changes: block_fees.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    Ok(vec![source_update, block_fees])
}

/// Applies balance changes by updating both source and destination accounts.
fn apply_balance_changes(
    host: &mut impl Runtime,
    giver_account: &impl TezlinkAccount,
    receiver_account: &impl TezlinkAccount,
    amount: &num_bigint::BigUint,
) -> Result<(), TransferError> {
    let giver_balance = giver_account
        .balance(host)
        .map_err(|_| TransferError::FailedToFetchSenderBalance)?;
    let new_giver_balance = match giver_balance.0.checked_sub(amount) {
        None => {
            log!(host, Debug, "Balance is too low");
            return Err(TransferError::BalanceTooLow(BalanceTooLow {
                contract: giver_account.contract(),
                balance: giver_balance,
                amount: amount.into(),
            }));
        }
        Some(new_source_balance) => new_source_balance.into(),
    };
    giver_account
        .set_balance(host, &new_giver_balance)
        .map_err(|_| TransferError::FailedToApplyBalanceChanges)?;
    let receiver_balance = receiver_account
        .balance(host)
        .map_err(|_| TransferError::FailedToFetchDestinationBalance)?
        .0;
    let new_receiver_balance = (&receiver_balance + amount).into();
    receiver_account
        .set_balance(host, &new_receiver_balance)
        .map_err(|_| TransferError::FailedToUpdateDestinationBalance)?;

    log!(
        host,
        Debug,
        "Transfer: OK - the new balance of the giver is {:?} and the new balance of the receiver is {:?}",
    new_giver_balance, new_receiver_balance);

    Ok(())
}

/// Executes the entrypoint logic of an originated smart contract and returns the new storage.
fn execute_smart_contract<'a>(
    code: Vec<u8>,
    storage: Vec<u8>,
    entrypoint: &Entrypoint,
    value: Micheline<'a>,
    parser: &'a Parser<'a>,
    ctx: &mut impl CtxTrait<'a>,
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

pub fn validate_and_apply_operation<Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    hash: OperationHash,
    operation: Operation,
    level: &BlockNumber,
    now: &Timestamp,
    chain_id: &ChainId,
) -> Result<Vec<OperationResultSum>, OperationError> {
    let mut safe_host = SafeStorage {
        host,
        world_state: context::contracts::root(context).unwrap(),
    };

    safe_host.start()?;

    log!(safe_host, Debug, "Verifying that the batch is valid");

    let validation_info =
        match validate::execute_validation(&mut safe_host, context, operation) {
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

    let mut origination_nonce = OriginationNonce::initial(hash);
    let (receipts, applied) = apply_batch(
        &mut safe_host,
        context,
        &mut origination_nonce,
        validation_info,
        level,
        now,
        chain_id,
    );

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
    origination_nonce: &mut OriginationNonce,
    validation_info: validate::ValidationInfo,
    level: &BlockNumber,
    now: &Timestamp,
    chain_id: &ChainId,
) -> (Vec<OperationResultSum>, bool) {
    let validate::ValidationInfo {
        source_account,
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
                origination_nonce,
                &content,
                &source_account,
                balance_uppdate,
                level,
                now,
                chain_id,
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

#[allow(clippy::too_many_arguments)]
fn apply_operation<Host: Runtime>(
    host: &mut Host,
    context: &Context,
    origination_nonce: &mut OriginationNonce,
    content: &ManagerOperation<OperationContent>,
    source_account: &TezlinkImplicitAccount,
    balance_updates: Vec<BalanceUpdate>,
    level: &BlockNumber,
    now: &Timestamp,
    chain_id: &ChainId,
) -> OperationResultSum {
    let mut internal_operations_receipts = Vec::new();
    match &content.operation {
        OperationContent::Reveal(RevealContent { pk, .. }) => {
            let reveal_result = reveal(host, source_account, pk);
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
                source_account,
                amount,
                destination,
                parameters,
                &mut internal_operations_receipts,
                origination_nonce,
                level,
                now,
                chain_id,
            );
            let manager_result = produce_operation_result(
                balance_updates,
                transfer_result.map_err(Into::into),
                internal_operations_receipts,
            );
            OperationResultSum::Transfer(manager_result)
        }
        OperationContent::Origination(OriginationContent {
            ref balance,
            delegate: _,
            ref script,
        }) => {
            let address = origination_nonce.generate_kt1();
            let origination_result = originate_contract(
                host,
                context,
                address,
                source_account,
                source_account,
                balance,
                script,
                true,
            );
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
    use crate::{
        account_storage::TezlinkOriginatedAccount, address::OriginationNonce,
        TezlinkImplicitAccount,
    };
    use mir::ast::{Entrypoint, Micheline};
    use num_traits::ops::checked::CheckedSub;
    use pretty_assertions::assert_eq;
    use primitive_types::H256;
    use tezos_crypto_rs::hash::{ChainId, ContractKt1Hash, HashTrait, SecretKeyEd25519};
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::types::Narith;
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};
    use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
    use tezos_tezlink::{
        block::TezBlock,
        enc_wrappers::OperationHash,
        operation::{
            sign_operation, ManagerOperation, ManagerOperationContent, Operation,
            OperationContent, OriginationContent, Parameter, RevealContent, Script,
            TransferContent,
        },
        operation_result::{
            ApplyOperationError, ApplyOperationErrors, BacktrackedResult, Balance,
            BalanceTooLow, BalanceUpdate, ContentResult, CounterError,
            InternalContentWithMetadata, InternalOperationSum, OperationKind,
            OperationResult, OperationResultSum, Originated, OriginationError,
            OriginationSuccess, RevealError, RevealSuccess, TransferError,
            TransferSuccess, TransferTarget, UpdateOrigin, ValidityError,
        },
    };

    use crate::COST_PER_BYTES;
    use crate::ORIGINATION_COST;
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

    static SCRIPT_EMITING_INTERNAL_TRANSFER: &str = r#"
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

    /// Build the whole CREATE_CONTRACT { ... } block.
    fn make_create_contract_block(
        param_ty: &str,
        storage_ty: &str,
        code_body: &str, // inside `code { ... }`
    ) -> String {
        // Michelson `{ ... }` need to be doubled to escape Rust’s format! braces
        format!(
            "
                parameter {param_ty} ;
                storage {storage_ty} ;
                code {{ {code_body} }}
            "
        )
    }

    /// Embed a whole contract creation block into a surrounding
    /// script, with delegate hard-coded as `NONE key_hash`.
    /// Also, the created script must have a storage of type `unit`.
    fn make_script_emitting_internal_origination(create_block: &str) -> String {
        format!(
            r#"
            parameter unit;
            storage (option address);
            code {{
                DROP;
                UNIT;                   # starting storage for contract
                AMOUNT;                 # starting balance for the child
                NONE key_hash;          # delegate is always None
                CREATE_CONTRACT {{ {create_block} }};
                DIP {{ SOME ; NIL operation }} ; CONS ; PAIR
            }}"#
        )
    }

    /// Call CREATE_CONTRACT three times:
    /// Result of first call is dropped, result of other calls are swapped.
    fn make_script_emitting_two_internal_originations(
        create_block_1: &str,
        create_block_2: &str,
        create_block_3: &str,
    ) -> String {
        format!(
            r#"
            parameter unit;
            storage (option (pair address address address));
            code {{
                DROP;
                UNIT;                   # starting storage for contract
                PUSH mutez 0;           # starting balance for the child
                NONE key_hash;          # delegate is always None
                CREATE_CONTRACT {{ {create_block_1} }}; DROP;

                PUSH nat 1;             # starting storage for contract
                PUSH mutez 0;           # starting balance for the child
                NONE key_hash;          # delegate is always None
                CREATE_CONTRACT {{ {create_block_2} }};
                DIP {{ PAIR ; NIL operation }} ; CONS ;

                PUSH bytes 0x;          # starting storage for contract
                PUSH mutez 0;           # starting balance for the child
                NONE key_hash;          # delegate is always None
                CREATE_CONTRACT {{ {create_block_3} }};
                DIP {{ SWAP; DIP {{ PAIR ; SOME }} }} ; CONS ; PAIR
            }}"#
        )
    }

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

    #[allow(clippy::too_many_arguments)]
    fn make_origination_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: Bootstrap,
        balance: u64,
        script: Script,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source,
            vec![OperationContent::Origination(OriginationContent {
                balance: balance.into(),
                script,
                delegate: None,
            })],
        )
    }

    // This function setups an account that will pass the validity checks
    fn init_account(
        host: &mut impl Runtime,
        src: &PublicKeyHash,
        amount: u64,
    ) -> TezlinkImplicitAccount {
        // Setting the account in TezlinkImplicitAccount
        let contract = Contract::from_b58check(&src.to_b58check())
            .expect("Contract b58 conversion should have succeed");

        let context = context::Context::init_context();

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        // Allocate the account
        account
            .allocate(host)
            .expect("Account allocation should have succeed");

        // Setting the balance to pass the validity check
        account
            .set_balance(host, &amount.into())
            .expect("Set balance should have succeed");

        account
    }

    fn reveal_account(host: &mut impl Runtime, source: &Bootstrap) {
        let context = context::Context::init_context();
        let account = TezlinkImplicitAccount::from_public_key_hash(&context, &source.pkh)
            .expect("Account creation should have succeed");
        account.set_manager_public_key(host, &source.pk).unwrap()
    }

    // This function sets up an account that will pass the validity checks
    fn init_contract(
        host: &mut impl Runtime,
        src: &ContractKt1Hash,
        script: &str,
        storage_micheline: &Micheline,
        balance: &Narith,
    ) -> TezlinkOriginatedAccount {
        // Setting the account in TezlinkImplicitAccount
        let contract = Contract::Originated(src.clone());

        let context = context::Context::init_context();

        let account = TezlinkOriginatedAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let parser = mir::parser::Parser::new();
        let script_micheline = parser.parse_top_level(script).unwrap();

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

        let src_account = init_account(&mut host, &other.pkh, 50);

        let operation = make_reveal_operation(15, 1, 4, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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

        let src_account = init_account(&mut host, &source.pkh, 50);

        // Fees are too high for source's balance
        let operation = make_reveal_operation(100, 1, 4, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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

        let src_account = init_account(&mut host, &source.pkh, 50);

        // Counter is incoherent for source's counter
        let operation = make_reveal_operation(15, 15, 4, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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

        let account = init_account(&mut host, &source.pkh, 50);

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
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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

        let account = init_account(&mut host, &source.pkh, 50);

        // Set the an inconsistent manager with the source
        let inconsistent_pkh =
            PublicKeyHash::from_b58check("tz1UEQcU7M43yUECMpKGJcxCVwHRaP819qhN")
                .expect("PublicKeyHash b58 conversion should have succeed");

        account
            .force_set_manager_public_key_hash(&mut host, &inconsistent_pkh)
            .expect("Setting manager field should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, source.clone());

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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
        let account = init_account(&mut host, &source.pkh, 50);

        let operation = make_reveal_operation(15, 1, 4, 5, source.clone());

        let result = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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

        let account = init_account(&mut host, &source.pkh, 50);

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
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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
        let source_account = init_account(&mut host, &source.pkh, 50);
        reveal_account(&mut host, &source);

        let destination_account = init_account(&mut host, &dest.pkh, 50);

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
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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
        let source = init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let destination = init_account(&mut host, &dst.pkh, 50);

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
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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
        let source = init_account(&mut host, &src.pkh, 50);
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
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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
        let requester = init_account(&mut host, &src.pkh, 50);
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
            &Micheline::from(()),
        );
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
        let res = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect("validate_and_apply_operation should not have failed with a kernel error")
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
                        storage: Some(storage.encode()),
                        lazy_storage_diff: None,
                        balance_updates: vec![],
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

        let src = bootstrap1();

        let dest = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");

        let initial_storage = Micheline::from("initial");

        let source = init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);
        let destination =
            init_contract(&mut host, &dest, SCRIPT, &initial_storage, &50_u64.into());

        let storage_value = Micheline::from("Hello world").encode();
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
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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

        let src = bootstrap1();

        let dest = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");

        let initial_storage = Micheline::from(());

        let source = init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let destination = init_contract(
            &mut host,
            &dest,
            FAILING_SCRIPT,
            &initial_storage,
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
                value: Micheline::from(()).encode(),
            }),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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
            initial_storage.encode(),
            "Storage should not have been updated"
        )
    }

    #[test]
    fn apply_transfer_with_argument_to_implicit_fails() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        let dest = bootstrap2();

        init_account(&mut host, &src.pkh, 50);
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
                value: Micheline::from(0).encode(),
            }),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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

        let src = bootstrap1();

        let dest = bootstrap2();

        init_account(&mut host, &src.pkh, 50);
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
                value: Micheline::from(()).encode(),
            }),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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
        let src_acc = init_account(&mut host, &src.pkh, 50);
        let dest_acc = init_account(&mut host, &dest.pkh, 50);

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

        let receipts = validate_and_apply_operation(
            &mut host,
            &ctx,
            OperationHash(H256::zero()),
            batch,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .unwrap();

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
        let src_acc = init_account(&mut host, &src.pkh, 50);
        let _dst_acc = init_account(&mut host, &dest.pkh, 50);

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

        let receipts = validate_and_apply_operation(
            &mut host,
            &ctx,
            OperationHash(H256::zero()),
            batch,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        );

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

        let src = bootstrap1();
        let src_acc = init_account(&mut host, &src.pkh, 50);

        let fail_dest = ContractKt1Hash::from_base58_check(CONTRACT_1).unwrap();
        let succ_dest = ContractKt1Hash::from_base58_check(CONTRACT_2).unwrap();

        init_contract(
            &mut host,
            &fail_dest,
            FAILING_SCRIPT,
            &Micheline::from(()),
            &0_u64.into(),
        );
        let succ_account = init_contract(
            &mut host,
            &succ_dest,
            SCRIPT,
            &Micheline::from("initial"),
            &0_u64.into(),
        );

        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: src.pk.clone(),
            proof: None,
        });

        let succ_transfer = OperationContent::Transfer(TransferContent {
            amount: 1.into(),
            destination: Contract::Originated(succ_dest.clone()),
            parameters: Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: Micheline::from("Hello world").encode(),
            }),
        });

        let fail_transfer = OperationContent::Transfer(TransferContent {
            amount: 1.into(),
            destination: Contract::Originated(fail_dest.clone()),
            parameters: Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: Micheline::from(()).encode(),
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
            OperationHash(H256::zero()),
            batch,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
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
            succ_account.storage(&host).unwrap() == Micheline::from("initial").encode(),
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
        );
    }

    #[test]
    fn origination_of_a_smart_contract() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 1000000_u64);
        reveal_account(&mut host, &src);

        let context = context::Context::init_context();

        let src_account =
            TezlinkImplicitAccount::from_public_key_hash(&context, &src.pkh)
                .expect("Should have succeeded to create an account");

        // Retrieve initial balance for the end of the test
        let initial_balance = src_account
            .balance(&host)
            .expect("Should have found a balance");

        let fee = 15u64;
        let smart_contract_balance = 30u64;
        /*
        octez-client convert script "
                  parameter string;
                  storage string;
                  code { CAR; NIL operation; PAIR }
                " from Michelson to binary
         */
        let code =
            hex::decode("02000000170500036805010368050202000000080316053d036d0342")
                .unwrap();

        // octez-client -E https://rpc.tzkt.io/mainnet convert data  '"hello"' from Michelson to binary
        let storage = hex::decode("010000000568656c6c6f").unwrap();
        let operation = make_origination_operation(
            fee,
            1,
            4,
            5,
            src.clone(),
            smart_contract_balance,
            Script {
                code: code.clone(),
                storage: storage.clone(),
            },
        );

        let origination_storage_fee: u64 =
            ((code.len() as u64) + (storage.len() as u64)) * COST_PER_BYTES;

        let receipt = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let mut origination_nonce =
            OriginationNonce::initial(OperationHash(H256::zero()));
        let expected_kt1 = origination_nonce.generate_kt1();

        let expected_receipt = OperationResultSum::Origination(OperationResult {
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
            result: ContentResult::Applied(OriginationSuccess {
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Originated(
                            expected_kt1.clone(),
                        )),
                        changes: 30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -9500,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::StorageFees,
                        changes: 9500,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::Account(Contract::Implicit(src.pkh.clone())),
                        changes: -64250,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::StorageFees,
                        changes: 64250,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                originated_contracts: vec![Originated {
                    contract: expected_kt1.clone(),
                }],
                consumed_gas: 0u64.into(),
                storage_size: 38u64.into(),
                paid_storage_size_diff: 38u64.into(),
                lazy_storage_diff: None,
            }),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, vec![expected_receipt]);

        // Now check that everything in the durable storage is updated

        // Balance of the source
        let current_balance = src_account
            .balance(&host)
            .expect("Should have found a balance for the source");

        let expected_balance = initial_balance
            .0
            .checked_sub(&fee.into())
            .expect("Should have been able to debit the fees")
            .checked_sub(&smart_contract_balance.into())
            .expect("Should have been able to debit the smart contract balance")
            .checked_sub(&ORIGINATION_COST.into())
            .expect("Should have been able to debit the origination cost")
            .checked_sub(&origination_storage_fee.into())
            .expect("Should have been able to debit the storage fees");

        assert_eq!(
            current_balance.0, expected_balance,
            "Source current balance doesn't match the expected one"
        );

        let smart_contract_account = TezlinkOriginatedAccount::from_contract(
            &context,
            &Contract::Originated(expected_kt1),
        )
        .expect("Should have been able to create an account from the KT1");

        // Balance of the smart contract
        let current_kt1_balance = smart_contract_account
            .balance(&host)
            .expect("Should have found a balance for the smart contract");

        assert_eq!(
            current_kt1_balance,
            smart_contract_balance.into(),
            "Smart contract current balance doesn't match the expected one"
        );

        // Verify code and storage
        let smart_contract_code = smart_contract_account
            .code(&host)
            .expect("Should have found a code for the KT1");
        assert_eq!(
            smart_contract_code, code,
            "Current code for smart contract is not the same as the one originated"
        );
        let smart_contract_storage = smart_contract_account
            .storage(&host)
            .expect("Should have found a code for the KT1");
        assert_eq!(
            smart_contract_storage, storage,
            "Current storage for smart contract is not the same as the one originated"
        );
    }

    #[test]
    fn test_internal_receipts_failure_backtrack_all() {
        let mut host = MockKernelHost::default();
        let src = bootstrap1();

        // Initialize accounts with higher balances for the test
        init_account(&mut host, &src.pkh, 100);

        // Create a script that emits internal operations to multiple targets
        let contract_chapo_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &contract_chapo_hash,
            SCRIPT_EMITING_INTERNAL_TRANSFER,
            &Micheline::from(()),
            &100_u64.into(),
        );

        // Create a failing contract
        let internal_fail_contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_2)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &internal_fail_contract_hash,
            FAILING_SCRIPT,
            &Micheline::from(()),
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
            &Micheline::from(()),
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
        let receipts = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
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
                    ContentResult::BackTracked(BacktrackedResult {
                        errors: None,
                        result:
                            TransferTarget::ToContrat(TransferSuccess {
                                balance_updates, ..
                            }),
                    }),
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

    #[test]
    fn test_smart_contract_amount_instruction() {
        // Write AMOUNT in the storage
        const SCRIPT: &str = "
            parameter unit;
            storage mutez;
            code {
                DROP;
                AMOUNT;
                NIL operation;
                PAIR
            }
        ";
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_3)
            .expect("ContractKt1Hash b58 conversion should have succeed");

        let initial_amount = 0;
        let transfer_amount = 30;
        let src_contract = init_contract(
            &mut host,
            &contract_hash,
            SCRIPT,
            &Micheline::from(initial_amount),
            &0_u64.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            transfer_amount.into(),
            Contract::Originated(contract_hash.clone()),
            Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: Micheline::from(()).encode(),
            }),
        );

        let _receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.hash().unwrap(),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(
            src_contract.storage(&host).unwrap(),
            Micheline::from(i128::from(transfer_amount)).encode(),
            "Storage should contain the amount sent"
        );
    }

    #[test]
    fn test_smart_contract_balance_instruction() {
        // Write BALANCE in the storage
        const SCRIPT: &str = "
            parameter unit;
            storage mutez;
            code {
                DROP;
                BALANCE;
                NIL operation;
                PAIR
            }
        ";
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_3)
            .expect("ContractKt1Hash b58 conversion should have succeed");

        let transfer_amount = 30;
        let initial_balance = 200;
        let src_contract = init_contract(
            &mut host,
            &contract_hash,
            SCRIPT,
            &Micheline::from(0),
            &initial_balance.into(),
        );
        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            transfer_amount.into(),
            Contract::Originated(contract_hash.clone()),
            Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: Micheline::from(()).encode(),
            }),
        );

        let _receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.hash().unwrap(),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(
            src_contract.storage(&host).unwrap(),
            Micheline::from(i128::from(initial_balance + transfer_amount)).encode(),
            "Storage should contain the balance of the contract"
        );
    }

    #[test]
    fn test_smart_contract_self_address_instruction() {
        // Write SELF_ADDRESS in the storage
        const SCRIPT_ADDR: &str = "
            parameter unit;
            storage address;
            code {
                DROP;
                SELF_ADDRESS;
                NIL operation;
                PAIR
            }
        ";
        let mut host = MockKernelHost::default();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 50);
        reveal_account(&mut host, &src);

        let contract_hash = ContractKt1Hash::from_base58_check(CONTRACT_3)
            .expect("ContractKt1Hash b58 conversion should have succeed");

        let micheline_address = Micheline::Bytes(
            Contract::Originated(contract_hash.clone())
                .to_bytes()
                .unwrap(),
        );
        let src_contract = init_contract(
            &mut host,
            &contract_hash,
            SCRIPT_ADDR,
            &micheline_address,
            &0_u64.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(contract_hash.clone()),
            Some(Parameter {
                entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                value: Micheline::from(()).encode(),
            }),
        );

        let _receipt = validate_and_apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.hash().unwrap(),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(
            src_contract.storage(&host).unwrap(),
            micheline_address.encode(),
            "Storage should contain the self address of the contract"
        );
    }

    fn get_internal_receipts(op: &OperationResultSum) -> &Vec<InternalOperationSum> {
        if let OperationResultSum::Transfer(OperationResult {
            internal_operation_results,
            ..
        }) = op
        {
            internal_operation_results
        } else {
            panic!("Expected a Transfer operation result")
        }
    }

    #[test]
    fn test_internal_origination_of_a_smart_contract() {
        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();
        let src = bootstrap1();
        let init_src_balance = 100000;
        let expected_init_contract_balance = 1000000;
        init_account(&mut host, &src.pkh, 100000);
        reveal_account(&mut host, &src);
        let context = context::Context::init_context();

        let originated_code = "CDR;
                        NIL operation;
                        PAIR;";
        let originated_script =
            make_create_contract_block("unit", "unit", originated_code);
        let parsed_script = parser
            .parse_top_level(&originated_script)
            .expect("Should have parsed the script");
        let init_script = make_script_emitting_internal_origination(&originated_script);

        // Create a script that emits internal operations to multiple targets
        let contract_chapo_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &contract_chapo_hash,
            &init_script,
            &Micheline::prim0(mir::lexer::Prim::None),
            &1000000_u64.into(),
        );

        let operation = make_operation(
            10,
            1,
            0,
            0,
            src.clone(),
            vec![OperationContent::Transfer(TransferContent {
                amount: 1000.into(),
                destination: Contract::Originated(contract_chapo_hash.clone()),
                parameters: Some(Parameter {
                    entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                    value: Micheline::from(()).encode(),
                }),
            })],
        );
        let receipts = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        assert_eq!(
            receipts.len(),
            1,
            "There should be one receipt for the transfer operation"
        );
        assert!(
            matches!(
                &receipts[0],
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "First receipt should be an Applied Transfer but is {:?}",
            receipts[0]
        );
        let internal_receipts = get_internal_receipts(&receipts[0]);

        assert_eq!(
            internal_receipts.len(),
            1,
            "There should be one internal operation"
        );
        let expected_address =
            OriginationNonce::initial(OperationHash(H256::zero())).generate_kt1();

        assert_eq!(
            internal_receipts[0],
            InternalOperationSum::Origination(InternalContentWithMetadata {
                content: OriginationContent {
                    balance: 1000.into(),
                    delegate: None,
                    script: Script {
                        code: parsed_script.encode(),
                        storage: Micheline::from(()).encode(),
                    },
                },
                result: ContentResult::Applied(OriginationSuccess {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Originated(
                                contract_chapo_hash.clone()
                            )),
                            changes: -1000,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Originated(
                                expected_address.clone()
                            )),
                            changes: 1000,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -7500,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 7500,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -64250,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 64250,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    originated_contracts: vec![Originated {
                        contract: expected_address.clone(),
                    }],
                    consumed_gas: 0_u64.into(),
                    storage_size: 30_u64.into(),
                    paid_storage_size_diff: 30_u64.into(),
                    lazy_storage_diff: None,
                }),
                sender: Contract::Originated(contract_chapo_hash.clone()),
                nonce: 1
            }),
            "Internal origination should match the expected structure"
        );

        // Check Balances of everything is correct
        let src_account =
            TezlinkImplicitAccount::from_public_key_hash(&context, &src.pkh)
                .expect("Should have succeeded to create an account");
        let init_contract_account = TezlinkOriginatedAccount::from_contract(
            &context,
            &Contract::Originated(contract_chapo_hash),
        )
        .expect("Should have succeeded to create an account");
        let originated_account = TezlinkOriginatedAccount::from_contract(
            &context,
            &Contract::Originated(expected_address),
        )
        .expect("Should have succeeded to create an account");
        let expected_src_balance = init_src_balance
            - 10 // fee for the operation
            - 7500 // origination cost paid by the contract
            - 64250 // storage cost paid by the source
            - 1000; // amount sent to the originated contract
        assert_eq!(
            src_account.balance(&host).unwrap(),
            expected_src_balance.into(),
            "Source balance should be correct"
        );
        assert_eq!(
            init_contract_account.balance(&host).unwrap(),
            expected_init_contract_balance.into(),
            "Init contract balance should be correct"
        );
        assert_eq!(
            originated_account.balance(&host).unwrap(),
            1000u64.into(),
            "Originated contract balance should be correct"
        );
    }

    /// In this test, the CREATE_CONTRACT instruction is called three
    /// times.  The result of the first call is dropped.  The results
    /// of the two other calls are swapped.  The point is to check
    /// that the addresses of the originated contracts are not mixed
    /// up.
    #[test]
    fn test_internal_originations_generated_addresses() {
        let mut host = MockKernelHost::default();
        let parser = mir::parser::Parser::new();
        let src = bootstrap1();
        init_account(&mut host, &src.pkh, 1000000);
        reveal_account(&mut host, &src);
        let context = context::Context::init_context();

        let originated_code = "CDR;
                        NIL operation;
                        PAIR;";
        let originated_script_1 =
            make_create_contract_block("unit", "unit", originated_code);
        let originated_script_2 =
            make_create_contract_block("nat", "nat", originated_code);
        let parsed_script_2 = parser
            .parse_top_level(&originated_script_2)
            .expect("Should have parsed the script");
        let originated_script_3 =
            make_create_contract_block("bytes", "bytes", originated_code);
        let parsed_script_3 = parser
            .parse_top_level(&originated_script_3)
            .expect("Should have parsed the script");
        let init_script = make_script_emitting_two_internal_originations(
            &originated_script_1,
            &originated_script_2,
            &originated_script_3,
        );

        // Create a script that emits internal operations to multiple targets
        let contract_chapo_hash = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeed");
        init_contract(
            &mut host,
            &contract_chapo_hash,
            &init_script,
            &Micheline::prim0(mir::lexer::Prim::None),
            &0.into(),
        );

        let operation = make_operation(
            10,
            1,
            0,
            0,
            src.clone(),
            vec![OperationContent::Transfer(TransferContent {
                amount: 0.into(),
                destination: Contract::Originated(contract_chapo_hash.clone()),
                parameters: Some(Parameter {
                    entrypoint: mir::ast::entrypoint::Entrypoint::default(),
                    value: Micheline::from(()).encode(),
                }),
            })],
        );
        let receipts = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );
        assert_eq!(
            receipts.len(),
            1,
            "There should be one receipt for the transfer operation"
        );
        assert!(
            matches!(
                &receipts[0],
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "First receipt should be an Applied Transfer but is {:?}",
            receipts[0]
        );
        let internal_receipts = get_internal_receipts(&receipts[0]);

        assert_eq!(
            internal_receipts.len(),
            2,
            "There should be two internal operations"
        );
        let (_expected_address_1, expected_address_2, expected_address_3) = {
            let mut nonce = OriginationNonce::initial(OperationHash(H256::zero()));
            let expected_address_1 = nonce.generate_kt1();
            let expected_address_2 = nonce.generate_kt1();
            let expected_address_3 = nonce.generate_kt1();
            (expected_address_1, expected_address_2, expected_address_3)
        };

        assert_eq!(
            internal_receipts[0],
            InternalOperationSum::Origination(InternalContentWithMetadata {
                content: OriginationContent {
                    balance: 0.into(),
                    delegate: None,
                    script: Script {
                        code: parsed_script_3.encode(),
                        storage: Micheline::from(vec![]).encode(),
                    },
                },
                result: ContentResult::Applied(OriginationSuccess {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -8250,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 8250,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -64250,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 64250,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    originated_contracts: vec![Originated {
                        contract: expected_address_3,
                    },],
                    consumed_gas: 0_u64.into(),
                    storage_size: 33_u64.into(),
                    paid_storage_size_diff: 33_u64.into(),
                    lazy_storage_diff: None,
                }),
                sender: Contract::Originated(contract_chapo_hash.clone()),
                nonce: 3
            }),
            "Internal origination should match the expected structure"
        );

        assert_eq!(
            internal_receipts[1],
            InternalOperationSum::Origination(InternalContentWithMetadata {
                content: OriginationContent {
                    balance: 0.into(),
                    delegate: None,
                    script: Script {
                        code: parsed_script_2.encode(),
                        storage: Micheline::from(num_bigint::BigUint::from(1u32))
                            .encode(),
                    },
                },
                result: ContentResult::Applied(OriginationSuccess {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -7500,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 7500,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
                            changes: -64250,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 64250,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    originated_contracts: vec![Originated {
                        contract: expected_address_2,
                    }],
                    consumed_gas: 0_u64.into(),
                    storage_size: 30_u64.into(),
                    paid_storage_size_diff: 30_u64.into(),
                    lazy_storage_diff: None,
                }),
                sender: Contract::Originated(contract_chapo_hash),
                nonce: 2
            }),
            "Internal origination should match the expected structure"
        );
    }

    fn backtrack_result<M: OperationKind>(result: M::Success) -> BacktrackedResult<M> {
        BacktrackedResult {
            errors: None,
            result,
        }
    }

    #[test]
    fn test_try_apply_three_origination_batch() {
        let mut host = MockKernelHost::default();
        let ctx = context::Context::init_context();
        let parser = mir::parser::Parser::new();

        let src = bootstrap1();

        // src & dest each credited with 400000ꜩ
        let src_acc = init_account(&mut host, &src.pkh, 400000);

        // op‑1: reveal
        let reveal_content = OperationContent::Reveal(RevealContent {
            pk: src.pk.clone(),
            proof: None,
        });

        println!("Balance: {:?}", src_acc.balance(&host).unwrap());

        // op‑2 orgination: create a contract with 15ꜩ balance successfully
        let origination_content_1 = OperationContent::Origination(OriginationContent {
            balance: 15.into(),
            delegate: None,
            script: Script {
                code: parser.parse_top_level(UNIT_SCRIPT).unwrap().encode(),
                storage: Micheline::from(()).encode(),
            },
        });

        // op‑3 orgination: create a contract with 20ꜩ balance successfully
        let origination_content_2 = OperationContent::Origination(OriginationContent {
            balance: 20.into(),
            delegate: None,
            script: Script {
                code: parser.parse_top_level(UNIT_SCRIPT).unwrap().encode(),
                storage: Micheline::from(()).encode(),
            },
        });

        // op‑4 orgination: create a contract with 999999ꜩ balance fails
        let origination_content_3 = OperationContent::Origination(OriginationContent {
            balance: 999999.into(),
            delegate: None,
            script: Script {
                code: parser.parse_top_level(UNIT_SCRIPT).unwrap().encode(),
                storage: Micheline::from(()).encode(),
            },
        });

        let batch = make_operation(
            5,
            1,
            0,
            0,
            src.clone(),
            vec![
                reveal_content,
                origination_content_1,
                origination_content_2,
                origination_content_3,
            ],
        );

        let receipts = validate_and_apply_operation(
            &mut host,
            &ctx,
            OperationHash(H256::zero()),
            batch,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .unwrap();

        let mut orignation_nonce = OriginationNonce::initial(OperationHash(H256::zero()));
        let expected_kt1_1 = orignation_nonce.generate_kt1();
        let expected_kt1_2 = orignation_nonce.generate_kt1();
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
                result: ContentResult::BackTracked(backtrack_result(RevealSuccess {
                    consumed_gas: 0_u64.into(),
                })),
                internal_operation_results: vec![],
            }),
            OperationResultSum::Origination(OperationResult {
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
                result: ContentResult::BackTracked(backtrack_result(
                    OriginationSuccess {
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -15,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Originated(
                                    expected_kt1_1.clone(),
                                )),
                                changes: 15,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -7500,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::StorageFees,
                                changes: 7500,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -64250,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::StorageFees,
                                changes: 64250,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        originated_contracts: vec![Originated {
                            contract: expected_kt1_1.clone(),
                        }],
                        consumed_gas: 0_u64.into(),
                        storage_size: 30.into(),
                        paid_storage_size_diff: 30.into(),
                        lazy_storage_diff: None,
                    },
                )),
                internal_operation_results: vec![],
            }),
            OperationResultSum::Origination(OperationResult {
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
                result: ContentResult::BackTracked(backtrack_result(
                    OriginationSuccess {
                        balance_updates: vec![
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -20,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Originated(
                                    expected_kt1_2.clone(),
                                )),
                                changes: 20,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -7500,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::StorageFees,
                                changes: 7500,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::Account(Contract::Implicit(
                                    src.pkh.clone(),
                                )),
                                changes: -64250,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                            BalanceUpdate {
                                balance: Balance::StorageFees,
                                changes: 64250,
                                update_origin: UpdateOrigin::BlockApplication,
                            },
                        ],
                        originated_contracts: vec![Originated {
                            contract: expected_kt1_2.clone(),
                        }],
                        consumed_gas: 0_u64.into(),
                        storage_size: 30.into(),
                        paid_storage_size_diff: 30.into(),
                        lazy_storage_diff: None,
                    },
                )),
                internal_operation_results: vec![],
            }),
            OperationResultSum::Origination(OperationResult {
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
                result: ContentResult::Failed(
                    ApplyOperationError::Origination(
                        OriginationError::FailedToApplyBalanceUpdate,
                    )
                    .into(),
                ),
                internal_operation_results: vec![],
            }),
        ];
        assert_eq!(
            receipts, expected_receipts,
            "Receipts do not match the expected ones"
        );
        // Check the balances
        assert_eq!(
            src_acc.balance(&host).unwrap(),
            399980.into(),
            "Source account balance should be 399980ꜩ after the operations"
        );

        // Check the counters
        assert_eq!(
            src_acc.counter(&host).unwrap(),
            4.into(),
            "Source account counter should be 4 after the operations"
        );

        // Check the originated contracts
        let expected_contracts = [expected_kt1_1, expected_kt1_2];
        for (i, expected_kt1) in expected_contracts.iter().enumerate() {
            let account = TezlinkOriginatedAccount::from_contract(
                &ctx,
                &Contract::Originated(expected_kt1.clone()),
            )
            .unwrap();
            assert!(
                account.code(&host).is_err(),
                "Account {i} for KT1{expected_kt1} should not exist"
            );
        }
    }

    #[test]
    fn test_origination_contract_typecheck_storage() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        init_account(&mut host, &src.pkh, 50000);
        reveal_account(&mut host, &src);

        let context = context::Context::init_context();
        let balance = 10.into();

        let code = mir::parser::Parser::new()
            .parse_top_level(UNIT_SCRIPT)
            .expect("Should have succeeded to parse the script")
            .encode();
        let storage = Micheline::from(42).encode();
        let orination_content = OriginationContent {
            balance,
            delegate: None,
            script: Script { code, storage },
        };
        let operation = make_operation(
            10,
            1,
            0,
            0,
            src.clone(),
            vec![OperationContent::Origination(orination_content)],
        );
        let receipts = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts.len(), 1, "There should be one receipt");
        assert!(matches!(
            &receipts[0],
            OperationResultSum::Origination(OperationResult {
            result: ContentResult::Failed(ApplyOperationErrors { errors }),
            ..
            }) if errors.len() == 1 && matches!(
            &errors[0],
            ApplyOperationError::Origination(
                OriginationError::MirTypecheckingError(_)
            )
            )
        ), "Expected Failed Origination operation result with MirTypecheckingError, got {:?}", receipts[0]);
    }

    #[test]
    // Tests that empty transfers (external or internal) to implicit accounts
    // fail, and empty transfers (external or internal) to smart contracts
    // succeed.
    fn test_empty_transfers() {
        let mut host = MockKernelHost::default();
        let context = context::Context::init_context();
        let src = bootstrap1();
        let dst = bootstrap2();
        let kt1_addr =
            ContractKt1Hash::from_base58_check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton")
                .expect("ContractKt1Hash b58 conversion should have succeeded");
        // Setup accounts with 50 mutez in their balance
        init_account(&mut host, &src.pkh, 1000);
        reveal_account(&mut host, &src);
        let (code, storage) = (
            r#"
                        parameter (or (unit %default) (address %call));
                        storage unit;
                        code
                        { UNPAIR;
                          IF_LEFT
                            { DROP; NIL operation; PAIR }
                            { CONTRACT unit;
                              { IF_NONE { { UNIT ; FAILWITH } } {} } ;
                              PUSH mutez 0;
                              UNIT;
                              TRANSFER_TOKENS;
                              NIL operation;
                              SWAP;
                              CONS;
                              PAIR } }
            "#,
            &Micheline::from(()),
        );
        init_contract(&mut host, &kt1_addr, code, storage, &0.into());

        // An empty external transfer to an implicit account fails.
        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            0.into(),
            Contract::Implicit(dst.pkh),
            None,
        );
        let receipts1 = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts1.len(), 1, "There should be one receipt");
        assert!(matches!(
            &receipts1[0],
            OperationResultSum::Transfer(OperationResult {
                result: ContentResult::Failed(ApplyOperationErrors { errors }),
                ..
            }) if errors.len() == 1 && matches!(
                &errors[0],
                ApplyOperationError::Transfer(
                    TransferError::EmptyImplicitTransfer
                )
            )
        ), "Expected Failed Transfer operation result with EmptyImplicitTransfer, got {:?}", receipts1[0]);

        // An empty external transfer to a smart contract succeeds.
        let operation = make_transfer_operation(
            15,
            2,
            4,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(kt1_addr.clone()),
            Some(Parameter {
                entrypoint: Entrypoint::try_from("default")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(()).encode(),
            }),
        );
        let receipts2 = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts2.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts2[0],
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess { .. }
                    )),
                    ..
                })
            ),
            "Expected Successful Transfer operation result, got {:?}",
            receipts2[0]
        );

        // An empty internal transfer to an implicit account fails.
        let operation = make_transfer_operation(
            15,
            3,
            4,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(kt1_addr.clone()),
            Some(Parameter {
                entrypoint: Entrypoint::try_from("call")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(src.clone().pkh.to_b58check()).encode(),
            }),
        );
        let receipts3 = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts3.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts3[0],
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(BacktrackedResult { result: TransferTarget::ToContrat(TransferSuccess { .. }), .. }),
                    internal_operation_results,
                    ..
                }) if internal_operation_results.len() == 1 && matches!(
                    &internal_operation_results[0],
                    InternalOperationSum::Transfer(InternalContentWithMetadata {result: ContentResult::Failed(ApplyOperationErrors { errors }), ..})
                        if errors.len() == 1 && matches!(
                            &errors[0],
                            ApplyOperationError::Transfer(
                                TransferError::EmptyImplicitTransfer
                            )
                        )
                )
            ),
            "Expected Failed Transfer operation result with EmptyImplicitTransfer, got {:?}",
            receipts3[0]
        );

        // An empty internal transfer to a smart contract succeeds.
        let operation = make_transfer_operation(
            15,
            4,
            4,
            5,
            src,
            0.into(),
            Contract::Originated(kt1_addr.clone()),
            Some(Parameter {
                entrypoint: Entrypoint::try_from("call")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(kt1_addr.to_b58check()).encode(),
            }),
        );
        let receipts4 = validate_and_apply_operation(
            &mut host,
            &context,
            OperationHash(H256::zero()),
            operation,
            &0u32.into(),
            &0i64.into(),
            &ChainId::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts4.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts4[0],
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess{ .. })),
                    internal_operation_results,
                    ..
                }) if internal_operation_results.len() == 1 && matches!(
                    &internal_operation_results[0],
                    InternalOperationSum::Transfer(InternalContentWithMetadata {result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess{ .. })), ..})
                )
            ),
            "Expected Successful Transfer operation result, got {:?}",
            receipts4[0]
        );
    }
}
