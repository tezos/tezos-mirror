// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use account_storage::Code;
use account_storage::Manager;
use account_storage::TezlinkAccount;
use enshrined_contracts::get_enshrined_contract_entrypoint;
use mir::ast::{AddressHash, Entrypoint, OperationInfo, TransferTokens, TypedValue};
use mir::context::TypecheckingCtx;
use mir::{
    ast::{big_map::BigMapId, IntoMicheline, Micheline},
    context::CtxTrait,
    gas::Gas,
    parser::Parser,
};
use num_bigint::{BigInt, BigUint};
use num_traits::ops::checked::CheckedMul;
use num_traits::ops::checked::CheckedSub;
use std::collections::{BTreeMap, HashMap};
use tezos_crypto_rs::{hash::ContractKt1Hash, PublicKeyWithHash};
use tezos_data_encoding::types::Narith;
use tezos_evm_logging::{log, Level::*, Verbosity};
use tezos_evm_runtime::{runtime::Runtime, safe_storage::SafeStorage};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKey;
use tezos_tezlink::enc_wrappers::OperationHash;
use tezos_tezlink::lazy_storage_diff::LazyStorageDiffList;
use tezos_tezlink::operation::{
    ManagerOperationContentConv, Operation, OriginationContent, Script,
};
use tezos_tezlink::operation_result::{
    produce_skipped_receipt, ApplyOperationError, ContentResult,
    InternalContentWithMetadata, InternalOperationSum, OperationWithMetadata, Originated,
    OriginationSuccess, TransferTarget,
};
use tezos_tezlink::{
    operation::{OperationContent, Parameters, RevealContent, TransferContent},
    operation_result::{
        produce_operation_result, Balance, BalanceTooLow, BalanceUpdate, EventContent,
        EventSuccess, OperationError, OperationResultSum, OriginationError, RevealError,
        RevealSuccess, TransferError, TransferSuccess, UpdateOrigin,
    },
};
use tezosx_interfaces::Registry;

use crate::account_storage::{TezosImplicitAccount, TezosOriginatedAccount};
use crate::address::OriginationNonce;
use crate::context::Context;
use crate::gas::Cost;
use crate::mir_ctx::{
    clear_temporary_big_maps, convert_big_map_diff, BlockCtx, Ctx, ExecCtx, HasHost,
    OperationCtx, TcCtx,
};

extern crate alloc;
pub mod account_storage;
mod address;
mod alias;
pub mod context;
mod enshrined_contracts;
mod gas;
pub mod mir_ctx;
mod validate;

fn reveal<Host: Runtime, C: Context>(
    tc_ctx: &mut TcCtx<'_, Host, C>,
    source_account: &C::ImplicitAccountType,
    public_key: &PublicKey,
) -> Result<RevealSuccess, RevealError> {
    log!(tc_ctx.host, Debug, "Applying a reveal operation");
    let manager = source_account
        .manager(tc_ctx.host)
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
        .set_manager_public_key(tc_ctx.host, public_key)
        .map_err(|_| RevealError::FailedToWriteManager)?;

    log!(tc_ctx.host, Debug, "Reveal operation succeed");

    Ok(RevealSuccess {
        consumed_milligas: tc_ctx.operation_gas.milligas_consumed_by_operation(),
    })
}

fn contract_from_address(address: AddressHash) -> Result<Contract, TransferError> {
    match address {
        AddressHash::Kt1(kt1) => Ok(Contract::Originated(kt1)),
        AddressHash::Implicit(pkh) => Ok(Contract::Implicit(pkh)),
        AddressHash::Sr1(_) => Err(TransferError::MirAddressUnsupportedError),
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
        consumed_milligas: 0_u64.into(),
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

fn execute_internal_operations<'a, Host: Runtime, C: Context>(
    tc_ctx: &mut TcCtx<'a, Host, C>,
    operation_ctx: &mut OperationCtx<'a, C::ImplicitAccountType>,
    registry: &impl Registry,
    internal_operations: impl Iterator<Item = OperationInfo<'a>>,
    sender_account: &C::OriginatedAccountType,
    parser: &'a Parser<'a>,
    all_internal_receipts: &mut Vec<InternalOperationSum>,
) -> Result<(), ApplyOperationError> {
    let mut failed = None;
    for (index, OperationInfo { operation, counter }) in
        internal_operations.into_iter().enumerate()
    {
        tc_ctx
            .operation_gas
            .consume(Cost::manager_operation())
            .map_err(|_| TransferError::OutOfGas)?;
        log!(
            tc_ctx.host,
            Debug,
            "Executing internal operation {operation:?} with counter {counter:?}"
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
                    parameters: Parameters {
                        entrypoint: destination_address.entrypoint,
                        value: encoded_value,
                    },
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
                        tc_ctx,
                        operation_ctx,
                        registry,
                        sender_account,
                        &content.amount,
                        &content.destination,
                        &content.parameters.entrypoint,
                        value,
                        parser,
                        all_internal_receipts,
                    );
                    InternalOperationSum::Transfer(InternalContentWithMetadata {
                        content,
                        sender: sender_account.contract(),
                        nonce,
                        result: match receipt {
                            Ok(success) => ContentResult::Applied(success.into()),
                            Err(err) => {
                                failed = Some(index);
                                log!(
                                    tc_ctx.host,
                                    Error,
                                    "Internal transfer failed: {err:?}"
                                );
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
                        .clone()
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
                        tc_ctx,
                        address,
                        operation_ctx.source,
                        sender_account,
                        &amount,
                        &script.code,
                        storage,
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
                                log!(
                                    tc_ctx.host,
                                    Error,
                                    "Internal origination failed: {err:?}"
                                );
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

            mir::ast::Operation::Emit(mir::ast::Emit { tag, value, arg_ty }) => {
                let tag: Option<Entrypoint> = tag
                    .map(|t| {
                        t.try_into().map_err(|e: mir::ast::ByteReprError| {
                            ApplyOperationError::UnSupportedEmit(format!(
                                "Invalid emit tag: {e:?}"
                            ))
                        })
                    })
                    .transpose()?;
                let payload = Some(
                    value
                        .into_micheline_optimized_legacy(&parser.arena)
                        .encode(),
                );
                let ty = match arg_ty {
                    mir::ast::Or::Left(typ) => {
                        typ.into_micheline_optimized_legacy(&parser.arena).encode()
                    }
                    mir::ast::Or::Right(mic) => mic.encode(),
                };
                let result = if failed.is_some() {
                    ContentResult::Skipped
                } else {
                    // Same semantics as OCaml:
                    // Gas.consumed ~since:ctxt_before_op ~until:ctxt
                    let consumed_milligas =
                        tc_ctx.operation_gas.milligas_consumed_by_operation();
                    ContentResult::Applied(EventSuccess { consumed_milligas })
                };
                InternalOperationSum::Event(InternalContentWithMetadata {
                    content: EventContent { tag, payload, ty },
                    sender: sender_account.contract(),
                    nonce,
                    result,
                })
            }
        };
        log!(
            tc_ctx.host,
            Debug,
            "Internal operation executed successfully"
        );
        all_internal_receipts.push(internal_receipt);
    }
    if let Some(index) = failed {
        log!(
            tc_ctx.host,
            Debug,
            "Internal operation execution failed at index {index}"
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
fn transfer<'a, Host: Runtime, C: Context>(
    tc_ctx: &mut TcCtx<'a, Host, C>,
    operation_ctx: &mut OperationCtx<'a, C::ImplicitAccountType>,
    registry: &impl Registry,
    sender_account: &impl TezlinkAccount,
    amount: &Narith,
    dest_contract: &Contract,
    entrypoint: &Entrypoint,
    param: Micheline<'a>,
    parser: &'a Parser<'a>,
    all_internal_receipts: &mut Vec<InternalOperationSum>,
) -> Result<TransferSuccess, TransferError> {
    match dest_contract {
        Contract::Implicit(pkh) => {
            tc_ctx
                .operation_gas
                .consume(Cost::transaction())
                .map_err(|_| TransferError::OutOfGas)?;

            if param != Micheline::from(()) || !entrypoint.is_default() {
                return Err(TransferError::NonSmartContractExecutionCall);
            }
            // Transfers of 0 tez to an implicit contract are rejected.
            if amount.eq(&0_u64.into()) {
                return Err(TransferError::EmptyImplicitTransfer);
            };

            let dest_account = tc_ctx
                .context
                .implicit_from_public_key_hash(pkh)
                .map_err(|_| TransferError::FailedToFetchDestinationAccount)?;
            // Allocated is not being used on purpose (see below the comment on the allocated_destination_contract field)
            let _allocated = dest_account
                .allocate(tc_ctx.host)
                .map_err(|_| TransferError::FailedToAllocateDestination)?;
            let receipt =
                transfer_tez(tc_ctx.host, sender_account, amount, &dest_account)?;
            Ok(TransferSuccess {
                // This boolean is kept at false on purpose to maintain compatibility with TZKT.
                // When transferring to a non-existent account, we need to allocate it (I/O to durable storage).
                // This incurs a cost, and TZKT expects balance updates in the operation receipt representing this cost.
                // So, as long as we don't have balance updates to represent this cost, we keep this boolean false.
                allocated_destination_contract: false,
                consumed_milligas: tc_ctx.operation_gas.milligas_consumed_by_operation(),
                ..receipt
            })
        }
        Contract::Originated(kt1) => {
            let dest_account = tc_ctx
                .context
                .originated_from_kt1(kt1)
                .map_err(|_| TransferError::FailedToFetchDestinationAccount)?;
            let receipt =
                transfer_tez(tc_ctx.host, sender_account, amount, &dest_account)?;
            let code = dest_account
                .code(tc_ctx.host)
                .map_err(|_| TransferError::FailedToFetchContractCode)?;
            let storage = dest_account
                .storage(tc_ctx.host)
                .map_err(|_| TransferError::FailedToFetchContractStorage)?;
            let exec_ctx =
                ExecCtx::create(tc_ctx.host, sender_account, &dest_account, amount)?;
            let mut ctx = Ctx {
                tc_ctx,
                exec_ctx,
                operation_ctx,
            };
            let (internal_operations, new_storage) = execute_smart_contract(
                code, storage, entrypoint, param, parser, &mut ctx, registry,
            )?;
            dest_account
                .set_storage(ctx.host(), &new_storage)
                .map_err(|_| TransferError::FailedToUpdateContractStorage)?;

            // In L1, the receipt of an operation only shows its own gas
            // consumption, i.e. it does not include that of its internal
            // operations.
            let consumed_milligas =
                ctx.tc_ctx.operation_gas.milligas_consumed_by_operation();
            let lazy_storage_diff =
                convert_big_map_diff(std::mem::take(&mut ctx.tc_ctx.big_map_diff));
            execute_internal_operations(
                ctx.tc_ctx,
                ctx.operation_ctx,
                registry,
                internal_operations,
                &dest_account,
                parser,
                all_internal_receipts,
            )
            .map_err(|err| {
                TransferError::FailedToExecuteInternalOperation(err.to_string())
            })?;
            log!(ctx.host(), Debug, "Transfer operation succeeded");
            Ok(TransferSuccess {
                storage: Some(new_storage),
                lazy_storage_diff,
                consumed_milligas,
                ..receipt
            })
        }
    }
}

fn get_originated_contract_entrypoint(
    code: Vec<u8>,
) -> Option<HashMap<Entrypoint, mir::ast::Type>> {
    let parser = Parser::new();
    let micheline = Micheline::decode_raw(&parser.arena, &code).ok()?;
    // TODO (Linear issue L2-383): handle gas consumption here.
    let typechecked = micheline
        .split_script()
        .ok()?
        .typecheck_script(&mut Gas::default(), true, false)
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

fn get_contract_entrypoint<C: Context>(
    host: &impl Runtime,
    context: &C,
    address: &AddressHash,
) -> Option<HashMap<mir::ast::Entrypoint, mir::ast::Type>> {
    let contract = contract_from_address(address.clone()).ok()?;
    let contract_account = context.originated_from_contract(&contract).ok()?;
    let code = contract_account.code(host).ok()?;
    match code {
        Code::Code(code) => get_originated_contract_entrypoint(code),
        Code::Enshrined(contract) => get_enshrined_contract_entrypoint(contract),
    }
}

// Handles manager transfer operations.
#[allow(clippy::too_many_arguments)]
fn transfer_external<'a, Host: Runtime, C: Context>(
    tc_ctx: &mut TcCtx<'a, Host, C>,
    operation_ctx: &mut OperationCtx<'a, C::ImplicitAccountType>,
    registry: &impl Registry,
    amount: &Narith,
    dest: &Contract,
    parameters: &Parameters,
    all_internal_receipts: &mut Vec<InternalOperationSum>,
    parser: &'a Parser<'a>,
) -> Result<TransferTarget, TransferError> {
    log!(
        tc_ctx.host,
        Debug,
        "Applying an external transfer operation from {} to {dest:?} of {amount:?} mutez with parameters {parameters:?}",
        operation_ctx.source.pkh()
    );
    let entrypoint = &parameters.entrypoint;
    let value = Micheline::decode_raw(&parser.arena, &parameters.value)?;

    transfer(
        tc_ctx,
        operation_ctx,
        registry,
        operation_ctx.source,
        amount,
        dest,
        entrypoint,
        value,
        parser,
        all_internal_receipts,
    )
    .map(Into::into)
}

/// This function typechecks both fields of a &Script: the code and the storage.
/// It returns the typechecked storage.
fn typecheck_code_and_storage<'a, Host: Runtime, C: Context>(
    ctx: &mut TcCtx<'a, Host, C>,
    parser: &'a Parser<'a>,
    script: &Script,
) -> Result<TypedValue<'a>, OriginationError> {
    let contract_micheline = Micheline::decode_raw(&parser.arena, &script.code)
        .map_err(|e| OriginationError::MichelineDecodeError(e.to_string()))?;
    let allow_lazy_storage_in_storage = true;
    let contract_typechecked = contract_micheline
        .split_script()
        .map_err(|e| {
            OriginationError::MirTypecheckingError(format!("Splitting script : {e}"))
        })?
        .typecheck_script(ctx.gas(), allow_lazy_storage_in_storage, true)
        .map_err(|e| OriginationError::MirTypecheckingError(format!("Script : {e}")))?;
    let storage_micheline = Micheline::decode_raw(&parser.arena, &script.storage)
        .map_err(|e| OriginationError::MichelineDecodeError(e.to_string()))?;
    contract_typechecked
        .typecheck_storage(ctx, &storage_micheline)
        .map_err(|e| OriginationError::MirTypecheckingError(format!("Storage : {e}")))
}

fn handle_storage_with_big_maps<'a, Host: Runtime, C: Context>(
    ctx: &mut TcCtx<'a, Host, C>,
    mut storage: TypedValue<'a>,
) -> Result<(Vec<u8>, Option<LazyStorageDiffList>), OriginationError> {
    let parser = Parser::new();

    let mut big_maps = vec![];
    storage.view_big_maps_mut(&mut big_maps);

    // Dump big_map allocation, starting with empty big_maps
    mir::ast::big_map::dump_big_map_updates(ctx, &[], &mut big_maps, false)
        .map_err(|err| OriginationError::MirBigMapAllocation(err.to_string()))?;
    let storage = storage
        .into_micheline_optimized_legacy(&parser.arena)
        .encode();
    let lazy_storage_diff = convert_big_map_diff(std::mem::take(&mut ctx.big_map_diff));
    Ok((storage, lazy_storage_diff))
}

// Values from src/proto_023_PtSeouLo/lib_parameters/default_parameters.ml.
const ORIGINATION_SIZE: u64 = 257;
const COST_PER_BYTES: u64 = 250;
const ORIGINATION_COST: u64 = ORIGINATION_SIZE * COST_PER_BYTES;

/// Originate a contract deployed by the public key hash given in parameter. For now
/// the origination is not correctly implemented.
fn originate_contract<'a, Host: Runtime, C: Context>(
    ctx: &mut TcCtx<'a, Host, C>,
    contract: ContractKt1Hash,
    source_account: &C::ImplicitAccountType,
    sender_account: &impl TezlinkAccount,
    initial_balance: &Narith,
    script_code: &[u8],
    script_storage: TypedValue<'a>,
) -> Result<OriginationSuccess, OriginationError> {
    // If the origination is internal the big map are handled by the first transfer
    // The big_maps vector will be filled only if the origination is "external"

    let (new_storage, lazy_storage_diff) =
        handle_storage_with_big_maps(ctx, script_storage)?;

    // Set the storage of the contract
    let smart_contract = ctx
        .context
        .originated_from_kt1(&contract)
        .map_err(|_| OriginationError::FailedToFetchOriginated)?;

    let total_size = smart_contract
        .init(ctx.host, script_code, &new_storage)
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
        compute_storage_balance_updates(source_account.contract(), storage_fees.clone())
            .map_err(|_| OriginationError::FailedToComputeBalanceUpdate)?;
    balance_updates.extend(storage_fees_balance_updates);

    // Balance updates for the base origination cost.
    let origination_fees_balance_updates = compute_storage_balance_updates(
        source_account.contract(),
        ORIGINATION_COST.into(),
    )
    .map_err(|_| OriginationError::FailedToComputeBalanceUpdate)?;
    balance_updates.extend(origination_fees_balance_updates);

    // Apply the balance change, accordingly to the balance updates computed
    apply_balance_changes(
        ctx.host,
        sender_account,
        &smart_contract,
        &initial_balance.0,
    )
    .map_err(|_| OriginationError::FailedToApplyBalanceUpdate)?;

    let _ = burn_tez(ctx.host, source_account, &(ORIGINATION_COST + storage_fees))
        .map_err(|_| OriginationError::FailedToApplyBalanceUpdate)?;

    let dummy_origination_sucess = OriginationSuccess {
        balance_updates,
        originated_contracts: vec![Originated { contract }],
        consumed_milligas: ctx.operation_gas.milligas_consumed_by_operation(),
        // TODO https://linear.app/tezos/issue/L2-325/fix-storage-size-and-paid-diff-at-origination
        // These are probably not the right values for storage_size and
        // paid_storage_size_diff, but having something different than 0
        // participates in having the TzKT front-end not crash when originating.
        storage_size: total_size.clone().into(),
        paid_storage_size_diff: total_size.into(),
        lazy_storage_diff,
    };
    Ok(dummy_origination_sucess)
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
    source_contract: Contract,
    fee: BigUint,
) -> Result<Vec<BalanceUpdate>, num_bigint::TryFromBigIntError<num_bigint::BigInt>> {
    if fee.eq(&0_u64.into()) {
        return Ok(vec![]);
    };
    let source_delta = BigInt::from_biguint(num_bigint::Sign::Minus, fee.clone());
    let block_fees = BigInt::from_biguint(num_bigint::Sign::Plus, fee);

    let source_update = BalanceUpdate {
        balance: Balance::Account(source_contract),
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
        "Transfer: OK - the new balance of the giver is {new_giver_balance:?} and the new balance of the receiver is {new_receiver_balance:?}"
    );

    Ok(())
}

/// Executes the entrypoint logic of an originated smart contract and returns the new storage.
fn execute_smart_contract_originated<'a>(
    code: Vec<u8>,
    storage: Vec<u8>,
    entrypoint: &Entrypoint,
    value: Micheline<'a>,
    parser: &'a Parser<'a>,
    ctx: &mut impl CtxTrait<'a>,
) -> Result<(impl Iterator<Item = OperationInfo<'a>>, Vec<u8>), TransferError> {
    // Parse and typecheck the contract
    let contract_micheline = Micheline::decode_raw(&parser.arena, &code)?;
    let contract_typechecked =
        contract_micheline
            .split_script()?
            .typecheck_script(ctx.gas(), true, false)?;
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

/// A type-unifying wrapper for internal operation iterators.
///
/// ### Why this exists
/// `impl Iterator` is an opaque type resolved at compile-time. If a function
/// needs to return iterators that _could_ have different implementation, because
/// their type is opaque, then the typechecker refuses.
///
/// ### How it works
/// This enum allows us to unify two distinct iterator types into a single named type
/// without the overhead of dynamic dispatch (`Box<dyn Iterator>`). The `Box` solution
/// was tried, but it involved propagating lifetimes in other modules far and wide.
///
/// - `Active(I)`: Wraps the complex iterator chain produced by the logic.
/// - `Empty`: Represents a no-op state without requiring a heap allocation.
///
/// The change is very local, as the enum doesn't need to be propagated to the
/// rest of the code, it's hidden behind an opaque type.
enum InternalOperationIterator<I> {
    Active(I),
    Empty,
}

impl<'a, I> Iterator for InternalOperationIterator<I>
where
    I: Iterator<Item = OperationInfo<'a>>,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Active(iter) => iter.next(),
            Self::Empty => None,
        }
    }
}

fn execute_smart_contract<'a, Host: Runtime>(
    code: account_storage::Code,
    storage: Vec<u8>,
    entrypoint: &Entrypoint,
    value: Micheline<'a>,
    parser: &'a Parser<'a>,
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host>),
    registry: &impl Registry,
) -> Result<(impl Iterator<Item = OperationInfo<'a>>, Vec<u8>), TransferError> {
    match code {
        Code::Code(code) => {
            // Parse and typecheck the contract
            let (iter, storage) = execute_smart_contract_originated(
                code, storage, entrypoint, value, parser, ctx,
            )?;
            Ok((InternalOperationIterator::Active(iter), storage))
        }
        Code::Enshrined(contract) => {
            // TODO POC
            enshrined_contracts::execute_enshrined_contract(
                contract, entrypoint, value, ctx, registry,
            )?;
            Ok((InternalOperationIterator::<_>::Empty, vec![]))
        }
    }
}

pub fn validate_and_apply_operation<Host: Runtime, C: Context>(
    host: &mut Host,
    registry: &impl Registry,
    context: &C,
    hash: OperationHash,
    operation: Operation,
    block_ctx: &BlockCtx,
    skip_signature_check: bool,
) -> Result<Vec<OperationWithMetadata>, OperationError> {
    let mut safe_host = SafeStorage {
        host,
        world_state: context.path(),
    };

    safe_host.start()?;

    log!(safe_host, Debug, "Verifying that the batch is valid");

    let validation_info = match validate::execute_validation(
        &mut safe_host,
        context,
        operation,
        skip_signature_check,
    ) {
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
        registry,
        context,
        &mut origination_nonce,
        validation_info,
        block_ctx,
    );

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
        log!(safe_host, Debug, "Receipts: {receipts:#?}");
        safe_host.revert()?;
    }

    Ok(receipts)
}

fn apply_batch<Host: Runtime, C: Context>(
    host: &mut Host,
    registry: &impl Registry,
    context: &C,
    origination_nonce: &mut OriginationNonce,
    validation_info: validate::ValidatedBatch<C::ImplicitAccountType>,
    block_ctx: &BlockCtx,
) -> (Vec<OperationWithMetadata>, bool) {
    let validate::ValidatedBatch {
        source_account,
        validated_operations,
    } = validation_info;
    let mut first_failure: Option<usize> = None;
    let mut receipts = Vec::with_capacity(validated_operations.len());
    let mut next_temporary_id = BigMapId { value: (-1).into() };
    for (index, validated_operation) in validated_operations.into_iter().enumerate() {
        log!(
            host,
            Debug,
            "Applying operation #{index} in the batch with counter {:?}.",
            validated_operation.content.counter
        );
        let receipt = if first_failure.is_some() {
            log!(
                host,
                Debug,
                "Skipping this operation because we already failed on {first_failure:?}."
            );
            produce_skipped_receipt(
                validated_operation.content,
                validated_operation.balance_updates,
            )
        } else {
            apply_operation(
                host,
                registry,
                context,
                origination_nonce,
                &source_account,
                validated_operation,
                &mut next_temporary_id,
                block_ctx,
            )
        };

        if first_failure.is_none() && !receipt.receipt.is_applied() {
            first_failure = Some(index);
        }

        receipts.push(receipt);
    }

    // Clear all the temporaries big_map after the application of the batch
    let cleared = clear_temporary_big_maps(host, context, &mut next_temporary_id);

    if let Err(lazy_storage_err) = cleared {
        log!(
            host,
            Error,
            "Cleaning the temporary big_map in the storage failed: {lazy_storage_err}"
        )
    }

    if let Some(failure_idx) = first_failure {
        receipts[..failure_idx].iter_mut().for_each(|receipt| {
            OperationResultSum::transform_result_backtrack(&mut receipt.receipt)
        });
        return (receipts, false);
    }

    (receipts, true)
}

#[allow(clippy::too_many_arguments)]
fn apply_operation<Host: Runtime, C: Context>(
    host: &mut Host,
    registry: &impl Registry,
    context: &C,
    origination_nonce: &mut OriginationNonce,
    source_account: &C::ImplicitAccountType,
    validated_operation: validate::ValidatedOperation,
    next_temporary_id: &mut BigMapId,
    block_ctx: &BlockCtx,
) -> OperationWithMetadata {
    let mut internal_operations_receipts = Vec::new();
    let mut gas = validated_operation.gas;
    let mut tc_ctx = TcCtx {
        host,
        context,
        operation_gas: &mut gas,
        big_map_diff: BTreeMap::new(),
        next_temporary_id,
    };
    let parser = Parser::new();
    match &validated_operation.content.operation {
        OperationContent::Reveal(RevealContent { pk, .. }) => {
            let reveal_result = reveal(&mut tc_ctx, source_account, pk);

            if reveal_result.is_err() {
                log!(host, Error, "Reveal failed because of: {reveal_result:?}");
            }

            let manager_result = produce_operation_result(
                validated_operation.balance_updates,
                reveal_result.map_err(Into::into),
                internal_operations_receipts,
            );
            OperationWithMetadata {
                content: validated_operation.content.into_manager_operation_content(),
                receipt: OperationResultSum::Reveal(manager_result),
            }
        }
        OperationContent::Transfer(TransferContent {
            amount,
            destination,
            parameters,
        }) => {
            let mut operation_ctx = OperationCtx {
                source: source_account,
                counter: &mut 0u128,
                origination_nonce,
                level: block_ctx.level,
                now: block_ctx.now,
                chain_id: block_ctx.chain_id,
            };
            let transfer_result = transfer_external(
                &mut tc_ctx,
                &mut operation_ctx,
                registry,
                amount,
                destination,
                parameters,
                &mut internal_operations_receipts,
                &parser,
            );

            if transfer_result.is_err() {
                log!(
                    host,
                    Error,
                    "Transfer failed because of: {transfer_result:?}"
                );
            }

            let manager_result = produce_operation_result(
                validated_operation.balance_updates,
                transfer_result.map_err(Into::into),
                internal_operations_receipts,
            );
            OperationWithMetadata {
                content: validated_operation.content.into_manager_operation_content(),
                receipt: OperationResultSum::Transfer(manager_result),
            }
        }
        OperationContent::Origination(OriginationContent {
            ref balance,
            delegate: _,
            ref script,
        }) => {
            let address = origination_nonce.generate_kt1();
            let typechecked_storage =
                typecheck_code_and_storage(&mut tc_ctx, &parser, script);
            let origination_result = match typechecked_storage {
                Ok(storage) => originate_contract(
                    &mut tc_ctx,
                    address,
                    source_account,
                    source_account,
                    balance,
                    &script.code,
                    storage,
                ),
                Err(err) => Err(err),
            };

            if origination_result.is_err() {
                log!(
                    host,
                    Error,
                    "Origination failed because of: {origination_result:?}"
                );
            }

            let manager_result = produce_operation_result(
                validated_operation.balance_updates,
                origination_result.map_err(|e| e.into()),
                internal_operations_receipts,
            );
            OperationWithMetadata {
                content: validated_operation.content.into_manager_operation_content(),
                receipt: OperationResultSum::Origination(manager_result),
            }
        }
    }
}

/// Shared test utilities for gateway testing
#[cfg(test)]
#[allow(clippy::type_complexity)]
pub(crate) mod test_utils {
    use primitive_types::U256;
    use std::cell::RefCell;
    use tezos_evm_runtime::runtime::Runtime;
    use tezosx_interfaces::{Registry, RuntimeId, TezosXRuntimeError};

    /// Mock Registry for testing gateway operations.
    /// Tracks all calls to bridge and generate_alias for verification.
    pub struct MockRegistry {
        pub generate_alias_calls: RefCell<Vec<(Vec<u8>, RuntimeId)>>,
        pub bridge_calls: RefCell<Vec<(RuntimeId, Vec<u8>, Vec<u8>, U256)>>,
        generated_alias: Vec<u8>,
    }

    impl MockRegistry {
        pub fn new(generated_alias: Vec<u8>) -> Self {
            Self {
                generate_alias_calls: RefCell::new(Vec::new()),
                bridge_calls: RefCell::new(Vec::new()),
                generated_alias,
            }
        }
    }

    impl Registry for MockRegistry {
        fn bridge<Host: Runtime>(
            &self,
            _host: &mut Host,
            destination_runtime: RuntimeId,
            destination_address: &[u8],
            source_address: &[u8],
            amount: U256,
            _data: &[u8],
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            self.bridge_calls.borrow_mut().push((
                destination_runtime,
                destination_address.to_vec(),
                source_address.to_vec(),
                amount,
            ));
            Ok(vec![])
        }

        fn generate_alias<Host: Runtime>(
            &self,
            _host: &mut Host,
            native_address: &[u8],
            runtime_id: RuntimeId,
            _context: tezosx_interfaces::AliasCreationContext,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            self.generate_alias_calls
                .borrow_mut()
                .push((native_address.to_vec(), runtime_id));
            Ok(self.generated_alias.clone())
        }

        fn address_from_string(
            &self,
            address_str: &str,
            _runtime_id: RuntimeId,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            Ok(address_str.as_bytes().to_vec())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::account_storage::{
        Code, TezlinkImplicitAccount, TezosImplicitAccount, TezosOriginatedAccount,
    };
    use crate::context::Context;
    use crate::{
        account_storage::TezlinkOriginatedAccount, address::OriginationNonce,
        mir_ctx::BlockCtx,
    };
    use mir::ast::big_map::BigMapId;
    use mir::ast::{Address, Entrypoint, IntoMicheline, Micheline, Type, TypedValue};
    use mir::context::TypecheckingCtx;
    use mir::parser::Parser;
    use mir::typechecker::typecheck_value;
    use num_traits::ops::checked::CheckedSub;
    use pretty_assertions::assert_eq;
    use primitive_types::H256;
    use std::collections::BTreeMap;
    use std::fs::read_to_string;
    use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait, SecretKeyEd25519};
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::types::Narith;
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};
    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup::types::{PublicKey, PublicKeyHash};
    use tezos_tezlink::{
        block::TezBlock,
        enc_wrappers::OperationHash,
        operation::{
            sign_operation, ManagerOperation, ManagerOperationContent,
            ManagerOperationContentConv, Operation, OperationContent, OriginationContent,
            Parameters, RevealContent, Script, TransferContent,
        },
        operation_result::{
            ApplyOperationError, ApplyOperationErrors, BacktrackedResult, Balance,
            BalanceTooLow, BalanceUpdate, ContentResult, CounterError,
            InternalContentWithMetadata, InternalOperationSum, OperationKind,
            OperationResult, OperationResultSum, OperationWithMetadata, Originated,
            OriginationError, OriginationSuccess, RevealError, RevealSuccess,
            TransferError, TransferSuccess, TransferTarget, UpdateOrigin, ValidityError,
        },
    };
    use typed_arena::Arena;

    use crate::gas::TezlinkOperationGas;
    use crate::TcCtx;
    use crate::ORIGINATION_COST;
    use crate::{
        account_storage::{Manager, TezlinkAccount},
        context, validate_and_apply_operation, OperationError,
    };
    use crate::{make_default_ctx, COST_PER_BYTES};
    use tezosx_interfaces::{
        AliasCreationContext, Registry, RuntimeId, TezosXRuntimeError,
    };

    // Mock Registry for tests that don't need cross-runtime functionality
    struct MockRegistry;

    impl Registry for MockRegistry {
        fn bridge<Host: Runtime>(
            &self,
            _host: &mut Host,
            destination_runtime: RuntimeId,
            _destination_address: &[u8],
            _source_address: &[u8],
            _amount: primitive_types::U256,
            _data: &[u8],
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            Err(TezosXRuntimeError::RuntimeNotFound(destination_runtime))
        }

        fn generate_alias<Host: Runtime>(
            &self,
            _host: &mut Host,
            _native_address: &[u8],
            runtime_id: RuntimeId,
            _context: AliasCreationContext,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            Err(TezosXRuntimeError::RuntimeNotFound(runtime_id))
        }

        fn address_from_string(
            &self,
            _address_str: &str,
            runtime_id: RuntimeId,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            Err(TezosXRuntimeError::RuntimeNotFound(runtime_id))
        }
    }

    macro_rules! block_ctx {
        () => {
            BlockCtx {
                level: &0u32.into(),
                now: &0i64.into(),
                chain_id: &HashTrait::try_from_bytes(&[0, 0, 0, 0]).unwrap(),
            }
        };
    }

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
                .into_manager_operation_content()
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
        parameters: Parameters,
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

        let context = context::TezlinkContext::init_context();

        let account = context
            .implicit_from_contract(&contract)
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
        let context = context::TezlinkContext::init_context();
        let account = context
            .implicit_from_public_key_hash(&source.pkh)
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

        let context = context::TezlinkContext::init_context();

        let account = context
            .originated_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let parser = mir::parser::Parser::new();
        let script_micheline = parser.parse_top_level(script).unwrap();

        account
            .init(
                host,
                &script_micheline.encode(),
                &storage_micheline.encode(),
            )
            .expect("Account initialisation should have succeeded");

        account
            .set_balance(host, balance)
            .expect("Set balance should have succeeded");

        account
    }

    fn zip_operations(
        operation: Operation,
        receipt: Vec<OperationResultSum>,
    ) -> Vec<OperationWithMetadata> {
        operation
            .content
            .into_iter()
            .zip(receipt)
            .map(|(c, r)| OperationWithMetadata {
                content: c,
                receipt: r,
            })
            .collect::<Vec<OperationWithMetadata>>()
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

        let operation = make_reveal_operation(15, 1, 1000, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
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
        let operation = make_reveal_operation(100, 1, 1000, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
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
        let operation = make_reveal_operation(15, 15, 1000, 5, source);

        let result = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
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
        let operation = make_reveal_operation(15, 1, 1000, 5, source.clone());
        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        // Reveal operation should fail
        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Reveal(OperationResult {
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
            }),
        }];

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

        let operation = make_reveal_operation(15, 1, 1000, 5, source.clone());

        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Reveal(OperationResult {
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
            }),
        }];

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

        let operation = make_reveal_operation(15, 1, 1000, 5, source.clone());

        let result = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
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

        let operation = make_reveal_operation(15, 1, 1000, 5, source.clone());

        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Reveal(OperationResult {
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
                    consumed_milligas: 168913_u64.into(),
                }),
                internal_operation_results: vec![],
            }),
        }];

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
            21040,
            5,
            source.clone(),
            100_u64.into(),
            Contract::Implicit(dest.pkh),
            Parameters::default(),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
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
            }),
        }];

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
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dst.pkh.clone()),
            Parameters::default(),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
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
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
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
                        consumed_milligas: 2168615_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: false,
                    },
                )),
                internal_operation_results: vec![],
            }),
        }];
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
            21000,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh.clone()),
            Parameters::default(),
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
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
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
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
                        consumed_milligas: 2168615_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: false,
                    },
                )),
                internal_operation_results: vec![],
            }),
        }];

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
        let context = context::TezlinkContext::init_context();
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
            22080,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(desthash.clone()),
            Parameters {
                entrypoint: Entrypoint::try_from("fund")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(requested_amount as i128).encode(),
            },
        );
        let res = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect("validate_and_apply_operation should not have failed with a kernel error")
        .remove(0);
        assert_eq!(
            res,
            OperationWithMetadata {
                content: operation.content[0].clone(),
                receipt: OperationResultSum::Transfer(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone()
                            )),
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
                            consumed_milligas: 176061_u64.into(),
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
                                parameters: Parameters {
                                    entrypoint: Entrypoint::default(),
                                    value: Micheline::from(()).encode(),
                                },
                            },
                            sender: Contract::Originated(desthash.clone()),
                            nonce: 1,
                            result: ContentResult::Applied(TransferTarget::ToContrat(
                                TransferSuccess {
                                    storage: None,
                                    lazy_storage_diff: None,
                                    balance_updates: vec![
                                        BalanceUpdate {
                                            balance: Balance::Account(
                                                Contract::Originated(desthash.clone())
                                            ),
                                            changes: 0 - (requested_amount as i64),
                                            update_origin: UpdateOrigin::BlockApplication,
                                        },
                                        BalanceUpdate {
                                            balance: Balance::Account(
                                                Contract::Implicit(src.pkh.clone())
                                            ),
                                            changes: requested_amount as i64,
                                            update_origin: UpdateOrigin::BlockApplication,
                                        },
                                    ],
                                    ticket_receipt: vec![],
                                    originated_contracts: vec![],
                                    consumed_milligas: 2_100_000_u64.into(),
                                    storage_size: 0_u64.into(),
                                    paid_storage_size_diff: 0_u64.into(),
                                    allocated_destination_contract: false,
                                }
                            )),
                        }
                    )],
                })
            }
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
            1040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(dest),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: storage_value.clone(),
            },
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let storage = Some(storage_value.clone());

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
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
                result: ContentResult::Applied(TransferTarget::ToContrat(
                    TransferSuccess {
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
                        consumed_milligas: 171923_u64.into(),
                        storage_size: 0_u64.into(),
                        paid_storage_size_diff: 0_u64.into(),
                        allocated_destination_contract: false,
                    },
                )),
                internal_operation_results: vec![],
            }),
        }];

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
            1040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(dest),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(()).encode(),
            },
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
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
            })}];

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
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(0).encode(),
            },
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
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
            }),
        }];

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
            21000,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest.pkh),
            Parameters {
                entrypoint: mir::ast::Entrypoint::try_from("non_default")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(()).encode(),
            },
        );

        let receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let expected_receipt = vec![OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Transfer(OperationResult {
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
            }),
        }];

        assert_eq!(receipt, expected_receipt);
    }

    #[test]
    fn apply_three_valid_operations() {
        let mut host = MockKernelHost::default();
        let ctx = context::TezlinkContext::init_context();

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
            parameters: Parameters::default(),
        });

        // op‑3: transfer 20ꜩ to dest
        let transfer_content_2 = OperationContent::Transfer(TransferContent {
            amount: 20.into(),
            destination: Contract::Implicit(dest.pkh.clone()),
            parameters: Parameters::default(),
        });

        let batch = make_operation(
            5,
            1,
            21000,
            0,
            src.clone(),
            vec![reveal_content, transfer_content_1, transfer_content_2],
        );

        let receipts = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &ctx,
            OperationHash(H256::zero()),
            batch.clone(),
            &block_ctx!(),
            false,
        )
        .unwrap();

        let expected_receipts = zip_operations(
            batch,
            vec![
                OperationResultSum::Reveal(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
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
                        consumed_milligas: 172391_u64.into(),
                    }),
                    internal_operation_results: vec![],
                }),
                OperationResultSum::Transfer(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
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
                            consumed_milligas: 2100000_u64.into(),
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
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
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
                                        src.pkh,
                                    )),
                                    changes: -20,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                                BalanceUpdate {
                                    balance: Balance::Account(Contract::Implicit(
                                        dest.pkh,
                                    )),
                                    changes: 20,
                                    update_origin: UpdateOrigin::BlockApplication,
                                },
                            ],
                            ticket_receipt: vec![],
                            originated_contracts: vec![],
                            consumed_milligas: 2100000_u64.into(),
                            storage_size: 0_u64.into(),
                            paid_storage_size_diff: 0_u64.into(),
                            allocated_destination_contract: false,
                        },
                    )),
                    internal_operation_results: vec![],
                }),
            ],
        );

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
        let ctx = context::TezlinkContext::init_context();

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
            parameters: Parameters::default(),
        });

        let batch = make_operation(
            100,
            1,
            22000,
            0,
            src.clone(),
            vec![reveal_content, transfer_content],
        );

        let receipts = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &ctx,
            OperationHash(H256::zero()),
            batch,
            &block_ctx!(),
            false,
        );

        let expected_error =
            OperationError::Validation(ValidityError::CantPayFees(100_u64.into()));

        assert_eq!(receipts, Err(expected_error));

        assert_eq!(
            ctx.implicit_from_public_key_hash(&src.pkh)
                .unwrap()
                .balance(&host)
                .unwrap(),
            50u64.into()
        );

        assert_eq!(
            ctx.implicit_from_public_key_hash(&src.pkh)
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
            parameters: Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from("Hello world").encode(),
            },
        });

        let fail_transfer = OperationContent::Transfer(TransferContent {
            amount: 1.into(),
            destination: Contract::Originated(fail_dest.clone()),
            parameters: Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(()).encode(),
            },
        });

        let batch = make_operation(
            10_u64,
            1,
            21010,
            0,
            src.clone(),
            vec![reveal_content, succ_transfer, fail_transfer],
        );

        let receipts = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            batch,
            &block_ctx!(),
            false,
        )
        .unwrap();

        println!("{receipts:?}");

        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Reveal(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "First receipt should be BackTracked Reveal"
        );

        assert!(
            matches!(
                &receipts[1].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "Second receipt should be BackTracked Transfer"
        );

        assert!(
            matches!(
                &receipts[2].receipt,
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

        let context = context::TezlinkContext::init_context();

        let src_account = context
            .implicit_from_public_key_hash(&src.pkh)
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
            1040,
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
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        let mut origination_nonce =
            OriginationNonce::initial(OperationHash(H256::zero()));
        let expected_kt1 = origination_nonce.generate_kt1();

        let expected_receipt = OperationWithMetadata {
            content: operation.content[0].clone(),
            receipt: OperationResultSum::Origination(OperationResult {
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
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
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
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -9500,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::StorageFees,
                            changes: 9500,
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
                        contract: expected_kt1.clone(),
                    }],
                    consumed_milligas: 171777u64.into(),
                    storage_size: 38u64.into(),
                    paid_storage_size_diff: 38u64.into(),
                    lazy_storage_diff: None,
                }),
                internal_operation_results: vec![],
            }),
        };

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

        let smart_contract_account = context
            .originated_from_contract(&Contract::Originated(expected_kt1))
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
            smart_contract_code,
            Code::Code(code),
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
            21150,
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
                    parameters: Parameters {
                        entrypoint: mir::ast::Entrypoint::default(),
                        value: param_value.encode(),
                    },
                }),
            ],
        );
        let context = context::TezlinkContext::init_context();
        let receipts = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
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
                &receipts[0].receipt,
                OperationResultSum::Reveal(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "First receipt should be a BackTracked Reveal but is {:?}",
            receipts[0].receipt
        );
        assert!(
            matches!(
                &receipts[1].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::BackTracked(_),
                    ..
                })
            ),
            "Second receipt should be a BackTracked Transfer but is {:?}",
            receipts[1].receipt
        );

        // Check Internal Operations
        if let OperationResultSum::Transfer(OperationResult {
            internal_operation_results,
            ..
        }) = &receipts[1].receipt
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
            21040,
            5,
            src.clone(),
            transfer_amount.into(),
            Contract::Originated(contract_hash.clone()),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(()).encode(),
            },
        );

        let _receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            operation.hash().unwrap(),
            operation,
            &block_ctx!(),
            false,
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
            21040,
            5,
            src.clone(),
            transfer_amount.into(),
            Contract::Originated(contract_hash.clone()),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(()).encode(),
            },
        );

        let _receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            operation.hash().unwrap(),
            operation,
            &block_ctx!(),
            false,
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
            21040,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Originated(contract_hash.clone()),
            Parameters {
                entrypoint: mir::ast::Entrypoint::default(),
                value: Micheline::from(()).encode(),
            },
        );

        let _receipt = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context::TezlinkContext::init_context(),
            operation.hash().unwrap(),
            operation,
            &block_ctx!(),
            false,
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
        let context = context::TezlinkContext::init_context();

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
            22100,
            0,
            src.clone(),
            vec![OperationContent::Transfer(TransferContent {
                amount: 1000.into(),
                destination: Contract::Originated(contract_chapo_hash.clone()),
                parameters: Parameters {
                    entrypoint: mir::ast::Entrypoint::default(),
                    value: Micheline::from(()).encode(),
                },
            })],
        );
        let receipts = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
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
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "First receipt should be an Applied Transfer but is {:?}",
            receipts[0]
        );
        let internal_receipts = get_internal_receipts(&receipts[0].receipt);

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
                    consumed_milligas: 100000_u64.into(),
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
        let src_account = context
            .implicit_from_public_key_hash(&src.pkh)
            .expect("Should have succeeded to create an account");
        let init_contract_account = context
            .originated_from_contract(&Contract::Originated(contract_chapo_hash))
            .expect("Should have succeeded to create an account");
        let originated_account = context
            .originated_from_contract(&Contract::Originated(expected_address))
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
        let context = context::TezlinkContext::init_context();

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
            22300,
            0,
            src.clone(),
            vec![OperationContent::Transfer(TransferContent {
                amount: 0.into(),
                destination: Contract::Originated(contract_chapo_hash.clone()),
                parameters: Parameters {
                    entrypoint: mir::ast::Entrypoint::default(),
                    value: Micheline::from(()).encode(),
                },
            })],
        );
        let receipts = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation.clone(),
            &block_ctx!(),
            false,
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
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "First receipt should be an Applied Transfer but is {:?}",
            receipts[0].receipt
        );
        let internal_receipts = get_internal_receipts(&receipts[0].receipt);

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
                    consumed_milligas: 100000_u64.into(),
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
                    consumed_milligas: 100000_u64.into(),
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
        let ctx = context::TezlinkContext::init_context();
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

        // op-5 transfer: self-transfer 1ꜩ is skipped
        let transfer_content = OperationContent::Transfer(TransferContent {
            amount: 1.into(),
            destination: src_acc.contract(),
            parameters: Parameters::default(),
        });

        let batch = make_operation(
            5,
            1,
            21040,
            0,
            src.clone(),
            vec![
                reveal_content,
                origination_content_1,
                origination_content_2,
                origination_content_3,
                transfer_content,
            ],
        );

        let receipts = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &ctx,
            OperationHash(H256::zero()),
            batch.clone(),
            &block_ctx!(),
            false,
        )
        .unwrap();

        let mut orignation_nonce = OriginationNonce::initial(OperationHash(H256::zero()));
        let expected_kt1_1 = orignation_nonce.generate_kt1();
        let expected_kt1_2 = orignation_nonce.generate_kt1();
        let expected_receipts = zip_operations(
            batch,
            vec![
                OperationResultSum::Reveal(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
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
                        consumed_milligas: 177493_u64.into(),
                    })),
                    internal_operation_results: vec![],
                }),
                OperationResultSum::Origination(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
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
                            consumed_milligas: 102400_u64.into(),
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
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
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
                            consumed_milligas: 102400u64.into(),
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
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
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
                OperationResultSum::Transfer(OperationResult {
                    balance_updates: vec![
                        BalanceUpdate {
                            balance: Balance::Account(Contract::Implicit(
                                src.pkh.clone(),
                            )),
                            changes: -5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                        BalanceUpdate {
                            balance: Balance::BlockFees,
                            changes: 5,
                            update_origin: UpdateOrigin::BlockApplication,
                        },
                    ],
                    result: ContentResult::Skipped,
                    internal_operation_results: vec![],
                }),
            ],
        );
        assert_eq!(
            receipts, expected_receipts,
            "Receipts do not match the expected ones"
        );
        // Check the balances
        assert_eq!(
            src_acc.balance(&host).unwrap(),
            399975.into(),
            "Source account balance should be 399980ꜩ after the operations"
        );

        // Check the counters
        assert_eq!(
            src_acc.counter(&host).unwrap(),
            5.into(),
            "Source account counter should be 4 after the operations"
        );

        // Check the originated contracts
        let expected_contracts = [expected_kt1_1, expected_kt1_2];
        for (i, expected_kt1) in expected_contracts.iter().enumerate() {
            let account = ctx
                .originated_from_contract(&Contract::Originated(expected_kt1.clone()))
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

        let context = context::TezlinkContext::init_context();
        let balance = 10.into();

        let code = mir::parser::Parser::new()
            .parse_top_level(UNIT_SCRIPT)
            .expect("Should have succeeded to parse the script")
            .encode();
        let storage = Micheline::from(42).encode();
        let origination_content = OriginationContent {
            balance,
            delegate: None,
            script: Script { code, storage },
        };
        let operation = make_operation(
            10,
            1,
            1000,
            0,
            src.clone(),
            vec![OperationContent::Origination(origination_content)],
        );
        let receipts = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts.len(), 1, "There should be one receipt");
        assert!(matches!(
            &receipts[0].receipt,
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
        let context = context::TezlinkContext::init_context();
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
            21000,
            5,
            src.clone(),
            0.into(),
            Contract::Implicit(dst.pkh),
            Parameters::default(),
        );
        let receipts1 = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts1.len(), 1, "There should be one receipt");
        assert!(matches!(
            &receipts1[0].receipt,
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
            1110,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(kt1_addr.clone()),
            Parameters {
                entrypoint: Entrypoint::try_from("default")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(()).encode(),
            },
        );
        let receipts2 = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts2.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts2[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess { .. }
                    )),
                    ..
                })
            ),
            "Expected Successful Transfer operation result, got {:?}",
            receipts2[0].receipt
        );

        // An empty internal transfer to an implicit account fails.
        let operation = make_transfer_operation(
            15,
            3,
            23000,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(kt1_addr.clone()),
            Parameters {
                entrypoint: Entrypoint::try_from("call")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(src.clone().pkh.to_b58check()).encode(),
            },
        );
        let receipts3 = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts3.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts3[0].receipt,
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
            receipts3[0].receipt
        );

        // An empty internal transfer to a smart contract succeeds.
        let operation = make_transfer_operation(
            15,
            4,
            3000,
            5,
            src,
            0.into(),
            Contract::Originated(kt1_addr.clone()),
            Parameters {
                entrypoint: Entrypoint::try_from("call")
                    .expect("Entrypoint should be valid"),
                value: Micheline::from(kt1_addr.to_b58check()).encode(),
            },
        );
        let receipts4 = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts4.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts4[0].receipt,
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
            receipts4[0].receipt
        );
    }

    #[test]
    fn test_view_instruction() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let src = bootstrap1();
        let mut orignation_nonce = OriginationNonce::initial(OperationHash(H256::zero()));
        let view_addr = orignation_nonce.generate_kt1();
        let caller_addr = orignation_nonce.generate_kt1();

        let arena = Arena::new();

        init_account(&mut host, &src.pkh, 1000);
        reveal_account(&mut host, &src);
        let (code_view, storage_view) = (
            r#"
                parameter unit ;
                storage int ;
                code { CDR ; NIL operation ; PAIR } ;
                view "get_value" unit int { CDR ; }
            "#,
            &Micheline::from(5),
        );
        init_contract(&mut host, &view_addr, code_view, storage_view, &0.into());

        let mich_addr = TypedValue::Address(Address {
            hash: mir::ast::AddressHash::Kt1(view_addr),
            entrypoint: Entrypoint::default(),
        })
        .into_micheline_optimized_legacy(&arena);

        let (code_caller, storage_caller) = (
            r#"
                parameter unit ;
                storage address ;
                code { CDR ;
                    DUP ;
                    UNIT ;
                    VIEW "get_value" int ;
                    IF_NONE { PUSH string "View failed" ; FAILWITH }
                            { PUSH int 5 ; ASSERT_CMPEQ ; NIL operation ; PAIR } }
            "#,
            &mich_addr,
        );

        init_contract(
            &mut host,
            &caller_addr,
            code_caller,
            storage_caller,
            &0.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            21000,
            5,
            src.clone(),
            0.into(),
            Contract::Originated(caller_addr),
            Parameters::default(),
        );

        let receipts = validate_and_apply_operation(
            &mut host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert_eq!(receipts.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipts[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(TransferTarget::ToContrat(
                        TransferSuccess { .. }
                    )),
                    ..
                })
            ),
            "Expected Successful Transfer operation result, got {:?}",
            receipts[0].receipt
        );
    }

    const SCRIPTS_FOLDER: &str = "../../../michelson_test_scripts/big_maps/";

    fn read_script(file: &str) -> String {
        read_to_string(format!("{SCRIPTS_FOLDER}{file}"))
            .unwrap_or_else(|_| panic!("Contract source code not found for {file}"))
    }

    fn big_map_was_removed<Host: Runtime, C: Context>(
        ctx: &mut TcCtx<'_, Host, C>,
        id: BigMapId,
    ) {
        let types = ctx
            .big_map_get_type(&id)
            .expect("Get big_map type should not panic");
        assert_eq!(types, None, "Temporary big_map was not correctly removed");
    }

    struct BigMapTransfer {
        sender: TezlinkOriginatedAccount,
        receiver: TezlinkOriginatedAccount,
        receipts: Vec<OperationWithMetadata>,
    }

    fn transfer_big_map<C: Context>(
        ctx: &mut TcCtx<'_, impl Runtime, C>,
        tz1: &Bootstrap,
        script_sender: &str,
        init_sender: &str,
        script_receiver: &str,
        init_receiver: &str,
    ) -> BigMapTransfer {
        let sender_addr = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");
        let receiver_addr = ContractKt1Hash::from_base58_check(CONTRACT_2)
            .expect("ContractKt1Hash b58 conversion should have succeeded");
        init_account(ctx.host, &tz1.pkh, 1000);
        reveal_account(ctx.host, tz1);

        let parser = Parser::new();

        let sender_contract = init_contract(
            ctx.host,
            &sender_addr,
            script_sender,
            &parser
                .parse(init_sender)
                .expect("Failed to parse sender storage"),
            &0.into(),
        );

        let receiver_contract = init_contract(
            ctx.host,
            &receiver_addr,
            script_receiver,
            &parser
                .parse(init_receiver)
                .expect("Failed to parse receiver storage"),
            &0.into(),
        );

        let operation = make_transfer_operation(
            15,
            1,
            21040,
            5,
            tz1.clone(),
            10.into(),
            Contract::Originated(sender_addr.clone()),
            Parameters {
                entrypoint: Entrypoint::default(),
                value: Micheline::from(CONTRACT_2).encode(),
            },
        );

        let receipts = validate_and_apply_operation(
            ctx.host,
            &MockRegistry,
            ctx.context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        BigMapTransfer {
            sender: sender_contract,
            receiver: receiver_contract,
            receipts,
        }
    }

    fn test_transfer_big_map<'a>(
        script_receiver: &str,
        init_receiver: &str,
        script_sender: &str,
        init_sender: &str,
        expected_sender_big_map: Option<BTreeMap<TypedValue<'a>, TypedValue<'a>>>,
        expected_receiver_big_map: Option<BTreeMap<TypedValue<'a>, TypedValue<'a>>>,
    ) {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let tz1 = bootstrap1();

        let result = transfer_big_map(
            &mut ctx,
            &tz1,
            script_sender,
            init_sender,
            script_receiver,
            init_receiver,
        );

        big_map_was_removed(&mut ctx, (-1).into());

        for r in result.receipts {
            assert!(r.receipt.is_applied())
        }

        let parser = Parser::new();
        let sender_contract = result.sender;
        let receiver_contract = result.receiver;

        if let Some(expected_sender_big_map) = expected_sender_big_map {
            let storage = sender_contract.storage(ctx.host).unwrap();
            let mich_storage = Micheline::decode_raw(&parser.arena, &storage)
                .expect("Coudln't decode storage.");
            let big_map_id = typecheck_value(&mich_storage, &mut ctx, &Type::Int)
                .expect("Storage has unexpected type");
            match big_map_id {
                TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                    &mut ctx,
                    &parser.arena,
                    &id.into(),
                    Type::String,
                    Type::Bytes,
                    expected_sender_big_map,
                ),
                _ => panic!("ID should've been integer"),
            };
        }

        if let Some(expected_receiver_big_map) = expected_receiver_big_map {
            let storage = receiver_contract.storage(ctx.host).unwrap();
            let mich_storage = Micheline::decode_raw(&parser.arena, &storage)
                .expect("Coudln't decode storage.");
            match mich_storage {
                Micheline::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                    &mut ctx,
                    &Arena::new(),
                    &id.into(),
                    Type::String,
                    Type::Bytes,
                    expected_receiver_big_map,
                ),
                _ => panic!("ID should've been integer"),
            };
        }
    }

    // Receiver in {drop, store, store_updated} and Sender in {fresh, stored, stored_updated}
    #[test]
    fn big_map_transfer_receiver_drop_sender_fresh() {
        let script_receiver = read_script("receiver_drop.tz");
        let script_sender = read_script("sender_fresh.tz");
        test_transfer_big_map(
            &script_receiver,
            "Unit",
            &script_sender,
            "Unit",
            None,
            None,
        );
    }

    #[test]
    fn big_map_transfer_receiver_drop_sender_stored() {
        let script_receiver = read_script("receiver_drop.tz");
        let script_sender = read_script("sender_stored.tz");
        test_transfer_big_map(
            &script_receiver,
            "Unit",
            &script_sender,
            "{Elt \"d\" 0x; }",
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
            None,
        );
    }

    #[test]
    fn big_map_transfer_receiver_drop_sender_stored_updated() {
        let script_receiver = read_script("receiver_drop.tz");
        let script_sender = read_script("sender_stored_updated.tz");
        test_transfer_big_map(
            &script_receiver,
            "Unit",
            &script_sender,
            "{Elt \"b\" 0x; Elt \"d\" 0x; }",
            Some(BTreeMap::from([
                (TypedValue::String("d".into()), TypedValue::Bytes("".into())),
                (TypedValue::String("b".into()), TypedValue::Bytes("".into())),
            ])),
            None,
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_sender_fresh() {
        let script_receiver = read_script("receiver_store.tz");
        let script_sender = read_script("sender_fresh.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "Unit",
            None,
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_sender_stored() {
        let script_receiver = read_script("receiver_store.tz");
        let script_sender = read_script("sender_stored.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "{Elt \"d\" 0x; }",
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_sender_stored_updated() {
        let script_receiver = read_script("receiver_store.tz");
        let script_sender = read_script("sender_stored_updated.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "{Elt \"b\" 0x; Elt \"d\" 0x; }",
            Some(BTreeMap::from([
                (TypedValue::String("d".into()), TypedValue::Bytes("".into())),
                (TypedValue::String("b".into()), TypedValue::Bytes("".into())),
            ])),
            Some(BTreeMap::from([
                (TypedValue::String("d".into()), TypedValue::Bytes("".into())),
                (
                    TypedValue::String("a".into()),
                    TypedValue::Bytes(vec![0u8, 16u8]),
                ),
            ])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_updated_sender_fresh() {
        let script_receiver = read_script("receiver_store_updated.tz");
        let script_sender = read_script("sender_fresh.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "Unit",
            None,
            Some(BTreeMap::from([(
                TypedValue::String("c".into()),
                TypedValue::Bytes(vec![(16 + 1), (2 * 16 + 4)]),
            )])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_updated_sender_stored() {
        let script_receiver = read_script("receiver_store_updated.tz");
        let script_sender = read_script("sender_stored.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "{Elt \"d\" 0x; }",
            Some(BTreeMap::from([(
                TypedValue::String("d".into()),
                TypedValue::Bytes("".into()),
            )])),
            Some(BTreeMap::from([(
                TypedValue::String("c".into()),
                TypedValue::Bytes(vec![(16 + 1), (2 * 16 + 4)]),
            )])),
        );
    }

    #[test]
    fn big_map_transfer_receiver_store_updated_sender_stored_updated() {
        let script_receiver = read_script("receiver_store_updated.tz");
        let script_sender = read_script("sender_stored_updated.tz");
        test_transfer_big_map(
            &script_receiver,
            "{}",
            &script_sender,
            "{Elt \"b\" 0x; Elt \"d\" 0x; }",
            Some(BTreeMap::from([
                (TypedValue::String("d".into()), TypedValue::Bytes("".into())),
                (TypedValue::String("b".into()), TypedValue::Bytes("".into())),
            ])),
            Some(BTreeMap::from([
                (
                    TypedValue::String("c".into()),
                    TypedValue::Bytes(vec![(16 + 1), (2 * 16 + 4)]),
                ),
                (
                    TypedValue::String("a".into()),
                    TypedValue::Bytes(vec![0u8, 16u8]),
                ),
            ])),
        );
    }

    #[test]
    fn big_map_transfer_with_creation() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let tz1 = bootstrap1();

        let originator_addr = ContractKt1Hash::from_base58_check(CONTRACT_1)
            .expect("ContractKt1Hash b58 conversion should have succeeded");

        let script_originator = read_script("originator.tz");

        init_account(ctx.host, &tz1.pkh, 100_000_000);
        reveal_account(ctx.host, &tz1);
        let parser = Parser::new();

        let originator_contract = init_contract(
            ctx.host,
            &originator_addr,
            &script_originator,
            &parser
                .parse("{Elt \"b\" 0x; Elt \"d\" 0x; }")
                .expect("Could not parse initial storage"),
            &0.into(),
        );

        let operation = make_transfer_operation(
            0,
            1,
            21040,
            5,
            tz1.clone(),
            0.into(),
            Contract::Originated(originator_addr),
            Parameters::default(),
        );

        let receipts = validate_and_apply_operation(
            ctx.host,
            &MockRegistry,
            &context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        assert!(receipts.len() == 1);

        let receipt = receipts.first().unwrap();

        let internal_operation_results = match &receipt.receipt {
            OperationResultSum::Transfer(OperationResult {
                internal_operation_results,
                ..
            }) => internal_operation_results.as_slice(),
            _ => panic!("Failed to read originated contracts from receipt"),
        };

        let mut contracts: Vec<Originated> = vec![];
        for ir in internal_operation_results {
            let originated_contracts = match &ir {
                InternalOperationSum::Origination(InternalContentWithMetadata {
                    result:
                        ContentResult::Applied(OriginationSuccess {
                            originated_contracts,
                            ..
                        }),
                    ..
                }) => originated_contracts,
                _ => &vec![],
            };
            contracts.extend_from_slice(originated_contracts);
        }

        assert!(contracts.len() == 3);

        let Originated {
            contract: created_addr_0,
        } = &contracts[0];
        let created_acount_0 = ctx
            .context
            .originated_from_kt1(created_addr_0)
            .expect("Failed to retrieve generated account");

        let Originated {
            contract: created_addr_1,
        } = &contracts[1];
        let created_acount_1 = ctx
            .context
            .originated_from_kt1(created_addr_1)
            .expect("Failed to retrieve generated account");

        let Originated {
            contract: created_addr_2,
        } = &contracts[2];
        let created_acount_2 = ctx
            .context
            .originated_from_kt1(created_addr_2)
            .expect("Failed to retrieve generated account");

        let storage_originator = originator_contract
            .storage(ctx.host)
            .expect("Failed to fetch storage for originator");
        let mich_storage_originator =
            Micheline::decode_raw(&parser.arena, &storage_originator)
                .expect("Couldn't decode storage.");
        let big_map_id_originator =
            typecheck_value(&mich_storage_originator, &mut ctx, &Type::Int)
                .expect("Storage has unexpected type");
        match big_map_id_originator {
            TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                &mut ctx,
                &parser.arena,
                &id.into(),
                Type::String,
                Type::Bytes,
                BTreeMap::from([
                    (TypedValue::String("d".into()), TypedValue::Bytes(vec![])),
                    (TypedValue::String("b".into()), TypedValue::Bytes(vec![])),
                ]),
            ),
            _ => panic!("ID should've been integer"),
        }

        println!("Originator OK");

        let storage_0 = created_acount_0
            .storage(ctx.host)
            .expect("Failed to fetch storage for created account #0");
        let mich_storage_0 = Micheline::decode_raw(&parser.arena, &storage_0)
            .expect("Couldn't decode storage.");
        let big_map_id_0 = typecheck_value(&mich_storage_0, &mut ctx, &Type::Int)
            .expect("Storage has unexpected type");
        match big_map_id_0 {
            TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                &mut ctx,
                &parser.arena,
                &id.into(),
                Type::String,
                Type::Bytes,
                BTreeMap::from([(
                    TypedValue::String("d".into()),
                    TypedValue::Bytes("".into()),
                )]),
            ),
            _ => panic!("ID should've been integer"),
        }

        println!("Created_0 OK");

        let storage_1 = created_acount_1
            .storage(ctx.host)
            .expect("Failed to fetch storage for created account #1");
        let mich_storage_1 = Micheline::decode_raw(&parser.arena, &storage_1)
            .expect("Coudln't decode storage.");
        let big_map_id_1 = typecheck_value(&mich_storage_1, &mut ctx, &Type::Int)
            .expect("Storage has unexpected type");
        match big_map_id_1 {
            TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                &mut ctx,
                &parser.arena,
                &id.into(),
                Type::String,
                Type::Bytes,
                BTreeMap::from([
                    (TypedValue::String("d".into()), TypedValue::Bytes(vec![])),
                    (TypedValue::String("b".into()), TypedValue::Bytes(vec![])),
                ]),
            ),
            _ => panic!("ID should've been integer"),
        }

        println!("Created_1 OK");

        let storage_2 = created_acount_2
            .storage(ctx.host)
            .expect("Failed to fetch storage for created account #2");
        let mich_storage_2 = Micheline::decode_raw(&parser.arena, &storage_2)
            .expect("Coudln't decode storage.");
        let big_map_id_2 = typecheck_value(&mich_storage_2, &mut ctx, &Type::Int)
            .expect("Storage has unexpected type");
        match big_map_id_2 {
            TypedValue::Int(id) => crate::mir_ctx::tests::assert_big_map_eq(
                &mut ctx,
                &parser.arena,
                &id.into(),
                Type::String,
                Type::Bytes,
                BTreeMap::from([
                    (TypedValue::String("d".into()), TypedValue::Bytes(vec![])),
                    (
                        TypedValue::String("a".into()),
                        TypedValue::Bytes(vec![0u8, 16u8]),
                    ),
                ]),
            ),
            _ => panic!("ID should've been integer"),
        }

        println!("Created_2 OK");
    }

    #[test]
    fn verify_temp_big_map_content_is_cleaned() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let tz1 = bootstrap1();
        let parser = Parser::new();

        // A contract that receives a big_map and checks if
        // there's a "d" key in the big_map received
        let script_receiver = r#"
                parameter (big_map string bytes) ;
                storage bool ;
                code { CAR; PUSH string "d"; MEM; NIL operation; PAIR }
            "#;
        let script_sender = read_script("sender_stored.tz");

        // Transfer the big_map in the storage of the sender ({ Elt "d" 0x })
        // And the receiver checks if the big_map received contains the key "d"
        // The storage of the receiver contract should change to 'True' after this
        // transfer.
        let result = transfer_big_map(
            &mut ctx,
            &tz1,
            &script_sender,
            "{Elt \"d\" 0x; }",
            script_receiver,
            "False",
        );

        let receiver = result.receiver;

        for r in result.receipts {
            assert!(r.receipt.is_applied())
        }

        // Checks that the storage of the receiver is true
        let storage = receiver
            .storage(ctx.host)
            .expect("Get storage should succeed");
        let storage = Micheline::decode_raw(&parser.arena, &storage)
            .expect("Micheline should be decodable");
        let typed_storage = typecheck_value(&storage, &mut ctx, &Type::Bool)
            .expect("Typecheck value should succeed");
        assert_eq!(typed_storage, TypedValue::Bool(true));

        // We redeploy a second contract sender, that will send a different big_map
        // to the receiver contract ({Elt "a" 0x})
        let second_sender_contract =
            ContractKt1Hash::from_base58_check(CONTRACT_3).unwrap();

        let _ = init_contract(
            ctx.host,
            &second_sender_contract,
            &script_sender,
            &parser
                .parse("{Elt \"a\" 0x; }")
                .expect("Failed to parse receiver storage"),
            &0.into(),
        );

        let operation = make_transfer_operation(
            0,
            2,
            21040,
            5,
            tz1.clone(),
            0.into(),
            Contract::Originated(second_sender_contract),
            Parameters {
                entrypoint: Entrypoint::default(),
                value: Micheline::from(receiver.kt1().to_base58_check()).encode(),
            },
        );

        // After that call, the storage of the receiver should be 'False'
        // as the big_map passed in argument doesn't have the key "d"
        let receipts = validate_and_apply_operation(
            ctx.host,
            &MockRegistry,
            ctx.context,
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        for r in receipts {
            assert!(r.receipt.is_applied())
        }

        // Checks that the storage of the receiver contract is 'False'
        let storage = receiver
            .storage(ctx.host)
            .expect("Get storage should succeed");
        let storage = Micheline::decode_raw(&parser.arena, &storage)
            .expect("Micheline should be decodable");
        let typed_storage = typecheck_value(&storage, &mut ctx, &Type::Bool)
            .expect("Typecheck value should succeed");
        assert_eq!(typed_storage, TypedValue::Bool(false));
    }

    /// Test a successful transfer from a tz1 address to an Ethereum address via the gateway.
    /// This tests the happy path: source has enough balance, gateway executes, bridge succeeds.
    #[test]
    fn apply_transfer_to_gateway_happy_path() {
        let mut host = MockKernelHost::default();

        let src = bootstrap1();

        // Gateway contract address: KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw
        let gateway_kt1 =
            ContractKt1Hash::from_base58_check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw")
                .expect("Gateway KT1 address should be valid");

        // Set up source with enough balance (100 mutez: 15 for fees + 50 for transfer + some extra)
        let source_account = init_account(&mut host, &src.pkh, 100);
        reveal_account(&mut host, &src);

        // Create Micheline parameters: just the destination Ethereum address
        // The amount is taken from the operation's amount field via ctx.amount()
        let eth_destination = "0x1234567890123456789012345678901234567890";
        let params_micheline = Micheline::String(eth_destination.to_string());

        let operation = make_transfer_operation(
            15,      // fee
            1,       // counter
            100_000, // gas_limit
            100,     // storage_limit
            src.clone(),
            50_u64.into(), // amount to transfer
            Contract::Originated(gateway_kt1),
            Parameters {
                entrypoint: Entrypoint::default(),
                value: params_micheline.encode(),
            },
        );

        let registry = crate::test_utils::MockRegistry::new(vec![0x12; 20]);
        let receipt = validate_and_apply_operation(
            &mut host,
            &registry,
            &context::TezlinkContext::init_context(),
            OperationHash(H256::zero()),
            operation,
            &block_ctx!(),
            false,
        )
        .expect(
            "validate_and_apply_operation should not have failed with a kernel error",
        );

        // Verify the operation succeeded
        assert_eq!(receipt.len(), 1, "There should be one receipt");
        assert!(
            matches!(
                &receipt[0].receipt,
                OperationResultSum::Transfer(OperationResult {
                    result: ContentResult::Applied(_),
                    ..
                })
            ),
            "Expected Applied Transfer result, got {:?}",
            receipt[0].receipt
        );

        // Verify that source balance decreased by fee (15) + transfer amount (50)
        // Initial: 100, Final: 100 - 15 - 50 = 35
        assert_eq!(
            source_account.balance(&host).unwrap(),
            35_u64.into(),
            "Source balance should have decreased by fee + transfer amount"
        );

        // Verify that bridge was called with the correct amount
        let bridge_calls = registry.bridge_calls.borrow();
        assert_eq!(bridge_calls.len(), 1, "Bridge should have been called once");
        assert_eq!(
            bridge_calls[0].3, // amount is the 4th field in the tuple
            primitive_types::U256::from(50),
            "Bridge should have been called with transfer amount of 50"
        );
    }
}
