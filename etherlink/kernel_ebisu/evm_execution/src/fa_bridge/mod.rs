// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! FA token bridge.
//!
//! A permissionless transport protocol, that enables ticket transfers
//! from L1 to L2 and back, supporting two destination types:
//!     1. Simple address, which can be both externally owner account,
//!        or a smart contract wallet (that supports tickets)
//!     2. Proxy contract, exposing standard methods for deposits (on L2)
//!        and withdrawals (on L1); must handle both ticket and
//!        routing info that carries the final receiver address.
//!
//! FA bridge maintains the global ticket table, which is a ledger
//! tracking internal ticket ownerships on Etherlink side.
//!
//! FA bridge consists of two main parts:
//!     * The one responsible for deposit handling: integrates with the
//!       inbox handling flow, results in a pseudo transaction from
//!       Zero account.
//!     * The one responsible for withdrawal handling: implemented as
//!       as precompiled contract, which can be invoked both by EOA
//!       or another smart contract.
//!
//! It should be noted that FA withdrawal precompile DOES NOT post any
//! messages to the outbox since it cannot know if the outer transaction
//! fails or succeeds.
//!
//! All the state updates (ticket table, outbox message counter) are done
//! using the transactional Eth account storage, so that they are discarded
//! in case of a revert/failure.

use crate::account_storage::EthereumAccount;
use crate::fa_bridge::withdrawal::FaFastWithdrawal;
use crate::fa_bridge::withdrawal::FaWithdrawalMethods;
use crate::handler::trace_call;
use crate::withdrawal_counter::WithdrawalCounter;
use deposit::FaDeposit;
use deposit::FaDepositWithProxy;
use enum_dispatch::enum_dispatch;
use evm::CallScheme;
use evm::Capture;
use evm::ExitRevert;
use evm::{Config, ExitReason};
use host::path;
use host::path::OwnedPath;
use host::path::RefPath;
use host::runtime::RuntimeError;
use primitive_types::H256;
use primitive_types::{H160, U256};
use rlp::Decodable;
use rlp::Rlp;
use tezos_ethereum::access_list::empty_access_list;
use tezos_ethereum::block::BlockConstants;
use tezos_ethereum::Log;
use tezos_evm_logging::{
    log,
    Level::{Debug, Error, Info},
};
use tezos_evm_runtime::runtime::Runtime;
use ticket_table::TicketTable;
use withdrawal::FaWithdrawal;

use crate::{
    account_storage::EthereumAccountStorage,
    handler::{CreateOutcome, EvmHandler, ExecutionOutcome, Withdrawal},
    precompiles::{PrecompileBTreeMap, PrecompileOutcome, SYSTEM_ACCOUNT_ADDRESS},
    trace::TracerInput,
    transaction::TransactionContext,
    transaction_layer_data::CallContext,
    EthereumError,
};

pub mod deposit;
pub mod error;
pub mod ticket_table;
pub mod withdrawal;

#[cfg(test)]
mod tests;

#[cfg(any(test, feature = "fa_bridge_testing"))]
pub mod test_utils;

pub const FA_DEPOSIT_QUEUE_GAS_LIMIT: u64 = 0;

/// Overapproximation of the amount of ticks for updating
/// the global ticket table and emitting deposit event.
///
/// It does not include the ticks consumed by the ERC proxy execution
/// as it is accounted independently by the EVM hander.
///
/// Obtained by running the `bench_fa_deposit` script and examining the
/// `run_transaction_ticks` for the maximum value.
/// The final ticks amount has +50% safe reserve.
pub const FA_DEPOSIT_EXECUTE_TICKS: u64 = 2_250_000;

/// Overapproximation of the amount of ticks for parsing FA deposit.
/// Also includes hashing costs.
///
/// Obtained by running the `bench_fa_deposit` and examining both
/// `hashing_ticks` and `signature_verification_ticks` (parsing).
/// The final value is maximum total plus +50% reserve.
///
/// NOTE that we have a hard cap because of the maximum inbox message size limitation.
/// If it is lifted at some point in the future, we need to reflect that.
pub const TICKS_PER_FA_DEPOSIT_PARSING: u64 = 3_500_000;

macro_rules! create_outcome_error {
    ($($arg:tt)*) => {
        (evm::ExitReason::Error(evm::ExitError::Other(
            std::borrow::Cow::from(format!($($arg)*))
        )), None, vec![])
    };
}

#[enum_dispatch(FaWithdrawalMethods)]
pub enum FaWithdrawalKind {
    Standard(FaWithdrawal),
    Fast(FaFastWithdrawal),
}

const DEPOSIT_QUEUE_TABLE: RefPath = RefPath::assert_from(b"/deposits_table");

fn deposit_path(
    system: &EthereumAccount,
    withdrawal_id: &U256,
) -> Result<OwnedPath, EthereumError> {
    path::concat(
        &system.custom_path(&DEPOSIT_QUEUE_TABLE)?,
        &RefPath::assert_from(format!("/{withdrawal_id}").as_bytes()),
    )
    .map_err(EthereumError::from)
}

fn read_deposit_from_queue<Host: Runtime>(
    host: &Host,
    system: &EthereumAccount,
    deposit_id: &U256,
) -> Result<Option<FaDepositWithProxy>, EthereumError> {
    let deposit_path = deposit_path(system, deposit_id)?;
    let raw_deposit = host.store_read_all(&deposit_path);

    match raw_deposit {
        Ok(bytes) => FaDepositWithProxy::decode(&Rlp::new(&bytes))
            .map(Some)
            .map_err(EthereumError::from),
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(other) => Err(EthereumError::from(other)),
    }
}

fn remove_deposit<Host>(
    host: &mut Host,
    system: &EthereumAccount,
    withdrawal_id: &U256,
) -> Result<(), EthereumError>
where
    Host: Runtime,
{
    host.store_delete_value(&deposit_path(system, withdrawal_id)?)
        .map_err(EthereumError::from)
}

fn precompile_outcome_from_deposit_result<Host>(
    host: &mut Host,
    result: Result<(ExitReason, Option<H160>, Vec<u8>), EthereumError>,
) -> Result<PrecompileOutcome, EthereumError>
where
    Host: Runtime,
{
    result
        .inspect_err(|err| {
            // This is really problematic but should never happen; the ticket has been lost.
            // Needs an update to unlock.
            log!(
                host,
                Error,
                "FA deposit failed: couldn't even move the tickets into the ticket table {err:?}"
            );
        })
        .map(|(exit_status, _, output)| PrecompileOutcome {
            exit_status,
            output,
            withdrawals: vec![],
            estimated_ticks: 0,
        })
}

/// Executes FA deposit.
///
/// From the EVM perspective this is a "system contract" call,
/// that tries to perform an internal invocation of the proxy
/// contract, and emits an additional deposit event.
///
/// This method can only be called by the kernel, not by any
/// other contract. Therefore we assume there is no open
/// account storage transaction, and we can open one.
#[allow(clippy::too_many_arguments)]
#[cfg_attr(feature = "benchmark", inline(never))]
pub fn queue_fa_deposit<'a, Host: Runtime>(
    host: &'a mut Host,
    block: &'a BlockConstants,
    evm_account_storage: &'a mut EthereumAccountStorage,
    precompiles: &'a PrecompileBTreeMap<Host>,
    config: &Config,
    caller: H160,
    deposit: &FaDeposit,
    tracer_input: Option<TracerInput>,
) -> Result<(ExecutionOutcome, Option<U256>), EthereumError> {
    log!(host, Info, "Going to queue a {}", deposit.display());

    let mut handler = EvmHandler::<'_, Host>::new(
        host,
        evm_account_storage,
        caller,
        block,
        config,
        precompiles,
        block.base_fee_per_gas(),
        tracer_input,
        empty_access_list(),
    );

    handler.begin_initial_transaction(
        CallContext {
            is_static: false,
            is_creation: false,
        },
        Some(FA_DEPOSIT_QUEUE_GAS_LIMIT),
    )?;

    if let Some(deposit) = deposit.to_fa_deposit_with_proxy() {
        // Updating the ticket table in accordance with the ownership.
        let mut system = handler.get_or_create_account(SYSTEM_ACCOUNT_ADDRESS)?;
        let withdrawal_id =
            system.withdrawal_counter_get_and_increment(handler.borrow_host())?;

        let deposit_path = deposit_path(&system, &withdrawal_id)?;

        handler
            .host
            .store_write_all(&deposit_path, &deposit.to_rlp_bytes())?;

        handler.add_log(deposit.queued_log(&withdrawal_id))?;

        let res = Ok((
            ExitReason::Succeed(evm::ExitSucceed::Returned),
            None,
            Vec::new(),
        ));

        handler
            .end_initial_transaction(res)
            .map(|outcome| (outcome, Some(withdrawal_id)))
    } else {
        let deposit_res = inner_execute_deposit(&mut handler, deposit.receiver, deposit);

        // Even if the proxy fails, we can't revert the transaction: we need the
        // tickets to be moved in the ticket table.
        if let Err(ref ticket_err) = deposit_res {
            // This is really problematic: the ticket has been lost. Will need
            // an update to unlock.
            log!(
                handler.borrow_host(),
                Error,
                "FA deposit failed: couldn't even move the tickets into the \
                ticket table {ticket_err:?}"
            );
        }

        handler
            .end_initial_transaction(deposit_res)
            .map(|outcome| (outcome, None))
    }
}

#[cfg_attr(feature = "benchmark", inline(never))]
pub fn claim_fa_deposit<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    deposit_id: U256,
) -> Result<PrecompileOutcome, EthereumError> {
    let system = handler.get_or_create_account(SYSTEM_ACCOUNT_ADDRESS)?;

    if let Some(deposit) = read_deposit_from_queue(handler.host, &system, &deposit_id)? {
        let gas_limit = handler.nested_call_gas_limit(None);
        if let Err(err) = handler.record_cost(gas_limit.unwrap_or_default()) {
            log!(
                handler.host,
                Debug,
                "Not enough gas for the proxy call. Returned with error {:?}. \
                 Required at least: {:?}",
                err,
                gas_limit
            );

            return Ok(PrecompileOutcome {
                exit_status: ExitReason::Error(evm::ExitError::OutOfGas),
                withdrawals: vec![],
                estimated_ticks: 0,
                output: vec![],
            });
        }

        handler.begin_inter_transaction(
            CallContext {
                is_static: false,
                is_creation: false,
            },
            gas_limit,
        )?;

        let gas_before = handler.gas_used();

        // {Note system address}
        // For backward compatibility, we need to lie about the caller of the transaction.
        // This is because the proxy contract already originated to the existing network
        // assume they are called by the system address, as it was the workflow in the
        // previous version of the bridge.
        let inner_result = inner_execute_proxy(
            handler,
            SYSTEM_ACCOUNT_ADDRESS,
            deposit.proxy,
            deposit.calldata(),
        );
        let gas_after = handler.gas_used();

        let proxy_res = handler.end_inter_transaction::<EthereumError>(inner_result);

        match proxy_res {
            Capture::Exit((exit_status, _, output)) => {
                trace_call(
                    handler,
                    CallScheme::Call,
                    // Put here for backward compatibility, see {Note system address}
                    SYSTEM_ACCOUNT_ADDRESS,
                    U256::zero(),
                    gas_after - gas_before,
                    deposit.calldata(),
                    deposit.proxy,
                    deposit.proxy,
                    gas_limit,
                    &output,
                    &exit_status,
                );

                if exit_status.is_succeed() {
                    log!(handler.host, Debug, "Proxy call succeeded (FA bridge)");
                    let deposit_res = inner_execute_deposit(
                        handler,
                        deposit.proxy,
                        &deposit.to_fa_deposit(),
                    );
                    remove_deposit(handler.host, &system, &deposit_id)?;
                    precompile_outcome_from_deposit_result(handler.host, deposit_res)
                } else if exit_status.is_out_of_gas() {
                    Ok(PrecompileOutcome {
                        exit_status,
                        withdrawals: vec![],
                        output,
                        estimated_ticks: 0,
                    })
                } else {
                    // The proxy call failed, and it was not because of a lack of gas (which an attacker could trigger by
                    // claiming it with a gas limit voluntarily too low)
                    log!(
                        handler.host,
                        Info,
                        "Proxy call failed with {:?}",
                        exit_status
                    );
                    let deposit_res = inner_execute_deposit(
                        handler,
                        deposit.receiver,
                        &deposit.to_fa_deposit(),
                    );
                    remove_deposit(handler.host, &system, &deposit_id)?;
                    precompile_outcome_from_deposit_result(handler.host, deposit_res)
                }
            }
            Capture::Trap(err) => Err(err),
        }
    } else {
        // The deposit is absent from the queue, meaning it is no longer possible to execute it
        Ok(PrecompileOutcome {
            exit_status: ExitReason::Revert(ExitRevert::Reverted),
            withdrawals: vec![],
            output: vec![],
            estimated_ticks: 0,
        })
    }
}

/// Execute the FA withdrawal within an execution layer. It aborts as soon as possible
/// on errors and the caller is responsible of cleaning up intermediate layer in
/// case of errors (i.e. using `end_inter_transaction`). It also can return the
/// withdrawal if it was succesful.
fn execute_layered_fa_withdrawal<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    caller: H160,
    withdrawal: FaWithdrawalKind,
) -> Result<(ExitReason, Option<Withdrawal>, Vec<u8>), EthereumError> {
    if let Some(withdrawal_id) = inner_execute_withdrawal(handler, &withdrawal)? {
        let mut exit_status = ExitReason::Succeed(evm::ExitSucceed::Stopped);
        let mut output = Vec::new();
        if withdrawal.ticket_owner() != withdrawal.sender() {
            // If the proxy call fails we need to rollback the entire transaction
            (exit_status, _, output) = inner_execute_proxy(
                handler,
                caller,
                withdrawal.ticket_owner(),
                withdrawal.calldata(),
            )?;
        }
        Ok((
            exit_status,
            Some(withdrawal.into_outbox_message(withdrawal_id)),
            output,
        ))
    } else {
        let (exit_status, _, _): CreateOutcome = create_outcome_error!(
            "Insufficient ticket balance: {} of {} at {}",
            withdrawal.amount(),
            withdrawal.ticket_hash(),
            withdrawal.ticket_owner()
        );
        Ok((exit_status, None, Vec::new()))
    }
}

/// Executes FA withdrawal.
///
/// From the EVM perspective this is a precompile contract
/// call, that can be potentially an internal invocation from
/// another smart contract.
///
/// We assume there is an open account storage transaction.
pub fn execute_fa_withdrawal<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    caller: H160,
    withdrawal: FaWithdrawalKind,
) -> Result<PrecompileOutcome, EthereumError> {
    log!(
        handler.borrow_host(),
        Info,
        "Going to execute a {}",
        withdrawal.display()
    );

    if handler.can_begin_inter_transaction_call_stack() {
        // Create a new transaction layer with 63/64 of the remaining gas.
        let gas_limit = handler.nested_call_gas_limit(None);

        if let Err(err) = handler.record_cost(gas_limit.unwrap_or_default()) {
            log!(
                handler.borrow_host(),
                Debug,
                "Not enough gas for create. Required at least: {:?} ({:?})",
                gas_limit,
                err
            );
            return Ok(PrecompileOutcome {
                exit_status: ExitReason::Error(evm::ExitError::OutOfGas),
                withdrawals: vec![],
                output: vec![],
                // Precompile and inner proxy calls have already registered their costs
                estimated_ticks: 0,
            });
        }

        handler.begin_inter_transaction(
            CallContext {
                is_static: false,
                is_creation: false,
            },
            gas_limit,
        )?;

        // Execute the withdrawal in the transaction layer and clean it based
        // on the result.
        let (end_inter_transaction_result, withdrawals) =
            match execute_layered_fa_withdrawal(handler, caller, withdrawal) {
                Ok((exit_status, withdrawal_opt, output)) => {
                    let withdrawals = match withdrawal_opt {
                        Some(withdrawal) => vec![withdrawal],
                        None => vec![],
                    };
                    (
                        handler.end_inter_transaction::<EthereumError>(Ok((
                            exit_status,
                            None,
                            output,
                        ))),
                        withdrawals,
                    )
                }
                Err(err) => (
                    handler.end_inter_transaction::<EthereumError>(Err(err)),
                    vec![],
                ),
            };
        // Transforms the transaction clean up result into a Sputnik call exit
        // type.
        match end_inter_transaction_result {
            evm::Capture::Exit((exit_status, _, output)) => {
                Ok(PrecompileOutcome {
                    exit_status,
                    withdrawals,
                    output,
                    // Precompile and inner proxy calls have already registered their costs
                    estimated_ticks: 0,
                })
            }
            evm::Capture::Trap(err) => Err(err),
        }
    } else {
        Ok(PrecompileOutcome {
            exit_status: ExitReason::Error(evm::ExitError::CallTooDeep),
            withdrawals: vec![],
            output: vec![],
            // Precompile and inner proxy calls have already registered their costs
            estimated_ticks: 0,
        })
    }
}

/// Updates ticket table according to the deposit and actual ticket owner.
/// Assuming there is an open account storage transaction.
fn inner_execute_deposit<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    ticket_owner: H160,
    deposit: &FaDeposit,
) -> Result<CreateOutcome, EthereumError> {
    // Updating the ticket table in accordance with the ownership.
    let mut system = handler.get_or_create_account(SYSTEM_ACCOUNT_ADDRESS)?;

    if system.ticket_balance_add(
        handler.borrow_host(),
        &deposit.ticket_hash,
        &ticket_owner,
        deposit.amount,
    )? {
        handler.add_log(deposit.event_log(&ticket_owner))?;
        Ok((
            ExitReason::Succeed(evm::ExitSucceed::Returned),
            None,
            vec![],
        ))
    } else {
        Ok(create_outcome_error!(
            "Ticket table balance overflow: {} at {}",
            deposit.ticket_hash,
            ticket_owner
        ))
    }
}

/// Updates ticket ledger and outbox counter according to the withdrawal.
/// Assuming there is an open account storage transaction.
fn inner_execute_withdrawal<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    withdrawal: &FaWithdrawalKind,
) -> Result<Option<U256>, EthereumError> {
    // Updating the ticket table in accordance with the ownership.
    let mut system = handler.get_or_create_account(SYSTEM_ACCOUNT_ADDRESS)?;

    if system.ticket_balance_remove(
        handler.borrow_host(),
        &withdrawal.ticket_hash(),
        &withdrawal.ticket_owner(),
        withdrawal.amount(),
    )? {
        let withdrawal_id =
            system.withdrawal_counter_get_and_increment(handler.borrow_host())?;
        handler.add_log(withdrawal.event_log(withdrawal_id))?;

        Ok(Some(withdrawal_id))
    } else {
        Ok(None)
    }
}

/// Invokes proxy (ERC wrapper) contract from within a deposit or
/// withdrawal handling function.
/// Assuming there is an open account storage transaction.
fn inner_execute_proxy<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    caller: H160,
    proxy: H160,
    input: Vec<u8>,
) -> Result<CreateOutcome, EthereumError> {
    // We need to check that the proxy contract exists and has code,
    // because otherwise the inner call will succeed although without
    // any effect.
    //
    // Of course, we cannot protect from cases where proxy contract
    // executes without errors, but does not actually update the ledger.
    // At very least we can protect from typos and other mistakes.
    if let Some(account) = handler.get_account(proxy)? {
        if let Ok(true) = account.code_exists(handler.borrow_host()) {
            handler.execute_call(
                proxy,
                None,
                input,
                TransactionContext::new(caller, proxy, U256::zero()),
            )
        } else {
            Ok(create_outcome_error!(
                "Proxy contract does not have code: {}",
                proxy
            ))
        }
    } else {
        Ok(create_outcome_error!(
            "Proxy contract does not exist: {}",
            proxy
        ))
    }
}
