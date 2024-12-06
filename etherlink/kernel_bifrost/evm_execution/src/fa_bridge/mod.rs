// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
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

use std::borrow::Cow;

use deposit::FaDeposit;
use evm::{Config, ExitReason};
use primitive_types::{H160, U256};
use tezos_ethereum::block::BlockConstants;
use tezos_evm_logging::{
    log,
    Level::{Debug, Info},
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
    withdrawal_counter::WithdrawalCounter,
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

/// Gas limit for calling "deposit" method of the proxy contract call.
/// Since we cannot control a particular destination,
/// we need to make sure there's no DoS attack vector.
///
/// Current value reflects roughly the fees paid on L1 for initiating
/// the deposit. Since only smart contracts can mint tickets and send
/// internal inbox messages, the lower bound for a single deposit is
/// approximately 0.0005ꜩ; assuming current price per L2 gas of 1 Gwei
/// the equivalent amount of gas is 0.0005 * 10^18 / 10^9 = 500_000
///
/// Multiplying by two to enable more involved proxy contract implementations.
///
/// /!\ Note that if the EVM gas changes over future upgrades, we might break
/// compatibility with contract relying on this gas limit. If the EVM consumes
/// more gas in the future, we need to increase this gas limit as well. /!\
pub const FA_DEPOSIT_PROXY_GAS_LIMIT: u64 = 1_000_000;

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
pub fn execute_fa_deposit<'a, Host: Runtime>(
    host: &'a mut Host,
    block: &'a BlockConstants,
    evm_account_storage: &'a mut EthereumAccountStorage,
    precompiles: &'a PrecompileBTreeMap<Host>,
    config: Config,
    caller: H160,
    deposit: &FaDeposit,
    allocated_ticks: u64,
    tracer_input: Option<TracerInput>,
    gas_limit: u64,
) -> Result<ExecutionOutcome, EthereumError> {
    log!(host, Info, "Going to execute a {}", deposit.display());

    let mut handler = EvmHandler::<'_, Host>::new(
        host,
        evm_account_storage,
        caller,
        block,
        &config,
        precompiles,
        allocated_ticks,
        block.base_fee_per_gas(),
        // Warm-cold access only used for evaluation (for checking EVM compatibility), but not in production
        false,
        tracer_input,
    );

    handler.begin_initial_transaction(false, Some(gas_limit))?;

    // It's ok if internal proxy call fails, we will update the ticket table anyways.
    let ticket_owner = if let Some(proxy) = deposit.proxy {
        let (exit_reason, _, _) =
            inner_execute_proxy(&mut handler, caller, proxy, deposit.calldata())?;
        // If proxy contract call succeeded, proxy becomes the owner,
        // otherwise we fall back and set the receiver as the owner instead.
        if exit_reason.is_succeed() {
            proxy
        } else {
            log!(
                handler.borrow_host(),
                Info,
                "FA deposit: proxy call failed w/ {:?}",
                exit_reason
            );
            deposit.receiver
        }
    } else {
        // Proxy contract is not specified
        deposit.receiver
    };

    // Deposit execution might fail because of the balance overflow
    // so we need to rollback the entire transaction in that case.
    let deposit_res = inner_execute_deposit(&mut handler, ticket_owner, deposit);

    let mut outcome = handler.end_initial_transaction(deposit_res)?;

    // Adjust resource consumption to account for the outer transaction
    outcome.gas_used += config.gas_transaction_call;
    outcome.estimated_ticks_used += FA_DEPOSIT_EXECUTE_TICKS;

    Ok(outcome)
}

/// Execute the FA withdrawal within an execution layer. It aborts as soon as possible
/// on errors and the caller is responsible of cleaning up intermediate layer in
/// case of errors (i.e. using `end_inter_transaction`). It also can return the
/// withdrawal if it was succesful.
fn execute_layered_fa_withdrawal<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    caller: H160,
    withdrawal: FaWithdrawal,
) -> Result<(ExitReason, Option<Withdrawal>), EthereumError> {
    let (mut exit_status, _, _) = inner_execute_withdrawal(handler, &withdrawal)?;

    // Withdrawal execution might fail because of non sufficient balance
    // so we need to rollback the entire transaction in that case.
    if exit_status.is_succeed() {
        // In most cases sender is user's EOA and ticket owner is ERC wrapper contract
        if withdrawal.ticket_owner != withdrawal.sender {
            // If the proxy call fails we need to rollback the entire transaction
            (exit_status, _, _) = inner_execute_proxy(
                handler,
                caller,
                withdrawal.ticket_owner,
                withdrawal.calldata(),
            )?;
        }
        Ok((exit_status, Some(withdrawal.into_outbox_message())))
    } else {
        Ok((exit_status, None))
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
    withdrawal: FaWithdrawal,
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

        handler.begin_inter_transaction(false, gas_limit)?;

        // Execute the withdrawal in the transaction layer and clean it based
        // on the result.
        let (end_inter_transaction_result, withdrawals) =
            match execute_layered_fa_withdrawal(handler, caller, withdrawal) {
                Ok((exit_status, withdrawal_opt)) => {
                    let withdrawals = match withdrawal_opt {
                        Some(withdrawal) => vec![withdrawal],
                        None => vec![],
                    };
                    (
                        handler.end_inter_transaction::<EthereumError>(Ok((
                            exit_status,
                            None,
                            vec![],
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
            evm::Capture::Exit((exit_status, _, _)) => {
                Ok(PrecompileOutcome {
                    exit_status,
                    withdrawals,
                    output: vec![],
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
        handler
            .add_log(deposit.event_log(&ticket_owner))
            .map_err(|e| EthereumError::WrappedError(Cow::from(format!("{:?}", e))))?;
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
    withdrawal: &FaWithdrawal,
) -> Result<CreateOutcome, EthereumError> {
    // Updating the ticket table in accordance with the ownership.
    let mut system = handler.get_or_create_account(SYSTEM_ACCOUNT_ADDRESS)?;

    if system.ticket_balance_remove(
        handler.borrow_host(),
        &withdrawal.ticket_hash,
        &withdrawal.ticket_owner,
        withdrawal.amount,
    )? {
        let withdrawal_id =
            system.withdrawal_counter_get_and_increment(handler.borrow_host())?;

        handler
            .add_log(withdrawal.event_log(withdrawal_id))
            .map_err(|e| EthereumError::WrappedError(Cow::from(format!("{:?}", e))))?;

        Ok((ExitReason::Succeed(evm::ExitSucceed::Stopped), None, vec![]))
    } else {
        Ok(create_outcome_error!(
            "Insufficient ticket balance: {} of {} at {}",
            withdrawal.amount,
            withdrawal.ticket_hash,
            withdrawal.ticket_owner
        ))
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
