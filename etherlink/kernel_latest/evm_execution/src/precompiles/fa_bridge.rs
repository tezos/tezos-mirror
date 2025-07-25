// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

//! FA bridge precompiled contract.
//!
//! Provides users with EVM interface for:
//!     * Submitting ticket withdrawal requests
//!
//! This is a stateful precompile:
//!     * Alters ticket table (changes balance)
//!     * Increments outbox counter
use crate::fa_bridge::claim_fa_deposit;
use crate::fa_bridge::withdrawal::FaFastWithdrawal;
use crate::fa_bridge::FaWithdrawalKind::{Fast, Standard};
use crate::fast_fa_withdrawals_enabled;
use crate::utilities::alloy::alloy_to_u256;
use crate::EthereumError;
use alloy_sol_types::SolEvent;
use evm::{Context, Handler, Transfer};
use primitive_types::{H160, U256};
use tezos_evm_runtime::runtime::Runtime;

use crate::{
    fa_bridge::{execute_fa_withdrawal, withdrawal::FaWithdrawal},
    handler::EvmHandler,
};

use super::{PrecompileOutcome, FA_BRIDGE_PRECOMPILE_ADDRESS};

/// Overapproximation of the amount of ticks for parsing
/// FA withdrawal from calldata, checking transfer value,
/// and executing the FA withdrawal, excluding the ticks consumed by
/// the inner proxy call.
///
/// Linear regression parameters are obtained by running `bench_fa_withdrawal`
/// script, evaluating `run_transaction_ticks` against `data_size`.
/// Intercept is extended with +50% safe reserve.
pub const FA_WITHDRAWAL_PRECOMPILE_TICKS_INTERCEPT: u64 = 4_000_000;
pub const FA_WITHDRAWAL_PRECOMPILE_TICKS_SLOPE: u64 = 700;

/// Added ("artificial") cost of doing FA withdrawal not including actual gas
/// spent for executing the withdrawal (and inner proxy contract call).
///
/// This is roughly the implied costs of executing the outbox message on L1
/// as a spam prevention mechanism (outbox queue clogging).
/// In particular it prevents cases when a big number of withdrawals is batched
/// together in a single transaction which exploits the system.
///
/// An execution of a single outbox message carrying a FA withdrawal
/// costs around 0.0025ꜩ on L1; the equivalent amount of gas units on L2 is:
///
///  0.0025 * 10^18 / GAS_PRICE
///
/// Multiplying the numerator by 2 for a safe reserve and this is our cost in Wei.
pub const FA_WITHDRAWAL_PRECOMPILE_ADDED_COST: u64 = 5_000_000_000_000_000;

/// Hard cap for the added gas cost (0.5 of the maximum gas limit per transaction).
/// If gas price drops the gas amount rises, but we don't want it to hit the transaction
/// gas limit.
pub const FA_WITHDRAWAL_PRECOMPILE_MAX_ADDED_CAS_COST: u64 = 15_000_000;

/// Calculate precompile gas cost given the estimated amount of ticks and gas price.
fn estimate_gas_cost(estimated_ticks: u64, gas_price: U256) -> u64 {
    // Using 1 gas unit ~= 1000 ticks convert ratio
    let execution_cost = estimated_ticks / 1000;
    let added_cost = U256::from(FA_WITHDRAWAL_PRECOMPILE_MAX_ADDED_CAS_COST)
        .min(U256::from(FA_WITHDRAWAL_PRECOMPILE_ADDED_COST) / gas_price);
    execution_cost + added_cost.as_u64()
}

macro_rules! precompile_outcome_error {
    ($($arg:tt)*) => {
        crate::precompiles::PrecompileOutcome {
            exit_status: evm::ExitReason::Error(evm::ExitError::Other(
                std::borrow::Cow::from(format!($($arg)*))
            )),
            withdrawals: vec![],
            output: vec![],
            estimated_ticks: 0,
        }
    };
}

alloy_sol_types::sol! {
    event SolClaimInput (
        uint256 deposit_id,
    );
}

pub fn guard_withdrawals<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
) -> Option<PrecompileOutcome> {
    // We register the cost of the precompile early to prevent cases where inner proxy call
    // consumes more ticks than allowed.
    let estimated_ticks = FA_WITHDRAWAL_PRECOMPILE_TICKS_SLOPE * (input.len() as u64)
        + FA_WITHDRAWAL_PRECOMPILE_TICKS_INTERCEPT;
    handler.estimated_ticks_used += estimated_ticks;

    // We also record gas cost which consists of computation cost (1 gas unit per 1000 ticks)
    // and added FA withdrawal cost (spam prevention measure).
    let estimated_gas_cost = estimate_gas_cost(estimated_ticks, handler.gas_price());
    if handler.record_cost(estimated_gas_cost).is_err() {
        return Some(precompile_outcome_error!(
            "FA withdrawal: gas limit too low"
        ));
    }

    None
}

pub fn guard_invalid_call(
    context: &Context,
    is_static: bool,
    transfer: Option<Transfer>,
) -> Option<PrecompileOutcome> {
    if is_static {
        // It is a STATICCALL that prevents storage modification
        // see https://eips.ethereum.org/EIPS/eip-214
        return Some(precompile_outcome_error!(
            "FA withdrawal: static call not allowed"
        ));
    }

    if context.address != FA_BRIDGE_PRECOMPILE_ADDRESS {
        // It is a DELEGATECALL or CALLCODE (deprecated) which can be impersonating
        // see https://eips.ethereum.org/EIPS/eip-7
        return Some(precompile_outcome_error!(
            "FA withdrawal: delegate call not allowed"
        ));
    }

    if transfer
        .as_ref()
        .map(|t| !t.value.is_zero())
        .unwrap_or(true)
    {
        return Some(precompile_outcome_error!(
            "FA withdrawal: unexpected value transfer {:?}",
            transfer
        ));
    }

    None
}

/// FA bridge precompile entrypoint.
#[allow(unused)]
pub fn fa_bridge_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    context: &Context,
    is_static: bool,
    transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    if let Some(outcome) = guard_invalid_call(context, is_static, transfer) {
        return Ok(outcome);
    }

    match input {
        // "withdraw"'s selector | 4 first bytes of keccak256("withdraw(address,bytes,uint256,bytes22,bytes)")
        [0x80, 0xfc, 0x1f, 0xe3, input_data @ ..] => {
            if let Some(outcome) = guard_withdrawals(handler, input) {
                return Ok(outcome);
            }

            // Withdrawal initiator is the precompile caller.
            // NOTE that since we deny delegate calls, it can either be EOA or
            // a smart contract that calls the precompile directly (e.g. AA wallet).
            match FaWithdrawal::try_parse(input_data, context.caller) {
                Ok(withdrawal) => {
                    // Using Zero account here so that the inner proxy call
                    // has the same sender as during the FA deposit
                    // (so that the proxy contract has a single admin).
                    execute_fa_withdrawal(handler, H160::zero(), Standard(withdrawal))
                }
                Err(err) => Ok(precompile_outcome_error!(
                    "FA withdrawal: parsing failed w/ `{err}`"
                )),
            }
        }

        // "fast withdrawal"'s selector | 4 first bytes of keccak256("fa_fast_withdraw(address,bytes,uint256,bytes22,bytes,string,bytes)")
        [0xff, 0xf7, 0xca, 0x5f, input_data @ ..] => {
            if let Some(outcome) = guard_withdrawals(handler, input) {
                return Ok(outcome);
            }

            if !fast_fa_withdrawals_enabled(handler.host) {
                return Ok(precompile_outcome_error!(
                    "Fast FA withdrawal: parsing failed w/ `The fast FA withdrawal
                     feature flag is not enabled, cannot call this entrypoint.`"
                ));
            };
            // Withdrawal initiator is the precompile caller.
            // NOTE that since we deny delegate calls, it can either be EOA or
            // a smart contract that calls the precompile directly (e.g. AA wallet).
            let timestamp_u256 = handler.block_timestamp();
            match FaFastWithdrawal::try_parse(input_data, timestamp_u256, context.caller)
            {
                Err(err) => Ok(precompile_outcome_error!(
                    "Fast FA withdrawal: parsing failed w/ `{err}`"
                )),
                Ok(withdrawal) => {
                    // Using Zero account here so that the inner proxy call
                    // has the same sender as during the FA deposit
                    // (so that the proxy contract has a single admin).
                    execute_fa_withdrawal(handler, H160::zero(), Fast(withdrawal))
                }
            }
        }
        // "claim"'s selector | 4 first bytes of keccak256("claim(uint256)")
        [0x37, 0x96, 0x07, 0xf5, input_data @ ..] => {
            match SolClaimInput::abi_decode_data(input_data) {
                Err(err) => Ok(precompile_outcome_error!(
                    "Claim FA deposit: parsing failed w/ `{err}`"
                )),
                Ok(deposit_id) => {
                    let id = alloy_to_u256(&deposit_id.0);
                    claim_fa_deposit(handler, id)
                }
            }
        }
        _ => Ok(precompile_outcome_error!(
            "FA withdrawal: unexpected selector"
        )),
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use alloy_sol_types::SolCall;
    use evm::ExitError;
    use primitive_types::{H160, U256};
    use tezos_data_encoding::enc::BinWriter;
    use tezos_ethereum::access_list::empty_access_list;
    use tezos_evm_runtime::runtime::MockKernelHost;

    use crate::{
        account_storage::{init_account_storage, EthereumAccountStorage},
        configuration::EVMVersion,
        fa_bridge::{
            deposit::ticket_hash,
            test_utils::{
                convert_h160, convert_u256, deploy_reentrancy_tester,
                dummy_fa_withdrawal, dummy_first_block, dummy_ticket, kernel_wrapper,
                set_balance, ticket_balance_add, ticket_id,
            },
        },
        handler::{EvmHandler, ExecutionOutcome, ExecutionResult},
        precompiles::{self, FA_BRIDGE_PRECOMPILE_ADDRESS},
        transaction::TransactionContext,
        transaction_layer_data::CallContext,
        utilities::{bigint_to_u256, keccak256_hash},
    };

    #[allow(clippy::too_many_arguments)]
    fn execute_precompile(
        host: &mut MockKernelHost,
        evm_account_storage: &mut EthereumAccountStorage,
        caller: H160,
        value: U256,
        input: Vec<u8>,
        gas_limit: Option<u64>,
        is_static: bool,
        disable_reentrancy_guard: bool,
    ) -> ExecutionOutcome {
        let block = dummy_first_block();
        let config = EVMVersion::current_test_config();
        let callee = FA_BRIDGE_PRECOMPILE_ADDRESS;

        let precompiles = precompiles::precompile_set::<MockKernelHost>(true);

        let mut handler = EvmHandler::new(
            host,
            evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            U256::from(21000),
            None,
            empty_access_list(),
        );

        if disable_reentrancy_guard {
            handler.disable_reentrancy_guard();
        }

        handler
            .call_contract(caller, callee, Some(value), input, gas_limit, is_static)
            .expect("Failed to invoke precompile")
    }

    #[test]
    fn fa_bridge_precompile_fails_due_to_bad_selector() {
        let mut mock_runtime = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let outcome = execute_precompile(
            &mut mock_runtime,
            &mut evm_account_storage,
            H160::from_low_u64_be(1),
            U256::zero(),
            vec![0x00, 0x01, 0x02, 0x03],
            None,
            false,
            false,
        );
        assert!(!outcome.is_success());
        assert!(
            matches!(outcome.result, ExecutionResult::Error(ExitError::Other(err)) if err.contains("unexpected selector"))
        );
    }

    #[test]
    fn fa_bridge_precompile_fails_due_to_low_gas_limit() {
        let mut mock_runtime = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let outcome = execute_precompile(
            &mut mock_runtime,
            &mut evm_account_storage,
            H160::from_low_u64_be(1),
            U256::zero(),
            vec![0x80, 0xfc, 0x1f, 0xe3],
            // Cover only basic cost
            Some(21000 + 16 * 4),
            false,
            false,
        );
        assert!(!outcome.is_success());
        assert!(
            matches!(outcome.result, ExecutionResult::Error(ExitError::Other(err)) if err.contains("gas limit too low"))
        );
    }

    #[test]
    fn fa_bridge_precompile_fails_due_to_non_zero_value() {
        let mut mock_runtime = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let caller = H160::from_low_u64_be(1);
        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            1_000_000_000.into(),
        );

        let outcome = execute_precompile(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            1_000_000_000.into(),
            vec![0x80, 0xfc, 0x1f, 0xe3],
            None,
            false,
            false,
        );
        assert!(!outcome.is_success());
        assert!(
            matches!(outcome.result, ExecutionResult::Error(ExitError::Other(err)) if err.contains("unexpected value transfer"))
        );
    }

    #[test]
    fn fa_bridge_precompile_fails_due_to_static_call() {
        let mut mock_runtime = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let caller = H160::from_low_u64_be(1);
        let outcome = execute_precompile(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            U256::zero(),
            vec![0x80, 0xfc, 0x1f, 0xe3],
            None,
            true,
            false,
        );
        assert!(!outcome.is_success());
        assert!(
            matches!(outcome.result, ExecutionResult::Error(ExitError::Other(err)) if err.contains("static call not allowed"))
        );
    }

    #[test]
    fn fa_bridge_precompile_fails_due_to_delegate_call() {
        let mut mock_runtime = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let caller = H160::from_low_u64_be(1);
        let callee = H160::from_low_u64_be(2);
        let block = dummy_first_block();
        let config = EVMVersion::current_test_config();

        let precompiles = precompiles::precompile_set::<MockKernelHost>(true);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            U256::from(21000),
            None,
            empty_access_list(),
        );

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_call(
            FA_BRIDGE_PRECOMPILE_ADDRESS,
            None,
            vec![0x80, 0xfc, 0x1f, 0xe3],
            TransactionContext::new(caller, callee, U256::zero()),
        );

        let outcome = handler.end_initial_transaction(result).unwrap();

        assert!(!outcome.is_success());
        assert!(
            matches!(outcome.result, ExecutionResult::Error(ExitError::Other(err)) if err.contains("delegate call not allowed"))
        );
    }

    #[test]
    fn fa_bridge_precompile_fails_due_to_invalid_input() {
        let mut mock_runtime = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let outcome = execute_precompile(
            &mut mock_runtime,
            &mut evm_account_storage,
            H160::from_low_u64_be(1),
            U256::zero(),
            vec![0x80, 0xfc, 0x1f, 0xe3],
            None,
            false,
            false,
        );
        assert!(!outcome.is_success());
        assert!(
            matches!(outcome.result, ExecutionResult::Error(ExitError::Other(err)) if err.contains("parsing failed"))
        );
    }

    #[test]
    fn fa_bridge_precompile_succeeds_without_l2_proxy_contract() {
        let mut mock_runtime = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let ticket_owner = H160::from_low_u64_be(1);
        let ticket = dummy_ticket();
        let ticket_hash = ticket_hash(&ticket).unwrap();
        let amount = bigint_to_u256(ticket.amount()).unwrap();

        // Patch ticket table
        ticket_balance_add(
            &mut mock_runtime,
            &mut evm_account_storage,
            &ticket_hash,
            &ticket_owner,
            amount,
        );

        let (ticketer, content) = ticket_id(&ticket);

        let routing_info = [
            [0u8; 22].to_vec(),
            vec![0x01],
            [0u8; 20].to_vec(),
            vec![0x00],
        ]
        .concat();

        let input = kernel_wrapper::withdrawCall::new((
            convert_h160(&ticket_owner),
            routing_info.into(),
            convert_u256(&amount),
            ticketer.into(),
            content.into(),
        ))
        .abi_encode();

        let outcome = execute_precompile(
            &mut mock_runtime,
            &mut evm_account_storage,
            ticket_owner,
            U256::zero(),
            input,
            Some(30_000_000),
            false,
            false,
        );
        assert!(outcome.is_success());
        assert_eq!(1, outcome.withdrawals.len());
        assert_eq!(1, outcome.logs.len());
    }

    #[test]
    fn fa_bridge_precompile_address() {
        assert_eq!(
            FA_BRIDGE_PRECOMPILE_ADDRESS,
            H160::from_str("ff00000000000000000000000000000000000002").unwrap()
        );
    }

    #[test]
    fn fa_bridge_precompile_withdraw_method_id() {
        let method_hash =
            keccak256_hash(b"withdraw(address,bytes,uint256,bytes22,bytes)");
        assert_eq!(method_hash.0[0..4], [0x80, 0xfc, 0x1f, 0xe3]);
    }

    #[test]
    fn fa_bridge_precompile_cannot_call_itself() {
        let mut mock_runtime = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let system = H160::zero();
        let sender = H160::from_low_u64_be(1);
        let ticket = dummy_ticket();

        let proxy = deploy_reentrancy_tester(
            &mut mock_runtime,
            &mut evm_account_storage,
            &ticket,
            &system,
            U256::from(2),
            U256::from(4),
        )
        .new_address()
        .expect("Failed to deploy reentrancy tester");

        ticket_balance_add(
            &mut mock_runtime,
            &mut evm_account_storage,
            &ticket_hash(&ticket).unwrap(),
            &proxy,
            U256::from(100),
        );

        let withdrawal = dummy_fa_withdrawal(ticket, sender, proxy);

        let mut receiver = Vec::new();
        withdrawal.receiver.bin_write(&mut receiver).unwrap();

        let mut proxy = Vec::new();
        withdrawal.proxy.bin_write(&mut proxy).unwrap();

        let mut ticketer = Vec::new();
        withdrawal
            .ticket
            .creator()
            .0
            .bin_write(&mut ticketer)
            .unwrap();

        let mut contents = Vec::new();
        withdrawal
            .ticket
            .contents()
            .bin_write(&mut contents)
            .unwrap();

        let input = kernel_wrapper::withdrawCall::new((
            convert_h160(&withdrawal.ticket_owner),
            [receiver, proxy].concat().into(),
            convert_u256(&withdrawal.amount),
            TryInto::<[u8; 22]>::try_into(ticketer).unwrap().into(),
            contents.into(),
        ));

        let outcome: ExecutionOutcome = execute_precompile(
            &mut mock_runtime,
            &mut evm_account_storage,
            sender,
            U256::zero(),
            input.abi_encode(),
            // Note that we set gas limit larger than hard cap for a single transaction:
            // that is to overcome the added cost per FA withdrawal which is pretty large
            // for the given gas price (up to 15M).
            Some(100_000_000),
            false,
            false,
        );

        /// Raw encoding of "Call to target contract failed" revert output from 'ReentrancyTester.sol'
        pub const CALL_TO_TARGET_CONTRACT_FAILED: [u8; 100] = [
            8, 195, 121, 160, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 67, 97, 108, 108,
            32, 116, 111, 32, 116, 97, 114, 103, 101, 116, 32, 99, 111, 110, 116, 114,
            97, 99, 116, 32, 102, 97, 105, 108, 101, 100, 0, 0,
        ];

        assert_eq!(
            outcome.result,
            ExecutionResult::CallReverted(CALL_TO_TARGET_CONTRACT_FAILED.to_vec())
        );

        let outcome = execute_precompile(
            &mut mock_runtime,
            &mut evm_account_storage,
            sender,
            U256::zero(),
            input.abi_encode(),
            Some(100_000_000),
            false,
            true,
        );
        assert!(outcome.is_success());
        assert!(!outcome.withdrawals.is_empty());
    }
}
