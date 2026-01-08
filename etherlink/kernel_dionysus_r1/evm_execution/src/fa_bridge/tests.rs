// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

use alloy_primitives::FixedBytes;
use alloy_sol_types::SolEvent;
use evm::ExitError;
use primitive_types::{H160, U256};
use tezos_evm_runtime::runtime::MockKernelHost;

pub const FA_DEPOSIT_PROXY_GAS_LIMIT: u64 = 1_000_000;

use crate::{
    account_storage::{account_path, init_account_storage},
    fa_bridge::{
        deposit::ticket_hash,
        test_utils::{
            convert_h160, convert_log, convert_u256, deploy_mock_wrapper,
            deploy_reentrancy_tester, dummy_fa_deposit, dummy_fa_withdrawal,
            dummy_ticket, fa_bridge_precompile_call_withdraw, get_storage_flag,
            kernel_wrapper, run_fa_deposit, ticket_balance_add, ticket_balance_get,
            token_wrapper, withdrawal_counter_next,
        },
    },
    handler::ExecutionResult,
};

#[test]
fn fa_deposit_reached_wrapper_contract() {
    let mut mock_runtime = MockKernelHost::default();
    let mut evm_account_storage = init_account_storage().unwrap();

    let caller = H160::zero();
    let ticket = dummy_ticket();

    let proxy = deploy_mock_wrapper(
        &mut mock_runtime,
        &mut evm_account_storage,
        &ticket,
        &caller,
        0,
    )
    .new_address()
    .unwrap();

    let deposit = dummy_fa_deposit(ticket, Some(proxy));
    let res = run_fa_deposit(
        &mut mock_runtime,
        &mut evm_account_storage,
        &deposit,
        &caller,
        FA_DEPOSIT_PROXY_GAS_LIMIT,
        false,
    );
    assert!(res.is_success());
    assert_eq!(2, res.logs.len());

    let flag = get_storage_flag(&mock_runtime, &evm_account_storage, proxy);
    assert_eq!(deposit.amount.as_u32(), flag);

    assert_eq!(
        ticket_balance_get(
            &mock_runtime,
            &evm_account_storage,
            &deposit.ticket_hash,
            &deposit.receiver
        ),
        U256::zero()
    );
    assert_eq!(
        ticket_balance_get(
            &mock_runtime,
            &evm_account_storage,
            &deposit.ticket_hash,
            &proxy
        ),
        deposit.amount
    );

    let mint_event =
        token_wrapper::Mint::decode_log_data(&convert_log(&res.logs[0]), true)
            .expect("Failed to parse Mint event");
    let deposit_event =
        kernel_wrapper::Deposit::decode_log_data(&convert_log(&res.logs[1]), true)
            .expect("Failed to parse Deposit event");

    assert_eq!(mint_event.amount, convert_u256(&deposit.amount));
    assert_eq!(mint_event.receiver, convert_h160(&deposit.receiver));

    assert_eq!(
        deposit_event.ticketHash,
        convert_u256(&U256::from(deposit.ticket_hash.as_bytes()))
    );
    assert_eq!(
        deposit_event.ticketOwner,
        convert_h160(&deposit.proxy.unwrap())
    ); // ticket owner is now wrapper contract
    assert_eq!(deposit_event.receiver, convert_h160(&deposit.receiver));
    assert_eq!(deposit_event.amount, convert_u256(&deposit.amount));
    assert_eq!(
        deposit_event.inboxLevel,
        convert_u256(&U256::from(deposit.inbox_level))
    );
    assert_eq!(
        deposit_event.inboxMsgId,
        convert_u256(&U256::from(deposit.inbox_msg_id))
    );
}

#[test]
fn fa_deposit_refused_due_non_existing_contract() {
    let mut mock_runtime = MockKernelHost::default();
    let mut evm_account_storage = init_account_storage().unwrap();

    let caller = H160::zero();
    let ticket = dummy_ticket();
    let deposit = dummy_fa_deposit(ticket, Some(H160([1u8; 20])));

    let res = run_fa_deposit(
        &mut mock_runtime,
        &mut evm_account_storage,
        &deposit,
        &caller,
        FA_DEPOSIT_PROXY_GAS_LIMIT,
        false,
    );
    assert_eq!(1, res.logs.len());

    assert_eq!(
        ticket_balance_get(
            &mock_runtime,
            &evm_account_storage,
            &deposit.ticket_hash,
            &deposit.receiver
        ),
        deposit.amount
    );
    assert_eq!(
        ticket_balance_get(
            &mock_runtime,
            &evm_account_storage,
            &deposit.ticket_hash,
            &deposit.proxy.unwrap()
        ),
        U256::zero()
    );

    let deposit_event =
        kernel_wrapper::Deposit::decode_log_data(&convert_log(&res.logs[0]), true)
            .expect("Failed to parse Deposit event");

    assert_eq!(
        deposit_event.ticketHash,
        convert_u256(&U256::from(deposit.ticket_hash.as_bytes()))
    );
    assert_eq!(deposit_event.ticketOwner, convert_h160(&deposit.receiver)); // ticket owner is deposit receiver
    assert_eq!(deposit_event.receiver, convert_h160(&deposit.receiver));
    assert_eq!(deposit_event.amount, convert_u256(&deposit.amount));
    assert_eq!(
        deposit_event.inboxLevel,
        convert_u256(&U256::from(deposit.inbox_level))
    );
    assert_eq!(
        deposit_event.inboxMsgId,
        convert_u256(&U256::from(deposit.inbox_msg_id))
    );
}

#[test]
fn fa_deposit_refused_non_compatible_interface() {
    let mut mock_runtime = MockKernelHost::default();
    let mut evm_account_storage = init_account_storage().unwrap();

    let caller = H160::zero();
    let proxy = H160([1u8; 20]);
    let ticket = dummy_ticket();
    let deposit = dummy_fa_deposit(ticket, Some(proxy));

    // Making it look as a smart contract
    let mut account = evm_account_storage
        .get_or_create(&mock_runtime, &account_path(&proxy).unwrap())
        .unwrap();
    account.set_code(&mut mock_runtime, &[255u8; 1024]).unwrap();

    let res = run_fa_deposit(
        &mut mock_runtime,
        &mut evm_account_storage,
        &deposit,
        &caller,
        FA_DEPOSIT_PROXY_GAS_LIMIT,
        false,
    );
    assert_eq!(1, res.logs.len());

    assert_eq!(
        ticket_balance_get(
            &mock_runtime,
            &evm_account_storage,
            &deposit.ticket_hash,
            &deposit.receiver
        ),
        deposit.amount
    );
    assert_eq!(
        ticket_balance_get(
            &mock_runtime,
            &evm_account_storage,
            &deposit.ticket_hash,
            &deposit.proxy.unwrap()
        ),
        U256::zero()
    );

    let deposit_event =
        kernel_wrapper::Deposit::decode_log_data(&convert_log(&res.logs[0]), true)
            .expect("Failed to parse Deposit event");

    assert_eq!(
        deposit_event.ticketHash,
        convert_u256(&U256::from(deposit.ticket_hash.as_bytes()))
    );
    assert_eq!(deposit_event.ticketOwner, convert_h160(&deposit.receiver)); // ticket owner is deposit receiver
    assert_eq!(deposit_event.receiver, convert_h160(&deposit.receiver));
    assert_eq!(deposit_event.amount, convert_u256(&deposit.amount));
    assert_eq!(
        deposit_event.inboxLevel,
        convert_u256(&U256::from(deposit.inbox_level))
    );
    assert_eq!(
        deposit_event.inboxMsgId,
        convert_u256(&U256::from(deposit.inbox_msg_id))
    );
}

#[test]
fn fa_deposit_proxy_state_reverted_if_ticket_balance_overflows() {
    let mut mock_runtime = MockKernelHost::default();
    let mut evm_account_storage = init_account_storage().unwrap();

    let caller = H160::zero();
    let ticket = dummy_ticket();

    let proxy = deploy_mock_wrapper(
        &mut mock_runtime,
        &mut evm_account_storage,
        &ticket,
        &caller,
        100500,
    )
    .new_address()
    .unwrap();

    let deposit = dummy_fa_deposit(ticket, Some(proxy));

    // Patch ticket table
    ticket_balance_add(
        &mut mock_runtime,
        &mut evm_account_storage,
        &deposit.ticket_hash,
        &proxy,
        U256::MAX,
    );

    let res = run_fa_deposit(
        &mut mock_runtime,
        &mut evm_account_storage,
        &deposit,
        &caller,
        FA_DEPOSIT_PROXY_GAS_LIMIT,
        false,
    );
    assert!(!res.is_success());
    assert!(res.logs.is_empty());

    let flag = get_storage_flag(&mock_runtime, &evm_account_storage, proxy);
    assert_eq!(100500, flag);

    assert_eq!(
        ticket_balance_get(
            &mock_runtime,
            &evm_account_storage,
            &deposit.ticket_hash,
            &deposit.receiver
        ),
        U256::zero()
    );
    assert_eq!(
        ticket_balance_get(
            &mock_runtime,
            &evm_account_storage,
            &deposit.ticket_hash,
            &deposit.proxy.unwrap()
        ),
        U256::MAX
    );
}

#[test]
fn fa_withdrawal_executed_via_l2_proxy_contract() {
    let mut mock_runtime = MockKernelHost::default();
    let mut evm_account_storage = init_account_storage().unwrap();

    let sender = H160::from_low_u64_be(1);
    let caller = H160::zero();
    let ticket = dummy_ticket();

    let proxy = deploy_mock_wrapper(
        &mut mock_runtime,
        &mut evm_account_storage,
        &ticket,
        &caller,
        0,
    )
    .new_address()
    .unwrap();

    let withdrawal = dummy_fa_withdrawal(ticket, sender, proxy);

    // Patch ticket table
    ticket_balance_add(
        &mut mock_runtime,
        &mut evm_account_storage,
        &withdrawal.ticket_hash,
        &proxy,
        withdrawal.amount,
    );

    let res = fa_bridge_precompile_call_withdraw(
        &mut mock_runtime,
        &mut evm_account_storage,
        withdrawal,
        caller,
    );
    assert!(res.is_success());
    assert!(!res.withdrawals.is_empty());
    assert_eq!(2, res.logs.len());

    // Re-create withdrawal struct
    let withdrawal = dummy_fa_withdrawal(dummy_ticket(), sender, proxy);

    // Ensure proxy contract state changed
    let flag = get_storage_flag(&mock_runtime, &evm_account_storage, proxy);
    assert_eq!(withdrawal.amount.as_u32(), flag);

    // Ensure ticket balance reduced to zero (if not then will overflow)
    assert!(ticket_balance_add(
        &mut mock_runtime,
        &mut evm_account_storage,
        &withdrawal.ticket_hash,
        &withdrawal.ticket_owner,
        U256::MAX,
    ));

    // Ensure events are emitted correctly
    let withdrawal_event =
        kernel_wrapper::Withdrawal::decode_log_data(&convert_log(&res.logs[0]), true)
            .expect("Failed to parse Withdrawal event");

    let burn_event =
        token_wrapper::Burn::decode_log_data(&convert_log(&res.logs[1]), true)
            .expect("Failed to parse Burn event");

    assert_eq!(
        withdrawal_event.ticketHash,
        alloy_primitives::U256::from_be_bytes(withdrawal.ticket_hash.0)
    );
    assert_eq!(withdrawal_event.sender, convert_h160(&sender));
    assert_eq!(
        withdrawal_event.ticketOwner,
        convert_h160(&withdrawal.ticket_owner)
    );
    assert_eq!(withdrawal_event.receiver, FixedBytes::new([0u8; 22]));
    assert_eq!(withdrawal_event.amount, convert_u256(&withdrawal.amount));
    assert_eq!(withdrawal_event.withdrawalId, convert_u256(&U256::from(0)));

    assert_eq!(burn_event.sender, convert_h160(&withdrawal.sender));
    assert_eq!(burn_event.amount, convert_u256(&withdrawal.amount));

    // Ensure withdrawal counter is incremented
    assert_eq!(
        Some(U256::one()),
        withdrawal_counter_next(&mock_runtime, &evm_account_storage)
    );
}

#[test]
fn fa_withdrawal_fails_due_to_faulty_l2_proxy() {
    let mut mock_runtime = MockKernelHost::default();
    let mut evm_account_storage = init_account_storage().unwrap();

    let sender = H160::from_low_u64_be(1);
    let caller = H160::zero();
    let ticket = dummy_ticket();
    let proxy = H160::from_low_u64_be(2); // non-existing contract

    let withdrawal = dummy_fa_withdrawal(ticket, sender, proxy);

    // Patch ticket table
    ticket_balance_add(
        &mut mock_runtime,
        &mut evm_account_storage,
        &withdrawal.ticket_hash,
        &proxy,
        withdrawal.amount,
    );

    let res = fa_bridge_precompile_call_withdraw(
        &mut mock_runtime,
        &mut evm_account_storage,
        withdrawal,
        caller,
    );
    assert!(!res.is_success());
    assert!(res.withdrawals.is_empty());
    assert!(res.logs.is_empty());

    // Re-create withdrawal struct
    let withdrawal = dummy_fa_withdrawal(dummy_ticket(), sender, proxy);

    // Ensure ticket balance is non-zero (should overflow)
    assert!(!ticket_balance_add(
        &mut mock_runtime,
        &mut evm_account_storage,
        &withdrawal.ticket_hash,
        &withdrawal.ticket_owner,
        U256::MAX,
    ));

    // Ensure withdrawal counter is reverted
    assert_eq!(
        None,
        withdrawal_counter_next(&mock_runtime, &evm_account_storage)
    );
    assert!(
        matches!(res.result, ExecutionResult::Error(ExitError::Other(err)) if err.contains("Proxy contract does not exist"))
    );
}

#[test]
fn fa_withdrawal_fails_due_to_insufficient_balance() {
    let mut mock_runtime = MockKernelHost::default();
    let mut evm_account_storage = init_account_storage().unwrap();

    let sender = H160::from_low_u64_be(1);
    let caller = H160::zero();
    let ticket = dummy_ticket();
    let proxy = H160::from_low_u64_be(2); // non-existing contract

    let withdrawal = dummy_fa_withdrawal(ticket, sender, proxy);

    let res = fa_bridge_precompile_call_withdraw(
        &mut mock_runtime,
        &mut evm_account_storage,
        withdrawal,
        caller,
    );
    assert!(!res.is_success());
    assert!(res.withdrawals.is_empty());
    assert!(res.logs.is_empty());

    // Ensure withdrawal counter is not updated (returned before incrementing the nonce)
    assert_eq!(
        None,
        withdrawal_counter_next(&mock_runtime, &evm_account_storage)
    );
    assert!(
        matches!(res.result, ExecutionResult::Error(ExitError::Other(err)) if err.contains("Insufficient ticket balance"))
    );
}

#[test]
fn fa_deposit_cannot_call_fa_withdrawal_precompile() {
    let mut mock_runtime = MockKernelHost::default();
    let mut evm_account_storage = init_account_storage().unwrap();

    let caller = H160::zero();
    let ticket = dummy_ticket();

    let proxy = deploy_reentrancy_tester(
        &mut mock_runtime,
        &mut evm_account_storage,
        &ticket,
        &caller,
        U256::one(),
        U256::one(),
    )
    .new_address()
    .expect("Failed to deploy reentrancy tester");

    ticket_balance_add(
        &mut mock_runtime,
        &mut evm_account_storage,
        &ticket_hash(&ticket).unwrap(),
        &proxy,
        U256::one(),
    );

    let deposit = dummy_fa_deposit(ticket, Some(proxy));

    // First let's show that it's possible to withdraw from the inner call
    let res = run_fa_deposit(
        &mut mock_runtime,
        &mut evm_account_storage,
        &deposit,
        &caller,
        100_000_000,
        true,
    );
    assert!(res.is_success());
    assert!(!res.withdrawals.is_empty());

    // Now let's do the same but without enabling the withdrawal precompile
    let res = run_fa_deposit(
        &mut mock_runtime,
        &mut evm_account_storage,
        &deposit,
        &caller,
        100_000_000,
        false,
    );
    assert!(res.is_success());
    assert!(res.withdrawals.is_empty());
}
