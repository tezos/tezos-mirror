// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::SolEvent;
use primitive_types::{H160, U256};
use tezos_smart_rollup_mock::MockHost;

use crate::{
    account_storage::{account_path, init_account_storage},
    fa_bridge::test_utils::{
        convert_h160, convert_log, convert_u256, deploy_mock_wrapper, dummy_fa_deposit,
        dummy_ticket, get_storage_flag, kernel_wrapper, run_fa_deposit,
        ticket_balance_add, ticket_balance_get, token_wrapper,
    },
};

#[test]
fn fa_deposit_reached_wrapper_contract() {
    let mut mock_runtime = MockHost::default();
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
    .new_address
    .unwrap();

    let deposit = dummy_fa_deposit(ticket, Some(proxy));
    let res = run_fa_deposit(
        &mut mock_runtime,
        &mut evm_account_storage,
        &deposit,
        &caller,
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
    let mut mock_runtime = MockHost::default();
    let mut evm_account_storage = init_account_storage().unwrap();

    let caller = H160::zero();
    let ticket = dummy_ticket();
    let deposit = dummy_fa_deposit(ticket, Some(H160([1u8; 20])));

    let res = run_fa_deposit(
        &mut mock_runtime,
        &mut evm_account_storage,
        &deposit,
        &caller,
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
    let mut mock_runtime = MockHost::default();
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
    let mut mock_runtime = MockHost::default();
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
    .new_address
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
