// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::helpers::{
    network_to_specid, parse_and_get_cmp, purify_network, u256_to_h256,
};
use crate::models::{AccountInfoFiller, FillerSource, SpecName};

use evm_execution::account_storage::EthereumAccount;
use tezos_smart_rollup_host::runtime::Runtime;

use bytes::Bytes;
use primitive_types::{H160, H256, U256};
use std::collections::HashMap;
use std::str::FromStr;

fn check_should_not_exist<Host: Runtime>(
    host: &mut Host,
    account: &EthereumAccount,
    invalid_state: &mut bool,
    shouldnotexist: &Option<String>,
    hex_address: &str,
) {
    if let Some(_shouldnotexist) = shouldnotexist {
        if account.balance(host).is_ok() {
            println!("Account {} should not exist.", hex_address);
            *invalid_state = true;
        } else {
            println!("Account {} rightfully do not exist.", hex_address)
        }
    }
}

fn check_balance<Host: Runtime>(
    host: &mut Host,
    account: &EthereumAccount,
    invalid_state: &mut bool,
    balance: &Option<U256>,
    hex_address: &str,
) {
    if let Some(balance) = balance {
        match account.balance(host) {
            Ok(current_balance) => {
                if current_balance != *balance {
                    *invalid_state = true;
                    println!("Account {}: balance don't match current one, {} was expected, but got {}.", hex_address, balance, current_balance)
                } else {
                    println!("Account {}: balance matched.", hex_address)
                }
            }
            Err(_) => {
                *invalid_state = true;
                println!("Account {} should have a balance.", hex_address);
            }
        }
    }
}

fn check_code<Host: Runtime>(
    host: &mut Host,
    account: &EthereumAccount,
    invalid_state: &mut bool,
    code: &Option<Bytes>,
    hex_address: &str,
) {
    if let Some(code) = &code {
        match account.code(host) {
            Ok(current_code) => {
                let current_code: Bytes = current_code.into();
                if current_code != code {
                    *invalid_state = true;
                    println!("Account {}: code don't match current one, {:?} was expected, but got {:?}.", hex_address, code, current_code)
                } else {
                    println!("Account {}: code matched.", hex_address)
                }
            }
            Err(_) => {
                *invalid_state = true;
                println!("Account {} should have a code.", hex_address);
            }
        }
    }
}

fn check_nonce<Host: Runtime>(
    host: &mut Host,
    account: &EthereumAccount,
    invalid_state: &mut bool,
    nonce: &Option<u64>,
    hex_address: &str,
) {
    if let Some(nonce) = nonce {
        match account.nonce(host) {
            Ok(current_nonce) => {
                if current_nonce != (*nonce).into() {
                    *invalid_state = true;
                    println!("Account {}: nonce don't match current one, {} was expected, but got {}.", hex_address, nonce, current_nonce)
                } else {
                    println!("Account {}: nonce matched.", hex_address)
                }
            }
            Err(_) => {
                *invalid_state = true;
                println!("Account {} should have a nonce.", hex_address);
            }
        }
    }
}

fn check_storage<Host: Runtime>(
    host: &mut Host,
    account: &EthereumAccount,
    invalid_state: &mut bool,
    storage: &Option<HashMap<H256, H256>>,
    hex_address: &str,
) {
    if let Some(storage) = &storage {
        if storage.is_empty() {
            println!("Account {}: storage matched (both empty).", hex_address)
        }
        for (index, value) in storage.iter() {
            match account.get_storage(host, &u256_to_h256(index)) {
                Ok(current_storage_value) => {
                    let storage_value = u256_to_h256(value);
                    if current_storage_value != storage_value {
                        *invalid_state = true;
                        println!("Account {}: storage don't match current one, {} was expected, but got {}.", hex_address, storage_value, current_storage_value)
                    } else {
                        println!("Account {}: storage matched.", hex_address)
                    }
                }
                Err(_) => {
                    *invalid_state = true;
                    println!("Account {} should have a storage.", hex_address);
                }
            }
        }
    }
}

fn check_durable_storage<Host: Runtime>(
    host: &mut Host,
    filler_expectation_result: &HashMap<String, AccountInfoFiller>,
    good_state: &mut bool,
) {
    for (account, info) in filler_expectation_result.iter() {
        let hex_address = if account.contains("0x") {
            account.to_owned()
        } else {
            "0x".to_owned() + account
        };
        let address = H160::from_str(&hex_address).expect("Expect valid hex digit(s).");
        let account = EthereumAccount::from_address(&address).unwrap();
        let mut invalid_state = false;

        // Enable checks when fields are available in the source filler file.

        check_should_not_exist(
            host,
            &account,
            &mut invalid_state,
            &info.shouldnotexist,
            &hex_address,
        );

        check_balance(
            host,
            &account,
            &mut invalid_state,
            &info.balance,
            &hex_address,
        );

        check_code(host, &account, &mut invalid_state, &info.code, &hex_address);

        check_nonce(
            host,
            &account,
            &mut invalid_state,
            &info.nonce,
            &hex_address,
        );

        check_storage(
            host,
            &account,
            &mut invalid_state,
            &info.storage,
            &hex_address,
        );

        if invalid_state {
            // One invalid state will cause the entire test to be a failure.
            *good_state = false;
            println!("==> [INVALID STATE]\n")
        } else {
            println!("==> [CORRECT STATE]\n")
        }
    }
}

pub fn process<Host: Runtime>(
    host: &mut Host,
    filler_source: FillerSource,
    spec_name: &SpecName,
) -> bool {
    let mut good_state = true;

    for (name, fillers) in filler_source.0.into_iter() {
        println!("\nProcessing checks with filler: {}Filler\n", name);
        for filler_expectation in fillers.expect {
            for filler_network in filler_expectation.network {
                let cmp_spec_id = parse_and_get_cmp(&filler_network);
                let network = purify_network(&filler_network);
                let check_network_id = network_to_specid(&network) as u8;
                let current_network_config_id =
                    network_to_specid(&spec_name.to_str()) as u8;

                if !cmp_spec_id(&current_network_config_id, &check_network_id) {
                    continue;
                }

                println!("CONFIG NETWORK ---- {}", spec_name.to_str());
                println!("CHECK  NETWORK ---- {}\n", filler_network);

                check_durable_storage(host, &filler_expectation.result, &mut good_state);
            }
        }
    }

    if good_state {
        println!("TX INTERPRETATION: GOOD STATE")
    } else {
        println!("TX INTERPRETATION: BAD STATE")
    }

    good_state
}
