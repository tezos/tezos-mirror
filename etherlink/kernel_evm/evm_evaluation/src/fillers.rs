// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::evalhost::EvalHost;
use crate::helpers::{parse_and_get_cmp, purify_network};
use crate::models::spec::SpecId;
use crate::models::{
    AccountInfoFiller, FillerResultIndexes, FillerSource, IndexKind, SpecName,
};
use crate::{write_host, ReportValue};

use evm_execution::account_storage::EthereumAccount;

use bytes::Bytes;
use primitive_types::{H160, H256, U256};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::str::FromStr;

fn check_filler_constraints(
    indexes: &FillerResultIndexes,
    tx_label: Option<&String>,
    tx_index: i64,
) -> bool {
    // A transaction matches the constraints if:
    // - there are no constraints
    // - OR, if it has a label, it matches one of the label of the test
    // - OR, its index is the range of those identified

    if indexes.data.is_empty() {
        return true;
    }

    for index_kind in indexes.to_owned().data.into_iter() {
        match index_kind {
            IndexKind::Label(label) => {
                if let Some(tx_label) = tx_label {
                    if tx_label.eq(&label) {
                        return true;
                    }
                }
            }
            IndexKind::Range(start, end) => {
                if start <= tx_index && tx_index <= end {
                    return true;
                }
            }
        }
    }

    // At this point, no constraint have been matched.
    false
}

fn check_if_network_match(filler_network: &str, spec_name: &SpecName) -> bool {
    let cmp_spec_id = parse_and_get_cmp(filler_network);
    let network = purify_network(filler_network);
    let check_network_id = SpecId::from(&network) as u8;
    let current_network_config_id = SpecId::from(&spec_name.to_str()) as u8;

    cmp_spec_id(&current_network_config_id, &check_network_id)
}

fn check_should_not_exist(
    host: &mut EvalHost,
    account: &EthereumAccount,
    invalid_state: &mut bool,
    shouldnotexist: &Option<String>,
    hex_address: &str,
) {
    if let Some(_shouldnotexist) = shouldnotexist {
        let balance_is_zero = if let Ok(balance) = account.balance(host) {
            balance == U256::zero()
        } else {
            false
        };

        let no_code = if let Ok(code_size) = account.code_size(host) {
            code_size == U256::zero()
        } else {
            false
        };

        if balance_is_zero && no_code {
            write_host!(host, "Account {} rightfully does not exist.", hex_address);
        } else {
            write_host!(host, "Account {} should not exist.", hex_address);
            *invalid_state = true;
        }
    }
}

fn check_balance(
    host: &mut EvalHost,
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
                    write_host!( host, "Account {}: balance don't match current one, {} was expected, but got {}.", hex_address, balance, current_balance);
                } else {
                    write_host!(host, "Account {}: balance matched.", hex_address);
                }
            }
            Err(_) => {
                *invalid_state = true;
                write_host!(host, "Account {} should have a balance.", hex_address);
            }
        }
    }
}

fn check_code(
    host: &mut EvalHost,
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
                    write_host!( host, "Account {}: code don't match current one, {:?} was expected, but got {:?}.", hex_address, code, current_code);
                } else {
                    write_host!(host, "Account {}: code matched.", hex_address);
                }
            }
            Err(_) => {
                *invalid_state = true;
                write_host!(host, "Account {} should have a code.", hex_address);
            }
        }
    }
}

fn check_nonce(
    host: &mut EvalHost,
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
                    write_host!( host, "Account {}: nonce don't match current one, {} was expected, but got {}.", hex_address, nonce, current_nonce);
                } else {
                    write_host!(host, "Account {}: nonce matched.", hex_address);
                }
            }
            Err(_) => {
                *invalid_state = true;
                write_host!(host, "Account {} should have a nonce.", hex_address);
            }
        }
    }
}

fn check_storage(
    host: &mut EvalHost,
    account: &EthereumAccount,
    invalid_state: &mut bool,
    storage: &Option<HashMap<H256, H256>>,
    hex_address: &str,
) {
    if let Some(storage) = &storage {
        if storage.is_empty() {
            write_host!(
                host,
                "Account {}: storage matched (both empty).",
                hex_address
            );
        }
        for (index, value) in storage.iter() {
            match account.get_storage(host, index) {
                Ok(current_storage_value) => {
                    let storage_value = value;
                    if current_storage_value != *storage_value {
                        *invalid_state = true;
                        write_host!( host, "Account {}: storage don't match current one for index {}, {} was expected, but got {}.", hex_address, index, storage_value, current_storage_value);
                    } else {
                        write_host!(
                            host,
                            "Account {}: storage matched for index {}.",
                            hex_address,
                            index
                        );
                    }
                }
                Err(_) => {
                    *invalid_state = true;
                    write_host!(host, "Account {} should have a storage.", hex_address);
                }
            }
        }
    }
}

fn check_durable_storage(
    host: &mut EvalHost,
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
            write_host!(host, "==> [INVALID STATE]\n");
        } else {
            write_host!(host, "==> [CORRECT STATE]\n");
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn process(
    host: &mut EvalHost,
    filler_source: FillerSource,
    spec_name: &SpecName,
    report_map: &mut HashMap<String, ReportValue>,
    report_key: String,
    output_file: &mut File,
    tx_label: Option<&String>,
    tx_index: i64,
    report_only: bool,
) {
    let mut good_state = true;

    for (name, fillers) in filler_source.0.into_iter() {
        write_host!(host, "Processing checks with filler: {}Filler\n", name);
        for filler_expectation in fillers.expect {
            for filler_network in filler_expectation.network {
                if check_filler_constraints(
                    &filler_expectation.indexes,
                    tx_label,
                    tx_index,
                ) && check_if_network_match(&filler_network, spec_name)
                {
                    write_host!(host, "CONFIG NETWORK ---- {}", spec_name.to_str());
                    write_host!(host, "CHECK  NETWORK ---- {}\n", filler_network);

                    check_durable_storage(
                        host,
                        &filler_expectation.result,
                        &mut good_state,
                    );
                }
            }
        }
    }

    if good_state {
        if !report_only {
            writeln!(output_file, "\nFINAL RESULT: SUCCESS\n").unwrap();
        }
        report_map.entry(report_key).and_modify(|report_value| {
            *report_value = ReportValue {
                successes: report_value.successes + 1,
                failures: report_value.failures,
            };
        });
    } else {
        if !report_only {
            write!(
                output_file,
                "{}",
                String::from_utf8(host.buffer.borrow_mut().to_vec()).unwrap()
            )
            .unwrap();
            writeln!(output_file, "FINAL RESULT: FAILURE\n").unwrap();
        }
        report_map.entry(report_key).and_modify(|report_value| {
            *report_value = ReportValue {
                successes: report_value.successes,
                failures: report_value.failures + 1,
            };
        });
    }
}
