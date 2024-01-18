// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::evalhost::EvalHost;
use crate::helpers::{parse_and_get_cmp, purify_network, LabelIndexes, OutputOptions};
use crate::models::spec::SpecId;
use crate::models::{
    AccountInfoFiller, FillerResultIndexes, FillerSource, IndexKind, SpecName,
};
use crate::{write_host, DiffMap, ReportMap, ReportValue};

use crate::models::TxPartIndices;
use evm_execution::account_storage::EthereumAccount;

use bytes::Bytes;
use primitive_types::{H160, H256, U256};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::str::FromStr;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TestResult {
    Skipped,
    Success,
    Failure,
}

fn check_kind(
    all_kind: &[IndexKind],
    kind_index: &i64,
    kind_label: Option<&String>,
) -> bool {
    if all_kind.is_empty() {
        return true;
    }

    for index_kind in all_kind.iter() {
        match index_kind {
            IndexKind::Label(label) => {
                if let Some(kind_label) = kind_label {
                    if kind_label.eq(label) {
                        // If the label that matches the test is the right, we can return.
                        return true;
                    }
                }
            }
            IndexKind::Range(start, end) => {
                if start <= kind_index && kind_index <= end {
                    return true;
                }
            }
        }
    }
    // At this point, no constraint have been matched.
    false
}

fn check_filler_constraints(
    indexes: &FillerResultIndexes,
    tx_label: &LabelIndexes,
    tx_index: &TxPartIndices,
) -> bool {
    // A transaction matches the constraints if:
    //       its index.data is in the range or has the right label or has no constraints
    //   AND its index.gas is in the range or has the right label or has no constraints
    //   AND its index.value is in the range or has the right label or has no constraints
    // Check if there is constraint on data
    check_kind(&indexes.data, &(tx_index.data as i64), tx_label.data_label) &&
    // Check if there is constraint on gas
    check_kind(&indexes.gas, &(tx_index.gas as i64), tx_label.gas_label) &&
    // Check if there is constraint on value
    check_kind(&indexes.value, &(tx_index.value as i64), tx_label.value_label)
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
    status: &mut TestResult,
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
            *status = TestResult::Failure;
            // One invalid state will cause the entire test to be a failure.
            write_host!(host, "==> [INVALID STATE]\n");
        } else {
            write_host!(host, "==> [CORRECT STATE]\n");
        }
    }
}

pub fn parse_result(line: &str) -> Option<(String, TestResult)> {
    match line.find(':') {
        None => None,
        Some(index) => {
            let name = line[..index].to_string();
            match &line[index + 2..] {
                "Skipped" => Some((name, TestResult::Skipped)),
                "Failure" => Some((name, TestResult::Failure)),
                "Success" => Some((name, TestResult::Success)),
                _ => None,
            }
        }
    }
}

pub fn output_result(file: &mut File, name: &str, status: TestResult) {
    // If the format of the following line is modified `parse_result` above
    // should be readapted accordingly to avoid any inconsistency.
    writeln!(file, "{}: {:?}", name, status).unwrap()
}

#[allow(clippy::too_many_arguments)]
pub fn process(
    host: &mut EvalHost,
    filler_source: FillerSource,
    spec_name: &SpecName,
    report_map: &mut ReportMap,
    report_key: String,
    output_file: &mut File,
    tx_label: LabelIndexes,
    tx_indices: &TxPartIndices,
    output: &OutputOptions,
    test_name: &str,
    diff_result_map: &mut DiffMap,
) {
    let mut status = TestResult::Success;

    for (name, fillers) in filler_source.0.into_iter() {
        write_host!(
            host,
            "Processing checks with filler: {}Filler and config: {}\n",
            name,
            tx_indices
        );
        for filler_expectation in fillers.expect {
            for filler_network in filler_expectation.network {
                if check_filler_constraints(
                    &filler_expectation.indexes,
                    &tx_label,
                    tx_indices,
                ) && check_if_network_match(&filler_network, spec_name)
                {
                    write_host!(host, "CONFIG NETWORK ---- {}", spec_name.to_str());
                    write_host!(host, "CHECK  NETWORK ---- {}\n", filler_network);

                    check_durable_storage(host, &filler_expectation.result, &mut status);
                }
            }
        }
    }

    let full_name = format!(
        "{}_{}_data_index_{}_gas_index_{}_value_index_{}",
        &report_key, test_name, tx_indices.data, tx_indices.gas, tx_indices.value
    );

    match status {
        TestResult::Success => {
            report_map.entry(report_key).and_modify(|report_value| {
                *report_value = ReportValue {
                    successes: report_value.successes + 1,
                    ..*report_value
                };
            });
        }
        TestResult::Failure => {
            if output.log {
                write!(
                    output_file,
                    "{}",
                    String::from_utf8(host.buffer.borrow_mut().to_vec()).unwrap()
                )
                .unwrap();
            }
            report_map.entry(report_key).and_modify(|report_value| {
                *report_value = ReportValue {
                    failures: report_value.failures + 1,
                    ..*report_value
                };
            });
        }
        _ => (),
    }
    if output.log {
        writeln!(output_file, "\nFINAL RESULT: {:?}\n", status).unwrap();
    }
    if output.result {
        output_result(output_file, &full_name, status);
    }
    if let Some(map) = diff_result_map {
        if let Some((result, _)) = map.get(&full_name) {
            map.insert(full_name, (*result, Some(status)));
        }
    }
}
