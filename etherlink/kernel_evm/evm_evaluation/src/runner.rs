// SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2021-2023 draganrakita
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use bytes::Bytes;
use evm_execution::account_storage::{
    init_account_storage, EthereumAccount, EthereumAccountStorage,
};
use evm_execution::handler::ExecutionOutcome;
use evm_execution::precompiles::{precompile_set, PrecompileBTreeMap};
use evm_execution::{run_transaction, Config, EthereumError};

use tezos_ethereum::block::{BlockConstants, BlockFees};

use hex_literal::hex;
use primitive_types::{H160, H256, U256};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use thiserror::Error;

use crate::evalhost::EvalHost;
use crate::fillers::{output_result, process, TestResult};
use crate::helpers::{
    construct_folder_path, string_of_hexa, LabelIndexes, OutputOptions,
};
use crate::models::{Env, FillerSource, SkipData, SpecName, Test, TestSuite, TestUnit};
use crate::{write_host, write_out, DiffMap, Opt, ReportMap};

const MAP_CALLER_KEYS: [(H256, H160); 6] = [
    (
        H256(hex!(
            "45a915e4d060149eb4365960e6a7a45f334393093061116b197e3240065ff2d8"
        )),
        H160(hex!("a94f5374fce5edbc8e2a8697c15331677e6ebf0b")),
    ),
    (
        H256(hex!(
            "c85ef7d79691fe79573b1a7064c19c1a9819ebdbd1faaab1a8ec92344438aaf4"
        )),
        H160(hex!("cd2a3d9f938e13cd947ec05abc7fe734df8dd826")),
    ),
    (
        H256(hex!(
            "044852b2a670ade5407e78fb2863c51de9fcb96542a07186fe3aeda6bb8a116d"
        )),
        H160(hex!("82a978b3f5962a5b0957d9ee9eef472ee55b42f1")),
    ),
    (
        H256(hex!(
            "6a7eeac5f12b409d42028f66b0b2132535ee158cfda439e3bfdd4558e8f4bf6c"
        )),
        H160(hex!("c9c5a15a403e41498b6f69f6f89dd9f5892d21f7")),
    ),
    (
        H256(hex!(
            "a95defe70ebea7804f9c3be42d20d24375e2a92b9d9666b832069c5f3cd423dd"
        )),
        H160(hex!("3fb1cd2cd96c6d5c0b5eb3322d807b34482481d4")),
    ),
    (
        H256(hex!(
            "fe13266ff57000135fb9aa854bbfe455d8da85b21f626307bf3263a0c2a8e7fe"
        )),
        H160(hex!("dcc5ba93a1ed7e045690d722f2bf460a51c61415")),
    ),
];

#[derive(Debug, Error)]
pub enum TestError {
    #[error("Serde json error")]
    SerdeDeserializeJSON(#[from] serde_json::Error),
    #[error("Serde yaml error")]
    SerdeDeserializeYAML(#[from] serde_yaml::Error),
    #[error("Unknown private key: {private_key:?}")]
    UnknownPrivateKey { private_key: H256 },
}

fn read_testsuite(path: &Path) -> Result<TestSuite, TestError> {
    let json_reader = std::fs::read(path).unwrap();
    serde_json::from_reader(&*json_reader).map_err(TestError::from)
}

fn prepare_host() -> EvalHost {
    let execution_buffer = Vec::new();
    let buffer = RefCell::new(execution_buffer);
    EvalHost::default_with_buffer(buffer)
}

fn prepare_host_with_buffer(execution_buffer: Vec<u8>) -> EvalHost {
    let buffer = RefCell::new(execution_buffer);
    EvalHost::default_with_buffer(buffer)
}

fn prepare_filler_source(
    host: &EvalHost,
    unit: &TestUnit,
    opt: &Opt,
) -> Result<Option<FillerSource>, TestError> {
    let full_filler_path =
        construct_folder_path(&unit._info.source, &opt.eth_tests, &None);
    write_host!(
        host,
        "Filler source: {}",
        &full_filler_path.to_str().unwrap()
    );
    let filler_path = Path::new(&full_filler_path);
    let reader = std::fs::read(filler_path).unwrap();
    if unit._info.source.contains(".json") {
        let filler_source: FillerSource = serde_json::from_reader(&*reader)?;
        Ok(Some(filler_source))
    } else if unit._info.source.contains(".yml") {
        let filler_source: FillerSource = serde_yaml::from_reader(&*reader)?;
        Ok(Some(filler_source))
    } else {
        // Test will be ignored, interpretation of results will not
        // be possible.
        Ok(None)
    }
}

fn initialize_accounts(host: &mut EvalHost, unit: &TestUnit) {
    write_host!(host, "\n[START] Accounts initialisation");

    for (address, info) in unit.pre.to_owned().iter() {
        let h160_address: H160 = address.as_fixed_bytes().into();
        write_host!(host, "\nAccount is {}", h160_address);
        let mut account =
            EthereumAccount::from_address(&address.as_fixed_bytes().into()).unwrap();
        if info.nonce != 0 {
            account.set_nonce(host, info.nonce).unwrap();
            write_host!(host, "Nonce is set for {} : {}", address, info.nonce);
        }
        account.balance_add(host, info.balance).unwrap();
        write_host!(host, "Balance for {} was added : {}", address, info.balance);
        if !info.code.is_empty() {
            account.set_code(host, &info.code).unwrap();
        }
        write_host!(host, "Code was set for {}", address);
        for (index, value) in info.storage.iter() {
            account.set_storage(host, index, value).unwrap();
        }
    }

    write_host!(host, "\n[END] Accounts initialisation\n");
}

fn initialize_env(unit: &TestUnit) -> Result<Env, TestError> {
    let map_caller_keys: HashMap<H256, H160> = MAP_CALLER_KEYS.into();

    let mut env = Env::default();

    // BlockEnv
    env.block.number = unit.env.current_number;
    env.block.coinbase = unit.env.current_coinbase;
    env.block.timestamp = unit.env.current_timestamp;
    env.block.gas_limit = unit.env.current_gas_limit;
    env.block.basefee = unit.env.current_base_fee.unwrap_or_default();
    env.block.prevrandao = unit.env.current_random;

    // TxEnv
    env.tx.caller = if let Some(caller) =
        map_caller_keys.get(&unit.transaction.secret_key.unwrap())
    {
        *caller
    } else {
        let private_key = unit.transaction.secret_key.unwrap();
        return Err(TestError::UnknownPrivateKey { private_key });
    };
    env.tx.gas_price = unit.transaction.gas_price.unwrap_or(
        env.block.basefee
            + unit
                .transaction
                .max_priority_fee_per_gas
                .unwrap_or_default(),
    );
    Ok(env)
}

#[allow(clippy::too_many_arguments)]
fn execute_transaction(
    host: &mut EvalHost,
    evm_account_storage: &mut EthereumAccountStorage,
    precompiles: &PrecompileBTreeMap<EvalHost>,
    config: &Config,
    unit: &TestUnit,
    env: &mut Env,
    test: &Test,
    data: Bytes,
) -> Result<Option<ExecutionOutcome>, EthereumError> {
    let gas_limit = *unit.transaction.gas_limit.get(test.indexes.gas).unwrap();
    let gas_limit = u64::try_from(gas_limit).unwrap_or(u64::MAX);
    env.tx.gas_limit = gas_limit;
    env.tx.data = data;
    env.tx.value = *unit.transaction.value.get(test.indexes.value).unwrap();
    env.tx.transact_to = unit.transaction.to;

    let block_fees = BlockFees::new(env.block.basefee, env.block.basefee, U256::zero());

    let block_constants = BlockConstants {
        number: env.block.number,
        coinbase: env.block.coinbase.to_fixed_bytes().into(),
        timestamp: env.block.timestamp,
        gas_limit: env.block.gas_limit.as_u64(),
        block_fees,
        chain_id: U256::from(1337),
        prevrandao: env.block.prevrandao,
    };
    let address = env.tx.transact_to.map(|addr| addr.to_fixed_bytes().into());
    let caller = env.tx.caller.to_fixed_bytes().into();
    let call_data = env.tx.data.to_vec();
    let gas_limit = env.tx.gas_limit;
    let transaction_value = env.tx.value;
    let pay_for_gas = true; // always, for now

    write_host!(
        host,
        "Executing transaction with:\n\
                    \t- data: {}\n\
                    \t- gas: {} gas\n\
                    \t- value: {} wei",
        string_of_hexa(&env.tx.data),
        gas_limit,
        env.tx.value
    );
    run_transaction(
        host,
        &block_constants,
        evm_account_storage,
        precompiles,
        config.clone(),
        address,
        caller,
        call_data,
        Some(gas_limit),
        env.tx.gas_price,
        transaction_value,
        pay_for_gas,
        u64::MAX, // don't account for ticks during the test
        false,
        true,
        None,
    )
}

fn data_to_skip(data: &[u8], skip_data: &SkipData) -> bool {
    for skip_data in skip_data.datas.iter() {
        if data == skip_data {
            return true;
        }
    }
    false
}

fn check_results(
    host: &EvalHost,
    name: &str,
    test: &Test,
    exec_result: &Result<Option<ExecutionOutcome>, EthereumError>,
) {
    match exec_result {
        Ok(execution_outcome_opt) => {
            let outcome_status = match execution_outcome_opt {
                Some(execution_outcome) => {
                    if execution_outcome.is_success() {
                        "[SUCCESS]"
                    } else {
                        "[FAILURE]"
                    }
                }
                None => "[INVALID]",
            };
            write_host!(host, "\nOutcome status: {}", outcome_status);
        }
        Err(e) => write_host!(host, "\nA test failed due to {:?}", e),
    }

    write_host!(host, "\nFinal check: ");
    match (test.expect_exception.clone(), exec_result) {
        (None, Ok(_)) => {
            write_host!(host, "No unexpected exception.")
        }
        (Some(_), Err(_)) => {
            write_host!(host, "Exception was expected.")
        }
        _ => {
            write_host!(
                host,
                "\nSomething unexpected happened for test {}.\n\
                 Expected exception is the following: {:?}\n\
                 Further details on the execution result: {:?}",
                name,
                &test.expect_exception,
                exec_result
            );
        }
    }
    write_host!(host, "\n=======> OK! <=======\n");
}

#[allow(clippy::too_many_arguments)]
pub fn run_test(
    path: &Path,
    report_map: &mut ReportMap,
    report_key: String,
    opt: &Opt,
    output_file: &mut Option<File>,
    skip: bool,
    diff_result_map: &mut DiffMap,
    output: &OutputOptions,
    skip_data: &SkipData,
) -> Result<(), TestError> {
    let suit = read_testsuite(path)?;
    let mut host = prepare_host();

    for (name, unit) in suit.0.into_iter() {
        if output.log {
            write_out!(output_file, "Running unit test: {}", name);
        }
        let precompiles = precompile_set::<EvalHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();

        let filler_source = prepare_filler_source(&host, &unit, opt)?;

        let mut env = initialize_env(&unit)?;
        let info = &unit._info;

        // post and execution
        for (spec_name, tests) in &unit.post {
            let config = match spec_name {
                SpecName::Shanghai => Config {
                    call_stack_limit: 256,
                    ..Config::shanghai()
                },
                // TODO: enable future configs when parallelization is enabled.
                // Other tests are ignored
                _ => continue,
            };

            for test_execution in tests.iter() {
                let data = test_execution.indexes.data;
                let gas = test_execution.indexes.gas;
                let value = test_execution.indexes.value;
                if skip {
                    let full_name = format!(
                        "{}_{}_data_index_{}_gas_index_{}_value_index_{}",
                        &report_key, name, data, gas, value
                    );
                    let status = TestResult::Skipped;
                    if output.result {
                        output_result(output_file, &full_name, status);
                    }
                    if let Some(map) = diff_result_map {
                        if let Some((result, _)) = map.get(&full_name) {
                            map.insert(full_name, (*result, Some(status)));
                        }
                    }
                    continue;
                }
                host = prepare_host_with_buffer(host.buffer.take());
                initialize_accounts(&mut host, &unit);
                let data_label = info.labels.get(&data);
                if let Some(data_label) = data_label {
                    if output.log {
                        write_out!(output_file, "Executing test {}", data_label);
                    }
                }

                let data = unit
                    .transaction
                    .data
                    .get(test_execution.indexes.data)
                    .unwrap()
                    .clone();

                if data_to_skip(&data, skip_data) {
                    continue;
                }

                let exec_result = execute_transaction(
                    &mut host,
                    &mut evm_account_storage,
                    &precompiles,
                    &config,
                    &unit,
                    &mut env,
                    test_execution,
                    data,
                );

                let labels = LabelIndexes {
                    data_label,
                    gas_label: info.labels.get(&gas),
                    value_label: info.labels.get(&value),
                };
                // Check the state after the execution of the result.
                match filler_source.clone() {
                    Some(filler_source) => {
                        let result = process(
                            &mut host,
                            filler_source,
                            spec_name,
                            report_map,
                            report_key.clone(),
                            output_file,
                            labels,
                            &test_execution.indexes,
                            output,
                            &name,
                            diff_result_map,
                        );
                        if opt.ci_mode && result == TestResult::Failure {
                            panic!(
                                "The execution isn't 100% compatible anymore. \
                                 Use the evaluation assessor to output debug traces and \
                                 find the issue."
                            )
                        }
                    }
                    None => write_host!(
                        host,
                        "No filler file, the outcome of this test is uncertain."
                    ),
                };

                check_results(&host, &name, test_execution, &exec_result);
                host.buffer.borrow_mut().clear();
            }
        }
    }
    Ok(())
}
