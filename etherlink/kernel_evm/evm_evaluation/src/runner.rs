// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2021-2023 draganrakita
//
// SPDX-License-Identifier: MIT

use evm_execution::account_storage::{init_account_storage, EthereumAccount};
use evm_execution::precompiles::precompile_set;
use evm_execution::{run_transaction, Config};

use tezos_ethereum::block::BlockConstants;

use hex_literal::hex;
use primitive_types::{H160, H256, U256};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use thiserror::Error;

use crate::evalhost::EvalHost;
use crate::fillers::process;
use crate::helpers::construct_folder_path;
use crate::models::{Env, FillerSource, SpecName, TestSuite, TestUnit};
use crate::{write_host, Opt, ReportValue};

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

pub fn run_test(
    path: &Path,
    report_map: &mut HashMap<String, ReportValue>,
    report_key: String,
    opt: &Opt,
    output_file: &mut File,
) -> Result<(), TestError> {
    let json_reader = std::fs::read(path).unwrap();
    let suit: TestSuite = serde_json::from_reader(&*json_reader)?;
    let execution_buffer = Vec::new();
    let buffer = RefCell::new(execution_buffer);
    let mut host = EvalHost::default_with_buffer(buffer);

    let map_caller_keys: HashMap<H256, H160> = MAP_CALLER_KEYS.into();

    for (name, unit) in suit.0.into_iter() {
        let precompiles = precompile_set::<EvalHost>();
        let mut evm_account_storage = init_account_storage().unwrap();

        writeln!(output_file, "Running unit test: {}", name).unwrap();
        let filler_source = prepare_filler_source(&host, &unit, opt)?;

        write_host!(host, "\n[START] Accounts initialisation");
        for (address, info) in unit.pre.into_iter() {
            let h160_address: H160 = address.as_fixed_bytes().into();
            write_host!(host, "\nAccount is {}", h160_address);
            let mut account =
                EthereumAccount::from_address(&address.as_fixed_bytes().into()).unwrap();
            if info.nonce != 0 {
                account.set_nonce(&mut host, info.nonce.into()).unwrap();
                write_host!(host, "Nonce is set for {} : {}", address, info.nonce);
            }
            account.balance_add(&mut host, info.balance).unwrap();
            write_host!(host, "Balance for {} was added : {}", address, info.balance);
            account.set_code(&mut host, &info.code).unwrap();
            write_host!(host, "Code was set for {}", address);
            for (index, value) in info.storage.iter() {
                account.set_storage(&mut host, index, value).unwrap();
            }
        }
        write_host!(host, "\n[END] Accounts initialisation\n");

        let mut env = Env::default();

        // BlockEnv
        env.block.number = unit.env.current_number;
        env.block.coinbase = unit.env.current_coinbase;
        env.block.timestamp = unit.env.current_timestamp;
        env.block.gas_limit = unit.env.current_gas_limit;
        env.block.basefee = unit.env.current_base_fee.unwrap_or_default();

        // TxEnv
        env.tx.caller = if let Some(caller) =
            map_caller_keys.get(&unit.transaction.secret_key.unwrap())
        {
            *caller
        } else {
            let private_key = unit.transaction.secret_key.unwrap();
            return Err(TestError::UnknownPrivateKey { private_key });
        };
        env.tx.gas_price = unit
            .transaction
            .gas_price
            .unwrap_or_else(|| unit.transaction.max_fee_per_gas.unwrap_or_default());

        // post and execution
        for (spec_name, tests) in unit.post {
            let config = match spec_name {
                SpecName::Shanghai => Config::shanghai(),
                // TODO: enable future configs when parallelization is enabled.
                // Other tests are ignored
                _ => continue,
            };

            for test_execution in tests.into_iter() {
                let gas_limit = *unit
                    .transaction
                    .gas_limit
                    .get(test_execution.indexes.gas)
                    .unwrap();
                let gas_limit = u64::try_from(gas_limit).unwrap_or(u64::MAX);
                env.tx.gas_limit = gas_limit;
                env.tx.data = unit
                    .transaction
                    .data
                    .get(test_execution.indexes.data)
                    .unwrap()
                    .clone();
                env.tx.value = *unit
                    .transaction
                    .value
                    .get(test_execution.indexes.value)
                    .unwrap();
                env.tx.transact_to = unit.transaction.to;

                let block_constants = BlockConstants {
                    gas_price: env.tx.gas_price,
                    number: env.block.number,
                    coinbase: env.block.coinbase.to_fixed_bytes().into(),
                    timestamp: env.block.timestamp,
                    gas_limit: env.block.gas_limit.as_u64(),
                    base_fee_per_gas: env.block.basefee,
                    chain_id: U256::from(1337),
                };
                let address = env.tx.transact_to.map(|addr| addr.to_fixed_bytes().into());
                let caller = env.tx.caller.to_fixed_bytes().into();
                let call_data = env.tx.data.to_vec();
                let gas_limit = Some(env.tx.gas_limit);
                let transaction_value = Some(env.tx.value);
                let pay_for_gas = true; // always, for now

                let exec_result = run_transaction(
                    &mut host,
                    &block_constants,
                    &mut evm_account_storage,
                    &precompiles,
                    config.clone(),
                    address,
                    caller,
                    call_data,
                    gas_limit,
                    transaction_value,
                    pay_for_gas,
                    u64::MAX, // don't account for ticks during the test
                );

                match &exec_result {
                    Ok(execution_outcome_opt) => {
                        let outcome_status = match execution_outcome_opt {
                            Some(execution_outcome) => {
                                if execution_outcome.is_success {
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

                write!(host.buffer.borrow_mut(), "\nFinal check: ").unwrap();
                match (&test_execution.expect_exception, &exec_result) {
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
                            test_execution.expect_exception,
                            exec_result
                        );
                    }
                }
                write_host!(host, "\n=======> OK! <=======\n");
            }

            // Check the state after the execution of the result.
            match filler_source.clone() {
                Some(filler_source) => process(
                    &mut host,
                    filler_source,
                    &spec_name,
                    report_map,
                    report_key.clone(),
                    output_file,
                ),
                None => write_host!(
                    host,
                    "No filler file, the outcome of this test is uncertain."
                ),
            };
        }
    }
    Ok(())
}
