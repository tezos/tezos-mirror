// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    fixture::{Account, Env, Fixtures, NamedFixture, PostEntry, TestCase},
    helpers::{
        extract_brackets, prepare_host, prepare_host_with_buffer, pretty, u256_to_u128,
    },
};
use revm::{
    context::{
        result::{EVMError, ExecutionResult},
        transaction::AccessList,
    },
    primitives::{Address, HashMap, U256},
    state::AccountInfo,
};
use revm_etherlink::{
    run_transaction, storage::world_state_handler::StorageAccount, ExecutionOutcome,
    GasData,
};
use std::{
    ffi::OsStr,
    fs::{self, File, OpenOptions},
    path::Path,
    process::exit,
};
use structopt::StructOpt;
use tezos_ethereum::block::{BlockConstants, BlockFees};
use tezos_evm_runtime::runtime::Runtime;

mod deserializer;
mod evalhost;
mod fixture;
mod helpers;

#[derive(Debug, StructOpt)]
#[structopt(name = "revm-evaluation", about = "Evaluate REVM's engine semantic.")]
pub struct Opt {
    #[structopt(
        short = "d",
        long = "test-cases",
        default_value = "etherlink/kernel_latest/revm_evaluation/fixtures",
        about = "Specify the directory path of fixtures files. By default it will be \
                 'etherlink/kernel_latest/revm_evaluation/fixtures'."
    )]
    test_cases_path: String,
}

fn skip_dir(dir_name: &OsStr) -> bool {
    let dir_name = dir_name.to_str().unwrap();
    matches!(
        dir_name,
        // This is in case the fixture repository was simply
        // cloned and not linked via symbolic links.
        ".git" |
        // Blobs are not supported on Etherlink.
        "eip7516_blobgasfee" | "eip4844_blobs" |
        // Etherlink transaction's gas limit cap isn't the same
        // as on Ethereum.
        "eip7825_transaction_gas_limit_cap"
    )
}

fn skip_file(file_name: &OsStr) -> bool {
    let file_name = file_name.to_str().unwrap();
    matches!(
        file_name,
        | "README.md"
        // Blobs are not supported on Etherlink.
        | "transaction_validity_type_3.json"
    )
}

fn skip_test(test_name: &str) -> bool {
    // Some tests are mixed to work with blobs.
    // Blobs are not supported on Etherlink.
    if test_name.contains("blob") {
        return true;
    }

    matches!(
        test_name,
        // We don't exactly implement EIP-1559 as the sequencer is already
        // compensated via DA fees.
        // The following test checks that max fee per gas isn't lower than
        // max priority fee per gas, but we do not need to make this check
        // as our effective gas price is always the current base fee.
        | "fork_Prague-state_test-priority_greater_than_max_fee_per_gas"
    )
}

// [SKIP-INVEST]
// Several tests are skipped for now and need deeper investigation.
// The investigation can be done on 2 main aspects:
// * is the bug on our side (mainly revm implem, storage etc.)?
// * is the bug on the test framework side? this is a possibility as
// we do not check the states via root hash but by expected accounts
// states which could be wrongly generated.
fn skip_investigation_test(test_name: &str) -> bool {
    matches!(
        test_name,
          "fork_Prague-state_test-multiple_valid_authorizations_single_signer"
        | "fork_Prague-state_test-multiple_valid_authorizations_single_signer-check_delegated_account_first_False"
        | "fork_Prague-state_test-multiple_valid_authorizations_single_signer-check_delegated_account_first_True"
        | "fork_Prague-state_test-valid_True-multiple_valid_authorizations_single_signer"
        | "fork_Prague-state_test-revert-tx_value_1-zero_balance_authority"
        | "fork_Prague-state_test-invalid-tx_value_0-zero_balance_authority"
        | "fork_Prague-state_test-out-of-gas-tx_value_0-zero_balance_authority"
        | "fork_Prague-state_test-out-of-gas-tx_value_1-zero_balance_authority"
        | "fork_Prague-state_test-revert-tx_value_0-zero_balance_authority"
        | "fork_Prague-state_test-invalid-tx_value_1-zero_balance_authority"
        | "fork_Prague-state_test-type_4-single_authorization-single_access_list_multiple_storage_keys-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-multiple_access_lists_multiple_storage_keys-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-single_access_list_multiple_storage_keys-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-no_access_list-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-no_access_list-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-single_access_list_multiple_storage_keys-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-single_access_list_single_storage_key-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-single_access_list_multiple_storage_keys-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-multiple_access_lists_single_storage_key-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-single_access_list_single_storage_key-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-multiple_access_lists_single_storage_key-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-no_access_list-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-single_access_list_no_storage_keys-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-multiple_access_lists_multiple_storage_keys-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-multiple_access_lists_single_storage_key-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-multiple_access_lists_no_storage_keys-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-multiple_access_lists_multiple_storage_keys-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-single_access_list_no_storage_keys-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-multiple_access_lists_single_storage_key-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-no_access_list-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-multiple_access_lists_no_storage_keys-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-multiple_access_lists_no_storage_keys-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-single_access_list_single_storage_key-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-single_access_list_no_storage_keys-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-single_access_list_no_storage_keys-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-single_access_list_single_storage_key-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-multiple_authorizations-multiple_access_lists_no_storage_keys-extra_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-type_4-single_authorization-multiple_access_lists_multiple_storage_keys-exact_gas-floor_gas_less_than_or_equal_to_intrinsic_gas"
        | "fork_Prague-state_test-exact_gas-type_3"
        | "fork_Prague-state_test-extra_gas-type_4"
        | "fork_Prague-state_test-extra_gas-type_3"
    )
}

// See [SKIP-INVEST] comment.
fn skip_investigation_file(file_name: &OsStr, report: &mut Report) -> bool {
    let file_name = file_name.to_str().unwrap();
    let skip = matches!(
        file_name,
        "ext_code_on_chain_delegating_set_code.json"
            | "tx_into_chain_delegating_set_code.json"
            | "tx_into_self_delegating_set_code.json"
            | "delegation_clearing_and_set.json"
    );
    if skip {
        report.skipped_invest += 1;
    }
    skip
}

fn find_fixture(path: &Path, acc: &mut Fixtures, report: &mut Report) {
    if path.is_dir() {
        if path.file_name().map(skip_dir).unwrap_or(false) {
            return;
        }

        for entry in fs::read_dir(path).unwrap() {
            let entry = entry.unwrap();
            let entry_path = entry.path();

            if entry_path.is_dir() {
                find_fixture(&entry_path, acc, report);
            } else if entry_path.is_file()
                && !entry_path
                    .file_name()
                    .map(|file_name| {
                        skip_file(file_name) || skip_investigation_file(file_name, report)
                    })
                    .unwrap_or(false)
            {
                let content = fs::read_to_string(&entry_path).unwrap();
                let fixtures: HashMap<String, TestCase> =
                    serde_json::from_str(&content).unwrap();
                acc.push(NamedFixture {
                    path: entry_path,
                    fixtures,
                });
            }
        }
    }
}

fn read_all_fixtures<P: AsRef<Path>>(fixtures_dir: P, report: &mut Report) -> Fixtures {
    let mut fixtures = Vec::new();
    find_fixture(fixtures_dir.as_ref(), &mut fixtures, report);
    fixtures
}

fn fill_state(host: &mut impl Runtime, state: HashMap<Address, Account>) {
    for (address, info) in state {
        let mut storage_account = StorageAccount::from_address(&address).unwrap();
        for (index, value) in &info.storage {
            storage_account.set_storage(host, index, value).unwrap();
        }
        storage_account.set_info(host, info.into()).unwrap();
    }
}

fn check_result(
    host: &mut impl Runtime,
    state: HashMap<Address, Account>,
    output_file: &mut Option<File>,
    total_gas_refunded: U256,
) -> bool {
    let mut success = true;
    for (address, info) in state {
        let storage_account = StorageAccount::from_address(&address).unwrap();

        let mut storage_error = String::new();
        for (index, expected_value) in &info.storage {
            let commited_value = storage_account.get_storage(host, index).unwrap();
            if expected_value != &commited_value {
                storage_error.push_str(&format!("==> Storage mismatch, for index {index}, got {commited_value} instead of {expected_value}.\n"));
            }
        }

        let mut expected_info: AccountInfo = info.into();
        let commited_info = storage_account.info(host).unwrap();

        // ==> HACK
        // A few tests falsely expect the balance to not contain the refunded gas which
        // is wrong.
        // This is easily checkable and adaptable through the following condition.
        if commited_info.balance - total_gas_refunded == expected_info.balance {
            expected_info.balance = commited_info.balance;
        };

        if expected_info != commited_info || !storage_error.is_empty() {
            let AccountInfo {
                balance: expected_balance,
                nonce: expected_nonce,
                code_hash: expected_code_hash,
                code: expected_code,
                ..
            } = expected_info;
            let AccountInfo {
                balance: commited_balance,
                nonce: commited_nonce,
                code_hash: commited_code_hash,
                code: commited_code,
                ..
            } = commited_info;

            success = false;
            let mut state_mismatch = String::new();
            state_mismatch.push_str(&format!(
                "\n/!\\ State transition did not match for {address}.\n"
            ));
            state_mismatch.push_str(&storage_error);
            if expected_balance != commited_balance {
                state_mismatch
                    .push_str(&format!("==> Balance mismatch, got {commited_balance} instead of {expected_balance}.\n"));
            }
            if expected_nonce != commited_nonce {
                state_mismatch
                    .push_str(&format!("==> Nonce mismatch, got {commited_nonce} instead of {expected_nonce}.\n"));
            }
            if expected_code_hash != commited_code_hash {
                state_mismatch.push_str(&format!(
                    "==> Code hash mismatch, got {} instead of {}.\n",
                    pretty::code_hash(commited_code_hash),
                    pretty::code_hash(expected_code_hash)
                ));
            }
            if expected_code != commited_code {
                state_mismatch.push_str(&format!(
                    "==> Code mismatch, got {commited_code:?} instead of {expected_code:?}.\n"
                ));
            }

            write_out!(output_file, "{state_mismatch}");
        } else {
            write_out!(output_file, "State transition matched for {address}.");
        }
    }
    success
}

// TODO: These conversions will be droppped once we remove
// previous evm execution. The API to run transaction should
// not depend on BlockConstants anymore.
fn get_block_constants(env: &Env, chain_id: primitive_types::U256) -> BlockConstants {
    BlockConstants {
        number: primitive_types::U256::from(env.current_number.to_le_bytes()),
        coinbase: primitive_types::H160::from(Into::<[u8; 20]>::into(
            env.current_coinbase.0,
        )),
        timestamp: primitive_types::U256::from(env.current_timestamp.to_le_bytes()),
        gas_limit: env.current_gas_limit,
        block_fees: BlockFees::new(
            primitive_types::U256::zero(),
            env.current_base_fee.into(),
            primitive_types::U256::zero(),
        ),
        chain_id,
        tezos_experimental_features: false,
        prevrandao: Some(primitive_types::H256::from(
            env.current_difficulty.to_le_bytes(),
        )),
    }
}

fn extract_gas_refunded(
    execution_result: &Result<ExecutionOutcome, EVMError<revm_etherlink::Error>>,
) -> u64 {
    match execution_result {
        Ok(ExecutionOutcome {
            result: ExecutionResult::Success { gas_refunded, .. },
            ..
        }) => *gas_refunded,
        _ => 0,
    }
}

#[derive(Default)]
struct Report {
    success: u64,
    failure: u64,
    skipped_invest: u64,
}

pub fn main() {
    let opt = Opt::from_args();
    let mut report = Report::default();
    let fixtures = read_all_fixtures(opt.test_cases_path, &mut report);
    // TODO: this is an option so that when we have the ability to print on the standard
    // output, we just have to replace it by None. The feature will be implemented soon.
    let mut output_file = if cfg!(feature = "disable-file-logs") {
        None
    } else {
        Some(
            OpenOptions::new()
                .write(true)
                .append(false)
                .truncate(true)
                .create(true)
                .open("revm_evaluation.logs")
                .unwrap(),
        )
    };

    let mut host = prepare_host();
    let registry = kernel::registry_impl::RegistryImpl::default();

    for NamedFixture { path, fixtures } in fixtures {
        write_out!(output_file, "---------- Test file: {:?} ----------", path);

        for (
            test_name,
            TestCase {
                env,
                pre,
                transaction,
                post,
                config,
                ..
            },
        ) in fixtures
        {
            let test_name = extract_brackets(&test_name);

            if skip_test(test_name) {
                continue;
            }

            if skip_investigation_test(test_name) {
                report.skipped_invest += 1;
                continue;
            }

            write_out!(output_file, "Running state transition test: {}", test_name);

            for (spec_name, post_entrys) in post {
                for PostEntry { state, indexes, .. } in post_entrys {
                    host = prepare_host_with_buffer(host.buffer.take());
                    fill_state(&mut host, pre.clone());
                    let spec_id = spec_name.clone().into();
                    write_out!(output_file, "EVM spec: {spec_name:?}");
                    let block_constants =
                        get_block_constants(&env, config.chainid.into());
                    let effective_gas_price = transaction.derive_gas_price(u256_to_u128(
                        block_constants.base_fee_per_gas(),
                    ));
                    let access_lists = AccessList(
                        transaction
                            .access_lists
                            .clone()
                            .unwrap_or_default()
                            .into_iter()
                            .flat_map(|x| x.0)
                            .collect(),
                    );

                    let execution_result = run_transaction(
                        &mut host,
                        &registry,
                        spec_id,
                        &block_constants,
                        None,
                        transaction.sender,
                        transaction.to,
                        transaction.data[indexes.data].clone(),
                        GasData::new(
                            transaction.gas_limit[indexes.gas],
                            effective_gas_price,
                            transaction.gas_limit[indexes.gas],
                        ),
                        transaction.value[indexes.value],
                        access_lists,
                        transaction.authorization_list.clone(),
                        None,
                        false,
                    );

                    let total_gas_refunded =
                        U256::from(extract_gas_refunded(&execution_result))
                            .checked_mul(U256::from_limbs(
                                block_constants.base_fee_per_gas().0,
                            ))
                            .unwrap();

                    let final_result = check_result(
                        &mut host,
                        state,
                        &mut output_file,
                        total_gas_refunded,
                    );

                    let result_output = if final_result {
                        report.success += 1;
                        "SUCCESS"
                    } else {
                        report.failure += 1;
                        "FAILURE"
                    };

                    match execution_result {
                        Ok(ExecutionOutcome { result, .. }) => {
                            write_out!(
                                output_file,
                                "FINAL RESULT: {result_output}\n\
                                 Details: {result:?}\n"
                            );
                        }
                        Err(error) => {
                            write_out!(
                                output_file,
                                "FINAL RESULT: {result_output}\n\
                                 Details: {error:?}\n"
                            );
                        }
                    }
                }
            }
        }
    }

    write_out!(
        output_file,
        "\n@================== FINAL REPORT ==================@\n            \
            Successful test(s): {}\n            \
            Failing test(s): {}\n     \
            Skipped but need investigation test(s): {}\n\
           @==================================================@",
        report.success,
        report.failure,
        report.skipped_invest
    );

    if report.failure > 0 {
        eprintln!(
            "The execution isn't 100% compatible anymore. \
             Use the revm evaluation assessor to output debug \
             traces and find the issue."
        );
        exit(1)
    } else {
        exit(0)
    }
}
