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
    context::transaction::AccessList,
    primitives::{hash_map::HashMap, Address},
    state::AccountInfo,
};
use revm_etherlink::{
    precompiles::provider::EtherlinkPrecompiles,
    run_transaction,
    storage::world_state_handler::{
        new_world_state_handler, StorageAccount, WorldStateHandler,
    },
    ExecutionOutcome,
};
use std::{
    ffi::OsStr,
    fs::{self, File, OpenOptions},
    path::Path,
};
use tezos_ethereum::block::{BlockConstants, BlockFees};
use tezos_evm_runtime::runtime::Runtime;

mod deserializer;
mod evalhost;
mod fixture;
mod helpers;

fn skip_dir(dir_name: &OsStr) -> bool {
    let dir_name = dir_name.to_str().unwrap();
    matches!(
        dir_name,
        // This is in case the fixture repository was simply
        // cloned and not linked via symbolic links.
        ".git" |
        // Blobs are not supported on Etherlink.
        "eip7516_blobgasfee" | "eip4844_blobs"
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

fn find_fixture(path: &Path, acc: &mut Fixtures) {
    if path.is_dir() {
        if path.file_name().map(skip_dir).unwrap_or(false) {
            return;
        }

        for entry in fs::read_dir(path).unwrap() {
            let entry = entry.unwrap();
            let entry_path = entry.path();

            if entry_path.is_dir() {
                find_fixture(&entry_path, acc);
            } else if entry_path.is_file()
                && !entry_path.file_name().map(skip_file).unwrap_or(false)
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

fn read_all_fixtures<P: AsRef<Path>>(fixtures_dir: P) -> Fixtures {
    let mut fixtures = Vec::new();
    find_fixture(fixtures_dir.as_ref(), &mut fixtures);
    fixtures
}

fn fill_state(
    host: &mut impl Runtime,
    world_state_handler: &mut WorldStateHandler,
    state: HashMap<Address, Account>,
) {
    for (address, info) in state {
        let mut storage_account =
            StorageAccount::get_or_create_account(host, world_state_handler, address)
                .unwrap();
        for (index, value) in &info.storage {
            storage_account.set_storage(host, index, value).unwrap();
        }
        storage_account.set_info(host, info.into()).unwrap();
    }
}

fn check_result(
    host: &mut impl Runtime,
    world_state_handler: &WorldStateHandler,
    state: HashMap<Address, Account>,
    output_file: &mut Option<File>,
) -> bool {
    let mut success = true;
    for (address, info) in state {
        let storage_account =
            StorageAccount::get_or_create_account(host, world_state_handler, address)
                .unwrap();

        let mut storage_error = String::new();
        for (index, expected_value) in &info.storage {
            let commited_value = storage_account.get_storage(host, index).unwrap();
            if expected_value != &commited_value {
                storage_error.push_str(&format!("==> Storage mismatch, for index {index}, got {commited_value} instead of {expected_value}.\n"));
            }
        }

        let expected_info: AccountInfo = info.into();
        let commited_info = storage_account.info(host).unwrap();

        if expected_info != commited_info || !storage_error.is_empty() {
            let AccountInfo {
                balance: expected_balance,
                nonce: expected_nonce,
                code_hash: expected_code_hash,
                code: expected_code,
            } = expected_info;
            let AccountInfo {
                balance: commited_balance,
                nonce: commited_nonce,
                code_hash: commited_code_hash,
                code: commited_code,
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
        prevrandao: Some(primitive_types::H256::from(
            env.current_difficulty.to_le_bytes(),
        )),
    }
}

#[derive(Default)]
struct Report {
    success: u64,
    failure: u64,
}

pub fn main() {
    let mut report = Report::default();
    let fixtures = read_all_fixtures("etherlink/kernel_latest/revm_evaluation/fixtures");
    // TODO: this is an option so that when we have the ability to print on the standard
    // output, we just have to replace it by None. The feature will be implemented soon.
    let mut output_file = Some(
        OpenOptions::new()
            .write(true)
            .append(false)
            .truncate(true)
            .create(true)
            .open("revm_evaluation.logs")
            .unwrap(),
    );

    let mut host = prepare_host();
    let mut world_state_handler = new_world_state_handler().unwrap();

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

            // Some tests are mixed to work with blobs.
            // Blobs are not supported on Etherlink.
            if test_name.contains("blob") {
                continue;
            }

            write_out!(output_file, "Running state transition test: {}", test_name);

            for (spec_name, post_entrys) in post {
                for PostEntry { state, indexes, .. } in post_entrys {
                    host = prepare_host_with_buffer(host.buffer.take());
                    fill_state(&mut host, &mut world_state_handler, pre.clone());
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
                        spec_id,
                        &block_constants,
                        &mut world_state_handler,
                        EtherlinkPrecompiles::new(),
                        transaction.sender,
                        transaction.to,
                        transaction.data[indexes.data].clone(),
                        transaction.gas_limit[indexes.gas],
                        effective_gas_price,
                        transaction.value[indexes.value],
                        access_lists,
                        transaction.authorization_list.clone().unwrap_or_default(),
                        None,
                    );

                    let final_result = check_result(
                        &mut host,
                        &world_state_handler,
                        state,
                        &mut output_file,
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
            Failing test(s): {}\n\
           @==================================================@",
        report.success,
        report.failure
    );
}
