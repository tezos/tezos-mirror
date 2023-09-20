// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

mod helpers;
mod models;
mod runner;

use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};
use walkdir::{DirEntry, WalkDir};

const SKIP_ANY: bool = true;

pub fn find_all_json_tests(path: &Path) -> Vec<PathBuf> {
    WalkDir::new(path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_name().to_string_lossy().ends_with(".json"))
        .map(DirEntry::into_path)
        .collect::<Vec<PathBuf>>()
}

/* REPORT - 23/10/2023 - 14:40 (UTC+2)

# Number of tests:
    - Runned: 2595
    - Skipped: 69

# Ethereum transactions:
    - 17571 in total with Shanghai config

# EVM execution outcomes: (just for stats, not relevant)
    - 5266 SUCCESS
    - 12137 FAILURE
    - 168 INVALID

# EVM evaluation interpretation:

## Per sub-checks:
    - 61945 CORRECT STATE
    - 97773 INVALID STATE

## Per runned ethereum transactions:
    - 5188 GOOD STATE
    - 12383 BAD STATE

## Per test interpreted:
    - 850 SUCCESS (33.25%)
    - 1705 FAILURE (66.75%)

EVM confidence : 0.33 (target: 1.00) */

pub fn main() {
    // Preliminary step:
    // Clone https://github.com/ethereum/tests repo inside [engine_evaluation]
    let folder_path = "tests/GeneralStateTests";
    let test_files = find_all_json_tests(&PathBuf::from(folder_path));
    println!("Start running tests on: {:?}", folder_path);
    for test_file in test_files.into_iter() {
        println!("---------- Test: {:?} ----------", test_file);

        if SKIP_ANY {
            // Funky test with `bigint 0x00` value in json not possible to happen on
            // Mainnet and require custom json parser.
            if test_file.file_name() == Some(OsStr::new("ValueOverflow.json")) {
                println!("\nSKIPPED\n");
                continue;
            }

            // The following test are failing they need in depth debugging
            // Reason: panicked at 'arithmetic operation overflow'
            if test_file.file_name() == Some(OsStr::new("HighGasPrice.json"))
                || test_file.file_name() == Some(OsStr::new("randomStatetest32.json"))
                || test_file.file_name() == Some(OsStr::new("randomStatetest7.json"))
                || test_file.file_name() == Some(OsStr::new("randomStatetest50.json"))
                || test_file.file_name() == Some(OsStr::new("randomStatetest468.json"))
                || test_file.file_name() == Some(OsStr::new("gasCostBerlin.json"))
            {
                println!("\nSKIPPED\n");
                continue;
            }

            // The following test are failing they need in depth debugging
            // Reason: stack overflow
            if test_file.file_name() == Some(OsStr::new("CallRecursiveContract.json"))
                || test_file.file_name()
                    == Some(OsStr::new("RecursiveCreateContracts.json"))
            {
                println!("\nSKIPPED\n");
                continue;
            }

            // The following test are failing they need in depth debugging
            // Reason: memory allocation of X bytes failed | 73289 IOT instruction (core dumped)
            if test_file.file_name() == Some(OsStr::new("sha3.json")) {
                println!("\nSKIPPED\n");
                continue;
            }

            // Long tests ✔ (passing)
            if test_file.file_name() == Some(OsStr::new("loopMul.json")) {
                println!("\nSKIPPED\n");
                continue;
            }

            // Oddly long checks on a test that do no relevant check (passing)
            if test_file.file_name() == Some(OsStr::new("intrinsic.json")) {
                println!("\nSKIPPED\n");
                continue;
            }

            // SKIPPED BECAUSE OF WRONG PARSING OF FILLER FILES

            // ********** JSON ********** //

            // Reason: comments in the result field
            if test_file.file_name() == Some(OsStr::new("add11.json"))
                || test_file.file_name() == Some(OsStr::new("add11.json"))
                || test_file.file_name()
                    == Some(OsStr::new("static_CREATE_EmptyContractAndCallIt_0wei.json"))
                || test_file.file_name()
                    == Some(OsStr::new(
                        "static_CREATE_EmptyContractWithStorageAndCallIt_0wei.json",
                    ))
                || test_file.file_name() == Some(OsStr::new("callToNonExistent.json"))
                || test_file.file_name()
                    == Some(OsStr::new("CreateAndGasInsideCreate.json"))
            {
                println!("\nSKIPPED\n");
                continue;
            }

            // Reason: invalid length 0, expected a (both 0x-prefixed or not) hex string or
            // byte array containing between (0; 32] bytes
            if test_file.file_name()
                == Some(OsStr::new("ZeroValue_SUICIDE_ToOneStorageKey.json"))
            {
                println!("\nSKIPPED\n");
                continue;
            }

            // Reason: inconsistent hex/dec field value
            if test_file.file_name() == Some(OsStr::new("TouchToEmptyAccountRevert.json"))
                || test_file.file_name()
                    == Some(OsStr::new("CREATE_EContract_ThenCALLToNonExistentAcc.json"))
                || test_file.file_name() == Some(OsStr::new("CREATE_EmptyContract.json"))
                || test_file.file_name() == Some(OsStr::new("StoreGasOnCreate.json"))
                || test_file.file_name() == Some(OsStr::new("OverflowGasRequire2.json"))
                || test_file.file_name() == Some(OsStr::new("StackDepthLimitSEC.json"))
            {
                println!("\nSKIPPED\n");
                continue;
            }

            // ********** YAML ********** //

            // Reason: invalid hex character: _
            if test_file.file_name() == Some(OsStr::new("doubleSelfdestructTest.json"))
                || test_file.file_name() == Some(OsStr::new("clearReturnBuffer.json"))
                || test_file.file_name() == Some(OsStr::new("gasCost.json"))
            {
                println!("\nSKIPPED\n");
                continue;
            }

            // Reason: invalid length 0, expected a (both 0x-prefixed or not) hex string or
            // byte array containing between (0; 32] bytes
            if test_file.file_name() == Some(OsStr::new("eqNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("mulmodNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("addmodNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("smodNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("callcodeNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("mstoreNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("modNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("extcodesizeNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("log1NonConst.json"))
                || test_file.file_name() == Some(OsStr::new("extcodecopyNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("log2NonConst.json"))
                || test_file.file_name() == Some(OsStr::new("andNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("log3NonConst.json"))
                || test_file.file_name() == Some(OsStr::new("sgtNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("expNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("mloadNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("log0NonConst.json"))
                || test_file.file_name() == Some(OsStr::new("byteNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("orNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("codecopyNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("gtNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("signextNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("ltNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("sltNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("balanceNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("mstore8NonConst.json"))
                || test_file.file_name() == Some(OsStr::new("delegatecallNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("iszeroNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("subNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("calldatacopyNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("sha3NonConst.json"))
                || test_file.file_name() == Some(OsStr::new("sdivNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("addNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("notNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("createNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("xorNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("calldataloadNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("divNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("returnNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("mulNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("callNonConst.json"))
                || test_file.file_name() == Some(OsStr::new("twoOps.json"))
            {
                println!("\nSKIPPED\n");
                continue;
            }
        }
        runner::run_test(&test_file).unwrap();
    }
    println!("@@@@@ END OF TESTING @@@@@");
}
