// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

mod evalhost;
mod fillers;
mod helpers;
mod models;
mod runner;

use std::{
    collections::HashMap,
    ffi::OsStr,
    fs::{File, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
};
use structopt::StructOpt;
use walkdir::{DirEntry, WalkDir};

use crate::helpers::construct_folder_path;

const SKIP_ANY: bool = true;

pub fn find_all_json_tests(path: &Path) -> Vec<PathBuf> {
    WalkDir::new(path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_name().to_string_lossy().ends_with(".json"))
        .map(DirEntry::into_path)
        .collect::<Vec<PathBuf>>()
}

#[derive(Default, Clone)]
pub struct ReportValue {
    pub successes: u16,
    pub failures: u16,
    pub skipped: u16,
}

#[derive(Debug, StructOpt)]
#[structopt(name = "evm-evaluation", about = "Evaluate EVM's engine semantic.")]
pub struct Opt {
    #[structopt(
        short = "d",
        long = "eth-tests",
        default_value = "tests",
        about = "Specify the directory path of [ethereum/tests]. By default it will be 'tests/'."
    )]
    eth_tests: String,
    #[structopt(
        short = "s",
        long = "sub-directory",
        about = "Specify the sub directory of tests you want to execute."
    )]
    sub_dir: Option<String>,
    #[structopt(
        short = "t",
        long = "test",
        about = "Specify the name of the test to execute."
    )]
    test: Option<String>,
    #[structopt(
        short = "o",
        long = "output",
        default_value = "evm_evaluation.regression",
        about = "Specify the file where the logs will be outputed. By default it will be outputed to 'evm_evaluation.regression'."
    )]
    output: String,
    #[structopt(
        short = "r",
        long = "report-only",
        about = "Output only the final report."
    )]
    report_only: bool,
    #[structopt(
        short = "h",
        long = "from-scratch",
        about = "Overwrite the target file where the logs will be outputed."
    )]
    from_scratch: bool,
}

fn generate_final_report(
    output_file: &mut File,
    report_map: &mut HashMap<String, ReportValue>,
) {
    let mut successes_total = 0;
    let mut failure_total = 0;
    let mut skipped_total = 0;
    let mut final_report: HashMap<&str, Vec<(String, u16, u16, u16)>> = HashMap::new();

    for (key, report_value) in report_map {
        let insert_element = (
            key.to_string(),
            report_value.successes,
            report_value.failures,
            report_value.skipped,
        );
        successes_total += report_value.successes;
        failure_total += report_value.failures;
        skipped_total += report_value.skipped;
        if report_value.successes != 0 || report_value.failures != 0 {
            if report_value.failures == 0 {
                let entry = if report_value.skipped == 0 {
                    "Fully Successful Tests"
                } else {
                    "Fully Successful Unskipped Tests"
                };
                final_report
                    .entry(entry)
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            } else if key == "stMemExpandingEIP150Calls"
                || key == "stEIP150singleCodeGasPrices"
                || key == "stEIP150Specific"
            {
                final_report
                    .entry("EIP-150")
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            } else if key == "stEIP1559" {
                final_report
                    .entry("EIP-1559")
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            } else if key == "stEIP158Specific"
                || key == "stEIP2930"
                || key == "stEIP3860-limitmeterinitcode"
                || key == "stEIP3607"
            {
                final_report
                    .entry("Other EIPs")
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            } else if key == "stPreCompiledContracts"
                || key == "stPreCompiledContracts2"
                || key == "stStaticFlagEnabled"
            {
                final_report
                    .entry("Precompiled Contracts")
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            } else if key == "vmIOandFlowOperations"
                || key == "vmTests"
                || key == "vmArithmeticTest"
                || key == "vmPerformance"
                || key == "vmLogTest"
                || key == "vmBitwiseLogicOperation"
            {
                final_report
                    .entry("VM Specific")
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            } else if key == "stBadOpcode"
                || key == "stSolidityTest"
                || key == "stRecursiveCreate"
                || key == "stCreateTest"
                || key == "stCreate2"
                || key == "stCallCodes"
                || key == "stCodeCopyTest"
                || key == "stCallCreateCallCodeTest"
                || key == "stZeroCallsTest"
                || key == "stSStoreTest"
                || key == "stSLoadTest"
                || key == "stStaticCall"
                || key == "stExtCodeHash"
                || key == "stRevertTest"
                || key == "stShift"
                || key == "stSelfBalance"
                || key == "stStackTests"
                || key == "stNonZeroCallsTest"
                || key == "stZeroCallsRevert"
                || key == "stRefundTest"
                || key == "stSystemOperationsTest"
                || key == "stCodeSizeLimit"
                || key == "stInitCodeTest"
                || key == "stReturnDataTest"
            {
                final_report
                    .entry("Smart Contracts - Opcodes - Solidity")
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            } else if key == "stMemoryTest" || key == "stMemoryStressTest" {
                final_report
                    .entry("Memory")
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            } else if key == "stZeroKnowledge"
                || key == "stZeroKnowledge2"
                || key == "stHomesteadSpecific"
                || key == "stCallDelegateCodesCallCodeHomestead"
                || key == "stCallDelegateCodesHomestead"
                || key == "stDelegatecallTestHomestead"
            {
                final_report
                    .entry("Investigation/Suspended")
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            } else {
                final_report
                    .entry("Arbitrary/Random Tests")
                    .and_modify(|section_elems| {
                        section_elems.push(insert_element.clone())
                    })
                    .or_insert_with(|| vec![insert_element]);
            }
        }
    }

    writeln!(output_file, "@========= FINAL REPORT =========@").unwrap();

    for (section, items) in final_report {
        writeln!(output_file, "\n••• {} •••\n", section).unwrap();
        for (key, successes, failures, skipped) in items {
            let skipped_msg = if skipped == 0 {
                String::new()
            } else {
                format!(" with {} test(s) skipped", skipped)
            };
            writeln!(
                output_file,
                "For sub-dir {}, there was(were) {} success(es) and {} failure(s){}.",
                key, successes, failures, skipped_msg
            )
            .unwrap();
        }
    }

    writeln!(
        output_file,
        "\nSUCCESSES IN TOTAL: {}\nFAILURES IN TOTAL: {}\nSKIPPED IN TOTAL: {}",
        successes_total, failure_total, skipped_total
    )
    .unwrap();
}

pub fn main() {
    let opt = Opt::from_args();
    let mut output_file = OpenOptions::new()
        .write(true)
        .append(!opt.from_scratch)
        .create(true)
        .open(&opt.output)
        .unwrap();
    let folder_path =
        construct_folder_path("GeneralStateTests", &opt.eth_tests, &opt.sub_dir);
    let test_files = find_all_json_tests(&folder_path);
    let mut report_map: HashMap<String, ReportValue> = HashMap::new();

    if !opt.report_only {
        writeln!(
            output_file,
            "Start running tests on: {}",
            folder_path.to_str().unwrap()
        )
        .unwrap();
    }

    for test_file in test_files.into_iter() {
        let splitted_path: Vec<&str> = test_file.to_str().unwrap().split('/').collect();
        let report_key = splitted_path
            .get(splitted_path.len() - 2)
            .unwrap()
            .to_owned();
        if !report_map.contains_key(report_key) {
            report_map.insert(report_key.to_owned(), ReportValue::default());
        }

        if let Some(test) = &opt.test {
            let mut file_name = PathBuf::from(test);
            file_name.set_extension("json");
            if test_file.file_name() == Some(OsStr::new(&file_name)) {
                runner::run_test(
                    &test_file,
                    &mut report_map,
                    report_key.to_owned(),
                    &opt,
                    &mut output_file,
                )
                .unwrap();
            }
            continue;
        }

        if !opt.report_only {
            writeln!(output_file, "---------- Test: {:?} ----------", test_file).unwrap();
        }

        let mut process_skip = || {
            report_map
                .entry(report_key.to_owned())
                .and_modify(|report_value| {
                    *report_value = ReportValue {
                        skipped: report_value.skipped + 1,
                        ..*report_value
                    };
                });
            if !opt.report_only {
                writeln!(output_file, "\nSKIPPED\n").unwrap()
            }
        };

        if SKIP_ANY {
            // Funky test with `bigint 0x00` value in json not possible to happen on
            // Mainnet and require custom json parser.
            if test_file.file_name() == Some(OsStr::new("ValueOverflow.json")) {
                process_skip();
                continue;
            }

            // The following test(s) is/are failing they need in depth debugging
            // Reason: panicked at 'arithmetic operation overflow'
            if test_file.file_name() == Some(OsStr::new("HighGasPrice.json"))
                || test_file.file_name() == Some(OsStr::new("randomStatetest32.json"))
                || test_file.file_name() == Some(OsStr::new("randomStatetest7.json"))
                || test_file.file_name() == Some(OsStr::new("randomStatetest50.json"))
                || test_file.file_name() == Some(OsStr::new("randomStatetest468.json"))
                || test_file.file_name() == Some(OsStr::new("gasCostBerlin.json"))
                || test_file.file_name() == Some(OsStr::new("underflowTest.json"))
                || test_file.file_name() == Some(OsStr::new("randomStatetest384.json"))
                || test_file.file_name()
                    == Some(OsStr::new("201503110226PYTHON_DUP6.json"))
            {
                process_skip();
                continue;
            }

            // The following test(s) is/are failing they need in depth debugging
            // Reason: memory allocation of X bytes failed | 73289 IOT instruction (core dumped)
            if test_file.file_name() == Some(OsStr::new("sha3.json")) {
                process_skip();
                continue;
            }

            // Long tests ✔ (passing)
            if test_file.file_name() == Some(OsStr::new("loopMul.json")) {
                process_skip();
                continue;
            }

            // Oddly long checks on a test that do no relevant check (passing)
            if test_file.file_name() == Some(OsStr::new("intrinsic.json")) {
                process_skip();
                continue;
            }

            // Long tests ~ (outcome is unknown)
            if test_file.file_name() == Some(OsStr::new("static_Call50000_sha256.json"))
                || test_file.file_name()
                    == Some(OsStr::new("static_Call50000_ecrec.json"))
                || test_file.file_name() == Some(OsStr::new("static_Call50000.json"))
            {
                process_skip();
                continue;
            }

            // Reason: panicked at 'attempt to add with overflow'
            if let Some(file_name) = test_file.to_str() {
                if file_name.contains("DiffPlaces.json") {
                    process_skip();
                    continue;
                }
            }

            // Reason: panicked at 'attempt to multiply with overflow'
            if test_file.file_name()
                == Some(OsStr::new("static_Call1024BalanceTooLow.json"))
                || test_file.file_name()
                    == Some(OsStr::new("static_Call1024BalanceTooLow2.json"))
                || test_file.file_name()
                    == Some(OsStr::new("static_Call1024PreCalls3.json"))
            {
                process_skip();
                continue;
            }

            // Reason: panicked at 'attempt to add with overflow'
            if test_file.file_name() == Some(OsStr::new("static_Call1024PreCalls.json"))
                || test_file.file_name()
                    == Some(OsStr::new("static_Call1024PreCalls2.json"))
                || test_file.file_name() == Some(OsStr::new("diffPlaces.json"))
            {
                process_skip();
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
                process_skip();
                continue;
            }

            // Reason: invalid length 0, expected a (both 0x-prefixed or not) hex string or
            // byte array containing between (0; 32] bytes
            if test_file.file_name()
                == Some(OsStr::new("ZeroValue_SUICIDE_ToOneStorageKey.json"))
            {
                process_skip();
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
                process_skip();
                continue;
            }

            // ********** YAML ********** //

            // Reason: invalid hex character: _
            if test_file.file_name() == Some(OsStr::new("doubleSelfdestructTest.json"))
                || test_file.file_name() == Some(OsStr::new("clearReturnBuffer.json"))
                || test_file.file_name() == Some(OsStr::new("gasCost.json"))
            {
                process_skip();
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
                process_skip();
                continue;
            }
        }

        runner::run_test(
            &test_file,
            &mut report_map,
            report_key.to_owned(),
            &opt,
            &mut output_file,
        )
        .unwrap();
    }

    if !opt.report_only {
        writeln!(output_file, "@@@@@ END OF TESTING @@@@@\n").unwrap();
    }

    generate_final_report(&mut output_file, &mut report_map)
}
