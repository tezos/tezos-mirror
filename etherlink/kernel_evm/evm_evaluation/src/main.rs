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
    fs::{read_to_string, File, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
};
use structopt::StructOpt;
use walkdir::{DirEntry, WalkDir};

use fillers::TestResult;
use helpers::{construct_folder_path, OutputOptions};

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

pub type ReportMap = HashMap<String, ReportValue>;

pub type DiffMap = Option<HashMap<String, (TestResult, Option<TestResult>)>>;

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
        about = "Specify the file where the logs will be outputed. \
                 By default it will be outputed to 'evm_evaluation.regression' \
                 (evm_evaluation.result with --result and evm_evaluation.diff with --diff)."
    )]
    output: Option<String>,
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
    #[structopt(
        long = "result",
        about = "Dump the result of the evaluation to be used for later diff."
    )]
    result: bool,
    #[structopt(long = "diff", about = "Compare result with a former evaluation.")]
    diff: Option<String>,
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

fn load_former_result(path: &str) -> HashMap<String, (TestResult, Option<TestResult>)> {
    let mut result_map: HashMap<String, (TestResult, Option<TestResult>)> =
        HashMap::new();
    for line in read_to_string(path).unwrap().lines() {
        match fillers::parse_result(line) {
            None => continue,
            Some((full_name, result)) => {
                result_map.insert(full_name, (result, None));
            }
        }
    }
    result_map
}

fn generate_diff(
    output_file: &mut File,
    diff_result_map: &HashMap<String, (TestResult, Option<TestResult>)>,
) {
    let mut empty = true;

    for (test_case, (former_res, new_res)) in diff_result_map.iter() {
        match new_res {
            None => {
                empty = false;
                writeln!(output_file, "{}: UNPROCESSED", test_case).unwrap()
            }
            Some(result) if former_res != result => {
                empty = false;
                writeln!(
                    output_file,
                    "{}: {:?} -> {:?}",
                    test_case, former_res, result
                )
                .unwrap()
            }
            _ => continue,
        }
    }

    if empty {
        writeln!(output_file, "None of the test evaluation was changed.").unwrap()
    }
}

pub fn check_skip(test_file_path: &Path) -> bool {
    let file_name = test_file_path.file_name().unwrap().to_str().unwrap();

    // Reason: panicked at 'attempt to add with overflow'
    if file_name.contains("DiffPlaces.json") {
        return true;
    }

    matches!(
        file_name,
        // Reason: chainId is tested for ethereum mainnet (1) not for etherlink (1337)
        | "chainId.json"

        // The following test(s) is/are failing they need in depth debugging
        // Reason: memory allocation of X bytes failed | 73289 IOT instruction (core dumped)
        | "sha3.json"

        // Long tests ✔ (passing)
        | "loopMul.json"

        // Oddly long checks on a test that do no relevant check (passing)
        | "intrinsic.json"

        // Long tests ~ (outcome is unknown)
        | "static_Call50000_sha256.json"
        | "static_Call50000_ecrec.json"
        | "static_Call50000.json"

        // Reason: panicked at 'attempt to multiply with overflow'
        | "static_Call1024BalanceTooLow.json"
        | "static_Call1024BalanceTooLow2.json"
        | "static_Call1024PreCalls3.json"

        // Reason: panicked at 'attempt to add with overflow'
        | "static_Call1024PreCalls.json"
        | "static_Call1024PreCalls2.json"
        | "diffPlaces.json"

        // The following test(s) is/are failing they need in depth debugging
        // Reason: panicked at 'arithmetic operation overflow'
        | "HighGasPrice.json"
        | "randomStatetest32.json"
        | "randomStatetest7.json"
        | "randomStatetest50.json"
        | "randomStatetest468.json"
        | "underflowTest.json"
        | "randomStatetest384.json"
        | "201503110226PYTHON_DUP6.json"

        // Reason: this test rely on hot/cold access and as of right now
        // this feature will not be part of Etherlink
        | "sloadGasCost.json"
    )
}

pub fn check_skip_parsing(test_file_path: &Path) -> bool {
    let file_name = test_file_path.file_name().unwrap().to_str().unwrap();

    matches!(
        file_name,
        // SKIPPED BECAUSE OF WRONG PARSING OF FILLER FILES

        // ********** JSON ********** //

        // The following test(s) is/are failing they need in depth debugging
        // Reason: panicked at 'Invalid character 'N' at position 62'
        | "gasCostBerlin.json"

        // Reason: comments in the result field
        | "static_CREATE_EmptyContractAndCallIt_0wei.json"
        | "static_CREATE_EmptyContractWithStorageAndCallIt_0wei.json"
        | "callToNonExistent.json"
        | "CreateAndGasInsideCreate.json"
        | "add11.json"

        // Reason: inconsistent hex/dec field value
        | "TouchToEmptyAccountRevert.json"
        | "CREATE_EContract_ThenCALLToNonExistentAcc.json"
        | "CREATE_EmptyContract.json"
        | "StoreGasOnCreate.json"
        | "OverflowGasRequire2.json"
        | "StackDepthLimitSEC.json"

        // ********** YAML ********** //

        // Reason: invalid hex character: _
        | "doubleSelfdestructTest.json"
        | "clearReturnBuffer.json"
        | "gasCost.json"

        // Reason: invalid length 0, expected a (both 0x-prefixed or not) hex string or
        // byte array containing between (0; 32] bytes
        | "ZeroValue_SUICIDE_ToOneStorageKey.json"
        | "ValueOverflow.json"

        // Reason: invalid length 0, expected a (both 0x-prefixed or not) hex string or
        // byte array containing between (0; 32] bytes
        | "eqNonConst.json"
        | "mulmodNonConst.json"
        | "addmodNonConst.json"
        | "smodNonConst.json"
        | "callcodeNonConst.json"
        | "mstoreNonConst.json"
        | "modNonConst.json"
        | "extcodesizeNonConst.json"
        | "log1NonConst.json"
        | "extcodecopyNonConst.json"
        | "log2NonConst.json"
        | "andNonConst.json"
        | "log3NonConst.json"
        | "sgtNonConst.json"
        | "expNonConst.json"
        | "mloadNonConst.json"
        | "log0NonConst.json"
        | "byteNonConst.json"
        | "orNonConst.json"
        | "codecopyNonConst.json"
        | "gtNonConst.json"
        | "signextNonConst.json"
        | "ltNonConst.json"
        | "sltNonConst.json"
        | "balanceNonConst.json"
        | "mstore8NonConst.json"
        | "delegatecallNonConst.json"
        | "iszeroNonConst.json"
        | "subNonConst.json"
        | "calldatacopyNonConst.json"
        | "sha3NonConst.json"
        | "sdivNonConst.json"
        | "addNonConst.json"
        | "notNonConst.json"
        | "createNonConst.json"
        | "xorNonConst.json"
        | "calldataloadNonConst.json"
        | "divNonConst.json"
        | "returnNonConst.json"
        | "mulNonConst.json"
        | "callNonConst.json"
        | "twoOps.json"
    )
}

fn process_skip(
    output: &OutputOptions,
    output_file: &mut File,
    report_map: &mut ReportMap,
    report_key: &str,
) {
    report_map
        .entry(report_key.to_owned())
        .and_modify(|report_value| {
            *report_value = ReportValue {
                skipped: report_value.skipped + 1,
                ..*report_value
            };
        });
    if output.log {
        writeln!(output_file, "\nSKIPPED\n").unwrap()
    };
}

pub fn main() {
    let opt = Opt::from_args();
    let diff = opt.diff.is_some();
    let output_name = match (opt.output.as_ref(), opt.result, diff) {
        (Some(name), _, _) => name,
        (_, true, _) => "evm_evaluation.result",
        (_, _, true) => "evm_evaluation.diff",
        _ => "evm_evaluation.regression",
    };

    let mut output_file = OpenOptions::new()
        .write(true)
        .append(!(opt.from_scratch || opt.result || diff))
        .truncate(opt.from_scratch || opt.result || diff)
        .create(true)
        .open(output_name)
        .unwrap();
    let folder_path =
        construct_folder_path("GeneralStateTests", &opt.eth_tests, &opt.sub_dir);
    let test_files = find_all_json_tests(&folder_path);
    let mut report_map: ReportMap = HashMap::new();
    let mut diff_result_map = opt.diff.as_ref().map(|p| load_former_result(p));

    let output = OutputOptions {
        summary: !(opt.result || diff),
        log: !(opt.report_only || opt.result || diff),
        result: opt.result,
        diff,
    };

    if output.log {
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
            if test_file.file_name() != Some(OsStr::new(&file_name)) {
                continue;
            }
        };

        if output.log {
            writeln!(output_file, "---------- Test: {:?} ----------", &test_file)
                .unwrap();
        }

        if check_skip_parsing(&test_file) {
            process_skip(&output, &mut output_file, &mut report_map, report_key);
            continue;
        }

        let skip = if check_skip(&test_file) {
            process_skip(&output, &mut output_file, &mut report_map, report_key);
            true
        } else {
            false
        };

        runner::run_test(
            &test_file,
            &mut report_map,
            report_key.to_owned(),
            &opt,
            &mut output_file,
            skip,
            &mut diff_result_map,
            &output,
        )
        .unwrap();
    }

    if output.log {
        writeln!(output_file, "@@@@@ END OF TESTING @@@@@\n").unwrap();
    }

    if let Some(map) = diff_result_map {
        generate_diff(&mut output_file, &map)
    }

    if output.summary {
        generate_final_report(&mut output_file, &mut report_map);
    }
}
