// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use serde_json::to_writer_pretty;
use std::collections::BTreeMap;
use std::env;
use std::fs::{read_to_string, File};
use std::io::{BufReader, BufWriter, Write};
use std::path::Path;
use std::process;

use mir::parser::Parser;
use mir::tzt::*;
use typed_arena::Arena;

fn run_test(file: &str) -> Result<(), String> {
    let contents = read_to_string(file).map_err(|e| e.to_string())?;
    let parser = Parser::new();
    let tzt_test = parser
        .parse_tzt_test(&contents)
        .map_err(|e| e.to_string())?;
    let arena = Arena::new();
    run_tzt_test(tzt_test, &arena).map_err(|e| format!("{e}"))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Collect command-line arguments, skipping the first one (the executable name)
    let args: Vec<String> = env::args().collect();
    let test_files = &args[1..];

    if test_files.is_empty() {
        eprintln!("No test files provided.");
        process::exit(1);
    }

    // Initialize counters for passed and failed tests
    let mut failed_tests = 0;
    let mut results: BTreeMap<String, bool> = BTreeMap::new();

    for test in test_files {
        let short_test = match Path::new(test).file_name() {
            Some(name) => name.to_string_lossy().to_string(),
            None => {
                eprintln!("Failed to get file name for '{test}'");
                process::exit(1);
            }
        };

        print!("Running {short_test}: ");
        match run_test(test) {
            Ok(_) => {
                println!("Ok");
                results.insert(short_test, true);
            }
            Err(e) => {
                println!("{e}");
                failed_tests += 1;
                results.insert(short_test, false);
            }
        }
    }

    let total_tests = test_files.len();
    println!(
        "Test results: Passed: {} Failed: {} Total: {}",
        total_tests - failed_tests,
        failed_tests,
        total_tests
    );

    // Construct the output file path
    let executable_path = env::args()
        .next()
        .ok_or("Failed to get the executable path")?;
    let file_path_str = executable_path.replace("tzt_runner", "../../MIR- Run TZT.out");
    let current_dir = env::current_dir()?;
    let file_path = current_dir.join(&file_path_str);

    // Read old results if the file exists
    let old_results: BTreeMap<String, bool> = if file_path.exists() {
        let file = File::open(&file_path)?;
        let reader = BufReader::new(file);
        serde_json::from_reader(reader)?
    } else {
        BTreeMap::new()
    };

    // Save new results to the file
    let file = File::create(&file_path)?;
    let mut writer = BufWriter::new(file);
    to_writer_pretty(&mut writer, &results)?;
    writeln!(writer)?;
    writer.flush()?;

    // Compare old and new results
    if old_results != results {
        eprintln!("Changes in the test results file.");
        eprintln!("Run `cargo run --manifest-path path/to/mir/Cargo.toml --bin tzt_runner path/to/tzt_reference_test_suite/*.tzt` before comitting.");
        process::exit(2);
    }

    Ok(())
}

#[cfg(test)]
mod tztrunner_tests {
    use std::error::Error;

    use mir::{parser::Parser, tzt::*};
    use TztTestError::*;

    fn parse_tzt_test(s: &'static str) -> Result<TztTest<'static>, Box<dyn Error>> {
        let parser = Box::leak(Box::new(Parser::new()));
        parser.parse_tzt_test(s)
    }

    pub fn run_tzt_test(test: TztTest) -> Result<(), TztTestError> {
        let temp = Box::leak(Box::default());
        mir::tzt::run_tzt_test(test, temp)
    }

    #[test]
    fn test_runner_success() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_ADD).unwrap();
        assert!(run_tzt_test(tzt_test).is_ok());
    }

    #[test]
    fn test_runner_mismatch_stack() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_ADD_MISMATCH_STACK).unwrap();
        assert!(matches!(run_tzt_test(tzt_test), Err(StackMismatch(_, _))));
    }

    #[test]
    fn test_runner_mismatch_stack_2() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_ADD_MISMATCH_STACK_2).unwrap();
        assert!(matches!(run_tzt_test(tzt_test), Err(StackMismatch(_, _))));
    }

    #[test]
    fn test_runner_push() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_PUSH).unwrap();
        assert!(matches!(run_tzt_test(tzt_test), Ok(())));
    }

    #[test]
    fn test_runner_amount() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_AMOUNT).unwrap();
        assert!(matches!(run_tzt_test(tzt_test), Ok(())));
    }

    #[test]
    fn test_runner_tc_expectation() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_TC_FAIL).unwrap();
        assert!(matches!(run_tzt_test(tzt_test), Ok(())));
    }

    #[test]
    fn test_runner_tc_specific_expectation() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_TC_FAIL_SPECIFIC).unwrap();
        assert!(matches!(run_tzt_test(tzt_test), Ok(())));
    }

    #[should_panic(expected = "Duplicate field 'input' in test")]
    #[test]
    fn test_duplicate_field() {
        let _ = parse_tzt_test(TZT_SAMPLE_DUPLICATE_FIELD).unwrap();
    }

    #[should_panic(expected = "Duplicate field 'output' in test")]
    #[test]
    fn test_duplicate_field_output() {
        let _ = parse_tzt_test(TZT_SAMPLE_DUPLICATE_FIELD_OUTPUT).unwrap();
    }

    #[test]
    fn test_runner_interpreter_error() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_MUTEZ_OVERFLOW).unwrap();
        let result = run_tzt_test(tzt_test);
        assert!(result.is_ok());
    }

    #[test]
    fn test_runner_interpreter_unexpected_fail() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_EXP_SUCC_BUT_FAIL).unwrap();
        assert!(matches!(run_tzt_test(tzt_test), Err(UnexpectedError(_))));
    }

    #[test]
    fn test_runner_interpreter_error_diff() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_INTERPRETER_DIFF_ERROR).unwrap();
        let result = run_tzt_test(tzt_test);
        assert!(result.is_err());
    }

    #[test]
    fn test_runner_interpreter_unexpected_success() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_EXP_FAIL_BUT_SUCCEED).unwrap();
        assert!(matches!(
            run_tzt_test(tzt_test),
            Err(UnexpectedSuccess(_, _))
        ));
    }

    #[test]
    fn test_runner_interpreter_unexpected_fail_val() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_FAIL_WITH_UNEXPECTED).unwrap();
        assert!(matches!(
            run_tzt_test(tzt_test),
            Err(ExpectedDifferentError(_))
        ));
    }

    #[test]
    fn test_runner_implicit_parameter() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_IMPLICIT_PARAMETER).unwrap();
        assert_eq!(run_tzt_test(tzt_test), Ok(()));
    }

    #[test]
    fn test_runner_other_contracts() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_OTHER_CONTRACTS).unwrap();
        assert_eq!(run_tzt_test(tzt_test), Ok(()));
    }

    #[test]
    fn test_runner_self_is_known() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_SELF_IS_KNOWN).unwrap();
        assert_eq!(run_tzt_test(tzt_test), Ok(()));
    }

    #[test]
    fn test_runner_chain_id() {
        assert_eq!(
            run_tzt_test(
                parse_tzt_test(
                    r#"code { CHAIN_ID };
                    input {};
                    chain_id "NetXdQprcVkpaWU";
                    output { Stack_elt chain_id 0x7a06a770 }"#,
                )
                .unwrap()
            ),
            Ok(())
        );
        assert_eq!(
            run_tzt_test(
                parse_tzt_test(
                    r#"code { CHAIN_ID };
                    input {};
                    chain_id 0xbeeff00d;
                    output { Stack_elt chain_id 0xbeeff00d }"#,
                )
                .unwrap()
            ),
            Ok(())
        );
    }

    #[test]
    fn test_runner_self_parameter() {
        assert_eq!(
            run_tzt_test(
                parse_tzt_test(
                    r#"code { SELF };
                    input {};
                    parameter int;
                    self "KT1Wr7sqVqpbuELSD5xpTBPSCjyNRFj9Xpba";
                    output { Stack_elt (contract int) "KT1Wr7sqVqpbuELSD5xpTBPSCjyNRFj9Xpba" }"#,
                )
                .unwrap()
            ),
            Ok(())
        );
    }

    const TZT_SAMPLE_ADD: &str = "code { ADD } ;
        input { Stack_elt int 5 ; Stack_elt int 5 } ;
        output { Stack_elt int 10 }";

    const TZT_SAMPLE_ADD_MISMATCH_STACK: &str = "code { ADD } ;
        input { Stack_elt int 5 ; Stack_elt int 5 } ;
        output { Stack_elt int 11 }";

    const TZT_SAMPLE_ADD_MISMATCH_STACK_2: &str = "code {} ;
        input { Stack_elt (list int) {} } ;
        output { Stack_elt (list nat) {} }";

    const TZT_SAMPLE_PUSH: &str = "code { PUSH nat 5; PUSH int 10 } ;
        input {} ;
        output { Stack_elt int 10; Stack_elt nat 5 }";

    const TZT_SAMPLE_AMOUNT: &str = "code { AMOUNT } ;
        input {} ;
        amount 10 ;
        output { Stack_elt mutez 10;}";

    const TZT_SAMPLE_DUPLICATE_FIELD: &str = "code { ADD } ;
        input { Stack_elt int 5 ; Stack_elt int 5 } ;
        input { Stack_elt int 5 ; Stack_elt int 5 } ;
        output { Stack_elt int 10 }";

    const TZT_SAMPLE_DUPLICATE_FIELD_OUTPUT: &str = "code { ADD } ;
        input { Stack_elt int 5 ; Stack_elt int 5 } ;
        output { Stack_elt int 10 } ;
        output { Stack_elt int 10 }";

    const TZT_SAMPLE_MUTEZ_OVERFLOW: &str = r#"code { ADD } ;
        input { Stack_elt mutez 9223372036854775807 ; Stack_elt mutez 1 } ;
        output Overflow"#;

    const TZT_SAMPLE_EXP_SUCC_BUT_FAIL: &str = r#"code { ADD } ;
        input { Stack_elt mutez 9223372036854775807 ; Stack_elt mutez 1 } ;
        output { Stack_elt mutez 10 }"#;

    const TZT_SAMPLE_EXP_FAIL_BUT_SUCCEED: &str = r#"code { ADD } ;
        input { Stack_elt mutez 10 ; Stack_elt mutez 1 } ;
        output (MutezOverflow 9223372036854775807 1)"#;

    const TZT_SAMPLE_INTERPRETER_DIFF_ERROR: &str = r#"code { ADD } ;
        input { Stack_elt mutez 9223372036854775807 ; Stack_elt mutez 1 } ;
        output (StaticError _)"#;

    const TZT_SAMPLE_FAIL_WITH_UNEXPECTED: &str = r#"code { FAILWITH } ;
        input { Stack_elt int 10 ;  } ;
        output (Failed 11)"#;

    const TZT_SAMPLE_TC_FAIL: &str = "code { ADD } ;
        input { Stack_elt mutez 5 ; Stack_elt int 5 } ;
        output(StaticError _)";

    const TZT_SAMPLE_TC_FAIL_SPECIFIC: &str = r#"code { ADD } ;
        input { Stack_elt mutez 5 ; Stack_elt int 5 } ;
        output(StaticError "no matching overload for ADD on stack Stack([Int, Mutez])")"#;

    const TZT_SAMPLE_IMPLICIT_PARAMETER: &str = r#"
      code SELF;
      input {};
      self "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi";
      output { Stack_elt (contract unit) "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi" }
    "#;

    const TZT_SAMPLE_OTHER_CONTRACTS: &str = r#"
      code { CONTRACT unit } ;
      input { Stack_elt address "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d" } ;
      output { Stack_elt (option (contract unit)) (Some "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d") } ;
      other_contracts { Contract "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d" unit }
    "#;

    const TZT_SAMPLE_SELF_IS_KNOWN: &str = r#"
      code { CONTRACT unit } ;
      input { Stack_elt address "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d" } ;
      output { Stack_elt (option (contract unit)) (Some "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d") } ;
      self "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d"
    "#;
}
