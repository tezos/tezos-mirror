/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::env;
use std::fs::read_to_string;

use mir::parser::Parser;
use mir::tzt::*;

fn run_test(file: &str) -> Result<(), String> {
    let contents = read_to_string(file).map_err(|e| e.to_string())?;
    let parser = Parser::new();
    let tzt_test = parser
        .parse_tzt_test(&contents)
        .map_err(|e| e.to_string())?;

    run_tzt_test(tzt_test).map_err(|e| format!("{}", e))
}

fn main() {
    // Read the cmd line arguments as a list of Strings.
    // First one is the name of the file being executed
    // and the rest are the actual arguments, so drop the first one.
    let test_files = &env::args().collect::<Vec<String>>()[1..];

    // Walk through all the test paths and execute each of them.
    // Print the result for each run.
    let mut exit_code = 0;
    for test in test_files {
        print!("Running {} : ", test);
        match run_test(test) {
            Ok(_) => println!("Ok"),
            Err(e) => {
                exit_code = 1;
                println!("{}", e);
            }
        }
    }
    std::process::exit(exit_code)
}

#[cfg(test)]
mod tztrunner_tests {
    use std::error::Error;

    use mir::{parser::Parser, tzt::*};
    use TztTestError::*;

    fn parse_tzt_test(s: &'static str) -> Result<TztTest, Box<dyn Error>> {
        let parser = Box::leak(Box::new(Parser::new()));
        parser.parse_tzt_test(s)
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
            Err(ExpectedDifferentError(_, _))
        ));
    }

    #[test]
    fn test_runner_implicit_parameter() {
        let tzt_test = parse_tzt_test(TZT_SAMPLE_IMPLICIT_PARAMETER).unwrap();
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
        output (MutezOverflow 9223372036854775807 1)"#;

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
      output { Stack_elt (contract unit) "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi" }
    "#;
}
