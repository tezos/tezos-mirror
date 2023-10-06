/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::env;
use std::fs::read_to_string;

use mir::tzt::*;

fn run_test(file: &str) -> Result<(), String> {
    let contents = read_to_string(file).map_err(|e| e.to_string())?;
    let tzt_test = parse_tzt_test(&contents).map_err(|e| e.to_string())?;

    run_tzt_test(tzt_test).map_err(|e| format!("{:?}", e))
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
