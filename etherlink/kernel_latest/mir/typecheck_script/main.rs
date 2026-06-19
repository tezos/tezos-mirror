// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::env;
use std::fs::read_to_string;
use std::path::Path;
use std::process;

use mir::ast::ContractScript;
use mir::gas::Gas;
use mir::parser::Parser;

fn typecheck_script(file: &str) -> Result<(), String> {
    let contents = read_to_string(file).map_err(|e| e.to_string())?;
    let parser = Parser::new();
    let script: mir::ast::Micheline<'_> = parser
        .parse_top_level(&contents)
        .map_err(|e| e.to_string())?;
    let script = script.split_script().map_err(|e| format!("{e}"))?;
    let mut gas = Gas::default();
    let allow_lazy_storage_in_storage = true;
    let typecheck_views = true;
    let _: ContractScript<'_> = script
        .typecheck_script(&mut gas, allow_lazy_storage_in_storage, typecheck_views)
        .map_err(|e| format!("{e}"))?;
    Ok(())
}

fn main() {
    // Collect command-line arguments, skipping the first one (the executable name)
    let args: Vec<String> = env::args().collect();
    let script_files = &args[1..];

    if script_files.is_empty() {
        eprintln!("No script files provided.");
        process::exit(1);
    }

    // Initialize a counter for failed scripts
    let mut failed_scripts = 0;

    for script in script_files {
        let short_script = match Path::new(script).file_name() {
            Some(name) => name.to_string_lossy().to_string(),
            None => {
                eprintln!("Failed to get file name for '{script}'");
                process::exit(1);
            }
        };

        print!("Running {short_script}: ");
        match typecheck_script(script) {
            Ok(_) => {
                println!("Well typed");
            }
            Err(e) => {
                println!("Ill-typed: {e}");
                failed_scripts += 1;
            }
        }
    }

    let total_scripts = script_files.len();
    println!(
        "Results: Well-typed: {} Ill-typed: {} Total: {}",
        total_scripts - failed_scripts,
        failed_scripts,
        total_scripts
    )
}
