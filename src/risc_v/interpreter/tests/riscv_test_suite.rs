// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use lazy_static::lazy_static;
use risc_v_interpreter::parser::{instruction::Instr, parse_block};
use std::process::Command;

fn compute_offset(src: &str, dest: &str) -> i64 {
    i64::from_str_radix(dest, 16).unwrap() - i64::from_str_radix(src, 16).unwrap()
}

/// For most instructions, the objdump output can be directly compared with
/// the pretty-printed parsed instructions. For branching instructions, objdump
/// prints the computed branching address, rather than the offset, which is what
/// these instructions actually encode in their immediate field.
fn transform_objdump_instr<'a>(address: &'a str, instr: &'a str, args: &'a str) -> String {
    let op = instr.trim();
    let args = args.split(' ').next().unwrap().trim();
    match op {
        "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" => {
            let mut args = args.split(',');
            let rs1 = args.next().unwrap();
            let rs2 = args.next().unwrap();
            let branch_address = args.next().unwrap();
            let offset = compute_offset(address, branch_address);
            format!("{} {},{},{}", op, rs1, rs2, offset)
        }
        "jal" => {
            let mut args = args.split(',');
            let rd = args.next().unwrap();
            let branch_address = args.next().unwrap();
            let offset = compute_offset(address, branch_address);
            format!("{} {},{}", op, rd, offset)
        }
        _ => {
            if args.is_empty() {
                op.to_string()
            } else {
                format!("{} {}", op, args)
            }
        }
    }
}

lazy_static! {
    static ref OBJDUMP_EXE: &'static str = {
        // Iterate through the known executable names to find a suitable objdump.
        for exe in ["riscv64-unknown-linux-gnu-objdump", "riscv64-elf-objdump", "objdump"] {
            let Some(output) = Command::new(exe).arg("--version").output().ok() else {
                continue;
            };

            let stdout =
                std::str::from_utf8(output.stdout.as_slice()).expect("Invalid UTF-8 encoding");

            if stdout.contains("GNU objdump") {
                return exe;
            }
        }

        panic!("Could not find a GNU-style objdump executable")
    };
}

const OBJDUMP_OPTS: [&str; 3] = ["--disassemble", "-M", "no-aliases,numeric"];

/// Disassemble a RISC-V binary using objdump and return a vector of tuples
/// consisting of the address of the instruction, the parsed instruction, and
/// the objdump output for the instruction. When `disassembled` is true,
/// `file_path` is expected to point to a file containing the output of
/// a previous objdump run.
fn objdump(file_path: &str, disassembled: bool) -> Vec<(String, Instr, String)> {
    let contents = if disassembled {
        std::fs::read_to_string(file_path).expect("Failed to read file")
    } else {
        let output = Command::new(*OBJDUMP_EXE)
            .args(OBJDUMP_OPTS)
            .arg(file_path)
            .output()
            .expect("Failed to run objdump");
        std::str::from_utf8(&output.stdout).unwrap().to_string()
    };

    let mut instructions = Vec::new();

    for line in contents.lines().skip(4) {
        let Some((address, rest)) = line.split_once(':') else {
            continue;
        };
        let Some((encoded, raw_objdump_instr)) = rest.trim().split_once(' ') else {
            continue;
        };
        let raw_objdump_instr = raw_objdump_instr.trim();
        let (instr, args) = raw_objdump_instr
            .split_once('\t')
            .unwrap_or((raw_objdump_instr, ""));
        let address = address.trim();
        let parsed_instr = parse_encoded(encoded);
        let objdump_instr = transform_objdump_instr(address, instr, args);
        instructions.push((address.to_string(), parsed_instr, objdump_instr))
    }
    assert!(
        !instructions.is_empty(),
        "Could not extract any instructions from {}",
        file_path
    );
    instructions
}

/// Parse one encoded instruction
fn parse_encoded(encoded: &str) -> Instr {
    let mut bytes = hex::decode(encoded).unwrap();
    bytes.reverse();
    parse_block(&bytes)[0]
}

fn check_instructions(fname: &str, instructions: Vec<(String, Instr, String)>) {
    for (address, parsed_instr, objdump_instr) in instructions {
        let printed_instr = parsed_instr.to_string();
        if printed_instr.starts_with("unknown") || objdump_instr == "unimp" {
            continue;
        }
        assert_eq!(
            printed_instr, objdump_instr,
            "{} at address {}",
            fname, address
        );
    }
}

#[test]
fn parser_riscv_test_suite() {
    let tests_dir = "../../../tezt/tests/riscv-tests/generated/";

    for f in std::fs::read_dir(tests_dir).unwrap() {
        let file = f.unwrap();
        if file.path().is_dir() {
            continue;
        }
        let path = file.path();
        let fname = path.to_string_lossy();
        let instructions = objdump(&fname, false);
        check_instructions(&fname, instructions)
    }
}

#[ignore]
#[test]
fn parser_riscv_jstz() {
    // Test currently disabled in CI because running objdump on the jstz kernel
    // is slow (~15 min) and the resulting file is very large (~110MB).
    // To run locally, generate a dump on the compiled jstz kernel:
    // `objdump -d -M no-aliases,numeric jstz/target/riscv64gc-unknown-hermit/release/jstz > interpreter/tests/jstz_objdump`
    let fname = "tests/jstz_objdump";
    let instructions = objdump(fname, true);
    check_instructions(fname, instructions)
}
