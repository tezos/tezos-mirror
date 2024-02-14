// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::process::Command;

use risc_v_interpreter::parser::instruction::Instr;
use risc_v_interpreter::parser::parse_block;

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

const OBJDUMP: &str = "objdump";
const OBJDUMP_OPTS: [&str; 3] = ["--disassemble", "-M", "no-aliases,numeric"];

/// Disassemble a RISC-V binary using objdump and return a vector of tuples
/// consisting of the address of the instruction, the parsed instruction, and
/// the objdump output for the instruction.
fn objdump(file_path: &str) -> Vec<(String, Instr, String)> {
    let output = Command::new(OBJDUMP)
        .args(OBJDUMP_OPTS)
        .arg(file_path)
        .output()
        .expect("Failed to run objdump");
    let contents = std::str::from_utf8(&output.stdout).unwrap();

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
    instructions
}

/// Parse one encoded instruction
fn parse_encoded(encoded: &str) -> Instr {
    let mut bytes = hex::decode(encoded).unwrap();
    bytes.reverse();
    parse_block(&bytes)[0]
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
        let instructions = objdump(&fname);
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
}
