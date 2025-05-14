// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use core::panic;
use std::fs::DirEntry;
use std::process::Command;

use lazy_static::lazy_static;
use octez_riscv::parser::instruction::Instr;
use octez_riscv::parser::parse_block;

fn compute_offset(src: &str, dest: &str) -> i64 {
    i64::from_str_radix(dest, 16).unwrap() - i64::from_str_radix(src, 16).unwrap()
}

/// For most instructions, the objdump output can be directly compared with
/// the pretty-printed parsed instructions. For branching instructions, objdump
/// prints the computed branching address, rather than the offset, which is what
/// these instructions actually encode in their immediate field. We also replace CSR numeric
/// identifiers with their symbolic name equivalent.
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
        "lr.w" | "lr.w.aq" | "lr.w.rl" | "lr.w.aqrl" | "lr.d" | "lr.d.aq" | "lr.d.rl"
        | "lr.d.aqrl" => {
            let args = args.replace(",(", ",zero,(");
            format!("{} {}", op, args)
        }
        "c.j" => {
            let offset = compute_offset(address, args);
            format!("{} {}", op, offset)
        }
        "c.addi" => {
            let mut args = args.split(',');
            let rd_rs1 = args.next().unwrap();
            let imm = args.next().unwrap();
            match (rd_rs1, imm) {
                // objdump seems to treat `c.nop` as a pseudoinstruction, but,
                // unlike `nop`, the spec defines it as an instruction
                ("zero", "0") => "c.nop".to_string(),
                _ => format!("{} {},{}", op, rd_rs1, imm),
            }
        }
        "c.beqz" | "c.bnez" => {
            let mut args = args.split(',');
            let rs1 = args.next().unwrap();
            let branch_address = args.next().unwrap();
            let offset = compute_offset(address, branch_address);
            format!("{} {},{}", op, rs1, offset)
        }
        instr if instr.starts_with("csr") => {
            let mut args = args.split(',').collect::<Vec<_>>();

            let name_replacement = try_blocks::try_block! {
                let &second_arg = args.get(1)?;

                // Translate numeric CSR names to symbolic ones
                match second_arg {
                    "0x740" => "mnscratch",
                    "0x741" => "mnepc",
                    "0x742" => "mncause",
                    "0x744" => "mnstatus",
                    _ => None?,
                }
            };

            // Replace the second argument with the symbolic name if one was found
            if let Some(name) = name_replacement {
                args[1] = name;
            }

            let args = args.join(",");
            format!("{} {}", op, args)
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
        for exe in ["riscv64-unknown-linux-musl-objdump", "riscv64-unknown-linux-gnu-objdump", "riscv64-linux-gnu-objdump", "riscv64-unknown-elf-objdump", "riscv64-elf-objdump", "objdump"] {
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

const OBJDUMP_OPTS: [&str; 3] = ["--disassemble", "-M", "no-aliases"];

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

/// List of uncompressed instructions that have a HINT encoding associated. Compressed instructions
/// are not included as there are no HINT encodings for them in the objdump.
const HINT_INSTR_LIST: [&str; 35] = [
    "lui", "auipc", "addi", "andi", "ori", "xori", "add", "sub", "and", "or", "xor", "sll", "srl",
    "sra", "slti", "sltiu", "slli", "srli", "srai", "slt", "sltu", "addiw", "addw", "subw", "sllw",
    "srlw", "sraw", "slti", "sltiu", "slli", "srli", "srai", "slliw", "srliw", "sraiw",
];

fn check_hint_instr(objdump_instr: String) {
    // objdump will decode a HINT instruction as `<opcode> zero, ...`. In order to check that it is
    // a valid HINT, we ensure the opcode is one containing HINTs and that the first argument (rd)
    // is zero.
    let mut opcode_rd = objdump_instr.split(',').next().unwrap().split(' ');
    let opcode = opcode_rd.next().unwrap();
    let rd = opcode_rd.next().unwrap();
    if HINT_INSTR_LIST.contains(&opcode) && rd == "zero" {
        return;
    }
    panic!("Instruction incorrectly parsed as HINT: {}", objdump_instr);
}

fn check_instructions(fname: &str, instructions: Vec<(String, Instr, String)>) {
    use octez_riscv::parser::instruction::InstrCacheable;
    for (address, parsed_instr, objdump_instr) in instructions {
        if let Instr::Cacheable(InstrCacheable::Hint { instr: _ }) = parsed_instr {
            check_hint_instr(objdump_instr);
            continue;
        }
        let printed_instr = parsed_instr.to_string();
        if objdump_instr.starts_with('.') || objdump_instr == "unimp" || objdump_instr == "c.unimp"
        {
            continue;
        }
        assert_eq!(
            printed_instr, objdump_instr,
            "{} at address {}",
            fname, address
        );
    }
}

fn should_skip_rv_test(file: &DirEntry) -> bool {
    let name = file.file_name();
    let name_str = name.to_str().unwrap();

    // Conditions to exclude the test
    let not_rv = !name_str.starts_with("rv");
    let dir = file.path().is_dir();
    // Skip zfh tests: extension not implemented
    let zfh = name_str.starts_with("rv64uzfh");
    not_rv || dir || zfh
}

#[test]
fn parser_riscv_test_suite() {
    let tests_dir = "../assets/generated";

    for f in std::fs::read_dir(tests_dir).unwrap() {
        let file = f.unwrap();

        if should_skip_rv_test(&file) {
            // Skip directories, zfh tests, extraneous files
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
    // `objdump -d -M no-aliases jstz/target/riscv64gc-unknown-linux-musl/release/jstz > tests/jstz_objdump`
    let fname = "../tests/jstz_objdump";
    let instructions = objdump(fname, true);
    check_instructions(fname, instructions)
}
