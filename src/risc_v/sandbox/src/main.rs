// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use cli::{DebugOptions, ExitMode, RunOptions, RvemuOptions};
use risc_v_interpreter::{
    machine_state::mode::Mode, traps::EnvironException, Interpreter, InterpreterResult,
};
use rvemu::emulator::Emulator;
use std::{error::Error, path::Path};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_smart_rollup_encoding::{
    michelson::MichelsonUnit, public_key_hash::PublicKeyHash, smart_rollup::SmartRollupAddress,
};

mod cli;
mod commands;
mod debugger;
mod devicetree;
mod inbox;
mod rvemu_boot;
mod rvemu_syscall;

fn format_status(result: &InterpreterResult) -> String {
    use InterpreterResult::*;
    match result {
        Exit { code: 0, .. } => "Ok (exit code = 0)".to_string(),
        Exit { code, .. } => format!("Failed with exit code = {}", code),
        Running(_) => "Timeout".to_string(),
        Exception(exc, _) => format!("{}", exception_to_error(exc)),
    }
}

/// Convert a RISC-V exception into an error.
pub fn exception_to_error(exc: &EnvironException) -> Box<dyn Error> {
    format!("{:?}", exc).into()
}

fn run(opts: RunOptions) -> Result<(), Box<dyn Error>> {
    let contents = std::fs::read(&opts.common.input)?;
    let mut backend = Interpreter::create_backend();
    let mut interpreter = Interpreter::new(
        &mut backend,
        &contents,
        None,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?;

    match interpreter.run(opts.common.max_steps) {
        InterpreterResult::Exit { code: 0, .. } => Ok(()),
        result => Err(format_status(&result).into()),
    }
}

fn debug(opts: DebugOptions) -> Result<(), Box<dyn Error>> {
    let path = Path::new(&opts.common.input);
    let fname = path
        .file_name()
        .ok_or("Invalid program path")?
        .to_str()
        .ok_or("File name cannot be converted to string")?;
    let contents = std::fs::read(path)?;
    Ok(debugger::DebuggerApp::launch(
        fname,
        &contents,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?)
}

fn rvemu(opts: RvemuOptions) -> Result<(), Box<dyn Error>> {
    let mut emu = Emulator::new();

    // Load the ELF binary into the emulator.
    let contents = std::fs::read(&opts.common.input)?;

    rvemu_boot::setup_boot(&mut emu, &contents, opts.initrd)?;

    // Rollup metadata
    let inbox_opt = opts.common.inbox;
    let meta = rvemu_syscall::RollupMetadata {
        origination_level: inbox_opt.origination_level,
        address: SmartRollupAddress::from_b58check(inbox_opt.address.as_str()).unwrap(),
    };

    // Prepare inbox
    let mut inbox = inbox::InboxBuilder::new();

    if let Some(inbox_file) = inbox_opt.file {
        inbox.load_from_file(&inbox_file)?;
    } else {
        inbox
            .insert_external(vec![1, 2, 3, 4])
            .insert_external(vec![1, 4, 3, 2])
            .next_level()
            .insert_external(vec![1, 1])
            .next_level()
            .insert_external(vec![1, 2])
            .next_level()
            .insert_transfer(
                ContractKt1Hash::from_base58_check("KT1EfTusMLoeCAAGd9MZJn5yKzFr6kJU5U91").unwrap(),
                PublicKeyHash::from_b58check("tz1dJ21ejKD17t7HKcKkTPuwQphgcSiehTYi").unwrap(),
                meta.address.clone(),
                MichelsonUnit,
            );
    }

    let mut inbox = inbox.build();

    let handle_syscall = if opts.posix {
        fn dummy(
            emu: &mut Emulator,
            _: &rvemu_syscall::RollupMetadata,
            _: &mut inbox::Inbox,
        ) -> Result<(), Box<dyn Error>> {
            rvemu_syscall::handle_posix(emu)
        }
        dummy
    } else {
        rvemu_syscall::handle_sbi
    };

    let mut prev_pc = emu.cpu.pc;

    while inbox.none_count() < 2 || inbox_opt.keep_going {
        emu.cpu.devices_increment();

        if let Some(interrupt) = emu.cpu.check_pending_interrupt() {
            interrupt.take_trap(&mut emu.cpu);

            // We don't do anything with the devices at the moment. So we'll
            // just panic if they magically come alive.
            panic!("Interrupt {:?}", interrupt);
        }

        emu.cpu
            .execute()
            .map(|_| ())
            .or_else(|exception| -> Result<(), Box<dyn Error>> {
                match exception {
                    rvemu::exception::Exception::EnvironmentCallFromSMode
                    | rvemu::exception::Exception::EnvironmentCallFromUMode => {
                        handle_syscall(&mut emu, &meta, &mut inbox).map_err(
                            |err| -> Box<dyn Error> {
                                format!("Failed to handle environment call at {prev_pc:x}: {}", err)
                                    .as_str()
                                    .into()
                            },
                        )?;

                        // We need to update the program counter ourselves now.
                        // This is a recent change in behaviour in RVEmu.
                        emu.cpu.pc += 4;

                        Ok(())
                    }

                    _ => {
                        let trap = exception.take_trap(&mut emu.cpu);

                        // Don't bother handling other exceptions. For now they're
                        // all fatal.
                        panic!("Exception {:?} at {:#x}: {:?}", exception, prev_pc, trap)
                    }
                }
            })?;

        // If the program loops in place we assume it is stuck.
        if prev_pc == emu.cpu.pc {
            panic!("Stuck at {:#x}", prev_pc);
        }

        prev_pc = emu.cpu.pc;
    }

    Ok(())
}

fn posix_exit_mode(exit_mode: &ExitMode) -> Mode {
    match exit_mode {
        ExitMode::User => Mode::User,
        ExitMode::Supervisor => Mode::Supervisor,
        ExitMode::Machine => Mode::Machine,
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = cli::parse();
    match cli.command {
        cli::Mode::Rvemu(opts) => rvemu(opts),
        cli::Mode::Run(opts) => run(opts),
        cli::Mode::Debug(opts) => debug(opts),
        cli::Mode::Bench(opts) => commands::bench::bench(opts),
    }
}
