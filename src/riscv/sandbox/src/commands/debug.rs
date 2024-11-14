// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{cli::DebugOptions, posix_exit_mode};
use octez_riscv::{
    machine_state::bus::main_memory::M1G,
    stepper::{pvm::PvmStepper, test::TestStepper},
};
use std::{error::Error, fs};
use tezos_smart_rollup::utils::inbox::InboxBuilder;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;

mod debugger_app;
mod errors;
mod tui;

pub fn debug(opts: DebugOptions) -> Result<(), Box<dyn Error>> {
    let fname = opts
        .input
        .file_name()
        .ok_or("Invalid program path")?
        .to_str()
        .ok_or("File name cannot be converted to string")?;
    let program = fs::read(&opts.input)?;
    let initrd = opts.initrd.as_ref().map(fs::read).transpose()?;

    if opts.common.pvm {
        debug_pvm(fname, program.as_slice(), initrd.as_deref(), &opts)
    } else {
        debug_test(fname, program.as_slice(), initrd.as_deref(), &opts)
    }
}

fn debug_test(
    fname: &str,
    program: &[u8],
    initrd: Option<&[u8]>,
    opts: &DebugOptions,
) -> Result<(), Box<dyn Error>> {
    Ok(debugger_app::DebuggerApp::<'_, TestStepper<M1G>>::launch(
        fname,
        program,
        initrd,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?)
}

fn debug_pvm(
    fname: &str,
    program: &[u8],
    initrd: Option<&[u8]>,
    opts: &DebugOptions,
) -> Result<(), Box<dyn Error>> {
    let mut inbox = InboxBuilder::new();
    if let Some(inbox_file) = &opts.common.inbox.file {
        inbox.load_from_file(inbox_file)?;
    }

    let rollup_address = SmartRollupAddress::from_b58check(opts.common.inbox.address.as_str())?;

    Ok(
        debugger_app::DebuggerApp::<'_, PvmStepper<'_, M1G>>::launch(
            fname,
            program,
            initrd,
            inbox.build(),
            rollup_address.into_hash().as_ref().try_into().unwrap(),
            opts.common.inbox.origination_level,
        )?,
    )
}
