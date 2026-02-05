// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Installer kernel for tezos smart rollups.
//!
//! # About
//!
//! When originating a smart rollup, you must supply a kernel - the program to be executed
//! by the rollup. This origination kernel must fit within the size of a Layer-1 operation
//! (about 32KB).
//!
//! Almost all useful kernels are larger than this, however. As a result, it is recommended
//! to use this installer kernel. When originating a rollup, you may use a configured
//! installer kernel - which will then proceed to upgrade to your desired kernel.

#![cfg_attr(all(pvm_kind = "wasm", not(feature = "std")), no_std)]
#![forbid(unsafe_code)]

mod instr;

use crate::instr::read_instruction_bytes;
use core::panic::PanicInfo;
use instr::read_config_program_size;
#[cfg(feature = "entrypoint")]
use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup::host::{HostDebug, Runtime};
use tezos_smart_rollup::storage::path::RefPath;
use tezos_smart_rollup_installer_config::binary::evaluation::eval_config_instr;
use tezos_smart_rollup_installer_config::binary::{
    completed, read_size, EncodingSize, NomReader, RefConfigInstruction,
};

// Path of currently running kernel.
const KERNEL_BOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/boot.wasm");

// Installer kernel will copy to this path before execution of config.
// This is done in order avoid rewriting kernel during config execution.
const AUXILIARY_CONFIG_INTERPRETATION_PATH: RefPath =
    RefPath::assert_from(b"/__installer_kernel/auxiliary/kernel/boot.wasm");

/// Installer.
#[cfg_attr(feature = "entrypoint", entrypoint::main)]
pub fn installer<Host: Runtime>(host: &mut Host) {
    if let Err(e) = install_kernel(host, KERNEL_BOOT_PATH) {
        HostDebug::write_debug(host, e)
    } else {
        let _ = host.mark_for_reboot();
    }
}

/// Panic handler used when targetting wasm.
///
/// Any error when installing a kernel is fatal, therefore we
/// handle panics by panicking again, which aborts execution.
#[cfg_attr(all(pvm_kind = "wasm", not(feature = "std")), panic_handler)]
#[allow(dead_code)]
fn panic(_info: &PanicInfo) -> ! {
    panic!()
}

/// Kernel installer function.
///
/// Non-trivial preliminary steps needs to be processed before calling
/// this function:
///     - Prepare preimages of the targeted kernel.
///     - Create a config program consisting of `reveal` instruction followed by a `move` one.
///     - Serialise the program and write the output to durable storage.
///     - Finally execute the config program by calling `install_kernel`, with the path the config
///       was written to.
// TODO: provide a concrete example (see https://gitlab.com/tezos/tezos/-/issues/5855)
pub fn install_kernel(
    host: &mut impl Runtime,
    config_interpretation_path: RefPath,
) -> Result<(), &'static str> {
    if let Ok(config_program_size) =
        read_config_program_size(host, &config_interpretation_path)
    {
        let mut config_instruction_buffer = [0; RefConfigInstruction::MAX_SIZE];

        let kernel_size = host
            .store_value_size(&config_interpretation_path)
            .map_err(|_| "Failed to read kernel boot path size")?;

        host.store_copy(
            &config_interpretation_path,
            &AUXILIARY_CONFIG_INTERPRETATION_PATH,
        )
        .map_err(|_| "Failed to copy kernel boot before config execution")?;

        let end_offset = kernel_size - 4;
        let mut instr_offset = end_offset - (config_program_size as usize);
        while instr_offset < end_offset {
            let instr_size = read_size(
                host,
                &AUXILIARY_CONFIG_INTERPRETATION_PATH,
                &mut instr_offset,
            )? as usize;
            read_instruction_bytes(
                host,
                &AUXILIARY_CONFIG_INTERPRETATION_PATH,
                &mut instr_offset,
                &mut config_instruction_buffer[..instr_size],
            )?;
            let instr =
                RefConfigInstruction::nom_read(&config_instruction_buffer[..instr_size])
                    .map_err(|_| "Couldn't decode config instruction")
                    .and_then(completed)?;
            eval_config_instr(host, &instr)?;
        }

        host.store_delete(&AUXILIARY_CONFIG_INTERPRETATION_PATH)
            .map_err(|_| {
                "Failed to delete auxiliary kernel boot after config execution"
            })?;

        Ok(())
    } else {
        Err("Failed to read size of config program")
    }
}
