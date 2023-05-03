// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod commands;
mod installer;
mod output;
mod preimages;

use clap::Parser;
use commands::Cli;
use commands::Commands;
use std::path::Path;
use tezos_smart_rollup_installer::{KERNEL_BOOT_PATH, PREPARE_KERNEL_PATH};
use tezos_smart_rollup_installer_config::bin::ConfigProgram;
use tezos_smart_rollup_installer_config::instr::ConfigInstruction;
use thiserror::Error;

fn main() -> Result<(), ClientError> {
    match Cli::parse().command {
        Commands::GetRevealInstaller {
            upgrade_to,
            output,
            preimages_dir,
        } => {
            let upgrade_to = Path::new(&upgrade_to);
            let output = Path::new(&output);
            let preimages_dir = Path::new(&preimages_dir);

            let root_hash = preimages::content_to_preimages(upgrade_to, preimages_dir)?;

            let kernel = installer::with_config_program(ConfigProgram(vec![
                ConfigInstruction::reveal_instr(root_hash.as_ref(), PREPARE_KERNEL_PATH),
                ConfigInstruction::move_instr(PREPARE_KERNEL_PATH, KERNEL_BOOT_PATH),
            ]));

            output::save_kernel(output, &kernel).map_err(ClientError::SaveInstaller)?;
        }
    }

    Ok(())
}

#[derive(Debug, Error)]
enum ClientError {
    #[error("Error preimaging kernel: {0}")]
    KernelPreimageError(#[from] preimages::Error),
    #[error("Unable to save installer kernel: {0}")]
    SaveInstaller(std::io::Error),
}
