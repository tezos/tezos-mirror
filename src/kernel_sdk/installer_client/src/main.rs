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
            let kernel = installer::with_reveal_hash(&root_hash);

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
