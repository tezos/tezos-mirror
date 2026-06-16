// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

mod commands;
mod installer;
mod output;
mod preimages;

use clap::Parser;
use commands::Cli;
use commands::Commands;
use std::fs::write;
use std::path::Path;
use tezos_smart_rollup_installer::config::{
    create_installer_config, merge_install_configs, ConfigurationError,
};
use thiserror::Error;

fn main() -> Result<(), ClientError> {
    match Cli::parse().command {
        Commands::GetRevealInstaller {
            upgrade_to,
            output,
            preimages_dir,
            setup_file,
            display_root_hash,
        } => {
            let upgrade_to = Path::new(&upgrade_to);
            let output = Path::new(&output);
            let preimages_dir = Path::new(&preimages_dir);

            let kernel =
                std::fs::read(upgrade_to).map_err(preimages::Error::ContentFile)?;

            let root_hash = preimages::content_to_preimages(kernel, preimages_dir)?;
            let root_hash_hex = hex::encode(root_hash.as_ref());

            let config =
                create_installer_config(root_hash, setup_file, Some(preimages_dir))?;
            let kernel = installer::with_config_program(config);

            output::save_kernel(output, &kernel).map_err(ClientError::SaveInstaller)?;

            if display_root_hash {
                println!("ROOT_HASH: {root_hash_hex}");
            };
        }
        Commands::MergeSetupFiles {
            output,
            setup_files,
        } => {
            let output_path = Path::new(&output);
            let encoded_yaml_config = merge_install_configs(setup_files)?;
            write(output_path, encoded_yaml_config)
                .map_err(ClientError::SaveMergedConfig)?;
        }
    }

    Ok(())
}

#[derive(Debug, Error)]
enum ClientError {
    #[error("Error preimaging kernel: {0}")]
    KernelPreimageError(#[from] preimages::Error),
    #[error("Error configuring kernel: {0}")]
    ConfigError(#[from] ConfigurationError),
    #[error("Unable to save installer kernel: {0}")]
    SaveInstaller(std::io::Error),
    #[error("Unable to save the merged config: {0}")]
    SaveMergedConfig(std::io::Error),
}
