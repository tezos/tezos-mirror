// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::{ffi::OsString, path::Path};

use std::fs::File;
use tezos_smart_rollup_encoding::dac::PreimageHash;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
use tezos_smart_rollup_installer_config::binary::owned::{
    OwnedConfigInstruction, OwnedConfigProgram,
};
use tezos_smart_rollup_installer_config::yaml::{ConfigConversionError, YamlConfig};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ConfigurationError {
    #[error("Unable to read config file: {0}.")]
    FileNotFound(std::io::Error),
    #[error("Unable to parse config file: {0}.")]
    ParseError(serde_yaml::Error),
    #[error("Unable to serialize yaml file: {0}.")]
    SerializeError(serde_yaml::Error),
    #[error("Unable to convert config to a valid program: {0}.")]
    YamlConfigInvalid(#[from] ConfigConversionError),
}

// Path that we write the kernel to, before upgrading.
const PREPARE_KERNEL_PATH: RefPath = RefPath::assert_from(b"/installer/kernel/boot.wasm");

// Path of currently running kernel.
const KERNEL_BOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/boot.wasm");

fn setup_file_to_config(setup_file: &OsString) -> Result<YamlConfig, ConfigurationError> {
    let setup_file =
        File::open(Path::new(&setup_file)).map_err(ConfigurationError::FileNotFound)?;
    let yaml_config: YamlConfig =
        YamlConfig::from_reader(setup_file).map_err(ConfigurationError::ParseError)?;
    Ok(yaml_config)
}

pub fn merge_install_configs(
    setup_files: Vec<OsString>,
) -> Result<String, ConfigurationError> {
    let mut merged_yaml_instructions = vec![];
    for setup_file in setup_files {
        let yaml = setup_file_to_config(&setup_file)?;
        merged_yaml_instructions.extend(yaml.instructions);
    }

    let merged_yaml_config = YamlConfig {
        instructions: merged_yaml_instructions,
    };
    serde_yaml::to_string(&merged_yaml_config).map_err(ConfigurationError::SerializeError)
}

pub fn create_installer_config(
    root_hash: PreimageHash,
    setup_file: Option<OsString>,
    preimages_dir: Option<&Path>,
) -> Result<OwnedConfigProgram, ConfigurationError> {
    let mut reveal_instructions = vec![
        OwnedConfigInstruction::reveal_instr(
            root_hash,
            OwnedPath::from(PREPARE_KERNEL_PATH),
        ),
        OwnedConfigInstruction::move_instr(
            OwnedPath::from(PREPARE_KERNEL_PATH),
            OwnedPath::from(KERNEL_BOOT_PATH),
        ),
    ];

    let content_to_config = |content: Vec<u8>| {
        crate::preimages::content_to_preimages(content, preimages_dir.unwrap()).ok()
    };

    let setup_program: OwnedConfigProgram = match setup_file {
        None => OwnedConfigProgram(vec![]),
        Some(setup_file) => {
            let yaml_config = setup_file_to_config(&setup_file)?;
            yaml_config.to_config_program(content_to_config)?
        }
    };

    reveal_instructions.extend(setup_program.0);

    Ok(OwnedConfigProgram(reveal_instructions))
}
