// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use clap::{Parser, Subcommand};
use std::ffi::OsString;

#[derive(Parser)]
#[command(long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    GetRevealInstaller {
        #[arg(short, long, value_name = "UPGRADE_TO_KERNEL")]
        upgrade_to: OsString,

        #[arg(short, long, value_name = "INSTALLER_OUTPUT_FILE")]
        output: OsString,

        #[arg(short = 'P', long, value_name = "PREIMAGES_OUTPUT_DIR")]
        preimages_dir: OsString,

        #[arg(short = 'S', long, value_name = "INSTALLER_SETUP_CONFIG")]
        setup_file: Option<OsString>,

        #[arg(short, long, value_name = "DISPLAY_ROOT_HASH")]
        display_root_hash: bool,
    },
    MergeSetupFiles {
        #[arg(short, long, value_name = "OUTPUT_SETUP_FILE")]
        output: OsString,

        #[arg(short = 'S', long, value_name = "INSTALLER_SETUP_CONFIGS", num_args = 1.., required = true)]
        setup_files: Vec<OsString>,
    },
}
