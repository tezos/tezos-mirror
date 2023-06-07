// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
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
    },
}
