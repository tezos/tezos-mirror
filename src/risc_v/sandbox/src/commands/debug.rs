// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{cli::DebugOptions, posix_exit_mode};
use std::{error::Error, path::Path};

mod debugger_app;
mod errors;
mod tui;

pub fn debug(opts: DebugOptions) -> Result<(), Box<dyn Error>> {
    let path = Path::new(&opts.common.input);
    let fname = path
        .file_name()
        .ok_or("Invalid program path")?
        .to_str()
        .ok_or("File name cannot be converted to string")?;
    let contents = std::fs::read(path)?;
    Ok(debugger_app::DebuggerApp::launch(
        fname,
        &contents,
        posix_exit_mode(&opts.common.posix_exit_mode),
    )?)
}
