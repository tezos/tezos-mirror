// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::num::ParseIntError;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum CliError {
    #[error("Invalid argument: Duration should be a positive number of milliseconds: {0}")]
    InvalidDuration(ParseIntError),
}
