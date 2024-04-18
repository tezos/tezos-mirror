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

#[derive(Error, Debug)]
/// Errors obtained as a result of http connection errors.
pub enum RpcError {
    /// Errors when handling stream connections
    #[error("Stream error {0}")]
    StreamError(hyper::Error),
    /// Errors when handling HTTP connections
    #[error("Connection error {0}")]
    ConnectionError(hyper::http::Error),
}

impl From<hyper::Error> for RpcError {
    fn from(error: hyper::Error) -> Self {
        RpcError::StreamError(error)
    }
}

impl From<hyper::http::Error> for RpcError {
    fn from(error: hyper::http::Error) -> Self {
        RpcError::ConnectionError(error)
    }
}
