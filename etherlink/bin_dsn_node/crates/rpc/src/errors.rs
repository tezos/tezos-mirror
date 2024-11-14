// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use thiserror::Error;

#[derive(Error, Debug)]
/// Errors obtained as a result of http connection errors.
pub enum RpcError {
    /// Errors when handling stream connections
    #[error("Stream error {0}")]
    StreamError(#[from] hyper::Error),
    /// Errors when handling HTTP connections
    #[error("Connection error {0}")]
    ConnectionError(#[from] hyper::http::Error),
    /// The server cannot bind to the host address
    #[error("Failed to bind: {0}")]
    TcpListenerError(#[source] std::io::Error),
}
