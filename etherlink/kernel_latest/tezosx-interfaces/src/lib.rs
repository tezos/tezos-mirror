// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use tezos_evm_logging::Logging;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use thiserror::Error;

/// Header names for the Tezos cross-runtime execution context.
///
/// These headers are injected by the gateway precompile so that the
/// target runtime's `serve` implementation can recover the call context
/// (sender, source, amount, gas, and block info).
pub const X_TEZOS_SENDER: &str = "X-Tezos-Sender";
pub const X_TEZOS_SOURCE: &str = "X-Tezos-Source";
pub const X_TEZOS_AMOUNT: &str = "X-Tezos-Amount";
pub const X_TEZOS_GAS_LIMIT: &str = "X-Tezos-Gas-Limit";
pub const X_TEZOS_TIMESTAMP: &str = "X-Tezos-Timestamp";
pub const X_TEZOS_BLOCK_NUMBER: &str = "X-Tezos-Block-Number";

/// Error message emitted when a user supplies an X-Tezos-* header that must
/// only be injected by the trusted gateway.
pub const ERR_FORBIDDEN_TEZOS_HEADER: &str =
    "user-supplied X-Tezos-* headers are forbidden";

/// Context shared across runtimes for cross-runtime operations.
#[derive(Clone, Debug)]
pub struct CrossRuntimeContext {
    /// block gas limit
    pub gas_limit: u64,
    /// Timestamp for the block
    pub timestamp: U256,
    /// Block number
    pub block_number: U256,
}

#[derive(Debug)]
pub enum CrossCallResult {
    /// execution completed normally, return data is the output
    Success(Vec<u8>),
    /// execution explicitly reverted (Solidity revert() / require()), return data is the error message/selector. Gas not consumed is refunded. State changes are rolled back.
    Revert(Vec<u8>),
    /// execution stopped abnormally (out of gas, invalid opcode, stack overflow, etc.). All gas is consumed. State changes are rolled back.
    Halt(Vec<u8>),
}

// TODO: L2-971
// cleanup this, and remove use of Custom for more specific errors
#[derive(Eq, PartialEq, Debug, Error)]
pub enum TezosXRuntimeError {
    #[error("RuntimeId not in registry")]
    RuntimeNotFound(RuntimeId),
    #[error("Conversion error: {0}")]
    ConversionError(String),
    #[error("Runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("Storage error: {0}")]
    Storage(#[from] tezos_storage::error::Error),
    #[error("Path error: {0}")]
    Path(#[from] tezos_smart_rollup_host::path::PathError),
    #[error("Custom error: {0}")]
    Custom(String),
    /// The request is malformed (invalid URL, missing/invalid headers, bad
    /// encoding). Maps to HTTP 400.
    #[error("Bad request: {0}")]
    BadRequest(String),
    /// The target address was not found. Maps to HTTP 404.
    #[error("Not found: {0}")]
    NotFound(String),
    /// An X-Tezos-* header is missing or contains an invalid value.
    /// Indicates a gateway bug; propagates as Err and reverts the blueprint.
    #[error("Header error: {0}")]
    HeaderError(String),
}
pub trait Registry {
    #[allow(clippy::too_many_arguments)]
    fn bridge<Host>(
        &self,
        host: &mut Host,
        destination_runtime: RuntimeId,
        destination_address: &[u8],
        source_address: &[u8],
        amount: U256,
        data: &[u8],
        context: CrossRuntimeContext,
    ) -> Result<CrossCallResult, TezosXRuntimeError>
    where
        Host: StorageV1 + Logging;

    fn generate_alias<Host>(
        &self,
        host: &mut Host,
        native_address: &[u8],
        runtime_id: RuntimeId,
        context: CrossRuntimeContext,
    ) -> Result<Vec<u8>, TezosXRuntimeError>
    where
        Host: StorageV1 + Logging;

    fn address_from_string(
        &self,
        address_str: &str,
        runtime_id: RuntimeId,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;

    /// Route an HTTP request to the appropriate runtime based on the URL host.
    fn serve<Host>(
        &self,
        host: &mut Host,
        request: http::Request<Vec<u8>>,
    ) -> Result<http::Response<Vec<u8>>, TezosXRuntimeError>
    where
        Host: StorageV1 + Logging;
}

pub trait RuntimeInterface {
    /// Generate an alias address for a native address in this runtime.
    ///
    /// This function creates an alias that allows the native address to receive
    /// funds in this runtime and have them forwarded to the original address.
    fn generate_alias<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        native_address: &[u8],
        context: CrossRuntimeContext,
    ) -> Result<Vec<u8>, TezosXRuntimeError>
    where
        Host: StorageV1 + Logging;

    /// Execute a cross-runtime call into this runtime.
    ///
    /// The caller's balance has already been debited by the source runtime
    /// before this function is called.
    ///
    /// `from` is the caller's **alias** in this (destination) runtime, not the
    /// native address from the source runtime. The alias is created by
    /// `generate_alias` and resolved by the registry before being passed here.
    /// For example, when an EVM address calls into the Tezos runtime, `from`
    /// contains the binary-encoded KT1 alias of that EVM address.
    ///
    /// `to` is the destination address, encoded in this runtime's native
    /// format.
    #[allow(clippy::too_many_arguments)]
    fn call<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        from: &[u8],
        to: &[u8],
        amount: U256,
        data: &[u8],
        context: CrossRuntimeContext,
    ) -> Result<CrossCallResult, TezosXRuntimeError>
    where
        Host: StorageV1 + Logging;

    /// Handle an incoming cross-runtime HTTP request.
    ///
    /// The request URL encodes the destination address and optional entrypoint.
    /// The body contains the payload in the target runtime's native encoding.
    ///
    /// All call context is carried in HTTP headers — there is no separate
    /// context parameter.
    ///
    /// Returns an HTTP response with a status code indicating success (200) or
    /// failure (4xx/5xx), along with runtime-specific response headers and body.
    fn serve<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        request: http::Request<Vec<u8>>,
    ) -> Result<http::Response<Vec<u8>>, TezosXRuntimeError>
    where
        Host: StorageV1 + Logging;

    /// The URL host that identifies this runtime in HTTP requests routed
    /// by the registry (e.g. `"tezos"`, `"ethereum"`).
    fn host(&self) -> &'static str;

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;

    #[cfg(feature = "testing")]
    fn string_from_address(&self, address: &[u8]) -> Result<String, TezosXRuntimeError>;

    #[cfg(feature = "testing")]
    fn get_balance(
        &self,
        host: &mut impl StorageV1,
        address: &[u8],
    ) -> Result<U256, TezosXRuntimeError>;
}

#[derive(Eq, PartialEq, Hash, Debug)]
pub enum RuntimeId {
    Tezos = 0,
    Ethereum = 1,
}

impl From<RuntimeId> for u8 {
    fn from(value: RuntimeId) -> Self {
        match value {
            RuntimeId::Tezos => 0,
            RuntimeId::Ethereum => 1,
        }
    }
}

impl TryFrom<u8> for RuntimeId {
    type Error = &'static str;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(RuntimeId::Tezos),
            1 => Ok(RuntimeId::Ethereum),
            _ => Err("Invalid RuntimeId value"),
        }
    }
}
