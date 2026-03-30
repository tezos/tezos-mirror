// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub mod headers;

use primitive_types::U256;
use tezos_evm_logging::Logging;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_journal::TezosXJournal;
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
pub const X_TEZOS_GAS_CONSUMED: &str = "X-Tezos-Gas-Consumed";
pub const X_TEZOS_TIMESTAMP: &str = "X-Tezos-Timestamp";
pub const X_TEZOS_BLOCK_NUMBER: &str = "X-Tezos-Block-Number";
pub const X_TEZOS_CRAC_ID: &str = "X-Tezos-CRAC-ID";

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
    fn generate_alias<Host>(
        &self,
        host: &mut Host,
        journal: &mut TezosXJournal,
        native_address: &str,
        runtime_id: RuntimeId,
        context: CrossRuntimeContext,
    ) -> Result<String, TezosXRuntimeError>
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
        journal: &mut TezosXJournal,
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
        journal: &mut TezosXJournal,
        native_address: &str,
        context: CrossRuntimeContext,
    ) -> Result<String, TezosXRuntimeError>
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
        journal: &mut TezosXJournal,
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

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
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

impl RuntimeId {
    /// Parse a `RuntimeId` from the URL host string used in cross-runtime
    /// HTTP requests (e.g. `"tezos"`, `"ethereum"`).
    pub fn from_host(host: &str) -> Option<Self> {
        match host {
            "tezos" => Some(RuntimeId::Tezos),
            "ethereum" => Some(RuntimeId::Ethereum),
            _ => None,
        }
    }
}

#[cfg(test)]
mod from_host_tests {
    use super::*;

    #[test]
    fn tezos() {
        assert_eq!(RuntimeId::from_host("tezos"), Some(RuntimeId::Tezos));
    }

    #[test]
    fn ethereum() {
        assert_eq!(RuntimeId::from_host("ethereum"), Some(RuntimeId::Ethereum));
    }

    #[test]
    fn unknown_returns_none() {
        assert_eq!(RuntimeId::from_host("michelson"), None);
        assert_eq!(RuntimeId::from_host(""), None);
    }

    #[test]
    fn case_sensitive() {
        assert_eq!(RuntimeId::from_host("Tezos"), None);
        assert_eq!(RuntimeId::from_host("Ethereum"), None);
    }
}

/// Gas conversion utilities for cross-runtime calls.
///
/// **Convention**: Both `X-Tezos-Gas-Limit` and `X-Tezos-Gas-Consumed` are
/// in the **called** runtime's gas units. Gateways convert on the way out
/// (for the limit) and on the way back (for consumed); receiving runtimes
/// read/write directly without conversion.
///
/// **Ratio**: 1 EVM gas = 100 Tezos milligas.
/// **Tezos unit**: milligas (the native unit of the Tezos runtime).
pub mod gas {
    use crate::RuntimeId;

    /// Convert `gas` from `source` runtime units to `target` runtime units.
    ///
    /// Returns `None` on overflow (only possible for Ethereum→Tezos when
    /// `gas > u64::MAX / 100`). Returns `Some(gas)` unchanged when
    /// `source == target`.
    pub fn convert(source: RuntimeId, target: RuntimeId, gas: u64) -> Option<u64> {
        match (source, target) {
            (RuntimeId::Ethereum, RuntimeId::Tezos) => gas.checked_mul(100),
            (RuntimeId::Tezos, RuntimeId::Ethereum) => Some(gas / 100),
            _ => Some(gas),
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn ethereum_to_tezos() {
            assert_eq!(
                convert(RuntimeId::Ethereum, RuntimeId::Tezos, 100),
                Some(10_000)
            );
            assert_eq!(
                convert(RuntimeId::Ethereum, RuntimeId::Tezos, 1_000_000),
                Some(100_000_000)
            );
        }

        #[test]
        fn ethereum_to_tezos_overflow() {
            let large = u64::MAX / 100 + 1;
            assert_eq!(convert(RuntimeId::Ethereum, RuntimeId::Tezos, large), None);
        }

        #[test]
        fn tezos_to_ethereum() {
            assert_eq!(
                convert(RuntimeId::Tezos, RuntimeId::Ethereum, 10_000),
                Some(100)
            );
        }

        #[test]
        fn tezos_to_ethereum_truncates() {
            assert_eq!(convert(RuntimeId::Tezos, RuntimeId::Ethereum, 150), Some(1));
        }

        #[test]
        fn identity() {
            assert_eq!(
                convert(RuntimeId::Ethereum, RuntimeId::Ethereum, 42),
                Some(42)
            );
            assert_eq!(convert(RuntimeId::Tezos, RuntimeId::Tezos, 42), Some(42));
        }

        #[test]
        fn zero() {
            assert_eq!(convert(RuntimeId::Ethereum, RuntimeId::Tezos, 0), Some(0));
            assert_eq!(convert(RuntimeId::Tezos, RuntimeId::Ethereum, 0), Some(0));
        }
    }
}

impl From<evm_types::Error> for TezosXRuntimeError {
    fn from(value: evm_types::Error) -> Self {
        match value {
            evm_types::Error::Runtime(err) => TezosXRuntimeError::Runtime(err),
            evm_types::Error::Custom(msg) => TezosXRuntimeError::Custom(msg),
            evm_types::Error::FeesToGasOverflow => TezosXRuntimeError::Custom(
                "Gas for fees overflowed u64::max in conversion".to_string(),
            ),
            evm_types::Error::GasToFeesUnderflow => TezosXRuntimeError::Custom(
                "Insufficient gas to cover the non-execution fees".to_string(),
            ),
        }
    }
}
