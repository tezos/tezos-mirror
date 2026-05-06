// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub mod headers;

use primitive_types::U256;
use tezos_data_encoding::{enc::BinWriter, encoding::HasEncoding, nom::NomReader};
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
    /// The HTTP method is not supported by the target resource (e.g.
    /// `PUT` on the cross-runtime endpoint, which only accepts `POST`
    /// for entrypoint calls and `GET` for view calls). Maps to HTTP 405.
    #[error("Method not allowed: {0}")]
    MethodNotAllowed(String),
    /// An X-Tezos-* header is missing or contains an invalid value.
    /// Indicates a gateway bug; propagates as Err and reverts the blueprint.
    #[error("Header error: {0}")]
    HeaderError(String),
    /// The callee ran out of gas. Maps to HTTP 429.
    #[error("Gas exhaustion")]
    OutOfGas,
}
pub trait Registry {
    /// Idempotently ensure that the alias of `alias_info` exists in
    /// `target_runtime`, materializing the forwarder and recording the
    /// classification record if needed.
    ///
    /// `alias_info` carries the source runtime where the native account
    /// lives and the UTF-8 bytes of its address. `target_runtime`
    /// selects the runtime that will host the alias.
    ///
    /// `gas_remaining` is the caller's remaining gas budget in the
    /// target runtime gas units (milligas for Tezos, EVM gas for
    /// Ethereum). The function consumes gas incrementally and fails
    /// early if the budget is exceeded.
    ///
    /// Returns `(alias, gas_remaining_after)`. On a fresh deploy the
    /// behavior matches the legacy alias generation. When the alias is
    /// already classified, the call is a no-op and the gas budget is
    /// returned unchanged. When a forwarder exists but the
    /// classification path is empty (a legacy account from before this
    /// work), the call writes the classification only and skips the
    /// redeploy.
    #[allow(clippy::too_many_arguments)]
    fn ensure_alias<Host>(
        &self,
        host: &mut Host,
        journal: &mut TezosXJournal,
        alias_info: AliasInfo,
        native_public_key: Option<&[u8]>,
        target_runtime: RuntimeId,
        context: CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1;

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
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1;
}

pub trait RuntimeInterface {
    /// Idempotently ensure that the alias of `alias_info` exists in
    /// this runtime. `alias_info.runtime` is the source runtime where
    /// the underlying native account lives, and `alias_info.native_address`
    /// holds the UTF-8 bytes of its canonical address string.
    ///
    /// `gas_remaining` is the caller's remaining budget in this runtime
    /// gas units. Returns `(alias, gas_remaining_after)`. Fails if the
    /// budget is exceeded.
    ///
    /// Three branches:
    /// - if the alias account already has an alias classification
    ///   recorded, the call is a no-op and returns the address with
    ///   `gas_remaining` unchanged;
    /// - if the alias account exists with forwarder bytecode but the
    ///   classification path is empty (a legacy account from before
    ///   this work), the call writes the classification only and
    ///   skips the redeploy;
    /// - otherwise the call deploys the forwarder and records the
    ///   classification.
    #[allow(clippy::too_many_arguments)]
    fn ensure_alias<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        alias_info: AliasInfo,
        native_public_key: Option<&[u8]>,
        context: CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1;

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
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1;

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

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy, BinWriter, NomReader, HasEncoding)]
#[encoding(tags = "u8")]
pub enum RuntimeId {
    #[encoding(tag = 0)]
    Tezos = 0,
    #[encoding(tag = 1)]
    Ethereum = 1,
}

/// Payload carried by the alias variant of Origin/
///
/// The address bytes hold the UTF-8 form of the address string (hex
/// for EVM, b58check for Tezos). That is the same byte sequence the
/// alias derivation hashes, so the read path can reconstruct the
/// original string with a UTF-8 decode instead of going through the
/// source runtime address decoder.
#[derive(Clone, Debug, Eq, PartialEq, BinWriter, NomReader, HasEncoding)]
pub struct AliasInfo {
    pub runtime: RuntimeId,
    #[encoding(dynamic, bytes)]
    pub native_address: Vec<u8>,
}

/// Classification record stored at an account classification path.
///
/// Three states are possible: native, alias (with the runtime where
/// the underlying account lives and the bytes of its address), and
/// unrecorded (the absence of any Origin value at the path). The alias
/// variant carries both the runtime and the address so that the kernel
/// can short-circuit a translation back to the originating runtime
/// without rerunning the derivation step.
#[derive(Clone, Debug, Eq, PartialEq, BinWriter, NomReader, HasEncoding)]
#[encoding(tags = "u8")]
pub enum Origin {
    #[encoding(tag = 0)]
    Native,
    #[encoding(tag = 1)]
    Alias(AliasInfo),
}

/// How a source's translation toward a chosen target runtime should be routed.
pub enum RoutingDecision {
    /// Source is recorded as an alias of an account in the target
    /// runtime. The recorded bytes are the answer; decode and return.
    RoundTrip(String),
    /// Source is recorded as an alias of an account in a runtime other
    /// than the target. Use the recorded info as the basis for derivation
    /// so chained translations agree with the direct path. Unreachable in
    /// two runtime mode.
    Transitive(AliasInfo),
    /// Source is recorded as native, or has no classification. Derive
    /// from the source address as if it were native on the source runtime.
    Native,
}

/// Resolve the routing of the source's translation toward the target
/// runtime from the source's classification record.
pub fn resolve_routing(
    origin: Option<Origin>,
    target_runtime: RuntimeId,
) -> Result<RoutingDecision, std::string::FromUtf8Error> {
    match origin {
        Some(Origin::Alias(info)) if info.runtime == target_runtime => {
            String::from_utf8(info.native_address).map(RoutingDecision::RoundTrip)
        }
        Some(Origin::Alias(info)) => Ok(RoutingDecision::Transitive(info)),
        Some(Origin::Native) | None => Ok(RoutingDecision::Native),
    }
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

#[cfg(test)]
mod origin_tests {
    use super::*;
    use tezos_data_encoding::{enc::BinWriter, nom::NomReader};

    fn encode(o: &Origin) -> Vec<u8> {
        let mut buf = Vec::new();
        o.bin_write(&mut buf).expect("encoding succeeds");
        buf
    }

    fn decode(bytes: &[u8]) -> Origin {
        let (rest, o) = Origin::nom_read(bytes).expect("decoding succeeds");
        assert!(rest.is_empty(), "decoder must consume all bytes");
        o
    }

    #[test]
    fn native_roundtrip() {
        let bytes = encode(&Origin::Native);
        assert_eq!(bytes, vec![0u8]);
        assert_eq!(decode(&bytes), Origin::Native);
    }

    #[test]
    fn alias_ethereum_roundtrip() {
        let origin = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: vec![0xde, 0xad, 0xbe, 0xef],
        });
        let bytes = encode(&origin);
        // Outer enum tag (1 byte) + RuntimeId tag (1 byte) +
        // dynamic length prefix (4 bytes) + 4 payload bytes = 10 bytes.
        assert_eq!(bytes.len(), 10);
        assert_eq!(bytes[0], 1u8);
        assert_eq!(bytes[1], 1u8);
        assert_eq!(decode(&bytes), origin);
    }

    #[test]
    fn alias_tezos_roundtrip() {
        let origin = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Tezos,
            native_address: b"tz1...".to_vec(),
        });
        let bytes = encode(&origin);
        assert_eq!(bytes[0], 1u8);
        assert_eq!(bytes[1], 0u8);
        assert_eq!(decode(&bytes), origin);
    }

    #[test]
    fn invalid_tag_rejected() {
        assert!(Origin::nom_read(&[0xff]).is_err());
    }

    #[test]
    fn truncated_alias_rejected() {
        assert!(Origin::nom_read(&[0x01]).is_err());
    }
}

/// Gas conversion utilities for cross-runtime calls.
///
/// **Convention**: Both `X-Tezos-Gas-Limit` and `X-Tezos-Gas-Consumed` are
/// in the **called** runtime's gas units. Gateways convert on the way out
/// (for the limit) and on the way back (for consumed); receiving runtimes
/// read/write directly without conversion.
///
/// **Ratio**: 1 EVM gas = [`tezosx_constants::EVM_GAS_TO_MILLIGAS`] Tezos
/// milligas.
/// **Tezos unit**: milligas (the native unit of the Tezos runtime).
pub mod gas {
    use crate::RuntimeId;
    use tezosx_constants::EVM_GAS_TO_MILLIGAS;

    /// Convert `gas` from `source` runtime units to `target` runtime units.
    ///
    /// Returns `None` on overflow (only possible for Ethereum→Tezos when
    /// `gas > u64::MAX / EVM_GAS_TO_MILLIGAS`). Returns `Some(gas)`
    /// unchanged when `source == target`.
    pub fn convert(source: RuntimeId, target: RuntimeId, gas: u64) -> Option<u64> {
        match (source, target) {
            (RuntimeId::Ethereum, RuntimeId::Tezos) => {
                gas.checked_mul(EVM_GAS_TO_MILLIGAS)
            }
            (RuntimeId::Tezos, RuntimeId::Ethereum) => Some(gas / EVM_GAS_TO_MILLIGAS),
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
                Some(100 * EVM_GAS_TO_MILLIGAS)
            );
            assert_eq!(
                convert(RuntimeId::Ethereum, RuntimeId::Tezos, 1_000_000),
                Some(1_000_000 * EVM_GAS_TO_MILLIGAS)
            );
        }

        #[test]
        fn ethereum_to_tezos_overflow() {
            let large = u64::MAX / EVM_GAS_TO_MILLIGAS + 1;
            assert_eq!(convert(RuntimeId::Ethereum, RuntimeId::Tezos, large), None);
        }

        #[test]
        fn tezos_to_ethereum() {
            assert_eq!(
                convert(
                    RuntimeId::Tezos,
                    RuntimeId::Ethereum,
                    100 * EVM_GAS_TO_MILLIGAS
                ),
                Some(100)
            );
        }

        #[test]
        fn tezos_to_ethereum_truncates() {
            // EVM_GAS_TO_MILLIGAS + half a unit's worth → still 1 EVM gas.
            assert_eq!(
                convert(
                    RuntimeId::Tezos,
                    RuntimeId::Ethereum,
                    EVM_GAS_TO_MILLIGAS + EVM_GAS_TO_MILLIGAS / 2
                ),
                Some(1)
            );
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
