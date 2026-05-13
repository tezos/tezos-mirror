// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Pure types and constants shared across TezosX crates.
//!
//! Lives separately from `tezosx-interfaces` (which carries traits
//! that depend on `tezosx-journal`) so that `tezosx-journal` can use
//! these types without creating a dependency cycle.

pub mod headers;

use primitive_types::U256;
use tezos_data_encoding::{enc::BinWriter, encoding::HasEncoding, nom::NomReader};
use tezos_smart_rollup_host::runtime::RuntimeError;
use thiserror::Error;

pub const X_TEZOS_SENDER: &str = "X-Tezos-Sender";
pub const X_TEZOS_SOURCE: &str = "X-Tezos-Source";
pub const X_TEZOS_AMOUNT: &str = "X-Tezos-Amount";
pub const X_TEZOS_GAS_LIMIT: &str = "X-Tezos-Gas-Limit";
pub const X_TEZOS_GAS_CONSUMED: &str = "X-Tezos-Gas-Consumed";
pub const X_TEZOS_TIMESTAMP: &str = "X-Tezos-Timestamp";
pub const X_TEZOS_BLOCK_NUMBER: &str = "X-Tezos-Block-Number";
pub const X_TEZOS_CRAC_ID: &str = "X-Tezos-CRAC-ID";

pub const ERR_FORBIDDEN_TEZOS_HEADER: &str =
    "user-supplied X-Tezos-* headers are forbidden";

/// Context shared across runtimes for cross-runtime operations.
#[derive(Clone, Debug)]
pub struct CrossRuntimeContext {
    pub gas_limit: u64,
    pub timestamp: U256,
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
    /// The HTTP method is not supported by the target resource. Maps to HTTP 405.
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

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy, BinWriter, NomReader, HasEncoding)]
#[encoding(tags = "u8")]
pub enum RuntimeId {
    #[encoding(tag = 0)]
    Tezos = 0,
    #[encoding(tag = 1)]
    Ethereum = 1,
}

/// Payload carried by the alias variant of Origin.
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
