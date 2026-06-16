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
/// Native runtime of the `X-Tezos-Source` address, encoded as the decimal
/// [`RuntimeId`] tag — the same numeric encoding used by the `originOf` /
/// `resolveAddress` ABI. Forwarded alongside `X-Tezos-Source` so the
/// receiving runtime can report a self-consistent
/// `(sourceRuntime, sourceAddress)` identity: on a nested
/// `EVM -> Michelson -> EVM` CRAC the forwarded source is the transitive
/// EVM origin, whose native runtime is Ethereum, not the immediate Tezos
/// sender's.
pub const X_TEZOS_SOURCE_RUNTIME: &str = "X-Tezos-Source-Runtime";
pub const X_TEZOS_AMOUNT: &str = "X-Tezos-Amount";
pub const X_TEZOS_GAS_LIMIT: &str = "X-Tezos-Gas-Limit";
pub const X_TEZOS_GAS_CONSUMED: &str = "X-Tezos-Gas-Consumed";
pub const X_TEZOS_TIMESTAMP: &str = "X-Tezos-Timestamp";
pub const X_TEZOS_BLOCK_NUMBER: &str = "X-Tezos-Block-Number";
pub const X_TEZOS_CRAC_ID: &str = "X-Tezos-Cross-Runtime-Call-ID";
pub const X_TEZOS_CRAC_DEPTH: &str = "X-Tezos-Cross-Runtime-Call-Depth";
pub const X_TEZOS_STORAGE_COST: &str = "X-Tezos-Storage-Cost";

/// Maximum permitted value of the `X-Tezos-CRAC-Depth` header, i.e. the
/// maximum number of cross-runtime (CRAC) hops a single chain may take.
///
/// Each cross-runtime crossing stamps `inbound + 1` on the outgoing
/// header, so the counter is a hop count. A self-recursive Michelson↔EVM
/// gateway cycle drives it up one hop at a time; without a bound such a
/// cycle never terminates and wedges sequencer block production. Every
/// runtime rejects an inbound CRAC whose depth exceeds this cap,
/// converting the would-be spin into a clean, catchable operation-level
/// failure.
///
/// This is a network-performance bound for Tezos X — not derived from
/// any protocol constant — chosen well above realistic legitimate
/// cross-runtime composition depth (a handful of hops) while keeping the
/// worst-case per-operation kernel work bounded. A gas-based termination
/// (charged symmetrically across runtimes) is the longer-term fix and
/// will subsume this cap.
pub const MAX_CRAC_DEPTH: u32 = 128;

pub const ERR_FORBIDDEN_TEZOS_HEADER: &str =
    "user-supplied X-Tezos-* headers are forbidden";

/// Context shared across runtimes for cross-runtime operations.
#[derive(Clone, Debug)]
pub struct CrossRuntimeContext {
    pub gas_limit: u64,
    pub timestamp: U256,
    pub block_number: U256,
}

/// A runtime-agnostic wrapper for durable-storage failures.
///
/// Wraps a `String` rather than a concrete error type so that
/// `tezosx-types` stays free of `revm-etherlink` (which would close a
/// dependency cycle). The variant name remains structured so call sites
/// can match on it; the message is for human readers only.
#[derive(Eq, PartialEq, Debug, Error)]
#[error("{0}")]
pub struct KernelStorageError(pub String);

impl From<tezos_storage::error::Error> for KernelStorageError {
    fn from(e: tezos_storage::error::Error) -> Self {
        KernelStorageError(e.to_string())
    }
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
    Storage(KernelStorageError),
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

impl From<KernelStorageError> for TezosXRuntimeError {
    fn from(e: KernelStorageError) -> Self {
        TezosXRuntimeError::Storage(e)
    }
}

/// Preserve existing `?`-ergonomics on the Tezos side: the many call
/// sites that propagate `tezos_storage::error::Error` with `?` do not
/// need to be touched.  `?` performs exactly one `From` hop, so without
/// this direct impl every Tezos-side `?` would need an explicit
/// `.map_err(Into::into)`.
impl From<tezos_storage::error::Error> for TezosXRuntimeError {
    fn from(e: tezos_storage::error::Error) -> Self {
        TezosXRuntimeError::Storage(KernelStorageError(e.to_string()))
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy, BinWriter, NomReader, HasEncoding)]
#[encoding(tags = "u8")]
pub enum RuntimeId {
    #[encoding(tag = 0)]
    Tezos = 0,
    #[encoding(tag = 1)]
    Ethereum = 1,
}

/// Canonical form of a native address for alias derivation and for
/// storing as the `Native` representation in classification records.
///
/// Ethereum hex is case-insensitive on the wire, so two callers that
/// pass `0xABC...` and `0xabc...` must derive the same alias; lowercase
/// is the canonical form. Tezos base58check is case-sensitive (`tz1ABC`
/// and `tz1abc` are different addresses, typically with one invalid),
/// so the input is passed through unchanged.
pub fn canonicalize_native_address(runtime: RuntimeId, addr: &str) -> String {
    match runtime {
        RuntimeId::Ethereum => addr.to_lowercase(),
        RuntimeId::Tezos => addr.to_string(),
    }
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

impl AliasInfo {
    /// Decode [`Self::native_address`] as the UTF-8 string it is
    /// invariantly supposed to be (see the type's doc comment),
    /// consuming `self`. A non-UTF-8 payload is data corruption;
    /// callers should surface it as a runtime-local kernel error, not
    /// silently substitute `U+FFFD`.
    pub fn into_native_address_string(
        self,
    ) -> Result<String, std::string::FromUtf8Error> {
        String::from_utf8(self.native_address)
    }
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

/// Query-result type for origin lookups across runtimes.
///
/// An address is either absent from the registry (`Unknown`), native to
/// its runtime (`Native`), or an alias whose underlying account lives
/// in another runtime (`Alias`). Never stored on-disk — use [`Origin`]
/// for the stored form.
///
/// `Unknown` means no classification record exists *and* no back-stop
/// evidence (e.g. positive EVM nonce) promoted the address to `Native`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Classification {
    /// No classification record found and no back-stop evidence.
    Unknown,
    /// Address is recorded (or inferred) as native to its runtime.
    Native,
    /// Address is an alias whose native account lives in another runtime.
    Alias(AliasInfo),
}

impl From<Origin> for Classification {
    fn from(o: Origin) -> Self {
        match o {
            Origin::Native => Classification::Native,
            Origin::Alias(info) => Classification::Alias(info),
        }
    }
}

impl From<Option<Origin>> for Classification {
    fn from(o: Option<Origin>) -> Self {
        match o {
            Some(origin) => Classification::from(origin),
            None => Classification::Unknown,
        }
    }
}

/// Identity of the top-level transaction originator (`tx.origin`), carried
/// across re-entrant frames and across the cross-runtime boundary.
///
/// Holds the originator's address in its *own* native runtime
/// ([`original_address`](Self::original_address)) together with
/// [`runtime`](Self::runtime), the runtime that address is native to. The
/// stored form is runtime-agnostic — lowercase `0x` hex when
/// `runtime == Ethereum`, base58check (`tz1`/`KT1`) when
/// `runtime == Tezos` — and it is the canonical native address from which
/// every other runtime's alias is *deterministically* derived. A gateway
/// targeting some runtime therefore obtains the originator's
/// representation there with no durable lookup: when the target equals
/// [`runtime`](Self::runtime) the native address is the answer directly,
/// otherwise the answer is the registry's `compute_alias` of the native
/// address (the same value the materialized alias holds). This is why the
/// Michelson alias is *not* cached here — translating on demand costs only
/// a pure hash, while caching it would force an eager `get_origin` read on
/// every originator even when the alias is never used. The cross-runtime
/// journal stores this once (set by the first outgoing gateway call), and
/// both the state-mutating and the read-only/view paths read it, which is
/// what lets `tx.origin` resolve to the same transitive native origin
/// independently of any durable alias/origin record.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OriginalSource {
    runtime: RuntimeId,
    original_address: String,
}

impl OriginalSource {
    pub fn new(runtime: RuntimeId, original_address: String) -> Self {
        Self {
            runtime,
            original_address,
        }
    }

    /// The runtime the originator's account is native to.
    pub fn runtime(&self) -> RuntimeId {
        self.runtime
    }

    /// The originator's address in its own native runtime
    /// ([`runtime`](Self::runtime)): lowercase `0x` hex for Ethereum,
    /// base58check for Tezos. The canonical native form, used directly
    /// when the wanted runtime matches [`runtime`](Self::runtime) and as
    /// the `compute_alias` basis otherwise.
    pub fn original_address(&self) -> &str {
        &self.original_address
    }
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

    /// The URL host string identifying this runtime in cross-runtime HTTP
    /// requests (e.g. `"tezos"`, `"ethereum"`). Inverse of [`from_host`].
    ///
    /// [`from_host`]: RuntimeId::from_host
    pub fn as_host(&self) -> &'static str {
        match self {
            RuntimeId::Tezos => "tezos",
            RuntimeId::Ethereum => "ethereum",
        }
    }
}

/// Cost of a single `/origin` durable-storage read. Equivalent to a cold
/// SLOAD (EIP-2929). Charged inside `read_origin` for every storage-
/// consulting path (skipped on malformed-addr short-circuits).
pub const ALIAS_LOOKUP_COST: u64 = 2_100;

/// Milligas equivalent of `ALIAS_LOOKUP_COST` (EVM gas × EVM_GAS_TO_MILLIGAS).
pub const ALIAS_LOOKUP_MILLIGAS: u64 =
    ALIAS_LOOKUP_COST * tezosx_constants::EVM_GAS_TO_MILLIGAS;

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

#[cfg(test)]
mod original_source_tests {
    use super::*;

    #[test]
    fn evm_native_address_and_runtime() {
        // An Ethereum-native originator: the native address is the EVM
        // address; the Michelson form is derived on demand, not stored.
        let src = OriginalSource::new(RuntimeId::Ethereum, "0xabc".to_string());
        assert_eq!(src.runtime(), RuntimeId::Ethereum);
        assert_eq!(src.original_address(), "0xabc");
    }

    #[test]
    fn tezos_native_address_and_runtime() {
        // A Tezos-native originator: the native address is its base58check
        // form; its runtime tag stays Tezos.
        let src = OriginalSource::new(RuntimeId::Tezos, "tz1def".to_string());
        assert_eq!(src.runtime(), RuntimeId::Tezos);
        assert_eq!(src.original_address(), "tz1def");
    }
}
