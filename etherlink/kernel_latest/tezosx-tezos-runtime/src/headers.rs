// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! HTTP header parsing for Tezos runtime requests.
//!
//! Header name constants are defined in [`tezosx_interfaces`] and
//! re-exported here for convenience. Required headers must be set by the
//! caller; absent or malformed headers produce a `HeaderError`.

pub use tezosx_interfaces::{
    X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER, X_TEZOS_GAS_LIMIT, X_TEZOS_SENDER,
    X_TEZOS_SOURCE, X_TEZOS_TIMESTAMP,
};

use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_data_encoding::types::Narith;
use tezos_smart_rollup::types::{PublicKeyHash, Timestamp};
use tezos_tezlink::enc_wrappers::BlockNumber;
use tezosx_interfaces::TezosXRuntimeError;

/// Values contained in the `X-Tezos-*` request headers, in their Tezos runtime
/// types. Used in parsing and inserting.
#[derive(Debug)]
pub struct MichelsonHeaders {
    /// Transfer amount in mutez.
    pub amount: Narith,
    /// Gas limit in gas units.
    pub gas_limit: Narith,
    /// Block timestamp.
    pub timestamp: Timestamp,
    /// Block level.
    pub block_number: BlockNumber,
    /// Sender KT1 contract address (for Michelson `SENDER`).
    pub sender: ContractKt1Hash,
    /// Source implicit account address (for Michelson `SOURCE`).
    /// `None` if the header is absent.
    pub source: Option<PublicKeyHash>,
}

/// Parse `X-Tezos-*` headers from `headers`.
///
/// Returns `HeaderError` if a required header is absent or any header value
/// cannot be parsed as its expected type.
pub fn parse_request_headers(
    headers: &http::HeaderMap,
) -> Result<MichelsonHeaders, TezosXRuntimeError> {
    Ok(MichelsonHeaders {
        amount: Narith(require_u64(headers, X_TEZOS_AMOUNT)?.into()),
        gas_limit: Narith(require_u64(headers, X_TEZOS_GAS_LIMIT)?.into()),
        timestamp: Timestamp::from(require_i64(headers, X_TEZOS_TIMESTAMP)?),
        block_number: BlockNumber::from(require_u32(headers, X_TEZOS_BLOCK_NUMBER)?),
        sender: require_kt1(headers, X_TEZOS_SENDER)?,
        source: parse_pkh(headers, X_TEZOS_SOURCE)?,
    })
}

fn require_str(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<String, TezosXRuntimeError> {
    parse_str(headers, name)?.ok_or_else(|| {
        TezosXRuntimeError::HeaderError(format!("Missing required header: {name}"))
    })
}

fn require_u64(headers: &http::HeaderMap, name: &str) -> Result<u64, TezosXRuntimeError> {
    let s = require_str(headers, name)?;
    s.parse::<u64>().map_err(|_| {
        TezosXRuntimeError::HeaderError(format!(
            "Invalid {name} header value: expected u64, got {s:?}"
        ))
    })
}

fn require_i64(headers: &http::HeaderMap, name: &str) -> Result<i64, TezosXRuntimeError> {
    let s = require_str(headers, name)?;
    s.parse::<i64>().map_err(|_| {
        TezosXRuntimeError::HeaderError(format!(
            "Invalid {name} header value: expected i64, got {s:?}"
        ))
    })
}

fn require_u32(headers: &http::HeaderMap, name: &str) -> Result<u32, TezosXRuntimeError> {
    let s = require_str(headers, name)?;
    s.parse::<u32>().map_err(|_| {
        TezosXRuntimeError::HeaderError(format!(
            "Invalid {name} header value: expected u32, got {s:?}"
        ))
    })
}

fn require_kt1(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<ContractKt1Hash, TezosXRuntimeError> {
    let s = require_str(headers, name)?;
    ContractKt1Hash::from_b58check(&s).map_err(|e| {
        TezosXRuntimeError::HeaderError(format!(
            "Invalid {name}: expected KT1 address: {e}"
        ))
    })
}

fn parse_pkh(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<Option<PublicKeyHash>, TezosXRuntimeError> {
    parse_str(headers, name)?
        .map(|s| {
            PublicKeyHash::from_b58check(&s).map_err(|e| {
                TezosXRuntimeError::HeaderError(format!("Invalid {name}: {e}"))
            })
        })
        .transpose()
}

/// Returns `None` if the header is absent, `Some(str)` if present and valid
/// UTF-8, or `HeaderError` if the value is not valid UTF-8.
fn parse_str(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<Option<String>, TezosXRuntimeError> {
    headers
        .get(name)
        .map(|v| {
            v.to_str().map(|s| s.to_owned()).map_err(|_| {
                TezosXRuntimeError::HeaderError(format!(
                    "Header {name} contains non-UTF-8 bytes"
                ))
            })
        })
        .transpose()
}

#[cfg(test)]
mod tests {
    use super::*;

    const KT1: &str = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT";
    const TZ1: &str = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU";

    fn headers_from(pairs: &[(&str, &str)]) -> http::HeaderMap {
        let mut map = http::HeaderMap::new();
        for (k, v) in pairs {
            map.insert(
                http::header::HeaderName::from_bytes(k.as_bytes()).unwrap(),
                http::header::HeaderValue::from_str(v).unwrap(),
            );
        }
        map
    }

    /// Build a valid set of all required headers.
    fn required_headers() -> Vec<(&'static str, &'static str)> {
        vec![
            (X_TEZOS_AMOUNT, "0"),
            (X_TEZOS_GAS_LIMIT, "1000000"),
            (X_TEZOS_TIMESTAMP, "1000000"),
            (X_TEZOS_BLOCK_NUMBER, "1"),
            (X_TEZOS_SENDER, KT1),
        ]
    }

    #[test]
    fn all_required_present_no_source() {
        let headers = headers_from(&required_headers());
        let parsed = parse_request_headers(&headers).unwrap();
        assert_eq!(parsed.amount, Narith(0u64.into()));
        assert_eq!(parsed.gas_limit, Narith(1_000_000u64.into()));
        assert_eq!(parsed.timestamp, Timestamp::from(1_000_000_i64));
        assert_eq!(parsed.block_number, BlockNumber::from(1u32));
        assert_eq!(parsed.sender, ContractKt1Hash::from_b58check(KT1).unwrap());
        assert_eq!(parsed.source, None);
    }

    #[test]
    fn source_optional() {
        let mut hdrs = required_headers();
        hdrs.push((X_TEZOS_SOURCE, TZ1));
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(
            parsed.source,
            Some(PublicKeyHash::from_b58check(TZ1).unwrap())
        );
    }

    #[test]
    fn amount_parsed() {
        let mut hdrs = required_headers();
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "1000000");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.amount, Narith(1_000_000u64.into()));
    }

    #[test]
    fn timestamp_parsed_negative() {
        let mut hdrs = required_headers();
        *hdrs
            .iter_mut()
            .find(|(k, _)| *k == X_TEZOS_TIMESTAMP)
            .unwrap() = (X_TEZOS_TIMESTAMP, "-1");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.timestamp, Timestamp::from(-1_i64));
    }

    #[test]
    fn missing_required_header_returns_header_error() {
        for required in [
            X_TEZOS_AMOUNT,
            X_TEZOS_GAS_LIMIT,
            X_TEZOS_TIMESTAMP,
            X_TEZOS_BLOCK_NUMBER,
            X_TEZOS_SENDER,
        ] {
            let hdrs: Vec<_> = required_headers()
                .into_iter()
                .filter(|(k, _)| *k != required)
                .collect();
            let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
            assert!(
                matches!(err, TezosXRuntimeError::HeaderError(_)),
                "expected HeaderError for missing {required}"
            );
        }
    }

    #[test]
    fn invalid_u64_returns_header_error() {
        let mut hdrs = required_headers();
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "not-a-number");
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }

    #[test]
    fn invalid_i64_returns_header_error() {
        let mut hdrs = required_headers();
        *hdrs
            .iter_mut()
            .find(|(k, _)| *k == X_TEZOS_TIMESTAMP)
            .unwrap() = (X_TEZOS_TIMESTAMP, "9999999999999999999999");
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }

    #[test]
    fn invalid_u32_returns_header_error() {
        let mut hdrs = required_headers();
        *hdrs
            .iter_mut()
            .find(|(k, _)| *k == X_TEZOS_BLOCK_NUMBER)
            .unwrap() = (X_TEZOS_BLOCK_NUMBER, "5000000000");
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }

    #[test]
    fn invalid_sender_returns_header_error() {
        let mut hdrs = required_headers();
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_SENDER).unwrap() =
            (X_TEZOS_SENDER, "not-a-kt1-address");
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }

    #[test]
    fn invalid_source_returns_header_error() {
        let mut hdrs = required_headers();
        hdrs.push((X_TEZOS_SOURCE, "not-a-tz-address"));
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }
}
