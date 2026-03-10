// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Shared HTTP header parsing helpers for TezosX runtimes.
//!
//! Each runtime re-uses these low-level extractors when building its own
//! typed `parse_request_headers` function.

use crate::TezosXRuntimeError;

/// Returns `None` if the header is absent, `Some(str)` if present and valid
/// UTF-8, or `HeaderError` if the value is not valid UTF-8.
pub fn parse_str(
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

/// Extract a required string header, returning `HeaderError` when absent.
pub fn require_str(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<String, TezosXRuntimeError> {
    parse_str(headers, name)?.ok_or_else(|| {
        TezosXRuntimeError::HeaderError(format!("Missing required header: {name}"))
    })
}

/// Extract a required `u64` header.
pub fn require_u64(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<u64, TezosXRuntimeError> {
    let s = require_str(headers, name)?;
    s.parse::<u64>().map_err(|_| {
        TezosXRuntimeError::HeaderError(format!(
            "Invalid {name} header value: expected u64, got {s:?}"
        ))
    })
}

/// Extract a required `i64` header.
pub fn require_i64(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<i64, TezosXRuntimeError> {
    let s = require_str(headers, name)?;
    s.parse::<i64>().map_err(|_| {
        TezosXRuntimeError::HeaderError(format!(
            "Invalid {name} header value: expected i64, got {s:?}"
        ))
    })
}

/// Extract a required `u32` header.
pub fn require_u32(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<u32, TezosXRuntimeError> {
    let s = require_str(headers, name)?;
    s.parse::<u32>().map_err(|_| {
        TezosXRuntimeError::HeaderError(format!(
            "Invalid {name} header value: expected u32, got {s:?}"
        ))
    })
}

/// Build an [`http::HeaderMap`] from key-value pairs. Useful in tests.
pub fn headers_from(pairs: &[(&str, &str)]) -> http::HeaderMap {
    let mut map = http::HeaderMap::new();
    for (k, v) in pairs {
        map.insert(
            http::header::HeaderName::from_bytes(k.as_bytes()).unwrap(),
            http::header::HeaderValue::from_str(v).unwrap(),
        );
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER, X_TEZOS_TIMESTAMP};

    #[test]
    fn parse_str_returns_none_for_absent_header() {
        let map = http::HeaderMap::new();
        assert!(parse_str(&map, "X-Missing").unwrap().is_none());
    }

    #[test]
    fn require_str_errors_on_absent_header() {
        let map = http::HeaderMap::new();
        let err = require_str(&map, "X-Missing").unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }

    #[test]
    fn require_u64_parses_valid_value() {
        let hdrs = headers_from(&[(X_TEZOS_AMOUNT, "42")]);
        assert_eq!(require_u64(&hdrs, X_TEZOS_AMOUNT).unwrap(), 42);
    }

    #[test]
    fn require_u64_rejects_invalid_value() {
        let hdrs = headers_from(&[(X_TEZOS_AMOUNT, "abc")]);
        assert!(require_u64(&hdrs, X_TEZOS_AMOUNT).is_err());
    }

    #[test]
    fn require_i64_parses_negative_value() {
        let hdrs = headers_from(&[(X_TEZOS_TIMESTAMP, "-1")]);
        assert_eq!(require_i64(&hdrs, X_TEZOS_TIMESTAMP).unwrap(), -1);
    }

    #[test]
    fn require_u32_rejects_overflow() {
        let hdrs = headers_from(&[(X_TEZOS_BLOCK_NUMBER, "5000000000")]);
        assert!(require_u32(&hdrs, X_TEZOS_BLOCK_NUMBER).is_err());
    }
}
