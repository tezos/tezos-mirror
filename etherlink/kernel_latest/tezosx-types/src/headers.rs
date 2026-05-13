// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Shared HTTP header parsing helpers for TezosX runtimes.
//!
//! Each runtime re-uses these low-level extractors when building its own
//! typed `parse_request_headers` function.

use crate::TezosXRuntimeError;
use primitive_types::U256;

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

// ---------------------------------------------------------------------------
// TEZ decimal-string formatting and parsing
// ---------------------------------------------------------------------------

/// Format a wei amount (`10^-18 TEZ`) as a canonical TEZ decimal string.
///
/// Examples: `0` → `"0"`, `10^18` → `"1"`, `1.5 * 10^18` → `"1.5"`.
/// Trailing fractional zeros are stripped; the decimal point is omitted for
/// whole amounts.
pub fn format_tez_from_wei(wei: U256) -> String {
    format_tez_amount(wei, 18)
}

/// Format a mutez amount (`10^-6 TEZ`) as a canonical TEZ decimal string.
///
/// Examples: `0` → `"0"`, `10^6` → `"1"`, `1` → `"0.000001"`.
pub fn format_tez_from_mutez(mutez: u64) -> String {
    format_tez_amount(U256::from(mutez), 6)
}

/// Parse a TEZ decimal string into wei (`10^-18 TEZ`).
///
/// Accepts up to 18 decimal places.
pub fn parse_tez_to_wei(s: &str) -> Result<U256, TezosXRuntimeError> {
    parse_tez_amount(s, 18)
        .map_err(|e| TezosXRuntimeError::HeaderError(format!("Invalid TEZ amount: {e}")))
}

/// Parse a TEZ decimal string into mutez (`10^-6 TEZ`).
///
/// Truncates excess decimal places beyond 6 (ADR L2-1004).
pub fn parse_tez_to_mutez(s: &str) -> Result<u64, TezosXRuntimeError> {
    let amount = parse_tez_amount(s, 6).map_err(|e| {
        TezosXRuntimeError::HeaderError(format!("Invalid TEZ amount: {e}"))
    })?;
    if amount > U256::from(u64::MAX) {
        return Err(TezosXRuntimeError::HeaderError(
            "TEZ amount too large for mutez".to_string(),
        ));
    }
    Ok(amount.as_u64())
}

fn format_tez_amount(amount: U256, decimals: usize) -> String {
    // Short-circuit the common case: every gateway call that does not
    // transfer value ends up formatting `0`. Skipping the divmod +
    // `format!` avoids a handful of allocations on the hot path.
    if amount.is_zero() {
        return "0".to_string();
    }
    let divisor = U256::exp10(decimals);
    let integer_part = amount / divisor;
    let remainder = amount % divisor;
    if remainder.is_zero() {
        format!("{integer_part}")
    } else {
        let remainder_str = format!("{remainder}");
        // Left-pad with zeros so the fractional part has exactly `decimals` digits,
        // then strip trailing zeros.
        let padded = format!("{remainder_str:0>decimals$}");
        let trimmed = padded.trim_end_matches('0');
        format!("{integer_part}.{trimmed}")
    }
}

fn parse_tez_amount(s: &str, decimals: usize) -> Result<U256, String> {
    if s.is_empty() {
        return Err("empty amount string".to_string());
    }
    // Short-circuit the canonical zero — skips the whole divmod/parse
    // path on the common `"0"` case emitted by `format_tez_amount` for
    // value-less gateway calls.
    if s == "0" {
        return Ok(U256::zero());
    }
    if s.bytes()
        .any(|b| b == b' ' || b == b'\t' || b == b'\n' || b == b'\r')
    {
        return Err("amount contains whitespace".to_string());
    }
    if s.starts_with('-') {
        return Err("negative amounts are not allowed".to_string());
    }

    let (integer_str, frac_str) = match s.split_once('.') {
        Some((int_part, frac_part)) => {
            if int_part.is_empty() && frac_part.is_empty() {
                return Err("invalid amount: \".\"".to_string());
            }
            (int_part, frac_part)
        }
        None => (s, ""),
    };

    if !integer_str.is_empty() && !integer_str.bytes().all(|b| b.is_ascii_digit()) {
        return Err(format!("invalid integer part: {integer_str:?}"));
    }
    if !frac_str.is_empty() && !frac_str.bytes().all(|b| b.is_ascii_digit()) {
        return Err(format!("invalid fractional part: {frac_str:?}"));
    }

    // Truncate excess decimal places instead of rejecting (ADR L2-1004):
    // the receiving runtime rounds down to its precision.
    let frac_str = if frac_str.len() > decimals {
        &frac_str[..decimals]
    } else {
        frac_str
    };

    let integer_part = if integer_str.is_empty() {
        U256::zero()
    } else {
        U256::from_dec_str(integer_str)
            .map_err(|_| format!("invalid integer part: {integer_str:?}"))?
    };

    let frac_value = if frac_str.is_empty() {
        U256::zero()
    } else {
        // Right-pad fractional part to `decimals` digits so that e.g. "5" with
        // decimals=18 becomes "500000000000000000".
        let padded = format!("{frac_str:0<decimals$}");
        U256::from_dec_str(&padded)
            .map_err(|_| format!("invalid fractional part: {frac_str:?}"))?
    };

    integer_part
        .checked_mul(U256::exp10(decimals))
        .and_then(|v| v.checked_add(frac_value))
        .ok_or_else(|| "amount overflow".to_string())
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

    // --- TEZ formatting tests ---

    #[test]
    fn format_wei_zero() {
        assert_eq!(format_tez_from_wei(U256::zero()), "0");
    }

    #[test]
    fn format_wei_one_tez() {
        assert_eq!(format_tez_from_wei(U256::exp10(18)), "1");
    }

    #[test]
    fn format_wei_fractional() {
        // 1.5 TEZ
        let wei = U256::exp10(18) + U256::exp10(17) * 5;
        assert_eq!(format_tez_from_wei(wei), "1.5");
    }

    #[test]
    fn format_wei_small_fraction() {
        // 0.001 TEZ = 10^15 wei
        assert_eq!(format_tez_from_wei(U256::exp10(15)), "0.001");
    }

    #[test]
    fn format_wei_one_wei() {
        // Smallest unit: 0.000000000000000001 TEZ
        assert_eq!(format_tez_from_wei(U256::from(1)), "0.000000000000000001");
    }

    #[test]
    fn format_wei_one_mutez_equivalent() {
        // 0.000001 TEZ = 10^12 wei
        assert_eq!(format_tez_from_wei(U256::exp10(12)), "0.000001");
    }

    #[test]
    fn format_mutez_zero() {
        assert_eq!(format_tez_from_mutez(0), "0");
    }

    #[test]
    fn format_mutez_one_tez() {
        assert_eq!(format_tez_from_mutez(1_000_000), "1");
    }

    #[test]
    fn format_mutez_fractional() {
        // 1.5 TEZ = 1_500_000 mutez
        assert_eq!(format_tez_from_mutez(1_500_000), "1.5");
    }

    #[test]
    fn format_mutez_smallest() {
        // 0.000001 TEZ = 1 mutez
        assert_eq!(format_tez_from_mutez(1), "0.000001");
    }

    // --- TEZ parsing tests ---

    #[test]
    fn parse_wei_zero() {
        assert_eq!(parse_tez_to_wei("0").unwrap(), U256::zero());
    }

    #[test]
    fn parse_wei_one_tez() {
        assert_eq!(parse_tez_to_wei("1").unwrap(), U256::exp10(18));
    }

    #[test]
    fn parse_wei_fractional() {
        let expected = U256::exp10(18) + U256::exp10(17) * 5;
        assert_eq!(parse_tez_to_wei("1.5").unwrap(), expected);
    }

    #[test]
    fn parse_wei_full_precision() {
        assert_eq!(
            parse_tez_to_wei("0.000000000000000001").unwrap(),
            U256::from(1)
        );
    }

    #[test]
    fn parse_wei_trailing_decimal() {
        // "1." is treated as "1"
        assert_eq!(parse_tez_to_wei("1.").unwrap(), U256::exp10(18));
    }

    #[test]
    fn parse_wei_leading_decimal() {
        // ".5" is treated as "0.5"
        assert_eq!(parse_tez_to_wei(".5").unwrap(), U256::exp10(17) * 5);
    }

    #[test]
    fn parse_mutez_zero() {
        assert_eq!(parse_tez_to_mutez("0").unwrap(), 0);
    }

    #[test]
    fn parse_mutez_one_tez() {
        assert_eq!(parse_tez_to_mutez("1").unwrap(), 1_000_000);
    }

    #[test]
    fn parse_mutez_fractional() {
        assert_eq!(parse_tez_to_mutez("1.5").unwrap(), 1_500_000);
    }

    #[test]
    fn parse_mutez_smallest() {
        assert_eq!(parse_tez_to_mutez("0.000001").unwrap(), 1);
    }

    #[test]
    fn parse_mutez_truncates_excess_decimals() {
        // "0.0000001" has 7 decimals; mutez precision is 6.
        // ADR L2-1004: truncate to "0.000000" = 0 mutez.
        assert_eq!(parse_tez_to_mutez("0.0000001").unwrap(), 0);
        // "1.1234567" truncates to "1.123456" = 1_123_456 mutez.
        assert_eq!(parse_tez_to_mutez("1.1234567").unwrap(), 1_123_456);
    }

    // --- Round-trip tests ---

    #[test]
    fn roundtrip_wei() {
        for wei in [
            U256::zero(),
            U256::from(1),
            U256::exp10(12),
            U256::exp10(18),
            U256::exp10(18) + U256::exp10(17) * 5,
            U256::from(123_456_789_000_000_000_000u128),
        ] {
            let s = format_tez_from_wei(wei);
            assert_eq!(
                parse_tez_to_wei(&s).unwrap(),
                wei,
                "round-trip failed for {s}"
            );
        }
    }

    #[test]
    fn roundtrip_mutez() {
        for mutez in [0, 1, 1_000_000, 1_500_000, 123_456, u64::MAX] {
            let s = format_tez_from_mutez(mutez);
            assert_eq!(
                parse_tez_to_mutez(&s).unwrap(),
                mutez,
                "round-trip failed for {s}"
            );
        }
    }

    // --- Error cases ---

    #[test]
    fn parse_rejects_empty() {
        assert!(parse_tez_to_wei("").is_err());
    }

    #[test]
    fn parse_rejects_dot_only() {
        assert!(parse_tez_to_wei(".").is_err());
    }

    #[test]
    fn parse_rejects_negative() {
        assert!(parse_tez_to_wei("-1").is_err());
    }

    #[test]
    fn parse_rejects_leading_whitespace() {
        assert!(parse_tez_to_wei(" 1").is_err());
    }

    #[test]
    fn parse_rejects_trailing_whitespace() {
        assert!(parse_tez_to_wei("1 ").is_err());
    }

    #[test]
    fn parse_rejects_non_numeric() {
        assert!(parse_tez_to_wei("abc").is_err());
    }

    #[test]
    fn parse_rejects_multiple_dots() {
        assert!(parse_tez_to_wei("1.2.3").is_err());
    }
}
