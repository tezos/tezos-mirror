// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! HTTP header parsing for Ethereum runtime requests.
//!
//! Header name constants shared with the Tezos runtime are defined in
//! [`tezosx_interfaces`] and re-exported here for convenience.
//!
//! Required headers must be set by the caller; absent or malformed
//! headers produce a `HeaderError`.

pub use tezosx_interfaces::{
    X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER, X_TEZOS_GAS_LIMIT, X_TEZOS_SENDER,
    X_TEZOS_TIMESTAMP,
};

use alloy_primitives::{hex::FromHex, Address, U256 as AlloyU256};
use primitive_types::U256;
use tezosx_interfaces::headers::{require_str, require_u32, require_u64};
use tezosx_interfaces::TezosXRuntimeError;

/// Values contained in the `X-Tezos-*` request headers, in their Ethereum
/// runtime types.
#[derive(Debug)]
pub struct EthereumHeaders {
    /// Sender EVM address (for `msg.sender`).
    pub sender: Address,
    /// Transfer amount (for `msg.value`).
    pub amount: AlloyU256,
    /// Gas limit in gas units.
    pub gas_limit: u64,
    /// Block timestamp.
    pub timestamp: U256,
    /// Block level.
    pub block_number: U256,
}

/// Parse `X-Tezos-*` headers from `headers`.
///
/// Returns `HeaderError` if a required header is absent or any header value
/// cannot be parsed as its expected type.
pub fn parse_request_headers(
    headers: &http::HeaderMap,
) -> Result<EthereumHeaders, TezosXRuntimeError> {
    Ok(EthereumHeaders {
        sender: require_address(headers, X_TEZOS_SENDER)?,
        amount: require_u256(headers, X_TEZOS_AMOUNT)?,
        gas_limit: require_u64(headers, X_TEZOS_GAS_LIMIT)?,
        timestamp: U256::from(require_u64(headers, X_TEZOS_TIMESTAMP)?),
        block_number: U256::from(require_u32(headers, X_TEZOS_BLOCK_NUMBER)?),
    })
}

fn require_u256(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<AlloyU256, TezosXRuntimeError> {
    let s = require_str(headers, name)?;
    AlloyU256::from_str_radix(&s, 10).map_err(|_| {
        TezosXRuntimeError::HeaderError(format!(
            "Invalid {name} header value: expected u256, got {s:?}"
        ))
    })
}

fn require_address(
    headers: &http::HeaderMap,
    name: &str,
) -> Result<Address, TezosXRuntimeError> {
    let s = require_str(headers, name)?;
    Address::from_hex(&s).map_err(|e| {
        TezosXRuntimeError::HeaderError(format!(
            "Invalid {name}: expected hex EVM address: {e}"
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezosx_interfaces::headers::headers_from;

    const ADDR: &str = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045";

    /// Build a valid set of all required headers.
    fn required_headers() -> Vec<(&'static str, &'static str)> {
        vec![
            (X_TEZOS_SENDER, ADDR),
            (X_TEZOS_AMOUNT, "0"),
            (X_TEZOS_GAS_LIMIT, "1000000"),
            (X_TEZOS_TIMESTAMP, "1000000"),
            (X_TEZOS_BLOCK_NUMBER, "1"),
        ]
    }

    #[test]
    fn all_required_present() {
        let headers = headers_from(&required_headers());
        let parsed = parse_request_headers(&headers).unwrap();
        assert_eq!(parsed.sender, Address::from_hex(ADDR).unwrap());
        assert_eq!(parsed.amount, AlloyU256::ZERO);
        assert_eq!(parsed.gas_limit, 1_000_000);
        assert_eq!(parsed.timestamp, U256::from(1_000_000));
        assert_eq!(parsed.block_number, U256::from(1));
    }

    #[test]
    fn amount_parsed() {
        let mut hdrs = required_headers();
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "1000000");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.amount, AlloyU256::from(1_000_000u64));
    }

    #[test]
    fn large_amount_parsed() {
        let mut hdrs = required_headers();
        // 10^30 wei — exceeds u64::MAX, requires U256
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "1000000000000000000000000000000");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        let expected =
            AlloyU256::from_str_radix("1000000000000000000000000000000", 10).unwrap();
        assert_eq!(parsed.amount, expected);
    }

    #[test]
    fn missing_required_header_returns_header_error() {
        for required in [
            X_TEZOS_SENDER,
            X_TEZOS_AMOUNT,
            X_TEZOS_GAS_LIMIT,
            X_TEZOS_TIMESTAMP,
            X_TEZOS_BLOCK_NUMBER,
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
    fn invalid_amount_returns_header_error() {
        let mut hdrs = required_headers();
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "not-a-number");
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
            (X_TEZOS_SENDER, "not-a-hex-address");
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }
}
