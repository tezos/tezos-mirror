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
use tezosx_interfaces::headers::{
    parse_tez_to_mutez, require_i64, require_str, require_u32, require_u64,
};
use tezosx_interfaces::TezosXRuntimeError;

use crate::NULL_PKH;

/// Values contained in the `X-Tezos-*` request headers, in their Tezos runtime
/// types. Used in parsing and inserting.
#[derive(Debug)]
pub struct MichelsonHeaders {
    /// Transfer amount in mutez.
    pub amount: Narith,
    /// Gas limit in milligas.
    pub gas_limit: u64,
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
        amount: Narith(
            parse_tez_to_mutez(&require_str(headers, X_TEZOS_AMOUNT)?)?.into(),
        ),
        gas_limit: require_u64(headers, X_TEZOS_GAS_LIMIT)?,
        timestamp: Timestamp::from(require_i64(headers, X_TEZOS_TIMESTAMP)?),
        block_number: BlockNumber::from(require_u32(headers, X_TEZOS_BLOCK_NUMBER)?),
        sender: require_kt1(headers, X_TEZOS_SENDER)?,
        // FIXME: We use the Tezos null address as source because the alias of
        // the 0x EVM source account is a KT1, and Michelson doesn't allow KT1
        // as a source. If the original call was emitted from Michelson (i.e. it
        // hit EVM before re-entering the Michelson runtime), we might want to
        // retrieve the original tz source in the future.
        source: Some(PublicKeyHash::from_b58check(NULL_PKH).map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to parse null address: {e}"
            ))
        })?),
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

#[cfg(test)]
mod tests {
    use super::*;
    use tezosx_interfaces::headers::headers_from;

    const KT1: &str = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT";
    const TZ1: &str = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU";

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
        assert_eq!(parsed.gas_limit, 1_000_000u64);
        assert_eq!(parsed.timestamp, Timestamp::from(1_000_000_i64));
        assert_eq!(parsed.block_number, BlockNumber::from(1u32));
        assert_eq!(parsed.sender, ContractKt1Hash::from_b58check(KT1).unwrap());
        assert_eq!(
            parsed.source,
            Some(PublicKeyHash::from_b58check(NULL_PKH).unwrap())
        );
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
        // 1 TEZ = 1_000_000 mutez
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "1");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.amount, Narith(1_000_000u64.into()));
    }

    #[test]
    fn amount_parsed_fractional() {
        let mut hdrs = required_headers();
        // 1.5 TEZ = 1_500_000 mutez
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "1.5");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.amount, Narith(1_500_000u64.into()));
    }

    #[test]
    fn amount_rejects_excess_decimals() {
        let mut hdrs = required_headers();
        // 7 decimals — exceeds Michelson's 6-decimal limit
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "0.0000001");
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
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

    // --- Amount edge cases ---

    #[test]
    fn amount_zero_parsed_correctly() {
        let hdrs = required_headers(); // default amount is "0"
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.amount, Narith(0u64.into()));
    }

    #[test]
    fn amount_one_mutez() {
        let mut hdrs = required_headers();
        // 0.000001 TEZ = 1 mutez
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "0.000001");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.amount, Narith(1u64.into()));
    }

    #[test]
    fn amount_rejects_negative() {
        let mut hdrs = required_headers();
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "-1");
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }

    #[test]
    fn amount_rejects_whitespace() {
        let mut hdrs = required_headers();
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, " 1 ");
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }

    // --- Gas limit edge cases ---

    #[test]
    fn gas_limit_zero() {
        let mut hdrs = required_headers();
        *hdrs
            .iter_mut()
            .find(|(k, _)| *k == X_TEZOS_GAS_LIMIT)
            .unwrap() = (X_TEZOS_GAS_LIMIT, "0");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.gas_limit, 0);
    }

    #[test]
    fn gas_limit_max_u64() {
        let mut hdrs = required_headers();
        *hdrs
            .iter_mut()
            .find(|(k, _)| *k == X_TEZOS_GAS_LIMIT)
            .unwrap() = (X_TEZOS_GAS_LIMIT, "18446744073709551615");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.gas_limit, u64::MAX);
    }

    #[test]
    fn gas_limit_overflow_u64() {
        let mut hdrs = required_headers();
        *hdrs
            .iter_mut()
            .find(|(k, _)| *k == X_TEZOS_GAS_LIMIT)
            .unwrap() = (X_TEZOS_GAS_LIMIT, "18446744073709551616"); // u64::MAX + 1
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }

    // --- Block number edge cases ---

    #[test]
    fn block_number_zero() {
        let mut hdrs = required_headers();
        *hdrs
            .iter_mut()
            .find(|(k, _)| *k == X_TEZOS_BLOCK_NUMBER)
            .unwrap() = (X_TEZOS_BLOCK_NUMBER, "0");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.block_number, BlockNumber::from(0u32));
    }

    #[test]
    fn block_number_max_u32() {
        let mut hdrs = required_headers();
        *hdrs
            .iter_mut()
            .find(|(k, _)| *k == X_TEZOS_BLOCK_NUMBER)
            .unwrap() = (X_TEZOS_BLOCK_NUMBER, "4294967295"); // u32::MAX
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.block_number, BlockNumber::from(u32::MAX));
    }

    // --- Timestamp edge cases ---

    #[test]
    fn timestamp_zero() {
        let mut hdrs = required_headers();
        *hdrs
            .iter_mut()
            .find(|(k, _)| *k == X_TEZOS_TIMESTAMP)
            .unwrap() = (X_TEZOS_TIMESTAMP, "0");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.timestamp, Timestamp::from(0_i64));
    }

    // --- Sender edge cases ---

    #[test]
    fn sender_tz1_address_rejected() {
        let mut hdrs = required_headers();
        // tz1 is not a valid KT1 address
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_SENDER).unwrap() =
            (X_TEZOS_SENDER, "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU");
        let err = parse_request_headers(&headers_from(&hdrs)).unwrap_err();
        assert!(matches!(err, TezosXRuntimeError::HeaderError(_)));
    }
}
