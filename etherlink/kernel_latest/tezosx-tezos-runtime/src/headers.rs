// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! HTTP header parsing for Tezos runtime requests.
//!
//! Header name constants are defined in [`tezosx_interfaces`] and
//! re-exported here for convenience. Required headers must be set by the
//! caller; absent or malformed headers produce a `HeaderError`.

pub use tezosx_interfaces::{
    X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER, X_TEZOS_CRAC_ID, X_TEZOS_GAS_LIMIT,
    X_TEZOS_SENDER, X_TEZOS_SOURCE, X_TEZOS_TIMESTAMP,
};

use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_data_encoding::types::Narith;
use tezos_smart_rollup::types::Timestamp;
use tezos_tezlink::enc_wrappers::BlockNumber;
use tezosx_interfaces::headers::{
    parse_str, parse_tez_to_mutez, require_i64, require_str, require_u32, require_u64,
};
use tezosx_interfaces::TezosXRuntimeError;

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
    /// CRAC origin KT1 contract address parsed from `X-Tezos-Source`.
    /// Used for CRAC receipt construction (alias of E_0).
    pub crac_origin_contract: Option<ContractKt1Hash>,
    /// Propagated CRAC-ID string from an incoming cross-runtime call.
    /// Kept as a raw string; verified by `journal.verify_crac_id()`.
    /// `None` if the header is absent (non-CRAC request).
    pub crac_id: Option<String>,
}

/// Parse `X-Tezos-*` headers from `headers`.
///
/// Returns `HeaderError` if a required header is absent or any header value
/// cannot be parsed as its expected type.
pub fn parse_request_headers(
    headers: &http::HeaderMap,
) -> Result<MichelsonHeaders, TezosXRuntimeError> {
    // Parse X-Tezos-Source as a KT1 if present (for CRAC receipt construction).
    // The EVM gateway sends the source alias as a KT1 string. If the value
    // is a tz1/tz2/tz3 (non-CRAC case), the KT1 parse will fail and we
    // set crac_origin_contract to None.
    let source_str = parse_str(headers, X_TEZOS_SOURCE)?;
    let crac_origin_contract = source_str
        .as_deref()
        .and_then(|s| ContractKt1Hash::from_b58check(s).ok());

    let crac_id = parse_str(headers, X_TEZOS_CRAC_ID)?;

    Ok(MichelsonHeaders {
        amount: Narith(
            parse_tez_to_mutez(&require_str(headers, X_TEZOS_AMOUNT)?)?.into(),
        ),
        gas_limit: require_u64(headers, X_TEZOS_GAS_LIMIT)?,
        timestamp: Timestamp::from(require_i64(headers, X_TEZOS_TIMESTAMP)?),
        block_number: BlockNumber::from(require_u32(headers, X_TEZOS_BLOCK_NUMBER)?),
        sender: require_kt1(headers, X_TEZOS_SENDER)?,
        crac_origin_contract,
        crac_id,
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
        assert!(
            parsed.crac_origin_contract.is_none(),
            "no source → no CRAC origin contract"
        );
        assert!(parsed.crac_id.is_none(), "no source → no CRAC-ID");
    }

    #[test]
    fn source_optional_tz1() {
        // tz1 source (non-CRAC): source_contract and crac_id should be None
        let mut hdrs = required_headers();
        hdrs.push((X_TEZOS_SOURCE, TZ1));
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert!(parsed.crac_origin_contract.is_none());
        assert!(parsed.crac_id.is_none());
    }

    #[test]
    fn source_kt1_with_crac_id() {
        // KT1 source (CRAC): crac_origin_contract and crac_id should be set
        let mut hdrs: Vec<(&str, &str)> = required_headers();
        hdrs.push((X_TEZOS_SOURCE, KT1));
        hdrs.push((X_TEZOS_CRAC_ID, "0-5"));
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(
            parsed.crac_origin_contract,
            Some(ContractKt1Hash::from_b58check(KT1).unwrap())
        );
        assert_eq!(parsed.crac_id, Some("0-5".to_string()));
    }

    #[test]
    fn amount_parsed() {
        let mut hdrs = required_headers();
        // 1 TEZ = 1_000_000 mutez
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "1");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.amount, Narith(1_000_000u64.into()));
        assert!(parsed.crac_origin_contract.is_none());
        assert!(parsed.crac_id.is_none());
    }

    #[test]
    fn amount_parsed_fractional() {
        let mut hdrs = required_headers();
        // 1.5 TEZ = 1_500_000 mutez
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "1.5");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.amount, Narith(1_500_000u64.into()));
        assert!(parsed.crac_origin_contract.is_none());
        assert!(parsed.crac_id.is_none());
    }

    #[test]
    fn amount_truncates_excess_decimals() {
        let mut hdrs = required_headers();
        // 7 decimals — exceeds Michelson's 6-decimal limit; truncated to 0 (ADR L2-1004)
        *hdrs.iter_mut().find(|(k, _)| *k == X_TEZOS_AMOUNT).unwrap() =
            (X_TEZOS_AMOUNT, "0.0000001");
        let parsed = parse_request_headers(&headers_from(&hdrs)).unwrap();
        assert_eq!(parsed.amount, Narith(0u64.into()));
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
