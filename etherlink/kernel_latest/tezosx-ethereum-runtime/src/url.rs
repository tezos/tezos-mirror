// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_primitives::{hex::FromHex, Address};
use http::Uri;
use tezosx_interfaces::TezosXRuntimeError;

/// Expected host for the Ethereum runtime.
///
/// Cross-runtime HTTP calls targeting the Ethereum runtime use this as
/// the authority: `http://ethereum/<address>`.
pub const ETHEREUM_RUNTIME_HOST: &str = "ethereum";

/// Result of parsing an Ethereum runtime URI.
///
/// URI format: `http://ethereum/<hex-address>`
#[derive(Debug, PartialEq)]
pub struct ParsedUrl {
    /// Destination EVM contract address (20 bytes).
    pub destination: Address,
}

/// Parse an Ethereum runtime URI into a destination contract address.
///
/// Expected format: `http://ethereum/<hex-address>`
///
/// The address must be a valid hex-encoded EVM address (with or without
/// `0x` prefix). Query parameters are ignored.
pub fn parse_ethereum_url(uri: &Uri) -> Result<ParsedUrl, TezosXRuntimeError> {
    if uri.host() != Some(ETHEREUM_RUNTIME_HOST) {
        return Err(TezosXRuntimeError::NotFound(format!(
            "Unknown host {:?}; expected {:?}",
            uri.host(),
            ETHEREUM_RUNTIME_HOST
        )));
    }

    let path = uri.path();

    let rest = match path.strip_prefix('/') {
        Some(r) if !r.is_empty() => r,
        _ => {
            return Err(TezosXRuntimeError::BadRequest(
                "Missing address in URL".into(),
            ))
        }
    };

    // Strip trailing slash if present
    let address_str = rest.trim_end_matches('/');
    if address_str.is_empty() {
        return Err(TezosXRuntimeError::BadRequest(
            "Missing address in URL".into(),
        ));
    }

    let destination = Address::from_hex(address_str).map_err(|e| {
        TezosXRuntimeError::BadRequest(format!(
            "Invalid EVM address in URL: {address_str}: {e}"
        ))
    })?;

    Ok(ParsedUrl { destination })
}

#[cfg(test)]
mod tests {
    use super::*;

    const ADDR: &str = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045";

    fn parse(url: &str) -> Result<ParsedUrl, TezosXRuntimeError> {
        let uri: Uri = url.parse().unwrap();
        parse_ethereum_url(&uri)
    }

    #[test]
    fn valid_address() {
        let result = parse(&format!("http://ethereum/{ADDR}")).unwrap();
        assert_eq!(result.destination, Address::from_hex(ADDR).unwrap());
    }

    #[test]
    fn trailing_slash() {
        let result = parse(&format!("http://ethereum/{ADDR}/")).unwrap();
        assert_eq!(result.destination, Address::from_hex(ADDR).unwrap());
    }

    #[test]
    fn query_params_are_ignored() {
        let result = parse(&format!("http://ethereum/{ADDR}?amount=100")).unwrap();
        assert_eq!(result.destination, Address::from_hex(ADDR).unwrap());
    }

    #[test]
    fn wrong_host() {
        assert!(parse(&format!("http://tezos/{ADDR}")).is_err());
    }

    #[test]
    fn missing_address() {
        assert!(parse("http://ethereum/").is_err());
    }

    #[test]
    fn bare_host() {
        assert!(parse("http://ethereum").is_err());
    }

    #[test]
    fn invalid_address() {
        assert!(parse("http://ethereum/not_a_real_address").is_err());
    }
}
