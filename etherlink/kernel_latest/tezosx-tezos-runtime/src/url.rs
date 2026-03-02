// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use http::Uri;
use tezos_protocol::contract::Contract;
use tezos_protocol::entrypoint::Entrypoint;
use tezosx_interfaces::TezosXRuntimeError;

/// Expected host for the Tezos runtime.
///
/// Cross-runtime HTTP calls targeting the Tezos runtime use this as the
/// authority: `http://tezos/<address>[/<entrypoint>]`.
pub const TEZOS_RUNTIME_HOST: &str = "tezos";

/// Result of parsing a Tezos runtime URI.
///
/// URI format: `http://tezos/<address>[/<entrypoint>][?<query>]`
#[derive(Debug, PartialEq)]
pub struct ParsedUrl {
    /// Destination contract address.
    pub destination: Contract,
    /// Entrypoint to call. Defaults to the `%default` entrypoint when
    /// not specified in the URL.
    pub entrypoint: Entrypoint,
}

/// Parse a Tezos runtime URI into a destination contract and entrypoint.
///
/// Expected format: `http://tezos/<address>[/<entrypoint>][?<query>]`
///
/// The address must be a valid base58check-encoded Tezos address (e.g.
/// `KT1...`, `tz1...`). The entrypoint, if present, must be a valid
/// Michelson entrypoint name. Query parameters are ignored.
pub fn parse_tezos_url(uri: &Uri) -> Result<ParsedUrl, TezosXRuntimeError> {
    if uri.host() != Some(TEZOS_RUNTIME_HOST) {
        return Err(TezosXRuntimeError::NotFound(format!(
            "Unknown host {:?}; expected {:?}",
            uri.host(),
            TEZOS_RUNTIME_HOST
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

    let (address_str, entrypoint_str) = match rest.split_once('/') {
        Some((addr, ep)) if !ep.is_empty() => (addr, Some(ep)),
        Some((addr, _)) => (addr, None),
        None => (rest, None),
    };

    let destination = Contract::from_b58check(address_str).map_err(|e| {
        TezosXRuntimeError::BadRequest(format!(
            "Invalid Tezos address in URL: {address_str}: {e}"
        ))
    })?;

    let entrypoint = match entrypoint_str {
        Some(ep) => Entrypoint::try_from(ep).map_err(|e| {
            TezosXRuntimeError::BadRequest(format!(
                "Invalid entrypoint in URL: {ep}: {e}"
            ))
        })?,
        None => Entrypoint::default(),
    };

    Ok(ParsedUrl {
        destination,
        entrypoint,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // Valid KT1 address for testing.
    const KT1: &str = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT";
    // Valid tz1 address for testing.
    const TZ1: &str = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU";

    fn parse(url: &str) -> Result<ParsedUrl, TezosXRuntimeError> {
        let uri: Uri = url.parse().unwrap();
        parse_tezos_url(&uri)
    }

    #[test]
    fn originated_address_default_entrypoint() {
        let result = parse(&format!("http://tezos/{KT1}")).unwrap();
        assert_eq!(result.destination, Contract::from_b58check(KT1).unwrap());
        assert_eq!(result.entrypoint, Entrypoint::default());
    }

    #[test]
    fn originated_address_with_entrypoint() {
        let result = parse(&format!("http://tezos/{KT1}/transfer")).unwrap();
        assert_eq!(result.destination, Contract::from_b58check(KT1).unwrap());
        assert_eq!(result.entrypoint, Entrypoint::try_from("transfer").unwrap());
    }

    #[test]
    fn trailing_slash_means_default_entrypoint() {
        let result = parse(&format!("http://tezos/{KT1}/")).unwrap();
        assert_eq!(result.entrypoint, Entrypoint::default());
    }

    #[test]
    fn implicit_address() {
        let result = parse(&format!("http://tezos/{TZ1}")).unwrap();
        assert_eq!(result.destination, Contract::from_b58check(TZ1).unwrap());
        assert_eq!(result.entrypoint, Entrypoint::default());
    }

    #[test]
    fn query_params_are_ignored() {
        let result = parse(&format!("http://tezos/{KT1}/transfer?amount=100")).unwrap();
        assert_eq!(result.entrypoint, Entrypoint::try_from("transfer").unwrap());
    }

    #[test]
    fn wrong_host() {
        assert!(parse(&format!("http://evm/{KT1}")).is_err());
    }

    #[test]
    fn missing_address() {
        assert!(parse("http://tezos/").is_err());
    }

    #[test]
    fn bare_host() {
        assert!(parse("http://tezos").is_err());
    }

    #[test]
    fn invalid_address() {
        assert!(parse("http://tezos/not_a_real_address").is_err());
    }
}
