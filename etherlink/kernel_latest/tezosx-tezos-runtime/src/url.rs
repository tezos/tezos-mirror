// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use http::Uri;
use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
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

/// Result of parsing a Tezos runtime view URI.
///
/// URI format: `http://tezos/<kt1>/<view_name>`. Unlike
/// [`parse_tezos_url`], the view name is surfaced as a raw string
/// instead of being re-parsed as a Michelson entrypoint: view names
/// and entrypoint names share most of their charset but not all
/// (view names allow `.`/`%`/`@` in the first position; entrypoint
/// names don't), and critically the literal view name `"default"`
/// must stay reachable — the entrypoint parser collapses both `""`
/// and `"default"` into [`Entrypoint::default`], which would make a
/// view literally named `"default"` unaddressable.
#[derive(Debug, PartialEq)]
pub struct ParsedViewUrl {
    /// Destination contract. Views live only on originated contracts.
    pub destination: ContractKt1Hash,
    /// Raw view name (third path segment, always non-empty).
    pub view_name: String,
}

/// Parse a Tezos runtime URI for a view call.
///
/// Expected format: `http://tezos/<kt1>/<view_name>[?<query>]`. The
/// destination must be an originated (KT1) address and the view name
/// must be present and non-empty. Query parameters are ignored.
pub fn parse_tezos_view_url(uri: &Uri) -> Result<ParsedViewUrl, TezosXRuntimeError> {
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

    let (address_str, view_name) = match rest.split_once('/') {
        Some((addr, name)) if !name.is_empty() => (addr, name.to_owned()),
        _ => {
            return Err(TezosXRuntimeError::BadRequest(
                "view name is required (URL: http://tezos/<kt1>/<view_name>)".into(),
            ));
        }
    };

    // Views live only on originated contracts. Parse the address as
    // KT1 directly (instead of as a generic `Contract`) so the
    // "implicit address" path returns an informative error.
    let destination = ContractKt1Hash::from_b58check(address_str).map_err(|e| {
        TezosXRuntimeError::BadRequest(format!(
            "views require an originated (KT1) destination, got {address_str}: {e}"
        ))
    })?;

    Ok(ParsedViewUrl {
        destination,
        view_name,
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

    // --- parse_tezos_view_url ---

    fn parse_view(url: &str) -> Result<ParsedViewUrl, TezosXRuntimeError> {
        let uri: Uri = url.parse().unwrap();
        parse_tezos_view_url(&uri)
    }

    #[test]
    fn view_url_parses_view_name() {
        let result = parse_view(&format!("http://tezos/{KT1}/my_view")).unwrap();
        assert_eq!(
            result.destination,
            ContractKt1Hash::from_b58check(KT1).unwrap()
        );
        assert_eq!(result.view_name, "my_view");
    }

    #[test]
    fn view_url_preserves_literal_default_name() {
        // A view literally named "default" must be reachable — the
        // motivating case for a dedicated view URL parser.
        let result = parse_view(&format!("http://tezos/{KT1}/default")).unwrap();
        assert_eq!(result.view_name, "default");
    }

    #[test]
    fn view_url_rejects_missing_view_name() {
        assert!(parse_view(&format!("http://tezos/{KT1}")).is_err());
        assert!(parse_view(&format!("http://tezos/{KT1}/")).is_err());
    }

    #[test]
    fn view_url_rejects_implicit_destination() {
        assert!(parse_view(&format!("http://tezos/{TZ1}/any_view")).is_err());
    }

    #[test]
    fn view_url_ignores_query() {
        let result = parse_view(&format!("http://tezos/{KT1}/my_view?arg=1")).unwrap();
        assert_eq!(result.view_name, "my_view");
    }

    #[test]
    fn view_url_wrong_host() {
        assert!(parse_view(&format!("http://evm/{KT1}/my_view")).is_err());
    }
}
