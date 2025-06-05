// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::{error::Error, net::SocketAddr, str::FromStr, time::*};

use igd_next::{self, SearchError, SearchOptions};
use ocaml::{Pointer, Runtime, Value};

#[ocaml::sig]
pub struct Gateway(igd_next::Gateway);

unsafe impl ocaml::ToValue for Gateway {
    fn to_value(&self, rt: &Runtime) -> Value {
        Pointer::alloc(self.0.clone()).to_value(rt)
    }
}

// TODO: classify the errors
fn parse_addr(addr: &str) -> Result<SocketAddr, String> {
    SocketAddr::from_str(addr).map_err(|e| e.to_string())
}

fn parse_timeout(timeout: f64) -> Result<Duration, String> {
    Duration::try_from_secs_f64(timeout).map_err(|e| e.to_string())
}

/// Search gateway, using the given search options:
/// - bind_addr: Bind address for UDP socket (defaults to all 0.0.0.0)
/// - broadcast_address: Broadcast address for discovery packets (defaults to 239.255.255.250:1900)
/// - Timeout for a search iteration (defaults to 10s)
/// - Timeout for a single search response (defaults to 5s)
///
/// The default options should suffice in most cases.
#[ocaml::func]
#[ocaml::sig(
    "bind_addr:string option -> broadcast_address: string option -> timeout:float option -> single_search_timeout: float option -> (gateway, string) result"
)]
pub fn search_gateway(
    bind_addr: Option<&str>,
    broadcast_address: Option<&str>,
    timeout: Option<f64>,
    single_search_timeout: Option<f64>,
) -> Result<Gateway, String> {
    let default = SearchOptions::default();

    let bind_addr = bind_addr.map_or(Ok(default.bind_addr), |addr| parse_addr(addr))?;

    let broadcast_address =
        broadcast_address.map_or(Ok(default.broadcast_address), |addr| parse_addr(addr))?;

    let timeout = timeout.map_or(Ok(default.timeout), |timeout| {
        parse_timeout(timeout).map(Some)
    })?;

    let single_search_timeout = single_search_timeout
        .map_or(Ok(default.single_search_timeout), |timeout| {
            parse_timeout(timeout).map(Some)
        })?;

    let options = SearchOptions {
        // Bind address for UDP socket (defaults to all 0.0.0.0)
        bind_addr,
        // Broadcast address for discovery packets (defaults to 239.255.255.250:1900)
        broadcast_address,
        // Timeout for a search iteration (defaults to 10s)
        timeout,
        // Timeout for a single search response (defaults to 5s)
        single_search_timeout,
    };

    match igd_next::search_gateway(options) {
        Ok(g) => Ok(Gateway(g)),
        Err(err) => Err(err.to_string()),
    }
}
