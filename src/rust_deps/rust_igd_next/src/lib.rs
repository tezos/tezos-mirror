// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::{error::Error, net::SocketAddr, str::FromStr, time::*};

use igd_next::{self, PortMappingProtocol, SearchError, SearchOptions};
use ocaml::{Pointer, Runtime, Value};

#[ocaml::sig]
pub struct Gateway(igd_next::Gateway);

ocaml::custom!(Gateway);

unsafe impl ocaml::ToValue for Gateway {
    fn to_value(&self, rt: &Runtime) -> Value {
        Pointer::alloc(self.0.clone()).to_value(rt)
    }
}

unsafe impl ocaml::FromValue for Gateway {
    fn from_value(v: Value) -> Self {
        unsafe {
            let ptr: *const igd_next::Gateway = v.abstract_ptr_val();
            Gateway((*ptr).clone())
        }
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

#[ocaml::sig("Tcp | Udp")]
#[derive(ocaml::FromValue, ocaml::ToValue)]
pub enum Protocol {
    Tcp,
    Udp,
}

impl From<Protocol> for PortMappingProtocol {
    fn from(p: Protocol) -> Self {
        match p {
            Protocol::Tcp => PortMappingProtocol::TCP,
            Protocol::Udp => PortMappingProtocol::UDP,
        }
    }
}

/// Add a port mapping.
///
/// The local_addr:local_port is the address where the traffic is sent to.
/// The lease_duration parameter is in seconds. A value of 0 is infinite or
/// 604800 seconds, according to the gateway version.
#[ocaml::func]
#[ocaml::sig(
    "gateway -> protocol -> local_addr:string -> local_port:int -> external_port:int -> lease_duration:int32 -> description:string -> (unit, string) result"
)]
pub fn gateway_map_port(
    gateway: &Gateway,
    protocol: Protocol,
    local_addr: &str,
    local_port: u16,
    external_port: u16,
    lease_duration: u32,
    description: &str,
) -> Result<(), String> {
    let local_addr = parse_addr(&format!("{local_addr}:{local_port}"))?;
    gateway
        .0
        .add_port(
            protocol.into(),
            external_port,
            local_addr,
            lease_duration,
            description,
        )
        .map_err(|e| e.to_string())
}

/// Add a port mapping with any external port.
///
/// The local_addr:local_port is the address where the traffic is sent to.
/// The lease_duration parameter is in seconds. A value of 0 is infinite or
/// 604800 seconds, according to the gateway version.
///
/// # Returns
///
/// The external port that was mapped on success. Otherwise an error.
#[ocaml::func]
#[ocaml::sig(
    "gateway -> protocol -> local_addr:string -> local_port:int -> lease_duration:int32 -> description:string -> (int, string) result"
)]
pub fn gateway_map_any_port(
    gateway: &Gateway,
    protocol: Protocol,
    local_addr: &str,
    local_port: u16,
    lease_duration: u32,
    description: &str,
) -> Result<u16, String> {
    let local_addr = parse_addr(&format!("{local_addr}:{local_port}"))?;
    gateway
        .0
        .add_any_port(protocol.into(), local_addr, lease_duration, description)
        .map_err(|e| e.to_string())
}

#[ocaml::func]
#[ocaml::sig("gateway -> string")]
pub fn gateway_ip(gateway: &Gateway) -> String {
    gateway.0.addr.ip().to_string()
}
