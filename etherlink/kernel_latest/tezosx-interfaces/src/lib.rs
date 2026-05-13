// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Traits binding TezosX runtimes to a shared journal. Pure types
//! shared with [`tezosx-journal`] live in [`tezosx-types`] and are
//! re-exported here for convenience — existing consumers can keep
//! `use tezosx_interfaces::{RuntimeId, ...}` unchanged.

pub use tezosx_types::headers;
pub use tezosx_types::{
    gas, resolve_routing, AliasInfo, CrossRuntimeContext, Origin, RoutingDecision,
    RuntimeId, TezosXRuntimeError, ERR_FORBIDDEN_TEZOS_HEADER, X_TEZOS_AMOUNT,
    X_TEZOS_BLOCK_NUMBER, X_TEZOS_CRAC_ID, X_TEZOS_GAS_CONSUMED, X_TEZOS_GAS_LIMIT,
    X_TEZOS_SENDER, X_TEZOS_SOURCE, X_TEZOS_TIMESTAMP,
};

#[cfg(feature = "testing")]
use primitive_types::U256;
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_journal::TezosXJournal;

pub trait Registry {
    /// Idempotently ensure that the alias of `alias_info` exists in
    /// `target_runtime`, materializing the forwarder and recording the
    /// classification record if needed.
    ///
    /// `alias_info` carries the source runtime where the native account
    /// lives and the UTF-8 bytes of its address. `target_runtime`
    /// selects the runtime that will host the alias.
    ///
    /// `gas_remaining` is the caller's remaining gas budget in the
    /// target runtime gas units (milligas for Tezos, EVM gas for
    /// Ethereum). The function consumes gas incrementally and fails
    /// early if the budget is exceeded.
    ///
    /// Returns `(alias, gas_remaining_after)`. On a fresh deploy the
    /// behavior matches the legacy alias generation. When the alias is
    /// already classified, the call is a no-op and the gas budget is
    /// returned unchanged. When a forwarder exists but the
    /// classification path is empty (a legacy account from before this
    /// work), the call writes the classification only and skips the
    /// redeploy.
    #[allow(clippy::too_many_arguments)]
    fn ensure_alias<Host>(
        &self,
        host: &mut Host,
        journal: &mut TezosXJournal,
        alias_info: AliasInfo,
        native_public_key: Option<&[u8]>,
        target_runtime: RuntimeId,
        context: CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1;

    fn compute_alias(&self, alias_info: AliasInfo) -> Result<String, TezosXRuntimeError>;

    fn address_from_string(
        &self,
        address_str: &str,
        runtime_id: RuntimeId,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;

    /// Route an HTTP request to the appropriate runtime based on the URL host.
    fn serve<Host>(
        &self,
        host: &mut Host,
        journal: &mut TezosXJournal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1;
}

pub trait RuntimeInterface {
    /// Idempotently ensure that the alias of `alias_info` exists in
    /// this runtime. `alias_info.runtime` is the source runtime where
    /// the underlying native account lives, and `alias_info.native_address`
    /// holds the UTF-8 bytes of its canonical address string.
    ///
    /// `gas_remaining` is the caller's remaining budget in this runtime
    /// gas units. Returns `(alias, gas_remaining_after)`. Fails if the
    /// budget is exceeded.
    ///
    /// Three branches:
    /// - if the alias account already has an alias classification
    ///   recorded, the call is a no-op and returns the address with
    ///   `gas_remaining` unchanged;
    /// - if the alias account exists with forwarder bytecode but the
    ///   classification path is empty (a legacy account from before
    ///   this work), the call writes the classification only and
    ///   skips the redeploy;
    /// - otherwise the call deploys the forwarder and records the
    ///   classification.
    #[allow(clippy::too_many_arguments)]
    fn ensure_alias<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        alias_info: AliasInfo,
        native_public_key: Option<&[u8]>,
        context: CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1;

    fn compute_alias(&self, native_address: &[u8]) -> Result<String, TezosXRuntimeError>;

    /// Handle an incoming cross-runtime HTTP request.
    ///
    /// The request URL encodes the destination address and optional entrypoint.
    /// The body contains the payload in the target runtime's native encoding.
    ///
    /// All call context is carried in HTTP headers — there is no separate
    /// context parameter.
    ///
    /// Returns an HTTP response with a status code indicating success (200) or
    /// failure (4xx/5xx), along with runtime-specific response headers and body.
    fn serve<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1;

    /// The URL host that identifies this runtime in HTTP requests routed
    /// by the registry (e.g. `"tezos"`, `"ethereum"`).
    fn host(&self) -> &'static str;

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;

    #[cfg(feature = "testing")]
    fn string_from_address(&self, address: &[u8]) -> Result<String, TezosXRuntimeError>;

    #[cfg(feature = "testing")]
    fn get_balance(
        &self,
        host: &mut impl StorageV1,
        address: &[u8],
    ) -> Result<U256, TezosXRuntimeError>;
}
