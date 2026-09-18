// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Traits binding TezosX runtimes to a shared journal. Pure types
//! shared with [`tezosx-journal`] live in [`tezosx-types`] and are
//! re-exported here for convenience — existing consumers can keep
//! `use tezosx_interfaces::{RuntimeId, ...}` unchanged.

pub use tezosx_types::headers;
pub use tezosx_types::{
    canonicalize_native_address, resolve_routing, AliasInfo, Classification,
    CrossRuntimeContext, EvmGas, Gas, KernelStorageError, Milligas, Origin,
    OriginalSource, RoutingDecision, RuntimeId, TezosXRuntimeError, ALIAS_LOOKUP_COST,
    ERR_FORBIDDEN_TEZOS_HEADER, ERR_SAME_RUNTIME_NAC, MAX_CRAC_DEPTH, X_TEZOS_AMOUNT,
    X_TEZOS_BLOCK_NUMBER, X_TEZOS_CRAC_DEPTH, X_TEZOS_CRAC_ID, X_TEZOS_GAS_CONSUMED,
    X_TEZOS_GAS_LIMIT, X_TEZOS_SENDER, X_TEZOS_SOURCE, X_TEZOS_SOURCE_RUNTIME,
    X_TEZOS_STORAGE_COST, X_TEZOS_TIMESTAMP,
};

#[cfg(feature = "testing")]
use primitive_types::U256;
use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{KeySpace, KeySpaceLoader};

/// Result of an alias-resolution call.
///
/// Returned alongside the alias string by `Registry::ensure_alias` and
/// `RuntimeInterface::ensure_alias`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasResolution {
    /// Gas remaining after the resolution work has been deducted, in the
    /// target runtime's unit.
    pub gas_remaining: Gas,
    /// Storage cost the target runtime asks the caller to bill, in
    /// mutez, for bytes allocated during this resolution. `None` when
    /// the target runtime makes no claim on the caller for this
    /// materialization.
    pub delegated_storage_cost: Option<u64>,
}

impl AliasResolution {
    pub fn build(gas_remaining: Gas) -> Self {
        Self {
            gas_remaining,
            delegated_storage_cost: None,
        }
    }

    pub fn build_with_delegated_storage_cost(
        gas_remaining: Gas,
        delegated_storage_cost: u64,
    ) -> Self {
        Self {
            gas_remaining,
            delegated_storage_cost: Some(delegated_storage_cost),
        }
    }
}

pub trait Registry {
    type Journal;

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
    /// Returns `(alias, AliasResolution)`. See [`AliasResolution`] for
    /// the meaning of each field. On a fresh deploy the behavior matches
    /// the legacy alias generation. When the alias is already classified,
    /// the call is a no-op and the gas budget is returned unchanged.
    /// When a forwarder exists but the classification path is empty
    /// (a legacy account from before this work), the call writes the
    /// classification only and skips the redeploy.
    #[allow(clippy::too_many_arguments)]
    fn ensure_alias<Host>(
        &self,
        rk: &mut RuntimeKeyspaces<'_, Host, Host::KeySpace>,
        journal: &mut Self::Journal,
        alias_info: AliasInfo,
        native_public_key: Option<&[u8]>,
        target_runtime: RuntimeId,
        context: CrossRuntimeContext,
        gas_remaining: Gas,
    ) -> Result<(String, AliasResolution), TezosXRuntimeError>
    where
        Host: StorageV1 + KeySpaceLoader;

    fn alias_exists<Host>(
        &self,
        rk: &mut RuntimeKeyspaces<'_, Host, Host::KeySpace>,
        journal: &mut Self::Journal,
        target_runtime: RuntimeId,
        alias: &str,
    ) -> Result<bool, TezosXRuntimeError>
    where
        Host: StorageV1 + KeySpaceLoader;

    /// Derive the alias naming `alias_info.native_address` inside
    /// `alias_info.runtime`.
    ///
    /// Note the direction: the `runtime` field here is the runtime the
    /// alias *lives in* (the target), not the native address's own
    /// runtime. It is the opposite of the [`AliasInfo`] stored in an
    /// `Origin::Alias` record, whose `runtime` names the source.
    fn compute_alias(&self, alias_info: &AliasInfo)
        -> Result<String, TezosXRuntimeError>;

    fn address_from_string(
        &self,
        address_str: &str,
        runtime_id: RuntimeId,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;

    /// Read the classification of `addr` in `addr_runtime`.
    ///
    /// `addr_runtime` is the dispatch key: the request is forwarded to the
    /// per-runtime impl that owns `addr`'s address format and storage layout.
    ///
    /// `budget` and the returned `consumed` value carry their own unit, so
    /// no conversion is needed at the boundary. `consumed` is what
    /// `read_origin` charged from the budget for its internal work — the
    /// primary `/origin` lookup, plus the EVM code-presence back-stop when
    /// it fires. `consumed ≤ budget` on the `Ok` path; insufficient budget
    /// returns `Err(OutOfGas)`.
    fn read_origin<Host, KS>(
        &self,
        rk: &RuntimeKeyspaces<'_, Host, KS>,
        addr_runtime: RuntimeId,
        addr: &str,
        budget: Gas,
    ) -> Result<(Classification, Gas /* consumed */), TezosXRuntimeError>
    where
        Host: StorageV1,
        KS: KeySpace;

    /// Route an HTTP request to the appropriate runtime based on the URL host.
    fn serve<Host, KS>(
        &self,
        rk: &mut RuntimeKeyspaces<'_, Host, KS>,
        journal: &mut Self::Journal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1 + KeySpaceLoader<KeySpace = KS>,
        KS: KeySpace;
}

pub trait RuntimeInterface {
    type Journal;

    /// Materialize `alias` — already derived by the registry and named
    /// in this runtime — for the account `alias_info` describes.
    #[allow(clippy::too_many_arguments)]
    fn create_alias<Host, KS>(
        &self,
        registry: &impl Registry<Journal = Self::Journal>,
        rk: &mut RuntimeKeyspaces<'_, Host, KS>,
        journal: &mut Self::Journal,
        alias: &str,
        alias_info: AliasInfo,
        native_public_key: Option<&[u8]>,
        context: CrossRuntimeContext,
        gas_remaining: Gas,
    ) -> Result<AliasResolution, TezosXRuntimeError>
    where
        Host: StorageV1 + KeySpaceLoader<KeySpace = KS>,
        KS: KeySpace;

    fn alias_exists<Host, KS>(
        &self,
        rk: &mut RuntimeKeyspaces<'_, Host, KS>,
        journal: &mut Self::Journal,
        alias: &str,
    ) -> Result<bool, TezosXRuntimeError>
    where
        Host: StorageV1,
        KS: KeySpace;

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
    fn serve<Host, KS>(
        &self,
        registry: &impl Registry<Journal = Self::Journal>,
        rk: &mut RuntimeKeyspaces<'_, Host, KS>,
        journal: &mut Self::Journal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1 + KeySpaceLoader<KeySpace = KS>,
        KS: KeySpace;

    /// The URL host that identifies this runtime in HTTP requests routed
    /// by the registry (e.g. `"tezos"`, `"ethereum"`).
    fn host(&self) -> &'static str;

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;

    /// Read the classification of `addr` in this runtime.
    ///
    /// `budget` carries its own unit; `consumed` is returned in this
    /// runtime's unit. Malformed addresses short-circuit to
    /// `(Unknown, Gas::ZERO)` — no storage read, no charge.
    ///
    /// For the EVM runtime, when the account is unclassified **and**
    /// exposes non-empty bytecode (CREATE contract or EIP-7702 SET_CODE
    /// delegation), the back-stop fires and returns `Native`. A single
    /// account-record read serves the whole lookup, charged
    /// `ALIAS_LOOKUP_COST`. Returns `OutOfGas` when the budget is
    /// insufficient.
    ///
    /// For the Tezos runtime, no back-stop is applied — a storage miss
    /// returns `Unknown` after charging `ALIAS_LOOKUP_COST`.
    fn read_origin<Host, KS>(
        &self,
        rk: &RuntimeKeyspaces<'_, Host, KS>,
        addr: &str,
        budget: Gas,
    ) -> Result<(Classification, Gas /* consumed */), TezosXRuntimeError>
    where
        Host: StorageV1,
        KS: KeySpace;

    #[cfg(feature = "testing")]
    fn string_from_address(&self, address: &[u8]) -> Result<String, TezosXRuntimeError>;

    #[cfg(feature = "testing")]
    fn get_balance(
        &self,
        host: &mut impl StorageV1,
        address: &[u8],
    ) -> Result<U256, TezosXRuntimeError>;
}

/// Translate the captured original source into its address in `target`.
///
/// [`OriginalSource`] stores the originator's address in its own native
/// runtime ([`OriginalSource::runtime`]), the canonical form from which
/// every runtime's alias is deterministically derived. So:
/// - when `target` is that native runtime, the native address is the
///   answer directly — a "straightforward conversion", no work;
/// - otherwise the answer is [`Registry::compute_alias`] of the native
///   address bytes, which reproduces the materialized alias the
///   state-mutating path would have written — a pure hash, no durable
///   `get_origin` read.
///
/// This is the on-demand translation that lets the journal cache only the
/// native `(runtime, address)` pair instead of every runtime's alias.
pub fn translate_original_source<R: Registry>(
    registry: &R,
    source: &OriginalSource,
    target: RuntimeId,
) -> Result<String, TezosXRuntimeError> {
    if target == source.runtime() {
        Ok(source.original_address().to_string())
    } else {
        registry.compute_alias(&AliasInfo {
            runtime: target,
            native_address: source.original_address().to_string(),
        })
    }
}

#[cfg(feature = "testing")]
pub mod testing;
