// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Shared `Registry` test stubs for kernel crates.
//!
//! Replaces the per-crate `MockRegistry` / `StubRegistry` copies that
//! used to duplicate trait-conformance boilerplate. Three presets cover
//! the recurring shapes: a panicking stub, a `RuntimeNotFound` stub, and
//! a configurable mock with call tracking.

use std::cell::{Cell, RefCell};
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_journal::TezosXJournal;

use crate::{
    AliasInfo, Classification, CrossRuntimeContext, Registry, RuntimeId,
    TezosXRuntimeError, X_TEZOS_GAS_CONSUMED,
};

/// Registry whose every method panics. Use when a test needs a
/// `Registry` only to satisfy the type system and must fail loudly if
/// the registry is unexpectedly invoked.
pub struct UnimplementedRegistry;

impl Registry for UnimplementedRegistry {
    fn ensure_alias<Host>(
        &self,
        _host: &mut Host,
        _journal: &mut TezosXJournal,
        _alias_info: AliasInfo,
        _native_public_key: Option<&[u8]>,
        _target_runtime: RuntimeId,
        _context: CrossRuntimeContext,
        _gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        unimplemented!("UnimplementedRegistry::ensure_alias")
    }

    fn compute_alias(
        &self,
        _alias_info: AliasInfo,
    ) -> Result<String, TezosXRuntimeError> {
        unimplemented!("UnimplementedRegistry::compute_alias")
    }

    fn address_from_string(
        &self,
        _address_str: &str,
        _runtime_id: RuntimeId,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        unimplemented!("UnimplementedRegistry::address_from_string")
    }

    fn read_origin<Host>(
        &self,
        _host: &Host,
        _addr_runtime: RuntimeId,
        _addr: &str,
        _gas: u64,
    ) -> Result<(crate::Classification, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        unimplemented!("UnimplementedRegistry::read_origin")
    }

    fn serve<Host>(
        &self,
        _host: &mut Host,
        _journal: &mut TezosXJournal,
        _request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1,
    {
        unimplemented!("UnimplementedRegistry::serve")
    }
}

/// Registry whose every fallible method returns
/// `RuntimeNotFound(runtime_id)`, and whose `serve()` returns a 500
/// with the body `b"stub"`. Use when a test asserts that a code path
/// does not cross runtimes — a stray dispatch surfaces as a
/// well-formed error rather than a panic.
pub struct NotWiredRegistry;

impl Registry for NotWiredRegistry {
    fn ensure_alias<Host>(
        &self,
        _host: &mut Host,
        _journal: &mut TezosXJournal,
        _alias_info: AliasInfo,
        _native_public_key: Option<&[u8]>,
        target_runtime: RuntimeId,
        _context: CrossRuntimeContext,
        _gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        Err(TezosXRuntimeError::RuntimeNotFound(target_runtime))
    }

    fn compute_alias(&self, alias_info: AliasInfo) -> Result<String, TezosXRuntimeError> {
        Err(TezosXRuntimeError::RuntimeNotFound(alias_info.runtime))
    }

    fn address_from_string(
        &self,
        _address_str: &str,
        runtime_id: RuntimeId,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        Err(TezosXRuntimeError::RuntimeNotFound(runtime_id))
    }

    fn read_origin<Host>(
        &self,
        _host: &Host,
        addr_runtime: RuntimeId,
        _addr: &str,
        _gas: u64,
    ) -> Result<(crate::Classification, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        Err(TezosXRuntimeError::RuntimeNotFound(addr_runtime))
    }

    fn serve<Host>(
        &self,
        _host: &mut Host,
        _journal: &mut TezosXJournal,
        _request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1,
    {
        http::Response::builder()
            .status(http::StatusCode::INTERNAL_SERVER_ERROR)
            .body(b"stub".to_vec())
            .expect("stub response must build")
    }
}

/// Configurable success mock with call tracking.
///
/// - `ensure_alias` / `compute_alias` return `generated_alias`.
/// - `address_from_string` returns the UTF-8 bytes of the input.
/// - `serve` returns 200 OK with an empty body and
///   `X-Tezos-Gas-Consumed: 0`, or the override pair set by
///   `with_serve_response`.
///
/// `ensure_alias_calls` and `serve_calls` are `RefCell` so tests can
/// `borrow()` them after the call to assert what was dispatched.
pub struct MockRegistry {
    pub generated_alias: String,
    pub serve_override: Option<(u16, Vec<u8>)>,
    pub ensure_alias_calls: RefCell<Vec<(AliasInfo, RuntimeId)>>,
    pub serve_calls: RefCell<Vec<http::Request<Vec<u8>>>>,
}

impl MockRegistry {
    pub fn new(generated_alias: impl Into<String>) -> Self {
        Self {
            generated_alias: generated_alias.into(),
            serve_override: None,
            ensure_alias_calls: RefCell::new(Vec::new()),
            serve_calls: RefCell::new(Vec::new()),
        }
    }

    /// Replace the default 200 OK `serve()` response with a caller-
    /// chosen status code and body. No headers are added on the
    /// override path.
    pub fn with_serve_response(mut self, status: u16, body: Vec<u8>) -> Self {
        self.serve_override = Some((status, body));
        self
    }
}

impl Registry for MockRegistry {
    fn ensure_alias<Host>(
        &self,
        _host: &mut Host,
        _journal: &mut TezosXJournal,
        alias_info: AliasInfo,
        _native_public_key: Option<&[u8]>,
        target_runtime: RuntimeId,
        _context: CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        self.ensure_alias_calls
            .borrow_mut()
            .push((alias_info, target_runtime));
        Ok((self.generated_alias.clone(), gas_remaining))
    }

    fn compute_alias(
        &self,
        _alias_info: AliasInfo,
    ) -> Result<String, TezosXRuntimeError> {
        Ok(self.generated_alias.clone())
    }

    fn address_from_string(
        &self,
        address_str: &str,
        _runtime_id: RuntimeId,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        Ok(address_str.as_bytes().to_vec())
    }

    fn read_origin<Host>(
        &self,
        _host: &Host,
        _addr_runtime: RuntimeId,
        _addr: &str,
        _budget: u64,
    ) -> Result<(crate::Classification, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        Ok((crate::Classification::Unknown, 0))
    }

    fn serve<Host>(
        &self,
        _host: &mut Host,
        _journal: &mut TezosXJournal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1,
    {
        self.serve_calls.borrow_mut().push(request);
        match &self.serve_override {
            None => http::Response::builder()
                .status(200)
                .header(X_TEZOS_GAS_CONSUMED, "0")
                .body(vec![])
                .expect("default serve response must build"),
            Some((status, body)) => http::Response::builder()
                .status(*status)
                .body(body.clone())
                .expect("override serve response must build"),
        }
    }
}

/// Configurable success stub built on top of [`MockRegistry`].
///
/// Delegates `ensure_alias` and `serve` to the inner [`MockRegistry`]
/// (so tests can still `inner.ensure_alias_calls.borrow()` if they
/// need to inspect call history). Overrides the rest:
/// - `read_origin` returns `classification` on the first call and
///   `destination_classification` (falling back to `classification`)
///   on subsequent calls. Differentiates source-side from
///   destination-side reads in `resolveAddress`-style dispatch tests.
/// - `compute_alias` returns `computed_alias`. If
///   `expected_derivation_runtime` is set, asserts the caller passed
///   that runtime — catches wrong-runtime dispatch bugs.
/// - `address_from_string` rejects strings starting with `"not-"` or
///   equal to `"invalid"` with `BadRequest`; everything else is
///   accepted as UTF-8 bytes.
pub struct StubRegistry {
    pub inner: MockRegistry,
    pub classification: Classification,
    pub computed_alias: String,
    pub destination_classification: Option<Classification>,
    pub read_count: Cell<u32>,
    pub expected_derivation_runtime: Option<RuntimeId>,
    pub expected_native_address: Option<Vec<u8>>,
}

impl StubRegistry {
    pub fn with_classification(classification: Classification) -> Self {
        Self {
            inner: MockRegistry::new(""),
            classification,
            computed_alias: String::new(),
            destination_classification: None,
            read_count: Cell::new(0),
            expected_derivation_runtime: None,
            expected_native_address: None,
        }
    }

    pub fn with_alias_and_expected_runtime(
        source_classification: Classification,
        alias: &str,
        destination_classification: Option<Classification>,
        expected_derivation_runtime: RuntimeId,
    ) -> Self {
        Self {
            inner: MockRegistry::new(""),
            classification: source_classification,
            computed_alias: alias.to_string(),
            destination_classification,
            read_count: Cell::new(0),
            expected_derivation_runtime: Some(expected_derivation_runtime),
            expected_native_address: None,
        }
    }

    pub fn expecting_native_address(mut self, expected: Vec<u8>) -> Self {
        self.expected_native_address = Some(expected);
        self
    }
}

impl Registry for StubRegistry {
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
        Host: StorageV1,
    {
        self.inner.ensure_alias(
            host,
            journal,
            alias_info,
            native_public_key,
            target_runtime,
            context,
            gas_remaining,
        )
    }

    fn compute_alias(&self, alias_info: AliasInfo) -> Result<String, TezosXRuntimeError> {
        if let Some(expected) = self.expected_derivation_runtime {
            assert_eq!(
                alias_info.runtime, expected,
                "StubRegistry::compute_alias called with wrong runtime \
                 (got {:?}, expected {:?})",
                alias_info.runtime, expected,
            );
        }
        if let Some(expected) = self.expected_native_address.as_deref() {
            assert_eq!(
                alias_info.native_address.as_slice(),
                expected,
                "StubRegistry::compute_alias called with wrong native_address \
                 (got {:?}, expected {:?})",
                String::from_utf8_lossy(&alias_info.native_address),
                String::from_utf8_lossy(expected),
            );
        }
        Ok(self.computed_alias.clone())
    }

    fn address_from_string(
        &self,
        address_str: &str,
        _runtime_id: RuntimeId,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        if address_str.starts_with("not-") || address_str == "invalid" {
            Err(TezosXRuntimeError::BadRequest(format!(
                "malformed address: {address_str}"
            )))
        } else {
            Ok(address_str.as_bytes().to_vec())
        }
    }

    fn read_origin<Host>(
        &self,
        _host: &Host,
        _addr_runtime: RuntimeId,
        _addr: &str,
        _budget: u64,
    ) -> Result<(Classification, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        let count = self.read_count.get();
        self.read_count.set(count + 1);
        let classification = if count == 0 {
            self.classification.clone()
        } else {
            self.destination_classification
                .clone()
                .unwrap_or_else(|| self.classification.clone())
        };
        Ok((classification, 0))
    }

    fn serve<Host>(
        &self,
        host: &mut Host,
        journal: &mut TezosXJournal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1,
    {
        self.inner.serve(host, journal, request)
    }
}
