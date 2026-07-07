// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::fmt;

use crate::{EvmJournal, MichelsonJournal};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
use tezos_ethereum::block::BlockConstants;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_types::{OriginalSource, RuntimeId};

/// Unique identifier for a cross-runtime call chain within a block.
///
/// Composed of the origin runtime and the transaction index within
/// the block. Known at journal creation time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CracId {
    /// Runtime that originated the transaction (0 = Tezos, 1 = Ethereum).
    pub origin_runtime: u8,
    /// Transaction index within the block.
    pub tx_index: u32,
}

impl CracId {
    /// Create a CracId for a given runtime and transaction index.
    pub fn new(origin_runtime: u8, tx_index: u32) -> Self {
        Self {
            origin_runtime,
            tx_index,
        }
    }

    /// Build a CracId for `runtime` with a placeholder (zero) transaction
    /// index.
    ///
    /// There is deliberately **no** `Default` impl. A CracId is only
    /// meaningful relative to an origin runtime, and a universal default
    /// silently stamped one runtime onto contexts belonging to the other:
    /// e.g. a simulated Tezos operation built from `Default` acquired an
    /// Ethereum origin, so a gateway crossing kept the EVM-form source
    /// address instead of resolving it back to the Tezos PKH and emitted the
    /// wrong `X-Tezos-CRAC-ID`. Call sites that lack a real per-block tx index
    /// — kernel-synthesized deposits, `eth_call`/`estimateGas` simulation, and
    /// tests — must still name the runtime they enter from.
    pub fn mock(runtime: RuntimeId) -> Self {
        Self::new(u8::from(runtime), 0)
    }
}

impl fmt::Display for CracId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.origin_runtime, self.tx_index)
    }
}

/// A single HTTP request/response pair captured during cross-runtime execution.
/// Nested calls are recorded in [inner_traces].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HttpTrace {
    pub method: String,
    pub url: String,
    pub request_headers: Vec<(String, String)>,
    pub request_body: Vec<u8>,
    pub response_status: u16,
    pub response_headers: Vec<(String, String)>,
    pub response_body: Vec<u8>,
    pub inner_traces: Vec<HttpTrace>,
}

impl Encodable for HttpTrace {
    fn rlp_append(&self, s: &mut RlpStream) {
        s.begin_list(8);
        s.append(&self.method);
        s.append(&self.url);
        // Request headers as list of 2-element lists
        s.begin_list(self.request_headers.len());
        for (k, v) in &self.request_headers {
            s.begin_list(2);
            s.append(k);
            s.append(v);
        }
        s.append(&self.request_body);
        s.append(&(self.response_status as u64).to_le_bytes().to_vec());
        // Response headers
        s.begin_list(self.response_headers.len());
        for (k, v) in &self.response_headers {
            s.begin_list(2);
            s.append(k);
            s.append(v);
        }
        s.append(&self.response_body);
        // Inner traces
        s.begin_list(self.inner_traces.len());
        for t in &self.inner_traces {
            s.append(t);
        }
    }
}

impl Decodable for HttpTrace {
    fn decode(rlp: &Rlp<'_>) -> Result<Self, DecoderError> {
        if !rlp.is_list() || rlp.item_count()? != 8 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let method: String = rlp.val_at(0)?;
        let url: String = rlp.val_at(1)?;
        let req_headers_rlp = rlp.at(2)?;
        let mut request_headers = Vec::new();
        for i in 0..req_headers_rlp.item_count()? {
            let pair = req_headers_rlp.at(i)?;
            let k: String = pair.val_at(0)?;
            let v: String = pair.val_at(1)?;
            request_headers.push((k, v));
        }
        let request_body: Vec<u8> = rlp.val_at(3)?;
        let status_bytes: Vec<u8> = rlp.val_at(4)?;
        let mut buf = [0u8; 8];
        let len = status_bytes.len().min(8);
        buf[..len].copy_from_slice(&status_bytes[..len]);
        let status = u64::from_le_bytes(buf);
        let resp_headers_rlp = rlp.at(5)?;
        let mut response_headers = Vec::new();
        for i in 0..resp_headers_rlp.item_count()? {
            let pair = resp_headers_rlp.at(i)?;
            let k: String = pair.val_at(0)?;
            let v: String = pair.val_at(1)?;
            response_headers.push((k, v));
        }
        let response_body: Vec<u8> = rlp.val_at(6)?;
        let inner_rlp = rlp.at(7)?;
        let mut inner_traces = Vec::new();
        for i in 0..inner_rlp.item_count()? {
            inner_traces.push(HttpTrace::decode(&inner_rlp.at(i)?)?);
        }
        Ok(HttpTrace {
            method,
            url,
            request_headers,
            request_body,
            response_status: status as u16,
            response_headers,
            response_body,
            inner_traces,
        })
    }
}

impl HttpTrace {
    /// Capture the request part of a trace from an `http::Request`.
    pub fn from_request(request: &http::Request<Vec<u8>>) -> Self {
        let method = request.method().to_string();
        let url = request.uri().to_string();
        let request_headers = request
            .headers()
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_str().unwrap_or("").to_string()))
            .collect();
        let request_body = request.body().clone();
        Self {
            method,
            url,
            request_headers,
            request_body,
            response_status: 0,
            response_headers: Vec::new(),
            response_body: Vec::new(),
            inner_traces: Vec::new(),
        }
    }

    /// Fill in the response part of a trace from an `http::Response`. Takes
    /// the response by reference and clones only the body it keeps, so a
    /// caller that still needs the response does not clone it up front.
    pub fn set_response(&mut self, response: &http::Response<Vec<u8>>) {
        self.response_status = response.status().as_u16();
        self.response_headers = response
            .headers()
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_str().unwrap_or("").to_string()))
            .collect();
        self.response_body = response.body().clone();
    }
}

/// The journal tracks both EVM/Michelson state changes and HTTP traces.
///
/// HTTP traces are recorded with proper nesting: when a cross-runtime call
/// triggers further cross-runtime calls, the inner calls appear in the
/// parent trace's [inner_traces] field.
#[derive(Debug, PartialEq, Eq)]
pub struct TezosXJournal {
    pub evm: EvmJournal,
    pub michelson: MichelsonJournal,
    /// Completed top-level HTTP traces.
    finalized_http_traces: Vec<HttpTrace>,
    /// Stack of in-progress traces (pending response).
    pending_http_traces: Vec<HttpTrace>,
    /// CRAC-ID for the current transaction context.
    /// Known at creation time from the origin runtime and tx index.
    crac_id: CracId,
    /// Top-level transaction originator (`tx.origin`), captured on the
    /// first outgoing gateway call and shared by both runtimes. See
    /// [`OriginalSource`].
    original_source: Option<OriginalSource>,
    /// When `false` (the default), [`record_request`] / [`record_response`]
    /// skip the request/response clones so a normal block pays nothing.
    http_trace_enabled: bool,
}

impl TezosXJournal {
    /// Construct a fresh journal for one synthetic Michelson manager
    /// operation.  [`operation_hash`] is propagated to the Michelson
    /// journal where it seeds the inbound-CRAC origination nonce; it
    /// MUST be derived deterministically from `crac_id` and the
    /// originating block so two synthetic ops in the same block see
    /// distinct seeds — see
    /// [`Self::synthetic_operation_hash`] for the canonical
    /// derivation.
    /// [`outer_block`] is the originating runtime's L2 block environment.
    /// Outside a block-producing context (traces, unit tests) pass
    /// [`BlockConstants::dummy`]. The cross-runtime journal is the single
    /// place that distributes block context to each runtime's sub-journal
    /// — it forwards the block to the EVM journal at *its* creation, so an
    /// inbound CRAC serviced under this journal exposes the live block
    /// observables (`BASEFEE`, `GASLIMIT`, ...) instead of zero. New
    /// runtimes plug in by receiving the same block here, rather than the
    /// kernel reaching into a specific runtime's journal after the fact.
    pub fn new(
        crac_id: CracId,
        operation_hash: tezos_crypto_rs::hash::OperationHash,
        outer_block: BlockConstants,
    ) -> Self {
        Self {
            evm: EvmJournal::new(outer_block),
            michelson: MichelsonJournal::new(operation_hash),
            finalized_http_traces: Vec::new(),
            pending_http_traces: Vec::new(),
            crac_id,
            original_source: None,
            http_trace_enabled: false,
        }
    }

    pub fn enable_debug_precompiles(&mut self) {
        self.evm.enable_debug_precompiles()
    }

    /// Build a journal entered from `runtime` with placeholder seeds: a zero
    /// operation hash (so origination addresses are non-deterministic — see
    /// [`Self::new`]) and [`BlockConstants::dummy`]. This reproduces what the
    /// removed `Default` impl yielded, but forces the caller to name the
    /// origin runtime so a journal can no longer be silently stamped with the
    /// wrong one (see [`CracId::mock`]). Intended for tests and the kernel
    /// contexts that genuinely lack a real tx index and block environment.
    pub fn mock(runtime: RuntimeId) -> Self {
        Self::new(
            CracId::mock(runtime),
            tezos_crypto_rs::hash::OperationHash::default(),
            BlockConstants::dummy(),
        )
    }

    /// Open a Tezos X *global* checkpoint: one cross-runtime unit of
    /// execution spanning every runtime's durable-overlay state. It
    /// bundles the per-runtime opens — the Michelson world-state frame
    /// marker and the EVM precompile layered-state checkpoint — behind a
    /// single call so a caller (e.g. the EVM `JournalTr` impl driving a
    /// call/create frame) can checkpoint the whole Tezos X state without
    /// reaching into, or naming, any individual runtime's sub-journal.
    /// New runtimes plug in here rather than at each call site.
    #[inline]
    pub fn global_checkpoint(&mut self) {
        self.michelson.push_external_checkpoint();
        self.evm.layered_state.checkpoint();
    }

    /// Commit the innermost global checkpoint opened by
    /// [`Self::global_checkpoint`]: keep this frame's durable-overlay
    /// changes across every runtime. The michelson commit is fallible
    /// (it touches durable storage); the EVM layered-state commit is an
    /// in-memory pop, so the two are independent and their order is
    /// immaterial.
    #[inline]
    pub fn global_commit<Host: StorageV1>(
        &mut self,
        host: &mut Host,
    ) -> Result<(), RuntimeError> {
        self.evm.layered_state.checkpoint_commit();
        self.michelson.commit_frame(host)
    }

    /// Revert the innermost global checkpoint opened by
    /// [`Self::global_checkpoint`]: roll back this frame's
    /// durable-overlay changes across every runtime. The michelson and
    /// EVM reverts touch disjoint state (Michelson durable storage vs the
    /// in-memory EVM overlay), so their order is immaterial.
    #[inline]
    pub fn global_revert<Host: StorageV1>(
        &mut self,
        host: &mut Host,
    ) -> Result<(), RuntimeError> {
        self.evm.layered_state.checkpoint_revert();
        self.michelson.revert_frame(host)
    }

    /// Canonical derivation for the synthetic operation hash that
    /// seeds an inbound-CRAC origination nonce.  Hashing
    /// `crac:<chain_id>:<block_number>:<crac_id>` gives every synthetic
    /// Michelson manager operation a distinct seed across operations,
    /// blocks AND chains.  This mirrors L1, where the operation hash is
    /// the hash of the signed bytes whose signature is bound to the
    /// chain id via the signing watermark — so identical operation
    /// bytes on two chains yield different op hashes (and child KT1s).
    /// Folding `chain_id` in keeps the kernel safe if the same binary
    /// runs on multiple chains (test scenarios, replays, forks).  Used
    /// by production journal-construction sites; tests that don't care
    /// about origination addresses may pass `OperationHash::default()`
    /// to [`Self::new`] directly.
    //
    // TODO: https://linear.app/tezos/issue/L2-1440
    // Tezos-originated paths also use this synthetic seed today to
    // keep the CRAC and native-Michelson origination-nonce universes
    // disjoint.  Once a single `OriginationNonce` is shared between
    // both paths inside one synthetic op, those sites can drop the
    // synthetic seed and pass the real `op.hash()` to
    // [`Self::new`], matching L1's per-operation nonce semantics.
    pub fn synthetic_operation_hash(
        crac_id: &CracId,
        chain_id: u64,
        block_number: u64,
    ) -> tezos_crypto_rs::hash::OperationHash {
        let seed = format!("cross-runtime-call:{chain_id}:{block_number}:{crac_id}");
        tezos_crypto_rs::hash::OperationHash::from(tezos_crypto_rs::blake2b::digest_256(
            seed.as_bytes(),
        ))
    }

    /// Record an HTTP request. The trace is pushed onto an internal stack;
    /// the matching [record_response] call will pop it and nest it under its
    /// parent if one exists. No-op when [`http_trace_enabled`] is unset.
    pub fn record_request(&mut self, request: &http::Request<Vec<u8>>) {
        if !self.http_trace_enabled {
            return;
        }
        self.pending_http_traces
            .push(HttpTrace::from_request(request));
    }

    /// Attach the response to the most recent pending trace and finalize it.
    /// If there is a parent trace on the stack, the completed trace becomes
    /// one of its [inner_traces]; otherwise it is added to the root list.
    /// Takes the response by reference and returns before touching it when
    /// [`http_trace_enabled`] is unset, so the caller pays no clone at all.
    pub fn record_response(&mut self, response: &http::Response<Vec<u8>>) {
        if !self.http_trace_enabled {
            return;
        }
        if let Some(mut trace) = self.pending_http_traces.pop() {
            trace.set_response(response);
            match self.pending_http_traces.last_mut() {
                Some(parent) => parent.inner_traces.push(trace),
                None => self.finalized_http_traces.push(trace),
            }
        }
    }

    /// Return a reference to all recorded HTTP traces.
    pub fn http_traces(&self) -> &[HttpTrace] {
        &self.finalized_http_traces
    }

    /// Consume the journal and return the HTTP traces.
    pub fn into_http_traces(self) -> Vec<HttpTrace> {
        self.finalized_http_traces
    }

    /// Get the CRAC-ID for this transaction context.
    pub fn crac_id(&self) -> &CracId {
        &self.crac_id
    }

    /// The captured top-level originator, or `None` before the first
    /// outgoing gateway call. See [`OriginalSource`].
    pub fn original_source(&self) -> Option<&OriginalSource> {
        self.original_source.as_ref()
    }

    /// Enable per-transaction HTTP trace capture: set from the once-per-block
    /// `http_trace_enabled` flag at apply sites, `true` on trace journals.
    pub fn set_http_trace_enabled(&mut self, enabled: bool) {
        self.http_trace_enabled = enabled;
    }

    /// First call captures; subsequent calls are no-ops so re-entrant
    /// frames cannot overwrite the original identity.
    pub fn set_original_source(&mut self, source: OriginalSource) {
        if self.original_source.is_none() {
            self.original_source = Some(source);
        }
    }

    /// Drop the captured originator. Called when a backtracked operation's
    /// in-memory state is discarded (alongside [`EvmJournal::clear`]) so a
    /// failed operation's originator does not leak into the next one.
    pub fn reset_original_source(&mut self) {
        self.original_source = None;
    }

    /// Verify that a received CRAC-ID (from `X-Tezos-CRAC-ID` header)
    /// matches the expected one. Used for debug/consistency checks on
    /// incoming CRACs.
    pub fn verify_crac_id(&self, received: &str) -> Result<(), anyhow::Error> {
        let expected = self.crac_id.to_string();
        if received == expected {
            Ok(())
        } else {
            Err(anyhow::anyhow!(
                "cross-runtime call ID mismatch: expected '{}', received '{}'",
                expected,
                received
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezosx_types::RuntimeId;

    fn test_crac_id() -> CracId {
        CracId {
            origin_runtime: 1,
            tx_index: 42,
        }
    }

    fn fresh_journal() -> TezosXJournal {
        TezosXJournal::new(
            test_crac_id(),
            tezos_crypto_rs::hash::OperationHash::default(),
            BlockConstants::dummy(),
        )
    }

    fn eth_origin(evm: &str) -> OriginalSource {
        OriginalSource::new(RuntimeId::Ethereum, evm.to_string())
    }

    #[test]
    fn test_original_source_starts_unset() {
        let journal = fresh_journal();
        assert_eq!(journal.original_source(), None);
    }

    #[test]
    fn test_original_source_first_set_wins() {
        let mut journal = fresh_journal();
        let eoa = eth_origin("0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee");
        let intermediate = eth_origin("0xcccccccccccccccccccccccccccccccccccccccc");

        journal.set_original_source(eoa.clone());
        assert_eq!(journal.original_source(), Some(&eoa));

        journal.set_original_source(intermediate);
        assert_eq!(journal.original_source(), Some(&eoa));
    }

    #[test]
    fn test_original_source_round_trips_through_journal() {
        let mut journal = fresh_journal();
        let tz_origin = OriginalSource::new(
            RuntimeId::Tezos,
            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx".to_string(),
        );
        journal.set_original_source(tz_origin.clone());
        assert_eq!(journal.original_source(), Some(&tz_origin));
    }

    #[test]
    fn test_reset_original_source() {
        let mut journal = fresh_journal();
        let eoa = eth_origin("0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee");
        journal.set_original_source(eoa.clone());
        assert_eq!(journal.original_source(), Some(&eoa));

        journal.reset_original_source();
        assert_eq!(journal.original_source(), None);

        // After a reset the next first-set wins again.
        let next_eoa = eth_origin("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
        journal.set_original_source(next_eoa.clone());
        assert_eq!(journal.original_source(), Some(&next_eoa));
    }

    // Golden vector for `synthetic_operation_hash`.
    //
    // Pins the digest of the seed `crac:1:2:3-4` so any drift in the
    // format template, `CracId::Display`, or the digest function
    // surfaces here rather than only via Tezt regressions watching
    // KT1 churn.  Regenerate by running this test and copying the
    // bytes printed in the failure message.
    #[test]
    fn test_synthetic_operation_hash_golden() {
        let hash = TezosXJournal::synthetic_operation_hash(
            &CracId::new(3, 4),
            /* chain_id = */ 1,
            /* block_number = */ 2,
        );
        // blake2b-256(b"cross-runtime-call:1:2:3-4")
        let expected = tezos_crypto_rs::hash::OperationHash::from([
            0x6d, 0xfd, 0xd4, 0xf4, 0x11, 0x20, 0xf9, 0x8c, 0x2a, 0xfd, 0x88, 0x87, 0x40,
            0x4d, 0x44, 0xd4, 0xa0, 0x28, 0x65, 0x3c, 0xba, 0x0f, 0xdf, 0xde, 0xb7, 0x4e,
            0xfb, 0x3c, 0x20, 0x5e, 0x42, 0xbf,
        ]);
        assert_eq!(
            hash,
            expected,
            "synthetic_operation_hash drifted from golden vector; got {:02x?}",
            hash.as_ref()
        );
    }

    #[test]
    fn test_crac_id_available_from_creation() {
        let journal = TezosXJournal::new(
            test_crac_id(),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        assert_eq!(journal.crac_id().origin_runtime, 1);
        assert_eq!(journal.crac_id().tx_index, 42);
        assert_eq!(journal.crac_id().to_string(), "1-42");
    }

    #[test]
    fn test_verify_crac_id_matching() {
        let journal = TezosXJournal::new(
            test_crac_id(),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        assert!(journal.verify_crac_id("1-42").is_ok());
    }

    #[test]
    fn test_verify_crac_id_mismatch() {
        let journal = TezosXJournal::new(
            test_crac_id(),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        assert!(journal.verify_crac_id("0-99").is_err());
    }

    #[test]
    fn test_crac_id_display() {
        let id = CracId {
            origin_runtime: 0,
            tx_index: 7,
        };
        assert_eq!(id.to_string(), "0-7");
    }

    #[test]
    fn test_crac_id_is_stable() {
        let j1 = TezosXJournal::new(
            test_crac_id(),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let j2 = TezosXJournal::new(
            test_crac_id(),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        assert_eq!(j1.crac_id(), j2.crac_id());
    }

    #[test]
    fn test_record_request_and_response() {
        let mut journal = TezosXJournal::new(
            test_crac_id(),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        journal.set_http_trace_enabled(true);

        let request = http::Request::builder()
            .method("POST")
            .uri("http://tezos/KT1abc/transfer")
            .header("X-Tezos-Sender", "KT1sender")
            .header("X-Tezos-Amount", "1.5")
            .body(vec![1, 2, 3])
            .unwrap();

        journal.record_request(&request);
        assert_eq!(journal.http_traces().len(), 0); // still pending

        let response = http::Response::builder()
            .status(200)
            .header("X-Tezos-Gas-Consumed", "500")
            .body(vec![4, 5, 6])
            .unwrap();

        journal.record_response(&response);
        assert_eq!(journal.http_traces().len(), 1);
        assert_eq!(journal.http_traces()[0].method, "POST");
        assert_eq!(journal.http_traces()[0].url, "http://tezos/KT1abc/transfer");
        assert_eq!(journal.http_traces()[0].response_status, 200);
        assert_eq!(journal.http_traces()[0].response_body, vec![4, 5, 6]);
        assert!(journal.http_traces()[0]
            .response_headers
            .iter()
            .any(|(k, v)| k == "x-tezos-gas-consumed" && v == "500"));
    }

    #[test]
    fn test_nested_traces() {
        let mut journal = TezosXJournal::new(
            test_crac_id(),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        journal.set_http_trace_enabled(true);

        // Outer call: EVM → Tezos
        let req1 = http::Request::builder()
            .uri("http://tezos/KT1first/default")
            .body(vec![])
            .unwrap();
        journal.record_request(&req1);

        // Inner call: Tezos → EVM (nested inside the first call)
        let req2 = http::Request::builder()
            .uri("http://ethereum/0xabc")
            .body(vec![])
            .unwrap();
        journal.record_request(&req2);

        // Inner response comes first (stack order)
        let resp2 = http::Response::builder().status(200).body(vec![]).unwrap();
        journal.record_response(&resp2);

        // Outer response
        let resp1 = http::Response::builder().status(200).body(vec![]).unwrap();
        journal.record_response(&resp1);

        // Only one root trace, with the inner call nested
        assert_eq!(journal.http_traces().len(), 1);
        assert_eq!(
            journal.http_traces()[0].url,
            "http://tezos/KT1first/default"
        );
        assert_eq!(journal.http_traces()[0].inner_traces.len(), 1);
        assert_eq!(
            journal.http_traces()[0].inner_traces[0].url,
            "http://ethereum/0xabc"
        );
    }

    /// With tracing off (the default), `record_*` must be no-ops so a normal
    /// block never pays the request/response clone — regression guard L2-1732.
    #[test]
    fn test_record_skipped_when_http_trace_disabled() {
        let mut journal = TezosXJournal::new(
            test_crac_id(),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        // Left at the default (`false`): no `set_http_trace_enabled`.

        let request = http::Request::builder()
            .method("POST")
            .uri("http://tezos/KT1abc/transfer")
            .body(vec![1, 2, 3])
            .unwrap();
        journal.record_request(&request);
        let response = http::Response::builder()
            .status(200)
            .body(vec![4, 5, 6])
            .unwrap();
        journal.record_response(&response);

        assert_eq!(journal.http_traces().len(), 0);
    }

    #[test]
    fn test_http_trace_rlp_roundtrip() {
        let trace = HttpTrace {
            method: "POST".to_string(),
            url: "http://tezos/KT1abc/transfer".to_string(),
            request_headers: vec![
                ("X-Tezos-Sender".to_string(), "KT1sender".to_string()),
                ("X-Tezos-Amount".to_string(), "1.5".to_string()),
            ],
            request_body: vec![1, 2, 3],
            response_status: 200,
            response_headers: vec![(
                "X-Tezos-Gas-Consumed".to_string(),
                "500".to_string(),
            )],
            response_body: vec![4, 5, 6],
            inner_traces: vec![],
        };

        let encoded = rlp::encode(&trace);
        let decoded: HttpTrace = rlp::decode(&encoded).unwrap();
        assert_eq!(trace, decoded);
    }

    #[test]
    fn test_http_trace_with_inner_traces_rlp_roundtrip() {
        let trace = HttpTrace {
            method: "POST".to_string(),
            url: "http://tezos/KT1a/default".to_string(),
            request_headers: vec![],
            request_body: vec![],
            response_status: 200,
            response_headers: vec![],
            response_body: vec![10, 20],
            inner_traces: vec![HttpTrace {
                method: "POST".to_string(),
                url: "http://ethereum/0xdef".to_string(),
                request_headers: vec![("X-Tezos-Sender".to_string(), "KT1x".to_string())],
                request_body: vec![0xab],
                response_status: 400,
                response_headers: vec![],
                response_body: vec![],
                inner_traces: vec![],
            }],
        };

        let encoded = rlp::encode(&trace);
        let decoded: HttpTrace = rlp::decode(&encoded).unwrap();
        assert_eq!(trace, decoded);
    }

    #[test]
    fn test_http_trace_list_rlp_roundtrip() {
        let traces = vec![
            HttpTrace {
                method: "POST".to_string(),
                url: "http://tezos/KT1a/default".to_string(),
                request_headers: vec![],
                request_body: vec![],
                response_status: 200,
                response_headers: vec![],
                response_body: vec![10, 20],
                inner_traces: vec![],
            },
            HttpTrace {
                method: "POST".to_string(),
                url: "http://ethereum/0xdef".to_string(),
                request_headers: vec![("X-Tezos-Sender".to_string(), "KT1x".to_string())],
                request_body: vec![0xab],
                response_status: 400,
                response_headers: vec![],
                response_body: vec![],
                inner_traces: vec![],
            },
        ];

        let mut stream = rlp::RlpStream::new_list(traces.len());
        for t in &traces {
            stream.append(t);
        }
        let encoded = stream.out();

        let rlp = Rlp::new(&encoded);
        let decoded: Vec<HttpTrace> = rlp.as_list().unwrap();
        assert_eq!(traces, decoded);
    }
}
