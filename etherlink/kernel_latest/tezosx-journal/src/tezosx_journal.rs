// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{EvmJournal, MichelsonJournal};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};

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

    /// Fill in the response part of a trace from an `http::Response`.
    pub fn set_response(&mut self, response: http::Response<Vec<u8>>) {
        self.response_status = response.status().as_u16();
        self.response_headers = response
            .headers()
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_str().unwrap_or("").to_string()))
            .collect();
        let (_, body) = response.into_parts();
        self.response_body = body;
    }
}

/// The journal tracks both EVM/Michelson state changes and HTTP traces.
///
/// HTTP traces are recorded with proper nesting: when a cross-runtime call
/// triggers further cross-runtime calls, the inner calls appear in the
/// parent trace's [inner_traces] field.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct TezosXJournal {
    pub evm: EvmJournal,
    pub michelson: MichelsonJournal,
    /// Completed top-level HTTP traces.
    finalized_http_traces: Vec<HttpTrace>,
    /// Stack of in-progress traces (pending response).
    pending_http_traces: Vec<HttpTrace>,
}

impl TezosXJournal {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record an HTTP request. The trace is pushed onto an internal stack;
    /// the matching [record_response] call will pop it and nest it under its
    /// parent if one exists.
    pub fn record_request(&mut self, request: &http::Request<Vec<u8>>) {
        self.pending_http_traces
            .push(HttpTrace::from_request(request));
    }

    /// Attach the response to the most recent pending trace and finalize it.
    /// If there is a parent trace on the stack, the completed trace becomes
    /// one of its [inner_traces]; otherwise it is added to the root list.
    pub fn record_response(&mut self, response: http::Response<Vec<u8>>) {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_record_request_and_response() {
        let mut journal = TezosXJournal::new();

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

        journal.record_response(response);
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
        let mut journal = TezosXJournal::new();

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
        journal.record_response(resp2);

        // Outer response
        let resp1 = http::Response::builder().status(200).body(vec![]).unwrap();
        journal.record_response(resp1);

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
