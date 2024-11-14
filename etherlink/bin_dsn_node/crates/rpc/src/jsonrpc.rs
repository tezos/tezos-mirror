// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::pin::Pin;

use futures::Future;
use http_body_util::{BodyExt, Full};
use hyper::{body::Incoming, Request, Response};
use jsonrpsee_core::Cow;
use jsonrpsee_types::{ErrorObjectOwned, Params, ResponsePayload, TwoPointZero};
use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;

use crate::{
    errors::RpcError,
    handlers::Resp,
    responses::{bad_request, internal_server_error},
};

/// Owned version of [jsonrpsee_types::Request] that also supports notifications,
/// i.e. missing `id` field
#[derive(Debug, Clone, Deserialize)]
pub struct JsonRpcRequest {
    /// JSON-RPC version.
    pub jsonrpc: TwoPointZero,
    /// Request ID
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<Id>,
    /// Name of the method to be invoked.
    pub method: String,
    /// Parameter values of the request.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<Box<RawValue>>,
}

/// Owned version of [jsonrpsee_types::Id]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
#[serde(untagged)]
pub enum Id {
    /// Null
    Null,
    /// Numeric id
    Number(u64),
    /// String id
    Str(String),
}

impl JsonRpcRequest {
    /// Get the params of the request.
    pub fn params(&self) -> Params {
        Params::new(self.params.as_ref().map(|p| p.get()))
    }
}

impl From<Id> for jsonrpsee_types::Id<'_> {
    fn from(val: Id) -> Self {
        match val {
            Id::Null => jsonrpsee_types::Id::Null,
            Id::Number(n) => jsonrpsee_types::Id::Number(n),
            Id::Str(s) => jsonrpsee_types::Id::Str(Cow::owned(s)),
        }
    }
}

/// Helper function for handling JSON-RPC requests and notifications (optionally batched)
pub async fn handle_jsonrpc_request<
    S: Clone + Serialize,
    F: Fn(
        JsonRpcRequest,
    ) -> Pin<Box<dyn 'static + Send + Future<Output = Result<S, ErrorObjectOwned>>>>,
>(
    req: Request<Incoming>,
    f: F,
) -> Result<Resp, RpcError> {
    let req_body = req.collect().await?.to_bytes();
    let Ok(req_json) = serde_json::from_slice::<serde_json::Value>(&req_body) else {
        return bad_request();
    };

    if req_json.is_array() {
        // Handle multiple batched requests / notifications
        let Ok(jsonrpc_req_batch) = serde_json::from_value::<Vec<JsonRpcRequest>>(req_json) else {
            return bad_request();
        };
        let mut jsonrpc_res_batch = Vec::new();
        for jsonrpc_req in jsonrpc_req_batch {
            if let Some(jsonrpc_res) = handle_jsonrpc_request_inner(jsonrpc_req, &f).await {
                jsonrpc_res_batch.push(jsonrpc_res);
            }
        }
        if jsonrpc_res_batch.is_empty() {
            Ok(Response::default())
        } else {
            let Ok(res_body) = serde_json::to_vec(&jsonrpc_res_batch) else {
                return internal_server_error();
            };
            Ok(Response::new(Full::new(res_body.into()).boxed()))
        }
    } else {
        // Handle single request / notification
        let Ok(jsonrpc_req) = serde_json::from_value::<JsonRpcRequest>(req_json) else {
            return bad_request();
        };
        if let Some(jsonrpc_res) = handle_jsonrpc_request_inner(jsonrpc_req, &f).await {
            let Ok(res_body) = serde_json::to_vec(&jsonrpc_res) else {
                return internal_server_error();
            };
            Ok(Response::new(Full::new(res_body.into()).boxed()))
        } else {
            Ok(Response::default())
        }
    }
}

async fn handle_jsonrpc_request_inner<
    'a,
    S: Clone + Serialize,
    F: Fn(
        JsonRpcRequest,
    ) -> Pin<Box<dyn 'static + Send + Future<Output = Result<S, ErrorObjectOwned>>>>,
>(
    jsonrpc_req: JsonRpcRequest,
    f: &F,
) -> Option<jsonrpsee_types::Response<'a, S>> {
    let jsonrpc_req_id = jsonrpc_req.id.clone();
    let jsonrpc_payload = match f(jsonrpc_req).await {
        Ok(s) => ResponsePayload::success(s),
        Err(e) => ResponsePayload::error(e),
    };
    jsonrpc_req_id.map(|id| jsonrpsee_types::Response::new(jsonrpc_payload, id.into()))
}

#[cfg(test)]
mod tests {
    use super::{Id, JsonRpcRequest};

    #[test]
    fn deserialize_jsonrpc_notification() {
        let value = r#"{
            "jsonrpc": "2.0",
            "method": "eth_sendRawTransaction",
            "params": ["0x00000000"]
        }"#;
        let req: JsonRpcRequest = serde_json::from_str(value).unwrap();
        let arg = req.params().one::<String>().unwrap();
        assert_eq!("0x00000000", arg.as_str());
    }

    #[test]
    fn deserialize_jsonrpc_request() {
        let value = r#"{
            "jsonrpc": "2.0",
            "method": "eth_sendRawTransaction",
            "params": ["0x00000000"],
            "id": 1
        }"#;
        let req: JsonRpcRequest = serde_json::from_str(value).unwrap();
        assert_eq!(Some(Id::Number(1)), req.id);
    }
}
