// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Standard responses that can be used when handling RPC requests.
//! Functions in this module are designed to be compatible with `Tezos_rpc_services`.

use std::convert::Infallible;

use http_body_util::{combinators::BoxBody, BodyExt, Full};
use hyper::{body::Bytes, Response, StatusCode};

use crate::errors::RpcError;

/// The [Body] of [Response]s returned by the Rpc server.
/// All application logic errors obtained when processing requests
/// are handed by the rpc handlers implemented in this module: in case of
/// errors a response with custom body and  status code is returned.
/// Therefore, the error type associated with the response is [Infallible].
pub type ResponseBody = BoxBody<Bytes, Infallible>;

pub type Resp = hyper::Response<ResponseBody>;

/// Simple handler for non-existing routes. It returns a response with status code `404 (Not found)`.
/// This handler returns a [RpcError] in case of connection errors.
pub fn not_found() -> Result<Resp, RpcError> {
    let response_body = Full::new("not found".into())
        .map_err(|never| match never {})
        .boxed();
    let response = Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(response_body)?;

    Ok(response)
}

/// Simple handler for bad requests. It returns a response with status code `400 (Bad request)`.
/// This handler returns a [RpcError] in case of connection errors.
pub fn bad_request() -> Result<Resp, RpcError> {
    let response_body: ResponseBody = Full::new("Bad request".into()).boxed();
    let response = Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .body(response_body)?;

    Ok(response)
}

/// Simple handler for bad requests. It returns a response with status code `500 (Internal server error)`.
/// This handler returns a [RpcError] in case of connection errors.
///
/// TODO: add custom error message
pub fn internal_server_error() -> Result<Resp, RpcError> {
    let response_body: ResponseBody = Full::new("Internal server error".into()).boxed();
    let response = Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .body(response_body)?;

    Ok(response)
}
