// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Helpers for handling http requests.
//! Functions in this module are designed to be compatible with `Tezos_rpc_services`.

// TODO: Move to separate rpc crate

use std::{convert::Infallible, pin::Pin};

use futures::{future, Future, Stream, StreamExt, TryStreamExt};
use http_body_util::{combinators::BoxBody, BodyExt, Full, StreamBody};
use hyper::{
    body::{Bytes, Frame, Incoming},
    Request, Response, StatusCode,
};
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use tracing::error;

use crate::errors::RpcError;

/// The [Body] of [Response]s returned by the Rpc server.
/// All application logic errors obtained when processing requests
/// are handed by the rpc handlers implemented in this module: in case of
/// errors a response with custom body and  status code is returned.
/// Therefore, the error type associated with the response is [Infallible].
pub type ResponseBody = BoxBody<Bytes, Infallible>;

pub type Resp = hyper::Response<ResponseBody>;

fn stream_until_first_error<T: Send + Sync + 'static, E: Send + Sync + Debug + 'static>(
    stream: impl Stream<Item = Result<T, E>> + Send + Sync + 'static,
    on_error: &'static str,
) -> impl Stream<Item = T> + Send + Sync + 'static {
    stream
        .inspect_err(move |e| error!("{on_error} - {:?}", e))
        .take_while(|v| future::ready(v.is_ok()))
        .filter_map(|v| future::ready(v.ok()))
}

/// Construct a streamed response body from a stream. The items are serialized
/// in json format before being sent as part of a response.
/// This handler returns a [RpcError] in case of connection errors.
pub fn monitor_stream_handler<T: Serialize>(
    stream: impl Stream<Item = T> + Send + Sync + 'static,
) -> Result<Resp, RpcError> {
    let json_stream = stream_until_first_error(
        stream.map(|v| serde_json::to_vec(&v)),
        "Failed to serialize value to json",
    );
    let frame_stream = json_stream.map(|v| Ok(Frame::data(v.into())));
    let body = BoxBody::new(StreamBody::new(frame_stream));

    Ok(hyper::Response::builder().body(body)?)
}

/// Constructs a streamed response body from a [tokio::sync::broadcast::receiver<T>].
/// Items are serialized in json format before being streamed in the response body.
/// This handler returns a [RpcError] in case of connection errors.
pub async fn monitor_broadcast_channel_handler<T: Serialize + Clone + Send + Sync + 'static>(
    receiver: tokio::sync::broadcast::Receiver<T>,
) -> Result<Resp, RpcError> {
    let stream = tokio_stream::wrappers::BroadcastStream::new(receiver);
    let stream_after_broadcast =
        stream_until_first_error(stream, "Failed to receive falue from broadcast channel");
    monitor_stream_handler(stream_after_broadcast)
}

/// Helper function for handling post requests.
/// The request body is expected to be a json object.
/// This function gets an asynchronous combinator in input to process the request. The output of the
/// combinator function is serialized in json format and sent as the response body in a single frame.
/// This handler returns a [RpcError] in case of connection errors.
pub async fn post_handler<
    T: for<'a> Deserialize<'a>,
    S: Serialize,
    F: FnOnce(
        T,
    ) -> Pin<
        Box<dyn Send + Future<Output = Result<S, Box<dyn std::error::Error + Send + Sync>>>>,
    >,
>(
    req: Request<Incoming>,
    f: F,
) -> Result<Resp, RpcError> {
    // We fail to deserialize the body request. This is likely a
    let value: T = match serde_json::from_slice(&req.collect().await?.to_bytes()) {
        Err(_e) => {
            return bad_request();
        }
        Ok(v) => v,
    };

    let output = match f(value).await {
        Err(e) => {
            error!("Error while processing request: {:?}", e);
            return internal_server_error();
        }
        Ok(output) => output,
    };

    let json_response = match serde_json::to_vec(&output) {
        Err(e) => {
            error!("Error while deserializing response {:?}", e);
            return internal_server_error();
        }
        Ok(response) => response,
    };

    let response_body = Full::new(json_response.into()).boxed();

    Ok(Response::new(response_body))
}

/// Helper function for handling get requests.
/// The request body is expected to be a json object.
/// This function gets an asynchronous combinator in input to process the request. The output of the
/// combinator function is serialized in json format and sent as the response body in a single frame.
/// This handler returns a [RpcError] in case of connection errors.
//TODO: Handle query parameters
//TODO: Remove warning suppression once this handler is used
#[allow(unused)]
pub async fn get_handler<
    T: for<'a> Deserialize<'a>,
    S: Serialize,
    F: FnOnce(
        T,
    ) -> Pin<
        Box<dyn Send + Future<Output = Result<S, Box<dyn std::error::Error + Send + Sync>>>>,
    >,
>(
    req: Request<Incoming>,
    f: F,
) -> Result<Resp, RpcError> {
    // We fail to deserialize the body request. This is likely a
    let value: T = match serde_json::from_slice(&req.collect().await?.to_bytes()) {
        Err(_e) => {
            return bad_request();
        }
        Ok(v) => v,
    };

    let output = match f(value).await {
        Err(e) => {
            error!("Error while processing request: {:?}", e);
            return internal_server_error();
        }
        Ok(output) => output,
    };

    let json_response = match serde_json::to_vec(&output) {
        Err(e) => {
            error!("Error while deserializing response {:?}", e);
            return internal_server_error();
        }
        Ok(response) => response,
    };

    let response_body = Full::new(json_response.into()).boxed();

    Ok(Response::new(response_body))
}

/// Simple handler for non-existing routes. It returns a response with status code `404 (Not found)`.
/// This handler returns a [RpcError] in case of connection errors.
pub async fn not_found_handler() -> Result<Resp, RpcError> {
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
pub fn internal_server_error() -> Result<Resp, RpcError> {
    let response_body: ResponseBody = Full::new("Internal server error".into()).boxed();
    let response = Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .body(response_body)?;

    Ok(response)
}
