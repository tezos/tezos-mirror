// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Helpers for handling http requests.
//! Functions in this module are designed to be compatible with `Tezos_rpc_services`.

use std::pin::Pin;

use futures::{Future, Stream, StreamExt, TryStreamExt};
use http_body_util::{combinators::BoxBody, BodyExt, Full, StreamBody};
use hyper::{
    body::{Bytes, Frame, Incoming},
    Request, Response, StatusCode,
};
use serde::{Deserialize, Serialize};

type Resp = hyper::Response<BoxBody<Bytes, Box<dyn std::error::Error + Send + Sync>>>;

/// Construct a streamed response body from a stream. The items are serialized
/// in json format before being sent as part of a response.
pub fn monitor_stream_handler<T: Serialize>(
    stream: impl Stream<Item = T> + Send + Sync + 'static,
) -> Result<Resp, Box<dyn std::error::Error + Send + Sync>> {
    let json_stream = stream.map(|v| Ok(serde_json::to_vec(&v)?.into()));

    let body = BoxBody::new(StreamBody::new(TryStreamExt::map_ok(
        json_stream,
        Frame::data,
    )));

    Ok(hyper::Response::builder().body(body)?)
}

/// Constructs a streamed response body from a [tokio::sync::broadcast::receiver<T>].
/// Items are serialized in json format before being streamed in the response body.
pub async fn monitor_broadcast_channel_handler<T: Serialize + Clone + Send + 'static>(
    receiver: tokio::sync::broadcast::Receiver<T>,
) -> Result<Resp, Box<dyn std::error::Error + Send + Sync>> {
    let stream = tokio_stream::wrappers::BroadcastStream::new(receiver);

    //let stream = tokio_stream::wrappers::BroadcastStream::new(receiver.resubscribe());
    let json_stream = stream.map(|v| Ok(serde_json::to_vec(&v?)?.into()));

    let body = BoxBody::new(StreamBody::new(TryStreamExt::map_ok(
        json_stream,
        Frame::data,
    )));

    Ok(hyper::Response::builder().body(body)?)
}

/// Helper function for handling post requests.
/// The request body is expected to be a json object.
/// This function gets an asynchronous combinator in input to process the request. The output of the
/// combinator function is serialized in json format and sent as the response body in a single frame.
pub async fn post_handler<
    T: for<'a> Deserialize<'a>,
    S: Serialize,
    F: Fn(
        T,
    )
        -> Pin<Box<dyn Send + Future<Output = Result<S, Box<dyn std::error::Error + Send + Sync>>>>>,
>(
    req: Request<Incoming>,
    f: F,
) -> Result<Resp, Box<dyn std::error::Error + Send + Sync>> {
    let value: T = serde_json::from_slice(&req.collect().await?.to_bytes())?;
    let output = f(value).await?;
    let json_response: Bytes = serde_json::to_vec(&output)?.into();
    let response_body = Full::new(json_response)
        .map_err(|never| match never {})
        .boxed();

    Ok(Response::new(response_body))
}

/// Helper function for handling get requests.
/// The request body is expected to be a json object.
/// This function gets an asynchronous combinator in input to process the request. The output of the
/// combinator function is serialized in json format and sent as the response body in a single frame.

//TODO: Handle query parameters
pub async fn get_handler<
    T: for<'a> Deserialize<'a>,
    S: Serialize,
    F: Fn(
        T,
    )
        -> Pin<Box<dyn Send + Future<Output = Result<S, Box<dyn std::error::Error + Send + Sync>>>>>,
>(
    req: Request<Incoming>,
    f: F,
) -> Result<Resp, Box<dyn std::error::Error + Send + Sync>> {
    let value: T = serde_json::from_slice(&req.collect().await?.to_bytes())?;
    let output = f(value).await?;
    let json_response: Bytes = serde_json::to_vec(&output)?.into();
    let response_body = Full::new(json_response)
        .map_err(|never| match never {})
        .boxed();

    Ok(Response::new(response_body))
}

/// Simple handler for non-existing routes. It returns a response with status code `404 (Not found)`.
pub async fn not_found_handler() -> Result<Resp, Box<dyn std::error::Error + Send + Sync>> {
    let response_body = Full::new("not found".into())
        .map_err(|never| match never {})
        .boxed();
    let response = Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(response_body)?;

    Ok(response)
}
