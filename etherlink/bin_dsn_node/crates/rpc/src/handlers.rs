// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Helpers for handling http requests.
//! Functions in this module are designed to be compatible with `Tezos_rpc_services`.

use std::{convert::Infallible, pin::Pin};

use futures::{future, Future, Stream, StreamExt, TryStreamExt};
use http_body_util::{combinators::BoxBody, BodyExt, Full, StreamBody};
use hyper::{
    body::{Bytes, Frame, Incoming},
    Request, Response,
};
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use tracing::{debug, error};

use crate::{
    errors::RpcError,
    responses::{bad_request, internal_server_error},
};

/// The body of [Response]s returned by the Rpc server.
/// All application logic errors obtained when processing requests
/// are handed by the rpc handlers implemented in this module: in case of
/// errors a response with custom body and  status code is returned.
/// Therefore, the error type associated with the response is [Infallible].
pub type ResponseBody = BoxBody<Bytes, Infallible>;

pub type Resp = hyper::Response<ResponseBody>;

//TODO: Do we need to handle errors a la Resto?
fn stream_until_first_error<T: Send + Sync + Debug + 'static, E: Send + Sync + Debug + 'static>(
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
pub fn handle_monitor_request_with_stream<T: Serialize>(
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
pub async fn handle_monitor_request_with_broadcast_receiver<
    T: Serialize + Clone + Send + Sync + Debug + 'static,
>(
    receiver: tokio::sync::broadcast::Receiver<T>,
) -> Result<Resp, RpcError> {
    let stream = tokio_stream::wrappers::BroadcastStream::new(receiver);
    let stream_after_broadcast =
        stream_until_first_error(stream, "Failed to receive value from broadcast channel");
    handle_monitor_request_with_stream(stream_after_broadcast)
}

/// Helper function for handling post requests.
/// The request body is expected to be a json object.
/// This function gets an asynchronous combinator in input to process the request. The output of the
/// combinator function is serialized in json format and sent as the response body in a single frame.
/// This handler returns a [RpcError] in case of connection errors.
pub async fn handle_post_request<
    T: for<'a> Deserialize<'a> + Debug,
    S: Serialize,
    F: FnOnce(
        T,
    ) -> Pin<
        Box<
            dyn 'static
                + Send
                + Future<Output = Result<S, Box<dyn std::error::Error + Send + Sync>>>,
        >,
    >,
>(
    req: Request<Incoming>,
    f: F,
) -> Result<Resp, RpcError> {
    // We fail to deserialize the body request. This is likely a
    let body = &req.collect().await?.to_bytes();
    debug!("Request body: {body:?}");
    let value: T = match serde_json::from_slice(body) {
        Err(e) => {
            debug!("Cannot deserialize received value: {e:?}");
            return bad_request();
        }
        Ok(v) => v,
    };
    debug!("Received POST request, value deserialized to {:?}", value);

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
pub async fn get<
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
