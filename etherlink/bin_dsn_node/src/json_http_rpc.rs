// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::convert::Infallible;
use std::fmt::Debug;

use futures::StreamExt;
use http_body_util::combinators::BoxBody;
use http_body_util::{BodyExt, BodyStream, Full};
use hyper::body::{Buf, Bytes, Incoming};
use hyper::client::conn::http1::SendRequest;
use hyper::http::request::Builder;
use hyper::{Request, Response};
use serde::de::{DeserializeOwned, StdError};
use serde::Serialize;

type Body = BoxBody<Bytes, Infallible>;

// TODO: This function would benefit from a refactoring
pub(crate) async fn transform_and_proxy_request<T, V, R, B, F>(
    mut request_sender: SendRequest<Body>,
    request: Request<Incoming>,
    f: F,
) -> anyhow::Result<T>
where
    T: Serialize + DeserializeOwned + Debug,
    B: http_body::Body,
    <B as http_body::Body>::Error: StdError + Send + Sync,
    F: FnOnce(V) -> R,
    R: Serialize,
    V: DeserializeOwned + Serialize + Debug,
{
    let proxied_request: Builder = hyper::Request::builder()
        .method(request.method())
        .uri(request.uri());

    let request_body: V = serde_json::from_slice(
        BodyStream::new(request.into_body())
            .filter_map(|f| async move { Some::<Vec<u8>>(f.ok()?.into_data().ok()?.into()) })
            .concat()
            .await
            .as_slice(),
    )?;

    let transformed_request_body: BoxBody<Bytes, Infallible> =
        Full::new(Bytes::from(serde_json::to_string(&f(request_body))?)).boxed();

    // TODO: Decouple forwarding the request
    let proxied_request: Request<BoxBody<Bytes, Infallible>> =
        proxied_request.body(transformed_request_body)?;

    let response: Response<Incoming> = request_sender.send_request(proxied_request).await?;

    let body = response.collect().await?.aggregate();

    Ok(serde_json::from_reader(body.reader())?)
}
