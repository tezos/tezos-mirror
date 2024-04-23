// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::convert::Infallible;
use std::error::Error as StdError;
use std::net::SocketAddr;
use std::sync::Arc;

use anyhow::Result;
use dsn_rpc::router::Router;
use futures::FutureExt;
use http_body_util::combinators::BoxBody;
use http_body_util::{BodyExt, Full};
use hyper::body::{Bytes, Incoming};
use hyper::client::conn::http1::SendRequest;
use hyper::{Method, Request};
use hyper_util::rt::TokioIo;
use tokio::net::TcpStream;
use tokio::sync::broadcast;
use tracing::{error, info};
use url::Url;

use crate::errors::Error;
use crate::json_http_rpc;
use dsn_rpc::rpc_encoding::{SendRawTransaction, SendRawTransactionResult};
use dsn_rpc::server::RpcServer;

// TODO: Move to separate bundler crate

// TODO: Handle errors and make the return type of the BoxBody Infallible
type Response = hyper::Response<BoxBody<Bytes, Infallible>>;

pub async fn run(
    listening_addr: SocketAddr,
    sequencer_endpoint: Url,
    rx_shutdown: broadcast::Receiver<Arc<dyn StdError + Send + Sync>>,
    tx_shutdown: broadcast::Sender<Arc<dyn StdError + Send + Sync>>,
) -> std::result::Result<(), ()> {
    match proxy_server(listening_addr, sequencer_endpoint, rx_shutdown, tx_shutdown).await {
        Err(err) => {
            error!("Protocol runner failed with {}", err);
            Err(())
        }
        Ok(()) => {
            info!("Protocol runner terminated");
            Ok(())
        }
    }
}

fn parse_url(url: Url) -> Result<(String, u16)> {
    let host = url.host_str().ok_or(Error::UriHostMissing)?;
    let port = url.port().ok_or(Error::UriPortMissing)?;
    Ok((host.to_string(), port))
}

async fn connect<B>(upstream_server: Url) -> Result<SendRequest<B>>
where
    B: hyper::body::Body + 'static + Send,
    B::Data: Send,
    B::Error: Into<Box<dyn StdError + Send + Sync>>,
{
    let stream = TcpStream::connect(parse_url(upstream_server)?).await?;
    let io = TokioIo::new(stream);

    let (sender, conn) = hyper::client::conn::http1::handshake(io).await?;
    tokio::task::spawn(async move {
        if let Err(err) = conn.await {
            info!("Connection failed: {:?}", err);
        }
    });
    Ok(sender)
}

async fn connect_exponential_backoff<B>(upstream_server: &Url) -> Result<SendRequest<B>>
where
    B: hyper::body::Body + 'static + Send,
    B::Data: Send,
    B::Error: Into<Box<dyn StdError + Send + Sync>>,
{
    let retry_strategy = tokio_retry::strategy::ExponentialBackoff::from_millis(10)
        .map(tokio_retry::strategy::jitter) // add jitter to delays
        .take(3); // limit to 3 retries

    tokio_retry::Retry::spawn(retry_strategy, || connect(upstream_server.clone())).await
}

async fn proxy_service(req: Request<Incoming>, upstream_server: Arc<Url>) -> Result<Response> {
    let sender = connect_exponential_backoff(&upstream_server).await?;

    // TODO: Parse the JSON RPC request first
    let value = json_http_rpc::transform_and_proxy_request::<
        Vec<SendRawTransactionResult>,
        Vec<SendRawTransaction>,
        _,
        Full<Bytes>,
        _,
    >(sender, req, |e| e)
    .await?;

    let body = Full::new(serde_json::to_string(&value)?.into())
        .map_err(|e| match e {})
        .boxed();

    Ok(Response::new(BoxBody::new(body)))
}

async fn proxy_server(
    listening_addr: SocketAddr,
    upstream_server: Url,
    rx_shutdown: broadcast::Receiver<Arc<dyn StdError + Send + Sync>>,
    tx_shutdown: broadcast::Sender<Arc<dyn StdError + Send + Sync>>,
) -> Result<()> {
    let upstream_server = Arc::new(upstream_server);
    let app = {
        Router::builder()
            .with_route("/", Method::POST, {
                let upstream_server = upstream_server.clone();
                move |(), req| proxy_service(req, upstream_server.clone()).boxed()
            })
            .build()
    };

    let mut server = RpcServer::new(listening_addr, rx_shutdown, tx_shutdown, ());

    server.serve(app).await
}
