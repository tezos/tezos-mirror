// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::net::SocketAddr;

use dsn_core::hex_string::HexString;
use dsn_rpc::errors::RpcError;
use dsn_rpc::handlers::Resp;
use dsn_rpc::jsonrpc::{handle_jsonrpc_request, JsonRpcRequest};
use dsn_rpc::router::Router;
use futures::FutureExt;
use hyper::body::Incoming;
use hyper::{Method, Request};
use jsonrpsee_types::{ErrorCode, ErrorObjectOwned};
use tokio::sync::broadcast;
use tracing::{error, info, warn};

use crate::client::BundlerClient;
use dsn_rpc::server::RpcServer;

pub struct BundlerRpc(RpcServer<BundlerClient>);

impl BundlerRpc {
    pub fn new(
        listening_addr: SocketAddr,
        rx_shutdown: broadcast::Receiver<()>,
        client: BundlerClient,
    ) -> Self {
        Self(RpcServer::new(listening_addr, rx_shutdown, client))
    }

    pub async fn run(&mut self) -> Result<(), ()> {
        match self.0.serve(router()).await {
            Err(err) => {
                error!("RPC server failed with {}", err);
                Err(())
            }
            Ok(()) => {
                info!("RPC server terminated");
                Ok(())
            }
        }
    }
}

fn router() -> Router<BundlerClient> {
    Router::builder()
        .with_route("/", Method::POST, move |client, req| {
            handle_http_post_request(client, req).boxed()
        })
        .build()
}

async fn handle_http_post_request(
    client: BundlerClient,
    req: Request<Incoming>,
) -> Result<Resp, RpcError> {
    handle_jsonrpc_request(req, move |jsonrpc_req| {
        handle_eth_jsonrpc_request(client.clone(), jsonrpc_req).boxed()
    })
    .await
}

async fn handle_eth_jsonrpc_request(
    client: BundlerClient,
    req: JsonRpcRequest,
) -> Result<HexString, ErrorObjectOwned> {
    match req.method.as_str() {
        "eth_sendRawTransaction" => {
            let tx = req.params().one::<HexString>()?;
            let tx_hash = client.bundle_raw_transaction(&tx.0).await.map_err(|e| {
                warn!("Bundler failed with {}", e);
                ErrorObjectOwned::from(ErrorCode::InternalError)
            })?;
            Ok(HexString(tx_hash.to_vec()))
        }
        method => {
            warn!("Method not supported: {}", method);
            Err(ErrorObjectOwned::from(ErrorCode::MethodNotFound))
        }
    }
}
