// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::{net::SocketAddr, sync::Arc};

use dsn_rpc::router::Router;
use futures::FutureExt;

use hyper::{body::Incoming, Method, Request};

use tokio::sync::broadcast;

use dsn_rpc::errors::RpcError;
use dsn_rpc::handlers::{
    handle_monitor_request_with_broadcast_receiver, handle_post_request, Resp,
};
use dsn_rpc::server::RpcServer;

use super::protocol::ProtocolClient;

pub async fn monitor_proposal(
    protocol_client: ProtocolClient,
    _req: Request<Incoming>,
) -> Result<Resp, RpcError> {
    let receiver = protocol_client.preblock_streams().await;
    handle_monitor_request_with_broadcast_receiver(receiver).await
}

pub async fn post_proposal_handler(
    protocol_client: ProtocolClient,
    req: Request<Incoming>,
) -> Result<Resp, RpcError> {
    let protocol_client = protocol_client.clone();
    let f = |proposal| {
        async move {
            protocol_client.submit_proposal(proposal).await?;
            Ok(())
        }
        .boxed()
    };

    handle_post_request(req, f).await
}

pub fn router() -> Router<ProtocolClient> {
    let post_proposal = |protocol_client: ProtocolClient, req| {
        async { post_proposal_handler(protocol_client, req).await }.boxed()
    };

    let monitor_proposal = |protocol_client: ProtocolClient, req| {
        async { monitor_proposal(protocol_client, req).await }.boxed()
    };

    Router::builder()
        .with_route("/proposal", Method::GET, post_proposal)
        .with_route("/monitor/preblocks", Method::GET, monitor_proposal)
        .build()
}

pub async fn start_server(
    listening_addr: SocketAddr,
    protocol_client: ProtocolClient,
    rx_shutdown: broadcast::Receiver<Arc<dyn std::error::Error + Send + Sync>>,
    tx_shutdown: broadcast::Sender<Arc<dyn std::error::Error + Send + Sync>>,
) -> Result<(), RpcError> {
    let router = router();

    let mut server = RpcServer::new(listening_addr, rx_shutdown, tx_shutdown, protocol_client);

    server.serve(router).await
}
