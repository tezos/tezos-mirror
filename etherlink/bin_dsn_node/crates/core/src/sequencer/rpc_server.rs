// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::{net::SocketAddr, sync::Arc};

use futures::FutureExt;

use hyper::{body::Incoming, server::conn::http1, service::service_fn, Method, Request};
use hyper_util::rt::TokioIo;

use tokio::net::TcpListener;
use tracing::info;

use dsn_rpc::errors::RpcError;
use dsn_rpc::rpc_helpers::{
    internal_server_error, monitor_broadcast_channel_handler, not_found_handler, post_handler, Resp,
};

use super::protocol::ProtocolClient;

pub async fn monitor_proposal(
    protocol_client: &ProtocolClient,
    _req: Request<Incoming>,
) -> Result<Resp, RpcError> {
    let receiver = protocol_client.preblock_streams().await;
    monitor_broadcast_channel_handler(receiver).await
}

pub async fn start_server(
    protocol_client: Arc<ProtocolClient>,
    rpc_address: SocketAddr,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let listener = TcpListener::bind(rpc_address).await?;
    info!("Listening on http://{}", rpc_address);

    loop {
        let (tcp, _) = listener.accept().await?;
        let io = TokioIo::new(tcp);
        let protocol_client = protocol_client.clone();
        tokio::task::spawn(async move {
            if let Err(err) = http1::Builder::new()
                .serve_connection(
                    io,
                    service_fn(|req| async {
                        router_service(&protocol_client, req)
                            .await
                            .or_else(|_e| internal_server_error())
                    }),
                )
                .await
            {
                info!("Error serving connection: {:?}", err);
            }
        });
    }
}

pub async fn post_proposal_handler(
    protocol_client: &ProtocolClient,
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

    post_handler(req, f).await
}

async fn router_service(
    protocol_client: &ProtocolClient,
    req: Request<Incoming>,
) -> Result<Resp, RpcError> {
    let (method, path) = (req.method(), req.uri().path());
    match (method, path) {
        (&Method::POST, "/proposal") => {
            let res = post_proposal_handler(protocol_client, req).await.unwrap();
            Ok(res)
        }
        (&Method::GET, "/monitor/preblocks") => {
            let res = monitor_proposal(protocol_client, req).await.unwrap();
            Ok(res)
        }
        _ => not_found_handler().await,
    }
}
