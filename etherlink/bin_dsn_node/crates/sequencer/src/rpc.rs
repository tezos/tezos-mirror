// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::net::SocketAddr;

use dsn_rpc::{
    errors::RpcError,
    handlers::{handle_monitor_request_with_broadcast_receiver, handle_post_request, Resp},
    router::Router,
    server::RpcServer,
};
use futures::FutureExt;
use hyper::{body::Incoming, Method, Request};
use tokio::sync::broadcast;
use tracing::{debug, error, info};

use crate::client::SequencerClient;

pub struct SequencerRpc(RpcServer<SequencerClient>);

impl SequencerRpc {
    pub fn new(
        listening_addr: SocketAddr,
        rx_shutdown: broadcast::Receiver<()>,
        client: SequencerClient,
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

fn router() -> Router<SequencerClient> {
    let post_proposal =
        |client: SequencerClient, req| async { post_proposal_handler(client, req).await }.boxed();

    let monitor_proposal =
        |client: SequencerClient, req| async { monitor_proposal(client, req).await }.boxed();

    Router::builder()
        .with_route("/proposal", Method::POST, post_proposal)
        .with_route("/monitor/preblocks", Method::GET, monitor_proposal)
        .build()
}

async fn monitor_proposal(
    client: SequencerClient,
    _req: Request<Incoming>,
) -> Result<Resp, RpcError> {
    debug!("New subscription to preblocks");
    let receiver = client.preblock_streams().await;
    handle_monitor_request_with_broadcast_receiver(receiver).await
}

async fn post_proposal_handler(
    client: SequencerClient,
    req: Request<Incoming>,
) -> Result<Resp, RpcError> {
    let client = client.clone();
    let f = |proposal| {
        async move {
            client.submit_proposal(proposal).await?;
            Ok(())
        }
        .boxed()
    };

    handle_post_request(req, f).await
}
