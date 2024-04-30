// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::convert::Infallible;
use std::future::Future;
use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::Arc;

use futures::FutureExt;
use http_body_util::combinators::BoxBody;
use hyper::body::{Bytes, Incoming};
use hyper::server::conn::http1;
use hyper::service::Service;
use hyper::Request;
use hyper_util::rt::TokioIo;
use serde::{Deserialize, Serialize};
use tokio::net::TcpListener;
use tokio::sync::broadcast;
use tracing::{debug, info, warn};

use crate::errors::RpcError;
use crate::handlers::Resp;
use crate::router::Router;

pub type Response = hyper::Response<BoxBody<Bytes, Infallible>>;

/// A singleton for spawning main RPC server thread.
#[derive(Debug)]
pub struct RpcServer<S> {
    /// RPC server configuration.
    config: RpcConfig,
    /// Shutdown receiver.
    rx_shutdown: broadcast::Receiver<()>,
    state: Arc<S>,
}

/// RPC server configuration.
#[derive(Debug, Serialize, Deserialize)]
pub struct RpcConfig {
    host: String,
    port: u16,
    socket_addr: SocketAddr,
}

struct RpcService<S> {
    router: Arc<Router<S>>,
    state: Arc<S>,
}

impl<S: Send + Sync + Clone + 'static> RpcServer<S> {
    pub fn new(
        listening_addr: SocketAddr,
        rx_shutdown: broadcast::Receiver<()>,
        state: S,
    ) -> RpcServer<S> {
        RpcServer {
            config: RpcConfig {
                host: listening_addr.ip().to_string(),
                port: listening_addr.port(),
                socket_addr: listening_addr,
            },
            rx_shutdown,
            state: Arc::new(state),
        }
    }

    pub async fn serve(&mut self, router: Router<S>) -> Result<(), RpcError> {
        let listener = TcpListener::bind(self.config.socket_addr)
            .await
            .map_err(RpcError::TcpListenerError)?;
        info!("Listening on http://{}", self.config.socket_addr);
        let router = Arc::new(router);
        loop {
            tokio::select! {
                Ok((tcp, ip_address)) = listener.accept() => {
                    debug!("Connection from {:?}", ip_address);
                    let io = TokioIo::new(tcp);
                    let service = RpcService {
                        router: router.clone(),
                        state: self.state.clone(),
                    };
                    tokio::task::spawn(async move {
                        if let Err(err) = http1::Builder::new()
                            .serve_connection(io, service)
                            .await
                        {
                            warn!("Error serving connection: {:?}", err);
                        }
                    });
                },
                _ = self.rx_shutdown.recv() => {
                    return Ok(())
                },
            }
        }
    }
}

impl<S: Send + Sync + Clone + 'static> Service<Request<Incoming>> for RpcService<S> {
    type Response = Resp;
    type Error = RpcError;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

    fn call(&self, req: Request<Incoming>) -> Self::Future {
        let router = self.router.clone();
        let state = self.state.clone();
        async move { router.handle_request(state, req).await }.boxed()
    }
}
