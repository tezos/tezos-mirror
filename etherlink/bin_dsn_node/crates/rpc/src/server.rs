// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//TODO: Move to separate bundler folder

use std::error::Error;
use std::future::Future;
use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::Arc;

use http_body_util::combinators::BoxBody;
use hyper::body::{Bytes, Incoming};
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::Request;
use hyper_util::rt::TokioIo;
use serde::{Deserialize, Serialize};
use tokio::net::TcpListener;
use tokio::sync::broadcast;
use tracing::{error, info, warn};

use crate::errors::RpcError;
use crate::router::Router;

// TODO: Handle errors and make the return type of the BoxBody Infallible
pub type Response = hyper::Response<BoxBody<Bytes, Box<dyn Error + Send + Sync>>>;

pub type Service = dyn Fn(
        Request<Incoming>,
    ) -> Pin<Box<dyn Future<Output = Result<Response, RpcError>> + Send + Sync + 'static>>
    + Send
    + Sync
    + 'static;

pub type Path = String;

/// A singleton for spawning main RPC server thread.
#[derive(Debug)]
pub struct RpcServer<S> {
    /// RPC server configuration.
    config: RpcConfig,
    /// Shutdown receiver.
    rx_shutdown: broadcast::Receiver<Arc<dyn Error + Send + Sync>>,
    /// Shutdown sender.
    tx_shutdown: broadcast::Sender<Arc<dyn Error + Send + Sync>>,
    state: Arc<S>,
}

/// RPC server configuration.
#[derive(Debug, Serialize, Deserialize)]
pub struct RpcConfig {
    host: String,
    port: u16,
    socket_addr: SocketAddr,
}

impl<S: Send + Sync + Clone + 'static> RpcServer<S> {
    pub fn new(
        listening_addr: SocketAddr,
        rx_shutdown: broadcast::Receiver<Arc<dyn Error + Send + Sync>>,
        tx_shutdown: broadcast::Sender<Arc<dyn Error + Send + Sync>>,
        state: S,
    ) -> RpcServer<S> {
        RpcServer {
            config: RpcConfig {
                host: listening_addr.ip().to_string(),
                port: listening_addr.port(),
                socket_addr: listening_addr,
            },
            rx_shutdown,
            tx_shutdown,
            state: Arc::new(state),
        }
    }

    pub async fn serve(&mut self, app: Router<S>) -> Result<(), RpcError> {
        let listener = TcpListener::bind(self.config.socket_addr).await;
        match listener {
            Err(e) => {
                if self.tx_shutdown.send(Arc::new(e)).is_err() {
                    error!(
                        "Server failed to bind for address {}",
                        self.config.socket_addr
                    );
                }
                Ok(())
            }
            Ok(listener) => {
                info!("Listening on http://{}", self.config.socket_addr);
                let app = Arc::new(app);
                loop {
                    tokio::select! {
                        Ok((tcp, _)) = listener.accept() => {
                            let io = TokioIo::new(tcp);
                            let app = app.clone();
                            // TODO: This is needed in the sequencer because resubscription to broadcast
                            // channels must happen by cloning the ProtocolClient.
                            // Should we allow cloning the state in general?
                            let state = Arc::new((*self.state).clone());
                            tokio::task::spawn(async move {
                                if let Err(err) = http1::Builder::new()
                                    .serve_connection(io, service_fn(|req| app.handle_request(state.clone(), req)))
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
    }
}
