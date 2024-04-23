// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//TODO: Move to separate bundler folder

use std::collections::HashMap;
use std::error::Error;
use std::future::Future;
use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::Arc;

use anyhow::Result;
use http_body_util::combinators::BoxBody;
use hyper::body::{Bytes, Incoming};
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Method, Request, StatusCode};
use hyper_util::rt::TokioIo;
use serde::{Deserialize, Serialize};
use tokio::net::TcpListener;
use tokio::sync::broadcast;
use tracing::{error, info, warn};

// TODO: Handle errors and make the return type of the BoxBody Infallible
pub type Response = hyper::Response<BoxBody<Bytes, Box<dyn Error + Send + Sync>>>;

pub type Service = dyn Fn(Request<Incoming>) -> Pin<Box<dyn Future<Output = Result<Response>> + Send + Sync + 'static>>
    + Send
    + Sync
    + 'static;

pub type Path = String;

pub struct Router {
    routes: HashMap<(Method, Path), Box<Service>>,
}

impl Router {
    pub fn new() -> Self {
        Router {
            routes: HashMap::new(),
        }
    }

    pub fn route<F, Fut>(mut self, path: &str, method: Method, handler: F) -> Self
    where
        F: Fn(Request<Incoming>) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Response>> + Send + Sync + 'static,
    {
        let service = move |req| {
            Box::pin(handler(req))
                as Pin<Box<dyn Future<Output = Result<Response>> + Send + Sync + 'static>>
        };
        self.routes
            .insert((method, path.to_string()), Box::new(service));
        self
    }

    async fn handle_request(&self, req: Request<Incoming>) -> Result<Response> {
        if let Some(handler) = self
            .routes
            .get(&(req.method().clone(), req.uri().path().to_string()))
        {
            return handler(req).await;
        }
        Ok(hyper::Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(BoxBody::default())?)
    }
}
/// A singleton for spawning main RPC server thread.
#[derive(Debug)]
pub struct RpcServer {
    /// RPC server configuration.
    config: RpcConfig,
    /// Shutdown receiver.
    rx_shutdown: broadcast::Receiver<Arc<dyn Error + Send + Sync>>,
    /// Shutdown sender.
    tx_shutdown: broadcast::Sender<Arc<dyn Error + Send + Sync>>,
}

/// RPC server configuration.
#[derive(Debug, Serialize, Deserialize)]
pub struct RpcConfig {
    host: String,
    port: u16,
    socket_addr: SocketAddr,
}

impl RpcServer {
    pub fn new(
        listening_addr: SocketAddr,
        rx_shutdown: broadcast::Receiver<Arc<dyn Error + Send + Sync>>,
        tx_shutdown: broadcast::Sender<Arc<dyn Error + Send + Sync>>,
    ) -> RpcServer {
        RpcServer {
            config: RpcConfig {
                host: listening_addr.ip().to_string(),
                port: listening_addr.port(),
                socket_addr: listening_addr,
            },
            rx_shutdown,
            tx_shutdown,
        }
    }

    pub async fn serve(&mut self, app: Router) -> Result<()> {
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
                            tokio::task::spawn(async move {
                                if let Err(err) = http1::Builder::new()
                                    .serve_connection(io, service_fn(|req| app.handle_request(req)))
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
