// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Module for building route tables for http requests.

use std::collections::HashMap;
use std::convert::Infallible;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use http_body_util::combinators::BoxBody;
use hyper::body::{Bytes, Incoming};
use hyper::{Method, Request};
use tracing::debug;

use crate::errors::RpcError;
use crate::responses::not_found;

pub type ResponseBody = BoxBody<Bytes, Infallible>;

pub type Response = hyper::Response<ResponseBody>;

/// The type of a service handler that will be executed
/// once a request has been routed to that service.
/// It is an asynchronous function that takes in input an atomic
/// reference to the server state, as well as the whole request to
/// be processed.
pub type Service<S> = dyn Fn(
        S,
        Request<Incoming>,
    ) -> Pin<Box<dyn Future<Output = Result<Response, RpcError>> + Send + 'static>>
    + Send
    + Sync
    + 'static;

/// Auxiliary builder factory for building routers.
pub struct RouterBuilder<S> {
    routes: HashMap<(Method, String), Box<Service<S>>>,
}

impl<S> RouterBuilder<S> {
    fn new() -> Self {
        Self {
            routes: HashMap::new(),
        }
    }

    /// Binds a route to a service handler. The result is a [RouterBuilder] that
    /// can be used to append more routes
    pub fn with_route<F>(mut self, path: &str, method: Method, handler: F) -> Self
    where
        F: Fn(
                S,
                Request<Incoming>,
            ) -> Pin<Box<dyn Send + Future<Output = Result<Response, RpcError>>>>
            + Send
            + Sync
            + 'static,
    {
        self.routes
            .insert((method, path.to_string()), Box::new(handler));
        self
    }

    /// Returns a new [Router] built using the routes specified by [self].]
    pub fn build(self) -> Router<S> {
        Router {
            routes: self.routes,
        }
    }
}

pub struct Router<S> {
    routes: HashMap<(Method, String), Box<Service<S>>>,
}

impl<S: Clone> Router<S> {
    /// Handles a http request received by an external client. The service
    /// handler for handling the request is retrieved from the [Router]'s internal
    /// [HashMap], using the request [Method]  and request path.
    /// The body of the request must be of type [Incoming], which means that
    /// the request can be chunked into frames. When successful, the result is
    /// the [Response] that is returned to the client, or an error of type [RpcError].
    pub(crate) async fn handle_request(
        &self,
        s: Arc<S>,
        req: Request<Incoming>,
    ) -> Result<Response, RpcError> {
        debug!(
            "Received Request {:?} {:?} - Resolving route",
            req.method(),
            req.uri().path().to_string()
        );
        if let Some(handler) = self
            .routes
            .get(&(req.method().clone(), req.uri().path().to_string()))
        {
            let state = (*s).clone();
            return handler(state, req).await;
        }
        debug!("Route not found");
        not_found()
    }

    /// Returns a new [RouterBuilder].
    pub fn builder() -> RouterBuilder<S> {
        RouterBuilder::<S>::new()
    }
}
