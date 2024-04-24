// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//TODO: Move to separate bundler folder

use std::collections::HashMap;
use std::convert::Infallible;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use http_body_util::combinators::BoxBody;
use hyper::body::{Bytes, Incoming};
use hyper::{Method, Request, StatusCode};

use crate::errors::RpcError;

pub type ResponseBody = BoxBody<Bytes, Infallible>;

// TODO: Handle errors and make the return type of the BoxBody Infallible
pub type Response = hyper::Response<ResponseBody>;

pub type Service<S> = dyn Fn(
        Arc<S>,
        Request<Incoming>,
    ) -> Pin<Box<dyn Future<Output = Result<Response, RpcError>> + Send + 'static>>
    + Send
    + Sync
    + 'static;

pub type Path = String;

pub struct RouterBuilder<S> {
    routes: HashMap<(Method, Path), Box<Service<S>>>,
}

impl<S> RouterBuilder<S> {
    fn new() -> Self {
        Self {
            routes: HashMap::new(),
        }
    }

    pub fn with_route<F>(mut self, path: &str, method: Method, handler: F) -> Self
    where
        F: Fn(
                Arc<S>,
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

    pub fn build(self) -> Router<S> {
        Router {
            routes: self.routes,
        }
    }
}

pub struct Router<S> {
    routes: HashMap<(Method, Path), Box<Service<S>>>,
}

impl<S> Router<S> {
    pub(crate) async fn handle_request(
        &self,
        s: Arc<S>,
        req: Request<Incoming>,
    ) -> Result<Response, RpcError> {
        if let Some(handler) = self
            .routes
            .get(&(req.method().clone(), req.uri().path().to_string()))
        {
            return handler(s, req).await;
        }
        Ok(hyper::Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(BoxBody::default())?)
    }

    pub fn builder() -> RouterBuilder<S> {
        RouterBuilder::<S>::new()
    }
}
