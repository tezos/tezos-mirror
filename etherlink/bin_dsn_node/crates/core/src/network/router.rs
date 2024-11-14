// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Router is a synchronization primitive enabling late binding of different
//! components of the application, that otherwise require manual wiring with
//! multiple channels on the top level (which breaks abstractions).
//!
//! It is basically a pubsub message queue, but with single subscriber per topic,
//! and with a default topic everyone are subscribed to automatically.
//!
//! Router is built on top of the Tokio's MPSC primitive, but has an interface
//! closer to the broadcast channel: sync send and async receive.

use parking_lot::RwLock;
use std::{collections::HashMap, hash::Hash, sync::Arc};
use tokio::sync::mpsc::{self, error::TrySendError};

#[derive(Debug, thiserror::Error)]
pub enum RouterError {
    /// This happens if a component tries to subscribe to a topic which is already handled by other component.
    #[error("Route already exists: {0}")]
    RouteAlreadyExists(String),
    /// This happens if there is no handler for the route specified in the send method (unicast only).
    #[error("No handlers for route: {0}")]
    NoRouteHandlers(String),
    /// This happens if the channel is dropped by the receiving party (unicast only)
    #[error("Route handler has been dropped: {0}")]
    RouteHandlerDropped(String),
    /// This happens if the receiving queue is at its full capacity (unicast only)
    #[error("Route handler is overwhelmed: {0}")]
    RouteHandlerOverwhelmed(String),
}

/// Router can be cheaply cloned and shared across threads.
#[derive(Debug, Clone)]
pub struct Router<Route: Hash + PartialEq + Eq + ToString, Message: Clone> {
    handlers: Arc<RwLock<HashMap<Route, mpsc::Sender<Message>>>>,
}

impl<R: Hash + PartialEq + Eq + ToString, M: Clone> Default for Router<R, M> {
    fn default() -> Self {
        Self {
            handlers: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

impl<R: Hash + PartialEq + Eq + ToString, M: Clone> Router<R, M> {
    /// Remove handlers for all routes.
    pub fn clear(&mut self) -> Result<(), RouterError> {
        self.handlers.write().clear();
        Ok(())
    }

    /// Add handler (subscription) for a particular route.
    ///
    /// Will fail if there is an existing handler for this route.
    pub fn add_handler(
        &mut self,
        route: R,
        queue_size: usize,
    ) -> Result<mpsc::Receiver<M>, RouterError> {
        if self.handlers.read().contains_key(&route) {
            return Err(RouterError::RouteAlreadyExists(route.to_string()));
        }

        let (tx, rx) = mpsc::channel(queue_size);
        self.handlers.write().insert(route, tx);

        Ok(rx)
    }

    /// Send targeted message using the specified route.
    ///
    /// Will fail if something is wrong on the receiving side:
    /// * No handlers
    /// * Handler channel is dropeed (shutdown in progress?)
    /// * Handler queue is full
    pub fn unicast(&self, route: &R, message: M) -> Result<(), RouterError> {
        self.handlers
            .read()
            .get(route)
            .ok_or_else(|| RouterError::NoRouteHandlers(route.to_string()))?
            .try_send(message)
            .map_err(|err| match err {
                TrySendError::Closed(_) => RouterError::RouteHandlerDropped(route.to_string()),
                TrySendError::Full(_) => RouterError::RouteHandlerOverwhelmed(route.to_string()),
            })
    }

    /// Send message to all handlers.
    ///
    /// Returns the number of handlers that have received the message (queues are not full).
    pub fn broadcast(&self, message: M) -> Result<usize, RouterError> {
        Ok(self
            .handlers
            .read()
            .values()
            .filter_map(|tx| tx.try_send(message.clone()).ok().map(|_| 1))
            .sum())
    }

    /// Send message to some handlers based on the predicates.
    ///
    /// Returns the number of handlers that have received the message (queues are not full).
    pub fn multicast<F: Fn(&R) -> bool>(
        &self,
        predicate: F,
        message: M,
    ) -> Result<usize, RouterError> {
        Ok(self
            .handlers
            .read()
            .iter()
            .filter_map(|(route, tx)| {
                if predicate(route) {
                    tx.try_send(message.clone()).ok().map(|_| 1)
                } else {
                    None
                }
            })
            .sum())
    }
}

#[cfg(test)]
mod tests {
    use futures::future::try_join_all;

    use super::{Router, RouterError};

    #[tokio::test]
    async fn test_router_unicast() {
        let mut router = Router::default();
        if let Err(RouterError::NoRouteHandlers(_)) = router.unicast(&1u8, &[0u8]) {
        } else {
            assert!(false)
        }

        let mut handler = router.add_handler(1u8, 1).unwrap();
        router.unicast(&1u8, &[0u8]).unwrap();
        if let Err(RouterError::RouteHandlerOverwhelmed(_)) = router.unicast(&1u8, &[0u8]) {
        } else {
            assert!(false)
        }

        handler.close();
        if let Err(RouterError::RouteHandlerDropped(_)) = router.unicast(&1u8, &[0u8]) {
        } else {
            assert!(false)
        }
    }

    #[tokio::test]
    async fn test_router_broadcast() {
        let mut router = Router::default();
        let mut rx = router.add_handler(0u8, 3).unwrap();

        let mut r1 = router.clone();
        let h1 = tokio::spawn(async move {
            let mut rx = r1.add_handler(1u8, 1).unwrap();
            assert_eq!(1, r1.multicast(|&r| r == 0, &[1u8]).unwrap());
            rx.recv().await.unwrap();
        });

        let mut r2 = router.clone();
        let h2 = tokio::spawn(async move {
            let mut rx = r2.add_handler(2u8, 1).unwrap();
            assert_eq!(1, r2.multicast(|&r| r == 0, &[1u8]).unwrap());
            rx.recv().await.unwrap();
        });

        let mut r3 = router.clone();
        let h3 = tokio::spawn(async move {
            let mut rx = r3.add_handler(3u8, 1).unwrap();
            assert_eq!(1, r3.multicast(|&r| r == 0, &[1u8]).unwrap());
            rx.recv().await.unwrap();
        });

        let mut buffer = Vec::new();
        assert_eq!(3, rx.recv_many(&mut buffer, 3).await);

        rx.close();
        assert_eq!(3, router.broadcast(&[0u8]).unwrap());

        try_join_all([h1, h2, h3]).await.unwrap();
    }
}
