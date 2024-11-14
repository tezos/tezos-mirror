// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Simulated network for testing purposes.
//!
//! Does not require a network manager, the client is self-contained.
//! The connection between peers is done via a shared router.
//! Peers transmit data payloads that consist of sender peer IDs and messages.

// TODO: introduce deterministically random network delays

use std::collections::HashMap;

use async_trait::async_trait;
use tokio::sync::mpsc;

use super::{router::Router, NetworkClient, NetworkError, Topic};

/// Simulated network client
#[derive(Debug)]
pub struct SimulatedPeer {
    pub peer_id: PeerId,
    /// Route is concatenated target peer ID and message topic
    send_router: Router<Route, (PeerId, Vec<u8>)>,
    /// Queue per message topic
    recv_queues: HashMap<Topic, mpsc::Receiver<(PeerId, Vec<u8>)>>,
}

/// Simulated peer ID is single byte
pub type PeerId = u8;

/// Simulated network route is (peer ID, topic)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Route(pub PeerId, pub Topic);

impl ToString for Route {
    fn to_string(&self) -> String {
        format!("[peer={}, topic={}]", self.0, self.1)
    }
}

impl SimulatedPeer {
    /// Create several peers connected with each other.
    ///
    /// Peer ids are equal to the indices in the resulting vector.
    pub fn create_network(size: u8) -> Vec<Self> {
        let router = Router::default();
        (0..size)
            .map(|peer_id| Self {
                peer_id,
                send_router: router.clone(),
                recv_queues: HashMap::new(),
            })
            .collect()
    }
}

impl Clone for SimulatedPeer {
    fn clone(&self) -> Self {
        Self {
            peer_id: self.peer_id,
            send_router: self.send_router.clone(),
            recv_queues: HashMap::new(),
        }
    }
}

#[async_trait]
impl NetworkClient for SimulatedPeer {
    type PeerId = PeerId;

    fn subscribe(self, topic: &Topic, queue_size: usize) -> Result<Self, NetworkError> {
        let mut client = self;
        let route = Route(client.peer_id, *topic);
        let rx = client
            .send_router
            .add_handler(route, queue_size)
            .map_err(|e| NetworkError::Internal(Box::new(e)))?;
        client.recv_queues.insert(*topic, rx);
        Ok(client)
    }

    fn broadcast_bytes(&mut self, topic: &Topic, message: Vec<u8>) -> Result<usize, NetworkError> {
        self.send_router
            .multicast(|route| &route.1 == topic, (self.peer_id, message))
            .map_err(|err| NetworkError::Internal(Box::new(err)))
    }

    fn send_bytes(
        &mut self,
        topic: &Topic,
        peer: &Self::PeerId,
        message: Vec<u8>,
    ) -> Result<(), NetworkError> {
        let route = Route(*peer, *topic);
        self.send_router
            .unicast(&route, (self.peer_id, message))
            .map_err(|err| NetworkError::Internal(Box::new(err)))
    }

    async fn recv_bytes(&mut self, topic: &Topic) -> Result<(Self::PeerId, Vec<u8>), NetworkError> {
        self.recv_queues
            .get_mut(topic)
            .ok_or_else(|| NetworkError::NotSubscribed(*topic))?
            .recv()
            .await
            .ok_or(NetworkError::ShutdownInProgress)
    }
}

#[cfg(test)]
mod tests {
    use futures::future::join_all;
    use itertools::Itertools;
    use serde::{Deserialize, Serialize};

    use super::SimulatedPeer;
    use crate::network::{HasTopic, NetworkClient, NetworkError, Topic};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    struct MessageA {
        a: u32,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    struct MessageB {
        b: u32,
    }

    impl HasTopic for MessageA {
        fn topic() -> Topic {
            Topic(1u8)
        }
    }

    impl HasTopic for MessageB {
        fn topic() -> Topic {
            Topic(2u8)
        }
    }

    #[tokio::test]
    async fn test_simulated_network() {
        let (mut p0, mut p1, mut p2) = SimulatedPeer::create_network(3)
            .into_iter()
            .collect_tuple()
            .unwrap();

        p0 = p0.subscribe(&MessageA::topic(), 1).unwrap();
        p1 = p1.subscribe(&MessageB::topic(), 2).unwrap();
        p2 = p2.subscribe(&MessageB::topic(), 1).unwrap();

        let h0 = tokio::spawn(async move {
            assert_eq!(2, p0.broadcast(&MessageB { b: 1 }).unwrap());
            assert_eq!(1, p0.broadcast(&MessageB { b: 2 }).unwrap());

            let (peer, msg): (u8, MessageA) = p0.recv().await.unwrap();
            assert_eq!(peer, 1u8);
            assert_eq!(msg.a, 3);
        });

        let h1 = tokio::spawn(async move {
            assert_eq!(1, p1.broadcast(&MessageA { a: 3 }).unwrap());

            let (peer, msg): (u8, MessageB) = p1.recv().await.unwrap();
            assert_eq!(peer, 0u8);
            assert_eq!(msg.b, 1);

            let (peer, msg): (u8, MessageB) = p1.recv().await.unwrap();
            assert_eq!(peer, 0u8);
            assert_eq!(msg.b, 2);
        });

        let h2 = tokio::spawn(async move {
            let (peer, msg): (u8, MessageB) = p2.recv().await.unwrap();
            assert_eq!(peer, 0u8);
            assert_eq!(msg.b, 1);

            let res: Result<(u8, MessageA), NetworkError> = p2.recv().await;
            if let Err(NetworkError::NotSubscribed(_)) = res {
            } else {
                assert!(false)
            }

            p2.send_router.clear().unwrap();
            let res: Result<(u8, MessageA), NetworkError> = p2.recv().await;
            if let Err(NetworkError::ShutdownInProgress) = res {
            } else {
                assert!(false)
            }
        });

        join_all([h0, h1, h2]).await;
    }
}
