// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Network client abstraction.
//!
//! This is an opinionated interface tailored to the DSN case:
//! a small permissioned network where peers can send messages
//! to one or all peers (broadcast).
//!
//! Network client typically works in tandem with a network manager,
//! which actually handles external connections. Network clients can be
//! cheaply cloned and used from different components of the application.
//!
//! The interface allows to dispense incoming messages into several
//! independent queues identified by the message topic, which allows
//! to decouple different components that share the access to the network (manager).
//!
//! Finally, clients can track the message sender in order to e.g. reply with
//! a targeted response.

pub mod router;
pub mod simulated;

use async_trait::async_trait;
use serde::{de::DeserializeOwned, Serialize};

/// Generic error type that can be used to handle special scenarios.
///
/// Network client implementations have to respect this enumeration and cast
/// their internal errors to the generic variants if applicable.
#[derive(Debug, thiserror::Error)]
pub enum NetworkError {
    /// Implement `From<YourErrorType>` for `NetworkError` for convenience.
    #[error("Internal network error: {0}")]
    Internal(#[source] Box<dyn std::error::Error + Send + Sync + 'static>),
    /// This happens when client tries to receive a message for a topic he has not subscribed to.
    #[error("Not subscribed to this particular topic: {0}")]
    NotSubscribed(Topic),
    /// This happens when channels are closed which typically means the application is shutting down.
    #[error("Shutdown in progress")]
    ShutdownInProgress,
    /// This happens when client fails to encode/decode typed message into/from bytes.
    #[error("Failed to encode/decode message: {0}")]
    BinCodec(#[source] Box<bcs::Error>),
}

/// Network client trait contains abstract methods for transmitting raw bytes and
/// auto methods for handling typed messages using those raw primitives.
#[async_trait]
pub trait NetworkClient: Clone + Send + Sync + 'static {
    /// Peer ID is implementation specific
    type PeerId: Sized + Clone + Send + Sync + 'static;

    /// Create a clone of the client that is subscribed to a particular topic.
    ///
    /// Note that the specified queue size is the maximum capacity of the channel.
    /// If it is overwhelmed, the messages will be lost.
    fn subscribe(self, topic: &Topic, queue_size: usize) -> Result<Self, NetworkError>;

    /// Send a raw message with a given topic to the specified peer.
    ///
    /// Note that if the receiver has not subscribed to this topic,
    /// the message will be dropped on his end.
    fn send_bytes(
        &mut self,
        topic: &Topic,
        peer: &Self::PeerId,
        message: Vec<u8>,
    ) -> Result<(), NetworkError>;

    /// Send a raw message to all peers that have subscribed to the specified topic.
    fn broadcast_bytes(&mut self, topic: &Topic, message: Vec<u8>) -> Result<usize, NetworkError>;

    /// Receive a raw message with a specified topic from any peer.
    ///
    /// If the client has not subscribed to that topic, this method will fail with an error.
    async fn recv_bytes(&mut self, topic: &Topic) -> Result<(Self::PeerId, Vec<u8>), NetworkError>;

    /// Send a typed message to the specified peer.
    ///
    /// Note that if the receiver has not subscribed to the topic assosiated with the type,
    /// the message will be dropped on his end.
    fn send<T: HasTopic + Serialize + Send + Sync>(
        &mut self,
        peer: &Self::PeerId,
        message: &T,
    ) -> Result<(), NetworkError> {
        let bytes = bcs::to_bytes(message)?;
        self.send_bytes(&T::topic(), peer, bytes)
    }

    /// Send a raw message to all peers that have subscribed to the topic assosiated with the type.
    fn broadcast<T: HasTopic + Serialize + Send + Sync>(
        &mut self,
        message: &T,
    ) -> Result<usize, NetworkError> {
        let bytes = bcs::to_bytes(message)?;
        self.broadcast_bytes(&T::topic(), bytes)
    }

    /// Receive a typed message from any peer.
    ///
    /// If the client has not subscribed to the topic assosiated with the type,
    /// this method will fail with an error.
    async fn recv<T: HasTopic + DeserializeOwned>(
        &mut self,
    ) -> Result<(Self::PeerId, T), NetworkError> {
        let (peer, bytes) = self.recv_bytes(&T::topic()).await?;
        let message = bcs::from_reader(bytes.as_slice())?;
        Ok((peer, message))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Topic(pub u8);

impl std::fmt::Display for Topic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

/// This trait has to be implemented for any message type transmitted over the network.
pub trait HasTopic {
    fn topic() -> Topic;
}

impl From<bcs::Error> for NetworkError {
    fn from(value: bcs::Error) -> Self {
        Self::BinCodec(Box::new(value))
    }
}
