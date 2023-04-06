// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! The possible types that may be returned by (Runtime)[crate::runtime::Runtime] when reading an input.
//!
//! *N.B.* Reading input is currently only supported when the `alloc` feature is enabled.
#![cfg(feature = "alloc")]

use alloc::vec::Vec;

/// An input from Layer 1 contains the inbox level, message number, and message payload.
#[derive(Debug, PartialEq, Eq)]
pub struct Message {
    /// Inbox level of this message.
    pub level: u32,
    /// The message index in the rollup inbox.
    pub id: u32,
    payload: Vec<u8>,
}

impl Message {
    /// Create a message input.
    pub const fn new(level: u32, id: u32, payload: Vec<u8>) -> Self {
        Self { level, id, payload }
    }
}

impl AsRef<[u8]> for Message {
    fn as_ref(&self) -> &[u8] {
        self.payload.as_ref()
    }
}
