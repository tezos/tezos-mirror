// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! The *outbox* allows kernels to send messages from their rollup (Layer 2) to the Tezos layer 1.
//!
//! Messages are contract calls, and may contain *tickets*. For every Tezos level, the kernel may
//! produce _up to 100_ outbox messages.
//!
//! The most common use of outbox messages is to implement a _withdrawal-like_ flow. Typically,
//! a user may deposit funds or assets into a rollup by sending tickets from layer 1, to the rollup
//! using an [inbox message]. Outbox messages are the corollary, allowing tickets to be sent
//! from a user's layer 2 account, back to their layer 1 account.
//!
//! # The outbox queue
//!
//! Due to the limit of 100 outbox messages per level, there may be times when higher demand for
//! withdrawals causes bottlenecks. To avoid degraded user-experience, the SDK provides a mechanism
//! for managing temporary spikes in withdrawals (or other messages): the [outbox queue].
//!
//! The outbox queue allows for a larger number of messages to be 'queued' for sending to the outbox,
//! and the ability to flush the queue.
//!
//! ## Example usage
//!
//! Say we wanted to send a message to the outbox, for every inbox message received. There may be more
//! than 100 inbox messages, so we need to use the outbox queue. There is a [default] instance provided
//! by the SDK, ready for use.
//!
//! We'll send this message to the contract *KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc* on layer 1.
//!
//! *NB* the outbox queue does not allow an infinite number of messages to be queued, but rather a much
//! higher 'maximum'. Unlike below, you should still be cautious about allowing an unbounded number of
//! messages to be pushed to the queue.
//!
//! ```rust
//! use tezos_protocol::contract::Contract;
//! use tezos_smart_rollup::prelude::*;
//! use tezos_smart_rollup::outbox::*;
//! use tezos_smart_rollup::inbox::InboxMessage;
//! use tezos_smart_rollup::michelson::MichelsonString;
//! use tezos_smart_rollup::types::Entrypoint;
//!
//! fn kernel_run(host: &mut impl Runtime) {
//!   while let Ok(Some(message)) = host.read_input() {
//!     debug_msg!(host, "found {message:?}");
//!     let message = make_outbox_message(message.level, message.id);
//!
//!     OUTBOX_QUEUE.queue_message(host, message).expect("able to queue message");
//!   }
//!
//!   let num_flushed = OUTBOX_QUEUE.flush_queue(host);
//!   debug_msg!(host, "wrote {num_flushed} messages to the outbox");
//! }
//!
//! fn make_outbox_message(level: u32, index: u32) -> OutboxMessageTransaction<MichelsonString> {
//!    let message = format!("message at level {level} and index {index}");
//!    OutboxMessageTransaction {
//!      parameters: message.into(),
//!      destination: Contract::from_b58check("KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc").unwrap(),
//!      entrypoint: Entrypoint::default()
//!    }
//! }
//!
//! # use tezos_smart_rollup::testing::prelude::*;
//! # let mut host = MockHost::default();
//! # let level = host.run_level(kernel_run);
//! # assert_eq!(3, host.outbox_at(level).len());
//! ```
//!
//! [inbox message]: crate::inbox
//! [outbox queue]: OutboxQueue
//! [default]: OUTBOX_QUEUE

use tezos_data_encoding::enc::BinWriter;
use tezos_smart_rollup_core::MAX_OUTPUT_SIZE;
#[doc(inline)]
pub use tezos_smart_rollup_encoding::outbox::*;
use tezos_smart_rollup_host::path::{concat, Path, PathError};
use tezos_smart_rollup_host::Error;
use tezos_smart_rollup_host::{
    path::{self, OwnedPath, RefPath, PATH_MAX_SIZE},
    runtime::{Runtime, RuntimeError},
};

use alloc::vec::Vec;

const OUTBOX_QUEUE_ROOT: RefPath = RefPath::assert_from(b"/__sdk/outbox");
const OUTBOX_QUEUE_META: RefPath = RefPath::assert_from(b"/__sdk/outbox/meta");
const META_SUFFIX: RefPath = RefPath::assert_from(b"/meta");

/// The default outbox queue.
///
/// Unless you have good reason to, you should probably use this instance.
///
/// It is configured with a maximum queue size of 65536. If you flush the queue every
/// tezos level, it will take roughly 2h45mins for a message to make its way through
/// the queue, and be written to the outbox.
pub const OUTBOX_QUEUE: OutboxQueue<'static, RefPath> = OutboxQueue {
    root: &OUTBOX_QUEUE_ROOT,
    meta: None,
    max: u16::MAX as u32,
};

/// An outbox queue, allowing for messages to be queued and flushed separately.
///
/// See [`OUTBOX_QUEUE`] for more information.
#[derive(Debug)]
pub struct OutboxQueue<'a, P: Path> {
    root: &'a P,
    meta: Option<OwnedPath>,
    max: u32,
}

impl<'a, P: Path> OutboxQueue<'a, P> {
    /// Setup the outbox queue to operate over non-default parameters.
    ///
    /// - `root` specifies the path in storage that the queue will be stored at.
    /// - `max` allows you to specify a different maximum queue length to the default.
    pub fn new(root: &'a P, max: u32) -> Result<Self, PathError> {
        // Ensure we have enough room for the hex-encoded u32 indexes
        if root.size() > PATH_MAX_SIZE - 2 * core::mem::size_of::<u32>() + 1 {
            return Err(PathError::PathTooLong);
        }

        // The meta suffix is less long than the index paths.
        let meta = concat(root, &META_SUFFIX)?;

        Ok(Self {
            root,
            max,
            meta: Some(meta),
        })
    }

    /// Queues a message into the outbox queue.
    ///
    /// See [`flush_queue`] to actually produce the messages in the outbox.
    /// Returns the current length of the queue.
    ///
    /// [`flush_queue`]: Self::flush_queue
    pub fn queue_message<Batch: AtomicBatch>(
        &self,
        host: &mut impl Runtime,
        message: impl Into<OutboxMessageFull<Batch>>,
    ) -> Result<usize, RuntimeError> {
        let (start, len) = self.read_meta(host);

        if len >= self.max {
            return Err(RuntimeError::HostErr(Error::FullOutbox));
        }

        let end = start.wrapping_add(len);

        let message = message.into();

        let mut buffer = Vec::with_capacity(MAX_OUTPUT_SIZE);
        message
            .bin_write(&mut buffer)
            .expect("outbox message always serializable");

        if buffer.len() > MAX_OUTPUT_SIZE {
            return Err(RuntimeError::HostErr(Error::InputOutputTooLarge));
        }

        let path = self.message_path(end);

        host.store_write_all(&path, buffer.as_slice())?;

        let len = len.saturating_add(1);
        self.save_meta(host, start, len);

        Ok(len as usize)
    }

    /// Flush queue pops up to 100 messages from the queue, and writes them to the outbox proper.
    ///
    /// Returns the number of messages written. This may be less than 100, if messages have been
    /// written to the outbox at the current level before calling `flush_queue`.
    ///
    /// # SAFETY
    ///
    /// This function *must never* be called from a re-tryable context. If it is possible to trigger
    /// `flush_queue` twice, on the same *durable storage state*, then a single message could be
    /// executed twice - leading to double spending of an L2 account's funds on L1 - this would result
    /// later in some accounts being unable to execute otherwise valid withdrawals on L1.
    pub fn flush_queue(&self, host: &mut impl Runtime) -> usize {
        // Consider: `store_read_extend(&mut Vec)`
        let mut num_flushed = 0;
        let (mut start, mut len) = self.read_meta(host);

        loop {
            if len == 0 {
                return num_flushed;
            }

            let next = self.message_path(start);
            // We know this path exists according to `meta`.
            let read = host.store_read_all(&next).unwrap();

            if host.write_output(&read).is_err() {
                // Either:
                // - outbox is full (stop flushing)
                // - outbox message too large
                // We return at that point too. This condition is checked by 'queue_message'
                // and if it fails here, then a user's withdrawal could have been 'accepted'
                // but breaks here. We choose to block the outbox at this point, and require
                // a kernel upgrade to intervene/fix the issue. This could be relaxed in the
                // future.
                return num_flushed;
            };

            num_flushed += 1;
            len = len.saturating_sub(1);
            start = start.wrapping_add(1);

            // We just read from this path
            host.store_delete(&next).unwrap();
            self.save_meta(host, start, len);
        }
    }

    // if we had a 'path::append_hex_into' method, we could have an alloc free
    // version of this.
    // 'concat_into' would be useful also.
    fn message_path(&self, idx: u32) -> OwnedPath {
        let mut path = [b'/'; 1 + 2 * core::mem::size_of::<u32>()];
        let idx = idx.to_le_bytes();

        let _ = hex::encode_to_slice(idx.as_slice(), &mut path[1..]);

        let path = RefPath::assert_from(path.as_slice());

        // We know this path fits into `PATH_MAX_LEN` from `Self::new`
        path::concat(self.root, &path).unwrap()
    }

    #[inline(always)]
    fn read_meta(&self, host: &impl Runtime) -> (u32, u32) {
        const BUFF_SIZE: usize = 2 * core::mem::size_of::<u32>();
        let mut buffer = [0; BUFF_SIZE];

        let result = if let Some(meta) = &self.meta {
            host.store_read_slice(meta, 0, buffer.as_mut_slice())
        } else {
            host.store_read_slice(&OUTBOX_QUEUE_META, 0, buffer.as_mut_slice())
        };

        let Ok(BUFF_SIZE) = result else { return (0, 0) };

        let start = u32::from_le_bytes(buffer[..BUFF_SIZE / 2].try_into().unwrap());
        let len = u32::from_le_bytes(buffer[BUFF_SIZE / 2..].try_into().unwrap());

        (start, len)
    }

    #[inline(always)]
    fn save_meta(&self, host: &mut impl Runtime, start: u32, len: u32) {
        if len == 0 {
            // We only call this when we've just emptied the queue
            let _ = host.store_delete(self.root);
            return;
        }

        let [s0, s1, s2, s3] = start.to_le_bytes();
        let [l0, l1, l2, l3] = len.to_le_bytes();

        let buffer = [s0, s1, s2, s3, l0, l1, l2, l3];

        if let Some(meta) = &self.meta {
            host.store_write(meta, buffer.as_slice(), 0).unwrap();
        } else {
            host.store_write(&OUTBOX_QUEUE_META, buffer.as_slice(), 0)
                .unwrap();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::michelson::MichelsonBytes;
    use crate::testing::prelude::*;
    use crate::types::Entrypoint;
    use tezos_data_encoding::nom::NomReader;
    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;

    #[test]
    fn flushing_empty_queue_no_op() {
        let mut host = MockHost::default();
        let level = host.level();

        let flushed = OUTBOX_QUEUE.flush_queue(&mut host);

        assert_eq!(0, flushed);
        assert!(host.outbox_at(level).is_empty());
        assert!(matches!(host.store_has(&OUTBOX_QUEUE_ROOT), Ok(None)));
    }

    #[test]
    fn push_then_flush() {
        let mut host = MockHost::default();

        for i in 0..150 {
            let msg = make_outbox_message(vec![i as u8]);
            assert!(
                matches!(OUTBOX_QUEUE.queue_message(&mut host, msg), Ok(l) if l == i + 1)
            );
        }

        assert_eq!(100, OUTBOX_QUEUE.flush_queue(&mut host));
        let level = host.run_level(|_| {});
        assert_eq!(100, host.outbox_at(level).len());

        assert_eq!(50, OUTBOX_QUEUE.flush_queue(&mut host));
        let level = host.run_level(|_| {});
        assert_eq!(50, host.outbox_at(level).len());
        println!("{:?}", host.store_has(&OUTBOX_QUEUE_ROOT));
        assert!(matches!(host.store_has(&OUTBOX_QUEUE_ROOT), Ok(None)));
    }

    #[test]
    fn push_then_flush_large() {
        let mut host = MockHost::default();
        let msg = make_outbox_message(vec![5; MAX_FILE_CHUNK_SIZE + 20]);

        OUTBOX_QUEUE.queue_message(&mut host, msg).unwrap();
        OUTBOX_QUEUE.flush_queue(&mut host);

        let level = host.run_level(|_| {});
        let outbox = host.outbox_at(level);

        assert!(outbox.len() == 1);

        let (_, message) = OutboxMessage::nom_read(&outbox[0]).unwrap();

        let expected = make_outbox_message(vec![5; MAX_FILE_CHUNK_SIZE + 20]).into();
        assert_eq!(message, expected);
    }

    #[test]
    fn push_overflow_then_flush() {
        let mut host = MockHost::default();
        let root = RefPath::assert_from(b"/hello");
        let queue = OutboxQueue::new(&root, 15).unwrap();

        let start = u32::MAX - 12;

        // Start close to the overflow point of u32::MAX
        let message_path = queue.message_path(start);
        host.store_write_all(&message_path, b"CAFEBABE").unwrap();
        queue.save_meta(&mut host, start, 1);

        for i in 1..15 {
            let msg = make_outbox_message(vec![i as u8]);
            assert!(matches!(queue.queue_message(&mut host, msg), Ok(l) if l == i + 1));
        }
        // Can't push more messages to the queue than the configured max
        assert!(queue
            .queue_message(&mut host, make_outbox_message(vec![23]))
            .is_err());

        assert_eq!(15, queue.flush_queue(&mut host));
        let level = host.run_level(|_| {});
        assert_eq!(15, host.outbox_at(level).len());

        assert!(matches!(host.store_has(&root), Ok(None)));
    }

    fn make_outbox_message(msg: Vec<u8>) -> OutboxMessageTransaction<MichelsonBytes> {
        OutboxMessageTransaction {
            parameters: msg.into(),
            destination: Contract::from_b58check("KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc")
                .unwrap(),
            entrypoint: Entrypoint::default(),
        }
    }
}
