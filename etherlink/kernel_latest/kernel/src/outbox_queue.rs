// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! The withdrawal outbox queue.
//!
//! The queue keeps outbox messages in order until a flush writes them to the
//! outbox. Its layout is the one of the SDK
//! [`OutboxQueue`](tezos_smart_rollup::outbox::OutboxQueue): under the root
//! key `<root>` of the queue, the message at index `i` is at
//! `<root>/<hex of the little-endian bytes of i>`, and `<root>/meta` holds the
//! index of the oldest message then the number of messages, as little-endian
//! `u32`s. A queue without metadata, or with a metadata value shorter than 8
//! bytes, is empty. A longer metadata value counts by its first 8 bytes.

use tezos_data_encoding::enc::BinWriter;
use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
use tezos_smart_rollup::outbox::{AtomicBatch, OutboxMessageFull};
use tezos_smart_rollup_core::MAX_OUTPUT_SIZE;
use tezos_smart_rollup_host::wasm::WasmHost;
use tezos_smart_rollup_keyspace::{
    Key, KeySpace, KeySpaceError, KeySpaceLoader, ReadKind,
};
use tezosx_types::StorageReadErrorKind;
use thiserror::Error;

/// Root key of the queue in the EVM world-state keyspace.
const ROOT_SEGMENT: Key = Key::from_static(b"/__outbox_queue");

/// Key of the metadata of the queue in the EVM world-state keyspace.
const META_KEY: Key = Key::from_static(b"/__outbox_queue/meta");

/// Size in bytes of the metadata value of the queue.
const META_SIZE: usize = 2 * core::mem::size_of::<u32>();

/// Failure of an outbox queue operation.
#[derive(Debug, Error)]
pub enum OutboxQueueError {
    /// The queue holds `u32::MAX` messages and takes no more.
    #[error("the outbox queue is full")]
    Full,
    /// The encoding of a message, of the given number of bytes, exceeds
    /// [`MAX_OUTPUT_SIZE`].
    #[error("an outbox message of {0} bytes exceeds the {max} bytes of an output", max = MAX_OUTPUT_SIZE)]
    TooLarge(usize),
    /// A keyspace operation failed, or a message does not encode.
    #[error(transparent)]
    KeySpace(#[from] KeySpaceError<StorageReadErrorKind>),
}

impl From<KeySpaceError> for OutboxQueueError {
    fn from(err: KeySpaceError) -> Self {
        Self::KeySpace(err.widen())
    }
}

/// Appends `message` to the withdrawal outbox queue, in the
/// [EVM world-state keyspace](RuntimeKeyspaces::evm_world_state) of `rk`, and
/// returns the number of messages the queue then holds.
///
/// # Errors
///
/// - [`OutboxQueueError::Full`] when the queue holds `u32::MAX` messages.
/// - [`OutboxQueueError::TooLarge`] when the encoding of `message` exceeds
///   [`MAX_OUTPUT_SIZE`].
/// - [`OutboxQueueError::KeySpace`] when `message` does not encode, or a
///   write fails.
pub fn queue_outbox_message<Host, Batch>(
    rk: &mut RuntimeKeyspaces<'_, Host>,
    message: impl Into<OutboxMessageFull<Batch>>,
) -> Result<u32, OutboxQueueError>
where
    Host: KeySpaceLoader,
    Batch: AtomicBatch,
{
    let ks = rk.evm_world_state_mut();
    let (start, len) = meta(ks);
    let new_len = len.checked_add(1).ok_or(OutboxQueueError::Full)?;
    let key = message_key(start.wrapping_add(len));
    let mut buffer = Vec::with_capacity(MAX_OUTPUT_SIZE);
    message
        .into()
        .bin_write(&mut buffer)
        .map_err(|err| -> KeySpaceError {
            KeySpaceError::write(ks.name(), &key, err.into())
        })?;
    if buffer.len() > MAX_OUTPUT_SIZE {
        return Err(OutboxQueueError::TooLarge(buffer.len()));
    }
    ks.set(&key, buffer)?;
    set_meta(ks, start, new_len)?;
    Ok(new_len)
}

/// Writes the messages of the withdrawal outbox queue, in the
/// [EVM world-state keyspace](RuntimeKeyspaces::evm_world_state) of `rk`, to
/// the outbox of the host of `rk`, oldest first. Removes them from the queue,
/// and returns how many it wrote.
///
/// Stops at the first message that the outbox refuses, which stays at the
/// head of the queue. Two flushes from the same durable state write the same
/// messages twice.
///
/// # Errors
///
/// [`OutboxQueueError::KeySpace`] when a message that the metadata counts is
/// absent. The messages before it are then in the outbox and out of the
/// queue.
pub fn flush_outbox_queue<Host>(
    rk: &mut RuntimeKeyspaces<'_, Host>,
) -> Result<u32, OutboxQueueError>
where
    Host: KeySpaceLoader + WasmHost,
{
    let (mut start, mut len) = meta(rk.evm_world_state());
    let mut flushed = 0;
    while len > 0 {
        let key = message_key(start);
        let message = message(rk.evm_world_state(), &key)?;
        if rk.host_mut().write_output(&message).is_err() {
            // The outbox is full for this level, or the message is larger than
            // an output. A full outbox takes the message at the next level. A
            // message too large blocks the queue until a kernel upgrade, but
            // `queue_outbox_message` never queues one.
            break;
        }
        let ks = rk.evm_world_state_mut();
        ks.delete(&key);
        start = start.wrapping_add(1);
        len -= 1;
        flushed += 1;
        // Cannot leave the deleted message counted: `set` refuses only a
        // value too large for the keyspace, and the metadata is 8 bytes.
        set_meta(ks, start, len)?;
    }
    Ok(flushed)
}

/// Returns the message at `key`, which the metadata counts, or a `NotFound`
/// read error when the keyspace has no value there.
fn message(
    ks: &impl KeySpace,
    key: &Key,
) -> Result<Vec<u8>, KeySpaceError<StorageReadErrorKind>> {
    ks.get(key).ok_or_else(|| {
        KeySpaceError::read(
            ks.name(),
            key,
            ReadKind::Ext(StorageReadErrorKind::NotFound),
        )
    })
}

fn message_key(index: u32) -> Key {
    // The root key, `/` and 8 hex digits always form a valid key.
    ROOT_SEGMENT
        .concat(format!("/{}", hex::encode(index.to_le_bytes())))
        .expect("the key of a queued message is valid")
}

/// Returns the index of the oldest message and the number of messages, from
/// the first [`META_SIZE`] bytes of the metadata value. A queue without
/// metadata, or with a shorter value, is empty.
fn meta(ks: &impl KeySpace) -> (u32, u32) {
    let Some(meta) = ks.get_prefix_exact::<META_SIZE>(&META_KEY) else {
        return (0, 0);
    };
    let [s0, s1, s2, s3, l0, l1, l2, l3] = meta;
    (
        u32::from_le_bytes([s0, s1, s2, s3]),
        u32::from_le_bytes([l0, l1, l2, l3]),
    )
}

/// Records the index of the oldest message and the number of messages. An
/// empty queue has no metadata value.
fn set_meta(ks: &mut impl KeySpace, start: u32, len: u32) -> Result<(), KeySpaceError> {
    if len == 0 {
        ks.delete(&META_KEY);
        return Ok(());
    }
    let mut meta = [0; META_SIZE];
    meta[..4].copy_from_slice(&start.to_le_bytes());
    meta[4..].copy_from_slice(&len.to_le_bytes());
    ks.set(&META_KEY, meta)
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use std::collections::BTreeSet;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup::michelson::MichelsonBytes;
    use tezos_smart_rollup::outbox::{
        OutboxMessage, OutboxMessageTransaction, OutboxQueue as SdkOutboxQueue,
    };
    use tezos_smart_rollup::types::{Contract, Entrypoint};
    use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
    use tezos_smart_rollup_host::storage::StorageV1;

    /// Durable path of the queue.
    const DURABLE_ROOT: RefPath =
        RefPath::assert_from(b"/evm/world_state/__outbox_queue");

    pub(crate) fn message(payload: Vec<u8>) -> OutboxMessageTransaction<MichelsonBytes> {
        OutboxMessageTransaction {
            parameters: payload.into(),
            destination: Contract::from_b58check("KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc")
                .unwrap(),
            entrypoint: Entrypoint::default(),
        }
    }

    /// The bytes that the outbox receives for `message(payload)`.
    fn encoded(payload: Vec<u8>) -> Vec<u8> {
        let mut bytes = Vec::new();
        OutboxMessage::from(message(payload))
            .bin_write(&mut bytes)
            .unwrap();
        bytes
    }

    /// The bytes that the outbox receives for `message(vec![payload])`.
    pub(crate) fn output(payload: u8) -> Vec<u8> {
        encoded(vec![payload])
    }

    /// Returns the durable path of the queue followed by `suffix`.
    fn durable_path(suffix: &str) -> OwnedPath {
        OwnedPath::try_from(format!("/evm/world_state/__outbox_queue/{suffix}")).unwrap()
    }

    /// Makes sure that the queue writes its messages and its metadata at
    /// `/evm/world_state/__outbox_queue`.
    #[test]
    fn queue_keeps_its_durable_path() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        assert_eq!(queue_outbox_message(&mut rk, message(vec![7])).unwrap(), 1);

        assert_eq!(
            rk.host().store_read_all(&durable_path("00000000")).unwrap(),
            output(7)
        );
        assert_eq!(
            rk.host().store_read_all(&durable_path("meta")).unwrap(),
            [0, 0, 0, 0, 1, 0, 0, 0]
        );
    }

    /// Makes sure that a flush writes the oldest messages first, stops at the
    /// 100 messages that the outbox takes per level, and keeps the others
    /// for the next level.
    #[test]
    fn flush_stops_at_the_outbox_limit() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        for payload in 0..150 {
            assert_eq!(
                queue_outbox_message(&mut rk, message(vec![payload])).unwrap(),
                u32::from(payload) + 1
            );
        }

        let level = rk.host_mut().unprefixed_mut().unwrap().host.level();
        assert_eq!(flush_outbox_queue(&mut rk).unwrap(), 100);
        assert_eq!(
            rk.host_mut()
                .unprefixed_mut()
                .unwrap()
                .host
                .outbox_at(level),
            (0..100).map(output).collect::<Vec<_>>()
        );

        rk.host_mut()
            .unprefixed_mut()
            .unwrap()
            .host
            .run_level(|_| ());
        let level = rk.host_mut().unprefixed_mut().unwrap().host.level();
        assert_eq!(flush_outbox_queue(&mut rk).unwrap(), 50);
        assert_eq!(
            rk.host_mut()
                .unprefixed_mut()
                .unwrap()
                .host
                .outbox_at(level),
            (100..150).map(output).collect::<Vec<_>>()
        );
        assert!(!rk.evm_world_state().contains(&META_KEY));
    }

    /// Returns the durable values of the queue: the metadata, then the
    /// message at each of `indices`. `None` stands for an absent value.
    fn durable_queue(
        host: &impl StorageV1,
        indices: &BTreeSet<u32>,
    ) -> Vec<Option<Vec<u8>>> {
        let read = |suffix: String| host.store_read_all(&durable_path(&suffix)).ok();
        let mut values = vec![read("meta".to_string())];
        values.extend(
            indices
                .iter()
                .map(|index| read(hex::encode(index.to_le_bytes()))),
        );
        values
    }

    /// Returns the index of the next queued message, from the durable
    /// metadata of the queue.
    fn next_index(host: &impl StorageV1) -> u32 {
        let Ok(meta) = host.store_read_all(&durable_path("meta")) else {
            return 0;
        };
        let start = u32::from_le_bytes(meta[..4].try_into().unwrap());
        let len = u32::from_le_bytes(meta[4..8].try_into().unwrap());
        start.wrapping_add(len)
    }

    /// Makes sure that the queue reads and extends a queue that the SDK
    /// queue wrote at the same durable path.
    #[test]
    fn extends_the_queue_of_the_sdk() {
        let mut host = MockKernelHost::default();
        let sdk_queue = SdkOutboxQueue::new(&DURABLE_ROOT, u32::MAX).unwrap();
        for payload in 0..2 {
            sdk_queue
                .queue_message(&mut host, message(vec![payload]))
                .unwrap();
        }

        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        assert_eq!(queue_outbox_message(&mut rk, message(vec![2])).unwrap(), 3);
        let level = rk.host_mut().unprefixed_mut().unwrap().host.level();
        assert_eq!(flush_outbox_queue(&mut rk).unwrap(), 3);
        assert_eq!(
            rk.host_mut()
                .unprefixed_mut()
                .unwrap()
                .host
                .outbox_at(level),
            vec![output(0), output(1), output(2)]
        );
    }

    /// Makes sure that the SDK queue reads and extends a queue that the queue
    /// wrote at the same durable path.
    #[test]
    fn the_sdk_extends_the_queue() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        for payload in 0..2 {
            queue_outbox_message(&mut rk, message(vec![payload])).unwrap();
        }
        drop(rk);

        let sdk_queue = SdkOutboxQueue::new(&DURABLE_ROOT, u32::MAX).unwrap();
        assert_eq!(
            sdk_queue
                .queue_message(&mut host, message(vec![2]))
                .unwrap(),
            3
        );
        let level = host.host.level();
        assert_eq!(sdk_queue.flush_queue(&mut host), 3);
        assert_eq!(
            host.host.outbox_at(level),
            vec![output(0), output(1), output(2)]
        );
    }

    /// One step of `behaves_as_the_sdk`.
    #[derive(Clone, Debug)]
    enum Step {
        /// Queues one message for each payload.
        Queue(Vec<Vec<u8>>),
        /// Flushes the queue.
        Flush,
        /// Ends the level. The outbox of the next level is empty.
        NextLevel,
    }

    /// Returns any step. A step queues up to 149 messages, so that a flush
    /// can meet the limit of 100 outputs per level.
    fn step() -> impl Strategy<Value = Step> {
        prop_oneof![
            vec(vec(any::<u8>(), 0..4), 0..150).prop_map(Step::Queue),
            Just(Step::Flush),
            Just(Step::NextLevel),
        ]
    }

    proptest! {
        // Each case queues up to a few thousand messages on two hosts.
        #![proptest_config(ProptestConfig::with_cases(64))]

        /// Makes sure that, from a queue of `len` messages at index `start`,
        /// each step gives the same result through the SDK queue and through
        /// the queue, on two hosts, with the same outbox and the same durable
        /// values. `start` is often close to `u32::MAX`, so that the indices
        /// wrap.
        #[test]
        fn behaves_as_the_sdk(
            start in prop_oneof![any::<u32>(), u32::MAX - 300..=u32::MAX],
            len in 0u32..150,
            steps in vec(step(), 0..24),
        ) {
            let mut sdk_host = MockKernelHost::default();
            let mut host = MockKernelHost::default();
            let mut indices: BTreeSet<u32> =
                (0..len).map(|offset| start.wrapping_add(offset)).collect();
            if len > 0 {
                for host in [&mut sdk_host, &mut host] {
                    for index in &indices {
                        host.store_write_all(
                            &durable_path(&hex::encode(index.to_le_bytes())),
                            &output(0),
                        )
                        .unwrap();
                    }
                    let mut meta = start.to_le_bytes().to_vec();
                    meta.extend(len.to_le_bytes());
                    host.store_write_all(&durable_path("meta"), &meta).unwrap();
                }
            }
            let sdk_queue = SdkOutboxQueue::new(&DURABLE_ROOT, u32::MAX).unwrap();
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

            for step in steps {
                match step {
                    Step::Queue(payloads) => {
                        for payload in payloads {
                            indices.insert(next_index(&sdk_host));
                            let sdk_len = sdk_queue
                                .queue_message(&mut sdk_host, message(payload.clone()))
                                .unwrap();
                            let len =
                                queue_outbox_message(&mut rk, message(payload)).unwrap();
                            prop_assert_eq!(sdk_len, len as usize);
                        }
                    }
                    Step::Flush => {
                        let level = sdk_host.host.level();
                        prop_assert_eq!(
                            level,
                            rk.host_mut().unprefixed_mut().unwrap().host.level()
                        );
                        let flushed = flush_outbox_queue(&mut rk).unwrap();
                        prop_assert_eq!(sdk_queue.flush_queue(&mut sdk_host), flushed as usize);
                        prop_assert_eq!(
                            sdk_host.host.outbox_at(level),
                            rk.host_mut()
                                .unprefixed_mut()
                                .unwrap()
                                .host
                                .outbox_at(level)
                        );
                    }
                    Step::NextLevel => {
                        sdk_host.host.run_level(|_| ());
                        rk.host_mut()
                            .unprefixed_mut()
                            .unwrap()
                            .host
                            .run_level(|_| ());
                    }
                }
                prop_assert_eq!(
                    durable_queue(&sdk_host, &indices),
                    durable_queue(rk.host(), &indices)
                );
            }
        }
    }

    /// Makes sure that the indices wrap around `u32::MAX` and keep the order.
    #[test]
    fn queue_outbox_message_wraps_around_the_last_index() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let ks = rk.evm_world_state_mut();
        ks.set(&message_key(u32::MAX), output(0)).unwrap();
        set_meta(ks, u32::MAX, 1).unwrap();

        for payload in 1..3 {
            queue_outbox_message(&mut rk, message(vec![payload])).unwrap();
        }
        assert!(rk.evm_world_state().contains(&message_key(1)));

        let level = rk.host_mut().unprefixed_mut().unwrap().host.level();
        assert_eq!(flush_outbox_queue(&mut rk).unwrap(), 3);
        assert_eq!(
            rk.host_mut()
                .unprefixed_mut()
                .unwrap()
                .host
                .outbox_at(level),
            vec![output(0), output(1), output(2)]
        );
    }

    /// Makes sure that a message whose encoding fills an output is queued,
    /// and that a message one byte larger is rejected and leaves the queue as
    /// it was.
    #[test]
    fn queue_outbox_message_rejects_a_message_larger_than_an_output() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let largest = MAX_OUTPUT_SIZE - encoded(vec![]).len();

        assert_eq!(
            queue_outbox_message(&mut rk, message(vec![0; largest])).unwrap(),
            1
        );
        assert_eq!(
            rk.evm_world_state().get(&message_key(0)),
            Some(encoded(vec![0; largest]))
        );

        let pushed = queue_outbox_message(&mut rk, message(vec![0; largest + 1]));

        assert!(matches!(
            pushed,
            Err(OutboxQueueError::TooLarge(size)) if size == MAX_OUTPUT_SIZE + 1
        ));
        assert_eq!(meta(rk.evm_world_state()), (0, 1));
        assert!(!rk.evm_world_state().contains(&message_key(1)));
    }

    /// Makes sure that a queue of `u32::MAX` messages takes no more.
    #[test]
    fn queue_outbox_message_rejects_a_full_queue() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        set_meta(rk.evm_world_state_mut(), 0, u32::MAX).unwrap();

        let pushed = queue_outbox_message(&mut rk, message(vec![0]));

        assert!(matches!(pushed, Err(OutboxQueueError::Full)));
        assert_eq!(meta(rk.evm_world_state()), (0, u32::MAX));
    }

    /// Makes sure that a flush fails at a message that the metadata counts
    /// but the keyspace does not hold, after it writes the messages before it
    /// to the outbox and removes them from the queue.
    #[test]
    fn flush_outbox_queue_rejects_a_missing_message() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let ks = rk.evm_world_state_mut();
        ks.set(&message_key(0), output(0)).unwrap();
        set_meta(ks, 0, 2).unwrap();

        let level = rk.host_mut().unprefixed_mut().unwrap().host.level();
        let flushed = flush_outbox_queue(&mut rk);

        let missing = KeySpaceError::read(
            rk.evm_world_state().name(),
            &message_key(1),
            ReadKind::Ext(StorageReadErrorKind::NotFound),
        );
        assert!(
            matches!(flushed, Err(OutboxQueueError::KeySpace(err)) if err == missing)
        );
        assert_eq!(
            rk.host_mut()
                .unprefixed_mut()
                .unwrap()
                .host
                .outbox_at(level),
            vec![output(0)]
        );
        assert!(!rk.evm_world_state().contains(&message_key(0)));
        assert_eq!(meta(rk.evm_world_state()), (1, 1));
    }
}
