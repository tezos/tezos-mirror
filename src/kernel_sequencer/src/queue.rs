// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path, RefPath},
    runtime::{Runtime, RuntimeError},
    Error,
};

use crate::storage::sequencer_prefix;

/// A simple FIFO (First-In-First-Out) queue implementation.
///
/// The queue can support any type of element.
/// The element has to implement NomReader and BinWriter
/// The queue has a maximum size of 2^32 elements.
/// When the queue is full, it won't accept new elements.
pub struct Queue {
    prefix: OwnedPath,
    pointer_path: OwnedPath,
    pointer: Option<Pointer>,
}

/// Pointer that indicates where is the head and the tail of the queue.
///
/// `head` represents the index of the element to be popped out from the queue.
/// `next_add_slot` represents the exclusive tail,
/// or the index of the next element to be added to the queue.
#[derive(NomReader, BinWriter, Copy, Clone)]
struct Pointer {
    head: u32,
    next_add_slot: u32,
}

/// Compute the path where the pointer to the `head` and `next_add_slot` is stored.
fn pointer_path(prefix: &impl Path) -> Result<OwnedPath, RuntimeError> {
    let pointer_path = RefPath::assert_from(b"/pointer");
    concat(prefix, &pointer_path).map_err(|_| RuntimeError::HostErr(Error::StoreInvalidKey))
}

/// Load the pointer from the durable storage.
fn load_pointer<H: Runtime>(
    pointer_path: &OwnedPath,
    host: &H,
) -> Result<Option<Pointer>, RuntimeError> {
    match host.store_has(pointer_path)? {
        None => Ok(None),
        Some(_) => {
            // 8 bytes because the pointer is 2 u32
            let data = host.store_read(pointer_path, 0, 8)?;
            let (_, pointer) = Pointer::nom_read(&data).map_err(|_| RuntimeError::DecodingError)?;
            Ok(Some(pointer))
        }
    }
}

impl Queue {
    /// Loads a queue from the given path.
    ///
    /// If the given path does not point to a queue
    /// a new empty queue is created.
    pub fn new<H: Runtime, P: Path>(host: &H, prefix: &P) -> Result<Self, RuntimeError> {
        // Prefix the path of the queue by "__sequencer/"
        let prefix = sequencer_prefix(prefix)?;
        // Get the pointer path to avoid compute it too many times.
        let pointer_path = pointer_path(&prefix)?;
        // Load the pointer or use the default one
        let pointer = load_pointer(&pointer_path, host)?;
        Ok(Queue {
            prefix,
            pointer_path,
            pointer,
        })
    }

    /// Compute the path of one element of the queue.
    fn element_path(&self, id: u32) -> Result<OwnedPath, RuntimeError> {
        let path = OwnedPath::try_from(format!("/elements/{}", id))
            .map_err(|_| RuntimeError::HostErr(Error::StoreInvalidKey))?;

        concat(&self.prefix, &path).map_err(|_| RuntimeError::HostErr(Error::StoreInvalidKey))
    }

    /// Save the pointer of the queue in the durable storage.
    fn save_pointer<H: Runtime>(&self, host: &mut H) -> Result<(), RuntimeError> {
        match self.pointer {
            None => {
                // If the pointer is not present then it's removed from the durable storage.
                host.store_delete(&self.pointer_path)
            }
            Some(pointer) => {
                // Serialize the pointer
                let mut output = Vec::new();
                pointer
                    .bin_write(&mut output)
                    .map_err(|_| RuntimeError::HostErr(Error::GenericInvalidAccess))?;
                // Save the pointer to the durable storage
                host.store_write(&self.pointer_path, &output, 0)?;
                Ok(())
            }
        }
    }

    /// Returns the next pointer with the `next_add_slot` incremented.
    fn increment_next_add_slot_pointer(&self) -> Pointer {
        match self.pointer {
            None => Pointer {
                head: 0,
                next_add_slot: 1,
            },
            Some(Pointer {
                head,
                next_add_slot,
            }) => Pointer {
                head,
                next_add_slot: {
                    let (incremented_next_add_slot, _) = next_add_slot.overflowing_add(1);
                    incremented_next_add_slot
                },
            },
        }
    }

    /// Returns the next pointer with an incremented head.
    ///
    /// Returns None if the pointer indicated an empty queue.
    fn increment_head_pointer(&self) -> Option<Pointer> {
        match self.pointer {
            None => None,
            Some(Pointer {
                head,
                next_add_slot,
            }) => {
                let (next_head, _) = head.overflowing_add(1);
                // Check if the pointer indicates an empty list
                if next_head == next_add_slot {
                    return None;
                }
                Some(Pointer {
                    head: next_head,
                    next_add_slot,
                })
            }
        }
    }

    /// Returns the pointer to the head of the queue.
    fn head_pointer(&self) -> u32 {
        match self.pointer {
            Some(Pointer {
                head,
                next_add_slot: _,
            }) => head,
            None => 0,
        }
    }

    /// Returns the pointer to the `next_add_slot` of the queue.
    fn next_add_slot_pointer(&self) -> u32 {
        match self.pointer {
            Some(Pointer {
                head: _,
                next_add_slot,
            }) => next_add_slot,
            None => 0,
        }
    }

    /// Check if the queue is empty.
    fn is_empty(&self) -> bool {
        self.pointer.is_none()
    }

    /// Check if the queue is full.
    fn is_full(&self) -> bool {
        match self.pointer {
            None => false, // If the pointer is not present, the queue is empty.
            Some(Pointer {
                head,
                next_add_slot,
            }) => {
                let (next_add_slot, _) = next_add_slot.overflowing_add(1);
                next_add_slot == head // The queue is full when the next element slot is equal to the current head.
            }
        }
    }

    /// Add an element to the back of the queue
    ///
    /// Returns an error when the queue is full.
    pub fn add<H: Runtime, E>(&mut self, host: &mut H, element: &E) -> Result<(), RuntimeError>
    where
        E: BinWriter,
    {
        // Check if the queue is full
        if self.is_full() {
            return Err(RuntimeError::HostErr(Error::GenericInvalidAccess));
        }

        // Compute the path of the element
        let id = self.next_add_slot_pointer();
        let path = self.element_path(id)?;

        // Get the bytes of the element
        let mut bytes = Vec::new();
        element
            .bin_write(&mut bytes)
            .map_err(|_| RuntimeError::DecodingError)?;

        // write the bytes to the store
        host.store_write(&path, bytes.as_ref(), 0)?;

        // update the queue pointer
        self.pointer = Some(self.increment_next_add_slot_pointer());

        // save the pointer to the durable storage
        self.save_pointer(host)?;

        Ok(())
    }

    /// Remove and returns the first element of the queue.
    ///
    /// If the queue is empty None is returned
    #[allow(dead_code)]
    pub fn pop<H: Runtime, E>(&mut self, host: &mut H) -> Result<Option<E>, RuntimeError>
    where
        E: NomReader,
    {
        // Check if the queue is empty
        if self.is_empty() {
            return Ok(None);
        }

        // Get the path of the element
        let id = self.head_pointer();
        let path = self.element_path(id)?;

        // Check if the element is present
        let Some(_) = host.store_has(&path)? else {return Ok(None)};

        // Deserialize the element
        let size = host.store_value_size(&path)?;
        let bytes = host.store_read(&path, 0, size)?;
        let (_, element) = E::nom_read(&bytes).map_err(|_| RuntimeError::DecodingError)?;
        host.store_delete(&path)?;

        // Update the pointer
        self.pointer = self.increment_head_pointer();
        self.save_pointer(host)?;

        Ok(Some(element))
    }
}

#[cfg(test)]
mod tests2 {
    use crate::storage::DELAYED_INBOX_PATH;

    use super::{Pointer, Queue};
    use tezos_data_encoding_derive::{BinWriter, NomReader};
    use tezos_smart_rollup_host::{
        path::RefPath,
        runtime::{Runtime, RuntimeError},
    };
    use tezos_smart_rollup_mock::MockHost;

    #[derive(BinWriter, NomReader, Eq, PartialEq, Debug)]
    struct Element {
        inner: u32,
    }

    impl Element {
        fn new(inner: u32) -> Self {
            Self { inner }
        }
    }

    const QUEUE_PATH: RefPath = RefPath::assert_from(b"/queue");

    /// Initialize a Queue with a custom pointer
    fn queue_with_pointer<H: Runtime>(host: &H, pointer: Pointer) -> Queue {
        let queue = Queue::new(host, &QUEUE_PATH).unwrap();
        Queue {
            prefix: queue.prefix,
            pointer_path: queue.pointer_path,
            pointer: Some(pointer),
        }
    }

    #[test]
    fn test_empty() {
        let host = MockHost::default();
        let queue = Queue::new(&host, &QUEUE_PATH).unwrap();
        assert!(queue.is_empty())
    }

    #[test]
    fn test_is_not_full() {
        let host = MockHost::default();
        let queue = Queue::new(&host, &QUEUE_PATH).unwrap();
        assert!(!queue.is_full())
    }

    #[test]
    fn test_is_full() {
        let mut host = MockHost::default();
        let mut queue = queue_with_pointer(
            &host,
            Pointer {
                head: 0,
                next_add_slot: u32::MAX - 3,
            },
        );

        let _ = queue.add(&mut host, &Element::new(0));
        let _ = queue.add(&mut host, &Element::new(0));
        assert!(!queue.is_full());
        let _ = queue.add(&mut host, &Element::new(0));
        assert!(queue.is_full());
    }

    #[test]
    fn test_add_element() {
        let mut host = MockHost::default();
        let mut queue = Queue::new(&host, &DELAYED_INBOX_PATH).unwrap();

        queue
            .add(&mut host, &Element::new(0))
            .expect("Element should be added");
        assert!(!queue.is_empty());
    }

    #[test]
    fn test_cannot_pop_elements_empty_queue() {
        let mut host = MockHost::default();
        let mut queue = Queue::new(&host, &DELAYED_INBOX_PATH).unwrap();
        let element: Result<Option<Element>, RuntimeError> = queue.pop(&mut host);
        assert!(queue.is_empty());
        assert!(matches!(element, Ok(None)))
    }

    #[test]
    fn test_empty_queue_of_one_element() {
        let mut host = MockHost::default();
        let mut queue = Queue::new(&host, &DELAYED_INBOX_PATH).unwrap();
        queue
            .add(&mut host, &Element::new(0))
            .expect("adding element should work");
        let _: Option<Element> = queue
            .pop(&mut host)
            .expect("should be able to pop out an element");
        assert!(queue.is_empty());
    }

    #[test]
    fn test_cycling_queue() {
        let mut host = MockHost::default();
        // The queue starts at u32::MAX - 2 to simulate an overflow of the pointer
        let mut queue = queue_with_pointer(
            &host,
            Pointer {
                head: u32::MAX - 2,
                next_add_slot: u32::MAX - 2,
            },
        );
        // Adding 4 elements
        queue
            .add(&mut host, &Element::new(0))
            .expect("adding element should work");
        queue
            .add(&mut host, &Element::new(1))
            .expect("adding element should work");
        queue
            .add(&mut host, &Element::new(2))
            .expect("adding element should work");
        queue
            .add(&mut host, &Element::new(3))
            .expect("adding element should work");

        // Pop out 2 elements
        let e1: Element = queue
            .pop(&mut host)
            .unwrap()
            .expect("element should be present");
        let e2: Element = queue
            .pop(&mut host)
            .unwrap()
            .expect("element should be present");
        let e3: Element = queue
            .pop(&mut host)
            .unwrap()
            .expect("element should be present");
        let e4: Element = queue
            .pop(&mut host)
            .unwrap()
            .expect("element should be present");

        assert_eq!(e1, Element::new(0));
        assert_eq!(e2, Element::new(1));
        assert_eq!(e3, Element::new(2));
        assert_eq!(e4, Element::new(3));
        assert!(queue.is_empty());
    }

    #[test]
    fn test_queue_correctly_saved() {
        let mut host = MockHost::default();
        let mut queue1 = Queue::new(&host, &QUEUE_PATH).expect("create first queue");

        queue1
            .add(&mut host, &Element::new(0))
            .expect("Should work");

        queue1
            .add(&mut host, &Element::new(1))
            .expect("Should work");

        let mut queue = Queue::new(&host, &QUEUE_PATH).expect("create second queue");

        let e1: Element = queue
            .pop(&mut host)
            .unwrap()
            .expect("element should be present");
        let e2: Element = queue
            .pop(&mut host)
            .unwrap()
            .expect("element should be present");

        assert_eq!(e1, Element::new(0));
        assert_eq!(e2, Element::new(1));
    }
}
