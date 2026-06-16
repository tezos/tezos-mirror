// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>

use anyhow::{Context, Result};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpIterator, RlpStream};
use std::marker::PhantomData;
use tezos_ethereum::rlp_helpers::{append_option, decode_field, decode_option, next};
use tezos_smart_rollup_keyspace::{Key, KeySpace};
use tezos_storage::keyspace::{read_optional_rlp, read_rlp, store_rlp};

/// Doubly linked list backed by a [`KeySpace`].
///
/// The list is generic over element and index.
/// The element has to implement Decodable and Encodable
/// The Id has to implement AsRef<[u8]>
///
/// The list is lazy. Not all the list is loaded at initialization.
/// Elements are saved/read at the appropriate moment.
///
/// All entries are addressed by keys derived from the element id relative to
/// the `prefix` key inside the keyspace, mirroring the historical durable
/// layout: for a list rooted at `prefix` the metadata lives at
/// `{prefix}/meta`, a pointer at `{prefix}/{hex(id)}/pointer` and its data at
/// `{prefix}/{hex(id)}/data`. A keyspace loaded under name `/base` with
/// `prefix = /delayed-inbox` therefore resolves to exactly the durable paths
/// the previous [`StorageV1`]-based list used.
///
/// [`StorageV1`]: tezos_smart_rollup_host::storage::StorageV1
pub struct LinkedList<Id, Elt>
where
    Id: AsRef<[u8]> + Encodable + Decodable + Clone,
    Elt: Encodable + Decodable + Clone,
{
    /// Relative key, inside the keyspace, under which the list is stored.
    prefix: Key,
    /// None indicates an empty list
    pointers: Option<LinkedListPointer<Id, Elt>>,
    _type: PhantomData<(Id, Elt)>,
}

/// Pointers that indicates the front and the back of the list
struct LinkedListPointer<Id, Elt> {
    front: Pointer<Id, Elt>,
    back: Pointer<Id, Elt>,
}

/// Each element in the list has a pointer
#[derive(Clone, Debug, PartialEq)]
struct Pointer<Id, Elt> {
    /// Current index of the pointer
    id: Id,
    /// Previous index of the pointer
    previous: Option<Id>,
    /// Next index of the pointer
    next: Option<Id>,
    _type: PhantomData<Elt>,
}

/// Helper function to decode the relative key prefix from rlp.
fn decode_prefix(
    it: &mut RlpIterator,
    field_name: &'static str,
) -> Result<Key, rlp::DecoderError> {
    let prefix: Vec<u8> = decode_field(&next(it)?, field_name)?;
    Key::from_bytes(&prefix).map_err(|_| DecoderError::Custom("not a key"))
}

impl<Id: Decodable, Elt> Decodable for LinkedListPointer<Id, Elt> {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(rlp::DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(rlp::DecoderError::RlpInvalidLength);
        }
        let mut it = decoder.iter();
        let front = decode_field(&next(&mut it)?, "front")?;
        let back = decode_field(&next(&mut it)?, "back")?;
        Ok(LinkedListPointer { front, back })
    }
}

impl<Id: Encodable, Elt> Encodable for LinkedListPointer<Id, Elt> {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(2);
        stream.append(&self.front);
        stream.append(&self.back);
    }
}

impl<Id: Decodable, Elt> Decodable for Pointer<Id, Elt> {
    fn decode(decoder: &Rlp) -> Result<Self, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(rlp::DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 3 {
            return Err(rlp::DecoderError::RlpInvalidLength);
        }
        let mut it = decoder.iter();
        let id = decode_field(&next(&mut it)?, "id")?;
        let previous = decode_option(&next(&mut it)?, "previous")?;
        let next = decode_option(&next(&mut it)?, "next")?;

        Ok(Pointer {
            id,
            next,
            previous,
            _type: PhantomData,
        })
    }
}

impl<Id: Encodable, Elt> Encodable for Pointer<Id, Elt> {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(3);
        stream.append(&self.id);
        append_option(stream, &self.previous);
        append_option(stream, &self.next);
    }
}

impl<Id, Elt> Encodable for LinkedList<Id, Elt>
where
    Id: AsRef<[u8]> + Encodable + Decodable + Clone,
    Elt: Encodable + Decodable + Clone,
{
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append(&self.prefix.as_bytes());
        append_option(stream, &self.pointers);
    }
}

impl<Id, Elt> Decodable for LinkedList<Id, Elt>
where
    Id: AsRef<[u8]> + Encodable + Decodable + Clone,
    Elt: Encodable + Decodable + Clone,
{
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let prefix = decode_prefix(&mut it, "prefix")?;
        let pointers = decode_option(&next(&mut it)?, "pointers")?;

        Ok(Self {
            prefix,
            pointers,
            _type: PhantomData,
        })
    }
}

#[allow(dead_code)]
impl<Id, Elt> Pointer<Id, Elt>
where
    Id: Decodable + Encodable + AsRef<[u8]>,
    Elt: Encodable + Decodable,
{
    fn pointer_key(id: &Id, prefix: &Key) -> Result<Key> {
        Key::try_from(format!("{}/{}/pointer", prefix, hex::encode(id)))
            .context("invalid pointer key")
    }

    /// Key to the pointer.
    ///
    /// This key is used when you want to read a pointer or to remove it.
    fn key(&self, prefix: &Key) -> Result<Key> {
        Self::pointer_key(&self.id, prefix)
    }

    /// Key to the data held by the pointer.
    fn data_key(id: &Id, prefix: &Key) -> Result<Key> {
        Key::try_from(format!("{}/{}/data", prefix, hex::encode(id)))
            .context("invalid data key")
    }

    fn save_data(&self, ks: &mut impl KeySpace, prefix: &Key, data: &Elt) -> Result<()> {
        let key = Self::data_key(&self.id, prefix)?;
        store_rlp(data, ks, &key).context("cannot save the pointer's data")
    }

    fn get_data(&self, ks: &impl KeySpace, prefix: &Key) -> Result<Elt> {
        let key = Self::data_key(&self.id, prefix)?;
        read_rlp(ks, &key).context("cannot read the pointer's data")
    }

    /// Load the pointer from the keyspace.
    fn read(ks: &impl KeySpace, prefix: &Key, id: &Id) -> Result<Option<Self>> {
        read_optional_rlp(ks, &Self::pointer_key(id, prefix)?)
            .context("cannot read the pointer")
    }

    /// Save the pointer in the keyspace.
    fn save(&self, ks: &mut impl KeySpace, prefix: &Key) -> Result<()> {
        store_rlp(self, ks, &self.key(prefix)?).context("cannot save pointer to storage")
    }

    /// Removes the pointer and its data from the keyspace.
    fn remove_with_data(&self, ks: &mut impl KeySpace, prefix: &Key) -> Result<()> {
        ks.delete(&self.key(prefix)?);
        ks.delete(&Self::data_key(&self.id, prefix)?);
        Ok(())
    }
}

#[allow(dead_code)]
impl<Id, Elt> LinkedList<Id, Elt>
where
    Id: AsRef<[u8]> + Encodable + Decodable + Clone + PartialEq,
    Elt: Encodable + Decodable + Clone,
{
    /// Load a list from the keyspace.
    /// If the list does not exist, a new empty list is created.
    /// Otherwise the existing list is read from the keyspace.
    pub fn new(prefix: &Key, ks: &impl KeySpace) -> Result<Self> {
        let mut list = Self::read(ks, prefix)?.unwrap_or(Self {
            prefix: prefix.clone(),
            pointers: None,
            _type: PhantomData,
        });
        // Always use the caller-supplied prefix. The serialised metadata
        // may contain a stale prefix when the subtree has been relocated
        // (e.g. /evm/delayed-inbox → /base/delayed-inbox by V53).
        list.prefix = prefix.clone();
        Ok(list)
    }

    /// Key to the metadata that defines the list.
    fn metadata_key(prefix: &Key) -> Result<Key> {
        Key::try_from(format!("{prefix}/meta")).context("invalid metadata key")
    }

    /// Saves the LinkedList in the keyspace.
    ///
    /// Only save the back and front pointers.
    fn save(&self, ks: &mut impl KeySpace) -> Result<()> {
        let key = Self::metadata_key(&self.prefix)?;
        store_rlp(self, ks, &key).context("cannot save linked list to the storage")
    }

    /// Load the LinkedList from the keyspace.
    fn read(ks: &impl KeySpace, prefix: &Key) -> Result<Option<Self>> {
        let key = Self::metadata_key(prefix)?;
        read_optional_rlp(ks, &key).context("cannot read linked list from the storage")
    }

    /// Returns true if the list contains no elements.
    pub fn is_empty(&self) -> bool {
        self.pointers.is_none()
    }

    /// Appends an element to the back of a list.
    ///
    /// An element cannot be mutated.
    /// The Id has to be unique by element.
    /// The Id will be later use to retrieve the element
    /// (example: it can be the hash of the element).
    pub fn push(&mut self, ks: &mut impl KeySpace, id: &Id, elt: &Elt) -> Result<()> {
        // Check if the pointer already exists
        if (Pointer::read(ks, &self.prefix, id)? as Option<Pointer<Id, Elt>>).is_some() {
            return Ok(());
        }
        match &self.pointers {
            Some(LinkedListPointer { front, back }) => {
                // The list is not empty
                // Modifies the old back pointer
                let penultimate = Pointer {
                    next: Some(id.clone()), // Points to the inserted element
                    ..back.clone()
                };
                // Creates a new back pointer
                let back = Pointer {
                    id: id.clone(),
                    previous: Some(penultimate.id.clone()), // Points to the penultimate pointer
                    next: None, // None because there is no element after
                    _type: PhantomData,
                };
                // Saves the pointer
                penultimate.save(ks, &self.prefix)?;
                back.save(ks, &self.prefix)?;
                // And the save the data
                back.save_data(ks, &self.prefix, elt)?;
                // update the back pointer of the list

                // Before the addition, penultimate might be the front
                // *and* the back of the list. The back is always updated to
                // the inserted cell, but the front cell might need an update.
                if penultimate.id == front.id {
                    self.pointers = Some(LinkedListPointer {
                        front: penultimate,
                        back,
                    });
                } else {
                    // If not, we need to update only the back pointer.
                    self.pointers = Some(LinkedListPointer {
                        front: front.clone(),
                        back,
                    });
                }
            }
            None => {
                // This case corresponds to the empty list
                // A new pointer has to be created
                let back = Pointer {
                    id: id.clone(),
                    previous: None, // None because it's the only element of the list
                    next: None,     // None because it's the only element of the list
                    _type: PhantomData,
                };
                // Saves the pointer and its data
                back.save(ks, &self.prefix)?;
                back.save_data(ks, &self.prefix, elt)?;
                // update the front and back pointers of the list
                self.pointers = Some(LinkedListPointer {
                    front: back.clone(),
                    back,
                });
            }
        };
        self.save(ks)
    }

    /// Returns an element at a given index.
    ///
    /// Returns None if the element is not present
    pub fn find(&self, ks: &impl KeySpace, id: &Id) -> Result<Option<Elt>> {
        let Some::<Pointer<Id, Elt>>(pointer) = Pointer::read(ks, &self.prefix, id)?
        else {
            return Ok(None);
        };
        read_optional_rlp(
            ks,
            &Pointer::<Id, Elt>::data_key(&pointer.id, &self.prefix)?,
        )
        .context("cannot read the pointer's data")
    }

    /// Removes and returns the element at position index within the vector.
    pub fn remove(&mut self, ks: &mut impl KeySpace, id: &Id) -> Result<Option<Elt>> {
        // Check if the list is empty
        let Some(LinkedListPointer { front, back }) = &self.pointers else {
            return Ok(None);
        };
        // Get the previous and the next pointer
        let Some(pointer) = Pointer::read(ks, &self.prefix, id)? else {
            return Ok(None);
        };
        let previous = match pointer.previous {
            Some(ref previous) => Pointer::read(ks, &self.prefix, previous)?,
            None => None,
        };
        let next = match pointer.next {
            Some(ref next) => Pointer::read(ks, &self.prefix, next)?,
            None => None,
        };

        // retrieve the data
        let data = pointer.get_data(ks, &self.prefix)?;
        // delete the pointer and the data
        pointer.remove_with_data(ks, &self.prefix)?;
        match (previous, next) {
            // This case represents the list with only one element
            (None, None) => {
                // update the list pointers
                self.pointers = None;
            }
            // The head of the list is being removed
            (None, Some(next)) => {
                let new_front = Pointer {
                    previous: None, // because it's the head
                    ..next
                };
                // update the pointer
                new_front.save(ks, &self.prefix)?;

                // If the list has 2 elements and if the front is removed,
                // then the front and the back are equal.
                if new_front.id == back.id {
                    self.pointers = Some(LinkedListPointer {
                        front: new_front.clone(),
                        back: new_front,
                    });
                } else {
                    self.pointers = Some(LinkedListPointer {
                        front: new_front,
                        back: back.clone(),
                    });
                }
            }
            // The end of the list is being removed
            (Some(previous), None) => {
                let new_back = Pointer {
                    next: None, // because it's the end of the list
                    ..previous
                };
                new_back.save(ks, &self.prefix)?;
                // If the list has 2 elements and if the back is removed,
                // then the front and the back are equal.
                if new_back.id == front.id {
                    self.pointers = Some(LinkedListPointer {
                        front: new_back.clone(),
                        back: new_back,
                    });
                } else {
                    self.pointers = Some(LinkedListPointer {
                        front: front.clone(),
                        back: new_back,
                    });
                }
            }
            // Removes an element between two elements
            (Some(previous), Some(next)) => {
                let previous_id = previous.id.clone();
                let new_previous = Pointer {
                    next: Some(next.id.clone()),
                    ..previous
                };
                let new_next = Pointer {
                    previous: Some(previous_id),
                    ..next
                };
                new_previous.save(ks, &self.prefix)?;
                new_next.save(ks, &self.prefix)?;

                // We remove the second item of a list of three. It means back and front
                // have been updated and are pointing to each other now.
                if new_previous.clone().id == front.clone().id
                    && new_next.clone().id == back.clone().id
                {
                    self.pointers = Some(LinkedListPointer {
                        front: new_previous.clone(),
                        back: new_next.clone(),
                    });
                } else {
                    // If the new previous is the front pointer, we need to
                    // update the front pointer to handle the new next pointer
                    if new_previous.clone().id == front.clone().id {
                        self.pointers = Some(LinkedListPointer {
                            front: new_previous,
                            back: back.clone(),
                        });
                    } else {
                        // Similarly to the previous case. If the new next is the
                        // back pointer, we need to update the back pointer to handle
                        // the previous pointer.
                        if new_next.clone().id == back.clone().id {
                            self.pointers = Some(LinkedListPointer {
                                front: front.clone(),
                                back: new_next.clone(),
                            });
                        }
                    }
                }
            }
        };
        self.save(ks)?;
        Ok(Some(data))
    }

    /// Returns the first element of the list
    /// or `None` if it is empty.
    pub fn first(&self, ks: &impl KeySpace) -> Result<Option<Elt>> {
        let Some(LinkedListPointer { front, .. }) = &self.pointers else {
            return Ok(None);
        };
        Ok(Some(front.get_data(ks, &self.prefix)?))
    }

    /// Returns the first element of the list alongside its id
    /// or `None` if it is empty.
    pub fn first_with_id(&self, ks: &impl KeySpace) -> Result<Option<(Id, Elt)>> {
        let Some(LinkedListPointer { front, .. }) = &self.pointers else {
            return Ok(None);
        };
        Ok(Some((front.id.clone(), front.get_data(ks, &self.prefix)?)))
    }

    /// Removes the first element of the list and returns it
    pub fn pop_first(&mut self, ks: &mut impl KeySpace) -> Result<Option<Elt>> {
        let Some(LinkedListPointer { front, .. }) = &self.pointers else {
            return Ok(None);
        };
        let to_remove = front.id.clone();
        self.remove(ks, &to_remove)
    }

    /// Deletes the entire list
    pub fn delete(&mut self, ks: &mut impl KeySpace) -> Result<()> {
        ks.delete(&Self::metadata_key(&self.prefix)?);
        while self.pop_first(ks)?.is_some() {}
        self.pointers = None;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use proptest::proptest;
    use rlp::{Decodable, DecoderError, Encodable};
    use std::collections::HashMap;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_keyspace::KeySpaceLoader;

    /// Relative key, inside the test keyspace, under which the list lives.
    const PREFIX: Key = Key::from_static(b"/list");

    /// Load the test keyspace.
    fn keyspace(host: &mut MockKernelHost) -> impl KeySpace + '_ {
        host.load_or_create("/q".parse().unwrap()).unwrap()
    }

    #[derive(Clone, PartialEq, Debug)]
    struct Hash([u8; 8]);

    impl Encodable for Hash {
        fn rlp_append(&self, s: &mut rlp::RlpStream) {
            s.encoder().encode_value(&self.0);
        }
    }

    impl Decodable for Hash {
        fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
            let hash: Vec<u8> = decoder.as_val()?;
            let hash = hash
                .try_into()
                .map_err(|_| DecoderError::Custom("expected a vec of 32 elements"))?;
            Ok(Hash(hash))
        }
    }

    impl AsRef<[u8]> for Hash {
        fn as_ref(&self) -> &[u8] {
            &self.0
        }
    }

    fn traverse_f(
        ks: &impl KeySpace,
        prefix: &Key,
        pointer: Pointer<Hash, u8>,
        f: &dyn Fn(&Pointer<Hash, u8>) -> Option<Hash>,
    ) -> (Pointer<Hash, u8>, usize) {
        let mut size = 0;
        let mut cell = pointer.clone();
        loop {
            size += 1;
            match f(&cell) {
                None => break,
                Some(next) => {
                    match Pointer::<Hash, u8>::read(ks, prefix, &next).unwrap() {
                        Some(next) => cell = next,
                        None => {
                            panic!("Pointer should exist in storage");
                        }
                    }
                }
            }
        }
        (cell, size)
    }

    fn traverse(ks: &impl KeySpace, list: &LinkedList<Hash, u8>) -> usize {
        let Some(LinkedListPointer {
            ref front,
            ref back,
        }) = list.pointers
        else {
            return 0;
        };

        let (found_front, size_from_back) =
            traverse_f(ks, &list.prefix, back.clone(), &|cell: &Pointer<
                Hash,
                u8,
            >| {
                cell.previous.clone()
            });
        let (found_back, size_from_front) =
            traverse_f(ks, &list.prefix, front.clone(), &|cell: &Pointer<
                Hash,
                u8,
            >| {
                cell.next.clone()
            });

        assert_eq!(found_front, front.clone());
        assert_eq!(found_back, back.clone());
        assert_eq!(size_from_back, size_from_front);

        size_from_back
    }

    fn assert_length(ks: &impl KeySpace, list: &LinkedList<Hash, u8>, expected_len: u64) {
        let traverse_length = traverse(ks, list);
        assert_eq!(
            expected_len, traverse_length as u64,
            "Traverse size is unexpected"
        );
    }

    #[test]
    fn test_empty() {
        let mut host = MockKernelHost::default();
        let ks = keyspace(&mut host);
        let list =
            LinkedList::<Hash, u8>::new(&PREFIX, &ks).expect("list should be created");
        assert!(list.is_empty())
    }

    #[test]
    fn test_find_returns_none_when_empty() {
        let mut host = MockKernelHost::default();
        let ks = keyspace(&mut host);
        let list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
        let hash = Hash([0; 8]);
        let read: Option<u8> = list.find(&ks, &hash).expect("storage should work");
        assert!(read.is_none())
    }

    #[test]
    fn test_insert() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
        let id = Hash([0x0; 8]);
        let elt = 0x32_u8;

        list.push(&mut ks, &id, &elt).expect("storage should work");

        let read: u8 = list
            .find(&ks, &id)
            .expect("storage should work")
            .expect("element should be present");

        assert_eq!(read, elt);
    }

    #[test]
    fn test_remove() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
        let id = Hash([0x0; 8]);
        let elt = 0x32_u8;

        assert_length(&ks, &list, 0u64);
        list.push(&mut ks, &id, &elt).expect("storage should work");
        assert_length(&ks, &list, 1u64);
        let _: Option<u8> = list.remove(&mut ks, &id).expect("storage should work");
        assert_length(&ks, &list, 0u64);

        let read: Option<u8> = list.find(&ks, &id).expect("storage should work");

        assert!(read.is_none())
    }

    #[test]
    fn test_remove_nothing() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
        let id = Hash([0x0; 8]);

        let removed: Option<u8> = list.remove(&mut ks, &id).expect("storage should work");

        assert!(removed.is_none())
    }

    #[test]
    fn test_first() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
        let id = Hash([0x0; 8]);
        let elt = 0x32_u8;

        list.push(&mut ks, &id, &elt).expect("storage should workd");

        let read: u8 = list
            .first(&ks)
            .expect("storage should work")
            .expect("element should be present");

        assert_eq!(read, 0x32);
    }

    #[test]
    fn test_first_when_only_two_elements() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
        let id_1 = Hash([0x0; 8]);
        let id_2 = Hash([0x1; 8]);

        list.push(&mut ks, &id_1, &0x32_u8)
            .expect("storage should workd");
        list.push(&mut ks, &id_2, &0x33_u8)
            .expect("storage should workd");

        let read: u8 = list
            .first(&ks)
            .expect("storage should work")
            .expect("element should be present");

        assert_eq!(read, 0x32);
    }

    #[test]
    fn test_first_after_two_push() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
        let id_1 = Hash([0x0; 8]);
        let id_2 = Hash([0x1; 8]);

        list.push(&mut ks, &id_1, &0x32_u8)
            .expect("storage should workd");
        list.push(&mut ks, &id_2, &0x33_u8)
            .expect("storage should workd");
        let _: Option<u8> = list.remove(&mut ks, &id_1).expect("storage should work");

        let read: u8 = list
            .first(&ks)
            .expect("storage should work")
            .expect("element should be present");

        assert_eq!(read, 0x33);
    }

    #[test]
    fn test_delete() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let mut list: LinkedList<Hash, u8> =
            LinkedList::new(&PREFIX, &ks).expect("list should be created");
        let id_1 = Hash([0x0; 8]);
        let id_2 = Hash([0x1; 8]);
        list.push(&mut ks, &id_1, &0x32_u8)
            .expect("push should have worked");
        list.push(&mut ks, &id_2, &0x33_u8)
            .expect("push should have worked");
        list.delete(&mut ks).expect("delete should have worked");
        let reloaded_list: LinkedList<Hash, u8> =
            LinkedList::new(&PREFIX, &ks).expect("list should be created");
        assert!(reloaded_list.is_empty());
    }

    /// The keyspace-backed list must lay its entries out at exactly the
    /// durable paths the previous `StorageV1`-based list used, so an
    /// existing on-chain delayed inbox keeps being read after the upgrade.
    /// Here a keyspace named `/base` with prefix `/delayed-inbox` must land
    /// data, pointer and metadata under `/base/delayed-inbox/...`.
    #[test]
    fn keyspace_list_lands_at_historical_absolute_paths() {
        use tezos_smart_rollup_host::path::RefPath;
        use tezos_smart_rollup_host::storage::StorageV1;

        let mut host = MockKernelHost::default();
        let id = Hash([0xab; 8]);
        let elt = 0x42_u8;
        {
            let mut ks = host.load_or_create("/base".parse().unwrap()).unwrap();
            let mut list = LinkedList::new(&Key::from_static(b"/delayed-inbox"), &ks)
                .expect("list should be created");
            list.push(&mut ks, &id, &elt).expect("push should work");
        }

        // The element data is readable, as raw rlp, at the historical path.
        let data_path =
            RefPath::assert_from(b"/base/delayed-inbox/abababababababab/data");
        let raw: u8 = tezos_storage::read_rlp(&host, &data_path)
            .expect("data should be readable at the historical path");
        assert_eq!(raw, elt);

        // The pointer and metadata nodes exist at their historical paths.
        assert!(host
            .store_read_all(&RefPath::assert_from(
                b"/base/delayed-inbox/abababababababab/pointer"
            ))
            .is_ok());
        assert!(host
            .store_read_all(&RefPath::assert_from(b"/base/delayed-inbox/meta"))
            .is_ok());
    }

    fn fill_list(
        ks: &mut impl KeySpace,
        elements: &HashMap<[u8; 8], u8>,
    ) -> LinkedList<Hash, u8> {
        let mut list = LinkedList::new(&PREFIX, ks).expect("list should be created");
        for (pos, (id, elt)) in elements.iter().enumerate() {
            list.push(ks, &Hash(*id), elt).expect("storage should work");

            assert_length(ks, &list, pos as u64 + 1);
        }
        list
    }

    fn make_list(n: u8, ks: &mut impl KeySpace) -> (LinkedList<Hash, u8>, Vec<Hash>) {
        let mut list = LinkedList::new(&PREFIX, ks).expect("list should be created");
        let mut elements = vec![];
        for i in 0..n {
            let id = Hash([i; 8]);
            list.push(ks, &id, &i).unwrap();
            elements.push(id);
        }

        (list, elements)
    }

    #[test]
    fn test_push_4_remove_pos_0() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let (mut list, elements) = make_list(4, &mut ks);

        assert_length(&ks, &list, 4);
        let _: Option<u8> = list
            .remove(&mut ks, &elements[0])
            .expect("storage should work");
        assert_length(&ks, &list, 3);
    }

    #[test]
    fn test_push_4_remove_pos_1() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let (mut list, elements) = make_list(4, &mut ks);

        assert_length(&ks, &list, 4);
        let _: Option<u8> = list
            .remove(&mut ks, &elements[1])
            .expect("storage should work");
        assert_length(&ks, &list, 3);
    }

    #[test]
    fn test_push_4_remove_pos_2() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let (mut list, elements) = make_list(4, &mut ks);

        assert_length(&ks, &list, 4);
        let _: Option<u8> = list
            .remove(&mut ks, &elements[2])
            .expect("storage should work");
        assert_length(&ks, &list, 3);
    }

    #[test]
    fn test_push_4_remove_pos_3() {
        let mut host = MockKernelHost::default();
        let mut ks = keyspace(&mut host);
        let (mut list, elements) = make_list(4, &mut ks);

        assert_length(&ks, &list, 4);
        let _: Option<u8> = list
            .remove(&mut ks, &elements[3])
            .expect("storage should work");
        assert_length(&ks, &list, 3);
    }

    fn elements() -> impl Strategy<Value = (HashMap<[u8; 8], u8>, usize)> {
        // Generate a HashMap with keys of size [u8; 32] and u8 values
        prop::collection::hash_map(any::<[u8; 8]>(), any::<u8>(), 1..10).prop_flat_map(
            |map| {
                let map_len = map.len(); // Get the length of the HashMap

                // Ensure there's at least one element in the map
                let random_index_strategy = 0..map_len; // Strategy to pick a random index

                // Return the HashMap and the randomly selected index value
                (Just(map.clone()), random_index_strategy)
                    .prop_map(move |(map, idx)| (map, idx))
            },
        )
    }

    proptest! {

        #![proptest_config(ProptestConfig::with_cases(200))]

        #[test]
        fn test_push_remove_consistency((elements, idx) in elements()) {
            let mut host = MockKernelHost::default();
            let mut ks = keyspace(&mut host);
            let mut list = fill_list(&mut ks, &elements);

            let elt = elements.keys().nth(idx).unwrap();
            list.remove(&mut ks, &Hash(*elt)).unwrap();

            assert_length(&ks, &list, (elements.len() as u64) - 1);

        }

        #[test]
        fn test_pushed_elements_are_present((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let mut ks = keyspace(&mut host);
            let list = fill_list(&mut ks, &elements);
            for (id, elt) in & elements {
                let read: u8 = list.find(&ks, &Hash(*id)).expect("storage should work").expect("element should be present");
                assert_eq!(elt, &read)
            }
        }

        #[test]
        fn test_push_element_create_non_empty_list((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let mut ks = keyspace(&mut host);
            let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
            assert!(list.is_empty());
            for (id, elt) in elements {
                list.push(&mut ks, &Hash(id), &elt).expect("storage should work");
                assert!(!list.is_empty())
            }
        }

        #[test]
        fn test_remove_from_empty_creates_empty_list(elements: Vec<[u8; 8]>) {
            let mut host = MockKernelHost::default();
            let mut ks = keyspace(&mut host);
            let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
            assert!(list.is_empty());
            for id in elements {
                let _: Option<u8> = list.remove(&mut ks, &Hash(id)).expect("storage to work");
            }
            assert!(list.is_empty());
        }

        #[test]
        fn test_remove_returns_the_appropriate_element((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let mut ks = keyspace(&mut host);
            let mut list = fill_list(&mut ks, &elements);
            let mut length : u64 = elements.len().try_into().unwrap();
            for (id, elt) in &elements {
                let removed: u8 = list.remove(&mut ks, &Hash(*id)).expect("storage should work").expect("element should be present");
                length -= 1;
                assert_length(&ks, &list, length);
                assert_eq!(elt, &removed)
            }
        }

        #[test]
        fn test_remove_everything_creates_the_empty_list((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let mut ks = keyspace(&mut host);
            let mut list = fill_list(&mut ks, &elements);
            for (id, _) in elements {
                let _: u8 = list.remove(&mut ks, &Hash(id)).expect("storage should work").expect("element should be present");
            }
            assert!(list.is_empty())
        }

        #[test]
        fn test_list_is_kept_between_reboots((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let mut ks = keyspace(&mut host);
            // Reconstructing the list from the keyspace re-reads its metadata,
            // simulating a reboot while the durable storage persists.
            for (id, elt) in &elements {
                let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
                list.push(&mut ks, &Hash(*id), elt)
                    .expect("storage should work");
                assert!(!list.is_empty())
            }
            for (id, elt) in &elements {
                let list = LinkedList::new(&PREFIX, &ks).expect("list should be created");
                let read: u8 = list
                    .find(&ks, &Hash(*id))
                    .expect("storage should work")
                    .expect("element should be present");
                assert_eq!(elt, &read);
            }
        }

        #[test]
        fn test_pop_first_after_push((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let mut ks = keyspace(&mut host);
            let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");

            for (id, elt) in elements {
                list.push(&mut ks, &Hash(id), &elt).expect("storage should work");
                let removed = list.pop_first(&mut ks).expect("storage should work").expect("element should be present");
                assert_eq!(elt, removed);
            }
        }

        #[test]
        fn test_pop_first_keep_the_order((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let mut ks = keyspace(&mut host);
            let mut list = LinkedList::new(&PREFIX, &ks).expect("list should be created");

            let mut inserted = vec![];
            let mut removed = vec![];

            for (id, elt) in elements {
                list.push(&mut ks, &Hash(id), &elt).expect("storage should work");
                inserted.push(elt);
            }

            while !list.is_empty() {
                let pop = list.pop_first(&mut ks).expect("storage should work").expect("element should be present");
                removed.push(pop);
            }

            assert_eq!(inserted, removed);
        }
    }
}
