// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>

use anyhow::{Context, Result};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpIterator, RlpStream};
use std::marker::PhantomData;
use tezos_ethereum::rlp_helpers::{append_option, decode_field, decode_option, next};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::{concat, OwnedPath, Path};
use tezos_storage::{read_optional_rlp, read_rlp, store_rlp};

/// Doubly linked list using the durable storage.
///
/// The list is generic over element and index.
/// The element has to implement Decodable and Encodable
/// The Id has to implement AsRef<[u8]>
///
/// The list is lazy. Not all the list is loaded at initialization.
/// Elements are saved/read at the appropriate moment.
pub struct LinkedList<Id, Elt>
where
    Id: AsRef<[u8]> + Encodable + Decodable + Clone,
    Elt: Encodable + Decodable + Clone,
{
    /// Absolute path to queue
    path: OwnedPath,
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

/// Helper function to decode a path from rlp
fn decode_path(
    it: &mut RlpIterator,
    field_name: &'static str,
) -> Result<OwnedPath, rlp::DecoderError> {
    let path: Vec<u8> = decode_field(&next(it)?, field_name)?;
    OwnedPath::try_from(path).map_err(|_| DecoderError::Custom("not a path"))
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
        stream.append(&self.path.as_bytes());
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
        let path = decode_path(&mut it, "path")?;
        let pointers = decode_option(&next(&mut it)?, "pointers")?;

        Ok(Self {
            path,
            pointers,
            _type: PhantomData,
        })
    }
}

#[allow(dead_code)]
impl<Id: Decodable + Encodable + AsRef<[u8]>, Elt: Encodable + Decodable>
    Pointer<Id, Elt>
{
    fn pointer_path(id: &Id, prefix: &impl Path) -> Result<OwnedPath> {
        let path = hex::encode(id);
        let path: Vec<u8> = format!("/{path}/pointer").into();
        let path = OwnedPath::try_from(path)?;
        let path = concat(prefix, &path)?;
        Ok(path)
    }

    /// Path to the pointer
    ///
    /// This path is used when you want to read a pointer or to remove it.
    fn path(&self, prefix: &impl Path) -> Result<OwnedPath> {
        Self::pointer_path(&self.id, prefix)
    }

    /// Path to the data held by the pointer.
    fn data_path(&self, prefix: &impl Path) -> Result<OwnedPath> {
        let path = hex::encode(&self.id);
        let path: Vec<u8> = format!("/{path}/data").into();
        let path = OwnedPath::try_from(path)?;
        let path = concat(prefix, &path)?;
        Ok(path)
    }

    fn save_data(
        &self,
        host: &mut impl Runtime,
        prefix: &impl Path,
        data: &Elt,
    ) -> Result<()> {
        let path = self.data_path(prefix)?;
        store_rlp(data, host, &path).context("cannot save the pointer's data")
    }

    fn get_data(&self, host: &impl Runtime, prefix: &impl Path) -> Result<Elt> {
        let path = self.data_path(prefix)?;
        read_rlp(host, &path).context("cannot read the pointer's data")
    }

    /// Load the pointer from the durable storage
    fn read(host: &impl Runtime, prefix: &impl Path, id: &Id) -> Result<Option<Self>> {
        read_optional_rlp(host, &Self::pointer_path(id, prefix)?)
    }

    /// Save the pointer in the durable storage
    fn save(&self, host: &mut impl Runtime, prefix: &impl Path) -> Result<()> {
        store_rlp(self, host, &self.path(prefix)?)
            .context("cannot save pointer to storage")
    }

    /// Removes the pointer and its data frm the durable storage.
    fn remove_with_data(
        &self,
        host: &mut impl Runtime,
        prefix: &impl Path,
    ) -> Result<()> {
        let path = hex::encode(&self.id);
        let path: Vec<u8> = format!("/{path}").into();
        let path = OwnedPath::try_from(path)?;
        let path = concat(prefix, &path)?;
        host.store_delete(&path)
            .context("cannot remove the pointer")
    }
}

#[allow(dead_code)]
impl<Id, Elt> LinkedList<Id, Elt>
where
    Id: AsRef<[u8]> + Encodable + Decodable + Clone + PartialEq,
    Elt: Encodable + Decodable + Clone,
{
    /// Load a list from the storage.
    /// If the list does not exist, a new empty list is created.
    /// Otherwise the existing list is read from the storage.
    pub fn new(path: &impl Path, host: &impl Runtime) -> Result<Self> {
        let list = Self::read(host, path)?.unwrap_or(Self {
            path: path.into(),
            pointers: None,
            _type: PhantomData,
        });
        Ok(list)
    }

    /// Path to the metadata that defines the list.
    fn metadata_path(root: &impl Path) -> Result<OwnedPath> {
        let meta_path: Vec<u8> = "/meta".into();
        let meta_path = OwnedPath::try_from(meta_path)?;
        let path = concat(root, &meta_path)?;
        Ok(path)
    }

    /// Saves the LinkedList in the durable storage.
    ///
    /// Only save the back and front pointers.
    fn save(&self, host: &mut impl Runtime) -> Result<()> {
        let path = Self::metadata_path(&self.path)?;
        store_rlp(self, host, &path).context("cannot save linked list from the storage")
    }

    /// Load the LinkedList from the durable storage.
    fn read(host: &impl Runtime, path: &impl Path) -> Result<Option<Self>> {
        let path = Self::metadata_path(path)?;
        read_optional_rlp(host, &path)
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
    pub fn push(&mut self, host: &mut impl Runtime, id: &Id, elt: &Elt) -> Result<()> {
        // Check if the path already exist
        if (Pointer::read(host, &self.path, id)? as Option<Pointer<Id, Elt>>).is_some() {
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
                penultimate.save(host, &self.path)?;
                back.save(host, &self.path)?;
                // And the save the data
                back.save_data(host, &self.path, elt)?;
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
                back.save(host, &self.path)?;
                back.save_data(host, &self.path, elt)?;
                // update the front and back pointers of the list
                self.pointers = Some(LinkedListPointer {
                    front: back.clone(),
                    back,
                });
            }
        };
        self.save(host)
    }

    /// Returns an element at a given index.
    ///
    /// Returns None if the element is not present
    pub fn find(&self, host: &impl Runtime, id: &Id) -> Result<Option<Elt>> {
        let Some::<Pointer<Id, Elt>>(pointer) = Pointer::read(host, &self.path, id)?
        else {
            return Ok(None);
        };
        read_optional_rlp(host, &pointer.data_path(&self.path)?)
    }

    /// Removes and returns the element at position index within the vector.
    pub fn remove(&mut self, host: &mut impl Runtime, id: &Id) -> Result<Option<Elt>> {
        // Check if the list is empty
        let Some(LinkedListPointer { front, back }) = &self.pointers else {
            return Ok(None);
        };
        // Get the previous and the next pointer
        let Some(pointer) = Pointer::read(host, &self.path, id)? else {
            return Ok(None);
        };
        let previous = match pointer.previous {
            Some(ref previous) => Pointer::read(host, &self.path, previous)?,
            None => None,
        };
        let next = match pointer.next {
            Some(ref next) => Pointer::read(host, &self.path, next)?,
            None => None,
        };

        // retrieve the data
        let data = pointer.get_data(host, &self.path)?;
        // delete the pointer and the data
        pointer.remove_with_data(host, &self.path)?;
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
                new_front.save(host, &self.path)?;

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
                new_back.save(host, &self.path)?;
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
                new_previous.save(host, &self.path)?;
                new_next.save(host, &self.path)?;

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
        self.save(host)?;
        Ok(Some(data))
    }

    /// Returns the first element of the list
    /// or `None` if it is empty.
    pub fn first(&self, host: &impl Runtime) -> Result<Option<Elt>> {
        let Some(LinkedListPointer { front, .. }) = &self.pointers else {
            return Ok(None);
        };
        Ok(Some(front.get_data(host, &self.path)?))
    }

    /// Returns the first element of the list alongside its id
    /// or `None` if it is empty.
    pub fn first_with_id(&self, host: &impl Runtime) -> Result<Option<(Id, Elt)>> {
        let Some(LinkedListPointer { front, .. }) = &self.pointers else {
            return Ok(None);
        };
        Ok(Some((front.id.clone(), front.get_data(host, &self.path)?)))
    }

    /// Removes the first element of the list and returns it
    pub fn pop_first(&mut self, host: &mut impl Runtime) -> Result<Option<Elt>> {
        let Some(LinkedListPointer { front, .. }) = &self.pointers else {
            return Ok(None);
        };
        let to_remove = front.id.clone();
        self.remove(host, &to_remove)
    }

    /// Deletes the entire list
    pub fn delete(&mut self, host: &mut impl Runtime) -> Result<()> {
        if host.store_has(&self.path)?.is_some() {
            host.store_delete(&self.path)?;
        };
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
    use tezos_smart_rollup_debug::Runtime;
    use tezos_smart_rollup_host::path::RefPath;

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
        host: &MockKernelHost,
        list_path: &OwnedPath,
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
                    match Pointer::<Hash, u8>::read(host, list_path, &next).unwrap() {
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

    fn traverse(host: &MockKernelHost, list: &LinkedList<Hash, u8>) -> usize {
        let Some(LinkedListPointer {
            ref front,
            ref back,
        }) = list.pointers
        else {
            return 0;
        };

        let (found_front, size_from_back) =
            traverse_f(host, &list.path, back.clone(), &|cell: &Pointer<
                Hash,
                u8,
            >| {
                cell.previous.clone()
            });
        let (found_back, size_from_front) =
            traverse_f(host, &list.path, front.clone(), &|cell: &Pointer<
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

    fn assert_length(
        host: &MockKernelHost,
        list: &LinkedList<Hash, u8>,
        expected_len: u64,
    ) {
        // Both the linked list pointers and the element pointers lies under
        // the `list.path`.
        let keys = host.store_count_subkeys(&list.path).unwrap_or(0u64);
        // Removes the linked list pointers to compute the actual length.
        let actual_length = keys.saturating_sub(1u64);
        assert_eq!(
            expected_len, actual_length,
            "Unexpected length for the list"
        );
        let traverse_length = traverse(host, list);
        assert_eq!(
            expected_len, traverse_length as u64,
            "Traverse size is unexpected"
        );
    }

    #[test]
    fn test_empty() {
        let host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let list =
            LinkedList::<Hash, u8>::new(&path, &host).expect("list should be created");
        assert!(list.is_empty())
    }

    #[test]
    fn test_find_returns_none_when_empty() {
        let host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let list = LinkedList::new(&path, &host).expect("list should be created");
        let hash = Hash([0; 8]);
        let read: Option<u8> = list.find(&host, &hash).expect("storage should work");
        assert!(read.is_none())
    }

    #[test]
    fn test_insert() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        let id = Hash([0x0; 8]);
        let elt = 0x32_u8;

        list.push(&mut host, &id, &elt)
            .expect("storage should work");

        let read: u8 = list
            .find(&host, &id)
            .expect("storage should work")
            .expect("element should be present");

        assert_eq!(read, elt);
    }

    #[test]
    fn test_remove() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        let id = Hash([0x0; 8]);
        let elt = 0x32_u8;

        assert_length(&host, &list, 0u64);
        list.push(&mut host, &id, &elt)
            .expect("storage should work");
        assert_length(&host, &list, 1u64);
        let _: Option<u8> = list.remove(&mut host, &id).expect("storage should work");
        assert_length(&host, &list, 0u64);

        let read: Option<u8> = list.find(&host, &id).expect("storage should work");

        assert!(read.is_none())
    }

    #[test]
    fn test_remove_nothing() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        let id = Hash([0x0; 8]);

        let removed: Option<u8> =
            list.remove(&mut host, &id).expect("storage should work");

        assert!(removed.is_none())
    }

    #[test]
    fn test_first() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        let id = Hash([0x0; 8]);
        let elt = 0x32_u8;

        list.push(&mut host, &id, &elt)
            .expect("storage should workd");

        let read: u8 = list
            .first(&host)
            .expect("storage should work")
            .expect("element should be present");

        assert_eq!(read, 0x32);
    }

    #[test]
    fn test_first_when_only_two_elements() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        let id_1 = Hash([0x0; 8]);
        let id_2 = Hash([0x1; 8]);

        list.push(&mut host, &id_1, &0x32_u8)
            .expect("storage should workd");
        list.push(&mut host, &id_2, &0x33_u8)
            .expect("storage should workd");

        let read: u8 = list
            .first(&host)
            .expect("storage should work")
            .expect("element should be present");

        assert_eq!(read, 0x32);
    }

    #[test]
    fn test_first_after_two_push() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        let id_1 = Hash([0x0; 8]);
        let id_2 = Hash([0x1; 8]);

        list.push(&mut host, &id_1, &0x32_u8)
            .expect("storage should workd");
        list.push(&mut host, &id_2, &0x33_u8)
            .expect("storage should workd");
        let _: Option<u8> = list.remove(&mut host, &id_1).expect("storage should work");

        let read: u8 = list
            .first(&host)
            .expect("storage should work")
            .expect("element should be present");

        assert_eq!(read, 0x33);
    }

    #[test]
    fn test_delete() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list: LinkedList<Hash, u8> =
            LinkedList::new(&path, &host).expect("list should be created");
        let id_1 = Hash([0x0; 8]);
        let id_2 = Hash([0x1; 8]);
        list.push(&mut host, &id_1, &0x32_u8)
            .expect("push should have worked");
        list.push(&mut host, &id_2, &0x33_u8)
            .expect("push should have worked");
        list.delete(&mut host).expect("delete should have worked");
        let reloaded_list: LinkedList<Hash, u8> =
            LinkedList::new(&path, &host).expect("list should be created");
        assert!(reloaded_list.is_empty());
    }

    fn fill_list(
        elements: &HashMap<[u8; 8], u8>,
    ) -> (MockKernelHost, LinkedList<Hash, u8>) {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        for (pos, (id, elt)) in elements.iter().enumerate() {
            list.push(&mut host, &Hash(*id), elt)
                .expect("storage should work");

            assert_length(&host, &list, pos as u64 + 1);
        }
        (host, list)
    }

    fn make_list(n: u8, host: &mut MockKernelHost) -> (LinkedList<Hash, u8>, Vec<Hash>) {
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, host).expect("list should be created");
        let mut elements = vec![];
        for i in 0..n {
            let id = Hash([i; 8]);
            list.push(host, &id, &i).unwrap();
            elements.push(id);
        }

        (list, elements)
    }

    #[test]
    fn test_push_4_remove_pos_0() {
        let mut host = MockKernelHost::default();
        let (mut list, elements) = make_list(4, &mut host);

        assert_length(&host, &list, 4);
        let _: Option<u8> = list
            .remove(&mut host, &elements[0])
            .expect("storage should work");
        assert_length(&host, &list, 3);
    }

    #[test]
    fn test_push_4_remove_pos_1() {
        let mut host = MockKernelHost::default();
        let (mut list, elements) = make_list(4, &mut host);

        assert_length(&host, &list, 4);
        let _: Option<u8> = list
            .remove(&mut host, &elements[1])
            .expect("storage should work");
        assert_length(&host, &list, 3);
    }

    #[test]
    fn test_push_4_remove_pos_2() {
        let mut host = MockKernelHost::default();
        let (mut list, elements) = make_list(4, &mut host);

        assert_length(&host, &list, 4);
        let _: Option<u8> = list
            .remove(&mut host, &elements[2])
            .expect("storage should work");
        assert_length(&host, &list, 3);
    }

    #[test]
    fn test_push_4_remove_pos_3() {
        let mut host = MockKernelHost::default();
        let (mut list, elements) = make_list(4, &mut host);

        assert_length(&host, &list, 4);
        let _: Option<u8> = list
            .remove(&mut host, &elements[3])
            .expect("storage should work");
        assert_length(&host, &list, 3);
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
            let (mut host, mut list) = fill_list(&elements);

            let elt = elements.keys().nth(idx).unwrap();
            list.remove(&mut host, &Hash(*elt)).unwrap();

            assert_length(&host, &list, (elements.len() as u64) - 1);

        }

        #[test]
        fn test_pushed_elements_are_present((elements, _) in elements()) {
            let (host, list) = fill_list(&elements);
            for (id, elt) in & elements {
                let read: u8 = list.find(&host, &Hash(*id)).expect("storage should work").expect("element should be present");
                assert_eq!(elt, &read)
            }
        }

        #[test]
        fn test_push_element_create_non_empty_list((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let path = RefPath::assert_from(b"/list");
            let mut list = LinkedList::new(&path, &host).expect("list should be created");
            assert!(list.is_empty());
            for (id, elt) in elements {
                list.push(&mut host, &Hash(id), &elt).expect("storage should work");
                assert!(!list.is_empty())
            }
        }

        #[test]
        fn test_remove_from_empty_creates_empty_list(elements: Vec<[u8; 8]>) {
            let mut host = MockKernelHost::default();
            let path = RefPath::assert_from(b"/list");
            let mut list = LinkedList::new(&path, &host).expect("list should be created");
            assert!(list.is_empty());
            for id in elements {
                let _: Option<u8> = list.remove(&mut host, &Hash(id)).expect("storage to work");
            }
            assert!(list.is_empty());
        }

        #[test]
        fn test_remove_returns_the_appropriate_element((elements, _) in elements()) {
            let (mut host, mut list) = fill_list(&elements);
            let mut length : u64 = elements.len().try_into().unwrap();
            for (id, elt) in &elements {
                let removed: u8 = list.remove(&mut host, &Hash(*id)).expect("storage should work").expect("element should be present");
                length -= 1;
                assert_length(&host, &list, length);
                assert_eq!(elt, &removed)
            }
        }

        #[test]
        fn test_remove_everything_creates_the_empty_list((elements, _) in elements()) {
            let (mut host, mut list) = fill_list(&elements);
            for (id, _) in elements {
                let _: u8 = list.remove(&mut host, &Hash(id)).expect("storage should work").expect("element should be present");
            }
            assert!(list.is_empty())
        }

        #[test]
        fn test_list_is_kept_between_reboots((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let path = RefPath::assert_from(b"/list");
            for (id, elt) in &elements {
                let mut list = LinkedList::new(&path, &host).expect("list should be created");
                list.push(&mut host, &Hash(*id), elt)
                    .expect("storage should work");
                assert!(!list.is_empty())
            }
            for (id, elt) in &elements {
                let list = LinkedList::new(&path, &host).expect("list should be created");
                let read: u8 = list
                    .find(&host, &Hash(*id))
                    .expect("storage should work")
                    .expect("element should be present");
                assert_eq!(elt, &read);
            }
        }

        #[test]
        fn test_pop_first_after_push((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let path = RefPath::assert_from(b"/list");
            let mut list = LinkedList::new(&path, &host).expect("list should be created");

            for (id, elt) in elements {
                list.push(&mut host, &Hash(id), &elt).expect("storage should work");
                let removed = list.pop_first(&mut host).expect("storage should work").expect("element should be present");
                assert_eq!(elt, removed);
            }
        }

        #[test]
        fn test_pop_first_keep_the_order((elements, _) in elements()) {
            let mut host = MockKernelHost::default();
            let path = RefPath::assert_from(b"/list");
            let mut list = LinkedList::new(&path, &host).expect("list should be created");

            let mut inserted = vec![];
            let mut removed = vec![];

            for (id, elt) in elements {
                list.push(&mut host, &Hash(id), &elt).expect("storage should work");
                inserted.push(elt);
            }

            while !list.is_empty() {
                let pop = list.pop_first(&mut host).expect("storage should work").expect("element should be present");
                removed.push(pop);
            }

            assert_eq!(inserted, removed);
        }
    }
}
