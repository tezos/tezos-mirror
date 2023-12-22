// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>

use crate::storage;
use anyhow::{Context, Result};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpIterator, RlpStream};
use std::marker::PhantomData;
use tezos_ethereum::rlp_helpers::{append_option, decode_field, decode_option, next};
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path},
    runtime::Runtime,
};

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
    Elt: Encodable + Decodable,
{
    /// Absolute path to queue
    path: OwnedPath,
    /// None indicates an empty list
    pointers: Option<LinkedListPointer<Id>>,
    _type: PhantomData<(Id, Elt)>,
}

/// Pointers that indicates the front and the back of the list
struct LinkedListPointer<Id> {
    front: Pointer<Id>,
    back: Pointer<Id>,
}

/// Each element in the list has a pointer
#[derive(Clone, Debug)]
struct Pointer<Id> {
    /// Current index of the pointer
    id: Id,
    /// Previous index of the pointer
    previous: Option<Id>,
    /// Next index of the pointer
    next: Option<Id>,
}

/// Helper function to decode a path from rlp
fn decode_path(
    it: &mut RlpIterator,
    field_name: &'static str,
) -> Result<OwnedPath, rlp::DecoderError> {
    let path: Vec<u8> = decode_field(&next(it)?, field_name)?;
    OwnedPath::try_from(path).map_err(|_| DecoderError::Custom("not a path"))
}

impl<Id: Decodable> Decodable for LinkedListPointer<Id> {
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

impl<Id: Encodable> Encodable for LinkedListPointer<Id> {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(2);
        stream.append(&self.front);
        stream.append(&self.back);
    }
}

impl<Id: Decodable> Decodable for Pointer<Id> {
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

        Ok(Pointer { id, next, previous })
    }
}

impl<Id: Encodable> Encodable for Pointer<Id> {
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
    Elt: Encodable + Decodable,
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
    Elt: Encodable + Decodable,
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
impl<Id: Decodable + Encodable + AsRef<[u8]>> Pointer<Id> {
    fn pointer_path(id: &Id, prefix: &impl Path) -> Result<OwnedPath> {
        let path = hex::encode(id);
        let path: Vec<u8> = format!("/{}/pointer", path).into();
        let path = OwnedPath::try_from(path)?;
        let path = concat(prefix, &path)?;
        Ok(path)
    }

    /// Path to the data held by the pointer.
    fn data_path(&self, prefix: &impl Path) -> Result<OwnedPath> {
        let path = hex::encode(&self.id);
        let path: Vec<u8> = format!("/{}/data", path).into();
        let path = OwnedPath::try_from(path)?;
        let path = concat(prefix, &path)?;
        Ok(path)
    }

    /// Load the pointer from the durable storage
    fn read(host: &impl Runtime, prefix: &impl Path, id: &Id) -> Result<Option<Self>> {
        storage::read_optional_rlp(host, &Self::pointer_path(id, prefix)?)
    }

    /// Save the pointer in the durable storage
    fn save(&self, host: &mut impl Runtime, prefix: &impl Path) -> Result<()> {
        storage::store_rlp(self, host, &Self::pointer_path(&self.id, prefix)?)
            .context("cannot save pointer to storage")
    }
}

#[allow(dead_code)]
impl<Id, Elt> LinkedList<Id, Elt>
where
    Id: AsRef<[u8]> + Encodable + Decodable + Clone,
    Elt: Encodable + Decodable,
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

    /// Saves the LinkedList in the durable storage.
    ///
    /// Only save the back and front pointers.
    fn save(&self, host: &mut impl Runtime) -> Result<()> {
        storage::store_rlp(self, host, &self.path)
            .context("cannot load linked list from the storage")
    }

    /// Load the LinkedList from the durable storage.
    fn read(host: &impl Runtime, path: &impl Path) -> Result<Option<Self>> {
        storage::read_optional_rlp(host, path)
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
        if Pointer::read(host, &self.path, id)?.is_some() {
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
                };
                // Saves the pointer
                penultimate.save(host, &self.path)?;
                back.save(host, &self.path)?;
                // And the save the data
                let data_path = back.data_path(&self.path)?;
                storage::store_rlp(elt, host, &data_path)?;
                // update the back pointer of the list
                self.pointers = Some(LinkedListPointer {
                    front: front.clone(),
                    back,
                });
            }
            None => {
                // This case corresponds to the empty list
                // A new pointer has to be created
                let back = Pointer {
                    id: id.clone(),
                    previous: None, // None because it's the only element of the list
                    next: None,     // None because it's the only element of the list
                };
                // Saves the pointer and its data
                back.save(host, &self.path)?;
                let data_path = back.data_path(&self.path)?;
                storage::store_rlp(elt, host, &data_path)?;
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
    pub fn get(&self, host: &impl Runtime, id: &Id) -> Result<Option<Elt>> {
        let Some(pointer) = Pointer::read(host, &self.path, id)? else {return Ok(None)};
        storage::read_optional_rlp(host, &pointer.data_path(&self.path)?)
    }
}

#[cfg(test)]
mod tests {
    use super::LinkedList;
    use proptest::proptest;
    use rlp::{Decodable, DecoderError, Encodable};
    use std::collections::HashMap;
    use tezos_ethereum::transaction::TRANSACTION_HASH_SIZE;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_mock::MockHost;

    #[derive(Clone)]
    struct Hash([u8; TRANSACTION_HASH_SIZE]);

    impl Encodable for Hash {
        fn rlp_append(&self, s: &mut rlp::RlpStream) {
            s.append(&self.0.to_vec());
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

    #[test]
    fn test_empty() {
        let host = MockHost::default();
        let path = RefPath::assert_from(b"/list");
        let list =
            LinkedList::<Hash, u8>::new(&path, &host).expect("list should be created");
        assert!(list.is_empty())
    }

    #[test]
    fn test_get_returns_none_when_empty() {
        let host = MockHost::default();
        let path = RefPath::assert_from(b"/list");
        let list = LinkedList::new(&path, &host).expect("list should be created");
        let hash = Hash([0; TRANSACTION_HASH_SIZE]);
        let read: Option<u8> = list.get(&host, &hash).expect("storage should work");
        assert!(read.is_none())
    }

    #[test]
    fn test_insert() {
        let mut host = MockHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        let id = Hash([0x0; TRANSACTION_HASH_SIZE]);
        let elt = 0x32_u8;

        list.push(&mut host, &id, &elt)
            .expect("storage should work");

        let read: u8 = list
            .get(&host, &id)
            .expect("storage should work")
            .expect("element should be present");

        assert_eq!(read, elt);
    }

    fn fill_list(
        elements: &HashMap<[u8; TRANSACTION_HASH_SIZE], u8>,
    ) -> (MockHost, LinkedList<Hash, u8>) {
        let mut host = MockHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        for (id, elt) in elements {
            list.push(&mut host, &Hash(*id), elt)
                .expect("storage should work");
            assert!(!list.is_empty())
        }
        (host, list)
    }

    proptest! {
    #[test]
    fn test_pushed_elements_are_present(elements: HashMap<[u8; TRANSACTION_HASH_SIZE], u8>) {
        let (host, list) = fill_list(&elements);
        for (id, elt) in & elements {
            let read: u8 = list.get(&host, &Hash(*id)).expect("storage should work").expect("element should be present");
            assert_eq!(elt, &read)
        }
    }


    #[test]
    fn test_push_element_create_non_empty_list(elements: HashMap<[u8; TRANSACTION_HASH_SIZE], u8>) {
        let mut host = MockHost::default();
        let path = RefPath::assert_from(b"/list");
        let mut list = LinkedList::new(&path, &host).expect("list should be created");
        assert!(list.is_empty());
        for (id, elt) in elements {
            list.push(&mut host, &Hash(id), &elt).expect("storage should work");
            assert!(!list.is_empty())
        }
    }

        #[test]
        fn test_list_is_kept_between_reboots(elements: HashMap<[u8; TRANSACTION_HASH_SIZE], u8>) {
            let mut host = MockHost::default();
            let path = RefPath::assert_from(b"/list");
            for (id, elt) in &elements {
                let mut list = LinkedList::new(&path, &host).expect("list should be created");
                list.push(&mut host, &Hash(*id), elt).expect("storage should work");
                assert!(!list.is_empty())
            }
            for (id, elt) in &elements {
                let list = LinkedList::new(&path, &host).expect("list should be created");
                let read: u8 = list.get(&host, &Hash(*id)).expect("storage should work").expect("element should be present");
                assert_eq!(elt, &read);
            }
        }
    }
}
