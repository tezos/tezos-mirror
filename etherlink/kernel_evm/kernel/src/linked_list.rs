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
}
