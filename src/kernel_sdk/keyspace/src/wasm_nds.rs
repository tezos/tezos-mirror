// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![cfg(not(feature = "irmin-compat"))]
#![allow(dead_code)]
#![allow(unused)]

//! Implementation of [`KeySpace`] for [`WasmNds`].
//!
//! # Safety
//!
//! If using this module, it is _required_ that you only interact with
//! `WasmNds` via this module - as it adds an additional structure over the
//! NDS registry.

use std::{mem::MaybeUninit, num::NonZeroUsize, rc::Rc};

use crate::{KeySpace, KeySpaceLoader, KeySpaceLoaderError};

use tezos_smart_rollup_host::{
    storage::v2::{NdsError, RegistryResizeRequest, WasmNds},
    wasm::WasmHost,
};

/// The zero-index db is reserved for [`Name`] to `db_index` mapping.
const NAME_DB: usize = 0;

/// Initialise a [`KeySpaceLoader`], backed by a [`WasmNds`] impl.
pub trait WasmHostNds: WasmHost {
    /// Initialise the loader. May fail - for example if the `nds` feature
    /// has not yet been activated.
    fn init(&self) -> Result<impl KeySpaceLoader, NdsError>;
}

/// Wrapper around a [`WasmNds`] impl.
#[derive(Debug)]
struct NdsKeySpaceLoader<Handle: WasmNds> {
    /// A registry, of size at least one.
    ///
    /// The `Nds` feature can be assumed to be enabled when accessing
    /// this field.
    registry: Rc<Handle>,
}

impl<Handle: WasmNds> NdsKeySpaceLoader<Handle> {
    /// Setup the nds-backed KeySpace loader.
    ///
    /// Ensures the registry has size at least one,
    /// and that the `nds` feature has been enabled for this
    /// `WasmNds` impl.
    fn new(registry: Handle) -> Result<Self, NdsError> {
        if registry.is_empty()? {
            registry.resize(RegistryResizeRequest::Increment)?;
        }

        Ok(Self {
            registry: Rc::new(registry),
        })
    }
}

impl<Handle: WasmNds> KeySpaceLoader for NdsKeySpaceLoader<Handle> {
    type KeySpace = WasmNdsKeySpace<Handle>;

    fn load_or_create(
        &mut self,
        name: crate::Name,
    ) -> Result<Self::KeySpace, KeySpaceLoaderError> {
        const INDEX_SIZE: usize = core::mem::size_of::<u32>();

        let mut index_buf = [MaybeUninit::uninit(); INDEX_SIZE];
        match self.registry.store_read(NAME_DB, name.as_key(), 0, index_buf.as_mut_slice()) {
            Ok(INDEX_SIZE) => {
                // SAFETY: index_buf is guaranteed to be fully initialised at this point.
                let index_buf = index_buf.map(|byte| unsafe { MaybeUninit::assume_init(byte) });

                // all platforms used are either 32 or 64 bit - this conversion is lossless
                let db_index = u32::from_le_bytes(index_buf)
                    as usize;

                let Some(db_index) = NonZeroUsize::new(db_index) else {
                    return Err(KeySpaceLoaderError::InconsistentNameMapping(name));
                };

                return Ok(WasmNdsKeySpace {
                    registry: self.registry.clone(),
                    db_index
                });
            }
            Ok(_size) => return Err(KeySpaceLoaderError::InconsistentNameMapping(name)),
            Err(NdsError::KeyNotFound) => {
                // mapping not found, initialises below
            },
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled by `NdsKeySpaceLoader::new`"),
            Err(NdsError::DatabaseOutOfBounds) => unreachable!("Nds guaranteed to contain at least one database by `NdsKeySpaceLoader::new`"),
            Err(NdsError::ResizeInvalid) => unreachable!("Not an NDS resize request"),
            Err(NdsError::InputOutputTooLarge) => unreachable!("{INDEX_SIZE} < MAX_FILE_CHUNK_SIZE"),
            Err(NdsError::OffsetTooLarge) => unreachable!("Offset is zero"),
            Err(NdsError::StoreValueSizeExceeded) => unreachable!("Not a write request")
        };

        // allocate a new databases for this name
        //
        // *warning* - there doesn't currently exist a way to deallocate a name -> index mapping
        // this will need to be addressed subsequently.
        // ultimately, the correct fix is that `KeySpace::clear` should take the argument by self,
        // allowing to drop the db, and clear the mapping simultaneously.
        let idx = match self.registry.resize(RegistryResizeRequest::Increment) {
            // cannot underflow - as size >= 2 as this point.
            Ok(size) => size - 1,
            Err(NdsError::ResizeInvalid) => {
                return Err(KeySpaceLoaderError::TooManyDatabases)
            }
            Err(NdsError::NdsDisabled) => unreachable!(
                "Nds is guaranteed to be enabled by `NdsKeySpaceLoader::new`"
            ),
            Err(
                NdsError::DatabaseOutOfBounds
                | NdsError::KeyNotFound
                | NdsError::InputOutputTooLarge
                | NdsError::OffsetTooLarge
                | NdsError::StoreValueSizeExceeded,
            ) => unreachable!("Error variants irrelevant to registry resize"),
        };

        match self.registry
            .store_set(
                NAME_DB,
                name.as_key(),
                u32::to_le_bytes(idx as u32).as_slice(),
            ) {
                Ok(()) => {},
                Err(NdsError::NdsDisabled) => unreachable!(
                    "Nds is guaranteed to be enabled by `NdsKeySpaceLoader::new`"
                ),
                Err(NdsError::DatabaseOutOfBounds) => unreachable!("Nds guaranteed to contain at least one database by `NdsKeySpaceLoader::new`"),
                Err(NdsError::InputOutputTooLarge) => unreachable!("{INDEX_SIZE} < MAX_FILE_CHUNK_SIZE"),
                Err(NdsError::StoreValueSizeExceeded) => unreachable!("Maximum size that can be set in one go is less than maximum nds value size"),
                Err(NdsError::ResizeInvalid | NdsError::KeyNotFound | NdsError::OffsetTooLarge) => unreachable!("Error variants irrelevant to store_set"),
            }

        let db_index = NonZeroUsize::new(idx).expect("New registry size after a succesful 'increment resize request' >= 2 => db_index >= 1");

        Ok(WasmNdsKeySpace {
            registry: self.registry.clone(),
            db_index,
        })
    }
}

/// [`KeySpace`] implementation backend by a [`WasmNds`] database.
pub struct WasmNdsKeySpace<Handle: WasmNds> {
    /// Internal registry handle. Must only be used for `store_` methods in conjuction with the below DB index.
    ///
    /// Nds is guaranteed to be enabled when accessing this field.
    registry: Rc<Handle>,
    /// Database index for this KeySpace index. All uses of this in conjuction with `store_` methods are guaranteed to
    /// *never* return an `DatabasesOutOfBounds` error.
    db_index: NonZeroUsize,
}

impl<Handle: WasmNds> KeySpace for WasmNdsKeySpace<Handle> {
    fn get(&self, key: &crate::Key) -> Option<Vec<u8>> {
        todo!()
    }

    fn read(&self, key: &crate::Key, offset: usize, buffer: &mut [u8]) -> Option<usize> {
        todo!()
    }

    fn set(
        &mut self,
        key: &crate::Key,
        value: impl AsRef<[u8]>,
    ) -> Result<(), crate::KeySpaceWriteError> {
        todo!()
    }

    fn write(
        &mut self,
        key: &crate::Key,
        offset: usize,
        data: impl AsRef<[u8]>,
    ) -> Result<usize, crate::KeySpaceWriteError> {
        todo!()
    }

    fn value_length(&self, key: &crate::Key) -> Option<usize> {
        todo!()
    }

    fn contains(&self, key: &crate::Key) -> bool {
        todo!()
    }

    fn delete(&mut self, key: &crate::Key) -> bool {
        todo!()
    }

    fn clear(&mut self) {
        todo!()
    }

    fn copy_from(&mut self, other: &Self) {
        todo!()
    }

    fn move_from(&mut self, other: &mut Self) {
        todo!()
    }

    fn hash(&self) -> Vec<u8> {
        todo!()
    }
}
