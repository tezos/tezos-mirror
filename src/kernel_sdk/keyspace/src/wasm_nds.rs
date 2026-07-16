// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![cfg(all(pvm_kind = "wasm", not(feature = "irmin-compat")))]

//! Implementation of [`KeySpace`] for [`WasmNds`].
//!
//! # Safety
//!
//! If using this module, it is _required_ that you only interact with
//! `WasmNds` via this module - as it adds an additional structure over the
//! NDS registry.

use std::{mem::MaybeUninit, num::NonZeroUsize, rc::Rc};

use crate::{KeySpace, KeySpaceLoader, KeySpaceLoaderError, KeySpaceWriteError};

use tezos_smart_rollup_constants::core::MAX_FILE_CHUNK_SIZE;
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
    ///
    /// # Safety
    ///
    /// If this call succeeds, it _must_ be the case that:
    /// - the WASM NDS feature flag has been enabled.
    /// - the underlying registry has size _at least 1_.
    unsafe fn init(&self) -> Result<impl KeySpaceLoader, NdsError>;
}

/// WASM PVM-specific wiring up for [`WasmNds`].
// TODO (SDK-147): add mock-host/native support for `WasmNds` for unit testing etc
#[cfg(pvm_kind = "wasm")]
impl WasmHostNds for tezos_smart_rollup_core::rollup_host::RollupHost {
    unsafe fn init(&self) -> Result<impl KeySpaceLoader, NdsError> {
        NdsKeySpaceLoader::new(tezos_smart_rollup_host::storage::v2::WasmNdsHandle)
    }
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
                    db_index,
                    name,
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
            name,
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
    /// The name this key space was loaded under.
    name: crate::Name,
}

impl<Handle: WasmNds> WasmNdsKeySpace<Handle> {
    /// Read into buffer, from the given offset of the value mapped to by `key`.
    ///
    /// Will only read up to [`MAX_FILE_CHUNK_SIZE`] at a time.
    /// Returns the number of bytes read, which will never be more than `buffer.len()`.
    ///
    /// # Panics
    ///
    /// Panics if the key does not exist in the current keyspace, or if `offset > value_length`.
    fn read_from(
        &self,
        key: &crate::Key,
        offset: usize,
        buffer: &mut [MaybeUninit<u8>],
    ) -> usize {
        let num_bytes = buffer.len().min(MAX_FILE_CHUNK_SIZE);

        match self.registry.store_read(
            self.db_index.get(),
            key.as_ref(),
            offset,
            &mut buffer[..num_bytes],
        ) {
            Ok(num_read) => num_read,
            Err(NdsError::KeyNotFound) => panic!("caller violation - key not found"),
            Err(NdsError::OffsetTooLarge) => {
                panic!("caller violation - offset too large")
            }
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled"),
            Err(NdsError::DatabaseOutOfBounds) => {
                unreachable!("db_idx guaranteed to be within registry size")
            }
            Err(NdsError::InputOutputTooLarge) => {
                unreachable!("Reads <= MAX_FILE_CHUNK_SIZE in size are allowed")
            }
            Err(NdsError::ResizeInvalid | NdsError::StoreValueSizeExceeded) => {
                unreachable!("Irrelevant error variants")
            }
        }
    }

    /// Write to the key, from the given offset of the value mapped to by `key`, where
    /// contents is from the start of the buffer.
    fn write_from(
        &self,
        key: &crate::Key,
        mut offset: usize,
        mut buffer: &[u8],
    ) -> Result<(), KeySpaceWriteError> {
        while !buffer.is_empty() {
            let num_bytes = buffer.len().min(MAX_FILE_CHUNK_SIZE);
            let (to_write, rest) = buffer.split_at(num_bytes);

            match self.registry.store_write(
                self.db_index.get(),
                key.as_ref(),
                offset,
                to_write,
            ) {
                Ok(()) => {
                    offset = offset.checked_add(num_bytes).expect("The maximum size of values allowed in NDS is less than usize::MAX");
                    buffer = rest;
                    continue;
                }
                Err(NdsError::OffsetTooLarge) => {
                    return Err(KeySpaceWriteError::InvalidOffset);
                }
                Err(NdsError::StoreValueSizeExceeded) => {
                    return Err(KeySpaceWriteError::ValueSizeExceeded)
                }
                Err(NdsError::NdsDisabled) => {
                    unreachable!("Nds guaranteed to be enabled")
                }
                Err(NdsError::DatabaseOutOfBounds) => {
                    unreachable!("db_idx guaranteed to be within registry size")
                }
                Err(NdsError::InputOutputTooLarge) => {
                    unreachable!("Writes <= MAX_FILE_CHUNK_SIZE in size are allowed")
                }
                Err(NdsError::KeyNotFound | NdsError::ResizeInvalid) => {
                    unreachable!("Irrelevant error variants")
                }
            }
        }

        Ok(())
    }
}

impl<Handle: WasmNds> KeySpace for WasmNdsKeySpace<Handle> {
    fn name(&self) -> &crate::Name {
        &self.name
    }

    fn get(&self, key: &crate::Key) -> Option<Vec<u8>> {
        let value_size = self.value_length(key)?;

        let mut value = Vec::with_capacity(value_size);
        let mut len = value.len();

        while len < value_size {
            let read = self.read_from(key, len, value.spare_capacity_mut());

            len = len
                .checked_add(read)
                .expect("Values in NDS are capped to a value smaller than usize::MAX");

            // on each iteration, an additional `read` bytes are guaranteed to be initialised.
            unsafe { value.set_len(len) }
        }

        Some(value)
    }

    fn read(&self, key: &crate::Key, offset: usize, buffer: &mut [u8]) -> Option<usize> {
        let value_size = self.value_length(key)?;

        // technically NDS supports reading from an offset exactly the
        // value size, but StorageV1 would error here. For compatibility, we should probably ensure
        // these behave the same.
        if offset >= value_size {
            return None;
        }

        // SAFETY: `MaybeUninit` is transparent over its contents, allowing this cast to be safe.
        let num_read = self.read_from(key, offset, unsafe {
            std::mem::transmute::<&mut [u8], &mut [MaybeUninit<u8>]>(buffer)
        });

        Some(num_read)
    }

    fn set(
        &mut self,
        key: &crate::Key,
        value: impl AsRef<[u8]>,
    ) -> Result<(), crate::KeySpaceWriteError> {
        let value = value.as_ref();
        let (initial_set, rest) = value.split_at(value.len().min(MAX_FILE_CHUNK_SIZE));

        match self
            .registry
            .store_set(self.db_index.get(), key.as_ref(), initial_set)
        {
            Ok(()) => {}
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled"),
            Err(NdsError::DatabaseOutOfBounds) => {
                unreachable!("db_idx guaranteed to be within registry size")
            }
            Err(NdsError::InputOutputTooLarge) => {
                unreachable!("Set of <= MAX_FILE_CHUNK_SIZE guaranteed to succeed")
            }
            Err(
                NdsError::ResizeInvalid
                | NdsError::KeyNotFound
                | NdsError::OffsetTooLarge
                | NdsError::StoreValueSizeExceeded,
            ) => unreachable!("Irrelevant error variants"),
        }

        self.write_from(key, initial_set.len(), rest)
    }

    fn write(
        &mut self,
        key: &crate::Key,
        offset: usize,
        data: impl AsRef<[u8]>,
    ) -> Result<usize, crate::KeySpaceWriteError> {
        let data = data.as_ref();

        self.write_from(key, offset, data).map(|()| data.len())
    }

    fn value_length(&self, key: &crate::Key) -> Option<usize> {
        match self
            .registry
            .store_value_size(self.db_index.get(), key.as_ref())
        {
            Ok(size) => Some(size),
            Err(NdsError::KeyNotFound) => None,
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled"),
            Err(NdsError::DatabaseOutOfBounds) => {
                unreachable!("db_idx guaranteed to be within registry size")
            }
            Err(
                NdsError::ResizeInvalid
                | NdsError::InputOutputTooLarge
                | NdsError::OffsetTooLarge
                | NdsError::StoreValueSizeExceeded,
            ) => unreachable!("Irrelevant error variants"),
        }
    }

    fn contains(&self, key: &crate::Key) -> bool {
        match self
            .registry
            .store_exists(self.db_index.get(), key.as_ref())
        {
            Ok(exists) => exists,
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled"),
            Err(NdsError::DatabaseOutOfBounds) => {
                unreachable!("db_idx guaranteed to be within registry size")
            }
            Err(
                NdsError::ResizeInvalid
                | NdsError::InputOutputTooLarge
                | NdsError::KeyNotFound
                | NdsError::OffsetTooLarge
                | NdsError::StoreValueSizeExceeded,
            ) => unreachable!("Irrelevant error variants"),
        }
    }

    fn delete(&mut self, key: &crate::Key) -> bool {
        if !self.contains(key) {
            return false;
        }

        match self
            .registry
            .store_delete(self.db_index.get(), key.as_ref())
        {
            Ok(()) => true,
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled"),
            Err(NdsError::DatabaseOutOfBounds) => {
                unreachable!("db_idx guaranteed to be within registry size")
            }
            Err(
                NdsError::ResizeInvalid
                | NdsError::InputOutputTooLarge
                | NdsError::KeyNotFound
                | NdsError::OffsetTooLarge
                | NdsError::StoreValueSizeExceeded,
            ) => unreachable!("Irrelevant error variants"),
        }
    }

    // TODO (SDK-148): enable dropping the underlying mapping to reclaim db allocations
    fn clear(&mut self) {
        match self.registry.clear_db(self.db_index.get()) {
            Ok(()) => (),
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled"),
            Err(NdsError::DatabaseOutOfBounds) => {
                unreachable!("db_idx guaranteed to be within registry size")
            }
            Err(
                NdsError::ResizeInvalid
                | NdsError::InputOutputTooLarge
                | NdsError::KeyNotFound
                | NdsError::OffsetTooLarge
                | NdsError::StoreValueSizeExceeded,
            ) => unreachable!("Irrelevant error variants"),
        }
    }

    fn copy_from(&mut self, other: &Self) {
        match self
            .registry
            .copy_db(other.db_index.get(), self.db_index.get())
        {
            Ok(()) => (),
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled"),
            Err(NdsError::DatabaseOutOfBounds) => {
                unreachable!("db_idx guaranteed to be within registry size")
            }
            Err(
                NdsError::ResizeInvalid
                | NdsError::InputOutputTooLarge
                | NdsError::KeyNotFound
                | NdsError::OffsetTooLarge
                | NdsError::StoreValueSizeExceeded,
            ) => unreachable!("Irrelevant error variants"),
        }
    }

    fn move_from(&mut self, other: &mut Self) {
        match self
            .registry
            .move_db(other.db_index.get(), self.db_index.get())
        {
            Ok(()) => (),
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled"),
            Err(NdsError::DatabaseOutOfBounds) => {
                unreachable!("db_idx guaranteed to be within registry size")
            }
            Err(
                NdsError::ResizeInvalid
                | NdsError::InputOutputTooLarge
                | NdsError::KeyNotFound
                | NdsError::OffsetTooLarge
                | NdsError::StoreValueSizeExceeded,
            ) => unreachable!("Irrelevant error variants"),
        }
    }

    fn hash(&self) -> Vec<u8> {
        match self.registry.hash_db(self.db_index.get()) {
            Ok(hash) => hash.to_vec(),
            Err(NdsError::NdsDisabled) => unreachable!("Nds guaranteed to be enabled"),
            Err(NdsError::DatabaseOutOfBounds) => {
                unreachable!("db_idx guaranteed to be within registry size")
            }
            Err(
                NdsError::ResizeInvalid
                | NdsError::InputOutputTooLarge
                | NdsError::KeyNotFound
                | NdsError::OffsetTooLarge
                | NdsError::StoreValueSizeExceeded,
            ) => unreachable!("Irrelevant error variants"),
        }
    }
}
