// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::store::Store;
use core::slice::{from_raw_parts, from_raw_parts_mut};
use tezos_smart_rollup_core::{MAX_FILE_CHUNK_SIZE, STORE_HASH_SIZE};
use tezos_smart_rollup_host::{
    path::{PathError, RefPath},
    Error,
};

/// Wrapper over `Store` which implements methods equivalent to the host
/// storage functions. It is used for both `MockHost` and for
/// `RollupHostWithInMemoryStorage` in the `entrypoint` crate.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct InMemoryStore(pub(crate) Store);

/// Implementations of the storage host functions in `SmartRollupCore`
/// backed by the in-memory store. See `smart_rollup_core.rs` in the `core`
/// crate for documentation.
#[allow(missing_docs, clippy::missing_safety_doc)]
impl InMemoryStore {
    pub unsafe fn store_has(&self, path: *const u8, path_len: usize) -> i32 {
        let path = from_raw_parts(path, path_len);
        self.handle_store_has(path).unwrap_or_else(Error::code)
    }

    pub unsafe fn store_read(
        &self,
        path: *const u8,
        len: usize,
        offset: usize,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        let path = from_raw_parts(path, len);
        let bytes = self.handle_store_read(path, offset, max_bytes);

        match bytes {
            Ok(bytes) => {
                assert!(bytes.len() <= max_bytes);

                let slice = from_raw_parts_mut(dst, bytes.len());
                slice.copy_from_slice(bytes.as_slice());

                bytes.len().try_into().unwrap()
            }
            Err(e) => e.code(),
        }
    }

    pub unsafe fn store_write(
        &mut self,
        path: *const u8,
        len: usize,
        offset: usize,
        src: *const u8,
        num_bytes: usize,
    ) -> i32 {
        let path = from_raw_parts(path, len);
        let bytes = from_raw_parts(src, num_bytes);

        self.handle_store_write(path, offset, bytes)
            .map(|_| 0)
            .unwrap_or_else(Error::code)
    }

    pub unsafe fn store_delete(&mut self, path: *const u8, len: usize) -> i32 {
        let path = from_raw_parts(path, len);

        self.handle_store_delete(path)
            .map(|_| 0)
            .unwrap_or_else(Error::code)
    }

    pub unsafe fn store_delete_value(&mut self, path: *const u8, len: usize) -> i32 {
        let path = from_raw_parts(path, len);

        self.handle_store_delete_value(path)
            .map(|_| 0)
            .unwrap_or_else(Error::code)
    }

    pub unsafe fn store_list_size(&self, path: *const u8, len: usize) -> i64 {
        let path = from_raw_parts(path, len);

        self.handle_store_list_size(path)
            .unwrap_or_else(|e| e.code() as i64)
    }

    pub unsafe fn store_move(
        &mut self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        let from_path = from_raw_parts(from_path, from_path_len);
        let to_path = from_raw_parts(to_path, to_path_len);

        self.handle_store_move(from_path, to_path)
            .map(|_| 0)
            .unwrap_or_else(Error::code)
    }

    pub unsafe fn store_copy(
        &mut self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        let from_path = from_raw_parts(from_path, from_path_len);
        let to_path = from_raw_parts(to_path, to_path_len);

        self.handle_store_copy(from_path, to_path)
            .map(|_| 0)
            .unwrap_or_else(Error::code)
    }

    pub unsafe fn store_value_size(&self, path: *const u8, path_len: usize) -> i32 {
        let path = from_raw_parts(path, path_len);
        self.handle_store_value_size(path)
            .unwrap_or_else(Error::code)
    }

    pub unsafe fn __internal_store_get_hash(
        &self,
        path: *const u8,
        path_len: usize,
        destination_addr: *mut u8,
        max_size: usize,
    ) -> i32 {
        let path = from_raw_parts(path, path_len);
        let bytes = from_raw_parts_mut(destination_addr, max_size);

        self.handle_internal_store_get_hash(path)
            .map(|hash| {
                let to_write = bytes.len().min(hash.len());
                bytes[..to_write].copy_from_slice(&hash[..to_write]);
                to_write as i32
            })
            .unwrap_or_else(Error::code)
    }
}

fn validate_path(s: &[u8]) -> Result<String, Error> {
    match RefPath::try_from(s) {
        Err(PathError::PathTooLong) => Err(Error::StoreKeyTooLarge),
        Err(_) => Err(Error::StoreInvalidKey),
        Ok(_) => {
            // SAFETY: a valid path is valid UTF-8
            Ok(unsafe { String::from_utf8_unchecked(s.to_vec()) })
        }
    }
}

fn validate_path_maybe_readonly(s: &[u8]) -> Result<String, Error> {
    const READONLY_PATH_SEGMENT: &[u8] = b"/readonly/";
    const READONLY_PREFIX: &[u8] = b"/readonly";
    let to_check = if s.len() > READONLY_PATH_SEGMENT.len()
        && &s[..READONLY_PATH_SEGMENT.len()] == READONLY_PATH_SEGMENT
    {
        // the path has form "/readonly/(.+)", so we check the "/(.+)" part
        &s[READONLY_PREFIX.len()..]
    } else if s == READONLY_PREFIX {
        return Ok(unsafe { String::from_utf8_unchecked(READONLY_PREFIX.to_vec()) });
    } else {
        s
    };
    match RefPath::try_from(to_check) {
        Err(PathError::PathTooLong) => Err(Error::StoreKeyTooLarge),
        Err(_) => Err(Error::StoreInvalidKey),
        Ok(_) => {
            // SAFETY: a valid path is valid UTF-8
            Ok(unsafe { String::from_utf8_unchecked(s.to_vec()) })
        }
    }
}

impl InMemoryStore {
    pub(crate) fn handle_store_list_size(&self, prefix: &[u8]) -> Result<i64, Error> {
        let prefix = validate_path(prefix)?;
        self.0
            .node_from_path(&prefix)
            .map(|n| n.inner.len() as i64)
            .ok_or(Error::StoreNotANode)
    }

    pub(crate) fn handle_store_has(&self, raw_path: &[u8]) -> Result<i32, Error> {
        let path = validate_path(raw_path)?;

        let has_value = self.0.has_entry(&path);
        let has_subvalue = self.handle_store_list_size(raw_path).unwrap_or_default()
            > i64::from(has_value);

        let result = match (has_value, has_subvalue) {
            (false, false) => tezos_smart_rollup_core::VALUE_TYPE_NONE,
            (true, false) => tezos_smart_rollup_core::VALUE_TYPE_VALUE,
            (false, true) => tezos_smart_rollup_core::VALUE_TYPE_SUBTREE,
            (true, true) => tezos_smart_rollup_core::VALUE_TYPE_VALUE_WITH_SUBTREE,
        };

        Ok(result)
    }

    pub(crate) fn handle_store_read(
        &self,
        path: &[u8],
        offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, Error> {
        let path = validate_path_maybe_readonly(path)?;

        if !self.0.has_entry(&path) {
            return Err(Error::StoreNotAValue);
        }

        let bytes = self.0.get_value(&path);
        if offset > bytes.len() {
            return Err(Error::StoreInvalidAccess);
        }

        let num_bytes = usize::min(
            MAX_FILE_CHUNK_SIZE,
            usize::min(max_bytes, bytes.len() - offset),
        );
        let mut value = Vec::with_capacity(num_bytes);

        value.extend_from_slice(&bytes[offset..(offset + num_bytes)]);
        Ok(value)
    }

    pub(crate) fn handle_store_write(
        &mut self,
        path: &[u8],
        offset: usize,
        bytes: &[u8],
    ) -> Result<(), Error> {
        if bytes.len() > MAX_FILE_CHUNK_SIZE {
            return Err(Error::InputOutputTooLarge);
        }

        let path = validate_path(path)?;

        let mut value = match self.0.maybe_get_value(&path) {
            Some(value) => value.as_ref().clone(),
            // No value, so only valid offset is zero (ie writing a new value).
            None => Vec::with_capacity(bytes.len()),
        };

        if offset > value.len() {
            return Err(Error::StoreInvalidAccess);
        } else if offset < value.len() && (offset + bytes.len()) <= value.len() {
            let _ = value
                .splice(offset..(offset + bytes.len()), bytes.iter().copied())
                .collect::<Vec<_>>();
        } else {
            value.truncate(offset);
            value.extend_from_slice(bytes);
        };

        self.0.set_value(&path, value);

        Ok(())
    }

    pub(crate) fn handle_store_delete(&mut self, prefix: &[u8]) -> Result<(), Error> {
        let durable_prefix = validate_path(prefix)?;

        self.0.node_delete(&durable_prefix);
        Ok(())
    }

    pub(crate) fn handle_store_delete_value(
        &mut self,
        prefix: &[u8],
    ) -> Result<(), Error> {
        let durable_prefix = validate_path(prefix)?;

        self.0.delete_value(&durable_prefix);
        Ok(())
    }

    pub(crate) fn handle_store_move(
        &mut self,
        from_path: &[u8],
        to_path: &[u8],
    ) -> Result<(), Error> {
        let from_durable_prefix = validate_path(from_path)?;
        let to_durable_prefix = validate_path(to_path)?;

        let from_node = self
            .0
            .node_from_path(&from_durable_prefix)
            .ok_or(Error::StoreNotANode)?
            .clone();

        self.0.node_delete(&from_durable_prefix);
        self.0.node_insert(&to_durable_prefix, from_node);

        Ok(())
    }

    pub(crate) fn handle_store_copy(
        &mut self,
        from_path: &[u8],
        to_path: &[u8],
    ) -> Result<(), Error> {
        let from_durable_prefix = validate_path(from_path)?;
        let to_durable_prefix = validate_path(to_path)?;

        let from_node = self
            .0
            .node_from_path(&from_durable_prefix)
            .ok_or(Error::StoreNotANode)?
            .clone();

        self.0.node_insert(&to_durable_prefix, from_node);

        Ok(())
    }

    pub(crate) fn handle_store_value_size(&self, path: &[u8]) -> Result<i32, Error> {
        let path = validate_path(path)?;
        if !self.0.has_entry(&path) {
            return Err(Error::StoreNotAValue);
        }
        Ok(self.0.get_value(&path).len() as i32)
    }

    /// Stand in implementation deterministically returns dummy hash
    /// values.
    ///
    /// This will _not_ match the values that the irmin-backed durable
    /// storage in the WASM PVM returns.
    pub(crate) fn handle_internal_store_get_hash(
        &self,
        path: &[u8],
    ) -> Result<[u8; STORE_HASH_SIZE], Error> {
        thread_local! {
            static COUNTER: std::cell::Cell<i32> = const { std::cell::Cell::new(0) };
        }

        let _ = validate_path(path)?;

        let count = COUNTER.with(|counter| {
            let count = counter.get();
            counter.set(count.wrapping_add(1));
            count
        });

        let mut to_hash = count.to_le_bytes().to_vec();
        to_hash.extend_from_slice(path);

        Ok(tezos_crypto_rs::blake2b::digest_256(to_hash.as_slice()))
    }
}
