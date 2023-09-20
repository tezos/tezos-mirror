// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Transaction layers
//!
//! Transactional storage is kept in layers - one on top of another. The bottom layer
//! holds the "original" values for each account.
//!
//! Each time a transaction is created with `begin_transaction`, a new layer
//! is added to the top of the layer stack. `rollback_transaction` will delete
//! the top layer, and `commit_transaction` moves the top layer back into the
//! previous one (overwriting it).
//!
//! This allows transactions to be arbitrarily nested.

use crate::StorageError;
use core::marker::PhantomData;
use tezos_smart_rollup_host::path::{concat, OwnedPath, Path};
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError, ValueType};

pub(crate) struct Layer<T: From<OwnedPath>> {
    pub(crate) path: OwnedPath,
    phantom: PhantomData<T>,
}

fn has_subtree_res(v: Result<Option<ValueType>, RuntimeError>) -> bool {
    use ValueType::*;

    matches!(v, Ok(Some(Subtree | ValueWithSubtree)))
}

impl<T: From<OwnedPath>> Layer<T> {
    /// Create layer for the path given.
    ///
    /// Create a new layer object with all account data stored under path given, ie,
    /// creating a Layer for path "/accounts" will assume "/accounts/alph" will
    /// contain all data for account with id "alpha".
    ///
    /// This function is used solely for creating the bottom layer of the storage.
    pub(crate) fn with_path(name: &impl Path) -> Self {
        Self {
            path: OwnedPath::from(name),
            phantom: PhantomData,
        }
    }

    /// Create layer on top of "self" layer.
    ///
    /// Create a new transaction layer which is a copy of the layer below it. Such
    /// layers should be either discarded or consumed using respective functions below.
    ///
    /// There is no check regarding neither the existence of `self`, nor if the
    /// `name` points to an already used path in the durable storage. It is the
    /// responsability of the caller to do so.
    pub(crate) fn force_make_copy(
        &self,
        host: &mut impl Runtime,
        name: &impl Path,
    ) -> Result<Self, StorageError> {
        let copy = Self {
            path: OwnedPath::from(name),
            phantom: PhantomData,
        };

        match host.store_copy(&self.path, &copy.path) {
            Ok(()) | Err(RuntimeError::PathNotFound) => Ok(copy),
            Err(e) => Err(e.into()),
        }
    }

    /// Merge changes from given layer into self layer.
    ///
    /// Consume a layer above the "self" layer. This is the same as applying all changes
    /// done in the layer above. This assumes that the layer above was created with the
    /// [make_copy] function.
    pub(crate) fn consume(
        &mut self,
        host: &mut impl Runtime,
        layer: Layer<T>,
    ) -> Result<(), StorageError> {
        if let Ok(Some(_)) = host.store_has(&layer.path) {
            // The layer we consume has content, so move it
            host.store_move(&layer.path, &self.path)
                .map_err(StorageError::from)
        } else if let Ok(Some(_)) = host.store_has(&self.path) {
            // The layer we consume has no content, so delete the "self" layer
            // as it should equal the consumed layer after this call
            host.store_delete(&self.path).map_err(StorageError::from)
        } else {
            // Both self layer and consumed layer are empty, so do nothing
            Ok(())
        }
    }

    /// Create new, empty stored object
    ///
    /// Create a new object in the current layer. Note that the data will only be written
    /// when the object does so.
    pub(crate) fn create_new(
        &mut self,
        host: &impl Runtime,
        id: &impl Path,
    ) -> Result<Option<T>, StorageError> {
        let account_path = concat(&self.path, id)?;

        if has_subtree_res(host.store_has(&account_path)) {
            Ok(None)
        } else {
            Ok(Some(T::from(account_path)))
        }
    }

    /// Get existing object
    ///
    /// Get an object from the current layer. This checks that the data actually exists, ie,
    /// that there is some data in durable storage for the object in this layer.
    pub(crate) fn get(
        &self,
        host: &impl Runtime,
        id: &impl Path,
    ) -> Result<Option<T>, StorageError> {
        let account_path = concat(&self.path, id)?;

        if has_subtree_res(host.store_has(&account_path)) {
            Ok(Some(T::from(account_path)))
        } else {
            Ok(None)
        }
    }

    /// Get or create a new object
    ///
    /// If no object exists at the path given, then create a new object and return that
    /// instead. (Use case).
    pub(crate) fn get_or_create(
        &self,
        _host: &impl Runtime,
        id: &impl Path,
    ) -> Result<T, StorageError> {
        // We could get rid of the host parameter, but in the future, it would be nice
        // if we had the option of interacting with storage when creating an object.
        let account_path = concat(&self.path, id)?;
        Ok(T::from(account_path))
    }

    /// Delete existing object
    ///
    /// Delete an object in the current layer. This updates the current layer in
    /// storage.
    pub(crate) fn delete(
        &mut self,
        host: &mut impl Runtime,
        id: &impl Path,
    ) -> Result<(), StorageError> {
        let account_path = concat(&self.path, id)?;

        host.store_delete(&account_path).map_err(StorageError::from)
    }

    /// Delete current layer
    ///
    /// Discard the current/self layer. This is the same as cancelling the
    /// current transaction.
    pub(crate) fn discard(self, host: &mut impl Runtime) -> Result<(), StorageError> {
        if let Ok(Some(_)) = host.store_has(&self.path) {
            host.store_delete(&self.path).map_err(StorageError::from)
        } else {
            Ok(())
        }
    }
}
