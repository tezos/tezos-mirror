// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_indexable_storage::IndexableStorageError;
use tezos_smart_rollup_keyspace::{KeyError, KeySpaceError};
use thiserror::Error;

/// Failures raised while the inspectors persist their output to durable storage.
#[derive(Error, Debug, PartialEq, Eq)]
pub enum InspectorError {
    #[error(transparent)]
    KeySpace(#[from] KeySpaceError),
    #[error(transparent)]
    Key(#[from] KeyError),
    #[error("Indexable storage push failed: {0}")]
    IndexableStorage(#[from] IndexableStorageError),
}
