// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_indexable_storage::IndexableStorageError;
use tezos_smart_rollup_host::{path::PathError, runtime::RuntimeError};
use thiserror::Error;

/// Failures raised while the inspectors persist their output to durable storage.
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum InspectorError {
    #[error("Runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("Path error: {0}")]
    Path(#[from] PathError),
    #[error("Indexable storage push failed: {0}")]
    IndexableStorage(#[from] IndexableStorageError),
}
