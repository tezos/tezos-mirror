// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use rlp::DecoderError;
use tezos_smart_rollup_host::path::PathError;
use tezos_smart_rollup_host::runtime::RuntimeError;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum Error {
    #[error(transparent)]
    Path(PathError),
    #[error(transparent)]
    Runtime(RuntimeError),
    #[error(transparent)]
    Storage(tezos_smart_rollup_storage::StorageError),
    #[error("Failed to decode: {0}")]
    RlpDecoderError(DecoderError),
    #[error("Storage error: error while reading a value (incorrect size). Expected {expected} but got {actual}")]
    InvalidLoadValue { expected: usize, actual: usize },
}

impl From<PathError> for Error {
    fn from(e: PathError) -> Self {
        Self::Path(e)
    }
}
impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    }
}

impl From<DecoderError> for Error {
    fn from(e: DecoderError) -> Self {
        Self::RlpDecoderError(e)
    }
}
