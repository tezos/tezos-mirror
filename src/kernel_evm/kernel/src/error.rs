// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT
use core::str::Utf8Error;
use evm_execution::{DurableStorageError, EthereumError};
use primitive_types::U256;
use rlp::DecoderError;
use tezos_ethereum::signatures::SigError;
use tezos_smart_rollup_host::path::{OwnedPath, PathError};
use tezos_smart_rollup_host::runtime::RuntimeError;

#[derive(Debug)]
pub enum TransferError {
    InvalidCallerAddress,
    InvalidSignature,
    InvalidNonce { expected: U256, actual: U256 },
    NotEnoughBalance,
    CumulativeGasUsedOverflow,
    InvalidAddressFormat(Utf8Error),
}

#[derive(Debug, Eq, PartialEq)]
pub enum StorageError {
    Path(PathError),
    Runtime(RuntimeError),
    IndexOutOfBounds,
    AccountInitialisation,
    GenesisAccountInitialisation,
    InvalidLoadValue { expected: usize, actual: usize },
    InvalidEncoding { path: OwnedPath, value: Vec<u8> },
}

#[derive(Debug)]
pub enum UpgradeProcessError {
    InvalidUpgradeNonce,
    InternalUpgrade(&'static str),
    NoDictator,
}

#[derive(Debug)]
pub enum Error {
    Transfer(TransferError),
    Storage(StorageError),
    InvalidConversion,
    InvalidParsing,
    InvalidRunTransaction(EthereumError),
    Simulation(EthereumError),
    UpgradeError(UpgradeProcessError),
    InvalidSignature(SigError),
    InvalidSignatureCheck,
}

impl From<PathError> for StorageError {
    fn from(e: PathError) -> Self {
        Self::Path(e)
    }
}
impl From<RuntimeError> for StorageError {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    }
}

impl From<PathError> for Error {
    fn from(e: PathError) -> Self {
        Self::Storage(StorageError::Path(e))
    }
}
impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Self::Storage(StorageError::Runtime(e))
    }
}

impl From<TransferError> for Error {
    fn from(e: TransferError) -> Self {
        Self::Transfer(e)
    }
}

impl From<DecoderError> for Error {
    fn from(_: DecoderError) -> Self {
        Self::InvalidConversion
    }
}

impl From<UpgradeProcessError> for Error {
    fn from(e: UpgradeProcessError) -> Self {
        Self::UpgradeError(e)
    }
}

impl From<StorageError> for Error {
    fn from(e: StorageError) -> Self {
        Self::Storage(e)
    }
}

impl From<DurableStorageError> for Error {
    fn from(e: DurableStorageError) -> Self {
        match e {
            DurableStorageError::PathError(e) => Self::Storage(StorageError::Path(e)),
            DurableStorageError::RuntimeError(e) => {
                Self::Storage(StorageError::Runtime(e))
            }
        }
    }
}
