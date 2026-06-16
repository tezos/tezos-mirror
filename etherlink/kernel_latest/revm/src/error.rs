// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use evm_types::{CustomPrecompileError, IntoWithRemainder};
use revm::{
    bytecode::BytecodeDecodeError,
    context::{result::EVMError, tx::TxEnvBuildError},
    database_interface::DBErrorMarker,
    interpreter::Gas,
    primitives::{B256, U256},
};
use tezos_indexable_storage::IndexableStorageError;
use tezos_smart_rollup_host::{path::PathError, runtime::RuntimeError};
use tezosx_interfaces::TezosXRuntimeError;
use thiserror::Error;

/// Failures from the EVM database when reading or writing storage
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum EvmDbError {
    #[error("Runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("Path error: {0}")]
    Path(#[from] PathError),
    #[error("Bytecode at {hash} failed validation: {source}")]
    InvalidBytecode {
        hash: B256,
        source: BytecodeDecodeError,
    },
    #[error(
        "Block hash entry for block {block_number} is malformed (length {actual_length})"
    )]
    MalformedBlockHash {
        block_number: u64,
        actual_length: usize,
    },
    #[error("Bytes from storage had length {actual}, expected {expected}")]
    UnexpectedBytesLength { expected: usize, actual: usize },
    #[error("Account balance overflow: balance={current}, amount={amount}")]
    BalanceOverflow { current: U256, amount: U256 },
    #[error("Account balance underflow: balance={current}, amount={amount}")]
    BalanceUnderflow { current: U256, amount: U256 },
    #[error("Commit ended in an inconsistent state")]
    CommitMismatch,
    #[error("Origin storage: {0}")]
    Origin(OriginStorageError),
}

impl DBErrorMarker for EvmDbError {}

/// Failures from origin storage reads and writes.
///
/// The origin path is the only storage location reachable from an EVM
/// precompile whose writes bypass the journal. State set there persists
/// even when the surrounding precompile reverts. This dedicated type
/// lets callers lift it into either EvmDbError or CustomPrecompileError
/// depending on context.
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum OriginStorageError {
    #[error("Runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("Path error: {0}")]
    Path(#[from] PathError),
    #[error("decoding Origin failed: {0}")]
    Decode(String),
    #[error("remaining bytes after decoding Origin")]
    DecodeTrailing,
    #[error("encoding Origin failed: {0}")]
    Encode(String),
}

impl From<OriginStorageError> for EvmDbError {
    fn from(value: OriginStorageError) -> Self {
        match value {
            OriginStorageError::Runtime(e) => EvmDbError::Runtime(e),
            OriginStorageError::Path(e) => EvmDbError::Path(e),
            other => EvmDbError::Origin(other),
        }
    }
}

impl IntoWithRemainder for OriginStorageError {
    fn into_with_remainder(self, gas: Gas) -> CustomPrecompileError {
        match self {
            OriginStorageError::Runtime(e) => e.into_with_remainder(gas),
            other => CustomPrecompileError::Revert(other.to_string(), gas),
        }
    }
}

/// Failures raised by the kernel around an EVM call:
/// - env construction
/// - fees
/// - parsing
/// - encoding
/// - validation
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum EvmKernelError {
    #[error("Runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("Path error: {0}")]
    Path(#[from] PathError),
    #[error("Base fee per gas does not fit in u64")]
    BaseFeePerGasOverflow,
    #[error("Gas for fees does not fit in u64")]
    GasForFeesOverflow,
    #[error("Insufficient gas to cover the non-execution fees (gas_limit={gas_limit}, gas_for_fees={gas_for_fees})")]
    GasToFeesUnderflow { gas_limit: u64, gas_for_fees: u64 },
    #[error("FA ticket creator encoding failed")]
    TicketCreatorEncoding,
    #[error("FA ticket contents encoding failed")]
    TicketContentsEncoding,
    #[error("FA ticket amount does not fit in U256")]
    TicketAmountOverflow,
    #[error("FA deposit routing info has an unsupported length ({actual})")]
    InvalidRoutingInfoLength { actual: usize },
    #[error("Deposit receiver address conversion failed")]
    DepositReceiverConversion,
    #[error("Authorization list cannot be empty per EIP-7702")]
    EmptyAuthorizationList,
    #[error("Access list has already been set for this context")]
    AccessListAlreadySet,
    #[error("Failed to build the transaction environment: {0}")]
    TxEnvBuild(#[from] TxEnvBuildError),
    #[error("Indexable storage push failed: {0}")]
    IndexableStorage(#[from] IndexableStorageError),
}

/// Top-level error from an EVM transaction run
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum EvmRunError {
    #[error(transparent)]
    Kernel(#[from] EvmKernelError),
    /// Error originated by a DB call made from revm during execution
    #[error(transparent)]
    RevmDB(#[from] EVMError<EvmDbError>),
    /// Failure from a DB call made outside the VM
    #[error(transparent)]
    DB(#[from] EvmDbError),
}

impl From<EvmDbError> for TezosXRuntimeError {
    fn from(value: EvmDbError) -> Self {
        match value {
            EvmDbError::Runtime(err) => TezosXRuntimeError::Runtime(err),
            other => TezosXRuntimeError::Custom(other.to_string()),
        }
    }
}

impl From<EvmKernelError> for TezosXRuntimeError {
    fn from(value: EvmKernelError) -> Self {
        match value {
            EvmKernelError::Runtime(err) => TezosXRuntimeError::Runtime(err),
            other => TezosXRuntimeError::Custom(other.to_string()),
        }
    }
}
