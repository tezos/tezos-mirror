// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::runtime::RuntimeError;
use thiserror::Error;

/// Context required for creating aliases
#[derive(Clone, Debug)]
pub struct AliasCreationContext {
    /// Gas limit
    pub gas_limit: u64,
    /// Timestamp for the block
    pub timestamp: U256,
    /// Block number
    pub block_number: U256,
}

pub enum CrossCallResult {
    /// execution completed normally, return data is the output
    Success(Vec<u8>),
    /// execution explicitly reverted (Solidity revert() / require()), return data is the error message/selector. Gas not consumed is refunded. State changes are rolled back.
    Revert(Vec<u8>),
    /// execution stopped abnormally (out of gas, invalid opcode, stack overflow, etc.). All gas is consumed. State changes are rolled back.
    Halt(Vec<u8>),
}

#[derive(Eq, PartialEq, Debug, Error)]
pub enum TezosXRuntimeError {
    #[error("RuntimeId not in registry")]
    RuntimeNotFound(RuntimeId),
    #[error("Conversion error: {0}")]
    ConversionError(String),
    #[error("Runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("Storage error: {0}")]
    Storage(#[from] tezos_storage::error::Error),
    #[error("Path error: {0}")]
    Path(#[from] tezos_smart_rollup_host::path::PathError),
    #[error("Custom error: {0}")]
    Custom(String),
}
pub trait Registry {
    fn bridge<Host: Runtime>(
        &self,
        host: &mut Host,
        destination_runtime: RuntimeId,
        destination_address: &[u8],
        source_address: &[u8],
        amount: U256,
        data: &[u8],
    ) -> Result<CrossCallResult, TezosXRuntimeError>;

    fn generate_alias<Host: Runtime>(
        &self,
        host: &mut Host,
        native_address: &[u8],
        runtime_id: RuntimeId,
        context: AliasCreationContext,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;

    fn address_from_string(
        &self,
        address_str: &str,
        runtime_id: RuntimeId,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;
}

pub trait RuntimeInterface {
    /// Generate an alias address for a native address in this runtime.
    ///
    /// This function creates an alias that allows the native address to receive
    /// funds in this runtime and have them forwarded to the original address.
    fn generate_alias<Host: Runtime>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        native_address: &[u8],
        context: AliasCreationContext,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;

    // This is a just a placeholder for now to show how the
    //interface would look like.
    // TODO: Probably need to pass and return more data to
    // initialize a real operation when we will implement this.
    // Amounts should have been subtracted from the sender
    // before calling this function.
    fn call<Host: Runtime>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        from: &[u8],
        to: &[u8],
        amount: U256,
        data: &[u8],
    ) -> Result<CrossCallResult, TezosXRuntimeError>;

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError>;

    #[cfg(feature = "testing")]
    fn string_from_address(&self, address: &[u8]) -> Result<String, TezosXRuntimeError>;

    #[cfg(feature = "testing")]
    fn get_balance<Host: Runtime>(
        &self,
        host: &mut Host,
        address: &[u8],
    ) -> Result<U256, TezosXRuntimeError>;
}

#[derive(Eq, PartialEq, Hash, Debug)]
pub enum RuntimeId {
    Tezos = 0,
    Ethereum = 1,
}

impl From<RuntimeId> for u8 {
    fn from(value: RuntimeId) -> Self {
        match value {
            RuntimeId::Tezos => 0,
            RuntimeId::Ethereum => 1,
        }
    }
}

impl TryFrom<u8> for RuntimeId {
    type Error = &'static str;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(RuntimeId::Tezos),
            1 => Ok(RuntimeId::Ethereum),
            _ => Err("Invalid RuntimeId value"),
        }
    }
}
