// SPDX-FileCopyrightText: 2022,2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pub mod tracer {
    use host::{
        path::{OwnedPath, RefPath},
        runtime::RuntimeError,
    };

    use primitive_types::H256;
    use tezos_evm_logging::log;
    use tezos_evm_logging::Level::Debug;
    use tezos_evm_runtime::runtime::Runtime;
    use tezos_indexable_storage::{IndexableStorage, IndexableStorageError};
    use tezos_smart_rollup_host::path::*;
    use thiserror::Error;

    use crate::trace::CallTrace;
    use crate::trace::StructLog;

    const EVM_TRACE: RefPath = RefPath::assert_from(b"/evm/trace");

    const GAS: RefPath = RefPath::assert_from(b"/gas");
    const FAILED: RefPath = RefPath::assert_from(b"/failed");
    const RETURN_VALUE: RefPath = RefPath::assert_from(b"/return_value");
    const STRUCT_LOGS: RefPath = RefPath::assert_from(b"/struct_logs");

    #[derive(Eq, Error, Debug, PartialEq)]
    pub enum Error {
        #[error("Error from the indexable storage while tracing: {0}")]
        IndexableStorageError(#[from] IndexableStorageError),
        #[error("Error from runtime while tracing: {0}")]
        RuntimeError(#[from] RuntimeError),
        #[error("Error from path while tracing: {0}")]
        PathError(#[from] PathError),
    }

    pub fn trace_tx_path(
        hash: &Option<H256>,
        field: &RefPath,
    ) -> Result<OwnedPath, Error> {
        let trace_tx_path = match hash {
            None => EVM_TRACE.into(),
            Some(hash) => {
                let hash = hex::encode(hash);
                let raw_tx_path: Vec<u8> = format!("/{}", &hash).into();
                let tx_path = OwnedPath::try_from(raw_tx_path)?;
                concat(&EVM_TRACE, &tx_path)?
            }
        };
        concat(&trace_tx_path, field).map_err(Error::PathError)
    }

    pub fn store_trace_gas<Host: Runtime>(
        host: &mut Host,
        gas: u64,
        hash: &Option<H256>,
    ) -> Result<(), Error> {
        let path = trace_tx_path(hash, &GAS)?;
        host.store_write_all(&path, gas.to_le_bytes().as_slice())?;
        Ok(())
    }

    pub fn store_trace_failed<Host: Runtime>(
        host: &mut Host,
        is_success: bool,
        hash: &Option<H256>,
    ) -> Result<(), Error> {
        let path = trace_tx_path(hash, &FAILED)?;
        host.store_write_all(&path, &[u8::from(!is_success)])?;
        log!(host, Debug, "Store trace info: is_success {}", is_success);
        Ok(())
    }

    pub fn store_return_value<Host: Runtime>(
        host: &mut Host,
        value: &[u8],
        hash: &Option<H256>,
    ) -> Result<(), Error> {
        let path = trace_tx_path(hash, &RETURN_VALUE)?;
        host.store_write_all(&path, value)?;
        log!(host, Debug, "Store trace info: value {:?}", value);
        Ok(())
    }

    pub fn store_struct_log<Host: Runtime>(
        host: &mut Host,
        struct_log: StructLog,
        hash: &Option<H256>,
    ) -> Result<(), Error> {
        let logs = rlp::encode(&struct_log);

        let path = trace_tx_path(hash, &STRUCT_LOGS)?;
        let struct_logs_storage = IndexableStorage::new_owned_path(path);

        struct_logs_storage.push_value(host, &logs)?;
        log!(host, Debug, "Store trace info: logs {:?}", logs);

        Ok(())
    }

    const CALL_TRACE: RefPath = RefPath::assert_from(b"/call_trace");

    pub fn store_call_trace<Host: Runtime>(
        host: &mut Host,
        call_trace: CallTrace,
        hash: &Option<H256>,
    ) -> Result<(), Error> {
        let encoded_call_trace = rlp::encode(&call_trace);

        let path = trace_tx_path(hash, &CALL_TRACE)?;
        let call_trace_storage = IndexableStorage::new_owned_path(path);

        call_trace_storage.push_value(host, &encoded_call_trace)?;
        log!(host, Debug, "Store call trace: {:?}", call_trace);

        Ok(())
    }
}

/// API to interact with blocks storage
pub mod blocks {
    use host::path::OwnedPath;
    use host::runtime::{Runtime, RuntimeError};
    use primitive_types::{H256, U256};
    use thiserror::Error;

    /// All errors that may happen as result of using this EVM storage interface.
    #[derive(Error, Debug, PartialEq)]
    pub enum EvmBlockStorageError {
        /// Some runtime error happened while using the hosts durable storage.
        #[error("Runtime error: {0:?}")]
        RuntimeError(host::runtime::RuntimeError),

        /// Some error happened when constructing the path to some resource
        /// associated with an account.
        #[error("Path error: {0:?}")]
        PathError(host::path::PathError),

        /// Passed block number is not sequential
        /// comparing to stored current block number
        #[error("Non sequential block levels. Current: {0}, new one: {1}")]
        NonSequentialBlockLevels(U256, U256),

        /// Some blockhash in storage has wrong number of bytes
        #[error("Malformed blockhash. Number of bytes: {0}")]
        MalformedBlockHash(usize),
    }

    impl From<RuntimeError> for EvmBlockStorageError {
        fn from(error: RuntimeError) -> Self {
            EvmBlockStorageError::RuntimeError(error)
        }
    }

    impl From<host::path::PathError> for EvmBlockStorageError {
        fn from(error: host::path::PathError) -> Self {
            EvmBlockStorageError::PathError(error)
        }
    }

    // Ref. https://www.evm.codes/#40?fork=shanghai
    // (opcode 0x40: BLOCKHASH)
    pub const BLOCKS_STORED: usize = 256;

    /// Get block hash by block number.
    pub fn get_block_hash(
        host: &impl Runtime,
        block_number: U256,
    ) -> Result<H256, EvmBlockStorageError> {
        let block_path = to_block_hash_path(block_number)?;
        let block_hash = host.store_read(&block_path, 0, 32)?;

        if block_hash.len() == 32 {
            Ok(H256::from_slice(&block_hash))
        } else {
            Err(EvmBlockStorageError::MalformedBlockHash(block_hash.len()))
        }
    }

    fn to_block_hash_path(block_number: U256) -> Result<OwnedPath, EvmBlockStorageError> {
        let path: Vec<u8> =
            format!("/evm/world_state/indexes/blocks/{block_number}").into();
        let owned_path = OwnedPath::try_from(path)?;
        Ok(owned_path)
    }

    /// Test utilities for block storage
    pub mod test_utils {
        use crypto::hash::BlockHash;
        use std::iter::Map;
        use std::ops::RangeFrom;

        type BlockIter = Map<RangeFrom<i32>, fn(i32) -> BlockHash>;

        /// Helper function for generation infinite iterator of blocks
        pub fn blocks_iter() -> BlockIter {
            (1_i32..).map(|level| {
                let level_bytes: Vec<u8> = Vec::from(level.to_be_bytes());
                BlockHash::try_from(level_bytes.repeat(8))
                    .expect("Hash expected to be valid")
            })
        }
    }
}
