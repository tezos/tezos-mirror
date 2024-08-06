// SPDX-FileCopyrightText: 2022,2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pub mod tracer {
    use host::{
        path::{OwnedPath, RefPath},
        runtime::{Runtime, RuntimeError},
    };

    use primitive_types::H256;
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
        Ok(())
    }

    pub fn store_return_value<Host: Runtime>(
        host: &mut Host,
        value: &[u8],
        hash: &Option<H256>,
    ) -> Result<(), Error> {
        let path = trace_tx_path(hash, &RETURN_VALUE)?;
        host.store_write_all(&path, value)?;
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

        Ok(())
    }

    const CALL_TRACE: RefPath = RefPath::assert_from(b"/call_trace");

    pub fn store_call_trace<Host: Runtime>(
        host: &mut Host,
        call_trace: CallTrace,
        hash: &Option<H256>,
    ) -> Result<(), Error> {
        let call_trace = rlp::encode(&call_trace);

        let path = trace_tx_path(hash, &CALL_TRACE)?;
        let call_trace_storage = IndexableStorage::new_owned_path(path);

        call_trace_storage.push_value(host, &call_trace)?;

        Ok(())
    }
}

/// API to interact with blocks storage
pub mod blocks {
    use host::path::{OwnedPath, Path};
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
    const BLOCKS_STORED: usize = 256;

    /// Add new block with provided hash and timestamp.
    /// Drop a block which gets out of BLOCKS_STORED window.
    pub fn add_new_block_hash(
        host: &mut impl Runtime,
        block_number: U256,
        hash: H256,
    ) -> Result<(), EvmBlockStorageError> {
        let block_hash_path = to_block_hash_path(block_number)?;
        write_h256(host, &block_hash_path, hash)?;

        // Clean up old block hash if any
        match block_number.checked_sub(U256::from(BLOCKS_STORED)) {
            Some(old_block_num) => delete_block_hash(host, old_block_num),
            None => Ok(()),
        }
    }

    /// Get block hash by block number.
    pub fn get_block_hash(
        host: &impl Runtime,
        block_number: U256,
    ) -> Result<H256, EvmBlockStorageError> {
        let block_path = to_block_hash_path(block_number)?;
        let block_hash = host.store_read_all(&block_path)?;

        if block_hash.len() == 32 {
            Ok(H256::from_slice(&block_hash))
        } else {
            Err(EvmBlockStorageError::MalformedBlockHash(block_hash.len()))
        }
    }

    fn to_block_hash_path(block_number: U256) -> Result<OwnedPath, EvmBlockStorageError> {
        let path: Vec<u8> =
            format!("/evm/world_state/blocks/{}/hash", block_number).into();
        let owned_path = OwnedPath::try_from(path)?;
        Ok(owned_path)
    }

    fn delete_block_hash(
        host: &mut impl Runtime,
        block_number: U256,
    ) -> Result<(), EvmBlockStorageError> {
        let block_hash_path = to_block_hash_path(block_number)?;

        match host.store_delete(&block_hash_path) {
            Ok(()) | Err(RuntimeError::PathNotFound) => Ok(()),
            Err(err) => Err(EvmBlockStorageError::RuntimeError(err)),
        }
    }

    fn write_h256(
        host: &mut impl Runtime,
        path: &impl Path,
        hash: H256,
    ) -> Result<(), EvmBlockStorageError> {
        let hash_bytes = hash.to_fixed_bytes();
        host.store_write(path, &hash_bytes, 0)
            .map_err(EvmBlockStorageError::RuntimeError)
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

    #[cfg(test)]
    pub mod tests {
        use super::test_utils::blocks_iter;
        use super::*;
        use primitive_types::U256;
        use sha3::{Digest, Keccak256};
        use tezos_smart_rollup_mock::MockHost;

        #[test]
        fn blocks_cleaned_up() {
            let mut mock_host = MockHost::default();
            const DELETE_BLOCKS_N: usize = 5;
            let mut last_kept_block_hash = None;

            blocks_iter()
                .take(BLOCKS_STORED + DELETE_BLOCKS_N)
                .enumerate()
                .for_each(|(i, hash)| {
                    let keccak_hash =
                        H256::from_slice(Keccak256::digest(hash.as_ref()).as_slice());
                    if i == DELETE_BLOCKS_N {
                        last_kept_block_hash = Some(keccak_hash)
                    }
                    add_new_block_hash(&mut mock_host, U256::from(i as i32), keccak_hash)
                        .unwrap()
                });

            // Make sure that blocks are cleaned up
            (0..DELETE_BLOCKS_N).for_each(|i| {
                assert_eq!(
                    get_block_hash(&mock_host, U256::from(i as i32))
                        .expect_err("Blocks should be cleaned up"),
                    EvmBlockStorageError::RuntimeError(
                        host::runtime::RuntimeError::PathNotFound
                    )
                );
            });

            // Make sure that last block is kept
            assert_eq!(
                get_block_hash(&mock_host, U256::from(DELETE_BLOCKS_N as i32))
                    .unwrap_or_else(|_| panic!(
                        "Block with number {} should be still kept",
                        DELETE_BLOCKS_N
                    )),
                last_kept_block_hash.unwrap()
            );
        }
    }
}
