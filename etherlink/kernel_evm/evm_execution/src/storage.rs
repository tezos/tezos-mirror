// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Storage interface for EVM kernel
//!
//! This interfaces to the part of Ethereum world state we keep in durable
//! storage. It does not contain any part of storage related to the tickets.
//! This module (and transactions.rs module) should be refactored completely
//! as part of milestone <https://gitlab.com/tezos/tezos/-/milestones/109>.

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
        let path: Vec<u8> = format!("/blocks/{}/hash", block_number).into();
        let owned_path = OwnedPath::try_from(path)?;
        Ok(owned_path)
    }

    fn delete_block_hash(
        host: &mut impl Runtime,
        block_number: U256,
    ) -> Result<(), EvmBlockStorageError> {
        let block_hash_path = to_block_hash_path(block_number)?;
        host.store_delete(&block_hash_path)
            .map_err(EvmBlockStorageError::RuntimeError)
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
                        H256::from_slice(Keccak256::digest(hash.0).as_slice());
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
