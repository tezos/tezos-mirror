// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
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
    use host::path::{concat, OwnedPath, Path, RefPath};
    use host::runtime::{Runtime, RuntimeError, ValueType};
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

    const ROOT_PATH: RefPath = RefPath::assert_from(b"/evm/blocks");

    const CURRENT_NUMBER_PATH: RefPath =
        RefPath::assert_from(b"/evm/blocks/current/number");

    const CURRENT_TIMESTAMP_PATH: RefPath =
        RefPath::assert_from(b"/evm/blocks/current/timestamp");

    const BLOCKS_STORED: usize = 256 + 1; // 256 previous ones + 1 current one

    /// Add new block with provided hash and timestamp.
    /// Drop a block which gets out of BLOCKS_STORED window.
    pub fn add_new_block(
        host: &mut impl Runtime,
        block_number: U256,
        hash: H256,
        timestamp: U256,
    ) -> Result<(), EvmBlockStorageError> {
        // Deal with current number first
        let current_number_exist = host.store_has(&CURRENT_NUMBER_PATH)?;
        if let Some(ValueType::Value | ValueType::ValueWithSubtree) = current_number_exist
        {
            let number_vec = host.store_read(&CURRENT_NUMBER_PATH, 0, 32)?;
            let current_number = U256::from_big_endian(&number_vec[..]);
            if block_number != current_number + U256::one() {
                return Err(EvmBlockStorageError::NonSequentialBlockLevels(
                    current_number,
                    block_number,
                ));
            }
        };
        // Strictly speaking we could enforce block_level to be equal to 0 or 1
        // if current_number doesn't exist in the storage yet.
        // But I am not sure if running a rollup node
        // from a snapshot will be possible in near future:
        // if so then it doesn't make sense to enforce this condition.

        // Deal with current block hash
        let block_hash_path = to_block_hash_path(block_number)?;
        write_h256(host, &block_hash_path, hash)?;

        // Write new current block number and timestamp
        write_u256(host, &CURRENT_NUMBER_PATH, block_number)?;
        write_u256(host, &CURRENT_TIMESTAMP_PATH, timestamp)?;

        // Clean up old block hash if any
        match block_number.checked_sub(U256::from(BLOCKS_STORED)) {
            Some(old_block_num) => delete_block_hash(host, old_block_num),
            None => Ok(()),
        }
    }

    /// Get number (level) and timestamp of current block.
    pub fn get_current_number_n_timestamp<Host: Runtime>(
        host: &Host,
    ) -> Result<(U256, U256), EvmBlockStorageError> {
        let number = host.store_read(&CURRENT_NUMBER_PATH, 0, 32)?;
        let timestamp = host.store_read(&CURRENT_TIMESTAMP_PATH, 0, 32)?;
        Ok((
            U256::from_big_endian(&number[..]),
            U256::from_big_endian(&timestamp[..]),
        ))
    }

    /// Get block hash by block number.
    pub fn get_block_hash(
        host: &impl Runtime,
        block_number: U256,
    ) -> Result<H256, EvmBlockStorageError> {
        let block_path = to_block_hash_path(block_number)?;
        let block_hash = host.store_read(&block_path, 0, 32)?;
        // TODO consider more accurately
        Ok(H256::from_slice(&block_hash[..]))
    }

    fn to_block_hash_path(block_number: U256) -> Result<OwnedPath, EvmBlockStorageError> {
        let path: Vec<u8> = alloc::format!("/{}/hash", block_number).into();
        let owned_path = OwnedPath::try_from(path)?;
        concat(&ROOT_PATH, &owned_path).map_err(EvmBlockStorageError::from)
    }

    fn delete_block_hash(
        host: &mut impl Runtime,
        block_number: U256,
    ) -> Result<(), EvmBlockStorageError> {
        let block_hash_path = to_block_hash_path(block_number)?;
        host.store_delete(&block_hash_path)
            .map_err(EvmBlockStorageError::RuntimeError)
    }

    fn write_u256(
        host: &mut impl Runtime,
        path: &impl Path,
        x: U256,
    ) -> Result<(), EvmBlockStorageError> {
        let mut x_be = [0u8; 32];
        x.to_big_endian(&mut x_be);
        host.store_write(path, &x_be, 0)
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
        use tezos_smart_rollup_encoding::timestamp::Timestamp;

        type BlockIter = Map<RangeFrom<i32>, fn(i32) -> (BlockHash, Timestamp)>;

        /// Helper function for generation infinite iterator of blocks
        pub fn blocks_iter() -> BlockIter {
            (1_i32..).map(|level| {
                let start_timestamp: i64 = 1674236056;
                let level_bytes: Vec<u8> = Vec::from(level.to_be_bytes());
                let tezos_hash = BlockHash::try_from(level_bytes.repeat(8))
                    .expect("Hash expected to be valid");
                let ts = Timestamp::from(start_timestamp + 30000 * i64::from(level));
                (tezos_hash, ts)
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
                .for_each(|(i, (hash, ts))| {
                    let keccak_hash =
                        H256::from_slice(Keccak256::digest(hash.0).as_slice());
                    if i == DELETE_BLOCKS_N {
                        last_kept_block_hash = Some(keccak_hash)
                    }
                    add_new_block(
                        &mut mock_host,
                        U256::from(i as i32),
                        keccak_hash,
                        U256::from(ts.i64()),
                    )
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
