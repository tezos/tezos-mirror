// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::B256;
use tezos_ethereum::keyspace::KeySpaceExtHash;
use tezos_smart_rollup_keyspace::{Key, KeySpace};

// Ref. https://www.evm.codes/?fork=cancun#40
// (opcode 0x40: BLOCKHASH)
pub const BLOCKS_STORED: u64 = 256;

/// Returns the key of the hash of block `block_number` in the index of the last
/// [`BLOCKS_STORED`] blocks of a world-state keyspace.
pub fn block_index_key(block_number: u64) -> Key {
    Key::try_from(format!("/blocks/indexes/{block_number}"))
        .expect("`/blocks/indexes/` followed by the digits of a `u64` is a valid key")
}

/// Returns the hash of block `block_number` from the block index of
/// `world_state`, or `None` when the index has no hash for `block_number` or
/// its hash holds fewer than 32 bytes.
pub fn get_block_hash(world_state: &impl KeySpace, block_number: u64) -> Option<B256> {
    world_state.get_b256(&block_index_key(block_number))
}

#[cfg(test)]
mod test {
    use super::{block_index_key, get_block_hash};
    use revm::primitives::B256;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
    use tezos_smart_rollup_host::{path::RefPath, storage::StorageV1};
    use tezos_smart_rollup_keyspace::KeySpace;

    #[test]
    fn test_short_hash() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let block_number = 1u64;
        rk.evm_world_state_mut()
            .set(&block_index_key(block_number), [1; 31])
            .unwrap();

        assert_eq!(get_block_hash(rk.evm_world_state(), block_number), None);
    }

    #[test]
    fn test_missing_hash() {
        let mut host = MockKernelHost::default();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();

        assert_eq!(get_block_hash(rk.evm_world_state(), 1), None);
    }

    #[test]
    fn test_valid_hash() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let block_number = 1u64;
        let block_hash = &[42; 32];
        rk.evm_world_state_mut()
            .set(&block_index_key(block_number), block_hash)
            .unwrap();

        assert_eq!(
            get_block_hash(rk.evm_world_state(), block_number),
            Some(B256::from(block_hash))
        )
    }

    /// The hash of block 1 resolves to `/evm/world_state/blocks/indexes/1`.
    #[test]
    fn block_index_key_keeps_its_durable_path() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let block_hash = [42; 32];

        rk.evm_world_state_mut()
            .set(&block_index_key(1), block_hash)
            .unwrap();

        assert_eq!(
            rk.host()
                .store_read_all(&RefPath::assert_from(
                    b"/evm/world_state/blocks/indexes/1"
                ))
                .unwrap(),
            block_hash
        );
    }
}
