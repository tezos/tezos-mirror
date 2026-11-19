// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H256, U256};
use revm_etherlink::storage::block::{block_index_key, get_block_hash, BLOCKS_STORED};
use tezos_ethereum::keyspace::{KeySpaceExtHash, KeySpaceExtU256};
use tezos_ethereum::{
    block::EthBlock,
    transaction::{TransactionObject, TransactionReceipt},
};
use tezos_evm_logging::{log, Level::Info};
use tezos_smart_rollup_keyspace::{Key, KeySpace, KeySpaceError};

use crate::l2block::L2Block;

/// Key of the transaction objects of the current block, as one rlp list,
/// inside the `/evm/world_state` keyspace.
const CURRENT_BLOCK_TRANSACTIONS_OBJECTS_KEY: Key =
    Key::from_static(b"/blocks/current/transactions_objects");

/// Key of the receipts of the transactions of the current block, as one rlp
/// list, inside the `/evm/world_state` keyspace.
const CURRENT_BLOCK_TRANSACTIONS_RECEIPTS_KEY: Key =
    Key::from_static(b"/blocks/current/transactions_receipts");

/// Key of the number of the current block, inside a world-state keyspace.
const CURRENT_BLOCK_NUMBER_KEY: Key = Key::from_static(b"/blocks/current/number");

/// Key of the hash of the current block, inside a world-state keyspace.
const CURRENT_BLOCK_HASH_KEY: Key = Key::from_static(b"/blocks/current/hash");

/// Key of the encoding of the current block, inside a world-state keyspace.
const CURRENT_BLOCK_KEY: Key = Key::from_static(b"/blocks/current/block");

/// Returns the key that marks the block of hash `hash` as one of the last
/// [`BLOCKS_STORED`] blocks of a world-state keyspace.
fn live_block_key(hash: &H256) -> Key {
    Key::try_from(format!("/live_blocks/{}", hex::encode(hash.as_bytes())))
        .expect("`/live_blocks/` followed by 64 hex characters is a valid key")
}

fn runtime_of_block(block: &L2Block) -> &'static str {
    match block {
        L2Block::Etherlink(_) => "EVM",
        L2Block::Tezlink(_) => "Michelson",
    }
}

/// Stores `block` as the current block of `world_state`: its number, its hash
/// and its encoding. Also adds its hash to the index of the last
/// [`BLOCKS_STORED`] blocks, and removes the block that leaves this index.
///
/// With `maintain_live_blocks`, `block` also becomes a live block of
/// `world_state` (see [`is_recent_block_hash`]), and the block that leaves the
/// index stops being one.
///
/// Errors when a write fails.
///
/// # Panics
///
/// Panics when the number of `block` does not fit in a `u64`.
pub fn store_current(
    world_state: &mut impl KeySpace,
    block: &L2Block,
    maintain_live_blocks: bool,
) -> Result<(), KeySpaceError> {
    store_current_number(world_state, block.number())?;
    world_state.set(&CURRENT_BLOCK_HASH_KEY, block.hash().as_bytes())?;
    update_block_indexes(world_state, block, maintain_live_blocks)?;
    world_state.set(&CURRENT_BLOCK_KEY, block.to_bytes())?;
    log!(
        Info,
        "Storing {} block {} at {} containing {} transaction(s) for {} gas used.",
        runtime_of_block(block),
        block.number(),
        block.timestamp(),
        block.number_of_transactions(),
        &block.gas_used_report()
    );
    Ok(())
}

/// Adds the hash of `block` to the index of the last [`BLOCKS_STORED`] blocks
/// of `world_state`, and removes the block that leaves this index. With
/// `maintain_live_blocks`, updates the live blocks of `world_state` the same
/// way.
///
/// Errors when a write fails.
///
/// # Panics
///
/// Panics when the number of `block` does not fit in a `u64`.
pub(crate) fn update_block_indexes(
    world_state: &mut impl KeySpace,
    block: &L2Block,
    maintain_live_blocks: bool,
) -> Result<(), KeySpaceError> {
    let block_number = block.number().as_u64();
    if let Some(old_block_number) = block_number.checked_sub(BLOCKS_STORED) {
        if maintain_live_blocks {
            // Evict the live_blocks entry for the block leaving the window; its
            // hash is read from the number index deleted just below.
            if let Some(old_hash) = get_block_hash(world_state, old_block_number) {
                world_state.delete(&live_block_key(&H256(old_hash.0)));
            }
        }
        world_state.delete(&block_index_key(old_block_number));
    }
    world_state.set(&block_index_key(block_number), block.hash().as_bytes())?;
    if maintain_live_blocks {
        // Membership is tested with `contains`, so an empty marker suffices;
        // the value is never read.
        world_state.set(&live_block_key(&block.hash()), b"")?;
    }
    Ok(())
}

/// Returns `true` when `hash` is one of the last [`BLOCKS_STORED`] blocks in
/// the live blocks of `world_state`.
pub fn is_recent_block_hash(world_state: &impl KeySpace, hash: &H256) -> bool {
    world_state.contains(&live_block_key(hash))
}

/// Stores `number` as the number of the current block of `world_state`.
///
/// Errors when the write fails.
pub(crate) fn store_current_number(
    world_state: &mut impl KeySpace,
    number: U256,
) -> Result<(), KeySpaceError> {
    world_state.store_u256_le(&CURRENT_BLOCK_NUMBER_KEY, number)
}

/// Stores `transactions_objects` as the transaction objects of the current
/// block of `evm_world_state`.
///
/// Errors when the write fails.
pub fn store_current_transactions_objects(
    evm_world_state: &mut impl KeySpace,
    transactions_objects: &[TransactionObject],
) -> Result<(), KeySpaceError> {
    evm_world_state.set(
        &CURRENT_BLOCK_TRANSACTIONS_OBJECTS_KEY,
        rlp::encode_list(transactions_objects),
    )
}

/// Stores `receipts` as the receipts of the transactions of the current block
/// of `evm_world_state`.
///
/// Errors when the write fails.
pub fn store_current_transactions_receipts(
    evm_world_state: &mut impl KeySpace,
    receipts: &[TransactionReceipt],
) -> Result<(), KeySpaceError> {
    evm_world_state.set(
        &CURRENT_BLOCK_TRANSACTIONS_RECEIPTS_KEY,
        rlp::encode_list(receipts),
    )
}

/// Returns the current block of `evm_world_state`, or `None` when it has no
/// current block.
///
/// Errors when the stored bytes do not decode as a block.
pub fn read_current_etherlink_block(
    evm_world_state: &impl KeySpace,
) -> Result<Option<EthBlock>, KeySpaceError> {
    evm_world_state
        .get(&CURRENT_BLOCK_KEY)
        .map(|bytes| {
            EthBlock::from_bytes(&bytes).map_err(|err| {
                KeySpaceError::read(
                    evm_world_state.name(),
                    &CURRENT_BLOCK_KEY,
                    err.into(),
                )
            })
        })
        .transpose()
}

/// Returns the number of the current block of `world_state`, or `None` when
/// it has no current block or its number holds fewer than 32 bytes.
pub fn read_current_number(world_state: &impl KeySpace) -> Option<U256> {
    world_state.get_u256_le(&CURRENT_BLOCK_NUMBER_KEY)
}

/// Returns the hash of the current block of `world_state`, or `None` when it
/// has no current block or its hash holds fewer than 32 bytes. A longer hash
/// reads as its first 32 bytes.
pub fn read_current_hash(world_state: &impl KeySpace) -> Option<H256> {
    world_state.get_h256(&CURRENT_BLOCK_HASH_KEY)
}

/// Returns the encoding of the current block of `tez_world_state`, or `None`
/// when it has no current block.
#[cfg(test)]
pub fn read_tez_current_block(tez_world_state: &impl KeySpace) -> Option<Vec<u8>> {
    tez_world_state.get(&CURRENT_BLOCK_KEY)
}

#[cfg(test)]
pub mod internal_for_tests {
    use tezos_ethereum::transaction::{TransactionHash, TransactionStatus};

    use crate::error::Error;

    use super::*;

    pub fn store_current_number(
        world_state: &mut impl KeySpace,
        number: U256,
    ) -> Result<(), KeySpaceError> {
        super::store_current_number(world_state, number)
    }

    /// Register `hash` as a recent block so [`super::is_recent_block_hash`] accepts it (test helper).
    pub fn register_live_block(
        world_state: &mut impl KeySpace,
        hash: &H256,
    ) -> Result<(), KeySpaceError> {
        world_state.set(&super::live_block_key(hash), b"")
    }

    /// Returns the receipt of `tx_hash` among the receipts of the current
    /// block of `evm_world_state`.
    ///
    /// Errors when the block has no receipt of `tx_hash`, or when the
    /// receipts do not decode.
    pub fn read_transaction_receipt(
        evm_world_state: &impl KeySpace,
        tx_hash: &TransactionHash,
    ) -> Result<TransactionReceipt, Error> {
        let receipts: Vec<TransactionReceipt> =
            match evm_world_state.get(&CURRENT_BLOCK_TRANSACTIONS_RECEIPTS_KEY) {
                Some(bytes) => rlp::Rlp::new(&bytes).as_list()?,
                None => vec![],
            };
        receipts
            .into_iter()
            .find(|receipt| &receipt.hash == tx_hash)
            .ok_or(Error::RlpDecoderError(rlp::DecoderError::Custom(
                "Receipt not found.",
            )))
    }

    /// Returns the status of the receipt that [`read_transaction_receipt`]
    /// returns.
    pub fn read_transaction_receipt_status(
        evm_world_state: &impl KeySpace,
        tx_hash: &TransactionHash,
    ) -> Result<TransactionStatus, Error> {
        let receipt = read_transaction_receipt(evm_world_state, tx_hash)?;
        Ok(receipt.status)
    }
}

#[cfg(test)]
mod live_blocks_tests {
    use super::*;
    use crate::l2block::L2Block;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
    use tezos_smart_rollup::types::Timestamp;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_tezlink::block::TezBlock;
    use tezos_tezlink::protocol::{INITIAL_PROTOCOL, TARGET_TEZOS_PROTOCOL};

    /// Store a Michelson block at `number` and return its hash.
    fn store_tez_block(
        tez_world_state: &mut impl KeySpace,
        number: u32,
        maintain_live_blocks: bool,
    ) -> H256 {
        let previous_protocol = INITIAL_PROTOCOL;
        let current_protocol = TARGET_TEZOS_PROTOCOL;
        let block = TezBlock::new(
            previous_protocol,
            current_protocol,
            number.into(),
            Timestamp::from(number as i64),
            // Not the genesis hash, so `new` keeps the provided predecessor.
            H256::zero(),
            vec![],
            [number as u8; 32],
        )
        .expect("TezBlock::new");
        let block = L2Block::Tezlink(block);
        let hash = block.hash();
        store_current(tez_world_state, &block, maintain_live_blocks)
            .expect("store_current");
        hash
    }

    #[test]
    fn recent_branch_is_member_foreign_is_not() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let h0 = store_tez_block(rk.tez_world_state_mut(), 0, true);
        let h1 = store_tez_block(rk.tez_world_state_mut(), 1, true);

        assert!(is_recent_block_hash(rk.tez_world_state(), &h0));
        assert!(is_recent_block_hash(rk.tez_world_state(), &h1));
        // A branch that is not a recent block of this instance (the replay case) is not a member.
        assert!(!is_recent_block_hash(
            rk.tez_world_state(),
            &H256::from([0xABu8; 32])
        ));
    }

    #[test]
    fn block_leaving_the_window_is_evicted() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut hashes = Vec::new();
        // Store BLOCKS_STORED + 1 blocks; storing block BLOCKS_STORED evicts block 0.
        for number in 0..=(BLOCKS_STORED as u32) {
            hashes.push(store_tez_block(rk.tez_world_state_mut(), number, true));
        }

        // Block 0 fell out of the window and is no longer a valid branch.
        assert!(!is_recent_block_hash(rk.tez_world_state(), &hashes[0]));
        // The oldest still-in-window block and the latest one remain members.
        assert!(is_recent_block_hash(rk.tez_world_state(), &hashes[1]));
        assert!(is_recent_block_hash(
            rk.tez_world_state(),
            &hashes[BLOCKS_STORED as usize]
        ));
    }

    #[test]
    fn maintain_flag_false_skips_live_blocks() {
        // The EVM path (maintain_live_blocks = false) must not populate live_blocks.
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let hash = store_tez_block(rk.tez_world_state_mut(), 0, false);
        assert!(!is_recent_block_hash(rk.tez_world_state(), &hash));
    }

    /// A Michelson block stored at number 3 resolves to the durable paths under
    /// `/tez/world_state`.
    #[test]
    fn store_current_keeps_the_durable_paths() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let hash = store_tez_block(rk.tez_world_state_mut(), 3, true);

        let read = |path: &str| {
            rk.host()
                .store_read_all(&RefPath::assert_from(path.as_bytes()))
                .unwrap()
        };
        let mut number = [0u8; 32];
        U256::from(3).to_little_endian(&mut number);
        assert_eq!(read("/tez/world_state/blocks/current/number"), number);
        assert_eq!(
            read("/tez/world_state/blocks/current/hash"),
            hash.as_bytes()
        );
        assert_eq!(
            Some(read("/tez/world_state/blocks/current/block")),
            read_tez_current_block(rk.tez_world_state())
        );
        assert_eq!(read("/tez/world_state/blocks/indexes/3"), hash.as_bytes());
        assert_eq!(
            read(&format!(
                "/tez/world_state/live_blocks/{}",
                hex::encode(hash)
            )),
            Vec::<u8>::new()
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use primitive_types::H160;
    use tezos_ethereum::block::BlockConstants;
    use tezos_ethereum::rlp_helpers::VersionedEncoding;
    use tezos_ethereum::transaction::{TransactionStatus, TransactionType};
    use tezos_ethereum::Bloom;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
    use tezos_smart_rollup::types::Timestamp;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;

    /// The transaction objects of the current block resolve to
    /// `/evm/world_state/blocks/current/transactions_objects`.
    #[test]
    fn current_block_transactions_objects_key_keeps_its_durable_path() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let object = TransactionObject {
            block_number: U256::from(3),
            from: H160::from_low_u64_be(1),
            gas: U256::from(21_000),
            gas_price: U256::one(),
            hash: [1; 32],
            input: vec![],
            nonce: 0,
            to: Some(H160::from_low_u64_be(2)),
            index: 0,
            value: U256::from(5),
            signature: None,
        };

        store_current_transactions_objects(
            rk.evm_world_state_mut(),
            std::slice::from_ref(&object),
        )
        .unwrap();

        assert_eq!(
            rk.host()
                .store_read_all(&RefPath::assert_from(
                    b"/evm/world_state/blocks/current/transactions_objects"
                ))
                .unwrap(),
            rlp::encode_list(std::slice::from_ref(&object)).to_vec()
        );
    }

    /// The receipts of the current block resolve to
    /// `/evm/world_state/blocks/current/transactions_receipts`.
    #[test]
    fn current_block_transactions_receipts_key_keeps_its_durable_path() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let receipt = TransactionReceipt {
            hash: [1; 32],
            index: 0,
            block_number: U256::from(3),
            from: H160::from_low_u64_be(1),
            to: None,
            cumulative_gas_used: U256::from(21_000),
            effective_gas_price: U256::one(),
            gas_used: U256::from(21_000),
            contract_address: None,
            logs: vec![],
            logs_bloom: Bloom::default(),
            type_: TransactionType::Legacy,
            status: TransactionStatus::Success,
        };

        store_current_transactions_receipts(
            rk.evm_world_state_mut(),
            std::slice::from_ref(&receipt),
        )
        .unwrap();

        assert_eq!(
            rk.host()
                .store_read_all(&RefPath::assert_from(
                    b"/evm/world_state/blocks/current/transactions_receipts"
                ))
                .unwrap(),
            rlp::encode_list(std::slice::from_ref(&receipt)).to_vec()
        );
        assert_eq!(
            internal_for_tests::read_transaction_receipt(
                rk.evm_world_state(),
                &receipt.hash
            )
            .unwrap(),
            receipt
        );
    }

    /// The number of the current block resolves to
    /// `/tez/world_state/blocks/current/number`.
    #[test]
    fn current_block_number_key_keeps_its_durable_path() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let number = U256::from(7);

        rk.tez_world_state_mut()
            .store_u256_le(&CURRENT_BLOCK_NUMBER_KEY, number)
            .unwrap();

        let mut bytes = [0u8; 32];
        number.to_little_endian(&mut bytes);
        assert_eq!(
            rk.host()
                .store_read_all(&RefPath::assert_from(
                    b"/tez/world_state/blocks/current/number"
                ))
                .unwrap(),
            bytes
        );
        assert_eq!(read_current_number(rk.tez_world_state()), Some(number));
    }

    /// The hash of the current block resolves to
    /// `/evm/world_state/blocks/current/hash`.
    #[test]
    fn current_block_hash_key_keeps_its_durable_path() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let hash = H256::from([9; 32]);

        rk.evm_world_state_mut()
            .set(&CURRENT_BLOCK_HASH_KEY, hash.as_bytes())
            .unwrap();

        assert_eq!(
            rk.host()
                .store_read_all(&RefPath::assert_from(
                    b"/evm/world_state/blocks/current/hash"
                ))
                .unwrap(),
            hash.as_bytes()
        );
        assert_eq!(read_current_hash(rk.evm_world_state()), Some(hash));
    }

    /// The current block resolves to `/evm/world_state/blocks/current/block`.
    #[test]
    fn current_block_key_keeps_its_durable_path() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let block = EthBlock::new(
            U256::from(3),
            vec![[4; 32]],
            Timestamp::from(5),
            H256::from([6; 32]),
            Bloom::default(),
            vec![7; 32],
            vec![8; 32],
            vec![9; 32],
            U256::from(21_000),
            &BlockConstants::dummy(),
            U256::one(),
        );

        rk.evm_world_state_mut()
            .set(&CURRENT_BLOCK_KEY, block.to_bytes())
            .unwrap();

        assert_eq!(
            rk.host()
                .store_read_all(&RefPath::assert_from(
                    b"/evm/world_state/blocks/current/block"
                ))
                .unwrap(),
            block.to_bytes()
        );
        assert_eq!(
            read_current_etherlink_block(rk.evm_world_state()).unwrap(),
            Some(block)
        );
    }

    /// A live block resolves to `/tez/world_state/live_blocks/<hex hash>`.
    #[test]
    fn live_block_key_keeps_its_durable_path() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let hash = H256::from([0xAB; 32]);

        internal_for_tests::register_live_block(rk.tez_world_state_mut(), &hash).unwrap();

        let path = format!("/tez/world_state/live_blocks/{}", "ab".repeat(32));
        assert!(rk
            .host()
            .store_has(&RefPath::assert_from(path.as_bytes()))
            .unwrap()
            .is_some());
        assert!(is_recent_block_hash(rk.tez_world_state(), &hash));
    }
}
