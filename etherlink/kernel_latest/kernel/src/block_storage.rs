// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::migration::allow_path_not_found;
use primitive_types::{H256, U256};
use revm_etherlink::storage::block::BLOCKS_STORED;
use tezos_ethereum::{
    block::EthBlock,
    transaction::{TransactionObject, TransactionReceipt},
};
use tezos_evm_logging::{log, Level::Info};
use tezos_smart_rollup_host::path::{concat, OwnedPath, Path, RefPath};
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_storage::{read_u256_le, write_h256_be, write_u256_le};

use crate::{chains::ETHERLINK_SAFE_STORAGE_ROOT_PATH, l2block::L2Block};
use rlp::Encodable;

mod path {
    use tezos_smart_rollup_host::path::{concat, OwnedPath, Path, PathError, RefPath};

    const EVM_CURRENT_BLOCK_PATH: RefPath =
        RefPath::assert_from(b"/blocks/current/block");
    const EVM_CURRENT_BLOCK_TRANSACTIONS_OBJECTS: RefPath =
        RefPath::assert_from(b"/blocks/current/transactions_objects");
    const EVM_CURRENT_BLOCK_TRANSACTIONS_RECEIPTS: RefPath =
        RefPath::assert_from(b"/blocks/current/transactions_receipts");
    const EVM_CURRENT_BLOCK_NUMBER_PATH: RefPath =
        RefPath::assert_from(b"/blocks/current/number");
    const EVM_CURRENT_BLOCK_HASH_PATH: RefPath =
        RefPath::assert_from(b"/blocks/current/hash");
    const INDEXES_PATH: RefPath = RefPath::assert_from(b"/blocks/indexes");
    // Recent block hashes (last `BLOCKS_STORED`), indexed by hash; Michelson runtime only.
    const LIVE_BLOCKS_PATH: RefPath = RefPath::assert_from(b"/live_blocks");

    pub fn current_number(root: &impl Path) -> Result<OwnedPath, PathError> {
        concat(root, &EVM_CURRENT_BLOCK_NUMBER_PATH)
    }

    pub fn current_hash(root: &impl Path) -> Result<OwnedPath, PathError> {
        concat(root, &EVM_CURRENT_BLOCK_HASH_PATH)
    }

    pub fn current_block(root: &impl Path) -> Result<OwnedPath, PathError> {
        concat(root, &EVM_CURRENT_BLOCK_PATH)
    }

    pub fn current_block_transactions_objects(
        root: &impl Path,
    ) -> Result<OwnedPath, PathError> {
        concat(root, &EVM_CURRENT_BLOCK_TRANSACTIONS_OBJECTS)
    }

    pub fn current_block_transactions_receipts(
        root: &impl Path,
    ) -> Result<OwnedPath, PathError> {
        concat(root, &EVM_CURRENT_BLOCK_TRANSACTIONS_RECEIPTS)
    }

    pub fn block_indexes(root: &impl Path) -> Result<OwnedPath, PathError> {
        concat(root, &INDEXES_PATH)
    }

    pub fn live_blocks(root: &impl Path) -> Result<OwnedPath, PathError> {
        concat(root, &LIVE_BLOCKS_PATH)
    }
}

pub fn store_current<Host>(
    host: &mut Host,
    root: &impl Path,
    block: &L2Block,
    maintain_live_blocks: bool,
) -> Result<(), crate::Error>
where
    Host: StorageV1,
{
    store_current_number(host, root, block.number())?;
    write_h256_be(host, &path::current_hash(root)?, block.hash())?;
    update_block_indexes(host, root, block, maintain_live_blocks)?;
    host.store_write_all(&path::current_block(root)?, &block.to_bytes())?;
    log!(
        Info,
        "Storing block {} at {} containing {} transaction(s) for {} gas used.",
        block.number(),
        block.timestamp(),
        block.number_of_transactions(),
        U256::to_string(&block.gas_used())
    );
    Ok(())
}

pub(crate) fn update_block_indexes(
    host: &mut impl StorageV1,
    root: &impl Path,
    block: &L2Block,
    maintain_live_blocks: bool,
) -> Result<(), crate::Error> {
    let indexes_path = path::block_indexes(root)?;
    let block_number = block.number().as_u64();
    let full_path = concat(
        &indexes_path,
        &RefPath::assert_from(format!("/{block_number}").as_bytes()),
    )?;
    if let Some(old_block_number) = block_number.checked_sub(BLOCKS_STORED) {
        let old_path = concat(
            &indexes_path,
            &RefPath::assert_from(format!("/{old_block_number}").as_bytes()),
        )?;
        if maintain_live_blocks {
            // Evict the live_blocks entry for the block leaving the window; its
            // hash is read from the number index deleted just below.
            if let Ok(old_hash) = host.store_read(&old_path, 0, 32) {
                if old_hash.len() == 32 {
                    let old_live =
                        live_block_entry_path(root, &H256::from_slice(&old_hash))?;
                    allow_path_not_found(host.store_delete(&old_live))?;
                }
            }
        }
        allow_path_not_found(host.store_delete(&old_path))?;
    }
    host.store_write_all(&full_path, block.hash().as_bytes())?;
    if maintain_live_blocks {
        // Membership is tested with `store_has`, so an empty marker suffices;
        // the value is never read.
        let live_path = live_block_entry_path(root, &block.hash())?;
        host.store_write_all(&live_path, b"")?;
    }
    Ok(())
}

/// Build the live_blocks entry path `{root}/live_blocks/{hex(hash)}`.
fn live_block_entry_path(
    root: &impl Path,
    hash: &H256,
) -> Result<OwnedPath, crate::Error> {
    let base = path::live_blocks(root)?;
    let entry = concat(
        &base,
        &RefPath::assert_from(format!("/{}", hex::encode(hash.as_bytes())).as_bytes()),
    )?;
    Ok(entry)
}

/// Returns `true` when `hash` is one of the last [`BLOCKS_STORED`] blocks in the live_blocks set under `root`.
pub fn is_recent_block_hash(
    host: &impl StorageV1,
    root: &impl Path,
    hash: &H256,
) -> Result<bool, crate::Error> {
    let path = live_block_entry_path(root, hash)?;
    Ok(host.store_has(&path)?.is_some())
}

pub(crate) fn store_current_number(
    host: &mut impl StorageV1,
    root: &impl Path,
    number: U256,
) -> Result<(), crate::Error> {
    write_u256_le(host, &path::current_number(root)?, number)?;
    Ok(())
}

pub fn store_current_transactions_objects(
    host: &mut impl StorageV1,
    root: &impl Path,
    transactions_objects: &[TransactionObject],
) -> Result<(), crate::Error> {
    let stream = rlp::RlpStream::new_list(transactions_objects.len());
    let bytes = transactions_objects
        .iter()
        .fold(stream, |mut acc, tx_obj| {
            tx_obj.rlp_append(&mut acc);
            acc
        })
        .out()
        .to_vec();
    host.store_write_all(&path::current_block_transactions_objects(root)?, &bytes)?;
    Ok(())
}

pub fn store_current_transactions_receipts(
    host: &mut impl StorageV1,
    root: &impl Path,
    receipts: &[TransactionReceipt],
) -> Result<(), crate::Error> {
    let stream = rlp::RlpStream::new_list(receipts.len());
    let bytes = receipts
        .iter()
        .fold(stream, |mut acc, receipt| {
            receipt.rlp_append(&mut acc);
            acc
        })
        .out()
        .to_vec();
    host.store_write_all(&path::current_block_transactions_receipts(root)?, &bytes)?;
    Ok(())
}

pub fn read_current_etherlink_block(
    host: &mut impl StorageV1,
) -> anyhow::Result<EthBlock> {
    let block_path = path::current_block(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)?;
    let bytes = &host.store_read_all(&block_path)?;
    let block_from_bytes = EthBlock::from_bytes(bytes)?;
    Ok(block_from_bytes)
}

pub fn read_current_number(
    host: &impl StorageV1,
    root: &impl Path,
) -> Result<U256, crate::Error> {
    Ok(read_u256_le(host, &path::current_number(root)?)?)
}

pub fn read_current_hash(
    host: &impl StorageV1,
    root: &impl Path,
) -> Result<H256, crate::Error> {
    let hash_path = path::current_hash(root)?;
    let bytes = &host.store_read_all(&hash_path)?;
    if bytes.len() != 32 {
        return Err(crate::Error::InvalidConversion);
    }
    Ok(H256::from_slice(bytes))
}

#[cfg(test)]
pub fn read_tez_current_block(host: &mut impl StorageV1) -> anyhow::Result<Vec<u8>> {
    use crate::chains::TEZ_BLOCKS_PATH;
    let block_path = path::current_block(&TEZ_BLOCKS_PATH)?;
    let bytes = host.store_read_all(&block_path)?;
    Ok(bytes)
}

#[cfg(test)]
pub mod internal_for_tests {
    use tezos_ethereum::transaction::{TransactionHash, TransactionStatus};

    use crate::{chains::ETHERLINK_SAFE_STORAGE_ROOT_PATH, error::Error};

    use super::*;

    pub fn store_current_number(
        host: &mut impl StorageV1,
        root: &impl Path,
        number: U256,
    ) -> Result<(), crate::Error> {
        super::store_current_number(host, root, number)
    }

    /// Register `hash` as a recent block so [`super::is_recent_block_hash`] accepts it (test helper).
    pub fn register_live_block(
        host: &mut impl StorageV1,
        root: &impl Path,
        hash: &H256,
    ) -> Result<(), crate::Error> {
        let live_path = super::live_block_entry_path(root, hash)?;
        host.store_write_all(&live_path, b"")?;
        Ok(())
    }

    pub fn read_transaction_receipt(
        host: &mut impl StorageV1,
        tx_hash: &TransactionHash,
    ) -> Result<TransactionReceipt, Error> {
        let receipts_path =
            path::current_block_transactions_receipts(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)?;
        let bytes = &host.store_read_all(&receipts_path)?;
        let rlp = rlp::Rlp::new(bytes);
        for i in 0..rlp.item_count()? {
            let receipt: TransactionReceipt = rlp.at(i)?.as_val()?;
            if &receipt.hash == tx_hash {
                return Ok(receipt);
            }
        }
        Err(Error::RlpDecoderError(rlp::DecoderError::Custom(
            "Receipt not found.",
        )))
    }

    /// Reads status from the receipt in storage.
    pub fn read_transaction_receipt_status<Host>(
        host: &mut Host,
        tx_hash: &TransactionHash,
    ) -> Result<TransactionStatus, Error>
    where
        Host: StorageV1,
    {
        let receipt = read_transaction_receipt(host, tx_hash)?;
        Ok(receipt.status)
    }
}

#[cfg(test)]
mod live_blocks_tests {
    use super::*;
    use crate::chains::TEZ_BLOCKS_PATH;
    use crate::l2block::L2Block;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup::types::Timestamp;
    use tezos_tezlink::block::TezBlock;
    use tezos_tezlink::protocol::Protocol;

    /// Store a Michelson block at `number` and return its hash.
    fn store_tez_block(
        host: &mut MockKernelHost,
        number: u32,
        maintain_live_blocks: bool,
    ) -> H256 {
        let block = TezBlock::new(
            Protocol::S023,
            Protocol::S023,
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
        store_current(host, &TEZ_BLOCKS_PATH, &block, maintain_live_blocks)
            .expect("store_current");
        hash
    }

    #[test]
    fn recent_branch_is_member_foreign_is_not() {
        let mut host = MockKernelHost::default();
        let h0 = store_tez_block(&mut host, 0, true);
        let h1 = store_tez_block(&mut host, 1, true);

        assert!(is_recent_block_hash(&host, &TEZ_BLOCKS_PATH, &h0).unwrap());
        assert!(is_recent_block_hash(&host, &TEZ_BLOCKS_PATH, &h1).unwrap());
        // A branch that is not a recent block of this instance (the replay case) is not a member.
        assert!(!is_recent_block_hash(
            &host,
            &TEZ_BLOCKS_PATH,
            &H256::from([0xABu8; 32])
        )
        .unwrap());
    }

    #[test]
    fn block_leaving_the_window_is_evicted() {
        let mut host = MockKernelHost::default();
        let mut hashes = Vec::new();
        // Store BLOCKS_STORED + 1 blocks; storing block BLOCKS_STORED evicts block 0.
        for number in 0..=(BLOCKS_STORED as u32) {
            hashes.push(store_tez_block(&mut host, number, true));
        }

        // Block 0 fell out of the window and is no longer a valid branch.
        assert!(!is_recent_block_hash(&host, &TEZ_BLOCKS_PATH, &hashes[0]).unwrap());
        // The oldest still-in-window block and the latest one remain members.
        assert!(is_recent_block_hash(&host, &TEZ_BLOCKS_PATH, &hashes[1]).unwrap());
        assert!(is_recent_block_hash(
            &host,
            &TEZ_BLOCKS_PATH,
            &hashes[BLOCKS_STORED as usize]
        )
        .unwrap());
    }

    #[test]
    fn maintain_flag_false_skips_live_blocks() {
        // The EVM path (maintain_live_blocks = false) must not populate live_blocks.
        let mut host = MockKernelHost::default();
        let hash = store_tez_block(&mut host, 0, false);
        assert!(!is_recent_block_hash(&host, &TEZ_BLOCKS_PATH, &hash).unwrap());
    }
}
