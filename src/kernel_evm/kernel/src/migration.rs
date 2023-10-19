// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::error::StorageError::InvalidLoadValue;
use crate::error::UpgradeProcessError::Fallback;
use crate::storage::{
    block_path, index_block, init_blocks_index, init_transaction_hashes_index,
    object_path, read_current_block_number, read_storage_version, receipt_path,
    store_rlp, store_storage_version, STORAGE_VERSION,
};
use ethbloom::Bloom;
use ethereum::Log;
use primitive_types::{H160, H256, U256};
use rlp::{Decodable, DecoderError, Rlp};
use tezos_ethereum::block::L2Block;
use tezos_ethereum::eth_gen::OwnedHash;
use tezos_ethereum::transaction::{
    TransactionHash, TransactionObject, TransactionReceipt, TransactionStatus,
    TransactionType, TRANSACTION_HASH_SIZE,
};
use tezos_ethereum::{
    rlp_helpers::*,
    tx_signature::{rlp_decode_opt, TxSignature},
};
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::runtime::Runtime;

pub enum MigrationStatus {
    None,
    InProgress,
    Done,
}

// Old blocks were serialized using an RLP encoding consisting of the following
// 6 fields:
// [number, hash, parent_hash, transactions, gas_used, timestamp]
// Now, we encode the full 13 fields of the new L2Block struct.
// Therefore, we need to re-introduce the old L2Block struct and it's RLP
// encoding.

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OldL2Block {
    pub number: U256,
    pub hash: H256,
    pub parent_hash: H256,
    pub logs_bloom: Option<OwnedHash>,
    pub transactions_root: OwnedHash,
    pub state_root: OwnedHash,
    pub receipts_root: OwnedHash,
    pub miner: OwnedHash,
    pub extra_data: OwnedHash,
    pub gas_limit: u64,
    pub gas_used: U256,
    pub timestamp: Timestamp,
    pub transactions: Vec<TransactionHash>,
}

impl OldL2Block {
    const DUMMY_HASH: &str = "0000000000000000000000000000000000000000";
    const BLOCK_HASH_SIZE: usize = 32;

    fn dummy_hash() -> OwnedHash {
        OldL2Block::DUMMY_HASH.into()
    }

    fn dummy_block_hash() -> H256 {
        H256([0; OldL2Block::BLOCK_HASH_SIZE])
    }
}

impl Default for OldL2Block {
    fn default() -> Self {
        Self {
            number: U256::default(),
            hash: H256::default(),
            parent_hash: OldL2Block::dummy_block_hash(),
            logs_bloom: None,
            transactions_root: OldL2Block::dummy_hash(),
            state_root: OldL2Block::dummy_hash(),
            receipts_root: OldL2Block::dummy_hash(),
            miner: OldL2Block::dummy_hash(),
            extra_data: OldL2Block::dummy_hash(),
            gas_limit: 1u64,
            gas_used: U256::zero(),
            timestamp: Timestamp::from(0),
            transactions: Vec::new(),
        }
    }
}

impl Decodable for OldL2Block {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        if decoder.is_list() {
            if Ok(6) == decoder.item_count() {
                let mut it = decoder.iter();
                let number: U256 = decode_field_u256_le(&next(&mut it)?, "number")?;
                let hash: H256 = decode_field_h256(&next(&mut it)?, "hash")?;
                let parent_hash: H256 =
                    decode_field_h256(&next(&mut it)?, "parent_hash")?;
                let transactions: Vec<TransactionHash> =
                    decode_transaction_hash_list(&next(&mut it)?, "transactions")?;
                let gas_used: U256 = decode_field_u256_le(&next(&mut it)?, "gas_used")?;
                let timestamp_bytes =
                    decode_field::<Vec<u8>>(&next(&mut it)?, "timestamp")?;
                let timestamp: Timestamp =
                    i64::from_le_bytes(timestamp_bytes.try_into().map_err(|_| {
                        DecoderError::Custom(
                            "Invalid conversion from timestamp vector of bytes to bytes.",
                        )
                    })?)
                    .into();
                Ok(OldL2Block {
                    number,
                    hash,
                    parent_hash,
                    gas_used,
                    timestamp,
                    transactions,
                    ..Default::default()
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }
}

fn migrate_one_block<Host: Runtime>(
    host: &mut Host,
    block_hash: H256,
) -> Result<(), Error> {
    let path = block_path(block_hash)?;
    let bytes = host.store_read_all(&path)?;
    host.store_delete(&path)?;
    // Read old block
    let old_block = OldL2Block::from_rlp_bytes(&bytes)?;
    let new_block = L2Block {
        number: old_block.number,
        hash: old_block.hash,
        parent_hash: old_block.parent_hash,
        logs_bloom: Bloom::default(),
        transactions_root: None,
        state_root: None,
        receipts_root: None,
        miner: None,
        extra_data: None,
        gas_limit: None,
        gas_used: old_block.gas_used,
        timestamp: old_block.timestamp,
        transactions: old_block.transactions,
    };
    // Write new block
    store_rlp(&new_block, host, &path)
}

fn migrate_blocks<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let head = read_current_block_number(host)?.as_u32();
    for number in 0..(head + 1) {
        let hash: H256 = H256(U256::from(number).into());
        migrate_one_block(host, hash)?;
    }
    Ok(())
}

// The genesis block hash 0x00..00, its parent hash is itself.
// Therefore block.0.parent_hash is 0x00..00
//           block.1.parent_hash is 0x00..00
// It's a problem for indexers, they want parent hashes to be unique.
fn replace_genesis_parent_hash<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    // Get the genesis block.
    let hash = H256::zero();
    let path = block_path(hash)?;
    let bytes = host.store_read_all(&path)?;
    let block = L2Block::from_rlp_bytes(&bytes)?;
    // Replace genesis.parent_hash.
    let block = L2Block {
        parent_hash: H256::from_slice(
            &hex::decode(
                "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
            )
            .unwrap(),
        ),
        ..block
    };
    // Write again genesis block. We go through `store_rlp` to avoid
    // an additional indexing of the block.
    store_rlp(&block, host, &path)
}

// When we migrated the blocks in the previous upgrade, we re-indexed by mistake
// all the blocks.
// Therefore:
// - /evm/blocks/indexes/0 -> 0x00..00
// - /evm/blocks/indexes/772940 -> 0x00..00
//
// One other way to understand is: |indexes| = HEAD * 2.
//
// There are two ways to fix this problem.
// 1. Remove the extra indexes and relocate the misplaced ones. Closer to
//    what we would do in production.
// 2. Recompute all the indexes because we know how block hashes work. Much
//    easier to implement because you start from scratch again.
//
// This function fixes the block indexes using the second approach.
fn fix_block_indexes<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let mut index = init_blocks_index()?;

    // Remove current indexes.
    host.store_delete(&index.path)?;

    // Repush all blocks to indexes.
    let head = read_current_block_number(host)?.as_u32();
    for number in 0..(head + 1) {
        let hash: H256 = H256(U256::from(number).into());
        index_block(host, &hash, &mut index)?;
    }

    Ok(())
}

// The Transaction Receipt used to include the block hash.
// This had to be mocked, as the block hash should depend on
// the receipts. This is why this field was removed.
#[derive(Debug, PartialEq, Clone)]
pub struct OldTransactionReceipt {
    pub hash: TransactionHash,
    pub index: u32,
    pub block_hash: H256,
    pub block_number: U256,
    pub from: H160,
    pub to: Option<H160>,
    pub cumulative_gas_used: U256,
    pub effective_gas_price: U256,
    pub gas_used: U256,
    pub contract_address: Option<H160>,
    pub logs: Vec<Log>,
    pub logs_bloom: Bloom,
    pub type_: TransactionType,
    pub status: TransactionStatus,
}

impl Decodable for OldTransactionReceipt {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if Ok(14) != decoder.item_count() {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let hash: TransactionHash = decode_transaction_hash(&next(&mut it)?)?;
        let index: u32 = decode_field(&next(&mut it)?, "index")?;
        let block_hash: H256 = decode_field(&next(&mut it)?, "block_hash")?;
        let block_number: U256 = decode_field_u256_le(&next(&mut it)?, "block_number")?;
        let from: H160 = decode_field(&next(&mut it)?, "from")?;
        let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
        let cumulative_gas_used: U256 =
            decode_field_u256_le(&next(&mut it)?, "cumulative_gas_used")?;
        let effective_gas_price: U256 =
            decode_field_u256_le(&next(&mut it)?, "effective_gas_price")?;
        let gas_used: U256 = decode_field_u256_le(&next(&mut it)?, "gas_used")?;
        let contract_address: Option<H160> =
            decode_option(&next(&mut it)?, "contract_address")?;
        let logs = decode_list(&next(&mut it)?, "logs")?;
        let logs_bloom = decode_field(&next(&mut it)?, "logs_bloom")?;
        let type_: TransactionType = decode_transaction_type(&next(&mut it)?)?;
        let status: TransactionStatus = decode_transaction_status(&next(&mut it)?)?;
        Ok(OldTransactionReceipt {
            hash,
            index,
            block_hash,
            block_number,
            from,
            to,
            cumulative_gas_used,
            effective_gas_price,
            gas_used,
            contract_address,
            logs,
            logs_bloom,
            type_,
            status,
        })
    }
}

// The Transaction Object used to include the block hash.
// This had to be mocked, as the block hash should depend on
// the objects. This is why this field was removed.

#[derive(Debug, PartialEq, Clone)]
pub struct OldTransactionObject {
    pub block_hash: H256,
    pub block_number: U256,
    pub from: H160,
    pub gas_used: U256,
    pub gas_price: U256,
    pub hash: TransactionHash,
    pub input: Vec<u8>,
    pub nonce: U256,
    pub to: Option<H160>,
    pub index: u32,
    pub value: U256,
    pub signature: Option<TxSignature>,
}

impl Decodable for OldTransactionObject {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if decoder.is_list() {
            if Ok(14) == decoder.item_count() {
                let mut it = decoder.iter();
                let block_hash: H256 = decode_field_h256(&next(&mut it)?, "block_hash")?;
                let block_number: U256 =
                    decode_field_u256_le(&next(&mut it)?, "block_number")?;
                let from: H160 = decode_field(&next(&mut it)?, "from")?;
                let gas_used: U256 = decode_field_u256_le(&next(&mut it)?, "gas_used")?;
                let gas_price: U256 = decode_field_u256_le(&next(&mut it)?, "gas_price")?;
                let hash: TransactionHash = decode_transaction_hash(&next(&mut it)?)?;
                let input: Vec<u8> = decode_field(&next(&mut it)?, "input")?;
                let nonce: U256 = decode_field_u256_le(&next(&mut it)?, "nonce")?;
                let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
                let index: u32 = decode_field(&next(&mut it)?, "index")?;
                let value: U256 = decode_field_u256_le(&next(&mut it)?, "value")?;
                let signature = rlp_decode_opt(&mut it)?;
                Ok(OldTransactionObject {
                    block_hash,
                    block_number,
                    from,
                    gas_used,
                    gas_price,
                    hash,
                    input,
                    nonce,
                    to,
                    index,
                    value,
                    signature,
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }
}

fn migrate_one_receipt<Host: Runtime>(
    host: &mut Host,
    receipt_hash: &TransactionHash,
) -> Result<(), Error> {
    let path = receipt_path(receipt_hash)?;
    let bytes = host.store_read_all(&path)?;
    host.store_delete(&path)?;
    // Read old receipt
    let old_receipt = OldTransactionReceipt::from_rlp_bytes(&bytes)?;
    let new_receipt = TransactionReceipt {
        hash: old_receipt.hash,
        index: old_receipt.index,
        block_number: old_receipt.block_number,
        from: old_receipt.from,
        to: old_receipt.to,
        cumulative_gas_used: old_receipt.cumulative_gas_used,
        effective_gas_price: old_receipt.effective_gas_price,
        gas_used: old_receipt.gas_used,
        contract_address: old_receipt.contract_address,
        logs: old_receipt.logs,
        logs_bloom: old_receipt.logs_bloom,
        type_: old_receipt.type_,
        status: old_receipt.status,
    };
    // Write new receipt
    store_rlp(&new_receipt, host, &path)
}

fn migrate_one_object<Host: Runtime>(
    host: &mut Host,
    tx_hash: &TransactionHash,
) -> Result<(), Error> {
    let path = object_path(tx_hash)?;
    let bytes = host.store_read_all(&path)?;
    host.store_delete(&path)?;
    // Read old object
    let old_object = OldTransactionObject::from_rlp_bytes(&bytes)?;
    let new_receipt = TransactionObject {
        block_number: old_object.block_number,
        from: old_object.from,
        gas_used: old_object.gas_used,
        gas_price: old_object.gas_price,
        hash: old_object.hash,
        input: old_object.input,
        nonce: old_object.nonce,
        to: old_object.to,
        index: old_object.index,
        value: old_object.value,
        signature: old_object.signature,
    };
    // Write new receipt
    store_rlp(&new_receipt, host, &path)
}

fn migrate_receipts_and_objects<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let indexed_transaction_hashes = init_transaction_hashes_index()?;
    let tx_len = indexed_transaction_hashes.length(host)?;
    for index in 0..tx_len {
        let bytes = indexed_transaction_hashes.unsafe_get_value(host, index)?;
        let tx_hash: TransactionHash =
            bytes.as_slice().try_into().map_err(|_| InvalidLoadValue {
                expected: TRANSACTION_HASH_SIZE,
                actual: bytes.len(),
            })?;
        migrate_one_receipt(host, &tx_hash)?;
        migrate_one_object(host, &tx_hash)?;
    }
    Ok(())
}

// The workflow for migration is the following:
//
// - bump `storage::STORAGE_VERSION` by one
// - fill the scope inside the conditional in `storage_migration` with all the
//   needed migration functions
// - compile the kernel and run all the E2E migration tests to make sure all the
//   data is still available from the EVM proxy-node.
fn migration<Host: Runtime>(host: &mut Host) -> Result<MigrationStatus, Error> {
    let current_version = read_storage_version(host)?;
    if STORAGE_VERSION == current_version + 1 {
        // MIGRATION CODE - START

        migrate_blocks(host)?;

        migrate_receipts_and_objects(host)?;

        replace_genesis_parent_hash(host)?;

        fix_block_indexes(host)?;

        // MIGRATION CODE - END
        store_storage_version(host, STORAGE_VERSION)?;
        return Ok(MigrationStatus::Done);
    }
    Ok(MigrationStatus::None)
}

pub fn storage_migration<Host: Runtime>(
    host: &mut Host,
) -> Result<MigrationStatus, Error> {
    let migration_result = migration(host);
    migration_result.map_err(|_| {
        // Something went wrong during the migration.
        // The fallback mechanism is triggered to retrograde to the previous kernel.

        Error::UpgradeError(Fallback)
    })
}
