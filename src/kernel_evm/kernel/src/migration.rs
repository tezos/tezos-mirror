// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::cmp;

use crate::error::Error;
use crate::error::StorageError::InvalidLoadValue;
use crate::error::UpgradeProcessError::Fallback;

use crate::storage::{
    block_path, init_blocks_index, init_transaction_hashes_index, object_path,
    read_current_block_number, read_storage_version, receipt_path, store_block_by_hash,
    store_rlp, store_storage_version, EVM_BLOCKS, EVM_TRANSACTIONS_OBJECTS,
    EVM_TRANSACTIONS_RECEIPTS, STORAGE_VERSION,
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
use tezos_ethereum::{rlp_helpers::*, tx_signature::TxSignature};
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};

use tezos_evm_logging::{log, Level::Debug};

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
        transactions_root: L2Block::dummy_hash(),
        state_root: L2Block::dummy_hash(),
        receipts_root: L2Block::dummy_hash(),
        miner: None,
        extra_data: None,
        gas_limit: None,
        gas_used: old_block.gas_used,
        timestamp: old_block.timestamp,
        transactions: old_block.transactions,
    };
    // Write new block
    store_block_by_hash(host, &new_block)
}

pub const MAX_MIGRATABLE_BLOCKS_PER_REBOOT: usize = 5000;

const NEXT_BLOCK_NUMBER_TO_MIGRATE: RefPath =
    RefPath::assert_from(b"/__migration/block_number");

pub fn store_next_block_number_to_migrate<Host: Runtime>(
    host: &mut Host,
    block_number: U256,
) -> Result<(), Error> {
    let mut le_block_number: [u8; 32] = [0; 32];
    block_number.to_little_endian(&mut le_block_number);
    host.store_write_all(&NEXT_BLOCK_NUMBER_TO_MIGRATE, &le_block_number)
        .map_err(Error::from)
}

pub fn read_next_block_number_to_migrate<Host: Runtime>(
    host: &mut Host,
) -> Result<U256, Error> {
    match host.store_read_all(&NEXT_BLOCK_NUMBER_TO_MIGRATE) {
        Ok(next_block_number_to_migrate) => {
            Ok(U256::from_little_endian(&next_block_number_to_migrate))
        }
        Err(RuntimeError::PathNotFound) => Ok(U256::zero()),
        Err(e) => Err(Error::from(e)),
    }
}

pub fn delete_next_block_number_to_migrate<Host: Runtime>(
    host: &mut Host,
) -> Result<(), Error> {
    match host.store_delete(&NEXT_BLOCK_NUMBER_TO_MIGRATE) {
        Ok(()) | Err(RuntimeError::PathNotFound) => Ok(()),
        Err(e) => Err(Error::from(e)),
    }
}

const TMP_BLOCK_MIGRATION: RefPath = RefPath::assert_from(b"/__migration/blocks");

pub fn store_old_blocks<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    host.store_copy(&EVM_BLOCKS, &TMP_BLOCK_MIGRATION)
        .map_err(Error::from)
}

pub fn commit_block_migration_changes<Host: Runtime>(
    host: &mut Host,
    success: bool,
) -> Result<(), Error> {
    if success {
        match host.store_delete(&TMP_BLOCK_MIGRATION) {
            Ok(()) | Err(RuntimeError::PathNotFound) => Ok(()),
            Err(e) => Err(Error::from(e)),
        }
    } else {
        host.store_move(&TMP_BLOCK_MIGRATION, &EVM_BLOCKS)
            .map_err(Error::from)
    }
}

fn migrate_blocks<Host: Runtime>(host: &mut Host) -> Result<MigrationStatus, Error> {
    let head = read_current_block_number(host)?;
    let next_block_number_to_migrate =
        read_next_block_number_to_migrate(host)?.as_usize();

    // There is nothing to migrate anymore
    if next_block_number_to_migrate > head.as_usize() {
        return Ok(MigrationStatus::Done);
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
    // The indexes are reset at the beginning of the migration
    if next_block_number_to_migrate == 0 {
        let index = init_blocks_index()?;
        host.store_delete(&index.path)?;
        store_old_blocks(host)?;
    }

    let max_migration_threshold =
        next_block_number_to_migrate + MAX_MIGRATABLE_BLOCKS_PER_REBOOT - 1;
    let last_block_to_migrate = cmp::min(max_migration_threshold, head.as_usize());

    log!(
        host,
        Debug,
        "Migration of L2 blocks from {} to {} begins",
        next_block_number_to_migrate,
        last_block_to_migrate
    );
    for number in next_block_number_to_migrate..=last_block_to_migrate {
        let hash: H256 = H256(U256::from(number).into());
        migrate_one_block(host, hash)?;
    }
    log!(
        host,
        Debug,
        "L2 blocks from {} to {} have been migrated",
        next_block_number_to_migrate,
        last_block_to_migrate
    );

    store_next_block_number_to_migrate(host, (last_block_to_migrate + 1).into())?;

    host.mark_for_reboot()?;
    Ok(MigrationStatus::InProgress)
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
                let v_bytes: Vec<u8> = next(&mut it)?.as_val()?;
                let v: U256 = U256::from_big_endian(&v_bytes);
                let r: H256 = decode_field_h256(&next(&mut it)?, "r")?;
                let s: H256 = decode_field_h256(&next(&mut it)?, "s")?;
                let signature = TxSignature::new(v, r, s).ok();
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

pub const MAX_MIGRATABLE_TRANSACTION_PER_REBOOT: u64 = 2000;

const NEXT_TRANSACTION_NUMBER_TO_MIGRATE: RefPath =
    RefPath::assert_from(b"/__migration/transaction_number");

pub fn store_next_transaction_number_to_migrate<Host: Runtime>(
    host: &mut Host,
    transaction_number: U256,
) -> Result<(), Error> {
    let mut le_transaction_number: [u8; 32] = [0; 32];
    transaction_number.to_little_endian(&mut le_transaction_number);
    host.store_write_all(&NEXT_TRANSACTION_NUMBER_TO_MIGRATE, &le_transaction_number)
        .map_err(Error::from)
}

pub fn read_next_transaction_number_to_migrate<Host: Runtime>(
    host: &mut Host,
) -> Result<U256, Error> {
    match host.store_read_all(&NEXT_TRANSACTION_NUMBER_TO_MIGRATE) {
        Ok(next_transaction_number_to_migrate) => Ok(U256::from_little_endian(
            &next_transaction_number_to_migrate,
        )),
        Err(RuntimeError::PathNotFound) => Ok(U256::zero()),
        Err(e) => Err(Error::from(e)),
    }
}

pub fn delete_next_transaction_number_to_migrate<Host: Runtime>(
    host: &mut Host,
) -> Result<(), Error> {
    match host.store_delete(&NEXT_TRANSACTION_NUMBER_TO_MIGRATE) {
        Ok(()) | Err(RuntimeError::PathNotFound) => Ok(()),
        Err(e) => Err(Error::from(e)),
    }
}

const TMP_OBJECTS_MIGRATION: RefPath =
    RefPath::assert_from(b"/__migration/transactions_objects");

pub fn store_old_objects<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    host.store_copy(&EVM_TRANSACTIONS_OBJECTS, &TMP_OBJECTS_MIGRATION)
        .map_err(Error::from)
}

pub fn commit_tx_objects_changes<Host: Runtime>(
    host: &mut Host,
    success: bool,
) -> Result<(), Error> {
    if success {
        match host.store_delete(&TMP_RECEIPTS_MIGRATION) {
            Ok(()) | Err(RuntimeError::PathNotFound) => Ok(()),
            Err(e) => Err(Error::from(e)),
        }
    } else {
        host.store_move(&TMP_OBJECTS_MIGRATION, &EVM_TRANSACTIONS_OBJECTS)
            .map_err(Error::from)
    }
}

const TMP_RECEIPTS_MIGRATION: RefPath =
    RefPath::assert_from(b"/__migration/transactions_receipts");

pub fn store_old_receipts<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    host.store_copy(&EVM_TRANSACTIONS_RECEIPTS, &TMP_RECEIPTS_MIGRATION)
        .map_err(Error::from)
}

pub fn commit_tx_receipts_changes<Host: Runtime>(
    host: &mut Host,
    success: bool,
) -> Result<(), Error> {
    if success {
        match host.store_delete(&TMP_RECEIPTS_MIGRATION) {
            Ok(()) | Err(RuntimeError::PathNotFound) => Ok(()),
            Err(e) => Err(Error::from(e)),
        }
    } else {
        host.store_move(&TMP_RECEIPTS_MIGRATION, &EVM_TRANSACTIONS_RECEIPTS)
            .map_err(Error::from)
    }
}

fn migrate_receipts_and_objects<Host: Runtime>(
    host: &mut Host,
) -> Result<MigrationStatus, Error> {
    let indexed_transaction_hashes = init_transaction_hashes_index()?;
    let tx_len = indexed_transaction_hashes.length(host)?;

    let next_transaction_number_to_migrate =
        read_next_transaction_number_to_migrate(host)?.as_u64();

    if next_transaction_number_to_migrate == 0 {
        store_old_objects(host)?;
        store_old_receipts(host)?;
    }

    // There is nothing to migrate anymore
    if next_transaction_number_to_migrate >= tx_len {
        return Ok(MigrationStatus::Done);
    }

    let max_migration_threshold =
        next_transaction_number_to_migrate + MAX_MIGRATABLE_TRANSACTION_PER_REBOOT - 1;
    let last_transaction_to_migrate = cmp::min(max_migration_threshold, tx_len - 1);

    log!(
        host,
        Debug,
        "Migration of transactions from {} to {} begins",
        next_transaction_number_to_migrate,
        last_transaction_to_migrate
    );
    for index in next_transaction_number_to_migrate..=last_transaction_to_migrate {
        let bytes = indexed_transaction_hashes.unsafe_get_value(host, index)?;
        let tx_hash: TransactionHash =
            bytes.as_slice().try_into().map_err(|_| InvalidLoadValue {
                expected: TRANSACTION_HASH_SIZE,
                actual: bytes.len(),
            })?;
        let left = migrate_one_receipt(host, &tx_hash);
        let right = migrate_one_object(host, &tx_hash);
        match (left, right) {
            (Ok(()), Ok(())) => (),
            // We have transactions indexed multiple times, the
            // migration fail for both the receipt and the object
            // because we try to migrate twice.
            (Err(_), Err(_)) => (),
            (Err(err), _) | (_, Err(err)) => return Err(err),
        }
    }
    log!(
        host,
        Debug,
        "Transactions from {} to {} have been migrated",
        next_transaction_number_to_migrate,
        last_transaction_to_migrate
    );

    store_next_transaction_number_to_migrate(
        host,
        (last_transaction_to_migrate + 1).into(),
    )?;

    host.mark_for_reboot()?;
    Ok(MigrationStatus::InProgress)
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

        match migrate_receipts_and_objects(host)? {
            MigrationStatus::Done | MigrationStatus::None => (),
            MigrationStatus::InProgress => return Ok(MigrationStatus::InProgress),
        }

        match migrate_blocks(host)? {
            MigrationStatus::Done | MigrationStatus::None => (),
            MigrationStatus::InProgress => return Ok(MigrationStatus::InProgress),
        }

        replace_genesis_parent_hash(host)?;

        // At the end, after blocks and transaction migrations are done we have to clean
        // up the temporary paths used for migration
        delete_next_transaction_number_to_migrate(host)?;
        commit_tx_objects_changes(host, true)?;
        commit_tx_receipts_changes(host, true)?;
        delete_next_block_number_to_migrate(host)?;
        commit_block_migration_changes(host, true)?;

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
        let _ = commit_block_migration_changes(host, false);
        let _ = commit_tx_objects_changes(host, false);
        let _ = commit_tx_receipts_changes(host, false);

        Error::UpgradeError(Fallback)
    })
}
