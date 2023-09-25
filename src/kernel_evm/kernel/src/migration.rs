// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::storage::{
    read_storage_version, store_storage_version, ADMIN_TO_MIGRATE, STORAGE_VERSION,
};
use tezos_smart_rollup_host::runtime::Runtime;

// The lowest(*) benchmarked number was 14453, but we let some margin in case
// there is some unexpected overhead.
// Other reason is that during the reboots, some block can be bigger than others
// resulting in more ticks consumed.
//
// (*): lowest meaning "before reaching the maximum number of ticks"
pub const MAX_MIGRATABLE_BLOCKS_PER_REBOOT: usize = 10000;

pub enum MigrationStatus {
    None,
    InProgress,
    Done,
}

mod old_storage {
    use crate::error::Error;
    use crate::storage::{store_read_slice, EVM_BLOCKS};
    use crate::OwnedPath;
    use crate::Timestamp;
    use primitive_types::{H160, H256, U256};
    use rlp::{DecoderError, Rlp};
    use tezos_ethereum::block::L2Block;
    use tezos_ethereum::rlp_helpers::{
        decode_field, decode_field_u256_le, decode_option, decode_transaction_hash,
        decode_transaction_status, decode_transaction_type, next,
    };
    use tezos_ethereum::transaction::{
        TransactionHash, TransactionReceipt, TransactionStatus, TransactionType,
        TRANSACTION_HASH_SIZE,
    };
    use tezos_ethereum::Bloom;
    use tezos_smart_rollup_host::path::*;
    use tezos_smart_rollup_host::runtime::Runtime;

    const BLOCK_TRANSACTIONS: RefPath = RefPath::assert_from(b"/transactions");
    const BLOCK_TIMESTAMP: RefPath = RefPath::assert_from(b"/timestamp");

    pub const DICTATOR: RefPath = RefPath::assert_from(b"/dictator_key");

    fn block_path(number: U256) -> Result<OwnedPath, Error> {
        let number: &str = &number.to_string();
        let raw_hash_path: Vec<u8> = format!("/{}", &number).into();
        let hash_path = OwnedPath::try_from(raw_hash_path)?;
        concat(&EVM_BLOCKS, &hash_path).map_err(Error::from)
    }

    fn read_timestamp_path<Host: Runtime>(
        host: &mut Host,
        path: &OwnedPath,
    ) -> Result<Timestamp, Error> {
        let mut buffer = [0u8; 8];
        store_read_slice(host, path, &mut buffer, 8)?;
        let timestamp_as_i64 = i64::from_le_bytes(buffer);
        Ok(timestamp_as_i64.into())
    }

    fn read_nth_block_timestamp<Host: Runtime>(
        host: &mut Host,
        block_path: &OwnedPath,
    ) -> Result<Timestamp, Error> {
        let path = concat(block_path, &BLOCK_TIMESTAMP)?;
        let timestamp = read_timestamp_path(host, &path)?;
        Ok(timestamp)
    }

    fn read_nth_block_transactions<Host: Runtime>(
        host: &mut Host,
        block_path: &OwnedPath,
    ) -> Result<Vec<TransactionHash>, Error> {
        let path: OwnedPath = concat(block_path, &BLOCK_TRANSACTIONS)?;
        let transactions_bytes = host.store_read_all(&path)?;
        host.store_delete(&path)?;
        Ok(transactions_bytes
            .chunks(TRANSACTION_HASH_SIZE)
            .filter_map(|tx_hash_bytes: &[u8]| -> Option<TransactionHash> {
                tx_hash_bytes.try_into().ok()
            })
            .collect::<Vec<TransactionHash>>())
    }

    pub fn read_and_remove_l2_block<Host: Runtime>(
        host: &mut Host,
        number: U256,
    ) -> Result<L2Block, Error> {
        let block_path = block_path(number)?;
        let transactions = read_nth_block_transactions(host, &block_path)?;
        let timestamp = read_nth_block_timestamp(host, &block_path)?;
        let parent_hash = if number == U256::zero() {
            H256::zero()
        } else {
            H256((number - 1).into())
        };
        host.store_delete(&block_path)?;
        Ok(L2Block::new(number, transactions, timestamp, parent_hash))
    }

    // Decode previous version of the receipt,
    // only actually used in the migration code
    pub fn rlp_decode_txreceipt_deprecated(
        bytes: &[u8],
    ) -> Result<TransactionReceipt, DecoderError> {
        let decoder = Rlp::new(bytes);
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if Ok(12) != decoder.item_count() {
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
        let type_: TransactionType = decode_transaction_type(&next(&mut it)?)?;
        let status: TransactionStatus = decode_transaction_status(&next(&mut it)?)?;
        Ok(TransactionReceipt {
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
            logs: vec![],
            logs_bloom: Bloom::default(),
            type_,
            status,
        })
    }
}

mod migration_block_helpers {
    use std::cmp;

    use super::{MigrationStatus, MAX_MIGRATABLE_BLOCKS_PER_REBOOT};
    use crate::{
        error::Error,
        migration::old_storage,
        storage::{
            read_current_block_number, store_block_by_hash, store_current_block,
            BLOCKS_TO_MIGRATE,
        },
    };
    use primitive_types::U256;
    use tezos_evm_logging::log;
    use tezos_evm_logging::Level::{Debug, Info};
    use tezos_smart_rollup_host::{
        path::RefPath,
        runtime::{Runtime, RuntimeError},
    };

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
        host.store_copy(&BLOCKS_TO_MIGRATE, &TMP_BLOCK_MIGRATION)
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
            host.store_move(&TMP_BLOCK_MIGRATION, &BLOCKS_TO_MIGRATE)
                .map_err(Error::from)
        }
    }

    pub fn migrate_l2_blocks<Host: Runtime>(
        host: &mut Host,
    ) -> Result<MigrationStatus, Error> {
        // Migrate L2Block storage
        let head_number = read_current_block_number(host)?;
        let next_block_number_to_migrate =
            read_next_block_number_to_migrate(host)?.as_usize();

        // There is nothing to migrate anymore
        if next_block_number_to_migrate > head_number.as_usize() {
            return Ok(MigrationStatus::Done);
        }

        let max_migration_threshold =
            next_block_number_to_migrate + MAX_MIGRATABLE_BLOCKS_PER_REBOOT - 1;
        let last_block_to_migrate =
            cmp::min(max_migration_threshold, head_number.as_usize());

        if next_block_number_to_migrate == 0 {
            store_old_blocks(host)?
        }

        log!(
            host,
            Debug,
            "Migration of L2 blocks from {} to {} begins",
            next_block_number_to_migrate,
            last_block_to_migrate
        );
        for number in next_block_number_to_migrate..=last_block_to_migrate {
            let block = old_storage::read_and_remove_l2_block(host, number.into())?;

            if number == head_number.as_usize() {
                // Needed to migrate current hash
                store_current_block(host, &block)?
            } else {
                store_block_by_hash(host, &block)?
            }
        }

        log!(
            host,
            Info,
            "L2 blocks from {} to {} have been migrated",
            next_block_number_to_migrate,
            last_block_to_migrate
        );

        store_next_block_number_to_migrate(host, (last_block_to_migrate + 1).into())?;

        host.mark_for_reboot()?;
        Ok(MigrationStatus::InProgress)
    }
}

mod migration_receipts_helpers {
    // This number of transaction receipts correspond to 7,8 billion ticks
    pub const MAX_MIGRATABLE_RECEIPTS_PER_REBOOT: usize = 10000;

    use std::cmp;

    use crate::{
        error::Error,
        migration::old_storage::rlp_decode_txreceipt_deprecated,
        storage::{init_transaction_hashes_index, receipt_path, TX_RECEIPTS_TO_MIGRATE},
    };
    use primitive_types::{H256, U256};
    use rlp::Encodable;
    use tezos_evm_logging::log;
    use tezos_evm_logging::Level::{Debug, Info};
    use tezos_smart_rollup_host::{
        path::RefPath,
        runtime::{Runtime, RuntimeError},
    };

    use super::MigrationStatus;

    const NEXT_TX_ID_TO_MIGRATE: RefPath =
        RefPath::assert_from(b"/__migration/transactions_receipts/next_tx_id");

    pub fn store_next_tx_id_to_migrate<Host: Runtime>(
        host: &mut Host,
        block_number: U256,
    ) -> Result<(), Error> {
        let mut le_txid: [u8; 32] = [0; 32];
        block_number.to_little_endian(&mut le_txid);
        host.store_write_all(&NEXT_TX_ID_TO_MIGRATE, &le_txid)
            .map_err(Error::from)
    }

    pub fn read_next_tx_id_to_migrate<Host: Runtime>(
        host: &mut Host,
    ) -> Result<U256, Error> {
        match host.store_read_all(&NEXT_TX_ID_TO_MIGRATE) {
            Ok(next_tx_id_to_migrate) => {
                Ok(U256::from_little_endian(&next_tx_id_to_migrate))
            }
            Err(RuntimeError::PathNotFound) => Ok(U256::zero()),
            Err(e) => Err(Error::from(e)),
        }
    }

    pub fn delete_next_tx_id_to_migrate<Host: Runtime>(
        host: &mut Host,
    ) -> Result<(), Error> {
        match host.store_delete(&NEXT_TX_ID_TO_MIGRATE) {
            Ok(()) | Err(RuntimeError::PathNotFound) => Ok(()),
            Err(e) => Err(Error::from(e)),
        }
    }

    const TMP_RECEIPTS_MIGRATION: RefPath =
        RefPath::assert_from(b"/__migration/transactions_receipts");

    pub fn store_old_receipts<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
        host.store_copy(&TX_RECEIPTS_TO_MIGRATE, &TMP_RECEIPTS_MIGRATION)
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
            host.store_move(&TMP_RECEIPTS_MIGRATION, &TX_RECEIPTS_TO_MIGRATE)
                .map_err(Error::from)
        }
    }

    pub fn migrate_tx_receipts<Host: Runtime>(
        host: &mut Host,
    ) -> Result<MigrationStatus, Error> {
        let transaction_hashes_index = init_transaction_hashes_index()?;
        let transactions_n = transaction_hashes_index.length(host)? as usize;

        let next_tx_id_to_migrate = read_next_tx_id_to_migrate(host)?.as_usize();
        // There is nothing to migrate anymore.
        // if transactions_n = 0 this condition inevitably holds
        if next_tx_id_to_migrate >= transactions_n {
            return Ok(MigrationStatus::Done);
        }

        if next_tx_id_to_migrate == 0 {
            store_old_receipts(host)?
        }

        let max_migration_threshold =
            next_tx_id_to_migrate + MAX_MIGRATABLE_RECEIPTS_PER_REBOOT - 1;
        let last_tx_id_to_migrate = cmp::min(max_migration_threshold, transactions_n - 1);

        // Proceed to the transaction receipts migration
        log!(
            host,
            Debug,
            "Migration of transaction receipts from {} to {} begins",
            next_tx_id_to_migrate,
            last_tx_id_to_migrate
        );

        for i in next_tx_id_to_migrate..=last_tx_id_to_migrate {
            let tx_hash_raw =
                transaction_hashes_index.unsafe_get_value(host, i as u64)?;
            let tx_hash = H256::from_slice(&tx_hash_raw).to_fixed_bytes();
            let receipt_path = receipt_path(&tx_hash)?;
            let bytes = host.store_read_all(&receipt_path)?;
            let receipt = rlp_decode_txreceipt_deprecated(&bytes)?;
            host.store_write_all(&receipt_path, &receipt.rlp_bytes())?;
        }

        log!(
            host,
            Info,
            "Transaction receipts from {} to {} have been migrated",
            next_tx_id_to_migrate,
            last_tx_id_to_migrate
        );

        store_next_tx_id_to_migrate(host, (last_tx_id_to_migrate + 1).into())?;

        host.mark_for_reboot()?;
        Ok(MigrationStatus::InProgress)
    }
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

        // TODO: https://gitlab.com/tezos/tezos/-/issues/6282
        // Replace the ticketer address by the exchanger contract.

        match migration_block_helpers::migrate_l2_blocks(host)? {
            MigrationStatus::InProgress => return Ok(MigrationStatus::InProgress),
            MigrationStatus::None | MigrationStatus::Done => (),
        }

        // Proceed to the tx receipts migration if blocks migration has been completed
        match migration_receipts_helpers::migrate_tx_receipts(host)? {
            MigrationStatus::InProgress => return Ok(MigrationStatus::InProgress),
            MigrationStatus::None | MigrationStatus::Done => (),
        }

        // At the end, after blocks and receipts migrations are done we have to clean
        // up the temporary paths used for migration
        migration_block_helpers::delete_next_block_number_to_migrate(host)?;
        migration_receipts_helpers::delete_next_tx_id_to_migrate(host)?;

        // Replace the dictator key by the administrator contract.
        host.store_delete(&old_storage::DICTATOR)?;
        // KT1Xu4UMxzdhk3tJwa73bXNwBBR3jVdN7QQ2
        let new_admin = hex::decode(
            "4B5431587534554D787A64686B33744A7761373362584E77424252336A56644E37515132",
        )
        .unwrap();
        host.store_write_all(&ADMIN_TO_MIGRATE, &new_admin)?;

        // MIGRATION CODE - END
        store_storage_version(host, STORAGE_VERSION)?;
        migration_block_helpers::commit_block_migration_changes(host, true)?;
        migration_receipts_helpers::commit_tx_receipts_changes(host, true)?;
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

        // We revert every changes made by the migration.
        // The following **CAN NOT** fail. We can not recover from it if
        // an error occurs.
        let _ = migration_block_helpers::commit_block_migration_changes(host, false);

        Error::UpgradeError(Fallback)
    })
}

#[cfg(test)]
mod test {
    use primitive_types::{H160, H256, U256};
    use tezos_ethereum::{
        transaction::{
            TransactionReceipt, TransactionStatus, TransactionType, TRANSACTION_HASH_SIZE,
        },
        Bloom,
    };

    use crate::migration::old_storage::rlp_decode_txreceipt_deprecated;

    fn address_of_str(s: &str) -> H160 {
        let data = &hex::decode(s).unwrap();
        H160::from_slice(data)
    }

    #[test]
    fn test_receipt_decoding_deprecated() {
        let receipt = TransactionReceipt {
            hash: [0; TRANSACTION_HASH_SIZE],
            index: 15u32,
            block_hash: H256::default(),
            block_number: U256::from(42),
            from: address_of_str("3535353535353535353535353535353535353535"),
            to: Some(address_of_str("3635353535353535353535353535353535353536")),
            cumulative_gas_used: U256::from(1252345235),
            effective_gas_price: U256::from(47457345),
            gas_used: U256::from(474573452),
            contract_address: Some(address_of_str(
                "4335353535353535353535353535353535353543",
            )),
            type_: TransactionType::Legacy,
            logs_bloom: Bloom::default(),
            logs: vec![],
            status: TransactionStatus::Success,
        };

        let deprecated_encoding = "f90108a000000000000000000000000000000000000000000000000000000000000000000fa00000000000000000000000000000000000000000000000000000000000000000a02a00000000000000000000000000000000000000000000000000000000000000943535353535353535353535353535353535353535943635353535353535353535353535353535353536a09345a54a00000000000000000000000000000000000000000000000000000000a04124d40200000000000000000000000000000000000000000000000000000000a08c6a491c000000000000000000000000000000000000000000000000000000009443353535353535353535353535353535353535438001";
        let raw_bytes = hex::decode(deprecated_encoding).expect("Should be valid hex");
        assert_eq!(Ok(receipt), rlp_decode_txreceipt_deprecated(&raw_bytes));
    }
}
