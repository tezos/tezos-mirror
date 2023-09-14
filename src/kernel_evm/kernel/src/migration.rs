// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::cmp;

use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::storage::{
    read_current_block_number, read_storage_version, store_block_by_hash,
    store_current_block, store_storage_version, STORAGE_VERSION,
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
    use primitive_types::U256;
    use tezos_ethereum::block::L2Block;
    use tezos_ethereum::transaction::{TransactionHash, TRANSACTION_HASH_SIZE};
    use tezos_smart_rollup_host::path::*;
    use tezos_smart_rollup_host::runtime::Runtime;

    const BLOCK_TRANSACTIONS: RefPath = RefPath::assert_from(b"/transactions");
    const BLOCK_TIMESTAMP: RefPath = RefPath::assert_from(b"/timestamp");

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
        host.store_delete(&block_path)?;
        Ok(L2Block::new(number, transactions, timestamp))
    }
}

mod migration_block_helpers {
    use crate::{error::Error, storage::BLOCKS_TO_MIGRATE};
    use primitive_types::U256;
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
            host.store_delete(&TMP_BLOCK_MIGRATION).map_err(Error::from)
        } else {
            host.store_move(&TMP_BLOCK_MIGRATION, &BLOCKS_TO_MIGRATE)
                .map_err(Error::from)
        }
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
        // Migrate the upgrade mechanism in the storage.
        // Replace the ticketer address by the exchanger contract.

        // Migrate L2Block storage
        let head_number = read_current_block_number(host)?;
        let next_block_number_to_migrate =
            migration_block_helpers::read_next_block_number_to_migrate(host)?.as_usize();
        if next_block_number_to_migrate == 0 {
            migration_block_helpers::store_old_blocks(host)?
        }

        let max_migration_threshold =
            next_block_number_to_migrate + MAX_MIGRATABLE_BLOCKS_PER_REBOOT - 1;
        let last_block_to_migrate =
            cmp::min(max_migration_threshold, head_number.as_usize());
        let keep_migrating = max_migration_threshold < head_number.as_usize();

        for number in next_block_number_to_migrate..(last_block_to_migrate + 1) {
            let block = old_storage::read_and_remove_l2_block(host, number.into())?;

            if number == head_number.as_usize() {
                // Needed to migrate current hash
                store_current_block(host, &block)?
            } else {
                store_block_by_hash(host, &block)?
            }
        }

        if keep_migrating {
            migration_block_helpers::store_next_block_number_to_migrate(
                host,
                (last_block_to_migrate + 1).into(),
            )?;
            host.mark_for_reboot()?;
            return Ok(MigrationStatus::InProgress);
        } else {
            migration_block_helpers::delete_next_block_number_to_migrate(host)?;

            // MIGRATION CODE - END
            store_storage_version(host, STORAGE_VERSION)?;
            migration_block_helpers::commit_block_migration_changes(host, true)?;
            return Ok(MigrationStatus::Done);
        }
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
