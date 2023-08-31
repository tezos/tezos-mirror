// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::storage::{
    read_current_block_number, read_storage_version, store_block_by_number,
    store_storage_version, STORAGE_VERSION,
};
use tezos_smart_rollup_host::runtime::Runtime;

mod old_storage {
    use crate::error::Error;
    use crate::storage::{block_path, store_read_slice};
    use crate::OwnedPath;
    use crate::Timestamp;
    use primitive_types::U256;
    use tezos_ethereum::block::L2Block;
    use tezos_ethereum::transaction::{TransactionHash, TRANSACTION_HASH_SIZE};
    use tezos_smart_rollup_host::path::*;
    use tezos_smart_rollup_host::runtime::Runtime;

    const BLOCK_TRANSACTIONS: RefPath = RefPath::assert_from(b"/transactions");
    const BLOCK_TIMESTAMP: RefPath = RefPath::assert_from(b"/timestamp");

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

// The workflow for migration is the following:
//
// - bump `storage::STORAGE_VERSION` by one
// - fill the scope inside the conditional in `storage_migration` with all the
//   needed migration functions
// - compile the kernel and run all the E2E migration tests to make sure all the
//   data is still available from the EVM proxy-node.
fn migration<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let current_version = read_storage_version(host)?;
    if STORAGE_VERSION == current_version + 1 {
        // MIGRATION CODE - START

        // TODO: https://gitlab.com/tezos/tezos/-/issues/6282
        // Migrate the upgrade mechanism in the storage.
        // Replace the ticketer address by the exchanger contract.

        // Migrate L2Block storage
        let head_number = read_current_block_number(host)?;
        for number in 0..(head_number.as_usize() + 1) {
            let block = old_storage::read_and_remove_l2_block(host, number.into())?;
            store_block_by_number(host, &block)?
        }
        // MIGRATION CODE - END
        store_storage_version(host, STORAGE_VERSION)?
    }
    Ok(())
}

pub fn storage_migration<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    if migration(host).is_err() {
        // Something went wrong during the migration.
        // The fallback mechanism is triggered to retrograde to the previous kernel.
        Err(Error::UpgradeError(Fallback))?
    }
    Ok(())
}
