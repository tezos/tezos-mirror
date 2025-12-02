// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H256, U256};
use revm_etherlink::storage::block::BLOCKS_STORED;
use tezos_ethereum::{
    block::EthBlock,
    transaction::{TransactionObject, TransactionReceipt},
};
use tezos_evm_logging::{log, Level::Info};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::host::RuntimeError;
use tezos_smart_rollup_host::path::{concat, Path, RefPath};
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
    const INDEXES_PATH: RefPath = RefPath::assert_from(b"/indexes/blocks");

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
}

pub fn store_current<Host: Runtime>(
    host: &mut Host,
    root: &impl Path,
    block: &L2Block,
) -> Result<(), crate::Error> {
    store_current_number(host, root, block.number())?;
    write_h256_be(host, &path::current_hash(root)?, block.hash())?;
    update_block_indexes(host, root, block)?;
    host.store_write_all(&path::current_block(root)?, &block.to_bytes()?)?;
    log!(
        host,
        Info,
        "Storing block {} at {} containing {} transaction(s) for {} gas used.",
        block.number(),
        block.timestamp(),
        block.number_of_transactions(),
        U256::to_string(&block.gas_used())
    );
    Ok(())
}

pub(crate) fn update_block_indexes<Host: Runtime>(
    host: &mut Host,
    root: &impl Path,
    block: &L2Block,
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
        match host.store_delete(&old_path) {
            Ok(_) | Err(RuntimeError::PathNotFound) => {}
            Err(err) => return Err(err.into()),
        }
    }
    host.store_write_all(&full_path, block.hash().as_bytes())?;
    Ok(())
}

pub(crate) fn store_current_number<Host: Runtime>(
    host: &mut Host,
    root: &impl Path,
    number: U256,
) -> Result<(), crate::Error> {
    write_u256_le(host, &path::current_number(root)?, number)?;
    Ok(())
}

pub fn store_current_transactions_objects<Host: Runtime>(
    host: &mut Host,
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

pub fn store_current_transactions_receipts<Host: Runtime>(
    host: &mut Host,
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

pub fn read_current_transactions_receipts<Host: Runtime>(
    host: &Host,
    root: &impl Path,
) -> Result<Vec<TransactionReceipt>, crate::Error> {
    let receipts_bytes =
        host.store_read_all(&path::current_block_transactions_receipts(root)?)?;
    let receipts = rlp::Rlp::new(&receipts_bytes).as_list::<TransactionReceipt>()?;
    Ok(receipts)
}

pub fn read_current_transactions_objects<Host: Runtime>(
    host: &Host,
    root: &impl Path,
) -> Result<Vec<TransactionObject>, crate::Error> {
    let transactions_objects_bytes =
        host.store_read_all(&path::current_block_transactions_objects(root)?)?;
    let transactions_objects =
        rlp::Rlp::new(&transactions_objects_bytes).as_list::<TransactionObject>()?;
    Ok(transactions_objects)
}

pub fn read_current_etherlink_block(host: &mut impl Runtime) -> anyhow::Result<EthBlock> {
    let block_path = path::current_block(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)?;
    let bytes = &host.store_read_all(&block_path)?;
    let block_from_bytes = EthBlock::from_bytes(bytes)?;
    Ok(block_from_bytes)
}

pub fn read_current_number(
    host: &impl Runtime,
    root: &impl Path,
) -> Result<U256, crate::Error> {
    Ok(read_u256_le(host, &path::current_number(root)?)?)
}

pub fn read_current_hash(
    host: &impl Runtime,
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
pub mod internal_for_tests {
    use tezos_ethereum::transaction::{TransactionHash, TransactionStatus};

    use crate::{chains::ETHERLINK_SAFE_STORAGE_ROOT_PATH, error::Error};

    use super::*;

    pub fn store_current_number<Host: Runtime>(
        host: &mut Host,
        root: &impl Path,
        number: U256,
    ) -> Result<(), crate::Error> {
        super::store_current_number(host, root, number)
    }

    pub fn read_transaction_receipt(
        host: &mut impl Runtime,
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
    pub fn read_transaction_receipt_status<Host: Runtime>(
        host: &mut Host,
        tx_hash: &TransactionHash,
    ) -> Result<TransactionStatus, Error> {
        let receipt = read_transaction_receipt(host, tx_hash)?;
        Ok(receipt.status)
    }
}
