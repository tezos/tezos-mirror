// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H256, U256};
use tezos_evm_logging::{
    log,
    Level::{Debug, Info},
};
use tezos_evm_runtime::runtime::Runtime;
use tezos_indexable_storage::IndexableStorage;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::path::{concat, Path};
use tezos_storage::{read_h256_be, read_u256_le, write_h256_be, write_u256_le};

use crate::storage::EVM_TRANSACTIONS_OBJECTS;
use crate::storage::EVM_TRANSACTIONS_RECEIPTS;
use crate::{chains::ChainFamily, l2block::L2Block, migration::allow_path_not_found};

mod path {
    use tezos_smart_rollup_host::path::Path;

    use super::*;

    const CURRENT_NUMBER: RefPath = RefPath::assert_from(b"/blocks/current/number");

    pub fn current_number(
        root: &impl Path,
    ) -> Result<OwnedPath, tezos_smart_rollup_host::path::PathError> {
        concat(root, &CURRENT_NUMBER)
    }

    const CURRENT_HASH: RefPath = RefPath::assert_from(b"/blocks/current/hash");

    pub fn current_hash(
        root: &impl Path,
    ) -> Result<OwnedPath, tezos_smart_rollup_host::path::PathError> {
        concat(root, &CURRENT_HASH)
    }

    pub fn blocks(
        root: &impl Path,
    ) -> Result<OwnedPath, tezos_smart_rollup_host::path::PathError> {
        concat(root, &RefPath::assert_from(b"/blocks"))
    }

    const INDEXES: RefPath = RefPath::assert_from(b"/indexes/blocks");

    pub fn indexes(
        root: &impl Path,
    ) -> Result<OwnedPath, tezos_smart_rollup_host::path::PathError> {
        concat(root, &INDEXES)
    }

    /// Path to the block in the storage. The path to the block is
    /// indexed by its hash.
    pub fn path(root: &impl Path, hash: H256) -> anyhow::Result<OwnedPath> {
        let hash = hex::encode(hash);
        let raw_hash_path: Vec<u8> = format!("/{}", &hash).into();
        let hash_path = OwnedPath::try_from(raw_hash_path)?;
        Ok(concat(&blocks(root)?, &hash_path)?)
    }
}

fn store_current_number(
    host: &mut impl Runtime,
    root: &impl Path,
    number: U256,
) -> anyhow::Result<()> {
    Ok(write_u256_le(host, &path::current_number(root)?, number)?)
}

fn store_current_hash(
    host: &mut impl Runtime,
    root: &impl Path,
    hash: H256,
) -> anyhow::Result<()> {
    write_h256_be(host, &path::current_hash(root)?, hash)
}

fn store_block(
    host: &mut impl Runtime,
    root: &impl Path,
    block: &L2Block,
    index_block: bool,
) -> anyhow::Result<()> {
    if index_block {
        // Index the block, /evm/world_state/indexes/blocks/<n> points to
        // the block hash.
        let index = IndexableStorage::new_owned_path(path::indexes(root)?);
        index.push_value(host, block.hash().as_bytes())?;
    }
    let path = path::path(root, block.hash())?;
    let bytes = block.to_bytes()?;
    Ok(host.store_write_all(&path, &bytes)?)
}

fn store_current_index_or_not(
    host: &mut impl Runtime,
    root: &impl Path,
    block: &L2Block,
    index_block: bool,
) -> anyhow::Result<()> {
    store_current_number(host, root, block.number())?;
    store_current_hash(host, root, block.hash())?;
    store_block(host, root, block, index_block)?;
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

pub fn store_current(
    host: &mut impl Runtime,
    root: &impl Path,
    block: &L2Block,
) -> anyhow::Result<()> {
    store_current_index_or_not(host, root, block, true)
}

pub fn restore_current(
    host: &mut impl Runtime,
    root: &impl Path,
    block: &L2Block,
) -> anyhow::Result<()> {
    store_current_index_or_not(host, root, block, false)
}

pub fn read_current_number(
    host: &impl Runtime,
    root: &impl Path,
) -> anyhow::Result<U256> {
    Ok(read_u256_le(host, &path::current_number(root)?)?)
}

pub fn read_current_hash(host: &impl Runtime, root: &impl Path) -> anyhow::Result<H256> {
    read_h256_be(host, &path::current_hash(root)?)
}

pub fn read_current(
    host: &mut impl Runtime,
    root: &impl Path,
    chain_family: &ChainFamily,
) -> anyhow::Result<L2Block> {
    let hash = read_current_hash(host, root)?;
    let block_path = path::path(root, hash)?;
    let bytes = &host.store_read_all(&block_path)?;
    let block_from_bytes = L2Block::try_from_bytes(chain_family, bytes)?;
    Ok(block_from_bytes)
}

pub fn garbage_collect_blocks(
    host: &mut impl Runtime,
    root: &impl Path,
    chain_family: &ChainFamily,
) -> anyhow::Result<()> {
    log!(host, Debug, "Garbage collecting blocks.");
    if let Ok(block) = read_current(host, root, chain_family) {
        // The kernel needs the current block to process the next one. Therefore
        // we garbage collect everything but the current block.
        host.store_delete(&path::blocks(root)?)?;
        restore_current(host, root, &block)?;
        // Clean all transactions, they are unused by the kernel.
        allow_path_not_found(host.store_delete(&EVM_TRANSACTIONS_OBJECTS))?;
        allow_path_not_found(host.store_delete(&EVM_TRANSACTIONS_RECEIPTS))?;
    }
    Ok(())
}

#[cfg(test)]
pub mod internal_for_tests {
    use super::*;

    pub fn init_blocks_index(root: &impl Path) -> anyhow::Result<IndexableStorage> {
        Ok(IndexableStorage::new_owned_path(path::indexes(root)?))
    }

    pub fn store_current_number(
        host: &mut impl Runtime,
        root: &impl Path,
        number: U256,
    ) -> anyhow::Result<()> {
        super::store_current_number(host, root, number)
    }
}
