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
use tezos_smart_rollup_host::path::concat;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::path::RefPath;
use tezos_storage::{read_h256_be, read_u256_le, write_h256_be, write_u256_le};

use crate::storage::EVM_TRANSACTIONS_OBJECTS;
use crate::storage::EVM_TRANSACTIONS_RECEIPTS;
use crate::{chains::ChainFamily, l2block::L2Block, migration::allow_path_not_found};

mod path {
    use super::*;

    pub const PATH: RefPath = RefPath::assert_from(b"/evm/world_state/blocks");

    pub const CURRENT_NUMBER: RefPath =
        RefPath::assert_from(b"/evm/world_state/blocks/current/number");
    pub const CURRENT_HASH: RefPath =
        RefPath::assert_from(b"/evm/world_state/blocks/current/hash");

    pub const INDEXES: RefPath = RefPath::assert_from(b"/evm/world_state/indexes/blocks");

    /// Path to the block in the storage. The path to the block is
    /// indexed by its hash.
    pub fn path(hash: H256) -> anyhow::Result<OwnedPath> {
        let hash = hex::encode(hash);
        let raw_hash_path: Vec<u8> = format!("/{}", &hash).into();
        let hash_path = OwnedPath::try_from(raw_hash_path)?;
        Ok(concat(&PATH, &hash_path)?)
    }
}

fn store_current_number(host: &mut impl Runtime, number: U256) -> anyhow::Result<()> {
    Ok(write_u256_le(host, &path::CURRENT_NUMBER, number)?)
}

fn store_current_hash(host: &mut impl Runtime, hash: H256) -> anyhow::Result<()> {
    write_h256_be(host, &path::CURRENT_HASH, hash)
}

fn store_block(
    host: &mut impl Runtime,
    block: &L2Block,
    index_block: bool,
) -> anyhow::Result<()> {
    if index_block {
        // Index the block, /evm/world_state/indexes/blocks/<n> points to
        // the block hash.
        let index = IndexableStorage::new(&path::INDEXES)?;
        index.push_value(host, block.hash().as_bytes())?;
    }
    let path = path::path(block.hash())?;
    let bytes = block.to_bytes();
    Ok(host.store_write_all(&path, &bytes)?)
}

fn store_current_index_or_not(
    host: &mut impl Runtime,
    block: &L2Block,
    index_block: bool,
) -> anyhow::Result<()> {
    store_current_number(host, block.number())?;
    store_current_hash(host, block.hash())?;
    store_block(host, block, index_block)?;
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

pub fn store_current(host: &mut impl Runtime, block: &L2Block) -> anyhow::Result<()> {
    store_current_index_or_not(host, block, true)
}

pub fn restore_current(host: &mut impl Runtime, block: &L2Block) -> anyhow::Result<()> {
    store_current_index_or_not(host, block, false)
}

pub fn read_current_number(host: &impl Runtime) -> anyhow::Result<U256> {
    Ok(read_u256_le(host, &path::CURRENT_NUMBER)?)
}

pub fn read_current_hash(host: &impl Runtime) -> anyhow::Result<H256> {
    read_h256_be(host, &path::CURRENT_HASH)
}

pub fn read_current(
    host: &mut impl Runtime,
    chain_family: &ChainFamily,
) -> anyhow::Result<L2Block> {
    let hash = read_current_hash(host)?;
    let block_path = path::path(hash)?;
    let bytes = &host.store_read_all(&block_path)?;
    let block_from_bytes = L2Block::try_from_bytes(chain_family, bytes)?;
    Ok(block_from_bytes)
}

pub fn garbage_collect_blocks(
    host: &mut impl Runtime,
    chain_family: &ChainFamily,
) -> anyhow::Result<()> {
    log!(host, Debug, "Garbage collecting blocks.");
    if let Ok(block) = read_current(host, chain_family) {
        // The kernel needs the current block to process the next one. Therefore
        // we garbage collect everything but the current block.
        host.store_delete(&path::PATH)?;
        restore_current(host, &block)?;
        // Clean all transactions, they are unused by the kernel.
        allow_path_not_found(host.store_delete(&EVM_TRANSACTIONS_OBJECTS))?;
        allow_path_not_found(host.store_delete(&EVM_TRANSACTIONS_RECEIPTS))?;
    }
    Ok(())
}

#[cfg(test)]
pub mod internal_for_tests {
    use super::*;

    pub fn init_blocks_index() -> anyhow::Result<IndexableStorage> {
        Ok(IndexableStorage::new(&path::INDEXES)?)
    }

    pub fn store_current_number(
        host: &mut impl Runtime,
        number: U256,
    ) -> anyhow::Result<()> {
        super::store_current_number(host, number)
    }
}
