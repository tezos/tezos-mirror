// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! TezosX block state-hash computation.
//!
//! Each runtime's block `state_root` is derived from its own accounts
//! subtree hash combined with a shared `blueprint_hash` that commits to
//! every blueprint input, EVM txs, delayed txs, Michelson ops, and
//! timestamp:
//!
//! ```text
//! michelson_ops_commitment = keccak256(concat(op.hash for op in tezos_ops))
//! blueprint_hash = keccak256(
//!     u32_le(valid_tx_hashes.len)  || concat(valid_tx_hashes)
//!  || u32_le(delayed_tx_hashes.len) || concat(delayed_tx_hashes)
//!  || michelson_ops_commitment
//!  || timestamp_le_bytes
//! )
//! tzx_state_hash = keccak256(h(/evm/world_state/eth_accounts) || blueprint_hash)
//! ```
//!
//! The `u32`-length prefixes on each tx list disambiguate the boundary
//! between `valid_txs` and `delayed_txs`, both of which are sequences of
//! fixed-width 32-byte hashes. Without the prefixes, a blueprint with
//! `valid_txs = [t], delayed_txs = []` would collide with
//! `valid_txs = [], delayed_txs = [t]` even though the two are
//! semantically distinct (different inclusion/fee rules).
//!
//! The `blueprint_hash` factor alone guarantees uniqueness across
//! distinct blueprints at the same level, any divergence in EVM txs,
//! delayed txs, Michelson ops, or timestamp flips at least one term.
//! This makes the formula storage-layout-agnostic: migration phases
//! that move accounts subtrees do not affect the invariant.
//!
//! Both paths populate `valid_txs`, `delayed_txs`,
//! `cumulative_tezos_operation_receipts`, and `timestamp` identically
//! in `BlockInProgress` (deterministic execution loop appends in the
//! same order), so both paths yield the same `state_root` for the same
//! semantic block.

use revm_etherlink::storage::world_state_handler::EVM_ACCOUNTS_PATH;
use sha3::{Digest, Keccak256};
use tezos_ethereum::transaction::TransactionHash;
use tezos_smart_rollup_core::STORE_HASH_SIZE;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_tezlink::block::AppliedOperation;

/// Sentinel returned by `safe_store_get_hash` when the input path is absent.
const EMPTY_STORE_HASH: [u8; STORE_HASH_SIZE] = [b'0'; STORE_HASH_SIZE];

fn safe_store_get_hash(host: &mut impl StorageV1, path: &RefPath) -> Vec<u8> {
    match host.store_get_hash(path) {
        Ok(hash) => hash.into(),
        Err(_) => EMPTY_STORE_HASH.to_vec(),
    }
}

/// Keccak256 over Michelson op hashes in execution order.
pub fn michelson_ops_commitment(ops: &[AppliedOperation]) -> [u8; 32] {
    let mut hasher = Keccak256::new();
    for op in ops {
        hasher.update(op.hash.as_ref());
    }
    hasher.finalize().into()
}

fn blueprint_hash(
    valid_txs: &[TransactionHash],
    delayed_txs: &[TransactionHash],
    michelson_commitment: &[u8; 32],
    timestamp: Timestamp,
) -> [u8; 32] {
    let mut hasher = Keccak256::new();
    hasher.update((valid_txs.len() as u32).to_le_bytes());
    for h in valid_txs {
        hasher.update(h);
    }
    hasher.update((delayed_txs.len() as u32).to_le_bytes());
    for h in delayed_txs {
        hasher.update(h);
    }
    hasher.update(michelson_commitment);
    hasher.update(timestamp.i64().to_le_bytes());
    hasher.finalize().into()
}

/// Compute `keccak256(h(/evm/world_state/eth_accounts) || blueprint_hash)` and return
/// it as a byte vector suitable for `EthBlock::state_root`.
pub fn tzx_state_hash<Host: StorageV1>(
    host: &mut Host,
    valid_txs: &[TransactionHash],
    delayed_txs: &[TransactionHash],
    michelson_commitment: &[u8; 32],
    timestamp: Timestamp,
) -> Vec<u8> {
    let world = safe_store_get_hash(host, &EVM_ACCOUNTS_PATH);
    let bh = blueprint_hash(valid_txs, delayed_txs, michelson_commitment, timestamp);
    let mut hasher = Keccak256::new();
    hasher.update(&world);
    hasher.update(bh);
    hasher.finalize().to_vec()
}
