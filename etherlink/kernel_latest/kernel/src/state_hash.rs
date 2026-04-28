// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! TezosX block state-hash computation.
//!
//! Each runtime's block `state_root` is derived from its own accounts
//! subtree hash combined with a shared `blueprint_hash` that commits to
//! every blueprint input: EVM txs, delayed txs, Michelson ops, and
//! timestamp.
//!
//! ```text
//! michelson_ops_commitment = keccak256(concat(op.hash for op in tezos_ops))
//! blueprint_hash = keccak256(
//!     u32_le(valid_tx_hashes.len)  || concat(valid_tx_hashes)
//!  || u32_le(delayed_tx_hashes.len) || concat(delayed_tx_hashes)
//!  || michelson_ops_commitment
//!  || timestamp_le_bytes
//! )
//! evm_state_hash          = keccak256(h(/evm/world_state/eth_accounts) || blueprint_hash)
//! tez_accounts_state_hash = keccak256(h(/tez/tez_accounts)             || blueprint_hash)
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
//! distinct blueprints at the same level: any divergence in EVM txs,
//! delayed txs, Michelson ops, or timestamp flips at least one term.
//! This makes the formula storage-layout-agnostic: migration phases
//! that move accounts subtrees do not affect the invariant.
//!
//! Both the blueprint and instant-confirmation paths populate
//! `valid_txs`, `delayed_txs`, `cumulative_tezos_operation_receipts`,
//! and `timestamp` identically in `BlockInProgress` (deterministic
//! execution loop appends in the same order), so both paths yield the
//! same `state_root` for the same semantic block.
//!
//! The two per-runtime helpers share the same `blueprint_hash` argument
//! so that, at a given level, both runtimes' state hashes diverge
//! together.

use revm_etherlink::storage::world_state_handler::EVM_ACCOUNTS_PATH;
use sha3::{Digest, Keccak256};
use tezos_ethereum::transaction::TransactionHash;
use tezos_smart_rollup_core::STORE_HASH_SIZE;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::Path;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_tezlink::block::AppliedOperation;

use crate::chains::TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH;

/// Sentinel returned by `safe_store_get_hash` when the input path is absent.
const EMPTY_STORE_HASH: [u8; STORE_HASH_SIZE] = [b'0'; STORE_HASH_SIZE];

fn safe_store_get_hash(host: &mut impl StorageV1, path: &impl Path) -> Vec<u8> {
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

/// Shared per-level commitment to every blueprint input. The same value
/// feeds both `evm_state_hash` and `tez_accounts_state_hash` so that two
/// blueprints at the same level produce distinct `state_root` values on
/// every runtime.
pub fn blueprint_hash(
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

fn runtime_state_hash<Host: StorageV1>(
    host: &mut Host,
    accounts_path: &impl Path,
    blueprint_hash: &[u8; 32],
) -> Vec<u8> {
    let accounts = safe_store_get_hash(host, accounts_path);
    let mut hasher = Keccak256::new();
    hasher.update(&accounts);
    hasher.update(blueprint_hash);
    hasher.finalize().to_vec()
}

/// Compute `keccak256(h(/evm/world_state/eth_accounts) || blueprint_hash)`
/// and return it as a byte vector suitable for `EthBlock::state_root`.
pub fn evm_state_hash<Host: StorageV1>(
    host: &mut Host,
    blueprint_hash: &[u8; 32],
) -> Vec<u8> {
    runtime_state_hash(host, &EVM_ACCOUNTS_PATH, blueprint_hash)
}

/// Compute `keccak256(h(/tez/tez_accounts) || blueprint_hash)` and return
/// it as a byte vector suitable for the Michelson block `state_root`.
pub fn tez_accounts_state_hash<Host: StorageV1>(
    host: &mut Host,
    blueprint_hash: &[u8; 32],
) -> Vec<u8> {
    runtime_state_hash(
        host,
        &TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
        blueprint_hash,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_smart_rollup_mock::MockHost;

    fn fixture_inputs() -> (
        [TransactionHash; 2],
        [TransactionHash; 1],
        [u8; 32],
        Timestamp,
    ) {
        let valid = [[7u8; 32], [8u8; 32]];
        let delayed = [[9u8; 32]];
        let michelson = [42u8; 32];
        let ts = Timestamp::from(1_700_000_000i64);
        (valid, delayed, michelson, ts)
    }

    /// Two blueprints at the same level with divergent inputs must
    /// produce distinct `blueprint_hash` values. Spot-check that every
    /// factor (valid_txs, delayed_txs, michelson ops, timestamp) flips
    /// the result.
    #[test]
    fn blueprint_hash_is_sensitive_to_every_input() {
        let (valid, delayed, michelson, ts) = fixture_inputs();
        let base = blueprint_hash(&valid, &delayed, &michelson, ts);

        // Flip valid_txs.
        let valid2 = [[7u8; 32], [9u8; 32]];
        assert_ne!(base, blueprint_hash(&valid2, &delayed, &michelson, ts));

        // Flip delayed_txs.
        let delayed2 = [[10u8; 32]];
        assert_ne!(base, blueprint_hash(&valid, &delayed2, &michelson, ts));

        // Flip michelson commitment.
        let michelson2 = [43u8; 32];
        assert_ne!(base, blueprint_hash(&valid, &delayed, &michelson2, ts));

        // Flip timestamp.
        let ts2 = Timestamp::from(ts.i64() + 1);
        assert_ne!(base, blueprint_hash(&valid, &delayed, &michelson, ts2));
    }

    /// The length prefix on each tx list disambiguates the boundary
    /// between `valid_txs` and `delayed_txs`. Without it, moving a hash
    /// from one list to the other would collide.
    #[test]
    fn blueprint_hash_tx_list_boundary_is_disambiguated() {
        let tx: TransactionHash = [42u8; 32];
        let michelson = [0u8; 32];
        let ts = Timestamp::from(0i64);

        let a = blueprint_hash(&[tx], &[], &michelson, ts);
        let b = blueprint_hash(&[], &[tx], &michelson, ts);
        assert_ne!(a, b);
    }

    /// The two per-runtime helpers must yield different hashes for the
    /// same `blueprint_hash` when their input subtrees differ.
    #[test]
    fn evm_and_tez_accounts_state_hashes_are_independent() {
        let (valid, delayed, michelson, ts) = fixture_inputs();
        let mut host = MockHost::default();

        // Write something distinct under each accounts path so the
        // subtree hashes differ. An empty subtree maps to the same
        // sentinel for both and would trivially match.
        host.store_write_all(&EVM_ACCOUNTS_PATH, b"evm").unwrap();
        host.store_write_all(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH, b"tez")
            .unwrap();

        let bh = blueprint_hash(&valid, &delayed, &michelson, ts);
        let evm = evm_state_hash(&mut host, &bh);
        let tez = tez_accounts_state_hash(&mut host, &bh);

        assert_ne!(evm, tez);
    }
}
