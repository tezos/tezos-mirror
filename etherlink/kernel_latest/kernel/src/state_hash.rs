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
//! evm_state_hash          = keccak256(h_keyspace(/evm/eth_accounts) || blueprint_hash)
//! tez_accounts_state_hash = keccak256(h_keyspace(/tez/tez_accounts) || blueprint_hash)
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

use sha3::{Digest, Keccak256};
use tezos_ethereum::transaction::TransactionHash;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_keyspace::KeySpace;
use tezos_tezlink::block::AppliedOperation;

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

/// Keccak over an accounts subtree hash and the blueprint hash.
fn runtime_state_hash(accounts: &[u8], blueprint_hash: &[u8; 32]) -> Vec<u8> {
    let mut hasher = Keccak256::new();
    hasher.update(accounts);
    hasher.update(blueprint_hash);
    hasher.finalize().to_vec()
}

/// Compute `keccak256(h_keyspace(/evm/eth_accounts) || blueprint_hash)` and
/// return it as a byte vector suitable for `EthBlock::state_root`.
pub fn evm_state_hash(
    eth_accounts: &impl KeySpace,
    blueprint_hash: &[u8; 32],
) -> Vec<u8> {
    runtime_state_hash(&eth_accounts.hash(), blueprint_hash)
}

/// Computes the [`TezBlock::state_root`] of a block, always 32 bytes long.
///
/// The value is `keccak256(tez_accounts.hash() || blueprint_hash)`.
/// `tez_accounts` is the keyspace rooted at `/tez/tez_accounts`, and
/// `blueprint_hash` is the result of [`blueprint_hash()`] for the same block.
///
/// [`TezBlock::state_root`]: tezos_tezlink::block::TezBlock::state_root
pub fn tez_accounts_state_hash(
    tez_accounts: &impl KeySpace,
    blueprint_hash: &[u8; 32],
) -> Vec<u8> {
    runtime_state_hash(&tez_accounts.hash(), blueprint_hash)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::{MockRuntimeKeyspaces, RuntimeKeyspaces};
    use tezos_evm_runtime::safe_storage::SafeStorage;
    use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_keyspace::Key;

    /// Durable root of the EVM accounts keyspace, spelled out and not derived
    /// from the keyspace name. The tests below compare the keyspace hash with
    /// the host hash of this exact path. With one builder for both sides, a
    /// change to the builder moves both sides, and no test fails.
    const EVM_ACCOUNTS_PATH: RefPath = RefPath::assert_from(b"/evm/eth_accounts");
    /// Durable root of the Tez accounts keyspace, spelled out for the same
    /// reason as [`EVM_ACCOUNTS_PATH`].
    const TEZ_ACCOUNTS_PATH: RefPath = RefPath::assert_from(b"/tez/tez_accounts");

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
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).unwrap();

        // Two empty roots hash the same, so each root gets its own value.
        rk.host_mut()
            .store_write_all(&EVM_ACCOUNTS_PATH, b"evm")
            .unwrap();
        rk.host_mut()
            .store_write_all(&TEZ_ACCOUNTS_PATH, b"tez")
            .unwrap();

        let bh = blueprint_hash(&valid, &delayed, &michelson, ts);
        let evm = evm_state_hash(rk.eth_accounts(), &bh);
        let tez = tez_accounts_state_hash(rk.tez_accounts(), &bh);

        assert_ne!(evm, tez);
    }

    /// Makes sure that the EVM keyspace hash equals the raw host hash of
    /// `/evm/eth_accounts`. A difference changes the `state_root` of every
    /// block.
    #[test]
    fn evm_hash_matches_the_durable_subtree_hash() {
        let (valid, delayed, michelson, ts) = fixture_inputs();
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).unwrap();
        rk.host_mut()
            .store_write_all(&EVM_ACCOUNTS_PATH, b"evm")
            .unwrap();

        let bh = blueprint_hash(&valid, &delayed, &michelson, ts);
        let through_keyspace = evm_state_hash(rk.eth_accounts(), &bh);
        let raw = rk.host_mut().store_get_hash(&EVM_ACCOUNTS_PATH).unwrap();
        let through_host = runtime_state_hash(&raw, &bh);

        assert_eq!(through_keyspace, through_host);
    }

    /// Makes sure that the Tez keyspace hash equals the raw host hash in the
    /// `/tmp` copy (the temporary copy that a `SafeStorage` writes to).
    ///
    /// Both sides hash the `/tmp` copy, with the big-map writes that go
    /// through the raw host. After `promote`, the live root keeps that hash.
    /// The block computes `state_root` before `promote`, so a difference on
    /// either side changes the `state_root`.
    #[test]
    fn tez_hash_matches_the_durable_root_hash_in_the_tmp_copy() {
        let (valid, delayed, michelson, ts) = fixture_inputs();
        let bh = blueprint_hash(&valid, &delayed, &michelson, ts);
        let mut host = MockKernelHost::default();
        // `start` copies the root, so it has to exist.
        host.store_write_all(&TEZ_ACCOUNTS_PATH, b"tez").unwrap();
        let mut safe = SafeStorage {
            host: &mut host,
            world_states: vec![OwnedPath::from(TEZ_ACCOUNTS_PATH)],
        };
        safe.start().unwrap();

        let through_tmp_copy = {
            let mut rk = RuntimeKeyspaces::init(&mut safe).unwrap();
            rk.tez_accounts_mut()
                .set(&Key::from_static(b"/contracts/index/kt1"), b"contract")
                .unwrap();
            let big_map = RefPath::assert_from(b"/tez/tez_accounts/big_map/0/key");
            rk.host_mut().store_write_all(&big_map, b"value").unwrap();

            let through_keyspace = tez_accounts_state_hash(rk.tez_accounts(), &bh);
            let raw = rk.host_mut().store_get_hash(&TEZ_ACCOUNTS_PATH).unwrap();
            assert_eq!(through_keyspace, runtime_state_hash(&raw, &bh));
            through_keyspace
        };

        safe.promote().unwrap();
        let live = host.store_get_hash(&TEZ_ACCOUNTS_PATH).unwrap();
        assert_eq!(through_tmp_copy, runtime_state_hash(&live, &bh));
    }
}
