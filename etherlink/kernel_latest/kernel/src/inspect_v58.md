Shell commands to verify the V58 migration
The general form is:


./octez-evm-node --data-dir ./observer-mainnet-v58 shell <subcommand> [--block latest] <PATH>

# 1. Verify Phase 6 — EVM config moved under world state

These keys should now exist under /evm/world_state/ (not /evm/):


./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /evm/world_state/chain_id
./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /evm/world_state/evm_version
./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /evm/world_state/sequencer_governance
./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /evm/world_state/sequencer_pool_address
./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /evm/world_state/maximum_gas_per_transaction
And the old paths should be gone:


./octez-evm-node --data-dir ./observer-mainnet-v58 shell ls /evm
# Should NOT list: chain_id, evm_version, sequencer_governance, etc. at this level
2. Verify block index reorg (indexes/blocks → blocks/indexes)

# New location — should list block number entries
./octez-evm-node --data-dir ./observer-mainnet-v58 shell ls /evm/world_state/blocks/indexes

# Old location — should NOT exist or be empty
./octez-evm-node --data-dir ./observer-mainnet-v58 shell ls /evm/world_state/indexes
3. Verify prior phases are still intact

# Phase 2/3: operational config under /base
./octez-evm-node --data-dir ./observer-mainnet-v58 shell ls /base
./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /base/kernel_version
./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /base/l1_level
./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /base/kernel_root_hash
./octez-evm-node --data-dir ./observer-mainnet-v58 shell ls /base/feature_flags

# World state is still intact
./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /evm/world_state/blocks/current/number
./octez-evm-node --data-dir ./observer-mainnet-v58 shell cat /evm/world_state/fees/minimum_base_fee_per_gas
4. Broad tree view to spot any leftovers

# Sanity check: no stale /evm root keys that should have moved
./octez-evm-node --data-dir ./observer-mainnet-v58 shell tree /evm --depth 2

# Full overview
./octez-evm-node --data-dir ./observer-mainnet-v58 shell tree / --depth 2
5. Compare pre/post migration (optional but useful)
Run the same commands against ./observer-mainnet (v57 kernel) and ./observer-mainnet-v58 (v58 kernel) side-by-side. The /evm top-level listing should shrink (those keys moved to /evm/world_state/), while /evm/world_state/blocks/indexes should appear.
