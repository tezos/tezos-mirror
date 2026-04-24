# Changelog

## Version 0.1 (162a573)

This is the first consolidated release of Tezos X. Tezos X extends Etherlink
with the Michelson runtime, enabling Tezos native applications to be deployed
alongside the preexisting Etherlink ecosystem.

Once the Michelson runtime is enabled, the kernel accepts to process Tezos
operations sequenced in incoming blueprints, and produces Tezos blocks. It also
exposes two gateways enabling native atomic composition between the two
runtimes.

Its storage version is 54.

### Internal

- Fix CRAC receipt merging when an EVM transaction interleaves failed and
  successful CRACs. Receipts pushed to the Michelson journal are now tagged
  with a monotonic sequence number shared across the pending and failed
  lists, and `drain_pending_crac_receipts` sorts the concatenated receipts
  by that sequence so internal operations are emitted in execution order
  rather than bucketed by outcome. The merged top-level `ContentResult`
  is reconciled against the internals: if at least one successful CRAC
  contributed to the merge it is forced to `Applied` (so that `Applied`
  internals never sit under a `Failed` parent, an L1-invalid combination);
  if all internals are `Failed`/`Skipped`/`Backtracked` the top-level is
  marked as `Failed`. (!21719)
- Add support for `DalAttestedSlots` internal inbox message introduced in
  protocol U. The kernel now processes DAL slot attestations directly from the
  Layer 1 protocol, and does not need to rely on external import signals.  Only
  DAL slots published by whitelisted public key hashes (representing batching
  operators) are accepted and processed by the kernel. The whitelist is stored
  in durable storage at `/evm/dal_publishers_whitelist` as an RLP-encoded list
  of binary public key hashes. An empty whitelist operates in strict mode: all
  DAL slots are rejected. (!20143)
- Add ability to disable legacy DAL slot import signals via the
  `disable_legacy_dal_signals` feature flag. When enabled, the kernel ignores
  external `DalSlotImportSignal` messages and relies exclusively on
  `DalAttestedSlots` internal messages for DAL data import. (!20143)
- Consolidate all feature flags under `/base/feature_flags/`. Moves the 5
  flags at `/evm/feature_flags/` and drops the dead `enable_revm`,
  `enable_fast_withdrawal` and `enable_fast_fa_withdrawal` flags from
  `/evm/world_state/feature_flags/`, none of them is read by
  kernel_latest (revm and fast-withdrawal code paths are now
  unconditional). Storage version bumped to V54. (!21668)
- Migrate governance, DAL, blueprints, delayed inbox, chain configurations and
  kernel events from `/evm/...` to `/base/...`. Storage version bumped to V53.
  (!21565)
- Migrate the sequencer key and sequencer upgrade paths to the world state. The
  sequencer key moves from `/evm/sequencer` to `/evm/world_state/sequencer` and
  the sequencer upgrade from `/evm/sequencer_upgrade` to
  `/evm/world_state/sequencer_upgrade`. Storage version bumped to V50. (!20813
  !21178)
- EVM block `state_root` now encodes a commitment to the blueprint that
  produced the block in addition to the EVM world state:
  `state_root = keccak256(h(/evm/world_state/eth_accounts) || blueprint_hash)`
  where `blueprint_hash = keccak256(valid_tx_hashes || delayed_tx_hashes
  || michelson_ops_commitment || timestamp_le_bytes)` and
  `michelson_ops_commitment` is the Keccak-256 of Michelson op hashes in
  execution order. Two distinct blueprints at the same level now yield
  distinct `state_root` values independently of the durable-storage
  layout, paving the way for the durable-storage reorganization to
  proceed without breaking the blueprint-uniqueness invariant. (!21676)
