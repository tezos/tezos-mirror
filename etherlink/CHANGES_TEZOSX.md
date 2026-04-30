# Changelog

## Unreleased

### Native atomic composability

### Internals

## Version 0.2 (017753c894e5bdaae7838c9501814c1ccc7290d6)

### Native atomic composability

- Expose Michelson views to EVM contracts through the cross-runtime
  HTTP protocol. The Michelson runtime's HTTP server now dispatches
  on the request method: `POST` keeps the existing entrypoint-call
  path, `GET` executes a named view on an originated contract and
  surfaces its Micheline-encoded result through the same
  `%collect_result` frame slot the entrypoint path uses. The EVM
  gateway precompile gains a typed
  `callMichelsonView(string,string,bytes) returns (bytes)` entry and
  the generic `call(url, headers, body, method)` entry treats
  `method=GET` as a read-only call. Both paths are
  STATICCALL-compatible: no log emission, no value transfer, no
  alias-cache write — the idiomatic
  `staticcall(GATEWAY, abi.encodeCall(callMichelsonView, …))` Solidity
  pattern works. (!21715)
- Reject DELEGATECALL and CALLCODE on every entry of the runtime
  gateway precompile (`transfer`, `callMichelson`, `callMichelsonView`,
  `call`). A delegated frame would silently rewire the sender used for
  alias resolution and inherit unrelated `msg.value`; there is no
  legitimate reason to reach the gateway through those opcodes.
  (!21715)
- Charge gas for `%collect_result` (!21710)
- Forbid tez transfers to `%collect_result` (!21738)
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

### EVM Runtime
- Refund unused gas when a precompile reverts, previously could charge
  the full gas limit. (!21724)

### Internals

- Sort entrypoints by name before encoding the
  `tezosx_michelson_entrypoints` kernel response, so the
  `/entrypoints` RPC output is deterministic across runs and across
  native vs WASM kernel executions (`HashMap` iteration order depends
  on a per-process random seed). (!21715)

## Version 0.1 (162a573)

This is the first consolidated release of Tezos X. Tezos X extends Etherlink
with the Michelson runtime, enabling Tezos native applications to be deployed
alongside the preexisting Etherlink ecosystem.

Once the Michelson runtime is enabled, the kernel accepts to process Tezos
operations sequenced in incoming blueprints, and produces Tezos blocks. It also
exposes two gateways enabling native atomic composition between the two
runtimes.

Its storage version is 54.

### Native atomic composability

- Add `%collect_result` entrypoint to the Michelson gateway: lets
  adapters deposit a bytes payload surfaced as the synchronous return
  value of the EVM call, with gas charged at deposit time. (!21658,
  !21659)

### Internal

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
