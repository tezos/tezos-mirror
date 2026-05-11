# Changelog

## Unreleased

### EVM Runtime

### Michelson Runtime

### Native atomic composability

### Internals

- Make `Micheline::encode` and `encode_for_pack` return
  `Result<Vec<u8>, BinError>` instead of panicking on zarith encoding
  failures, removing a class of unrecoverable kernel panics from the
  MIR Michelson encoder. (!21474)
- Propagate encode errors through typed variants per call site
  (`OriginationError::MichelineSerializationError`,
  `TransferError::MichelineSerializationError`,
  `ApplyOperationError::EmitMichelineSerializationError`,
  `LazyStorageError::BinWriteError`, and `TezosXRuntimeError::Custom`
  for the CRAC-receipt paths) so the originating operation surfaces a
  typed failure rather than aborting on panic. (!21474)
- Internal-operation encode failures still collapse onto their parent
  transfer's error path; per-op `ContentResult::Failed` routing
  matching L1's `apply.ml::apply_internal_operation_contents` is
  tracked as a follow-up. (!21474)
- Surface real failures from `lookup_view_storage_balance` (host I/O,
  decoded-code corruption, balance overflow) via the typed
  `LookupViewError` enum, instead of silently masquerading as "view
  not found". (!21474)


## Version 0.3 (d2a6743ebef523c88c986c21311307a4251e67e4)

### Native atomic composability

- Stop persisting a `U256::MAX` balance for the internal
  `TEZOSX_CALLER_ADDRESS` (`0x7e205800…01`) used by `generate_alias`.
  Earlier kernels wrote that balance to durable storage as a "safety"
  buffer, but the surrounding `run_transaction` is `CrossRuntime` so its
  EVM journal never commits — only the manual storage write persisted,
  leaking a visible huge balance on Blockscout. The funding has been
  removed (`gas_price = 0` and `value = 0` in the internal call mean no
  pre-flight balance is required), and storage version 55 cleans up the
  residue on TezosX networks. (L2-1296)
- Fix EVM logs from cross-VM `%call_evm` calls being dropped from the
  synthetic CRAC transaction receipt. Previously
  `commit_evm_journal_from_external` ran before
  `extract_cross_runtime_effects`; the former calls
  `JournalInner::finalize` which clears `inner.logs`, leaving the receipt
  builder with an empty buffer. Only the `CracIdEvent` (constructed by
  the receipt builder itself) survived; both the precompile's
  `CracReceived` log and any `LOG0..LOG4` from the inner EVM call were
  lost. The order is now reversed so the receipt builder reads
  `inner.logs` while revm's standard accumulation is still intact,
  restoring parity between the two ways into the EVM for indexers
  (subgraphs, on-chain analytics, ERC-1155 wallet trackers).
- Surface user Michelson `EMIT` events from re-entrant inner CRACs on
  the synthetic CRAC transaction receipt. Previously, events emitted by
  a Michelson contract reached through a nested cross-runtime call (EVM
  → Mich → EVM → Mich within one EVM transaction) were silently
  discarded, breaking parity for Michelson-side event indexers.
  (!21807)
- Preserve the CRAC-ID correlation event on the synthetic CRAC
  transaction receipt when the same EVM transaction also contains a
  failed CRAC whose Michelson contract emitted a user `EMIT`.
  Previously, the user EMIT could shadow the kernel's CRAC-ID event
  during receipt merging, leaving indexers without the correlation key
  for the whole transaction. (!21808)
- Fix the synthetic CRAC-ID event being absent from the synthetic
  Michelson manager-op when an EVM transaction performs more than
  one CRAC and the applied one is rolled back by an EVM revert,
  leaving only failed siblings on the receipt. Indexers correlating
  Michelson activity to the originating EVM transaction by CRAC-ID
  would otherwise lose entire transactions. (!21811)
- Preserve applied CRAC receipts on the synthetic Michelson manager-op
  when the enclosing EVM transaction reverts. Previously, an applied
  CRAC's entire body — top-level transfer plus internal operations —
  vanished from the receipt when a sibling CRAC failed and the EVM tx
  reverted, breaking parity with L1 manager-op semantics where
  applied internals reverted by a later failure appear as
  `backtracked` rather than disappear. Receipts pushed during a
  reverting frame are now drained, transformed in place to
  `BackTracked`, and migrated to a new backtracked list that — like
  the failed list — is not subject to further revert; their state
  effects still roll back via the snapshot mechanism. The merged
  top-level status reconciles to `failed` when at least one Failed
  sibling participated, `applied` when any currently-applied CRAC
  remains, and `backtracked` otherwise. (!21812)
- Fix the synthetic Michelson manager-op listing failed re-entrant
  inner CRACs before their outer parent's own transfer when an EVM
  transaction nests CRACs (EVM → Mich → EVM) and an intermediate EVM
  frame catches an inner CRAC failure. Indexers walking the receipt
  would otherwise see entries in a non-execution order and
  mis-attribute the call tree. (!21814)
- Fix the originating EOA being lost on every CRAC issued from a
  re-entrant EVM frame (Michelson `call_evm` back into the EVM
  runtime). Each fresh EVM execution started without remembering the
  outer transaction's `tx.origin`, causing the inner CRAC's source
  attribution to fall back to the alias of the Michelson contract
  that triggered the re-entry instead of the EOA. Indexers and any
  downstream code resolving per-CRAC source identity could no longer
  recover which EOA originated the transaction. (!21817)

### Michelson Runtime

- Raise the Michelson runtime `hard_gas_limit_per_operation` and
  `hard_gas_limit_per_block` from 1,040,000 to 3,000,000 gas units (i.e.
  3,000,000,000 milligas) to match the EVM 30M-gas per-transaction cap.
  These two parametric constants now diverge from the L1 mainnet defaults
  (which both stay at 1,040,000), so a Michelson operation accepted by
  Tezos X may exceed the gas budget the same operation would be allowed
  on L1. Without this change, an EVM transaction reaching the cross-
  runtime precompile with more than ~10.4M gas remaining would forward
  an oversized `X-Tezos-Gas-Limit` and be rejected by the Michelson
  runtime, surfacing as a misleading EVM out-of-gas. (!21791)

### Michelson Runtime

- Receipts of Michelson manager operations now surface non-zero
  `storage_size` and `paid_storage_size_diff`. After each successful
  operation the kernel bumps the contract's `paid_bytes` watermark
  to its `used_bytes`, and writes the absolute post-op `used_bytes`
  along with the newly-allocated delta into the receipt. Origination
  receipts surface the contract's full initial size in both fields,
  matching what L1 produces. Indexers and wallets reading these
  fields will observe the actual storage footprint instead of zeros.
  (!21798)

### Internals

- Unify the revert scope of infrastructure-class `TransferError`
  variants across the regular Tezos pipeline and the cross-runtime
  gateway HTTP path. Storage/encoding/host-I/O failures
  (`FailedToFetch*` / `FailedToUpdate*` /
  `FailedToApplyBalanceChanges` / `FailedToComputeBalanceUpdate` /
  `FailedToAllocateDestination`) now route to `CracError::BlockAbort`
  in `From<TransferError>`, so both paths discard the block instead of
  the regular pipeline silently downgrading to an op-level revert.
  Reject transfers to a never-originated KT1 with a typed user-level
  `TransferError::ContractDoesNotExist` before any state write, so
  `FailedToFetchContractCode` can be safely promoted to `BlockAbort`
  alongside the other infrastructure variants without giving any
  user a block-abort handle. (!21781)
- Consolidate all Tezos account state under a single new SafeStorage
  keyspace `/tez/tez_accounts/`: Michelson contracts and big_map move
  from `/tezlink/context/` and originated KT1s in TezosX mode also
  unify under `/tez/tez_accounts/contracts/` and
  `/tez/tez_accounts/big_map/`, while TezosX projected accounts,
  aliases and cross-runtime alias resolution move from
  `/evm/world_state/eth_accounts/tezos/` to `/tez/tez_accounts/tezosx/`.
  `/tezlink` is removed and standalone-Tezlink block storage is unified
  with TezosX-mode at `/tez/world_state/tez_blocks/`. The Michelson
  `TezBlock` gains a `state_root` field
  (`keccak256(h(/tez/tez_accounts) || blueprint_hash)`, RLP bumped to
  V2 with a 9-element list) so two blueprints at the same level with
  divergent Michelson ops produce distinct Michelson block hashes,
  upholding the ADR `adr_tzx_state_hash.md` invariant on both
  runtimes. (!21716)

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
- Tezos operations now trigger a reboot if they can exceed the per-reboot gas
  budget. (!21765)

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
