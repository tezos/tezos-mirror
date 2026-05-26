# Changelog

## Unreleased

### EVM Runtime

### Michelson Runtime

- Aliases originated as part of a native atomic calls are now surfaced via
  dedicated receipts. (!21904)
- MIR: harden the Michelson runtime — internal aborts now surface as
  structured errors. (!21867, !21868)
- The kernel's `tezosx_michelson_entrypoints` entrypoint now also
  returns the enshrined contract's synthetic views — currently the
  gateway's `staticcall_evm` view, declared as `view "staticcall_evm"
  (pair string bytes) bytes` at the script level (the `option bytes`
  described in !21875 is the caller-side type pushed by the MIR
  `VIEW` opcode, which wraps the body's return in `option`; the
  declaration itself returns `bytes`). The RLP result shape changes
  from `[entries]` to `[entries, views]`, where `views` is a list of
  `[name, parameter_type, return_type]` triples. The EVM node
  consumes this to embed `view` declarations in the synthesized stub
  `/script`, so wallets, indexers, and block explorers discover
  synthetic views through standard Tezos contract introspection.
  (!21936)
- Big map ids freshly allocated when a contract is originated via
  `CREATE_CONTRACT` are now assigned in source order, matching the
  convention used for externally originated contracts: among the big
  maps that receive a fresh id during the dump, the leftmost in the
  storage AST gets the lowest, the next the next, and so on. (Big
  maps that keep an existing storage id — e.g. a parent's big map
  threaded into the child's storage — are unaffected.) Indexers and
  tooling that map big map ids to positions in the storage type can
  rely on this for both origination paths. (!21953)

### Native Atomic Composability

- Cross-runtime calls now enforce a maximum chain depth
  (`X-Tezos-CRAC-Depth` ≤ 128): a self-recursive Michelson↔EVM gateway
  cycle that previously wedged sequencer block production now fails
  cleanly at the operation level (catchable revert) once the cap is
  reached, leaving the block intact. (!21972)
- Error that can be user triggered now maps now doesn't block abort. (!21918)
- EVM pre-execution validation failures (revm `validate()` errors such as a
  forwarded gas limit below the intrinsic cost) on the cross-runtime call
  and static-call path now surface as a catchable `400` instead of a
  block-aborting `500`. (!21974)
- Fix temporary big-maps leaking into durable storage on the inbound
  cross-runtime call path. The per-operation temporary big-map id was
  seeded to `0` (a non-negative, i.e. non-temporary, id) and never
  cleared, so transient big-maps allocated by a Michelson callee were
  written to — and persisted in — the durable `/big_map/<id>`
  namespace, colliding with real durable big-maps. The id is now seeded
  to `-1` (temporary) and the temporaries are cleared after the call,
  matching the native batch path. (!21991)
- Same-runtime gateway round-trips no longer launder the caller's
  identity: when the gateway's source and target runtimes are the
  same and the source is a native account, the gateway now delivers
  the real native `msg.sender` (EVM) / `SENDER` (Michelson) to the
  callee instead of re-deriving an alias from the caller's address. (!21963)

### Storage versions

- `chain_id`, `evm_version`, `maximum_gas_per_transaction`,
  `sequencer_governance`, `sequencer_pool_address` subtree move from `/evm/` to `/evm/world_state/`;
  the block-number index moves from `/evm/world_state/indexes/blocks/` to
  `/evm/world_state/blocks/indexes/` (storage version 58). (!21911)

- Move `/evm/world_state/eth_accounts/` to
  `/evm/eth_accounts/` and `/evm/world_state/eth_codes/` to
  `/evm/eth_accounts/eth_codes/`. Add `/evm/eth_accounts` as the fourth
  SafeStorage safe root (storage version 59). (!21933)

- Move ephemeral simulation and trace IPC out of `/evm/` and
  `/tez/world_state/` into `/base/`: `/evm/simulation_result` to
  `/base/evm_simulation_result`, `/evm/simulation_http_traces` to
  `/base/simulation_http_traces`, `/evm/trace/` to
  `/base/trace/`, `/evm/world_state/__http_trace/traces/` to
  `/base/__http_trace/traces/`, and `/tez/world_state/simulation_result` to
  `/base/tez_simulation_result`. No data migration runs — these paths are
  populated only inside ephemeral `simulate_and_read` calls. A dedicated
  `SafeStorage::promote_http_trace()` mirrors the existing `promote_trace()`
  so per-tx HTTP traces land at the new `/base/__http_trace/traces/`
  location after their replay (storage version 60). (!21957)

### Internals

- MIR: harden the Michelson runtime — internal aborts now surface as
  structured errors. (!21869, !21870, !21871, !21872, !21976)

## Version 0.5 (3038e37e7cfca6f6930dce8375fc97d3917e0518)

### EVM Runtime

- The EVM runtime's cross-runtime HTTP server now dispatches on the
  request method: `POST` keeps the existing state-mutating entry;
  `GET` is routed to a read-only entry that rejects `X-Tezos-Amount != 0`,
  suppresses the sender balance credit and the `CracReceived` log emission.
  REVM enforces the standard `STATICCALL` contract end-to-end:
  any state-mutating opcode (`SSTORE`, `LOG*`, `CREATE*`, `SELFDESTRUCT`,
  value-bearing `CALL`) halts the call with `StateChangeDuringStaticCall`,
  surfaced as a catchable `400`. (!21849)

### Michelson Runtime

- Storage burn now fires on Michelson manager operations: transfer-
  to-contract pays a variable burn proportional to
  `paid_storage_size_diff`, origination pays the variable burn plus
  a fixed slot burn of `origination_size × cost_per_byte`, and the
  first credit to an unallocated implicit account also pays the
  slot burn and flips `allocated_destination_contract` to `true` on
  the receipt. When the source cannot cover a burn, the operation
  is marked `Backtracked` with a new
  `ApplyOperationError::CannotPayStorageFee` error trace,
  `SafeStorage` rolls back the batch, and `Applied` internals are
  cascade-demoted so the manager-operation tree stays L1-coherent. (!21840)
- The MIR `VIEW` opcode can now reach a synthetic `staticcall_evm`
  view on the TezosXGateway, letting any Michelson code — including
  view bodies, where `TRANSFER_TOKENS` is forbidden — read EVM state
  synchronously. The view's typed shape is
  `pair string bytes : option bytes` (`(destination, calldata)`
  argument, EVM response body wrapped in `Some` on success). (!21875)

### Native Atomic Composability

- Reject `STATICCALL` on the runtime gateway precompile's
  state-mutating selectors. (!21905)

### Storage versions

- `/evm/michelson_runtime/sunrise_level` and
  `/evm/michelson_runtime/target_sunrise_level` move under
  `/tez/world_state/michelson_runtime/` (storage version 57). (!21851)

### Internals

- Generalize the captured transaction source (E_0 / `tx.origin`) on
  the EVM journal from a bare `Address` to a typed `OriginalSource`
  carrying the originating `RuntimeId`, the source's native address
  string (lowercase hex for Ethereum, b58check PKH for Tezos). (!21899)
- MIR: harden the Michelson runtime — internal aborts now surface as
  structured errors. (!21866)

## Version 0.4 (7e5806548ca8b86849300c461205742c24e7988a)

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
- Deposits targeting a Tezos implicit account now emit a Michelson `deposit`
  event of type `pair (nat %inbox_level) (nat %inbox_msg_id)` on the operation
  receipt. This gives indexers the `(level, index)` coordinates of the
  originating shared-inbox message. (!21877)
- Deposits targeting a Tezos implicit account now emit balance updates.
  (!21877)
- Storage burn now fires on Michelson manager operations: transfer-
  to-contract pays a variable burn proportional to
  `paid_storage_size_diff`, origination pays the variable burn plus
  a fixed slot burn of `origination_size × cost_per_byte`, and the
  first credit to an unallocated implicit account also pays the
  slot burn and flips `allocated_destination_contract` to `true` on
  the receipt. When the source cannot cover a burn, the operation
  is marked `Backtracked` with a new
  `ApplyOperationError::CannotPayStorageFee` error trace,
  `SafeStorage` rolls back the batch, and `Applied` internals are
  cascade-demoted so the manager-operation tree stays L1-coherent. (!21840)

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
- Account for gas during Micheline encode and decode in the Tezos X
  runtime: `Micheline::encode`, `decode_raw`, `decode_packed`, and
  `decode_raw_prefix` now charge `cost_ENCODING_MICHELINE` /
  `cost_DECODING_MICHELINE_bytes` (mirroring L1) and propagate
  `OutOfGas` through the application error chain. Wired against the
  real operation gas at transfer, origination, `mir_ctx` (big_map,
  view lookup, hash), view handler, and MIR interpreter PACK / UNPACK
  / VIEW sites; kernel-synthesised paths (CRAC receipts, canonical
  Unit body) use `Gas::default()` as an OOG safety cap. (L2-437,
  !21889)

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
