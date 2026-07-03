# Changelog

## Unreleased

### EVM Runtime

- A signed transaction's signature scalars `r`/`s` are now
  rejected unless canonically encoded. A list-shaped or leading-zero-padded
  scalar decodes to the same value but changes the raw bytes, so one signed
  transaction had several valid wire encodings, each with a distinct
  `tx_hash`, a transaction-hash malleability. There is no fund or consensus
  impact (both encodings share `(sender, nonce)` and execute at most once);
  the risk was to third parties tracking a transaction by its hash (wallets,
  indexers, bridges). (!22371)
- Cross-runtime calls: the EVM-side charge-back for consumed gas now rounds the
  milligas→EVM-gas conversion up instead of down, so the sub-22-milligas
  remainder of each hop is no longer left uncharged. (!22350)
- **Breaking change (requires a previewnet reset):** the address origin
  classification — alias payload included — now lives inside the
  account info record (`/info`, fourth RLP field); the sibling
  `/origin` path is gone. This removes the per-transaction and
  per-committed-contract `/origin` reads introduced with the default
  classification: the transfer hot path performs exactly the same host
  calls as before it, and `ensure_alias` / the origin translation
  resolve from a single account-record read (the translation now
  charges `ALIAS_LOOKUP_COST` only, the separate back-stop charge is
  gone). Legacy 3-field records decode as unclassified and converge as
  accounts are touched, but classifications recorded in the old
  `/origin` records are **not** migrated — networks holding such state
  must be reset. (!22126)
- Removed the legacy `transfer(string implicitAddress)` selector from
  the runtime gateway precompile. Use the generic
  `call(string,(string,string)[],bytes,uint8)` entrypoint instead: a
  POST to `http://tezos/<address>` with an empty body and the value
  attached performs the same transfer. A calldata carrying the removed
  selector now reverts with `invalid input encoding`. The AliasForwarder
  predeployed contract was updated accordingly and forwards through the
  generic entrypoint.
- The synthetic EVM transaction that mirrors a Michelson operation's
  inbound cross-runtime calls is now a neutral envelope: `from` and `to`
  are both the originator's alias and `value` is `0`. It no longer
  aggregates a single `value`/sender across crossings — a behaviour that
  let a leading read-only `staticcall_evm` pin the reported value to `0`
  and shadow a later value-bearing `%call_evm`, and that misattributed
  the summed value to the originator rather than the calling contracts
  that actually held it. The per-crossing sender, target, and amount are
  carried by the `CracReceived` logs instead. A Michelson operation that
  only ever reads EVM state (no mutating crossing) no longer produces a
  synthetic transaction at all. (!22097)
- The runtime gateway's typed `callMichelsonView` selector now charges
  the same per-word payload surcharge as the generic `call(GET)` path,
  on the inbound calldata, the outgoing view input body, and the
  returned response body. Previously it only charged the flat base cost
  plus the converted Michelson runtime gas, so a caller could use the
  typed selector instead of `call(GET)` to avoid the gateway-internal
  payload surcharge. The per-word gas-charging logic is now shared
  across every gateway entrypoint. (!22044)
- A cross-runtime call (CRAC) made inside a contract constructor is now
  rolled back when the constructor reverts. Previously the durable state
  it produced — for example an alias forwarder — survived the revert.
  (!22062)
- The first read-only gateway call (`callMichelsonView` or generic
  `call(..., GET)`) from an inbound cross-runtime frame now forwards the
  transitive originator as `X-Tezos-Source`, matching what the `ORIGIN`
  opcode reports, while `X-Tezos-Sender` keeps the immediate caller
  alias. Previously these selectors recaptured the source from the
  immediate sender, so a Michelson -> EVM frame forwarded the wrong
  identity to the read-only call. (!22033)
- Inbound Michelson-to-EVM calls (CRAC) now expose the live block
  observables to EVM bytecode instead of zero/placeholder values:
  `BASEFEE`, `GASLIMIT` (the block gas limit), `BLOBBASEFEE`,
  `PREVRANDAO`, and `GASPRICE`. The originating runtime's block
  environment is carried on the cross-runtime journal from its creation
  and inherited when servicing the call; `GASPRICE` is exposed through a
  custom opcode that returns the block basefee (Etherlink ignores
  priority fees, so every transaction's effective price is exactly the
  basefee). EVM-side fee accounting is unchanged: CRAC transactions
  still carry `gas_price = 0` for the caller debit and basefee burn, and
  the EIP-1559 basefee preflight is disabled for cross-runtime origins so
  the live basefee doesn't reject the `gas_price = 0` call. (!22022)
- XTZ-deposit feeder now refunds the prefund on EVM-internal revert
  (`Ok`-with-revert), not only on the `Err` arm — previously the deposit
  value could be stranded on `FEED_DEPOSIT_ADDR` while the receiver was
  uncredited. (!22017)
- The `CracSent` event now reports the same `targetAddress` regardless
  of the gateway surface used. The generic `call(POST)` entry previously
  emitted the full URL path including a trailing entrypoint
  (`<KT1>/<entrypoint>`), while the typed `callMichelson` entry emitted
  the bare contract address (`<KT1>`); both now report the contract
  address, so indexers correlating CRACs by `(targetRuntime,
  targetAddress)` no longer split one contract into two identities.
  (!22032)
- The `CracReceived` event now reports `sourceRuntime` as the native
  runtime of `sourceAddress` (carried in a new `X-Tezos-Source-Runtime`
  context header) instead of a hardcoded `"tezos"`. On a nested
  `EVM -> Michelson -> EVM` call the forwarded source is the transitive
  EVM origin, so `sourceRuntime` is now `"ethereum"` and the
  `(sourceRuntime, sourceAddress)` pair is self-consistent for event
  consumers (indexers, monitors, bridges). (!22067)
- The synthetic EVM receipt produced for a Michelson -> EVM
  cross-runtime call now reports the post-transaction block gas
  accumulator in `cumulativeGasUsed`, as required by EIP-658.
  Previously the receipt was stamped with the pre-transaction value
  (excluding the call's own `gasUsed`), so block-gas cross-checks and
  consecutive-receipt differencing reported wrong per-transaction gas
  for every cross-runtime-originated receipt. (!22049)

### Michelson Runtime

- **Bug fix:** Michelson contracts executed via the delayed inbox now observe
  the configured Michelson runtime chain id from `CHAIN_ID`, matching the
  sequenced and gateway ingress lanes. Previously the delayed path derived
  the chain id from the EVM chain id's low 4 bytes. (!22348)
- Block finalization now rejects, rather than saturating the receipt nonce
  into duplicates, if a block ever crosses the 65,535 internal-operation cap;
  the delayed-inbox and EVM-origin cross-runtime paths fold their
  internal-operation counts into the block's cumulative cap so it fires at
  apply time on every path, matching the native path. (!22343)
- Integer `MUL` is now priced on the product of operand sizes (`~ s1*s2`)
  instead of L1's FFT-shaped `a*log(a)`. The Michelson runtime multiplies with
  num-bigint, which has no FFT (schoolbook/Karatsuba/Toom-3), so the L1 formula
  under-charged large multiplications super-linearly; charged gas now
  upper-bounds the real multiply work at every operand size. (!22289)
- The binary Micheline decoder now rejects the internal-only primitive ids
  161-164 (`Transfer_tokens`, `Set_delegate`, `Create_contract`, `Emit`),
  matching L1, which assigns no binary tags to them. These ids are used
  solely to unparse `operation` values and never appear in valid on-chain
  data. (!22290)
- The typechecker now bounds a parsed type expression to 2001 nodes (L1's
  `michelson_maximum_type_size`) and rejects originated scripts larger than
  32 kB (L1's `max_operation_data_length`). Previously the Michelson runtime
  had neither limit, diverging from L1; a deeply nested `or` parameter type
  could make entrypoint parsing allocate memory quadratic in the type size
  while gas grew only linearly, trapping the PVM. (Types synthesized during
  typechecking are not bounded the way L1 threads `Type_size` through every
  constructor; that is O(N) memory, not the O(N²) trap.) (!22297)
- `COMPARE` (and `set`/`map` literal key-ordering checks at typechecking) now
  charge their gas incrementally while walking the value, so a comparison over
  a maximally-shared value (an O(n) in-memory DAG that expands to 2^n nodes)
  runs out of gas mid-walk instead of doing the full walk before charging. The
  total gas charged is unchanged. (L2-1654)
- Tickets are temporarily disabled in the Michelson runtime: the
  `ticket` type and the `TICKET`, `READ_TICKET`, `SPLIT_TICKET` and
  `JOIN_TICKETS` instructions are now rejected during typechecking.
  Support can be re-enabled by building with the `tickets` cargo
  feature once the conditions for re-enabling them are agreed.
  (!22292)
- The `APPLY` instruction now rejects a non-packable captured value
  (e.g. `big_map` or `ticket`) during typechecking, matching L1, which
  requires the captured type to be packable with contracts disallowed.
  Previously the Michelson runtime typechecker accepted such scripts,
  diverging from L1 and producing closure storage that failed to reload
  on the next call. (!22248)
- MIR `APPLY` now shares captured arguments inside applied closures and charges
  MIR's size-dependent type/value unparsing cost when the closure is created,
  at the same materialization point as L1, avoiding repeated deep copies when a
  duplicated applied closure is later executed. The unparsing computed at
  `APPLY` is the closure's sole serialized form, reused verbatim by every
  `PACK`/unparse (never recomputed), and executing an applied closure now
  charges MIR's `PUSH + PAIR` costs per captured argument, mirroring L1's
  `PUSH ty val ; PAIR` instruction sequence. (!22270)
- Michelson `signature` values are now compared and hashed by their raw
  bytes, matching L1 (`Signature.compare` over `to_bytes`). Previously the
  ordering keyed on the signature variant first, so the same bytes read as
  an `edsig...` literal and as optimized bytes ranked as distinct values;
  the Michelson runtime then accepted a `set signature` that L1 rejects as
  a duplicate, with divergence on `COMPARE`, set/map validation and storage
  round-trips. (!22244)
- A failing `UNPACK` of a packed (`0x05`) payload now charges the
  size-dependent `unpack_failed` gas penalty before pushing `None`, like L1.
  Previously the failure paths (binary decode or typechecking) returned
  `None` without the penalty, undercharging large failing payloads.
  (!22263)
- Internal operations are now capped at L1's limit of 65,535
  (`Too_many_internal_operations`): the producing instructions
  (`TRANSFER_TOKENS`, `SET_DELEGATE`, `EMIT`, `CREATE_CONTRACT`) fail once
  nonce 65,535 would be allocated. On the operation path the count is
  cumulative across the block like L1's `internal_nonce`; the offending
  operation fails in isolation (the block is not aborted) and internal
  operation nonces are now 0-based, matching L1. (L2-1644)
- The `VIEW` instruction now gates execution on two type-equality checks,
  matching L1: the callee view's declared output type must equal the
  caller-requested return type, and the caller's argument type must equal
  the callee's declared input type. On mismatch `VIEW` pushes `None`
  instead of executing the view (which previously could return `Some` with
  a value of the wrong type). The checks are gas-metered like L1's `ty_eq`.
  (!22234)
- `CREATE_CONTRACT` now validates the embedded child script's view
  declarations like L1, both at origination and when typechecking a
  parameter value that carries an embedded `CREATE_CONTRACT`: a child view
  with a forbidden input/output type (`big_map`, `ticket`, `operation`,
  `sapling_state`), an ill-typed body, or a forbidden-in-view instruction is
  now rejected instead of being silently originated. Re-typechecking an
  already-stored contract (entrypoint extraction, execution) is unchanged,
  so already-originated contracts keep executing. (!22235)
- Failed cross-runtime call receipts now persist the failing error once
  (on the `alias→target` internal op that indexers read) instead of twice.
  The per-byte gas charge for the persisted error body is halved accordingly. (!22257)
- When a cross-runtime call fails because a deeper internal operation
  FAILWITHs (the directly-called target itself succeeds), the synthetic
  `alias→target` internal op is now marked `backtracked` instead of `failed`,
  matching native Tezos L1 semantics where ancestor ops are backtracked and the
  error lives once on the actually-failing descendant. (!22259)
- The `TICKET` instruction now rejects a non-comparable ticket content
  (e.g. `ticket (ticket string)`) during typechecking, matching L1,
  which requires ticket content to be comparable. Previously the
  Michelson runtime typechecker accepted such scripts, diverging from
  L1 and allowing ticket duplication via `READ_TICKET` on a nested
  ticket. (!22218)
- The `CONTRACT` instruction now rejects a non-passable parameter type
  (e.g. `CONTRACT operation`) during typechecking, matching L1. Previously the Michelson
  runtime typechecker accepted such scripts, diverging from L1.
  (!22217)
- An originated contract's KT1 address is now derived from origination index 0
  for the first origination of an operation, matching Tezos L1
  (use-then-increment), instead of index 1. This realigns the KT1 returned by
  `CREATE_CONTRACT` and stored in `originated_contracts` with the L1-canonical
  address computed from the operation hash and the (renumbered) receipt nonce.
  Every originated KT1 (including inbound-CRAC child contracts) shifts by one
  index, so addresses on existing networks change. (L2-1642)
- Fixed a spurious failure when a contract moves a persisted big map from
  its parent storage into an outgoing operation (e.g. a `TRANSFER_TOKENS`
  parameter) while replacing its storage with one that no longer
  references that big map. The contract-result dump used to remove the
  big map during the returned-storage pass and then fail copying it during
  the operation-list pass; removal of no-longer-referenced big maps is now
  deferred until after the operation list has been dumped. (!22220)
- Duplicated internal operations are now rejected as replays. The
  `operation` type is `Duplicable`, so a contract can `DUP` an
  `operation` value and emit both copies; the runtime previously
  assigned each copy a fresh nonce and applied both, letting a
  transfer, ticket transfer, or origination run twice while L1
  backtracks. Two internal operations sharing the same operation
  counter within a single top-level operation are now rejected
  (matching L1 `Internal_operation_replay`) and the whole operation
  backtracks. The operation counter is now also threaded through the
  Michelson journal across reentrant inbound cross-runtime frames, so
  the replay identity spans the whole atomic cross-runtime transaction
  rather than restarting at zero for each `EVM → Michelson` frame —
  matching L1's single internal-nonce namespace for a manager
  operation's internal-op tree. (!22219)
- The Michelson storage-fees burn is now rendered on the CRAC-triggering
  operation: an Applied content that delegates storage cost to its
  callee carries the dual `(payer −V, storage fees +V)` balance-updates
  pair, where V aggregates every cost the operation delegated through
  CRAC. (!22166)
- **Security fix:** reject forged lazy-storage ids in operation
  parameters. A `big_map` parameter passed as a bare id (or id-plus-diff)
  is no longer accepted in an externally-supplied or cross-runtime
  operation parameter, mirroring L1's `allow_forged_lazy_storage_id`
  gate. Previously a contract could read or even overwrite another
  contract's `big_map` by forging its id; such a parameter now fails to
  typecheck. Forged ids remain allowed for a contract's own committed
  storage and for internal-call parameters. (!22207)
- **Security fix:** reject forged lazy-storage ids in the initial storage
  of an external origination, mirroring L1's
  `allow_forged_lazy_storage_id_in_storage`. Previously a contract could
  be originated with a `big_map` it does not own (a forged id), gaining
  read/write access to another contract's `big_map`; such an origination
  now fails to typecheck. Internal `CREATE_CONTRACT` originations are
  unaffected — their storage is already typechecked by the emitting
  contract and may legitimately carry the big_maps it owns. (!22208)
- Removed the legacy `%default` (simple transfer, `string`) entrypoint
  from the gateway contract. Use the generic `%call` entrypoint instead:
  a POST to `http://ethereum/<address>` with an empty body and the
  operation's amount attached performs the same transfer. Calling the
  removed entrypoint — including a plain tez send with no parameter —
  now fails with an unknown-entrypoint error. The Michelson alias
  forwarder script deployed for new aliases forwards through `%call`;
  forwarders originated before this change keep the old script and fail
  to forward until their alias is re-created.
- A native Michelson `FAILWITH` reached through an EVM→Michelson
  cross-runtime call now charges the gateway per-byte payload surcharge
  on the error body before it is persisted in the failed CRAC receipt and
  before the 4xx response body is returned to the EVM caller. Previously
  the attacker-controlled `FAILWITH` payload was copied unmetered into both
  sinks — the persisted receipt BSON and the EVM-side revert returndata —
  letting a malicious Michelson contract bloat Michelson block metadata at
  near-zero cost. The persisted form (`Debug`-rendered) is now bounded at
  the same per-byte rate used by `GatewayError` bodies (see !22068); on gas
  exhaustion the receipt records a small `OutOfGas` error instead of the
  full payload, preserving the receipt invariant. (!22158)
- Re-priced the gas costs of many Michelson instructions
  from the values inherited from L1 (a Tallinn-era snapshot) to values
  re-benchmarked directly on the MIR (Rust) interpreter. This changes the
  gas reported on receipts and `eth_estimateGas` for any Michelson script
  using these instructions. (L2-1553, L2-1540, L2-1544, L2-1552, L2-1555,
  L2-1549 and others to come)
- Big-map usage now counts toward a contract's storage footprint, so
  receipts and storage burn cover big-maps the same way they do on
  L1. (!22040)
- Bootstrap contracts injected at genesis now have their `used_bytes`
  / `paid_bytes` storage space initialised to the size they
  occupy. (!22089)
- Introduce a single shared, kernel-owned Michelson alias implementation at
  `/tez/tez_accounts/tezosx/__system__/alias_implementation`, seeded with the
  current forwarder code at Michelson runtime activation and via storage
  migration. Foundation for upgrading every alias atomically; no behaviour
  change yet, as nothing resolves the slot. (!22099)
- Michelson alias `KT1`s classified `Origin::Alias` and carrying no
  `/data/code` now resolve their script to the shared implementation,
  transparently for execution and the entrypoints RPC. Dormant until aliases
  are originated code-less. (!22103)
- Tezos X aliases are now materialized without a per-contract script: a new
  alias `KT1` stores no `/data/code` and resolves to the shared
  implementation. The EVM node's Michelson RPC backend resolves code-less
  aliases too, so `/script`, `/storage` and `/entrypoints` stay consistent
  with the kernel. (!22105)
- Add the O(1) primitive that upgrades the shared alias implementation in a
  single durable-storage write, changing the behaviour of every code-less
  alias at once. It enforces entrypoint monotonicity (an exposed entrypoint
  may never be removed, renamed, or retyped) to protect contracts that
  hardcode entrypoint annotations, and forbids changing the storage type
  (which would brick every alias at its next execution). No governance
  trigger is wired yet. (!22157)
- Improved the performance of reading an implicit account's balance,
  counter, and manager in the Michelson runtime by reducing the number
  of durable-storage accesses each read performs. (!22137, !22194)
- Improved the performance of originated-contract storage accounting in
  the Michelson runtime: a contract's code size, storage size, and
  used/paid storage watermarks are now kept in a single durable-storage
  record, read and written together instead of as four separate keys.
  (!22139)
- Dropped the per-account `/origin` classification record for implicit
  accounts in the Michelson runtime: a `tz1`/`tz2`/`tz3` is Tezos-native by
  construction (it can never be a cross-runtime alias), so its classification
  no longer touches durable storage. Reveal now performs one durable write
  instead of two, and cross-runtime classification of an implicit address
  needs no read. (!22170)
- Re-priced originated-contract storage-accounting gas in the Michelson
  runtime to charge the aggregated `/info` record actually read and
  written (each at its encoded size), instead of mirroring L1's
  per-counter cost model. This lowers the `consumed_milligas` reported on
  receipts and `eth_estimateGas` for operations that update an originated
  contract's storage. (!22173)
- Expose a contract's storage-space watermarks through two read-only
  RPCs, `GET .../contracts/<id>/storage/used_space` and `.../paid_space`,
  mirroring L1's `get_used_storage_space` / `get_paid_storage_space`
  (same URL shape and `Z`/`null` response). An originated contract
  returns the stored value (`0` when unwritten); an implicit account
  returns `null`. (!22083)
- The `big_maps/index/<id>` raw-info RPC now returns the big-map's actual
  `total_bytes`, read from the kernel-maintained per-big-map size counter
  in durable storage, instead of a hardcoded zero. (!22080)
- A failed cross-runtime call (4xx) now charges the gateway per-byte
  payload surcharge on the response body before folding it into the
  persisted `GatewayError`. Previously the attacker-controlled EVM revert
  body was copied unmetered into the failed CRAC receipt — twice, in both
  the top-level failed result and the failed internal transfer — letting
  an EVM callee bloat Michelson operation/block metadata. The charge
  tracks the decoded length actually stored, so the body is now gas-
  bounded like any `FAILWITH` payload. (!22068)
- A cross-runtime call or `staticcall_evm` view whose EVM target
  exhausts the forwarded gas (HTTP 429) now fails closed with an
  out-of-gas error instead of being collapsed onto a generic 4xx
  outcome. Previously the `staticcall_evm` view returned `None` on EVM
  out-of-gas, indistinguishable from an absent/no-result view, so a
  caller treating `None` as a default/allow/fallback branch could be
  driven there by a gas-exhausting EVM target. (!22035)
- MIR: prevent kernel stack overflow on deeply nested Michelson —
  Runtime-built deep `TypedValue`s left on a value stack, in a
  control-flow worklist frame, or inside `InterpretError::FailedWith`
  are flattened iteratively on the interpreter's *drop / error-unwind*
  path so their `Rc<TypedValue>` destructor cannot overflow when the
  kernel drops the error or the caller-restored stack. The COMPARE
  gas-cost computation and the `Ord` impl for `TypedValue` — reached by
  both the `COMPARE` instruction and `BTreeSet`/`BTreeMap` key ordering —
  also run on iterative worklists so deep `pair`/`option`/`or` values can
  no longer overflow the kernel stack inside the cost pre-charge (which
  previously ran before any gas could gate it).
  (!22024, !22025)
- MIR: charge Michelson gas for each pending frame on the iterative
  interpreter's worklist. L1's protocol Alpha already prices
  continuation pushes (`KCons`/`KLoop_in`/`KIter`/`KReturn` at 10
  milligas each), and MIR mirrors those constants for their own gas
  accounting purposes. The new charge is a *host-memory-protection
  surcharge layered on top* of that: L1's recursive OCaml
  interpreter holds the continuation chain on the runtime stack
  under automatic tail-call elimination so per-frame *memory* is
  ≈ 0, while MIR's iterative driver allocates each pending frame on
  the heap. Without per-push gas, a divergent
  `LAMBDA_REC int int { EXEC }` only paid `EXEC`'s ~10-milligas
  per-call constant and could grow the worklist into the multi-GB
  range under a single Michelson budget — long before Michelson gas
  exhausted — and abort the kernel WASM module with `unreachable`.
  Two new gas costs — `FRAME_PUSH = 100` and `STACK_PUSH = 500` —
  are now deducted at every worklist-growth push in `handle_step`
  (`OpenBlock` from `IF*`, `OpenDip`, `OpenLoop*`, `OpenIter*`,
  `OpenMap*`, `OpenExec`, `OpenView`).
  Concrete surcharges per affected Michelson instruction:
  `IF*` / single-arm `OpenBlock`: +100 milligas; `DIP`, the three
  `MAP` variants: +200 milligas; `LOOP*`, `ITER` (non-empty),
  `VIEW`: +100 milligas plus the `STACK_PUSH` for `VIEW` (=600
  milligas total); `EXEC`: +700 milligas per call (one
  `STACK_PUSH = 500` + two `FRAME_PUSH = 100`).
  Per-call gas profile therefore diverges from L1's by `+100
  × N_handle_step_pushes + 500 × N_OpenExec_OpenView` milligas —
  visible on receipts and `eth_estimateGas` for control-flow-heavy
  contracts. Clients with cached gas estimates should refresh after
  this release; static estimators tracking L1 must adjust their
  per-instruction tables accordingly.
  (!22071)
- MIR's `address` decoder now matches L1's `parse_address` on the
  trailing entrypoint name: it is validated only by L1's address-value
  rule (length ≤ 31 and not the explicit `default`), not the Michelson
  script-source annotation charset. Entrypoint names containing bytes
  outside `[_0-9a-zA-Z]` (e.g. `%!`), a leading `.`/`%`/`@` (e.g.
  `%.foo`), or non-UTF-8 bytes (e.g. a trailing `0xff`) are now accepted
  on both the binary (`UNPACK address`) and readable (`PUSH address
  "tz1…%…"`) paths, matching L1; previously they were rejected (`UNPACK`
  returned `None` and the literal was ill-typed), diverging from L1. The
  entrypoint name is now stored as a raw byte string, as on L1. The same
  Michelson source or packed payload now produces the same `address`
  value on L1 and the Michelson runtime. (!22013)
- MIR's `address` decoder now matches L1's `parse_address` on the
  trailing entrypoint name: it is validated only by L1's address-value
  rule (length ≤ 31 and not the explicit `default`), not the Michelson
  script-source annotation charset. Entrypoint names containing bytes
  outside `[_0-9a-zA-Z]` (e.g. `%!`), a leading `.`/`%`/`@` (e.g.
  `%.foo`), or non-UTF-8 bytes (e.g. a trailing `0xff`) are now accepted
  on both the binary (`UNPACK address`) and readable (`PUSH address
  "tz1…%…"`) paths, matching L1; previously they were rejected (`UNPACK`
  returned `None` and the literal was ill-typed), diverging from L1. The
  entrypoint name is now stored as a raw byte string, as on L1. The same
  Michelson source or packed payload now produces the same `address`
  value on L1 and the Michelson runtime. (!22013)
- The `run sandbox` and `run tezlink sandbox` commands accept
  `--michelson-hard-gas-limit-per-block` to raise the Michelson per-block
  gas cap (default 3M). Sandbox-only; intended for capacity benchmarking.
  (!21968)
- Michelson block receipts now bracket every CRAC frame with a paired
  begin (`"crac"`) and end (`"crac_end"`) synthetic event in
  `internal_operation_results`. The markers carry the same null-implicit
  sender and `crac_id` payload as before; the new `"crac_end"` tag
  distinguishes end from begin. Markers form a balanced parenthesis
  forest: an indexer can reconstruct the full frame tree by filtering to
  ops with null sender and tag `"crac"` or `"crac_end"`, then
  stack-walking in list order. Coverage is complete: EVM-originated
  top-level frames, nested re-entrant frames, sibling frames within one
  EVM transaction, and Tezos-originated re-entry into Michelson all
  carry bracket pairs. The existing begin-event byte shape is unchanged,
  so consumers that only inspect the `"crac"` begin event keep working.
  (!22151)
- Fix the Tezos-side cross-runtime gateway gas surcharges (value transfer,
  alias derivation, per-word payload crossing), which were stale `×100`-era
  milligas constants not rescaled when the EVM-to-milligas coefficient dropped
  to 22. A Tezos-originated cross-runtime call was over-charged for these costs
  relative to the equivalent EVM-originated call. The EVM base costs now live
  in a single shared crate that both runtimes derive from (the Michelson side
  via the milligas coefficient), and the payload-crossing surcharge is charged
  per 32-byte word exactly like the EVM precompile, so the two sides can no
  longer desync. The separate per-byte bound on attacker-controlled error
  bodies persisted into the failed-call receipt is kept at its prior rate (it
  is a DoS guard, not an EVM-parity crossing cost). (!22323)

### Native Atomic Composability

- **Breaking change for integrators:** the user-facing names of a
  cross-runtime call were renamed; internal kernel symbols are unchanged.
  - HTTP context headers `X-Tezos-CRAC-ID` and `X-Tezos-CRAC-Depth` are now
    `X-Tezos-Cross-Runtime-Call-ID` and `X-Tezos-Cross-Runtime-Call-Depth`.
  - The EVM events `CracSent`, `CracReceived` and `CracIdEvent` are now
    `CrossRuntimeCallSent`, `CrossRuntimeCallReceived` and
    `CrossRuntimeCallIdEvent`, and their `cracId` field is now
    `crossRuntimeCallId`. Each event's `topic0` signature hash changes
    accordingly, so log indexers must update both the signature and the
    field name.
  - The Michelson synthetic event tags `crac` and `crac_end` (the paired
    cross-runtime-call frame markers) are now `cross_runtime_call` and
    `cross_runtime_call_end`.
  - The synthetic transaction hash mirroring a cross-runtime call changed
    (its hash domain separators were renamed), so the hashes reported in
    receipts and traces differ.
  - Operator-visible error, abort and log messages now read "cross-runtime
    call" instead of "CRAC". (!22198)
- EVM alias creation is now tied to the transaction outcome: the forwarder
  code and classification are staged in the journal and flushed only when
  the enclosing operation commits, so a revert leaves no durable alias. (!22046)
- An alias forwarder the Michelson runtime creates for a cross-runtime
  call is now reverted with the parent operation. Previously it was
  written to durable storage during gateway resolution and survived a
  failed parent unburned, allowing storage growth at no cost. (!22056)
- Cross-runtime calls re-entering from an inbound CRAC now attribute the
  transitive originator (not the immediate caller) as the call source,
  fixing the source reported in EVM→Michelson CRAC receipts. (!22065)
- Nested EVM→Michelson CRAC legs are now recorded and folded into the
  originating Michelson operation's internal operations when the call chain
  originates in the Michelson runtime — including a top-level manager
  operation that calls the gateway `%call_evm` directly — instead of being
  dropped. (!22065)
- A self-recursive cross-runtime gateway cycle now fails as a clean
  operation-level revert instead of exhausting the kernel's WASM memory.
  The cross-runtime chain-depth cap is lowered so the catchable over-depth
  rejection fires well before the cycle reaches the heap-exhaustion
  threshold (the earlier cap sat above it, so a heap trap hit first). This
  keeps the offending operation's failure local: the rest of the blueprint
  still applies and any co-resident operations — including deposits — are
  preserved, rather than being lost when the whole blueprint is dropped on
  trap recovery. (!22320)
- The synthetic cross-runtime-call frame (begin marker, inner ops, end
  marker) is now ordered immediately after the gateway internal operation
  that triggered it and ahead of the result callback and its fan-out in the
  Michelson operation's `internal_operation_results`. Previously the
  callback and its sub-operations appeared before the frame that produced
  the payload they consume, inverting execution order in the receipt. (!22344)
- A cross-runtime call into the Michelson runtime no longer reports a
  maximum value as its consumed gas when it fails. A failure that has begun
  metering reports the gas it consumed; a failure before metering reports
  the granted operation limit. The EVM gateway classifies the response
  status before charging, so both are charged their reported gas and stay a
  catchable revert, with the unmetered case draining the granted budget.
  (!22340)

### Storage versions

- The storage version is now 61. (!22099)

### Internals

- The cross-runtime gateway no longer clones each leg's request body and
  full response into the journal's in-memory HTTP-trace stack unless HTTP
  trace capture is requested. The clone is gated on the `http_trace_enabled`
  flag on both the block-production path (off by default) and the simulation
  path — so a normal block, an `eth_call` and an `eth_estimateGas` retain no
  O(payload) uncharged bytes per cross-runtime leg, while the `http_trace*`
  RPCs still capture (the node sets the flag before the trace replay or
  simulation). (!22352)
- Michelson callee now reports the storage cost it allocates back to
  the caller through `X-Tezos-Storage-Cost` (post-execution) and
  `alias_storage_cost` (pre-dispatch, on alias materialization).
  Previously the cost was dropped. (!22124)
- Michelson caller now consumes the storage cost a callee delegates
  back through `X-Tezos-Storage-Cost` and `alias_storage_cost`, and
  burns the accumulated total on the sender's balance at the top-level
  manager-op. Previously the cost was dropped. (!22124)
- The Michelson burn pass no longer charges operations surfaced in
  the manager-op receipt tree by the CRAC drain mechanism. Each
  frame is now responsible for the storage fees of its own
  allocations; operations from a CRAC sub-execution are skipped at
  this burn pass — their cost is not billed against the parent's
  `storage_limit`. A standalone Michelson manager-op that does not
  cross a CRAC boundary observes no change. (!22095)
- EVM caller absorbs the storage cost a CRAC callee delegates back
  through `X-Tezos-Storage-Cost`: `g2 = V / base_fee_per_gas` is
  deducted from the gas remaining at the return site, reverting
  with `OutOfGas` if insufficient. (!22094)
- EVM caller absorbs the storage cost the target runtime delegates
  back through the alias-resolution interface when an alias is
  materialized on outgoing CRAC: `g2_alias = cost / base_fee_per_gas`
  is deducted from the gas remaining at the resolution site, before
  HTTP dispatch. (!22094)
- Reserve the `X-Tezos-Storage-Cost` CRAC response header in the
  `X-Tezos-*` namespace. The header lets a callee convey, in mutez,
  the storage cost it asks the caller to bill back for bytes
  allocated during a CRAC sub-execution. (!22093)
- The sequencer block producer now budgets transactions against the
  *serialized* blueprint size (the per-transaction runtime tag and RLP
  framing of the V1 format), instead of the raw transaction size, and
  reserves the fixed blueprint framing from the chunk budget. Previously a
  batch could satisfy the raw-size budget yet serialize to more than the
  maximum number of chunks the kernel accepts, causing block production to
  fail. (!22092)
- The sequencer block producer now clears the optimistically-popped pending
  transactions when block production fails, so a rejected blueprint can no
  longer permanently stall the sequencer. (!22092)
- The `blueprint_application` event now accounts for Michelson runtime
  operations in its `txs_nb` count. (!22155)

## Version 0.6 (ae3d731879b9443f52dc14de64e3208ab256d7a0)

### EVM Runtime

- Alias materialization is now atomic: if the forwarder initialization
  fails (revert, halt, or a gas budget below the intrinsic cost), the
  delegation `code_hash` write is rolled back instead of being left
  behind. Previously a failed init left a half-materialized account that
  a later call would bless without re-running initialization,
  permanently bricking the alias (uninitialized forwarder). (!21978)
- On an inbound cross-runtime call (a Michelson contract calling an EVM
  contract), the EVM `ORIGIN` opcode (`tx.origin`) now returns the
  transaction originator (`X-Tezos-Source`) instead of the immediate
  caller, while `msg.sender` keeps returning the immediate caller
  (`X-Tezos-Sender`). Previously both returned the immediate caller, so
  the Ethereum `require(tx.origin == msg.sender)` EOA-only guard was
  silently bypassed by routing a call through the Michelson runtime.
  When the originator is the immediate caller (a direct EOA call),
  `tx.origin == msg.sender` as before. (!21981)

### Michelson Runtime

- The `monitor/heads/main` RPC now honors the `protocol` and
  `next_protocol` query filters and emits the current head immediately as
  the first element of the stream, matching the L1 `monitor_heads`
  behavior. Previously the filters were ignored (clients could receive
  heads they had filtered out) and the current head was never sent on
  subscription, blocking wait-for-head flows until the next block. (!22023)
- When the Michelson runtime services an inbound cross-runtime call, the
  gateway now forwards the call's originator (the alias carried in
  `X-Tezos-Source`) as the outbound `X-Tezos-Source`, instead of this
  runtime's null operation source. This keeps `tx.origin` invariant
  across an `EVM -> Michelson -> EVM` round-trip — it resolves back to
  the originating EOA — rather than collapsing every originator onto a
  single `alias(null)`. (!21981)
- The `staticcall_evm` view's read-only path now resolves the nested EVM
  call's originator (→ `tx.origin`) from the cross-runtime originator
  captured on the shared journal, instead of reverse-resolving the
  inbound caller alias through a durable origin record. When a Michelson
  view services an inbound cross-runtime call, the entering EVM gateway
  has already stored the originator's complete native `(runtime, address)`
  identity there, so an `EVM -> callMichelsonView -> VIEW -> staticcall_evm
  -> EVM` round-trip preserves the outer EVM originator — and its
  `X-Tezos-Source-Runtime` — even when that originator's Michelson alias
  was never materialized. This makes the Michelson→EVM read-only path
  record-independent, matching the EVM→Michelson path. The immediate
  Michelson caller's alias stays `X-Tezos-Sender` (→ `msg.sender`); a
  top-level Michelson view with no captured originator forwards the
  operation source. To make the originator available across the boundary,
  it (`OriginalSource`) now lives on the shared cross-runtime journal
  rather than the EVM sub-journal, and the EVM gateway's read-only entries
  (`callMichelsonView`, GET `call`) persist it like the state-mutating
  entries already did. (L2-1455)
- The read-only `staticcall_evm` view dispatch charges one alias-lookup
  gas cost for the immediate caller's durable origin read. The captured
  originator is read from the journal at no durable-read cost, so an
  inbound-CRAC view pays only that single lookup, while a top-level view
  (which resolves the operation source itself) pays a second one.
  Previously these read-only origin lookups were unmetered, so the view
  under-priced its durable storage reads. (L2-1455)
- MIR's `PUSH timestamp "..."` value parser now matches L1's
  `Script_timestamp.of_string`: a decimal-string literal larger than
  `i64::MAX` falls back to arbitrary-precision parsing instead of being
  rejected, and an RFC3339 leap-second (`...:60Z`) realigns onto the
  following POSIX second the way L1's Ptime does, instead of silently
  decoding to the prior second. The integer fallback now also follows
  Zarith's `Z.of_string` grammar byte-for-byte — an optional `-` then an
  optional `+` then a `0x`/`0o`/`0b` radix prefix — so e.g. `"-+42"` and
  `"-+0x10"` parse (to `-42` and `-16`) while `"++42"`, `"+-42"` and
  `"0x+10"` are rejected, on both sides. Same Michelson source now
  produces the same `timestamp` value on L1 and the Michelson runtime.
  (L2-1375)
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
- MIR: prevent kernel stack overflow on deeply nested Michelson — Micheline
  (de)serialization, typechecking, interpretation, value/instruction
  destruction, and the residual gas-sizing / type-equality / PACK walks now
  run on iterative heap worklists instead of recursion, so adversarially deep
  input is bounded by gas rather than trapping the PVM. Runtime-built deep
  `TypedValue`s left on a value stack, in a control-flow worklist frame, or
  inside `InterpretError::FailedWith` are flattened iteratively on the
  interpreter's *drop / error-unwind* path so their `Rc<TypedValue>`
  destructor cannot overflow when the kernel drops the error or the caller-
  restored stack. The conjugate *observation* path — the auto-derived
  `Debug` walk on a deep `TypedValue` invoked by the kernel's error-body
  formatter — is closed separately by the iterative `Debug` rewrite
  (tracked under !21988).
  The COMPARE gas-cost computation and the `Ord` impl for `TypedValue` —
  reached by both the `COMPARE` instruction and `BTreeSet`/`BTreeMap` key
  ordering — also run on iterative worklists so deep `pair`/`option`/`or`
  values can no longer overflow the kernel stack inside the cost pre-charge
  (which previously ran before any gas could gate it).
  The value-level `PUSH (lambda T1 T2) <body>` typecheck is also routed
  through the worklist driver, closing a sequencer DoS reachable via
  `callMichelsonView` input bytes that overflowed the kernel's WASM stack
  around depth ~224 — well below the L1-parity 10 000-deep typecheck guard.
  (!21982, !21983, !21984, !21985, !21986, !22024, !22025, !22029)
- MIR: `UNPACK string` now rejects carriage return (`0x0d`) — and any
  other byte outside L1's permitted set of newline (`0x0a`) plus
  printable ASCII (`0x20..=0x7e`) — by returning `None` instead of
  `Some "...\\r..."`. The textual lexer no longer recognises `\r` as
  an escape for the same reason. Strings whose bytes are all in the
  permitted set are unaffected. (!22005)
- MIR: `UNPACK` now rejects non-canonical zarith encodings — multi-byte
  `int`/`nat`/`mutez`/`timestamp` values terminated by a `0x00` group
  (e.g. `0x0500b9c08100`, `0x05008100`) — returning `None` to match
  Tezos L1. Canonical encodings are unaffected. (!21992)
- MIR: `LSR bytes` no longer traps with `Overflow` when the shift
  count does not fit in `usize` (e.g. `2^64` on a 64-bit host). Any
  count at or beyond the operand's bit width now shifts the bytes
  out completely and returns empty bytes, matching L1's
  `Script_bytes.bytes_lsr`. Shift counts that fit in `usize` and are
  below the operand's bit width are unaffected. (!22011)


### Native Atomic Composability

- Cross-runtime calls now enforce a maximum chain depth
  (`X-Tezos-CRAC-Depth` ≤ 128): a self-recursive Michelson↔EVM gateway
  cycle that previously wedged sequencer block production now fails
  cleanly at the operation level (catchable revert) once the cap is
  reached, leaving the block intact. (!21972)
- Error that can be user triggered now maps now doesn't block abort. (!21918)
- A failed inbound cross-runtime call now always records its failed CRAC
  receipt in the Michelson block, even when the caller's leftover gas is
  too low to encode the synthetic CRAC-ID event. The kernel now sponsors
  that event's encoding against a dedicated gas counter instead of
  charging it to the caller, so a gas-tight failed CRAC can no longer be
  returned as a catchable failure to the EVM caller while silently
  dropping its receipt (which left indexers and call-graph
  reconstruction blind to the attempted call); by the same token, a
  successful CRAC can no longer be downgraded to an out-of-gas error by
  this receipt bookkeeping. (!22028)
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
- Add two read-only view selectors to the runtime-gateway precompile
  at `0xff…07`:
  - `originOf(addr, sourceRuntime)` — returns a three-field tuple
    `(uint8 kind, uint8 homeRuntime, string nativeAddress)` where
    `kind` is `0` (Unknown), `1` (Native), or `2` (Alias). For an
    EVM source with no `/origin` record, a code-presence check
    acts as a back-stop: a non-empty `code_hash` is treated as `Native`.
    This catches contracts deployed via CREATE/CREATE2 and EOAs that
    installed a delegate via EIP-7702 SET_CODE — both definitively
    native to the EVM runtime. Malformed addresses
    return `Unknown` without reverting.
  - `resolveAddress(addr, sourceRuntime, targetRuntime)` — returns
    `(bool classified, uint8 res, string translated)`. `classified`
    is `false` when the address is unknown or malformed.  When `true`,
    `res` is `0` (Recorded — the inverse alias is already materialized)
    or `1` (Derived — computed but not yet written).  A same-source
    short-circuit path skips all storage reads when
    `sourceRuntime == targetRuntime`. Both selectors are STATICCALL-
    compatible: non-payable, no journal writes, no log emission.
    Gas constants: `ORIGIN_OF_BASE_COST = 1 500`,
    `RESOLVE_ADDRESS_BASE_COST = 1 500`, `CODE_BACKSTOP_COST = 2 100`
    (conditional, charged only when the EVM back-stop path executes).
- `X-Tezos-Sender` on an inbound Michelson request now accepts an
  implicit (`tz1`/`tz2`/`tz3`) account in addition to an originated
  KT1, matching standard Michelson semantics (a KT1 sees `SENDER = tz1`
  when called directly by a user) and the round-trip principle already
  applied to same-runtime calls in !21963. The previous strict KT1
  requirement rejected a legitimate `tz1` round-tripped from the EVM
  gateway's stored alias `native_address`, which the gateway
  reclassified as `CracError::BlockAbort` — a latent block-abort
  trigger gated only by the per-hop gas geometry. (!22004)

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

- Add two synthetic read-only views on the TezosXGateway enshrined
  contract, callable via the Michelson `VIEW` opcode:
  - `originOf :: (pair string nat) -> (or unit (or nat (pair nat string)))`
    — Given `(address, source_runtime)`, returns `Left Unit` for an
    unknown address, `Right (Left nat)` with the source runtime id for a
    native address, or `Right (Right (Pair nat string))` with the alias's
    home runtime and native address string for an aliased address. For
    EVM sources with no `/origin` record, a code-presence back-stop is
    applied: non-empty bytecode (`code_hash != KECCAK_EMPTY`) is treated
    as `Native`. Invalid runtime ids FAILWITH
    `(Pair "INVALID_RUNTIME_ID" received_nat)`.
  - `resolveAddress :: (pair string (pair nat nat)) -> (option (pair nat string))`
    — Given `(address, source_runtime, target_runtime)`, returns `None`
    when the address is unknown, and `Some (Pair resolution_nat translated_str)`
    otherwise. `resolution_nat` is `0` (recorded — the inverse alias is
    already materialized in storage) or `1` (derived — the alias address
    was computed but not yet written). A same-source short-circuit skips
    all storage reads when `source_runtime == target_runtime`, returning
    the input address directly. Both views FAILWITH
    `(Pair "INVALID_RUNTIME_ID" received_nat)` when `source_runtime` or
    `target_runtime` does not map to a known runtime id. Both views are
    read-only and `STATICCALL`-compatible: no journal writes, no log
    emission. Gas model: a `resolveAddress` call with an `Origin::Native`
    (or transitive `Alias`) source pays `ALIAS_LOOKUP_MILLIGAS` (source
    origin read) + `DERIVE_ALIAS_MILLIGAS` (alias derivation) +
    `ALIAS_LOOKUP_MILLIGAS` (destination origin read); the same-source
    short-circuit pays only `ALIAS_LOOKUP_MILLIGAS`. The milligas
    equivalent of `CODE_BACKSTOP_COST` is charged additionally when the
    EVM code-presence back-stop fires. Constants:
    `ALIAS_LOOKUP_MILLIGAS = 210 000`,
    `DERIVE_ALIAS_MILLIGAS = 150 000` (placeholder, to be calibrated).

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
