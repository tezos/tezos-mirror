# lib_smart_rollup_node -- Core smart rollup node library

Library name: `octez_smart_rollup_node` (package `octez-smart-rollup-node-lib`)

Dune file is auto-generated from `manifest/main.ml` -- **do not edit directly**.

## Purpose

Implements the Tezos smart rollup node: follows L1 chain, executes PVM on inbox messages, publishes commitments, plays refutation games, exposes RPCs. Protocol-agnostic core with protocol plugins.

## Two sub-libraries in one dune file

This directory defines **two** libraries with different module sets and opens:

1. **`octez_smart_rollup_node_store`** (modules: `store_version`, `sql_store`, `store_sig`, `store`)
   Opens: `Tezos_base.TzPervasives`, `Tezos_base`, `Tezos_stdlib_unix`, `Tezos_layer2_store`, `Octez_sqlite`, `Octez_smart_rollup`

2. **`octez_smart_rollup_node`** (all other modules)
   Opens: `Tezos_base.TzPervasives`, `Tezos_base`, `Tezos_stdlib_unix`, `Tezos_crypto`, `Tezos_client_base`, `Tezos_client_base_unix`, `Octez_injector`, `Tezos_version_value`, `Tezos_layer2_store`, `Tezos_layer2_irmin_context`, `Tezos_layer2_riscv_context`, `Octez_smart_rollup_node_store`, `Octez_crawler`, `Tezos_workers`, `Octez_smart_rollup`

**Do not add `open` for these modules -- they are already in scope.** A module available in the main library may not be available in the store sub-library (e.g., `Octez_injector` is only in the main library).

## Node_context.t -- Central state object

Almost every function takes `Node_context.t`. It uses phantom types to encode **store** and **context** access permissions at compile time (see `lib_layer2_store/CLAUDE.md` for the full permission system).

### Three permission axes

`Node_context.t` tracks permissions on two resources independently: the **store** (SQLite database) and the **context** (PVM state backend). The context itself has two sub-permissions: the **index** (used for checkout/commit/GC) and the **state** (the PVM state being evaluated).

```ocaml
(* Node_context.t — store and context permissions *)
type 'a t = { ...; store : 'store store; context : 'context Context.index; ... }
  constraint 'a = < store : 'store ; context : 'context >
  constraint 'store = [< `Read | `Write > `Read]
  constraint 'context = [< `Read | `Write > `Read]

(* Context.t — index and state permissions *)
type 'a Context.t
  constraint 'a = < index : [< `Read | `Write > `Read] ;
                     state : [< `Read | `Write > `Read] >

(* Context.index — the handle used to checkout/commit *)
type 'a Context.index   (* 'a = [`Read] or [`Read | `Write] *)
```

The **store** permission controls SQLite reads/writes. The **context** permission on `Node_context.t` propagates to the `Context.index` (which controls checkout/commit). The **state** permission on `Context.t` controls PVM state mutation.

### Concrete type aliases

```ocaml
(* Node_context aliases *)
type rw = < store : [`Read | `Write] ; context : [`Read | `Write] > t
type ro = < store : [`Read] ; context : [`Read] > t
type 'a rw_store = < store : [`Read | `Write] ; context : 'a > t
type 'a rw_context = < store : 'a ; context : [`Read | `Write] > t

(* Context aliases *)
type Context.rw = < index : [`Read | `Write] ; state : [`Read | `Write] > Context.t
type Context.ro = < index : [`Read] ; state : [`Read] > Context.t
type Context.rw_index = [`Read | `Write] Context.index
type Context.ro_index = [`Read] Context.index
```

### Rollup_node_daemon.state

The daemon's top-level state holds a fully read-write node context and a read-write reference to the current context state:

```ocaml
type state = {
  mutable plugin : (module Protocol_plugin_sig.S);
  rpc_server : Rpc_server.t;
  configuration : Configuration.t;
  node_ctxt : Node_context.rw;
  current_ctxt : Access_mode.rw Node_context.context_state option Reference.rw;
}
```

`Node_context.context_state` ties together a context (with mutable state) and its validity status:

```ocaml
type 'index context_state = {
  ctxt : < index : 'index ; state : Access_mode.rw > Context.t;
  status : context_status;  (* Valid | Dirty *)
  block : Layer1.header;
}
```

Note that `state` is always `Access_mode.rw` in `context_state` — PVM evaluation always mutates state in place. The `'index` parameter tracks whether the context index allows committing.

### Permission rules for function signatures

**Prefer polymorphic signatures.** `ro` should only be used to restrict values, fields, etc. in monomorphic settings (e.g. record fields, module types) to make explicit that the access will only be read-only. When writing functions that only do read access, always prefer the polymorphic version `_ Node_context.t` — this allows callers to pass either a read-only or read-write value without needing to downcast.

| Signature | Meaning | When to use |
|-----------|---------|-------------|
| `_ Node_context.t` | Polymorphic over both store and context | Functions that only read. **Preferred over `ro`.** |
| `Node_context.rw` | Read-write on both store and context | Functions that write to both |
| `_ Node_context.rw_store` | Write store, polymorphic context | Functions that write store but only read context (most write operations) |
| `_ Node_context.rw_context` | Write context, polymorphic store | Functions that operate on context index (GC, commit) but don't write store |
| `Node_context.ro` | Read-only on both | Monomorphic restriction only (record fields, module constraints) |

**Example — `checkout_context` reads store and index, returns mutable state:**

```ocaml
val checkout_context :
  < store : _ ; context : 'a > t ->
  Block_hash.t ->
  < index : 'a ; state : Access_mode.rw > Context.t tzresult Lwt.t
```

This function reads from the store (polymorphic `_`), propagates the context index permission `'a` to the returned `Context.t`, and always returns a mutable state (`Access_mode.rw`) because the checked-out PVM state is always modifiable.

**Example — `save_l2_block` writes store, polymorphic over context:**

```ocaml
val save_l2_block : _ rw_store -> Sc_rollup_block.t -> unit tzresult Lwt.t
```

This requires write access to the store (`rw_store`) but doesn't care about context permissions (`_`).

### Downcasting

- `Node_context.readonly : _ t -> ro` — restricts both store and context to read-only
- `Node_context.readonly_store` — restricts only store
- `Node_context.readonly_context` — restricts only context
- **It is not possible to upcast for obvious safety reasons.**

Key tracked state:
- **lcc** (Last Cemented Commitment) -- most recently cemented on L1
- **lpc** (Last Published Commitment) -- commitment the operator is staked on
- **current_protocol** -- active protocol hash, level, constants
- **context_state** -- current PVM state with `Valid`/`Dirty` status
- **degraded** -- flag for degraded mode (failed processing, still defends games)

## Reference.t -- Mutable values with phantom permissions

Used for `lcc`, `lpc`, `current_protocol`, `degraded`, `private_info`, `current_ctxt`.

**Anti-pattern**: Do not use mutable fields in `Node_context.t`, prefer a
`Reference.t` to maintain sharing and avoid creating new pointers on implicit
copying.

## Error handling

The codebase uses the Tezos error monad pervasively:

```ocaml
let* x = ...   (* bind tzresult Lwt.t *)
let*! x = ...  (* bind Lwt.t, lifting into tzresult *)
let*? x = ...  (* bind tzresult, lifting into Lwt *)
return x        (* Ok x |> Lwt.return *)
return_unit     (* Ok () |> Lwt.return *)
fail [error]    (* Error [error] |> Lwt.return *)
```

## Main daemon loop

**File: `rollup_node_daemon.ml`**

```
Layer1.iter_heads  ->  on_layer_1_head  ->  process_l1_block  ->  process_unseen_head
```

`process_unseen_head` for each L1 block:
1. Save protocol info, handle migrations
2. Fetch inbox from L1 (via protocol plugin `INBOX.process_head`)
3. Execute PVM on messages (`Interpreter.process_head`) -- **context is Dirty during this**
4. Create/store commitments (`Publisher.process_head`)
5. Register outbox messages
6. Save L2 block to store -- **context becomes Valid**

Separate `refutation_daemon` runs in background for Accuser/Operator modes.

On unrecoverable errors, node enters **degraded mode**: stops normal processing but continues defending refutation games.

### Context state lifecycle

During block evaluation:
1. Context checked out and set as `Dirty` (via `start_block_evaluation`)
2. PVM evaluated -- interpreter modifies state in place
3. Context committed and set as `Valid` (via `finish_block_evaluation`)

When RPCs arrive during step 2 (context is `Dirty`), they currently do not
wait. RPCs that need to access the PVM state always checkout.

## Components

| Component | File(s) | Role |
|-----------|---------|------|
| **Publisher** | `publisher.ml` | Computes commitments every `sc_rollup_commitment_period` blocks; publishes, cements, recovers bond |
| **Batcher** | `batcher.ml` | Accepts L2 messages via RPC, batches per config, sends to injector |
| **Interpreter** | `interpreter.ml` | Executes PVM on inbox messages, returns tick count |
| **Refutation coordinator** | `refutation_coordinator.ml` | Monitors L1 for conflicts, launches/terminates refutation players |
| **Refutation player** | `refutation_player.ml` | Worker that plays individual refutation games |
| **Refutation game logic** | `refutation_game.ml` | Game moves: dissection, proof production |
| **Injector** | `injector.ml` | Bridges to `lib_injector` for L1 operation injection |
| **RPC server** | `rpc_server.ml` | Exposes block, commitment, outbox, batcher RPCs |
| **Layer1** | `layer1.ml` | Monitors L1 heads, caches blocks, handles reorgs |
| **Store** | `store.ml`, `sql_store.ml` | SQLite-backed persistent storage |
| **Context wrapper** | `context_wrapper.ml` | Bidirectional PVM context conversions (Irmin/RISC-V dispatch) |

Components use the `tezos-workers` framework. Each worker has a `*_worker_types.ml` companion defining request/event types.

## Node modes

Defined in `configuration.ml`. Use `can_inject` to check what operation kinds a mode allows.

| Mode | Behavior |
|------|----------|
| **Observer** | Passive: follow L1, reconstruct state. No L1 operations |
| **Accuser** | Defensive: publish commitments only on conflict, play refutation |
| **Bailout** | Defend existing commitments, cement, no new commitments |
| **Batcher** | Accept L2 txs, batch and inject on L1. No commitments |
| **Maintenance** | Publish commitments every period, cement |
| **Operator** | Maintenance + Batcher combined |
| **Custom of Operation_kind.t list** | User-specified operation kinds |

Operators are specialized by purpose (`purpose.ml`): `Operating`, `Batching`, `Cementing`, `Recovering`, `Executing_outbox` -- each with its own key(s).

The `etherlink : bool` flag in configuration activates Etherlink-specific behavior throughout the node (batcher message handling, DAL injection, interpreter events).

## Protocol plugins

Located at `src/proto_*/lib_sc_rollup_node/`. Each protocol implements `Protocol_plugin_sig.S`:

- `RPC_DIRECTORY` -- protocol-specific RPC handlers
- `DAL_SLOTS_TRACKER` -- DAL attestation processing
- `INBOX` -- inbox reconstruction from L1 blocks, message serialization
- `LAYER1_HELPERS` -- L1 block metadata extraction
- `DAEMON_HELPERS` -- protocol migration, constants
- `REVEALS` -- reveal data page handling

Plus: `pvm.ml` (PVM abstraction), `sc_rollup_injector.ml` (injector protocol client), `rollup_node_plugin.ml` (registration entry point).

Signatures defined in `protocol_plugin_sig.ml` and `pvm_plugin_sig.ml`.

The active protocol plugin is passed as a first-class module `(module Plugin : Protocol_plugin_sig.S)` through the call chain. Do not try to store it globally -- it changes on protocol migration.

## PVM execution

| PVM | Speed | Use | Context backend |
|-----|-------|-----|-----------------|
| **Arith** | Fast | Tests only | Irmin (`lib_layer2_irmin_context`) |
| **WASM 2.0** | Slow (pure OCaml) for proofs, fast (Wasmer JIT) for eval | Production | Irmin (`lib_layer2_irmin_context`) |
| **RISC-V** | Rust bindings | Production | RISC-V (`lib_layer2_riscv_context`) |

PVM implementations are split between protocol plugins (rollup-node-side glue) and core libraries (execution engines):

| PVM | Protocol plugin glue | Core implementation |
|-----|---------------------|---------------------|
| **WASM 2.0** | `src/proto_*/lib_sc_rollup_node/wasm_2_0_0_pvm.ml` | `src/lib_scoru_wasm/` (pure OCaml interpreter), `src/lib_scoru_wasm/fast/` (Wasmer JIT) |
| **RISC-V** | `src/proto_*/lib_sc_rollup_node/riscv_pvm.ml` | `src/lib_riscv/pvm/` (OCaml bindings), Rust implementation underneath |
| **Arith** | `src/proto_*/lib_sc_rollup_node/arith_pvm.ml` | Inline in protocol (test-only PVM) |

The per-protocol `pvm.ml` dispatches to the correct PVM based on the rollup kind.

### Fuel system (`fuel.ml`)

Controls PVM execution budget:
- `Fuel.Free` -- unlimited execution (normal block processing)
- `Fuel.Accounted` -- counted ticks (refutation dissection, simulation)

## Store (SQLite)

`sql_store.ml` defines tables (version `V5_sqlite`). Migrations in `migrations/` directory.

| Table | Content |
|-------|---------|
| `L2_blocks` | L2 blocks indexed by hash and level |
| `Commitments` | Commitment data and hashes |
| `Commitments_published_at_levels` | Publication/inclusion tracking |
| `Inboxes` | Inbox hash to full inbox |
| `Messages` | Payload hash to message lists |
| `Outbox_messages` | Pending/executed outbox messages |
| `Protocols` | Protocol activation history |
| `DAL_slots` | DAL slot status |

History modes: `Archive` (all from genesis) vs `Full` (after LCC only).

## CRITICAL: PVM state is mutable -- NEVER copy naively

PVM state can be **gigabytes** (WASM durable storage, RISC-V memory). It is stored as a mutable reference (`Context.PVMState.value = mut_state`).

- Functions modify state in place during evaluation
- Use `PVMState.copy` / `imm_copy` / `mut_copy` for explicit cloning only when truly needed (e.g., caching immutable snapshots for dissection)
- `to_mut_eval_state` / `to_imm_eval_state` in `pvm_plugin_sig.ml` make copies -- use deliberately

## ANTI-PATTERN: Do NOT rely on injector feedback

The injector reports when operations are injected/included, but this can be stale after L1 reorgs. **Always observe L1 blocks directly** to determine actual chain state (e.g., whether a commitment was published, cemented, etc.). The publisher follows this pattern -- it checks L1 state, not injector callbacks. See also `lib_injector/CLAUDE.md`.

## OpenTelemetry tracing

The rollup node emits OpenTelemetry spans for key operations. Configuration lives in `configuration.ml` (`opentelemetry` field, type `Octez_telemetry.Opentelemetry_config.t`). Setup is done once at startup via `Octez_telemetry.Opentelemetry_setup.setup` in `rollup_node_daemon.ml`.

To instrument a function, wrap it with `Octez_telemetry.Trace.with_tzresult`:

```ocaml
Octez_telemetry.Trace.with_tzresult ~service_name:"Batcher" "inject_batch"
@@ fun scope ->
Opentelemetry.Scope.add_attrs scope (fun () ->
    [("key", `String value); ("level", `Int n)]) ;
...
```

Currently instrumented: `on_layer1_head`, `process_unseen_block`, `process_block` (daemon), `eval_block`, `eval_messages` (interpreter), `inject_batch`, `produce_batches`, `register_messages` (batcher), `publish_executable_messages` (outbox), and all RPC handlers (via `rpc_directory_helpers.ml`). SQLite queries also emit spans automatically via `lib_sqlite` (see `lib_sqlite/CLAUDE.md`).

For Etherlink, kernel-level tracing emits per-block spans (`eval_etherlink_block`) by parsing `kernel_debug` log lines in `node_context.ml`.

## Common tasks

### Adding a new RPC endpoint
1. Define the service (path, query params) in `rollup_node_services.ml` (in `lib_smart_rollup`)
2. Register the handler in `rpc_directory.ml`
3. For protocol-specific RPCs, add to the protocol plugin's `RPC_DIRECTORY`, and
   modify `RPC_directory.ml`.

### Adding a new SQLite table
1. Add the table definition and queries to `sql_store.ml`
2. Add migration SQL to `migrations/` (create a new numbered `.sql` file)
3. Bump the store version in `store_version.ml`

### Adding a new operation kind
1. Add the variant to `Operation_kind.t` in `lib_smart_rollup/operation_kind.ml`
2. Modify the protocol plugins to handle conversions.
3. Update `can_inject` in `configuration.ml` for relevant modes
4. Add injector tag handling in `injector.ml`

### Adding a new event
1. Add to the relevant `*_events.ml` file (e.g., `daemon_event.ml`, `batcher_events.ml`)
2. Declare with `Internal_event.Simple.declare_N ~section ~name ~msg ~level`
3. Section convention: `["smart_rollup_node"; "<component>"]`
4. Emit with `Simple.(emit event_name) args`

### Adding a Prometheus metric
1. Add to `metrics.ml` using the private `sc_rollup_node_registry`
2. Use `v_gauge`, `v_label_counter`, `v_labels_counter` helpers
3. Namespace is `Tezos_version.Octez_node_version.namespace`, subsystem is `"sc_rollup_node"`

### Adding a configuration field
1. Add the field to `type t` in `configuration.ml/mli`
2. Add a default value in the defaults
3. Add to the JSON encoding (must be backward-compatible -- use `dft` with a default for existing config files)
4. Optionally add a CLI flag in `cli.ml` and in `main_smart_rollup_node.ml`

### Modifying protocol plugins
1. Protocol plugins are not created, they are snapshoted from `proto_alpha`.
2. A change that impacts the protocol plugins should first be made in
   `proto_alpha`. Then the changes should be backported to the other protocol
   plugins.

## Related directories

| Directory | CLAUDE.md | Role |
|-----------|-----------|------|
| `src/lib_smart_rollup/` | [CLAUDE.md](../lib_smart_rollup/CLAUDE.md) | Protocol-agnostic shared types (commitments, inbox, operations, RPC service definitions) |
| `src/bin_smart_rollup_node/` | [CLAUDE.md](../bin_smart_rollup_node/CLAUDE.md) | CLI binary entry point, protocol plugin linking |
| `src/lib_layer2_store/` | [CLAUDE.md](../lib_layer2_store/CLAUDE.md) | Storage abstraction: context signatures, phantom-typed access modes, context dispatch |
| `src/lib_layer2_irmin_context/` | [CLAUDE.md](../lib_layer2_irmin_context/CLAUDE.md) | Irmin context backend (WASM/Arith PVMs) |
| `src/lib_layer2_riscv_context/` | [CLAUDE.md](../lib_layer2_riscv_context/CLAUDE.md) | RISC-V context backend (Rust bindings) |
| `src/lib_injector/` | [CLAUDE.md](../lib_injector/CLAUDE.md) | Generic L1 operation injection framework (functor, lifecycle, retry) |
| `src/lib_sqlite/` | [CLAUDE.md](../lib_sqlite/CLAUDE.md) | Type-safe SQLite wrapper (Caqti, pooling, transactions) |
| `src/proto_*/lib_sc_rollup_node/` | -- | Protocol plugins (snapshoted from `proto_alpha`) |

## Build

```
dune build src/lib_smart_rollup_node/
```

## Tests

Unit tests: `test/*.ml`

Integration tests: `tezt/tests/sc_rollup.ml`

## Changelog

User-facing changes go in `CHANGES.rst` at repo root, under the "Smart Rollup node" section.

## User documentation

The user-facing documentation for the smart rollup node is at `docs/shell/smart_rollup_node.rst`. It covers operation modes, configuration, snapshots, WASM kernel development, and more. Keep it up to date when changes affect user-facing behavior (new CLI flags, modes, configuration options, RPC endpoints, etc.).

## Keeping CLAUDE.md files up to date

When modifying the rollup node codebase, update the relevant CLAUDE.md if your change affects documented patterns, adds new components/files worth mentioning, changes module opens, alters common task workflows, or introduces new anti-patterns. This applies to all 8 CLAUDE.md files in related directories (see "Related directories" above).
