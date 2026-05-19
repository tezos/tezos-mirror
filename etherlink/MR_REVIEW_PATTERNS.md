# Etherlink / Tezos X — MR review patterns

> **Audience:** this document is intended for **MR review** of code under `etherlink/`, not for day-to-day development. The short-form checklist for development lives in [`AGENTS.md`](./AGENTS.md).

Compiled from 264 GitLab MRs and 2,702 human review notes on the Tezos X multi-runtime kernel initiative. Each pattern carries a severity (`critical` / `important` / `nit`), a kind (style, architecture, testing, domain, process, safety), and an approximate frequency in the corpus.

---

## 1. Style & convention

### 1.1 Reuse existing helpers
**Severity:** important | **Frequency:** ~45+

Do not open-code conversions, encodings, or RPC flows. The codebase has centralized helpers for timestamp parsing, wei↔mutez conversion, RLP encoding, LE integer operations, and storage paths. Rolling one-off implementations introduces bugs (endianness, precision loss, off-by-one) that helpers have already solved. Reviewers recognize reimplementations immediately.

**Key helpers to check before coding:**
- `Ethereum_types.timestamp_of_bytes` (timestamp parsing)
- `wei::wei_from_mutez` / `mutez_from_wei` (cross-precision conversion)
- `rlp_helpers::append_list` / `decode_list` (RLP encoding)
- `read_u256_le` / `append_u64_le` (LE integer encoding)
- `Tez` module (OCaml-side tez operations)
- `Durable_storage_path.sequencer_key` (storage path constants)

### 1.2 Little-endian integer encoding
**Severity:** critical | **Kind:** convention + domain | **Frequency:** ~15

The Etherlink kernel uses **little-endian** for integer encodings on durable storage and the Rust↔OCaml boundary. Mismatches between encode/decode directions cause silent value corruption on write-then-read cycles. Always use project-specific `*_le` helpers. Asymmetries (e.g., encoder using big-endian while decoder expects LE) are round-trip corruption bugs.

### 1.3 Name constants and functions precisely
**Severity:** nit | **Kind:** style | **Frequency:** ~36

Two disciplines:

1. Replace magic numbers (`1_000_000`, `21_000`, `25_250`, `100`, `260`) with named constants, including in tests.
2. When a function's behavior changes, **rename it** — don't keep names that predate current semantics. Use `DEFAULT_` prefix when a constant becomes a fallback. Use `*_flag` suffix when a flag parameter is added.

### 1.4 Avoid "Tezlink" in Tezos X code
**Severity:** important | **Kind:** convention + domain | **Frequency:** ~18

Tezlink is a standalone Michelson-only chain (preview product); Tezos X is the multi-runtime chain (future Etherlink). In Tezos X paths, comments, error messages, and changelogs, use **"Michelson runtime"** instead of "Tezlink".

- Path naming: `TEZOSX_TEZOS_BLOCKS_PATH`, not `TEZLINK_BLOCKS_PATH`.
- Test registration: `register_tezosx_test`, not `register_tezlink_only_test`.

### 1.5 Use the type system: typed wrappers and exhaustive matches
**Severity:** important | **Kind:** style + architecture + safety | **Frequency:** ~32

Three disciplines:

1. Use enum/typed wrappers, not stringly-typed comparisons or magic integers. Replace `Vec<u8>` with `String` when the value must be ASCII. Introduce a struct rather than returning anonymous tuples. Two booleans → one enum.
2. Don't introduce per-runtime branching in runtime-agnostic interfaces; use headers or polymorphic arguments instead.
3. Prefer `match` exhaustiveness on enums; avoid catch-all `_` patterns so the compiler flags new variants.

### 1.6 Reword commits when scope shifts
**Severity:** important | **Kind:** process + convention | **Frequency:** ~18

During review, if a commit's paths or scope change, reword the title to reflect the new content. Be precise (e.g., "Track" vs "Propagate"). Prefix with the right component (`TezosX/Kernel`, `Tezos X/EVM node`, `Tezlink/Tezt`).

### 1.7 Kernel safety: never panic, never silently swallow errors
**Severity:** critical | **Kind:** safety | **Frequency:** ~33

The kernel must propagate errors faithfully.

1. Avoid `.expect`, `.unwrap`, `panic!`, `unreachable!` on inputs that are not statically guaranteed valid. Replace with explicit error paths or safe fallbacks (`saturating_sub`, `checked_*`, `Option::unwrap_or`).
   - `unwrap` on a constant whose decode is verified by tests is occasionally acceptable, but the bar is high.
   - `unimplemented!` in trait impls forced by external library design is accepted only with documented invariant audits.
   - `unreachable!` is acceptable for genuinely unreachable code paths (safe Rust); reachable via runtime-error mapping must be a real error variant.
2. Don't catch `Stack_overflow` or `Out_of_memory`; don't silently drop `Result` (`let _ = ...`) where failure is meaningful.

### 1.8 OCaml monadic syntax
**Severity:** nit | **Kind:** convention | **Frequency:** ~11

Use `Lwt_result_syntax` with `let*` / `let*?` / `let*!`. Prefer `filter_e` / `filter_map_ep` over manual loops. Prefer one-line helpers over hand-rolled `store_has` / `store_read_slice` chains.

---

## 2. Architecture

### 2.1 Don't mutate state after the fact; backtrack properly
**Severity:** critical | **Kind:** architecture + domain + safety | **Frequency:** ~22

Mutating receipts/balances after a `safe_host.promote/revert` is forbidden. Apply state changes inside the unit of execution that owns them; the journal — not post-hoc fix-up — is how reverts work.

When journal plumbing isn't viable (e.g., gas refund post-application), accept post-application mutation **only with**:

1. A Linear follow-up filed.
2. An explicit comment listing affected edge cases (self-transfers, simulation, preapplication).
3. Use of `add_balance` (the safe abstraction) rather than direct field writes.

The `with_temporary_credit` pattern (early credit, manual revert on EVM revert) is fragile because EVM `journal.revert` operates on a snapshot taken before the credit; push this into the journal proper.

### 2.2 Charge gas upfront; pre-charge symmetric across runtimes
**Severity:** critical | **Kind:** safety + domain | **Frequency:** ~22

Gas must be checked and charged **before** performing the work it pays for. Otherwise:

1. The kernel performs work for free (DOS vector).
2. A failure-after-write leaves persistent state (e.g., an alias) without gas consumption.
3. Same-runtime calls double-spend if `gas_limit` is forwarded instead of `gas.remaining()`.

**Charging order for gateway dispatch:**

1. Base cost.
2. Header validation (for `call()`).
3. Alias resolution per alias — first `ALIAS_CACHE_HIT_COST`, then on cache miss the cost reported by target runtime.
4. Value transfer surcharge if `msg.value > 0`.
5. Forward `gas.remaining()` as target runtime's budget — **NEVER** `inputs.gas_limit`.

**Symmetry across runtimes:** when charging gas for cross-runtime work (alias resolution, header validation, response parsing), both source and target runtimes pre-charge consistently, and validation cost fires before the I/O it gates. Watch for `header_cost` charged twice or after the work.

### 2.3 Place code in the right module/crate
**Severity:** important | **Kind:** architecture | **Frequency:** ~20

| Concern | Belongs in |
|---|---|
| Generic chain logic | `tezos_execution/src/lib.rs` or MIR `src/ast.rs`, not `chains.rs` |
| Cross-runtime types | `etherlink/kernel_latest/tezosx-interfaces/src/` |
| Wei↔mutez conversions | `etherlink/kernel_latest/ethereum/src/wei.rs` |
| Cross-runtime test scenarios | `etherlink/tezt/tests/cross_runtime.ml` |
| Node-kernel IPC paths | `/base/` storage prefix |

### 2.3bis Don't mix runtimes across sub-crates
**Severity:** important | **Kind:** architecture

Kernel sub-crates that are dedicated to one runtime must stay self-contained:

- Tezos/Michelson-specific sub-crates (`tezos_execution`, MIR, Michelson-side helpers) must contain **no EVM-specific code** — no REVM imports, no wei/Ethereum-address types, no EVM gas units, no `evm_*` paths.
- EVM-specific sub-crates (`ethereum`, REVM glue, EVM-side helpers) must contain **no Tezos/Michelson-specific code** — no MIR imports, no mutez/KT1 types, no Michelson gas units, no `michelson_*` paths.

Cross-runtime types and conversions live in shared crates such as `etherlink/kernel_latest/tezosx-interfaces/`. If you need both runtimes in the same module, the module belongs in a cross-runtime crate, not in a runtime-specific one. This rule keeps the runtime split clean for the multi-runtime kernel and avoids accidental coupling that breaks single-runtime builds and bleeds gas/encoding conventions across sides.

### 2.4 Centralize string error messages and conversions
**Severity:** important | **Kind:** architecture | **Frequency:** ~13

Don't let each runtime have its own variation of the same error string. Don't duplicate constants/conversions between Ethereum and Michelson runtimes. Avoid `String::from_utf8_lossy` on alias bytes (silently corrupts them).

### 2.5 Use feature flags and version gates instead of permanent escape hatches
**Severity:** important | **Kind:** architecture + domain | **Frequency:** ~20

Behavior dependent on storage version, kernel version, or experimental features must be gated explicitly. **Read the flag once and thread it; don't read durable storage repeatedly inside hot paths.**

**Flag locations:**

- `/base/feature_flags/` — current convention.
- `/evm/world_state/...` — discouraged for runtime-only flags.
- `enable_michelson_runtime` in `chain_config` — for sunrise activation.
- `EXPERIMENTAL_FEATURES` — for gas refund and experimental code paths.
- `LEGACY_EVM_NODE_FLAG` — dual-read fallback after V51 path move.

### 2.6 Use typed `Durable_storage` GADT, not raw paths
**Severity:** important | **Kind:** architecture + convention | **Frequency:** ~16

Routes to durable storage should use the typed/GADT API. Manual `Raw_path` + `Z.of_bits` is a regression. Distinguish directory paths from value paths via the type system. Use `unit path` for existence flags, not `bool path`. The migration to capability-typed paths (`[Read | Write | Tree]`) is in progress.

### 2.7 Avoid mutable sentinels for transient values
**Severity:** important | **Kind:** architecture | **Frequency:** ~10

A mutable global counter or reset-on-error sentinel is suspicious. Prefer passing the value or returning a tuple. When mutable state is read at the end of the happy path, **error paths must do the read explicitly** or use a different sentinel. Sentinels should be visible in final receipts as a bug, not as full gas consumption.

---

## 3. Testing

### 3.1 Test rigor: assert exact values, fail when fix is reverted, read state from chain
**Severity:** important / critical | **Kind:** testing | **Frequency:** ~57

Three sub-points separate a real test from one that "passes" without exercising the change:

1. **Assert actual values, not presence/length.** `is_some`, `> 0`, length checks miss regressions. Assert balances, hashes, nonces, sources, destinations, log contents, error variants, runtime IDs, crac-id values, and receipt-nonce ordering.
2. **Tests must fail if the fix is reverted.** A regression test should fail if the fix is reverted; if the test is green with the patch reverted, it's not exercising the right thing.
3. **Specific assertion patterns:**
   - OOG must consume `≥ 0.95 * gas_limit` (distinguishes OOG from cheap-error paths).
   - Dynamic-delta tests assert `delta_late > 10 * delta_early`, never just `delta > 0`.
   - Read state from durable storage rather than mutable test variables — mirroring chain state in a `ref` drifts.

### 3.2 Add tests for the failure / negative case
**Severity:** important | **Kind:** testing + domain | **Frequency:** ~20

Every successful-path test should have a corresponding failure test: revert, out-of-gas, invalid signature, missing input, NACK. CRACs in particular need backtracked-receipt tests.

### 3.3 Place tests in the right file; use the right registration helper
**Severity:** important | **Kind:** testing + convention | **Frequency:** ~22

| Test concern | File | Registration helper |
|---|---|---|
| Tezlink-only (standalone Michelson chain) | `etherlink/tezt/tests/tezlink.ml` | `register_tezlink_test` |
| Cross-runtime CRAC | `etherlink/tezt/tests/cross_runtime.ml` | `register_tezosx_test` |
| Tezos X (multi-runtime) | `etherlink/tezt/tests/tezosx.ml` | `register_tezosx_test` |

Don't tag a test "Tezlink" if it only exercises the Michelson runtime of Tezos X — register as Tezos X. Unify on a single helper exposing `kernels` (plural).

### 3.4 Test flakiness: race conditions, sleeps, and `produce_block` return-value misuse
**Severity:** important / critical | **Kind:** testing + domain | **Frequency:** ~17

Three flake sources:

1. **Race conditions / sleep-driven flakiness.** A test relying on timing or `Lwt_unix.sleep` has flake risk. Set up the watcher _before_ triggering the event using `and*` syntax.
2. **Storage-cache-miss flakiness is its own category.** The fix is upstream caching, not a sleep retry. Marking flaky should be a last resort tied to a Linear issue.
3. **`Rpc.produce_block` returns the operation count, not the block level.** Use `produce_block_and_wait_for` to wait for a target level. This footgun has been caught in 3+ MRs.

### 3.5 Add roundtrip / property tests for encodings
**Severity:** important | **Kind:** testing | **Frequency:** ~11

New encoders/decoders need a roundtrip test. For RLP / data-encoding pairs, write a generic `encode |> decode = id` helper and feed it diverse inputs. This is especially relevant for cross-language (Rust↔OCaml) encodings where LE/BE asymmetry caused real bugs.

### 3.6 Use regression tests for output-shape verification
**Severity:** important | **Kind:** testing + convention | **Frequency:** ~9

When a test asserts a structured output, use the regression framework rather than pinning fragile partial checks. Regenerate regression files when behavior changes. Regenerate `failed_migration.wasm` when storage version bumps.

### 3.7 Regenerate Michelson script regression traces
**Severity:** important | **Kind:** testing + convention

When adding a new Michelson script (or modifying an existing one) that is exercised by a regression test, the recorded trace must be regenerated and committed alongside the script. A stale or missing trace either fails CI immediately or — worse — passes locally while pinning the wrong behaviour.

Workflow:

1. Add or update the Michelson script and its consumer test.
2. Re-run the affected Tezt test with `--reset-regressions`, e.g.:

   ```bash
   dune exec etherlink/tezt/tests/main.exe -- --file cross_runtime.ml --title <test-title> --reset-regressions
   ```

3. `git add` the updated regression outputs together with the script in the same commit, so the change is self-contained and reviewable.

Do not regenerate traces without inspecting the diff: a regression update that silently changes a gas figure or a receipt field can mask a real behavioural change.

---

## 4. Domain pitfalls

### 4.1 Distinguish EVM gas, Michelson gas, milligas, and the multiplier
**Severity:** critical | **Kind:** domain | **Frequency:** ~22

Gas units must be carefully tracked:

| Unit | Used by | Notes |
|---|---|---|
| EVM gas | REVM (`u64`) | 1 EVM gas = 1000 milligas |
| Michelson gas | MIR (`u32` milligas) | Decode-per-byte gas already counted on origin runtime |
| `michelson_to_evm_gas_multiplier` | Gateway | Read from durable storage; never hardcoded |
| `MANAGER_OPERATION_MILLIGAS` | Base operation cost | Already counted on origin runtime |

Comments must clarify units. `michelson_gas_used / 1000` after wei→nanotez conversion, not before. The conversion direction `convert_gas(own_id, RuntimeId::Tezos, ...)` is asymmetric.

### 4.2 Wei ↔ mutez conversion: floor division, precision loss
**Severity:** important | **Kind:** domain | **Frequency:** ~12

`wei → mutez` is `wei / 10^12` (floor). Sub-mutez precision is lost. Centralize helpers in `etherlink/kernel_latest/ethereum/src/wei.rs`. Use explicit `Wei` / `Mutez` types where possible.

### 4.4 Error scope: 4xx catchable revert vs 5xx block abort, encoded as HTTP status
**Severity:** critical | **Kind:** domain + safety | **Frequency:** ~19

Some failures should be catchable inside Solidity (EVM revert → 4xx); others should abort the full blueprint (5xx / `BlockAbort`). **Storage / infrastructure failures must NOT be silently catchable.**

| Code | Meaning | Solidity behavior |
|---|---|---|
| 400 BAD_REQUEST | Catchable client error (bad signature, malformed payload) | Revert (catchable via `try/catch`) |
| 402 PAYMENT_REQUIRED / 429 TOO_MANY_REQUEST | OOG | Revert |
| 405 METHOD_NOT_ALLOWED | Wrong HTTP method | Revert |
| 5xx | Storage read inconsistency, balance accounting failure, gateway-injected header malformed | Cannot be caught — aborts full blueprint |

A 4xx for a storage-consistency failure means a Solidity `try/catch` swallows what should abort the block.

### 4.9 Migration discipline: paths, dual-writes, embedded paths, frozen constants
**Severity:** critical | **Kind:** domain | **Frequency:** ~32

Four sub-disciplines for migration code:

1. **Path moves use bytestring literals, not live constants.** Migrations using `pub const` constants silently break when a later migration rebinds the constant. Either hardcode the path bytes in the migration, or add a snapshot test that fails when a constant's resolved path changes.
2. **No dual-writes during migrations.** When a path moves, dual-writing both old and new (with later cleanup) creates stale-read bugs. Instead read storage version once and write to the canonical location.
3. **`store_move` of structures embedding their own path needs care.** Linked lists, blueprints, and other structures may serialize the path they live at; relocating the parent path requires updating the embedded path or making the runtime treat it as stale. Wrap with `allow_path_not_found` where relevant.
4. **Regenerate `failed_migration.wasm` when storage version bumps.** When storage version bumps, regenerate `failed_migration.wasm` (used in upgrade tests). New paths under `/base/` should have a fallback read at the legacy path until the kernel upgrade has rolled out, then remove the fallback in a tracked follow-up MR.

---

## 5. MR process & scoping

### 5.1 Split MRs by concern; don't mix refactors with features
**Severity:** important | **Kind:** process | **Frequency:** ~20

A single MR should focus on one concern. Migration code goes in its own commit. Refactors that "happen to" change behavior should be separated from feature work. New protocol versions (e.g., T024) need their own commit. Small, atomic commits should each compile and pass tests with no "peneloping" (undoing and redoing work).

### 5.2 TODOs must reference a Linear issue
**Severity:** important | **Kind:** process + convention | **Frequency:** ~50+

When a known limitation is left, file a Linear issue (`L2-XXXX`) and reference it in a TODO comment:

```
// TODO: L2-882 design CRAC receipts
```

Don't leave bare TODOs. Reviewers will file the issue themselves and ask the author to reference it.

### 5.3 Update `CHANGES_TEZOSX.md` / `CHANGES_KERNEL.md` for user-facing changes
**Severity:** important | **Kind:** process + convention | **Frequency:** ~15

Each user-facing change needs a changelog entry with the MR number and a concise user-impact-focused description. Tezlink terminology is forbidden in the Tezos X changelog. Entries go in `etherlink/CHANGES_TEZOSX.md` or `etherlink/CHANGES_KERNEL.md`. Sections: Internals, Native atomic composability, EVM Runtime, Michelson runtime.

After a rebase, **recheck where your changelog lines landed.** Releases are cut on `master` between the time an MR is opened and the time it merges, so a new "Version X.Y" header may now sit above the lines you added. The rebase will not move them down for you, and the result is an entry that documents a release it was never part of. Mechanical check before pushing the rebased branch:

1. Open `etherlink/CHANGES_TEZOSX.md` / `etherlink/CHANGES_KERNEL.md`.
2. Confirm the first header above your new lines is the *Unreleased* section, not a versioned release.
3. If a release section was inserted above your entry, move the entry back under *Unreleased*.

### 5.4 Update the RFC / spec when behavior diverges
**Severity:** important | **Kind:** process + domain | **Frequency:** ~15

When code diverges from the RFC (different event source, different runtime ID convention, callback handling), update the RFC alongside; or — if the RFC is correct — fix the code. Common RFCs: `RFC-Gateway-Protocol`, `RFC-CRAC-Derived-Block-Contents`.

### 5.5 Formatting must pass before submitting
**Severity:** important | **Kind:** process

Run the etherlink check target from the repo root before submitting an MR:

```bash
make -f etherlink.mk check
```

This target builds the dev dependencies and runs the formatting/lint checks specific to the etherlink subtree. CI runs the same target — failing it locally guarantees a red pipeline. Fix any reported diffs (formatting, semgrep rules, etc.) before pushing.

---

## 6. Rust SDK / kernel-trait hygiene

### 6.1 Narrow Rust style rules from the kernel-trait split
**Severity:** important | **Kind:** style + architecture + safety + performance | **Frequency:** ~13

Five narrow rules:

1. **No unwarranted `unsafe`.** `unsafe` should be on the function, not the trait. Move FFI calls inside the function body and minimize the trust boundary.
2. **Prefer `where` clauses for verbose bounds.** When bounds get verbose (`StorageV1 + HostDebug + Logging + WithGas`), promote to `where`. Drop redundant `SdkRuntime` bound when narrower traits suffice.
3. **Don't widen `&impl X` to `&mut impl X`** unless it actually mutates. Mechanical refactors sometimes promote read-only handles to `&mut` unnecessarily.
4. **Use `HostRuntime::abort`** instead of hand-rolled panics. The kernel provides an abstraction; use it.
5. **Prefer `use` imports over inline path notation.** When a new function, variant, trait, or type is referenced from another crate or module, add a `use` declaration at the top of the file rather than fully qualifying the call site:

   ```rust
   // BAD - inline path repeated at every call site.
   fn build(req: some_crate::some_module::Request) -> some_crate::some_module::Response {
       some_crate::some_module::handle(req)
   }
   ```

   ```rust
   // GOOD - one `use` at the top; call sites stay readable.
   use some_crate::some_module::{Request, Response, handle};

   fn build(req: Request) -> Response {
       handle(req)
   }
   ```

   Inline paths are appropriate only when (a) the symbol is used exactly once and the fully-qualified form is genuinely clearer (e.g., disambiguating between two identically-named items), or (b) bringing it into scope would shadow an existing import.

### 6.4 Runtime-only flags don't belong in `/evm/world_state`
**Severity:** important | **Kind:** architecture + domain | **Frequency:** ~8

Feature flags that control kernel execution flow (like gas-refund debug mode) should live in `/evm/feature_flags/` or `/base/feature_flags/`, **not** under `/evm/world_state/`. The `world_state` tree is interpreter-only state and must be stable across kernel versions.

References:

- MR [21713](https://gitlab.com/tezos/tezos/-/merge_requests/21713): "I would move `gas_refund_enabled` out of `world_state` and into a feature flag path, since it's not actually a piece of world state that can be read/written by contracts."
- MR [21159](https://gitlab.com/tezos/tezos/-/merge_requests/21159): "The problem is that this flag lives in `world_state`, which is only persisted/snapshotted for EVM execution. During non-EVM code (e.g. Michelson runtime setup) we lose the flag."

```rust
// BAD - debug flag mixed with EVM world state; bleeds state across kernel versions.
fn read_gas_refund_enabled(host: &Host) -> Result<bool, Error> {
    host.store_read_all(&path("/evm/world_state/gas_refund_enabled"))
        .map(|b| !b.is_empty())
}
```

```rust
// GOOD - flag lives in a dedicated feature-flags tree.
fn read_gas_refund_enabled(host: &Host) -> Result<bool, Error> {
    host.store_read_all(&path("/evm/feature_flags/gas_refund_debug"))
        .map(|b| !b.is_empty())
}
```

### 6.5 Always emit `X-Tezos-*` response headers on every code path
**Severity:** critical | **Kind:** domain + safety | **Frequency:** ~8

Response headers (`X-Tezos-Gas-Consumed`, `X-Tezos-Source`, etc.) must appear on **every** response, including error paths. Omitting them on failures causes the caller to see incomplete accounting or missing routing context.

References:

- MR [21546](https://gitlab.com/tezos/tezos/-/merge_requests/21546): "The headers should be added to **all** responses (success and error alike), so that the caller always has the context it needs."
- MR [21003](https://gitlab.com/tezos/tezos/-/merge_requests/21003): "We're creating a response without the headers on the error path; that breaks downstream accounting."

```rust
// BAD - headers only on the happy path.
match execute_call(req) {
    Ok(result) => {
        let mut resp = response_builder()
            .header("X-Tezos-Gas-Consumed", gas.spent())
            .body(result)?;
        Ok(resp)
    }
    Err(e) => {
        // Missing headers; caller has no gas consumption record.
        Err(e)
    }
}
```

```rust
// GOOD - headers on all paths; error responses still carry accounting context.
let gas_spent = gas.spent();
match execute_call(req) {
    Ok(result) => {
        response_builder()
            .status(StatusCode::OK)
            .header("X-Tezos-Gas-Consumed", gas_spent)
            .body(result)
    }
    Err(e) => {
        response_builder()
            .status(e.status_code())
            .header("X-Tezos-Gas-Consumed", gas_spent) // always emitted
            .body(e.to_string())
    }
}
```

### 6.6 Kernel entry points: explicit error propagation, no early returns on soft failures
**Severity:** important | **Kind:** safety + domain | **Frequency:** ~5 | **Scope:** `etherlink/kernel_latest/kernel/src/lib.rs`

Kernel entry points (`kernel_run`, `install`) may encounter retriable errors (transient host failure) vs permanent errors (bad input). Retriable errors should cause the **full block to revert** (not just the operation). Distinguish the two explicitly; don't silently swallow transient errors with early `return` or `?`.

Reference:

- MR [21546](https://gitlab.com/tezos/tezos/-/merge_requests/21546): "If the kernel encounters a transient error (e.g., a host RPC fails mid-block), we should abort the block, not pretend the op succeeded."

### 6.8 Use explicit error types over stringly-typed HTTP bodies
**Severity:** important | **Kind:** style + safety | **Frequency:** ~7

HTTP error responses that embed error messages as plain strings are hard to test and parse downstream. Define an `enum ErrorResponse` with variants for each error class, serialize to JSON, and let the HTTP layer map to status codes.

Reference:

- MR [21546](https://gitlab.com/tezos/tezos/-/merge_requests/21546): "Returning a raw string as the body makes it impossible to distinguish failure modes programmatically; use a structured error type instead."

---

## Summary

This selection of review patterns emphasizes **correctness through type safety**, **explicit gas accounting**, **careful error scope mapping (4xx vs 5xx)**, **centralized helpers** over reimplementation, **journaled state changes** over post-hoc mutation, and **rigorous testing** with exact assertions and negative cases. The highest-impact rules address gas unit confusion, LE/BE asymmetry, and error categorization — all of which have caused production bugs.

Source: gist [`lthms/11956e2320436c71311b4c5cc79e58b5`](https://gist.github.com/lthms/11956e2320436c71311b4c5cc79e58b5) (subset; some sections from the original gist were intentionally dropped to match `AGENTS.md`).
