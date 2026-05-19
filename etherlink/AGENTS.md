# AGENTS.md — Etherlink / Tezos X best practices

Best practices to follow when writing or modifying code under `etherlink/`. These rules are distilled from recurring review points on past MRs. Each item below is a one-line reminder; for full rationale, examples, and citations, see [`MR_REVIEW_PATTERNS.md`](./MR_REVIEW_PATTERNS.md) — that companion document is intended for **MR review** work, not day-to-day development.

---

## 1. Convention

> Repo-specific idioms — match what the surrounding code already does instead of introducing a parallel way.

- **Reuse existing helpers** rather than open-coding conversions, encodings, RPC flows, or storage paths.
- **Use little-endian** for integer encodings on durable storage and the Rust↔OCaml boundary.
- **Avoid "Tezlink" in Tezos X code**: say "Michelson runtime" in paths, comments, errors, and changelogs.
- **Reword commits when scope shifts** and use the right component prefix (`TezosX/Kernel`, `Tezos X/EVM node`, `Tezlink/Tezt`).
- **OCaml monadic syntax**: use `Lwt_result_syntax` with `let*` / `let*?` / `let*!` and prefer `filter_e` / `filter_map_ep` over manual loops.

## 2. Style

> Code clarity and safety patterns — let the compiler catch the bug at the type level instead of leaving it for runtime.

- **Name constants and functions precisely**: replace magic numbers, and rename functions when their behavior changes.
- **Use the type system**: typed wrappers, enums over booleans/strings, and exhaustive matches with no catch-all `_`.
- **Kernel safety**: never `panic!`/`unwrap`/`expect` on non-static inputs, and never silently drop a `Result`.

## 3. Architecture

> Where code lives, when state changes, and how cross-cutting concerns (gas, flags, sentinels, module placement) are structured.

- **Don't mutate state after the fact**: apply changes inside the journaled unit of execution, not post-hoc.
- **Charge gas upfront and symmetrically across runtimes**, and forward `gas.remaining()` (never `inputs.gas_limit`).
- **Place code in the right module/crate** (generic chain logic, cross-runtime types, wei↔mutez, cross-runtime tezt scenarios, IPC paths).
- **Don't mix runtimes across sub-crates**: kernel sub-crates dedicated to Tezos/Michelson must contain no EVM-specific code, and EVM sub-crates must contain no Tezos/Michelson-specific code.
- **Centralize error messages and conversions** instead of duplicating them per runtime.
- **Use feature flags / version gates** for version-dependent behavior, and read the flag once instead of repeatedly hitting durable storage.
- **Use the typed `Durable_storage` GADT**, not raw paths with `Z.of_bits`.
- **Avoid mutable sentinels** for transient values; pass the value or return a tuple instead.

## 4. Testing

> Tests that actually exercise the change, fail when the fix is reverted, and don't flake.

- **Assert exact values** (not `is_some`/length), make tests fail when the fix is reverted, and read state from the chain rather than mirrored test variables.
- **Add a failure / negative-case test** for every successful-path test (revert, OOG, invalid signature, NACK, backtracked CRAC).
- **Place tests in the right file** (`tezlink.ml`, `cross_runtime.ml`, `tezosx.ml`) with the matching registration helper.
- **Avoid flakiness**: set up watchers before triggering events, fix root causes (not sleeps), and remember that `Rpc.produce_block` returns the operation count — use `produce_block_and_wait_for` for levels.
- **Add roundtrip / property tests** for new encoders/decoders, especially across Rust↔OCaml.
- **Use the regression framework** for structured-output assertions, and regenerate `failed_migration.wasm` when storage version bumps.
- **Regenerate Michelson script regression traces** when adding or modifying a Michelson script used by tests — re-run the affected test with `--reset-regressions` and commit the updated traces alongside the script.

## 5. Domain pitfalls

> Etherlink-specific traps where the wrong default silently produces a correctness bug — gas units, wei↔mutez precision, HTTP error scope, migrations.

- **Distinguish EVM gas, Michelson gas, milligas, and the multiplier**, and read `michelson_to_evm_gas_multiplier` from storage rather than hardcoding.
- **Wei ↔ mutez is floor division** (`wei / 10^12`); centralize helpers and use explicit `Wei` / `Mutez` types.
- **Map error scope to HTTP status correctly**: 4xx is catchable revert, 5xx aborts the blueprint — storage/infra failures must be 5xx.
- **Migration discipline**: hardcode path bytes (no live constants), no dual-writes, handle structures embedding their own path, and regenerate `failed_migration.wasm`.

## 6. MR process & scoping

> What an MR must contain — and what must already pass — before it goes to review.

- **One concern per commit**: separate migrations, refactors, and feature work into their own commits.
- **Every TODO references a Linear issue** (`L2-XXXX`); no bare TODOs.
- **Update `CHANGES_TEZOSX.md` / `CHANGES_KERNEL.md`** for user-facing changes, with no Tezlink terminology in Tezos X.
- **After rebasing, recheck the changelog**: new release sections may have landed on `master`; make sure your entries are still in the *Unreleased* section, not stranded above a freshly added release header.
- **Update the RFC / spec** when behavior diverges (or fix the code if the RFC is right).
- **Formatting must pass**: run `make -f etherlink.mk check` from the repo root before submitting.

## 7. Rust SDK / kernel-trait hygiene

> Patterns specific to the kernel SDK and the ongoing split from a monolithic `Runtime` trait into capability-typed traits.

- **No unwarranted `unsafe`**: put `unsafe` on the function, not the trait, and minimize the trust boundary.
- **Prefer `where` clauses** for verbose bounds and drop redundant `SdkRuntime` when narrower traits suffice.
- **Don't widen `&impl X` to `&mut impl X`** unless the function actually mutates.
- **Use `HostRuntime::abort`** instead of hand-rolled panics.
- **Prefer `use` imports over inline paths**: add `use some_crate::some_module::some_item` at the top of the file rather than spelling out `some_crate::some_module::some_item(...)` at the call site. Applies to functions, variants, traits, and types.
- **Runtime-only flags don't belong in `/evm/world_state/`**; put them under `/evm/feature_flags/` or `/base/feature_flags/`.
- **Always emit `X-Tezos-*` response headers on every code path**, including errors.
- **Kernel entry points** must distinguish transient (abort the block) from permanent errors; never silently swallow with `?`.
- **Use structured error enums**, not stringly-typed HTTP bodies.

---

## Companion document

[`MR_REVIEW_PATTERNS.md`](./MR_REVIEW_PATTERNS.md) contains the full version of every rule above — severity, frequency, code examples, and MR citations. **Use it when reviewing MRs**, not when developing: keeping the review checklist out of the development context avoids overweighting style nits over the task at hand.
