# AGENTS.md for Octez (Tezos)

Comprehensive guidelines for AI agents and contributors working on the Octez repository.

## Project Overview

Octez is an OCaml implementation of the Tezos blockchain protocol. It includes the node software, client tools, baker and accuser daemons, smart rollup infrastructure, and related tooling. The codebase is primarily OCaml with Rust components.

### Repository Layout

```
src/                      # Main source code
  bin_*/                  # Binary executables (node, client, baker, etc.)
  lib_*/                  # Libraries
  proto_*/                # Protocol implementations (frozen and alpha)
  kernel_*/               # WASM kernels
  riscv/                  # RISC-V support
  rust_deps/              # Rust dependencies
tezt/                     # Tezt integration test framework
  tests/                  # Test implementations
  lib_tezos/              # Tezt-Tezos bindings
docs/                     # Documentation (Sphinx/RST)
etherlink/                # Etherlink (EVM rollup) components
  kernel_*/               # EVM kernels
contrib/                  # Additional tools
devtools/                 # Developer tools
scripts/                  # Build and CI scripts
opam/                     # OCaml package definitions
manifest/                 # Package manifest generation
client-libs/              # Client libraries
vendors/                  # Vendored dependencies
```

### Monorepo Structure

This repository is a **monorepo** containing multiple semi-independent components alongside the core Octez codebase. Each component lives in its own top-level directory and may have its own documentation, CI configuration, and test suites.

#### Product Components

| Component | Description |
|-----------|-------------|
| `etherlink/` | EVM-compatible rollup (Etherlink) — kernels, nodes, and tooling |
| `teztale/` | Block propagation monitoring and analytics |
| `grafazos/` | Grafana dashboard definitions for Tezos infrastructure |

#### Vendored/Forked Libraries

| Component | Description |
|-----------|-------------|
| `data-encoding/` | Binary and JSON serialization library |
| `brassaia/` | Storage library (Irmin fork optimized for Tezos) |
| `resto/` | Type-safe REST/RPC framework |
| `irmin/` | Key-value store library |

#### Standard Component Subdirectories

Components follow consistent structural conventions:

| Subdirectory | Purpose |
|--------------|---------|
| `bin_*/` | Binary executables |
| `lib_*/` | Libraries |
| `src/` | Source code (alternative to `lib_*/`) |
| `docs/` or `doc/` | Component-specific documentation |
| `ci/` | CI pipeline configurations |
| `test/` or `tezt/` | Test suites |
| `scripts/` | Component-specific scripts |
| `contrib/` | Additional tools and utilities |

When working on a specific component, check for a `README.md` in the component's root directory for component-specific guidelines and build instructions.

### Key Executables

- `octez-node` — The blockchain node
- `octez-client` — CLI client for interacting with the node
- `octez-baker-*` — Baker daemons (per protocol version)
- `octez-accuser-*` — Accuser daemons (per protocol version)
- `octez-smart-rollup-node` — Smart rollup node
- `octez-dal-node` — Data availability layer node
- `octez-evm-node` — EVM node (Etherlink)
- `octez-codec` — Binary encoding codec
- `octez-snoop` — Benchmarking tool
- `octez-protocol-compiler` — Protocol compiler

---

## Critical: Auto-Generated Files

Many `dune`, `.opam`, `dune-project`, `dune-workspace`, and `.gitlab-ci.yml` files are
**auto-generated** from `manifest/main.ml`. They contain the header:
`"This file was automatically generated, do not edit."`

**Do NOT edit these files directly.** Edit `manifest/main.ml` (or `manifest/product_*.ml`)
instead, then regenerate:
```bash
make -C manifest          # regenerate dune/opam/CI files
make -C manifest check    # verify generated files are up to date
```

Hand-written dune files (exceptions): top-level `dune`, `vendors/`, `scripts/`,
`src/lib_time_measurement/`, `src/riscv/dune`, `src/rust_libcrux/dune`.

## Build & Verification

### Build Commands

```bash
# Default build (all executables, dev profile)
make

# Build release executables
make release

# Build Octez-only executables (excludes Etherlink, Teztale)
make octez

# Build Layer 1 executables only
make octez-layer1

# Build specific executable
make octez-node
make octez-client

# Build with specific profile
make PROFILE=dev|release|static

# Build Tezt test suite
make build-tezt

# Build dependencies
make build-deps      # Production dependencies
make build-dev-deps  # Development dependencies
```

### Verification Before Commit

**Critical**: Every commit must be properly formatted and pass basic checks.

```bash
# Format all code (MUST pass before commit)
make fmt

# Check all linting
make check-linting

# Verify compilation
dune build

# Run unit tests
make test-unit
```

### Pre-commit Hooks

The repository includes pre-commit hooks (`.pre-commit-config.yaml`). Install them with:

```bash
pre-commit install
```

The hooks check:
- Trailing whitespace and end-of-file fixes
- Shell script linting (shellcheck, shfmt)
- OCaml formatting (ocamlformat)
- Dockerfile linting (hadolint)
- GitLab CI validation
- JSON/Jsonnet formatting

---

## OCaml LSP Server

AI coding agents can use the OCaml LSP server (`ocamllsp`) for code intelligence features like go-to-definition, find-references, hover documentation, and workspace symbol search.

### Setup

The LSP server is installed in the project's opam switch:

```bash
# Check if ocamllsp is available
opam exec -- which ocamllsp

# If not installed, install it
opam install ocaml-lsp-server

# Run through opam exec for proper environment
opam exec -- ocamllsp
```

### Available LSP Operations

| Operation | Status | Description |
|-----------|--------|-------------|
| `hover` | ✅ | Type signature and documentation |
| `goToDefinition` | ✅ | Jump to symbol definition |
| `findReferences` | ✅ | Find all usages |
| `documentSymbol` | ✅ | List symbols in current file |
| `workspaceSymbol` | ✅ | Search symbols across project |
| `goToImplementation` | ❌ | Not supported by ocaml-lsp |

### Building the Index for Project-Wide References

For project-wide find references, build the ocaml-index:

```bash
opam exec -- dune build @ocaml-index
```

This creates an index in `_build/default/.ocaml-index`.

Rebuild after significant code changes or keep in watch mode with:

```bash
opam exec -- dune build @ocaml-index --watch
```

---

## Testing

### Testing Frameworks

Octez uses several testing frameworks:

| Framework | Use Case | Location |
|-----------|----------|----------|
| **Tezt** | System/integration tests | `tezt/tests/` |
| **Alcotezt** | Unit tests (Alcotest wrapper for Tezt) | `src/*/test/` |
| **QCheck** | Property-based testing | Various |
| **ppx_expect** | Inline expectation tests | Various |

### Test Commands

```bash
# Run all tests
make test

# Run unit tests (fastest)
make test-unit

# Run protocol unit tests
make test-proto-unit

# Run non-protocol unit tests
make test-nonproto-unit

# Run Tezt integration tests
make test-tezt

# Run Tezt with info logs
make test-tezt-i

# Run Tezt verbose
make test-tezt-v

# Run specific Tezt tests by tag
dune exec tezt/tests/main.exe -- <tag>

# Run specific Tezt test file
dune exec tezt/tests/main.exe -- -f <file.ml>

# Run alpha protocol unit tests
make test-unit-alpha

# Run Etherlink unit tests
make test-etherlink-unit
```

### Test File Conventions

Test files should include a header comment:

```ocaml
(** Testing
    -------
    Component:    <component name>
    Invocation:   <command to run tests>
    Dependencies: <optional dependencies>
    Subject:      <brief description of what is tested>
*)
```

### Test Coverage

```bash
# Run tests with coverage instrumentation
./scripts/with_coverage.sh make test-coverage

# Generate coverage report
make coverage-report
# Report available at _coverage_report/index.html
```

---

## Code Formatting & Linting

### OCaml Formatting

OCaml code is formatted with `ocamlformat` (version 0.27.0). Configuration in `.ocamlformat`:

```
version=0.27.0
ocaml-version=4.14
wrap-fun-args=false
let-binding-spacing=compact
margin=80
doc-comments=before
```

### Formatting Commands

```bash
# Format all code (OCaml, Python, Shell)
make fmt

# Format OCaml only
make fmt-ocaml
# Or with auto-promote
dune build @fmt --auto-promote

# Format Python (in docs/)
make fmt-python

# Format shell scripts
make fmt-shell
```

### Linting Commands

```bash
# Check all linting
make check-linting

# Check OCaml linting with semgrep
make check-ocaml-linting
# Or directly:
./scripts/semgrep/lint-all-ocaml-sources.sh

# Check Python linting
make check-python-linting

# Check opam files
make check-opam-linting
```

### Semgrep Linting Rules (CI-enforced)

The `scripts/semgrep/` directory contains OCaml-specific linting rules that detect:
- **No `List.length` comparisons** — use `Compare.List_lengths` or `List.compare_lengths`
- **No `List.fold_left f () xs`** — use `List.iter` instead
- **No `List.concat (List.map ...)`** — use `List.concat_map`
- **No manual monadic folds** — use `List.fold_left_s`, `_e`, `_es` variants from Lwtreslib
- **No `List.compare_length_with xs 0`** — use pattern matching or `List.is_empty`
- Other common anti-patterns

---

## OCaml Coding Standards

### General Rules

- **Interface-first**: Provide `.mli` files for public modules
- **Documentation**: Use `(** ... *)` docstrings with `@param`, `@return` where helpful
- **Immutability**: Prefer immutable data structures and functional style
- **Error handling**: Use `Result` and `Option`, avoid exceptions for control flow

### Forbidden

- `Obj.magic` — Never use unsafe type coercion
- Mutable globals — Use proper state management
- Incomplete pattern matches — All cases must be handled

### Discouraged

- `List.hd`, `List.tl` — Use pattern matching instead
- `Option.get` — Use pattern matching or `Option.value`
- Stringly-typed code — Use variants/records for type safety
- Partial functions — Prefer total functions

### Imports and Opens

Most libraries globally open `Tezos_base.TzPervasives` via dune `(flags (-open ...))`,
which brings `Error_monad`, `Data_encoding`, syntax modules, etc. into scope.

**Local opens for syntax modules** — the dominant pattern:
```ocaml
let my_function arg =
  let open Lwt_result_syntax in       (* for 'a tzresult Lwt.t *)
  let* x = some_operation arg in      (* bind *)
  let*! y = some_lwt_operation x in   (* lift Lwt into Lwt+result *)
  let*? z = some_result_check y in    (* lift result into Lwt+result *)
  return z
```

Three syntax modules: `Lwt_result_syntax`, `Lwt_syntax`, `Result_syntax`.
Use `let open Data_encoding in` for encoding definitions.

Module aliases at top of file: `module Events = Chain_validator_events`.

### Naming Conventions

- **Functions, variables, types, fields**: `snake_case` — `check_chain_liveness`, `block_header`
- **Modules**: `CamelCase` — `Chain_validator`, `Block_header`
- **Module types**: `UPPERCASE` — `REQUESTER`, `COMPARABLE`
- **Protocol files**: `*_repr.ml` (pure types), `*_storage.ml` (storage access), `*_services.ml` (RPCs)
- **Monadic suffixes**: `_s` (Lwt), `_e` (result), `_es` (Lwt+result), `_p` (parallel Lwt)
- **Option/exception variants**: `_opt` (returns option), `_exn` (may raise)

### Error Handling

Binding operators (within syntax modules):

| Operator | Meaning |
|----------|---------|
| `let*`   | Main bind (semantics depend on open syntax module) |
| `let*!`  | Lift Lwt into Lwt+result (in `Lwt_result_syntax`) |
| `let*?`  | Lift result into Lwt+result (in `Lwt_result_syntax`) |
| `let+`   | Map (auto-wraps body in `return`) |

Common return helpers: `return`, `return_unit`, `return_none`, `return_some x`,
`return_nil`, `return_true`, `return_false`, `tzfail err`, `failwith "msg"`.

Errors must be declared and registered:
```ocaml
type error += My_error of {context : string}
let () = register_error_kind `Permanent ~id:"my_error" ~title:"..." ...
```

Add context to errors with `trace` / `record_trace`. Never let exceptions escape
module boundaries — return `result`/`tzresult` instead.

**Important**: Never catch `Stack_overflow` or `Out_of_memory` — these indicate non-deterministic runtime conditions.

### Code Duplication Prevention

Before writing a new function:

1. **Search the codebase** for similar implementations:
   ```bash
   grep -rn "your_keyword" src/
   ```

2. **Check common locations**:
   - `src/lib_base/` for base utilities
   - `src/lib_stdlib/` for standard library extensions
   - `src/lib_error_monad/` for error handling utilities

3. **If you find near-duplicates**: Refactor existing code to be more generic rather than creating a copy.

---

## Documentation Guidelines

### License Header

All OCaml files must start with this header:

```ocaml
(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: [year(s)] [Holder <email>]                        *)
(*                                                                           *)
(*****************************************************************************)
```

For Rust files:

```rust
//
// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: [year(s)] [Holder <email>]
//
```

### Docstrings

Document interfaces in `.mli` files using docstrings:

```ocaml
(** [my_function x y] computes the result of applying [x] to [y].

    @param x The first parameter
    @param y The second parameter
    @return The computed result
    @raise Invalid_argument if [x] is negative *)
val my_function : int -> int -> int
```

### TODO/FIXME Comments

Use this format for tracking work:

```ocaml
(* TODO: <issue reference>
   <one-line explanation>
*)

(* FIXME: <issue reference>
   <explanation for spec-violating code>
*)
```

- `TODO` — For future improvements (code works but could be better)
- `FIXME` — For code that doesn't fully implement its specification

Issue references can be:
- URL: `https://gitlab.com/tezos/tezos/-/issues/1234`
- GitLab notation: `#1234` or `!5678` (for MRs)

### README Files

README.md files are required in:
- Top-level directories (`src/`, `docs/`)
- Immediate subdirectories of `src/` (`src/lib_*/`, `src/bin_*/`)

Use the template from `docs/developer/guidelines.rst`.

---

## Logging Levels

Choose the appropriate level for each log event:

| Level | Use Case |
|-------|----------|
| `Debug` | Developer tracing, detailed execution flow |
| `Info` | Additional info, not critical for normal operation |
| `Notice` | Default level, user-relevant progress, concise |
| `Warning` | Potential anomalies requiring attention |
| `Error` | Issues requiring intervention |
| `Fatal` | Unrecoverable errors (use sparingly, never in libraries) |

### Logging Guidelines

- Make high-severity logs actionable with sufficient context
- Avoid logging in performance-critical sections
- Maintain consistent formatting in debug logs
- Never log sensitive information (keys, PII)

---

## Protocol Development

### Shell vs Protocol

The Octez codebase distinguishes between:

- **Shell** (`src/lib_*/`, `src/bin_*/`): Node implementation, networking, storage, client tools. Can be modified freely.
- **Protocol** (`src/proto_*/lib_protocol/`): Consensus state machine, fully deterministic. Changes require governance approval.

### Protocol Snapshots

- `proto_alpha/` — Development version (modifiable)
- `proto_NNN_*/` — Frozen protocol snapshots (read-only `lib_protocol/`)

**Important**: Never modify code in frozen protocol directories (`src/proto_0*/lib_protocol/`). The hash is computed from these directories.

### Migration Code

When submitting protocol migration code:
- Keep migration in separate commits from features
- Test the migration thoroughly
- Document testing procedures in MR description

For detailed protocol development guidelines, see:
- `docs/developer/protocol_playbook.rst`
- `docs/developer/entering_alpha.rst`

---

## Commit Messages

Use this format:

```
<Component>: <message in imperative mood>

[optional body with details]
```

**Examples**:
- `Shell: fix bug in RPC handler`
- `Client: add new transfer command`
- `Protocol/Alpha: optimize gas computation`
- `Tezt: add tests for double baking`

Keep the first line under 72 characters. Use imperative mood ("fix", "add", "update") not past tense ("fixed", "added").

---

## Pull Request Requirements

### Before Creating an MR

- [ ] Code is properly formatted (`make fmt`)
- [ ] Linting passes (`make check-linting`)
- [ ] Relevant tests pass (`make test-unit`)
- [ ] New functionality has tests
- [ ] Bug fixes include regression tests
- [ ] Documentation is updated if needed
- [ ] License headers are present on new files

### MR Description Should Include

1. **Context**: Why was this change made?
2. **What changed**: Summary of modifications
3. **Testing**: How was this tested? Manual testing instructions if applicable
4. **Breaking changes**: Any impact on users or other components

### Changelog

For user-facing changes, add an entry to `CHANGES.rst`:
- New features
- Bug fixes
- Breaking changes
- Deprecations

### Required Approvals

- 2 approvals from Octez Merge Team members
- All CI checks passing
- No unresolved discussions

---

## Git Hygiene

### Branch Strategy

- Semi-linear history (rebase before merge)
- Use merge requests, never push directly to master
- Branch naming: `username@feature-name`

### Commit Guidelines

- Small, atomic commits (each should compile and pass tests)
- No "peneloping" (doing something then undoing it)
- Don't mix refactoring with new features
- Use `git mv` for renames to preserve history

### Safety Rules

- **Never force push to master/main**
- **Ask confirmation before force pushing** to any shared branch
- **Never delete untracked files** without confirmation
- **Never commit secrets** or credentials

### Rebasing

Keep your branch up to date with master:

```bash
git fetch origin
git rebase origin/master
# Resolve conflicts if any
git push --force-with-lease  # Safer than --force
```

---

## Refactoring: Moving Code Between Files

When splitting large files or moving code between modules, **use shell commands** to avoid accidental modifications.

### DO: Use Shell Commands

```bash
# Extract lines 100-300 to a new file
sed -n '100,300p' src/large_file.ml > src/new_module.ml

# Extract from pattern to pattern
sed -n '/^let start_function/,/^let end_function/p' src/main.ml > src/extracted.ml
```

### DO: Use Edit Tool for Small Changes

- Adding license headers
- Adding `open` or `include` statements
- Updating `dune` files
- Removing extracted sections (after verification)

### DON'T: Copy Large Code Blocks

Never use Read+Write to "copy" code — AI agents can accidentally drop lines or modify content.

### Verification After Moving Code

```bash
# Verify line counts
wc -l src/original.ml src/new_module.ml

# Verify compilation
dune build

# Run tests
dune runtest

# Check formatting
dune fmt
```

---

## Code Review Guidelines

### Focus on Issues Only

- **Do**: Point out bugs, architectural problems, performance issues
- **Don't**: Praise what works well — assume good code is expected
- **Don't**: State that tests pass — CI validates this

### Be Concise

- Use bullet points
- One issue per bullet
- Include line numbers
- Provide fix suggestions

### Review Format

```markdown
## Review

### BLOCKER 🔴
- Issue description (line X)
- **Fix:** Concrete solution

### Issues
- Problem 1 (line Y)
- Problem 2 (lines Z-W)

### Questions
- Clarification needed on X
```

### What to Skip

- ❌ "What's great" sections
- ❌ Testing reports (CI handles this)
- ❌ Praise or encouragement
- ❌ Long explanations of why something is wrong

### What to Include

- ✅ Specific line numbers
- ✅ Concrete fix suggestions
- ✅ Links to correct patterns in codebase
- ✅ Severity indicators (BLOCKER, issue, question)

---

## CI Pipeline

### Special Labels

Add these labels to MRs to trigger specific CI behavior:

| Label | Effect |
|-------|--------|
| `ci--opam` | Trigger opam packaging tests |
| `ci--docs` | Test documentation scripts |
| `ci--docker` | Publish Docker image |
| `ci--arm64` | Build on ARM64 architecture |
| `ci--no-coverage` | Disable coverage job |

### Pipeline Types

- `before_merging` — MR pipelines
- `merge_train` — Merge queue
- `master_branch` — Post-merge validation

---

## Questions or Uncertainty

When unsure about:
- Architectural decisions
- API design choices
- Breaking changes
- Protocol modifications

**Ask for confirmation before proceeding.**

---

## Quick Reference

| Task | Command |
|------|---------|
| Build all | `make` |
| Build release | `make release` |
| Run all tests | `make test` |
| Run unit tests | `make test-unit` |
| Run Tezt tests | `make test-tezt` |
| Format code | `make fmt` |
| Check linting | `make check-linting` |
| Build docs | `make -C docs` |
| Clean build | `dune clean` |

---

## Further Reading

- Coding guidelines: `docs/developer/guidelines.rst`
- Contributing guide: `docs/developer/contributing.rst`
- Testing guide: `docs/developer/testing.rst`
- Error monad: `docs/developer/error_monad.rst`
- Tezt documentation: `docs/developer/tezt.rst`
