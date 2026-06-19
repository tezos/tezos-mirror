# MIR — gas, stack, and heap

## Background

MIR runs inside the Etherlink WASM kernel. The WASM PVM host pins the
Rust call stack at ~1 MiB. The Tezos protocol caps a single operation's
gas budget (~1 M gas mainnet today) and an operation's serialized size
(~32 KiB for transfers, more for originations). MIR's correctness goal
is that **every script accepted by the protocol completes inside its
gas budget**: success or failure is bounded by gas, never by stack or
heap.

This document records the budget interactions across the recursive →
iterative-worklist conversion (the "MIR: Explicit Stack Frames" MR
series) and the trade-offs the conversion introduces.

## The original threat model

A naive recursive walker (typechecker, interpreter, PACK encoder,
Micheline decoder, `Drop`) walks the AST in lockstep with the input
shape. Attacker-controlled input (a deep parameter type, a deep typed
value built via `PAIR ; PAIR ; …`, a deeply nested `IF` tree) drives
the Rust call stack proportional to input depth. On a 1 MiB WASM stack,
a depth of roughly 5k–10k frames overflows and **traps the PVM**. A
trap is non-deterministic from the protocol's point of view (no gas
receipt, no error result) and is not absorbed by the operation gas
budget — it's a kernel-level abort.

This is the failure mode the worklist conversion addresses: move the
walker's recursion off the call stack into a `Vec<Frame>` on the heap,
so the kernel cannot trap on input depth alone.

## The new trade-off

Worklists trade stack-bound failure for heap-bound or
gas-charging-bound failure. There are three regimes to think about:

### 1. Gas dominates (the goal)

The walker charges gas per frame visited. Once the per-operation gas
budget is exhausted, the walker returns `OutOfGas` — a clean protocol
failure with a receipt. This is the regime we want every walker to be
in.

Status:
- **Encoder** (`Micheline::encode_starting_with`) — gas charged at
  entry via `interpret_cost::micheline_encoding`, which calls
  `gas::collect_micheline_size`. That helper is itself a tree walk; if
  it stays recursive, the gas-charging step inherits the stack-overflow
  hazard. The iterative `encode_micheline` worklist is irrelevant
  unless `collect_micheline_size` is also iterative. It is converted to
  an iterative `Vec<&Micheline>` worklist in the residual-walks MR
  (!21986) of this stack (tracked in Linear as L2-1429); until that
  lands the encoder regression tests run at a reduced depth that fits
  the recursive helper, and the depth-100k variants are added in
  !21986.
- **Decoder** (`Micheline::decode_raw`) — gas charged at entry via
  `interpret_cost::micheline_decoding_bytes`, which is **byte-based**
  (`20 milligas/byte`). No tree walk in the gas computation. The
  iterative decoder is gas-dominated end-to-end.
- **Typechecker** — gas charged per step in `parse_ty`,
  `typecheck_value`, `typecheck_instruction`. Some property checks
  (`ensure_prop`, `size_for_gas`, `PartialEq for Type`,
  `ensure_ty_eq`) still walk the tree recursively from inside the
  driver. Until those are iterative, an adversarial deep type
  (e.g. `set (option (option …))`) parses fine then traps inside
  the property check.
- **Interpreter** — gas charged per instruction step. The instruction
  worklist is iterative, but: (a) `typecheck_map_block` still recurses on
  nested MAP_BLOCK, growing the Rust stack one frame per nesting level
  (the lambda paths no longer do: since L2-1663 both `PUSH (lambda …)` and
  value-level lambdas run their body on the shared worklist, and the
  dedicated `typecheck_lambda` helper was removed); (b) the public
  `Instruction::interpret` skips the worklist driver entirely; (c) on
  `Err` unwind (FAILWITH mid-ITER, OOG mid-DIP), the `Vec<InterpFrame>`
  and `Vec<IStack>` drop their `Rc<TypedValue>` children via the
  auto-derived `Drop`, which recurses.

### 2. Heap dominates (a hidden DoS amplification)

If the gas charge for an operation is much cheaper than the heap cost
of the worklist it triggers, an attacker can craft an input that's
in-budget for gas but out-of-budget for the kernel's WASM heap.

Worst case under the current design:

- **Decoder**: 20 milligas/byte → a 3 M gas budget funds **~150 MB**
  of input bytes. A pathological input of nested-singleton `Seq`s is
  5 bytes per layer, so 30 M layers in budget. Each `DecFrame` is
  ≈ 48 B plus an `iters` entry of ≈ 16 B, giving ≈ 80 B per layer →
  **~2.4 GB worklist heap**. Far above any realistic WASM heap budget.

  L1 protocol bounds this differently: the OCaml typechecker is
  recursion-bounded by the OCaml stack (~1 MiB by default), then the
  whole process has access to whatever the host gives it — not a
  hard 4 GiB linear-memory cap.

  Recommendation tracked here as a follow-up: either (a) add a
  structural depth cap inside `decode_micheline` (e.g. reject after
  10⁴ nested containers), or (b) tighten the cost helper to charge
  a structural-depth component on top of bytes, matching L1's
  `script_repr_costs_generated.ml:23` cost shape.

- **Encoder**: charges `100 milligas/node` → the gas charge **does**
  scale with structural depth. Encoder heap is bounded by gas.
  Status: safe.

- **Typechecker / interpreter**: charge per-step gas inside the worklist.
  Heap is bounded by gas as long as every recursive step pays its way.
  Status: safe modulo the residual recursive helpers above (which are
  the stack issue, not a heap-amplification issue).

### 3. Stack still dominates (the hazard the conversion is meant to remove)

Three residual paths still grow the Rust stack proportional to input
depth, and so retain the original WASM-PVM-trap hazard:

- `typecheck_map_block` / `typecheck_view` → `typecheck` (call-graph cycle
  through Rust function calls). (The lambda paths were converted to the shared
  worklist in L2-1663, removing the `typecheck_lambda` helper.)
- `ensure_prop`, `size_for_gas`, the derived `PartialEq for Type` (when
  the manual impl in `Type` has not yet landed).
- `Rc<TypedValue>` and the auto-derived `Drop` on values built by the
  interpreter — `TypedValue` does not have an iterative `Drop`, only a
  helper `drain_deep_typed_value` that callers must invoke explicitly.
  Until every kernel callsite drains before scope-exit, deep typed
  values trap on drop.

Until these are closed, the MR series' end-to-end claim ("MIR is
bounded by gas, not by stack") does not hold for adversarial inputs
shaped to hit one of the three.

## Designing for the kernel budget

When adding a new walker over `Type`, `TypedValue`, `Instruction`, or
`Micheline`, ask:

1. **Is the input attacker-controlled?** If yes (script storage,
   operation parameter, PACK input), the walker must be iterative
   *or* the surrounding gas charge must scale with structural depth
   *and* the per-step heap cost must be bounded by that charge.

2. **Does the walker run from within a gas-charged path?** If yes,
   verify the gas charge accumulates as the walker progresses
   (not lump-sum at entry), so OOG short-circuits cleanly.

3. **What's the worst-case Vec depth vs the operation gas budget?**
   For an input that maximizes structural depth within the operation
   size limit, compute: `(heap_per_frame) × (max_depth)`. If this
   exceeds the kernel's WASM heap, the walker has a hidden DoS
   amplification and either the gas charge must increase or a
   structural cap must be added.

4. **What happens on `Err` unwind?** If the walker's frame `Vec` holds
   `Rc<TypedValue>` values, dropping the Vec on `Err` invokes
   `TypedValue::drop` recursively. Either drain explicitly before
   the early return, or use a representation that drops iteratively.

## References

- The original threat model and worklist conversion: see commit
  `c540d5b9a5b` (`MIR/encoder: convert encode_micheline to an iterative
   worklist`) and the rest of the "MIR: Explicit Stack Frames" stack.
- `gas::interpret_cost::micheline_encoding` (`etherlink/kernel_latest/mir/src/gas.rs`)
  — encoder gas charge by node count.
- `gas::interpret_cost::micheline_decoding_bytes` — decoder gas charge
  by byte count.
- L1 reference: `src/proto_alpha/lib_protocol/script_repr_costs_generated.ml`
  for the protocol-side cost shapes (mirrored by MIR for parity).
- L1 reference: `src/proto_alpha/lib_protocol/script_ir_translator.ml`
  for the recursive typechecker that MIR mirrors.
