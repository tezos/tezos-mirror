### Design Rationale

#### Parser

The parser is generated using the Lalrpop parser generator library. We didn't
involve Micheline right now because it was not required within the current
scope.

There are some libraries that we could have used to parse Michelson, the most
complete being `airgap-it/tezos-rust-sdk`. However, we ended up not using it, since
we thought that the layout of its data structures did not match well with
our plan.

We thought about using this library for now (as it could work with the current
scope which is quite limited), with the possibility of switching to something
else later, but also decided against it, because we considered that it would be
too hard to switch away from such a basic component later in the project.

We used the Lalrpop library because we had some experience with it, and it seemed
to work well in our initial trials.

##### Known differences between Octez and MIR parsers

Currently, for the sake of simplicity, MIR parser is a bit more lenient wrt non-essential elements, specifically, unlike Octez parser, MIR parser allows:

- Nested parentheses, i.e. `{ PUSH (((int))) (((((3))))) }` is accepted
- Parentheses inside sequences, i.e. `{ (Unit) ; (Unit) }` is accepted
- Arbitrary indentation

Additionally, annotations are currently ignored completely; thus, annotation rules are not verified.

#### Micheline

Micheline is implemented using non-owning approach for sequences and primitive applications,
actual nodes are allocated in an arena.

This is done mostly to avoid costs associated with allocating and subsequently
freeing many small `Vec`s via the system allocator during
parsing/deserialization, but it also simplifies pattern-matching in the
typechecker somewhat.

#### Lambdas

Typechecked lambdas carry their Micheline representation. This is done to avoid
dealing with (deprecated) annotations in the lambda body (e.g. on instructions
etc), which have to be `PACK`ed. The protocol uses the same approach.

##### Known differences from the protocol

Lambda code (in Micheline representation) isn't normalized. As an example where this makes a difference, consider running `PACK` on the following lambda: `{PUSH (pair nat int bool) {0; -3; False}; DROP}`. The protocol normalizes `{0; -3; False}` to `Pair 0 (Pair -3 False)`; MIR doesn't, and instead uses `{0; -3; False}` verbatim.

#### UNPACK/deserialization

Implemented via a pretty run-of-the-mill recursive descent parser.

`BytesIt` is introduced for simplicity (and avoiding allocations),
`Iterator<Item = u8>` doesn't quite fit, because we need to chomp exact number
of bytes (take will produce at most the requested number, which is not what we
want), and producing variable-length slices from Iterator would require
allocations.

Extra care is taken to avoid unnecessary allocations in the common cases, to that effect `SmallVec` with a sensible on-stack buffer is used for variable-length fields. This wastes some on-stack memory, but this shouldn't be an issue. One kink is annotations are still always-allocating, amending this is left for future work.

`BigInt` parser is reimplemented, as going through `Zarith` from
`tezos_data_encoding` is more involved than it's worth. The code is loosely
based on the one from `tezos_data_encoding`.

There's an interaction with lambdas (and how they carry raw Micheline around): unpacking requires allocating Micheline long-term, which means interpreter needs access to an arena. Carrying it in Ctx doesn't quite pan out due to borrow checker (also, it doesn't quite match the lifetime semantics of the tzt runner), so it's passed around the interpreter via an extra argument.

#### Gas consumption

Gas counter is represented as the type `Gas`, containing an `Option<u32>` with the current milligas amount (or `None` after gas exhaustion). A mutable reference to the gas counter is passed to typechecker and interpreter.

A method `consume` is implemented on the `Gas` type, which tries to consume the
cost passed as an argument; if there is enough gas remaining, the internal
counter is decreased by the appropriate amount; otherwise an error `OutOfGas` is
returned, and the internal counter is set to `None`. Any further calls to
`consume` are considered a logic error, and thus trigger a panic.

It is possible to request the current milligas amount (useful for reporting and
debugging purposes) using the method `milligas()`. It is a logic error to
request the current remaining gas after gas exhaustion, and thus triggers a
panic.

Gas costs are defined in submodules (i.e. namespaces) to avoid naming clashes.
Constant costs are declared as `const: u32`, while parametric costs are
implemented as functions returning `Result<u32, OutOfGas>`. In case of overflow,
`Err(OutOfGas)` is returned.

Gas consumption is done in-line in typechecker and interpreter. Both typechecker
and interpreter will immediately abort the computation on `OutOfGas` errors and
return the corresponding version of the error (`TcError::OutOfGas` or
`InterpretError::OutOfGas` respectively).

The current design is broadly consistent with the design used by the Tezos
protocol, modulo the mutable reference and triggering a panic on accessing the
gas counter post exhaustion.

Alternative designs:

- Instead of a mutable reference, `typecheck` and `interpret` could return the
  remaining gas amount. This is more in line with the approach used by the Tezos
  protocol, however it's not idiomatic for Rust.

- Gas consumption could, in theory, be handled entirely separately from the main
  typechecker/interpreter code, however as gas consumption is highly dependent
  on what typechecker/interpreter actually do, it would lead to a lot of
  unnecessary code duplication and considerable difficulty in predicting the
  interpreter's behaviour without doing the interpretation proper.

- There was some discussion related to handling possible gas consumption after
  `OutOfGas` error is thrown. Alternatives included setting the gas counter to
  `0` after exhaustion (such that future calls to `consume` would trigger
  `OutOfGas` again), or leaving it be in some consistent but undefined state
  (which would simplify the code somewhat).

  Ultimately, trying to consume gas post-exhaustion was deemed to always
  indicate a logic error, and thus, instead of masking such errors, a decision
  was made to panic instead.

`DUP` instruction's gas consumption is treated as constant, following the
example of the Tezos protocol. However, it's not fully consistent with its
implementation, which copies the value, thus incurring O(n) cost for things like
pairs and lists. Until we have a proper gas model for MIR and a benchmarking
set-up, this is left as is -- in any case, gas model will need some adjustments
later.
