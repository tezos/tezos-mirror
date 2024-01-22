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
