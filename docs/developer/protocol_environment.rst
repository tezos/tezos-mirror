Economic protocol sandboxing
============================

In the currently active protocol, as in any sound future protocols, updates are approved by
voting. That way, the responsibility of switching to a new protocol code
is the responsibility of voters, and one could argue that it is up to
them to check that the code does not call, for instance, unsafe array
access functions.

Yet, we decided to introduce a minimum level of machine checks, by
compiling with a specific compiler that checks that no known-unsafe
function is used. This static form of sandboxing is performed by the
OCaml typechecker: we simply compile protocols in a restricted set of
modules with restricted interfaces that hide any unsafe, non wanted
feature.

Another goal of that specific environment is maintaining a stable OCaml
API for protocol development. Imagine that at some point, the OCaml
standard library changes (a function is added or removed, a type is
changed), then we will be able to upgrade to the new OCaml while still
remaining compatible with past protocols, by providing an adapter layer.

Here is a quick description of each file in this environment, all located under
:src:`src/lib_protocol_environment/`:

-  Files in the ``sigs/vX/`` directories declare the interface that the
   protocols can be compiled against. These interfaces are repeats of other
   library interfaces with some unsafe functions removed. These interfaces are
   frozen and cannot be changed: changing them can break previous protocols.
   These files are assembled into a single interface file ``VX.mli`` by a helper
   program located in ``s_packer/``.
-  Special cases: the files ``context.mli``, ``fitness.mli``, and
   ``updater.mli`` are interfaces that the protocol exposes to the shell.
-  Files in the ``structs/vX/`` directory declare compatibility layers for old
   protocol environments. E.g., ``structs/V0/error_monad_traversors.ml`` contain
   the code of error-monad-compatible list traversers that used to be part of
   the Error Monad at the time of the environment V0. These implementations are
   meant to evolve alongside the ecosystem: when an incompatibility is
   introduced in a library an implementation needs to be implemented or amended
   in order to provide a backwards compatible overlay to the protocol. These
   implementations are assembled into a single implementation file ``VX.ml`` by
   a helper program located in ``s_packer/``.


Environment versions
--------------------

The environment interfaces are frozen, providing forever an immutable interface
to the protocol. And so when a protocol needs new functions, types, or values,
this protocol must use a different environment. This is why the environments are
versioned.

A protocol's manifest (e.g., :src:`src/proto_alpha/lib_protocol/TEZOS_PROTOCOL`)
includes a field named ``expected_env_version``. This field specifies the
environment used to compile the protocol.

Protocol environments can only ever increase in version. Specifically, from
within a protocol P1 built on environment version X, the activation of a
protocol P2 built on environment Y fails unless X ≤ Y.
