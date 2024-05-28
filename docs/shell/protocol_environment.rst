Protocol Environment
====================

The protocol environment is a restricted API that may be used by the economic protocol's code, on one hand,
and the rest of the Octez code, on the other hand, to interact with each other. Firstly, this allows protocols
to only call authorized modules and functions. Secondly, it allows the node (i.e., the main binary built around the shell)
as well as the other binaries (e.g., the baker) to call a subset of the protocol functions.

Motivation
----------

In the currently active protocol, as in any sound future protocols, updates are approved by
voting. That way, the responsibility of switching to a new protocol code
is the responsibility of voters, and one could argue that it is up to
them to check that the code does not call, for instance, unsafe array
access functions.

Yet, we decided to introduce a minimum level of machine checks, by
compiling the protocol with a specific compiler that checks that no known-unsafe
function is used in its code. This static form of sandboxing is performed by the
OCaml typechecker: we simply compile protocols in a restricted set of
modules with restricted interfaces that hide any unsafe, non wanted
feature.

Another goal of that specific environment is maintaining a stable OCaml
API for protocol development. Imagine that at some point, the OCaml
standard library changes (a function is added or removed, a type is
changed), then we will be able to upgrade to the new OCaml while still
remaining compatible with past protocols, by providing an adapter layer.


Environment contents
--------------------

Here is a quick description of each file in this environment, all located under
:src:`src/lib_protocol_environment/`:

-  Files in the ``sigs/vX/`` directories declare the interface that the
   protocols can be compiled against. These interfaces are repeats of other
   library interfaces with some unsafe functions removed. These interfaces are
   frozen and cannot be changed: changing them can break previous protocols.

   *  Special cases: the files ``context.mli``, ``fitness.mli``, and ``updater.mli`` are interfaces that the *protocol* exposes to the shell (rather than the other way around).

   *  All the files in ``sigs/vX/`` are assembled into a single interface file ``VX.mli`` by a helper program located in ``s_packer/``.

-  Files in the ``structs/vX/`` directory declare compatibility layers for old
   protocol environments. E.g., ``structs/V0/error_monad_traversors.ml`` contain
   the code of error-monad-compatible list traversors that used to be part of
   the Error Monad at the time of the environment V0. These implementations are
   meant to evolve alongside the ecosystem: when an incompatibility is
   introduced in a library a compatibility layer needs to be implemented or amended
   in order to provide a backwards compatible overlay to the protocol. These
   implementations are assembled into a single implementation file ``VX.ml`` by
   a helper program located in ``s_packer/``.

The API can be found in :package-api:`tezos-protocol-environment <octez-proto-libs/Tezos_protocol_environment/index.html>`

.. _environment_versions:

Environment versions
--------------------

An environment interface includes a frozen, immutable interface provided
to the protocol. And so when a new protocol needs new functions, types, or values,
this protocol must use a new environment. This is why the environments are
versioned.

A protocol's manifest (e.g., :src:`src/proto_alpha/lib_protocol/TEZOS_PROTOCOL`)
includes a field named ``expected_env_version``. This field specifies the
environment used to compile the protocol.

Protocol environments can only ever increase in version. Specifically, from
within a protocol P1 built on environment version X, the activation of a
protocol P2 built on environment Y fails unless X ≤ Y.

Some examples of prior environment switches can be found within past protocol changelogs:

 - :doc:`Edo - environment V1 <../protocols/008_edo>`
 - :doc:`Hangzhou - environment V3 <../protocols/011_hangzhou>`
 - :doc:`Ithaca - environment V4 <../protocols/012_ithaca>`
 - :doc:`Jakarta - environment V5 <../protocols/013_jakarta>`
 - :doc:`Kathmandu - environment V6 <../protocols/014_kathmandu>`

For how to prepare and implement an environment switch, see :doc:`../developer/protocol_environment_upgrade`.
