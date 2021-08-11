.. _alpha:

Protocol Alpha
==============

This page contains all the relevant information for protocol Alpha, a
development version of the Tezos protocol.

The code can be found in the ``src/proto_alpha`` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Alpha with respect
to Granada.

The main novelties in the Alpha protocol are:

- Context storage flattening for better context access performance.  Hex-nested
  directories like `/12/af/83/3d/` are removed from the context.  (MR :gl:`!2771`)
- Gas calculation fix based on the new flattend context layout (MR :gl:`!2771`)
- Caching of smart contracts source code loading and typechecking (MR :gl:`!3234`)

.. contents:: Here is the complete list of changes:

New Environment Version (V3)
----------------------------

This protocol requires a different protocol environment than Granada.
It requires protocol environment V3, compared to V2 for Granada.

Bug fixes
---------

- A bug in Michelson comparison function has been fixed (MR :gl:`!3237`)

Minor changes
-------------

- Gas improvements for typechecking instruction ``CONTRACT`` (MR :gl:`!3241`)

- Other internal refactorings or documentation. (MRs :gl:`!2021` :gl:`!2984`
  :gl:`!3042` :gl:`!3049` :gl:`!3088` :gl:`!3075` :gl:`!3266`)

New Features
------------

- Expose timelock primitive to the Michelson interpreter.
  (MRs :gl:`!3160` :gl:`!2940` :gl:`!2950`) adds to michelson timelock
  related types and opcode. It's allows a smart contract to include a
  countermeasure against Block Producer Extractable Value.  More infos
  in docs/alpha/timelock.rst

Michelson onchain views
-----------------------

:ref:`Views <MichelsonViews_alpha>` are a new mechanism for contracts calls that:


- are read-only: they may depend on the contract storage but cannot modify it nor emit operations (but they can call other views),
- take arguments as input in addition to the contract storage,
- return results as output,
- are synchronous: the result is immediately available on the stack of the caller contract.

There are two added Michelson primitives: ``VIEW`` (instruction) and ``view`` (top-level keyword).

- `TZIP <https://gitlab.com/tezos/tzip/-/merge_requests/169>`__
- `MR <https://gitlab.com/tezos/tezos/-/merge_requests/2359>`__

Global constants
----------------

- A new manager operation and corresponding CLI command have been added
  allowing users to register Micheline expressions in a global table of
  constants, returning an index to the expression. A new primitive
  `constant <string>` has been added that allows contracts to reference
  these constants by their index. When a contract is called, any
  constants are expanded into their registered values. The result is
  that users can use constants to originate larger contracts, as well as
  share code between contracts.

- `TZIP: <https://gitlab.com/tezos/tzip/-/merge_requests/117>`__
- `MR: <https://gitlab.com/tezos/tezos/-/merge_requests/2962>`__
