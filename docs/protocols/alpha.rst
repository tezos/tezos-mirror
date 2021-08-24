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
- Caching of smart contracts loading and typechecking (MR :gl:`!3234`)

.. contents:: Here is the complete list of changes:

New Environment Version (V3)
----------------------------

This protocol requires a different protocol environment than Granada.
It requires protocol environment V3, compared to V2 for Granada.

Receipts, Balance updates
-------------------------

- /!\ Breaking change: Rewards balance updates for nonce revelations or endorsements now mention the cycle at which the rewards were granted instead of the cycle of the level carried by the operation.
  Likewise for deposits balance updates related to endorsement operations, they now mention the cycle at which the funds have been deposited.

Bug fixes
---------

- A bug in Michelson comparison function has been fixed (MR :gl:`!3237`)

RPC changes
-----------

- Deprecated RPC ``POST ../endorsing_power`` has been removed. Clients
  already used ``GET ../helpers/endorsement_rights`` which is clearer, as
  powerful and equaly costly in term of computation for the
  node. (MR :gl: `!3395`)

Minor changes
-------------

- Gas improvements for typechecking instruction ``CONTRACT`` (MR :gl:`!3241`)

- Other internal refactorings or documentation. (MRs :gl:`!2021` :gl:`!2984`
  :gl:`!3042` :gl:`!3049` :gl:`!3088` :gl:`!3075` :gl:`!3266` :gl:`!3270`
  :gl:`!3285`)

- Check order in the validation of endorsements has changed to not
  compute all endorsement slots of a level if the endorsement is
  invalid. (MR :gl: `!3395`)

New Features
------------

Timelock
--------

- Expose timelock primitive to the Michelson interpreter.
  (MRs :gl:`!3160` :gl:`!2940` :gl:`!2950` :gl:`!3304` :gl:`!3384`) adds to Michelson timelock
  related types and opcode. It allows a smart contract to include a
  countermeasure against Block Producer Extractable Value.  More info
  in :doc:`Timelock <timelock>`.

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

Cache
-----

- A chain-sensitive cache is now available to the protocol developers.
  This cache can be seen as an in-memory context providing fast access
  to the most recently used values.

- The protocol now keeps contracts' source code and storage in the
  cache. This reduces the gas consumption for the most recently used
  contracts.

- The new RPC ``context/cache/contracts`` provides the list of contracts
  in the cache.

- The new RPC ``context/cache/contract_rank`` gives the number of contracts
  older than the one provided as argument.
