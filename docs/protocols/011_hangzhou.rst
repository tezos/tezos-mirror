Protocol Hangzhou
=================

**Important: revision 011_PtHangzH of protocol Hangzhou contains a bug that
is corrected in the latest version 011_PtHangz2**

This page contains all the relevant information for protocol Hangzhou
(see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_011_PtHangz2` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Hangzhou with respect
to Granada.

.. contents::

New Environment Version (V3)
----------------------------

This protocol requires a different protocol environment than Granada.
It requires protocol environment V3, compared to V2 for Granada.
(MR :gl:`!3040`)

- Removed legacy logging interface. (MR :gl:`!2876`)

- Removed data-encoding backwards compatibility layer. (MR :gl:`!2686`)

- Reintroduced ``tag_size``. (MR :gl:`!2948`)

- Exposed ``Lwtreslib.List``. (MR :gl:`!3116`)

- Environment now uses ``Map`` and ``Set`` from ``Lwtreslib``. (MR :gl:`!3124`)

- Updated to data-encoding version 0.4. (MR :gl:`!3149`)

- Documented and renamed operation ordering in ``Updater``. (MR :gl:`!3179`)

- ``Lwtreslib``: now provides exception-catching functions. (MR :gl:`!2886`)

- Added context hash version. (MR :gl:`!3163`)

- Removed the ``fork_test_chain`` function. (MR :gl:`!3213`)

- Added ``Result`` and catching helpers. (MR :gl:`!3154`)

- Added support for chunking in RPCs. (MR :gl:`!3189`)

- Source of the Alpha protocol can mostly be compiled into Coq with ``coq-of-ocaml``. (MR :gl:`!3152`)

- Encapsulated script caching. (MR :gl:`!3352`)

- Updated Zarith to version 1.12. (MR :gl:`!2970`)

Receipts, Balance Updates
-------------------------

- **Breaking change:** Rewards balance updates for nonce revelations
  or endorsements now mention the cycle at which the rewards were
  granted instead of the cycle of the level carried by the operation.
  Likewise for deposits balance updates related to endorsement
  operations, they now mention the cycle at which the funds have been
  deposited. (MR :gl:`!3270`)

RPC Changes
-----------

- Deprecated RPC ``POST ../endorsing_power`` has been removed. Clients
  already used ``GET ../helpers/endorsing_rights`` which is clearer, as
  powerful and equally costly in terms of computation for the
  node. (MR :gl:`!3395`)

- The RPCs ``GET ../context/delegates/[PUBLIC_KEY_HASH]/..`` now fail
  gracefully with a specific error ``delegate.not_registered`` when
  ``PUBLIC_KEY_HASH`` is not a delegate instead of the generic
  ``context.storage_error``. (MR :gl:`!3258`, issues :gl:`#450`,
  :gl:`#451`, and :gl:`#1276`)

- RPC ``GET ../context/constants`` exposes the "fixed" constants
  ``cache_layout`` and ``michelson_maximum_type_size`` (MR
  :gl:`!3454`)

Timelock
--------

- Added timelock-related types and opcodes to Michelson.
  They allow a smart contract to include a countermeasure against
  Block Producer Extractable Value.
  More info in :doc:`Timelock <../alpha/timelock>`.
  (MRs :gl:`!3160` :gl:`!2940` :gl:`!2950` :gl:`!3304` :gl:`!3384`)

Michelson On-Chain Views
------------------------

:doc:`Views <../active/views>` are a new mechanism for contract calls that:

- are read-only: they may depend on the contract storage but cannot
  modify it nor emit operations (but they can call other views);

- take arguments as input in addition to the contract storage;

- return results as output;

- are synchronous: the result is immediately available on the stack of
  the caller contract.

There are two added Michelson primitives: ``VIEW`` (instruction) and
``view`` (top-level keyword).

- `TZIP <https://gitlab.com/tezos/tzip/-/merge_requests/169>`__
- MRs :gl:`!2359`, :gl:`!3431` and :gl:`!3654`

Global Constants
----------------

- A new manager operation and corresponding CLI command have been added
  allowing users to register Micheline expressions in a global table of
  constants, returning an index to the expression. A new primitive
  ``constant <string>`` has been added that allows contracts to reference
  these constants by their index. When a contract is called, any
  constants are expanded into their registered values. The result is
  that users can use constants to originate larger contracts, as well as
  share code between contracts.

- `TZIP <https://gitlab.com/tezos/tzip/-/merge_requests/117>`__

- MRs: :gl:`!2962`, :gl:`!3467`, :gl:`!3473`, :gl:`!3492`, :gl:`!3731`

Cache
-----

- A chain-sensitive cache is now available to the protocol developers.
  This cache can be seen as an in-memory context providing fast access
  to the most recently used values.

- The protocol now keeps contracts' source code and storage in the
  cache. This reduces the gas consumption for the most recently used
  contracts.

- The new RPC ``context/cache/contracts/all`` returns the list of contracts
  in the cache.

- The new RPC ``context/cache/contracts/size`` returns an overapproximation
  of the cache size (in bytes).

- The new RPC ``context/cache/contracts/size_limit`` returns the maximal
  cache size (in bytes). When this size is reached, the cache removes
  the least recently used entries.

- The new RPC ``context/cache/contracts/rank`` gives the number of contracts
  that are less recently used than the one provided as argument.

- The new RPC ``scripts/script_size`` gives the size of the script
  and its storage when stored in the cache.

- MRs: :gl:`!3234` :gl:`!3419` :gl:`!3717`

- Gas instrumentation implemented in MR :gl:`!3430`

Context Storage Flattening
--------------------------

Hex-nested directories like ``/12/af/83/3d/`` are removed from the
context. This results in better context access performance. (MR :gl:`!2771`)

Gas computation has been adapted to this new flattened context layout. (MR :gl:`!2771`)

Bug Fixes
---------

- A bug in Michelson comparison function has been fixed (MR :gl:`!3237`)

- Fix balance updates that indicate inaccurate burned amounts in some
  scenarios (MR :gl:`!3407`)

Minor Changes
-------------

- Gas improvements for typechecking instruction ``CONTRACT`` (MR
  :gl:`!3241`) and general gas cost adjustments (MRs :gl:`!3441`
  :gl:`!3724`)

- Other internal refactorings or documentation. (MRs :gl:`!2021` :gl:`!2984`
  :gl:`!3042` :gl:`!3049` :gl:`!3088` :gl:`!3075` :gl:`!3266` :gl:`!3270`
  :gl:`!3285` :gl:`!3375` :gl:`!3247`)

- Set the predecessor version of the protocol to Granada (MR :gl:`!3347`)

- Check order in the validation of endorsements has changed to not
  compute all endorsement slots of a level if the endorsement is
  invalid. (MR :gl:`!3395`)

- Fix handling of potential negative integer in ``Raw_level_repr``
  encoding. (MR :gl:`!3273`)

- RPCs ``GET ../helpers/endorsing_rights`` and ``GET ../helpers/baking_rewards``
  have been moved into the RPC plugin. Nothing has changed from the
  end-user perspective for now but further improvements to their
  performance will become easier now that they are decoupled from the
  protocol development cycle. (MR :gl:`!3368`)

- Gives an increase to the liquidity baking sunset level of
  211,681 blocks, or five voting periods plus 6,881 blocks to
  sync with the following voting period, roughly an additional two
  months and a half. Without this, the subsidy would halt during the lifespan of
  this protocol. With this change the subsidy can continue until the
  protocol after this one is activated, even accounting for some
  delays in proposal injection and/or a restarted voting process,
  while still making sure it won't extend to two protocols after this
  one without a more significant increase. This follows the spirit of
  `the liquidity baking TZIP <https://gitlab.com/tezos/tzip/-/blob/367628e1a576c3926bedc1d6107b2945607c2605/drafts/current/draft-liquidity_baking.md>`_ in that it is still roughly six months
  from Granada activation and requires a referendum on the subsidy in
  the protocol after this one. (MR :gl:`!3425` :gl:`!3464`)

- Reimplemented ``Logging``.  It now has Lwt-less APIs and the messages are handled
  by the shell. (MR :gl:`!3225`)

- The size limit on Michelson types has been roughly doubled (from 1000 to 2001). (MR :gl:`!3434`)
