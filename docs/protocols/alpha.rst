Protocol Alpha
==============

This page contains all the relevant information for protocol Alpha
(see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_alpha` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Alpha with respect
to Protocol I.

.. contents::

New Environment Version (V5)
----------------------------

This protocol requires a different protocol environment than Ithaca.
It requires protocol environment V5, compared to V4 for Ithaca.
(MR :gl:`!4071`)

Liquidity Baking
----------------

Several changes are made to the Liquidity Baking Escape Vote:

- The options are renamed ``On`` (instead of ``false``) and ``Off``
  (instead of ``true``) to reduce confusion.

- A third ``Pass`` option is added. When this option is used the
  exponential moving average (EMA) of escape votes is not affected by
  the block. Note to developers of baking software: we don't recommend to
  use this option as a default value; instead we recommend to force the user
  to explicitly choose one of the three options; this behavior has been
  implemented in Octez' ``tezos-baker``.

- The escape hatch threshold is reset to 50% to account for the new
  symmetry in the escape vote introduced by ``Pass`` option.

- The precision of the EMA computation has been increased by a factor
  of 1000. To achieve this without overflowing, this computation is
  now performed using arbitrary-precision arithmetic. The EMA itself
  and the EMA threshold are still stored on 32bits.

- EMA is always rounded toward the threshold.

- When the EMA reaches the threshold, the deactivation of the subsidy
  is not permanent anymore. If the proportion of bakers voting ``On``
  later increases and the EMA falls back below the threshold then the
  subsidy is restarted.

- The Liquidity Baking Escape Vote is renamed into "Liquidity Baking
  Toggle Vote".

Transaction Optimistic Rollups
------------------------------

- Feature flag & origination. (MR :gl:`!3915`)

Tickets Hardening
-----------------

- Tickets lazy storage diff. (MR :gl:`!4011`)

Smart Contract Optimistic Rollups
---------------------------------

- Add smart-contract rollup creation. (MR :gl:`!3941`)

- Add a smart contract rollup node. (MR :gl:`!4000`)

- Add Inbox. (MR :gl:`!4020`)

Voting procedure
----------------

The voting power of a delegate is no longer rounded to rolls, it is
now instead the full staking power of the delegate, currently
expressed in mutez.

Breaking Changes
----------------

- The binary encoding of the result of the ``Transaction`` operation
  has changed.  Its contents now vary depending on the kind of
  destination. The default cases (implicit and smart contracts) are
  prefixed with the tag ``0``.

- The `consumed_gas` field in the encoding of operations becomes
  **deprecated** in favour of `consumed_milligas`, which contains
  a more precise readout for the same value. `consumed_milligas`
  field was added to the encoding of block metadata for uniformity.
  (MR :gl:`!4388`)

- The following RPCs output format changed:

  1. ``/chains/<chain_id>/blocks/<block>/votes/proposals``,
  2. ``/chains/<chain_id>/blocks/<block>/votes/ballots``,
  3. ``/chains/<chain_id>/blocks/<block>/votes/listings``,
  4. ``/chains/<chain_id>/blocks/<block>/votes/total_voting_power``,
  5. ``/chains/<chain_id>/blocks/<block>/context/delegates/<public_key_hash>``
  6. ``/chains/<chain_id>/blocks/<block>/context/delegates/<public_key_hash>/voting_power``

  The voting power that was represented by ``int32`` (denoting rolls)
  is now represented by an ``int64`` (denoting mutez). Furthermore, in
  the RPC ``/chains/<chain_id>/blocks/<block>/votes/listings``, the
  field ``rolls`` has been replaced by the field ``voting_power``.

- Encoding of transaction and origination operations no longer contains
  deprecated `big_map_diff` field. `lazy_storage_diff` should be used
  instead. (MR: :gl:`!4387`)

- The JSON and binary encodings for Liquidity Baking Toggle Votes have
  changed as follows:

.. list-table:: Changes to encodings of Liquidity Baking Toggle Vote
   :widths: 20 20 20 20 20
   :header-rows: 1

   * - Vote option
     - Old binary encoding
     - Old JSON encoding
     - New binary encoding
     - New JSON encoding

   * - ``On``
     - ``0x00``
     - ``false``
     - ``0x00``
     - ``"on"``

   * - ``Off``
     - any other byte
     - ``true``
     - ``0x01``
     - ``"off"``

   * - ``Pass``
     - N/A
     - N/A
     - ``0x02``
     - ``"pass"``

- The values of the Liquidity Baking EMA in block receipts and the
  Liquidity Baking EMA threshold in the constants have been scaled by
  1000, the new value of the threshold is 1,000,000,000. To compute
  the proportion Off/(On + Off) of toggle votes the following formula
  can be used: liquidity_baking_toggle_ema / 2,000,000,000.

Bug Fixes
---------

- Expose `consumed_milligas` in the receipt of the `Register_global_constant`
  operation. (MR :gl:`!3981`)

- Refuse operations with inconsistent counters. (MR :gl:`!4024`)

Minor Changes
-------------

- The RPC ``../context/delegates`` takes two additional Boolean flags
  ``with_minimal_stake`` and ``without_minimal_stake``, which allow to
  enumerate only the delegates that have at least a minimal stake to
  participate in consensus and in governance, or do not have such a
  minimal stake, respectively. (MR :gl:`!3951`)

- Make cache layout a parametric constant of the protocol. (MR :gl:`!4035`)

- Change ``blocks_per_voting period`` in context with ``cycles_per_voting_period`` (MR :gl:`!4456`)

Michelson
---------

- Some operations are now forbidden in views: ``CREATE_CONTRACT``,
  ``SET_DELEGATE`` and ``TRANSFER_TOKENS`` cannot be used at the top-level of a
  view because they are stateful, and ``SELF`` because the entry-point does not
  make sense in a view.
  However, ``CREATE_CONTRACT``, ``SET_DELEGATE`` and ``TRANSFER_TOKENS`` remain
  available in lambdas defined inside a view.
  (MR :gl:`!3737`)

- Stack variable annotations are ignored and not propagated. All contracts that
  used to typecheck correctly before will still typecheck correctly afterwards.
  Though more contracts are accepted as branches with different stack variable
  annotations won't be rejected any more.
  The special annotation ``%@`` of ``PAIR`` has no effect.
  RPCs ``typecheck_code``, ``trace_code``, as well as typechecking errors
  reporting stack types, won't report stack annotations any more.
  In their output encodings, the objects containing the fields ``item`` and
  ``annot`` are replaced with the contents of the field ``item``.
  (MR :gl:`!4139`)

- Variable annotations in pairs are ignored and not propagated.
  (MR :gl:`!4140`)

- Type annotations are ignored and not propagated.
  (MR :gl:`!4141`)

- Field annotations are ignored and not propagated.
  (MR :gl:`!4175`, :gl:`!4311`, :gl:`!4259`)

- Annotating the parameter toplevel constructor to designate the root entrypoint
  is now forbidden. Put the annotation on the parameter type instead.
  E.g. replace ``parameter %a int;`` by ``parameter (int %a);``
  (MR :gl:`!4366`)

- The ``VOTING_POWER`` of a contract is no longer rounded to rolls. It
  is now instead the full staking power of the delegate, currently
  expressed in mutez. Though, developers should not rely on
  ``VOTING_POWER`` to query the staking power of a contract in
  ``mutez``: the value returned by ``VOTING_POWER`` is still of type`
  ``nat`` and it should only be considered relative to
  ``TOTAL_VOTING_POWER``.

- The new type ``tx_rollup_l2_address`` has been introduced. It is
  used to identify accounts on transaction rollups’ legders. Values of
  type ``tx_rollup_l2_address`` are 20-byte hashes of a BLS
  public keys (with a string notation based of a base58 encoding,
  prefixed with ``tz4``). (MR :gl:`!4431`)

- A new instruction ``MIN_BLOCK_TIME`` has been added. It can be used to
  push the current minimal time between blocks onto the stack. The value is
  obtained from the protocol's ``minimal_block_delay`` constant.
  (MR :gl:`!4471`)

- The existing type ``sapling_transaction`` is renamed
  ``sapling_transaction_deprecated``. Existing onchain contracts are
  automatically converted.

Internal
--------

The following changes are not visible to the users but reflect
improvements of the codebase.

- ``BALANCE`` is now passed to the Michelson interpreter as a step constant
  instead of being read from the context each time this instruction is
  executed. (MR :gl:`!3871`)

- Separate ``origination_nonce`` into its own module. (MR :gl:`!3928`)

- Faster gas monad. (MR :gl:`!4034`)

- Simplify cache limits for sampler state. (MR :gl:`!4041`)

- Tenderbrute - bruteforce seeds to obtain desired delegate selections in tests.
  (MR :gl:`!3842`)

- Clean Script_typed_ir_size.mli. (MR :gl:`!4088`)

- Improvements on merge type error flag. (MR :gl:`!3696`)

- Make entrypoint type abstract. (MR :gl:`!3755`)

- Make ``Slot_repr.t`` abstract. (MR :gl:`!4128`)

- Fix injectivity of types. (MR :gl:`!3863`)

- Split ``Ticket_storage`` in two and extract ``Ticket_hash_repr``.
  (MR :gl:`!4190`)

- Carbonated map utility module. (MR :gl:`!3845`)

- Extend carbonated-map with a fold operation. (MR :gl:`!4156`)

- Other internal refactorings or documentation. (MRs :gl:`!4276`, `!4385`, `!4457`)
