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

  
Breaking Changes
----------------

- The binary encoding of the result of the ``Transaction`` operation
  has changed.  Its contents now vary depending on the kind of
  destination. The default cases (implicit and smart contracts) are
  prefixed with the tag ``0``.

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

- Field annotations are ignored and not propagated on comparable types and on
  regular pairs (MR :gl:`!4175`)

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
