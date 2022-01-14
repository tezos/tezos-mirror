Protocol Alpha
==============

This page contains all the relevant information for protocol Alpha
(see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_alpha` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Alpha with respect
to Protocol I.

.. contents::

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

Internal
--------

The following changes are not visible to the users but reflect
improvements of the codebase.

- ``BALANCE`` is now passed to the Michelson interpreter as a step constant
  instead of being read from the context each time this instruction is
  executed. (MR :gl:`!3871`)
