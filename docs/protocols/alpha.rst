Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Mumbai (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

New Environment Version (V9)
----------------------------

This protocol requires a different protocol environment version than Mumbai.
It requires protocol environment V9, compared to V8 for Mumbai. (MR :gl:`!7178`)

Smart Rollups
-------------

- Update gas model for decoding output proofs. (MR :gl:`!7116`)

- Improve readability of ``assert_commitment_not_too_far_ahead``.
  (MR :gl:`!7231`)

- Improve readability of ``assert_commitment_is_not_past_curfew``.
  (MR :gl:`!7230`)

- Remove dead code: legacy Internal for Tests signatures (MR :gl:`!7234`)

- Prefer hex over b58check to encode filenames. (MR :gl:`!7181`)

- Code quality improvements. (MR :gl:`!7287`)

- Fix error raised when no commitment can be cemented. (MR :gl:`!7286`)

- Use ``Ticket_transfer`` module in ``sc_rollup_operations``. (MR :gl:`!7438`)

- Refactor WASM PVM to enable breaking changes such as new host functions and
  parameterization of maximum call depth. (MRs :gl:`!7724`, :gl:`!7726`)

- Allow PVMs to upgrade themselves when a new protocol activates. (MRs :gl:`!7729`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Rollups supporting cryptographic proofs of correct execution. (MRs :gl:`!7342`)

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MRs :gl:`!7074`, :gl:`!7102`,
:gl:`!7103`, :gl:`!7140`, :gl:`!7182`, :gl:`!7192`, :gl:`!7242`, :gl:`!7315`,
:gl:`!7407`, :gl:`!7566`, :gl:`!7606`, :gl:`!7541`, :gl:`!7779`)

Distribution of rollups data through reveal data channel. (MRs :gl:`!7571`)

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------

Bug Fixes
---------

- Fix consensus watermark encoding roundtrip. (MR :gl:`!7210`)

Minor Changes
-------------

- Adapt new mempool with proto add_operation. (MR :gl:`!6749`)

- Relax (pre)endorsements branch condition and allow denunciations of
  a same endorsement on different branches. (MR :gl:`!7828`)

- Relax (pre)endorsement checks during mempool validation. The mempool
  is now able to propagate (pre)endorsements for blocks in the near
  past or future, and from close cousin branches. Notably, the
  preendorsements that the baker is able to inject as soon as a block
  has been validated (without waiting for its full application) can
  now be immediately propagated by the mempool, allowing for a much
  faster PQC. (MR :gl:`!7815`)

- The mempool now accepts and propagates consensus operations with a
  non-minimal slot (for performance reasons: testing the minimality of
  the slot there is too costly). Such operations are still invalid in
  blocks. To avoid mempools getting spammed with operations with
  various slots, double (pre)endorsement denunciations can now punish
  multiple operations from the same delegate with distinct slots.
  (MR :gl:`!7927`)

Internal
--------

- Update migration for Mumbai. (MR :gl:`!7428`)

- Michelson: add a forgotten tailcall annotation (MR :gl:`!7656`)

- Michelson: the Michelson type "or", previously referred to as ``union`` internally,
  is now referred to as ``or`` if there is no clash with the OCaml keyword "or".
  Otherwise it is referred to as ``or_``. (MR :gl:`!7546`)

- Michelson: normalize all lambdas into optimized mode during elaboration. (MR :gl:`!7829`)

- Refactor and simplify consensus operation validation. (MR :gl:`!7720`)

- Better documentation for the ``Token`` module (MR :gl:`!7609`)

- Update gas model for hashing a skip list cell. (MR :gl:`!7737`)

- Update gas cost for upgrading to librustzcash v5.0.0. (MR :gl:`!7814`)

- Synchronized nodes' mempool are now able to consider early (pre)endorsements. (MR :gl:`!7828`)
