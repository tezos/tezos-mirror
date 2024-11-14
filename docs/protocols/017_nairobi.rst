Protocol Nairobi
================

This page documents the changes brought by protocol Nairobi with respect
to Mumbai (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_017_PtNairob` of the ``master``
branch of Octez.

New Environment Version (V9)
----------------------------

This protocol requires a different protocol environment version than Mumbai.
It requires protocol environment V9, compared to V8 for Mumbai. (MR :gl:`!7178`)

Smart Rollups
-------------

- Add a new kind of outbox messages batches allowing to specify the
  type of the transaction to execute on the L1. (MR :gl:`!7941`)

- Add a field ``"kind"`` to the outbox message transaction json
  encoding. (MR :gl:`!7941`)

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
  parameterization of maximum call depth. (MRs :gl:`!7724`, :gl:`!7726`,
  :gl:`!7910`, :gl:`!7912`, :gl:`!8076`, :gl:`!8280`)

- A new kind of internal message informs kernels when a protocol upgrade occurs.
  The ``Protocol_migration`` message is injected by the economic protocol during
  a protocol migration. (MRs :gl:`!7729`, :gl:`!8300`).

- Existing smart rollups can benefit from new protocol improvements. The WASM PVM
  checks if it needs to upgrade to a new revision when it receives a ``Protocol_migration``
  message. (MR :gl:`!7730`)

- Remove the failsafe mechanism in inbox construction, aimed at errors that can
  never happen at begin application, block finalization, and migration. (MR :gl:`!7833`)

- The field ``commitment`` in the operation ``Sc_rollup_cement`` is now deprecated.
  The protocol computes the valid candidate commitment to cement, and cements it.
  The provided ``commitment`` is omitted by the protocol and unchecked with the
  found one. (MR :gl:`!7316`)

- Two new host functions have been added to the WASM PVM:
  ``store_delete_value`` (MR :gl:`!8307`), and ``store_create`` (MR :gl:`!8375`).

- The ``store_get_nth_key`` host function is now deprecated, kernels **should
  not use it** as it is not fully deterministic (MR :gl:`!8458`).

- The stack size limit of the WASM PVM has been significantly increased (MRs
  :gl:`!7748`, :gl:`!8377`).

- Publishing twice the same commitment is no longer allowed. (MR :gl:`!8269`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Rollups supporting cryptographic proofs of correct execution. (MRs :gl:`!7342`)

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MRs :gl:`!7074`, :gl:`!7102`,
:gl:`!7103`, :gl:`!7140`, :gl:`!7182`, :gl:`!7192`, :gl:`!7242`, :gl:`!7315`,
:gl:`!7407`, :gl:`!7566`, :gl:`!7606`, :gl:`!7541`, :gl:`!7779`)

Distribution of rollups data through reveal data channel. (MRs :gl:`!7571`)

Gas improvements
----------------

Gas for signature checking of manager operations is made much more
precise. It is now only consumed when a signature is actually checked
(never for internal operations and only once per operation batch) and
it depends on both the signature scheme and the length of the signed
operation. This change leads to important reductions in the gas cost
of manager operations and to considerable increase in transaction
throughput. (MR :gl:`!7591`)

Breaking Changes
----------------

Some manager operations such as revelations of public keys used to
have constant gas costs. Due to the gas improvements of MR
:gl:`!7591`), their gas cost now depends on the signature scheme and
the length of the operation. For some schemes, the gas cost may even
be larger than in previous protocol versions. In particular, revealing
the public key of an implicit account consumed 1000 gas units
previously, it now has the following gas costs depending on the
signature scheme:

================ ============================
Signature scheme Gas cost of reveal operation
================ ============================
ed25519 (tz1)    166 gas units
secp256k1 (tz2)  152 gas units
p256 (tz3)       1091 gas units
bls (tz4)        1671 gas units
================ ============================

- The operation's result ``Sc_rollup_cement_result`` now have a new field
  ``commitment``, which is the commitment cemented by the application of
  the operation ``Sc_rollup_cement``.  (MR :gl:`!7316`)

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

- Michelson: ``IConst`` constructor renamed into ``IPush``. (MR :gl:`!7954`)

- Refactor and simplify consensus operation validation. (MR :gl:`!7720`)

- Better documentation for the ``Token`` module (MR :gl:`!7609`)

- Update gas model for hashing a skip list cell. (MR :gl:`!7737`)

- Update gas cost for upgrading to librustzcash v5.0.0. (MR :gl:`!7814`)

- Synchronized nodes' mempool are now able to consider early (pre)endorsements. (MR :gl:`!7828`)

- Removed obsolete TORU manager operations. (MR :gl:`!7650`)

- Validate: add preendorsement power and locked round checks during
  block finalization in ``Partial_validation`` mode. (MR :gl:`!7949`)
