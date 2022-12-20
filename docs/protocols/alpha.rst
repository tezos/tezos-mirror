Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Mumbai (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Tezos.

.. contents::

New Environment Version (V9)
----------------------------

This protocol requires a different protocol environment version than Mumbai.
It requires protocol environment V9, compared to V8 for Mumbai.

Smart Contract Optimistic Rollups
---------------------------------

- Remove redundant ``level_proof`` field. (MR :gl:`!7109`)

- Fix error description. (MR :gl:`!7114`)

- Weaken condition on LCC. (MR :gl:`!6968`)

- Let SENDER return the address of the rollup for outbox message.
  (MR :gl:`!7130`)

- Update gas model for decoding output proofs. (MR :gl:`!7116`)

- Require conflicting commitments hashes to start game. (MR :gl:`!7054`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MRs :gl:`!7074`, :gl:`!7102`,
:gl:`!7103`)

Breaking Changes
----------------

- Disable TORU. (MR :gl:`!7087`)

RPC Changes
-----------

Operation receipts
------------------

Bug Fixes
---------

Minor Changes
-------------

Internal
--------
