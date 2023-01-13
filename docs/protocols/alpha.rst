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

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MRs :gl:`!7074`, :gl:`!7102`,
:gl:`!7103`, :gl:`!7140`, :gl:`!7182`, :gl:`!7192`, :gl:`!7242`)

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

Internal
--------
