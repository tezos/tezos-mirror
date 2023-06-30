Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Oxford (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------

Smart Rollups
-------------

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Adaptive Inflation (ongoing)
----------------------------

Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

- The new ``current_voting_power`` RPC computes the voting power of a
  delegate based on its current stake (as opposed to reading it from
  the vote listings as the ``voting_power`` does) (MR :gl:`!9329`)

Operation receipts
------------------

Bug Fixes
---------

Minor Changes
-------------

- Rename ``endorsement`` into ``attestation`` in protocol errors (MR :gl:`!9192`)

Internal
--------

- Transaction rollup: removed left parameters (:gl:`!8700`)
