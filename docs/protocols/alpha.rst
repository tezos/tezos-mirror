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

- Reduce cost for internal transaction to smart rollup (MR :gl:`!9284`)

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

- Three new variants of the ``voting_power`` RPC (which returns the
  voting power of a delegate based on the stake it had when voting
  snapshot was taken) have been added:

  - ``current_voting_power`` the voting power of a delegate based on
    its current stake (MR :gl:`!9329`)

  - ``baking_power`` computes the baking power of a delegate based on
     the stake snapshot selected for the current cycle (MR
     :gl:`!9350`)

  - ``current_baking_power`` computes the baking power of a delegate
    based on its current stake (MR :gl:`!9350`)


Operation receipts
------------------

Bug Fixes
---------

- Fix the JSON field ``kind`` of the smart rollup preveal
  encoding. This constant field was wrongfully set for the
  ``metadata`` and ``request_dal_page`` case. (MR :gl:`!9307`)

Minor Changes
-------------

- Rename ``endorsement`` into ``attestation`` in protocol errors (MR :gl:`!9192`)

Internal
--------

- Transaction rollup: removed left parameters (:gl:`!8700`)
