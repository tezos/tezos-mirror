Protocol Alpha
==============

This page contains all the relevant information for protocol Alpha
(see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_alpha` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Alpha with respect
to Kathmandu.

.. contents::

New Environment Version (V7)
----------------------------

This protocol requires a different protocol environment than Kathmandu.
It requires protocol environment V7, compared to V6 for Kathmandu.

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------

Increase_paid_storage
---------------------

- Increase_paid_storage is a new operation that enable a payer to increase the paid storage of a smart contract by some bytes amount. (MR :gl:`!5605`)

Bug Fixes
---------

- Fix a discrepancy in gas consumption of contract origination between
  dry run and actual application (MR :gl:`!5659`)

- Fixes the ``delegated_balance`` rpc, which reported an incorrect value for delegates that have frozen bonds (MR :gl:`!5765`)

Minor Changes
-------------

Internal
--------
