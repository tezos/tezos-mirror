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

Minor Changes
-------------

Internal
--------
