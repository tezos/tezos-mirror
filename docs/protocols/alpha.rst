Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Oxford (see :ref:`naming_convention`).

For changes brought by Oxford with respect to Nairobi, see :doc:`../protocols/018_oxford`.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------


This protocol requires a different protocol environment version than Oxford.
It requires protocol environment V12, compared to V11 for Oxford.

Smart Rollups
-------------

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Adaptive Issuance (ongoing)
----------------------------

Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------

Protocol parameters
-------------------

- Added ``consensus_rights_delay`` parametric constant. (MR :gl:`!11188`)

Bug Fixes
---------

Minor Changes
-------------

- Michelson error traces for elaboration of invalid data was made more
  consistent by adding errors in some cases (BLS12-381 values, Sapling
  transactions, and timelocks). (MR :gl:`!10227`)

Internal
--------
