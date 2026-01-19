Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Tallinn (see :ref:`naming_convention`).

For changes brought by Tallinn with respect to Seoul, see :doc:`./023_seoul`.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

Environment Version
-------------------



Smart Rollups
-------------

 - The rollup now validates imported DAL pages using the DAL parameters that
   were active at the time of publication. This aligns rollup validation with
   the protocol rules when DAL parameters change across protocol versions.
   (MR :gl:`!20402`)

Consensus
----------

 - Implemented a new algorithm for the baker selection. The current Alias
   method is used to determine the validator that should bake a block
   for a given level and round. After the feature flag ``swrr_new_baker_lottery_enable``
   is activated, the selection would use SWRR (Smooth Weighted Round Robin),
   which is a deterministic method to distribute the round 0 of all the levels
   for a given cycle. The higher rounds are then using a shifted version of this list.
   This method still remains proportional to the stake of the baker, and aims to
   reduce variability of block distribution, especially for small bakers. (MR :gl:`!20084`)

Data Availability Layer
-----------------------

Adaptive Issuance
-----------------


Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------


Errors
------


Protocol parameters
-------------------

Feature flags
^^^^^^^^^^^^^

- Added ``native_contracts_enable``, that enables enshrined contracts in the
  protocol. The flag is disabled by default on the mainnet. (MR :gl:`!19709`)



Bug Fixes
---------

Minor Changes
-------------

Internal
--------
