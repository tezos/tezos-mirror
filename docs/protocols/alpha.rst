Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Quebec (see :ref:`naming_convention`).

For changes brought by Paris with respect to Oxford, see :doc:`../protocols/020_paris`.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

Environment Version
-------------------



Smart Rollups
-------------



Data Availability Layer
-----------------------

Adaptive Issuance
-----------------

- Bumped the
  :ref:`GLOBAL_LIMIT_OF_STAKING_OVER_BAKING<overstaking_alpha>`
  protocol constant from 5 to 9.  (MR :gl:`!14905`)

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

- Lower the number of blocks per cycle (``blocks_per_cycle``) from
  30720 (~2.8 days) to 10800 (~1 day). (MR :gl:`!15196`)

- Update the number of cycles per voting period
  (``cycles_per_voting_period``) from 5 cycles (that is, ~14.2 days
  with old cycle duration) to 14 cycles (~14 days with new cycle
  duration). (MR :gl:`!15196`)

- Lower the number of blocks per cycle (``blocks_per_cycle``) for
  ghostnet to 10800 (~12 hours). (MR :gl:`!15196`)

Bug Fixes
---------

Minor Changes
-------------

- Added a feature flag to enable the aggregation of block attestation lists into
  a single aggregate operation. (MR :gl:!15283)

Internal
--------

- Removed obsolete feature flags and code related to adaptive issuance
  and autostaking. (MR :gl:`!15215`)
