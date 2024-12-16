Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Quebec (see :ref:`naming_convention`).

For changes brought by Paris with respect to Oxford, see :doc:`../protocols/020_paris`.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

Environment Version
-------------------

This protocol requires a different protocol environment version than Quebec.
It requires protocol environment V14, compared to V13 for Quebec.


Smart Rollups
-------------



Data Availability Layer
-----------------------

Adaptive Issuance
-----------------

Gas improvements
----------------

- Increase gas cost for transfers to implicit accounts by 2000 gas
  units. (MR :gl:`!15993`)

Breaking Changes
----------------

RPC Changes
-----------

- Added RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/consecutive_round_zero``,
  which returns the number of blocks consecutively baked at round
  zero. (MR :gl:`!15945`)

Operation receipts
------------------


Errors
------

- tz4 (BLS) addresses are not forbidden to be registered as delegate and or as
  consensus keys if the ``allow_tz4_delegate_enable`` feature flag is set. (MR
  :gl:`!15302`)

Protocol parameters
-------------------

- Lower the number of blocks per cycle (``blocks_per_cycle``) from
  30720 (~2.8 days) to 10800 (~1 day). (MR :gl:`!15196`)

- Update the number of cycles per voting period
  (``cycles_per_voting_period``) from 5 cycles (that is, ~14.2 days
  with old cycle duration) to 14 cycles (~14 days with new cycle
  duration). (MR :gl:`!15196`)

- Make ``tolerated_inactivity_period`` a protocol constant, and lower
  from 3 cycles (~8.5 days with old cycle duration) to 1 cycle (~1 day
  with new cycle duration). (MRs :gl:`!15390`, :gl:`!15199`)

- Lower the number of blocks per cycle (``blocks_per_cycle``) for
  ghostnet to 10800 (~12 hours). (MR :gl:`!15196`)

Bug Fixes
---------

Minor Changes
-------------

- Added a feature flag to enable the aggregation of block attestation lists into
  a single aggregate operation. (MR :gl:!15283)

- Added a feature flag which would allow tz4 (BLS) addresses as delegate and or
  as consensus keys. (MR :gl:`!15311`)

- Added a feature flag for allowing all bakers to attest. (MR :gl:`!15584`, :gl:`!15764`)

Internal
--------

- Removed obsolete feature flags and code related to adaptive issuance
  activation, auto-staking, and old slashing. (MRs :gl:`!15215`,
  :gl:`!15223`, :gl:`!15211`)
