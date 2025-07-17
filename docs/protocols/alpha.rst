Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Seoul (see :ref:`naming_convention`).

For changes brought by Quebec with respect to Paris, see :doc:`../protocols/021_quebec`.

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

- Lowered the number of blocks per cycle (``blocks_per_cycle``) from
  10800 (~1 day) to 1800 (~4 hours) on mainnet (with 8-second minimal
  block time). (MR :gl:`!17583`)

  - On ghostnet where minimal block time is 4s, lowered
    ``blocks_per_cycle`` from 10800 (~12 hours) to 3600 (~4
    hours). (MR :gl:`!17583`)

- In order for the duration of voting periods to stay about the same,
  updated the number of cycles per voting period
  (``cycles_per_voting_period``) on mainnet from 14 cycles (that is,
  ~14 days with old cycle duration) to 84 cycles (~14 days with new
  cycle duration). (MR :gl:`!17583`)

- In order to keep the same duration in days for the activation of
  delegate parameters, updated the number of cycles after which
  submitted delegate parameters take effect
  (``delegate_parameters_activation_delay``) on mainnet from 5 cycles
  (that is, ~5 days with old cycle duration) to 30 cycles (~5 days
  with new cycle duration). (MR :gl:`!17583`)

- Reduced blocks per commitment (``blocks_per_commitment``) to 14 to
  keep the number of nonces per cycle to 128. (:gl:`!17583`)

- Reduced the nonce revelation period (``nonce_revelation_threshold``)
  from 300 blocks to 150 blocks. Reduced accordingly the VDF
  difficulty (``vdf_difficulty``) as well. (:gl:`!17583`)

Bug Fixes
---------

Minor Changes
-------------

Internal
--------
