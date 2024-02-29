Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Oxford (see :ref:`naming_convention`).

For changes brought by Oxford with respect to Nairobi, see :doc:`../protocols/018_oxford`.

For changes brought by Paris with respect to Oxford, see :doc:`../protocols/019_paris`.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------

Smart Rollups
-------------

Zero Knowledge Rollups (ongoing)
--------------------------------

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

- To better differentiate Deposits coming from 'rewards from bakers own stakes'
  from 'the edge bakers may take from their stakers rewards', the balance updates
  field has been specialized. The field {"staker":{"baker": <delegate_pkh>}} is now
  split into {"staker":{"baker_own_stake": <delegate_pkh>}} and {"staker":{"baker_edge":
  <delegate_pkh>}}. (MR :gl:`!12258`)

Protocol parameters
-------------------

Bug Fixes
---------

Minor Changes
-------------

Internal
--------
