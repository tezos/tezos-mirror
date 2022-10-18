Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Lima (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Tezos.

.. contents::

New Environment Version (V8)
----------------------------

This protocol requires a different protocol environment version than Lima.
It requires protocol environment V8, compared to V7 for Lima. (MR :gl:`!6439`)

Smart Contract Optimistic Rollups (ongoing)
-------------------------------------------

Rollups supporting execution of smart contracts. (MRs :gl:`!6118`, :gl:`!6425`,
:gl:`!6252`, :gl:`!6396`)

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MR :gl:`!6174`)

Breaking Changes
----------------

RPC Changes
-----------

- Add RPC to get contract's balance of ticket with specified ticketer, content type, and content:
  ``POST /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/ticket_balance``. (MR :gl:`!6488`)

Operation receipts
------------------

Bug Fixes
---------

Minor Changes
-------------

Internal
--------

- Introduce local context access APIs to the indexed subcontext for optimized accesses with locality. (MR :gl:`!5922`)

- Optimized cleaning of implicit contract with 0 balance using local context accesses (MR :gl:`!5922`)

- Improve ex_ticket_size. (MR :gl:`!6209`)
