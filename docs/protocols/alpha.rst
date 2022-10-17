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
:gl:`!6252`, :gl:`!6396`, :gl:`!6364`, :gl:`!6413`, :gl:`!6545`, :gl:`!6543`)

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MRs :gl:`!6174`, :gl:`!6388`,
:gl:`!6527`, :gl:`!6505`)

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

- Give a positive gas cost to the ``BALANCE`` instruction. (MR :gl:`!6564`)

Internal
--------

- Introduce local context access APIs to the indexed subcontext for optimized accesses with locality. (MR :gl:`!5922`)

- Optimized cleaning of implicit contract with 0 balance using local context accesses (MR :gl:`!5922`)

- Improve ex_ticket_size. (MR :gl:`!6209`)

- Clean up validation code. (MR :gl:`!6526`)

- Remove ``Script_typed_ir.comparable_option_t``. (MR :gl:`!6513`)
