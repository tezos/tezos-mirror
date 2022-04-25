Protocol Alpha
==============

This page contains all the relevant information for protocol Alpha
(see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_alpha` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Alpha with respect
to Protocol J.

.. contents::

Smart Contract Optimistic Rollups
---------------------------------

Rollups supporting execution of smart contracts. (MRs :gl:`!4933`, :gl:`!4812`)

Breaking Changes
----------------

RPC Changes
-----------

- Add a new RPC for querying data found on the voting listings for a
  delegate, i.e. voting power, casted ballots and proposals in the
  current voting period.  (MR :gl:`!4577`)

  ``/chains/<chain_id>/blocks/<block>/context/delegates/<delegate_pkh>/voting_info``

Bug Fixes
---------

Minor Changes
-------------

Internal
--------

- Make carbonated maps available to the Raw context (MRs :gl:`!4815`, `!4891`)

- Move Michelson representation modules above the Alpha_context abstraction
  barrier. (MR :gl:`!4418`)

- Further cleanup on Tenderbake code. (MR :gl:`!4513`)

- Add Raw_carbonated_map. (MR :gl:`!4815`)

- Other internal refactorings or documentation. (MRs :gl:`!4890`, :gl:`!4721`)
