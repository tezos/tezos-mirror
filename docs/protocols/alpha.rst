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

Smart Contract Optimistic Rollups (ongoing)
-------------------------------------------

Rollups supporting execution of smart contracts. (MRs :gl:`!5603`, :gl:`!5606`,
:gl:`!5447`, :gl:`!5655`, :gl:`!5660`, :gl:`!5680`, :gl:`!5598`, :gl:`!5677`,
:gl:`!5646`, :gl:`!5686`, :gl:`!5693`, :gl:`!5623`, :gl:`!5687`, :gl:`!5714`,
:gl:`!5689`, :gl:`!5708`, :gl:`!5565`, :gl:`!5561`, :gl:`!5567`, :gl:`!5332`,
:gl:`!5628`, :gl:`!5754`, :gl:`!5736`, :gl:`!5784`)

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MRs :gl:`!5711`)

Breaking Changes
----------------

RPC Changes
-----------

- The ``run_operation`` RPC description has been updated to indicate
  that the RPC does not support consensus operations. It could already
  give inconsistent results on such operations, which was not
  documented. It now returns on error when called on a consensus
  operation. (MR :gl:`!5707`)

Operation receipts
------------------

Bug Fixes
---------

Minor Changes
-------------

- Split internal transactions. (MR :gl:`!5585`)

Internal
--------

- Get rid of unparsing_mode. (MR :gl:`!5738`)

- Rename internal operation definitions. (MR :gl:`!5737`)

- Remove Coq attributes. (MR :gl:`!5735`)

- Internal refactorings in Michelson typechecker and interpreter. (MRs
  :gl:`!5586`, :gl:`!5587`)

- Refactor the ``run_operation`` RPC. This allowed us to remove a
  function from ``Validate_operation.TMP_for_plugin`` and to no longer
  expose ``apply_contents_list`` and ``apply_manager_operations`` in
  ``apply.mli``. (MR :gl:`!5770`)
