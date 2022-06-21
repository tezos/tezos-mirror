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

- Reveal operations can only occur at the head of a manager operation
  batch (MR :gl:`!5182`).

- Operations with non-deserializable scripts may now be propagated and
  included in blocks. If such an operation is in a block, its
  application will fail so the operation will have no effect, but its
  fees will still be taken. (MR :gl:`!5506`)

- The one-operation-per-manager-per-block restriction (1M) is now
  enforced in blocks. It was previously (optionally) enforced by the
  prevalidator using the plugin mempool filters. This meant that a
  baker could still include several operations from the same manager
  in its own block. This is no longer possible: the application of a
  block containing more than one operation from the same manager will
  now fail. (MR :gl:`!5557`)

RPC Changes
-----------

- Add a new RPC for querying data found on the voting listings for a
  delegate, i.e. voting power, casted ballots and proposals in the
  current voting period.  (MR :gl:`!4577`)

  ``/chains/<chain_id>/blocks/<block>/context/delegates/<delegate_pkh>/voting_info``

- Add a new RPC to execute contracts' views offchain. (MR :gl:`!4810`)

  ``/chains/<chain_id>/blocks/<block>/helpers/scripts/run_script_view``

- Deprecate the ``endorsing_rights`` RPC for whole cycles, by deprecating the ``cycle`` parameter. (:gl:`!5082`)

- Some contract RPCs working on originated contracts only may return a different
  error than before on implicit accounts. (MR :gl:`!5373`)

Operation receipts
------------------

- Remove field ``consumed_gas``, deprecated in Jakarta. Use field ``consumed_milligas`` instead. (:gl:`!5536`)

- Operations that are both manager operations and internal operations returned by Michelson scripts now have different names for receipt encodings. This concerns transations, originations and delegations, where the word "internal" explicitly appears in the case of internal operation receipts. (:gl:`!5149`)

Bug Fixes
---------

- Restore *all-or-nothing* semantics of manager operation batches by
  enforcing that failing reveal operations do not take effect (MR
  :gl:`!5182`).

- Consume constant gas `Michelson_v1_gas.Cost_of.manager_operation`
  during precheck: this fixes some cases of operations passing
  precheck even though they obviously do not have enough gas to apply
  the external operation, e.g. when `gas_limit = 0`. (MR :gl:`!5506`)

- Emptying an implicit account does not cost extra-gas anymore. (MR
  :gl:`!5566`)

- The ``helpers/scripts/run_operation`` RPC now checks whether all
  operations in a batch have the same source. (MR :gl:`!5557`)

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

- Rename `run_view` into `run_tzip4_view` for consistency with
  `run_script_view`. Does not affect the existing `run_view` RPC.
  (MR :gl:`!4810`)

- Precheck no longer returns the gas it has consumed. Instead of
  "replaying" the gas from precheck, `apply_manager_contents` consumes
  the same gas again step by step. (MR :gl:`!5506`)

- Precheck no longer tries to deserialize scripts. It does still check
  that the operation has enough gas for these deserializations (by
  consuming an estimated gas cost based on the bytes size: this has
  not changed). (MR :gl:`!5506`)

- Split precheck into two parts: checks and effects. The checks part
  is effect-free. The effects part consists of the modifications of
  the context that happen regardless of whether the application of the
  operation succeeds: take the fees, increment the account's counter,
  and remove the operation's gas limit from the available block
  gas. The checks part must ensure that the effects part cannot
  fail. (MR :gl:`!5557`)

- Move the checks part of precheck (see above) to a new file
  ``validate_operation.ml``. The effects part remains in
  ``apply_operation`` and is renamed to ``take_fees``. The new
  ``Validate_operation.validate_operation`` function is called before
  ``Apply.apply_operation`` in ``Main``. It stores its own state in
  ``Main.validation_state`` and works with the context from the
  beginning of the block (which is fine thanks to the 1M restriction).
  For now, ``validate_operation`` does nothing for non-manager
  operations, but we plan to extend it to all operations in the
  future. (MR :gl:`!5557`)

- Remove ``Main.check_manager_signature``. Instead,
  ``Main.precheck_manager`` now takes an additional argument that
  indicates whether it should check the signature. (MR :gl:`!5557`)

- Add a type ``Validate_operation.stamp`` in order to guarantee that
  an operation is always validated before it is applied. Indeed, a
  value of this type may only be created in ``Validate_operation``,
  and is required by ``Apply.apply_operation`` and a few other
  functions in ``Apply``. (MR :gl:`!5557`)
