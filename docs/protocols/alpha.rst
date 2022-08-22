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

This protocol requires a different protocol environment version than Kathmandu.
It requires protocol environment V7, compared to V6 for Kathmandu.
(MR :gl:`!5906`)

- Simplify the protocol-environment build process by including
  camlInternalFormatBasic — which is not intended for use by developers.
  (MR :gl:`!5910`)

- Abstract public_parameters type in Plonk. (MR :gl:`!6077`)

- Add option monad syntaxes and Update Lwt. (MR :gl:`!6040`)

- Introduce an Array module, making a subset of Stdlib.Array available to the
  protocol (MR :gl:`!6042`)


Smart Contract Optimistic Rollups (ongoing)
-------------------------------------------

Rollups supporting execution of smart contracts. (MRs :gl:`!5603`, :gl:`!5606`,
:gl:`!5447`, :gl:`!5655`, :gl:`!5660`, :gl:`!5680`, :gl:`!5598`, :gl:`!5677`,
:gl:`!5646`, :gl:`!5686`, :gl:`!5693`, :gl:`!5623`, :gl:`!5687`, :gl:`!5714`,
:gl:`!5689`, :gl:`!5708`, :gl:`!5565`, :gl:`!5561`, :gl:`!5567`, :gl:`!5332`,
:gl:`!5628`, :gl:`!5754`, :gl:`!5736`, :gl:`!5784`, :gl:`!5539`, :gl:`!5764`,
:gl:`!5690`, :gl:`!5826`, :gl:`!5812`, :gl:`!5814`, :gl:`!5829`, :gl:`!5813`,
:gl:`!5846`, :gl:`!5654`, :gl:`!5761`, :gl:`!5688`, :gl:`!5889`, :gl:`!5859`,
:gl:`!5882`, :gl:`!5811`, :gl:`!5898`, :gl:`!5925`, :gl:`!5909`, :gl:`!5902`,
:gl:`!5888`, :gl:`!5893`, :gl:`!5884`, :gl:`!5955`, :gl:`!5692`, :gl:`!5887`,
:gl:`!5900`, :gl:`!6014`, :gl:`!6009`, :gl:`!6015`, :gl:`!6019`, :gl:`!6012`,
:gl:`!5851`, :gl:`!5985`, :gl:`!5984`, :gl:`!6037`, :gl:`!5987`, :gl:`!5878`,
:gl:`!6050`, :gl:`!6030`, :gl:`!6060`, :gl:`!5891`, :gl:`!6071`, :gl:`!5926`,
:gl:`!6104`)

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MRs :gl:`!5711`, :gl:`!5938`,
:gl:`!6024`, :gl:`!5959`)

Breaking Changes
----------------

- Rename the parameter ``tokens_per_roll`` to ``minimal_stake``. (MR :gl:`!5897`)

RPC Changes
-----------

- The ``run_operation`` RPC description has been updated to indicate
  that the RPC does not support consensus operations. It could already
  give inconsistent results on such operations, which was not
  documented. It now returns on error when called on a consensus
  operation. (MR :gl:`!5707`)

- New RPC ``/chains/<chain_id>/blocks/<block>/context/constants/parametric``
  returning the value of parametric economic protocol constants. (MR :gl:`!5867`)

Operation receipts
------------------

Bug Fixes
---------

Minor Changes
-------------

- Split internal transactions. (MR :gl:`!5585`)

- Rename error ``Previously_revealed_nonce`` in
  ``Already_revealed_nonce`` (MR :gl:`!5849`)

- New error ``Conflicting_activation`` (MR :gl:`!5849`)

- New error ``Already_denounced`` replace ``Unrequired_denunciation``
  (MR :gl:`!5849`)

- New error ``Conflicting_denunciation`` (MR
  :gl:`!5849`)

- New error ``Conflicting_nonce_revelation`` (MR
  :gl:`!5849`)

- New errors in voting operations. (MR :gl:`!6046`)

- Patch smart contracts containing deprecated annotations. (MR :gl:`!5752`)

- Errors related to consensus operations have been reworked. See
  ``Validate_errors.Consensus``. (MR :gl:`!5927`)

- A delegate can no longer propose the same protocol hash multiple
  times in Proposals operations. An operation that contains a proposal
  which has already been proposed by the delegate in the same voting
  period will now fail (and so will an operation that contains
  multiple occurrences of the same proposal). This prevents the replay
  of a Proposals operation.  (MR :gl:`!5828`)

- Change the names and types of errors related to voting operations
  (Proposals and Ballot), and move them to ``Validate_errors``.
  (MR :gl:`!5828`)


Internal
--------

- Update migration for Kathmandu. (MR :gl:`!5837`)

- Get rid of unparsing_mode. (MR :gl:`!5738`)

- Rename internal operation definitions. (MR :gl:`!5737`)

- Remove Coq attributes. (MR :gl:`!5735`)

- Internal refactorings in Michelson typechecker and interpreter. (MRs
  :gl:`!5586`, :gl:`!5587`, :gl:`!5803`, :gl:`!5804`, :gl:`!5809`, :gl:`!5942`,
  :gl:`!5625`)

- Ensure payer is an implicit account. (MR :gl:`!5850`)

- Derive LB subsidy amount from other constants. (MR :gl:`!5875`)

- Refactor the ``run_operation`` RPC. This allowed us to remove a
  function from ``Validate_operation.TMP_for_plugin`` and to no longer
  expose ``apply_contents_list`` and ``apply_manager_operations`` in
  ``apply.mli``. (MR :gl:`!5770`)

- Rename the function ``Big_map.list_values`` to ``list_key_values`` and make
  it return a list of key-value pairs. Also change the name of the signature
  ``Non_iterable_indexed_carbonated_data_storage_with_values`` to
  ``Indexed_carbonated_data_storage``. (MR :gl:`!3491`)

- Move the checks part of anonymous operation to
  ``validate_operation.ml``. The effects part remains in
  ``apply_operation``. (MR :gl:`!5849`)

- split ``check_vdf_and_update_seed`` function from
  ``seed_storage.ml`` between the checks part, ``check_vdf``, and the
  application part, ``update_seed``. (MR :gl:`!5849`)

- Move the checks part of consensus operation to
  ``validate_operation.ml``. The effects part remains in
  ``apply_operation``. (MR :gl:`!5927`)

- Implement ``Validate_operation.validate_operation`` on voting
  operations (Proposals and Ballot). The checks are now done there,
  while ``Apply.apply_operation`` only applies the effects.
  (MR :gl:`!5828`)

- A Testnet Dictator Proposals operation is now mutually exclusive
  with any other voting operation inside a same block or mempool.
  (MR :gl:`!5828`)

- Remove redundant ``Delegate_storage.pubkey`` and use directly
  ``Contract_manager_storage.get_manager_key`` instead. In situations
  where the later used to fail with ``Unregistered_delegate``, we now
  get either ``Missing_manager_contract`` or
  ``Unrevealed_manager_key``, which describe the issue more
  precisely. (MR :gl:`!5828`)
