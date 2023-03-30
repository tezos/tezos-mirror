Protocol Lima
=============

This page documents the changes brought by protocol Lima with respect
to Kathmandu (see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_015_PtLimaPt` directory of the
``master`` branch of Tezos.

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

- Introduce a module Q, making a subset of Zarith.Q available to the
  protocol (MR :gl:`!6092`)

- Generalise the Bounded module to support more datatypes. (MR :gl:`!6076`)

- Remove PVSS from lib_crypto and protocol environments. (MR :gl:`!6183`)

- Adapt to Resto 0.10. (MR :gl:`!6085`)

- Introduce a ``Bls`` module to gather both high and low level functions
  for BLS cryptography. (MR :gl:`!6295`)

- Introduce a new protocol API adapted to pipelined validation.
  (MR :gl:`!6335`)

- Add missing list functions. (MR :gl:`!6316`)

Consensus key
-------------

The "consensus key" feature allows bakers to use a different key,
called the *consensus key* for consensus, that is, for baking and for
signing consensus operations (i.e. preendorsements and
endorsements). It also allows them to update this key. The update
becomes active after ``PRESERVED_CYCLES + 1`` cycles. We therefore
distinguish the *active* consensus key and the *pending* consensus
keys. (There can be multiple pending updates.) The active consensus
key is by default the baker's regular key, called its *manager key*,
which cannot change.

Two new operations have been added:

  ``Update_consensus_key (<public_key>)``
      This is a manager operation that must be signed by the manager
      key of a baker.  This operation updates the consensus key of the
      baker to ``public_key`` starting from the current cycle plus
      ``PRESERVED_CYCLES + 1``.  A consensus key can only be used by a
      single baker, the operation fails otherwise.

  ``Drain_delegate (<baker_pkh, consensus_pkh, destination_pkh>)``
     This is an operation that must be signed by the active consensus
     key ``consensus_pkh`` of the baker ``baker_pkh``.  This operation
     immediately transfers all the spendable balance of the
     ``baker_pkh``'s implicit account into the ``destination_pkh``
     implicit account. It has no effect on the frozen balance.  This
     operation is included in pass 2 (anonymous operations). So drain
     operations don't compete with regular manager operations for gas
     and block size quota; the :doc:`1M restriction<014_kathmandu>`
     (one-operation-per-manager-per-block) applies to drain operations
     as well, meaning that a drain for a baker and a transfer
     operation from the same baker are in conflict. As an incentive
     for bakers to include drain operations, a fixed fraction of the
     drained baker's spendable balance is transferred as fees to the
     baker that includes the operation, i.e. the maximum between 1tz
     or 1% of the spendable balance.

(Breaking changes) Some existing RPCs have been updated:

- ``/chains/main/blocks/head/metadata``

  The block metadata is extended with the active consensus key of the
  baker and the proposer. The fields ``proposer`` and ``baker`` still
  hold the respective public key hashes of the manager keys of the
  proposer and the baker.

::

  "proposer_consensus_key": "[PUBLIC_KEY_HASH]",
  "baker_consensus_key": "[PUBLIC_KEY_HASH]",

- ``/chains/main/blocks/head/context/delegates/[PUBLIC_KEY_HASH]``

  The delegate data is extended with the active and pending consensus keys.

::

 {"full_balance": "4000000000000",
  "current_frozen_deposits": "200000000000",
  "frozen_deposits": "200000000000",
  "staking_balance": "4000000000000",
  "delegated_contracts": [ "[PUBLIC_KEY_HASH]" ],
  "delegated_balance": "0",
  "deactivated": false,
  "grace_period": 5,
  "voting_power": "4000000000000",
  "active_consensus_key": "[PUBLIC_KEY_HASH]",
  "pending_consensus_keys": [
      { "cycle": 7, "pkh": "[PUBLIC_KEY_HASH]},
      { "cycle": 9, "pkh": "[PUBLIC_KEY_HASH]}
    ]}}


- ``/chains/main/blocks/head/helpers/baking_rights``

  The baking rights RPC now returns both the manager key, required to
  identify the rewarded delegate, and the active consensus key
  required to sign a block. The RPC also accepts a new parameter
  ``consensus_key=<pkh>`` to filter the result by the active consensus
  key.

::

 [{ "level": 2, "delegate": "[PUBLIC_KEY_HASH]",
    "round": 0, "estimated_time": "[TIMESTAMP]",
    "consensus_key": "[PUBLIC_KEY_HASH]" },
  { "level": 2, "delegate": "[PUBLIC_KEY_HASH]",
    "round": 1, "estimated_time": "[TIMESTAMP]",
    "consensus_key": "[PUBLIC_KEY_HASH]" },
  { "level": 2, "delegate": "[PUBLIC_KEY_HASH]",
    "round": 2, "estimated_time": "[TIMESTAMP]",
    "consensus_key": "[PUBLIC_KEY_HASH]" },
  { "level": 2, "delegate": "[PUBLIC_KEY_HASH]",
    "round": 3, "estimated_time": "[TIMESTAMP]",
    "consensus_key": "[PUBLIC_KEY_HASH]" },
  { "level": 2, "delegate": "[PUBLIC_KEY_HASH]",
    "round": 10, "estimated_time": "[TIMESTAMP]",
    "consensus_key": "[PUBLIC_KEY_HASH]" }]

- ``/chains/main/blocks/head/helpers/endorsing_rights``

  The endorsing rights RPC now returns both the manager key, required
  to identify the rewarded delegate, and the active consensus key
  required to sign a block. The RPC also accepts a new parameter
  ``consensus_key=<pkh>`` to filter the result by the active consensus
  key.

::

 [ { "level": 1,
     "delegates":
      [ { "delegate": "[PUBLIC_KEY_HASH]",
          "first_slot": 11, "endorsing_power": 50,
          "consensus_key": "[PUBLIC_KEY_HASH]" },
        { "delegate": "[PUBLIC_KEY_HASH]",
          "first_slot": 4, "endorsing_power": 47,
          "consensus_key": "[PUBLIC_KEY_HASH]" },
        { "delegate": "[PUBLIC_KEY_HASH]",
          "first_slot": 2, "endorsing_power": 46,
          "consensus_key": "[PUBLIC_KEY_HASH]" },
        { "delegate": "[PUBLIC_KEY_HASH]",
          "first_slot": 1, "endorsing_power": 55,
          "consensus_key": "[PUBLIC_KEY_HASH]" },
        { "delegate": "[PUBLIC_KEY_HASH]",
          "first_slot": 0, "endorsing_power": 58,
          "consensus_key": "[PUBLIC_KEY_HASH]" } ] } ]

MRs: :gl:`!5936`, :gl:`!5961`, :gl:`!5970`

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
:gl:`!6104`, :gl:`!6102`, :gl:`!5973`, :gl:`!6132`, :gl:`!6146`, :gl:`!6185`,
:gl:`!6197`, :gl:`!6230`, :gl:`!6237`, :gl:`!6236`, :gl:`!6056`, :gl:`!6186`,
:gl:`!6249`, :gl:`!6182`, :gl:`!6243`, :gl:`!6234`, :gl:`!6254`, :gl:`!6280`,
:gl:`!6250`, :gl:`!6258`, :gl:`!6130`, :gl:`!6305`, :gl:`!6290`, :gl:`!6303`,
:gl:`!6315`, :gl:`!6177`, :gl:`!6294`, :gl:`!6263`, :gl:`!6361`, :gl:`!6278`,
:gl:`!6358`, :gl:`!6231`, :gl:`!6400`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Rollups supporting cryptographic proofs of correct execution. (MRs :gl:`!6044`,
:gl:`!6184`, :gl:`!6045`)

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MRs :gl:`!5711`, :gl:`!5938`,
:gl:`!6024`, :gl:`!5959`, :gl:`!6062`, :gl:`!6210`, :gl:`!6216`, :gl:`!6033`,
:gl:`!6022`, :gl:`!6265`, :gl:`!6266`, :gl:`!6273`, :gl:`!6272`, :gl:`!6328`,
:gl:`!6279`, :gl:`!6348`, :gl:`!6256`, :gl:`!6321`)

Liquidity Baking
------------------

The ``liquidity_baking_sunset_level`` parameter has been removed since the subsidy
can now be shut off with the toggle introduced in Jakarta. (MR :gl:`!6215`)

Breaking Changes
----------------

- Deprecate timelock functionality, that is the ``CHEST_OPEN``
  instruction, in Michelson to prevent origination of new contracts using it. A
  safer version of timelock will come in a future procotol.  (MRs :gl:`!6260`,
  :gl:`!6327`)

- Rename the parameter ``tokens_per_roll`` to ``minimal_stake``. (MR :gl:`!5897`)

- Disallow creation, transfer and storage of tickets with zero amounts.
  ``TICKET`` instruction now returns ``option ticket 'a`` instead of ``ticket 'a``.
  For contracts already originated, their ``TICKET`` instructions are renamed to ``TICKET_DEPRECATED``.
  Note that it is not possible to originate contracts containing ``TICKET_DEPRECATED``
  after the migration.
  (MR :gl:`!5963`)

RPC Changes
-----------

- The ``run_operation`` RPC description has been updated to indicate
  that the RPC does not support consensus operations. It could already
  give inconsistent results on such operations, which was not
  documented. It now returns an error when called on a consensus
  operation. (MR :gl:`!5707`)

- New RPC ``/chains/<chain_id>/blocks/<block>/context/constants/parametric``
  returning the value of parametric economic protocol constants. (MR :gl:`!5867`)

Operation receipts
------------------

- Added ``ticket_updates`` field that represents the increase/decrease of tickets in the storage. (MR :gl:`!6267`)

Bug Fixes
---------

- Ghostnet-specific fixes. (MR :gl:`!6401`)

Minor Changes
-------------

- Split internal transactions. (MR :gl:`!5585`)

- Add a new LAMBDA_REC instruction to create recursive lambda functions. (MRs
  :gl:`!5194`, :gl:`!6144`)

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

- Replace ``acceptable_passes`` by ``acceptable_pass`` that returns an
  optional integer instead of a list of at most one element. (MR
  :gl:`!6092`)

- Removed ``relative_position_within_block``. (MR :gl:`!6092`)

- New function ``compare_operations`` which defines a total ordering
  relation. (MR :gl:`!6092`)

- Removed conflict between proposals/ballots operations and testnet
  dictator proposals. Ballots and proposals become noops
  when applying the block after a testnet dictator enacted a protocol
  change. (MR :gl:`!6313`)

- Add used and paid storage space services/commands. (MR :gl:`!6178`)

- The encoding of the proof argument of the transaction rollup’s
  rejection operation is now opaque, to avoid exposing unnecessary
  details about their implementation. (MR :gl:`!6318`)

- Update gas for L. (MR :gl:`!6519`)

- A new case has been added to the entrypoint encoding, with tag ``5``.
  This corresponds to the ``deposit`` entrypoint used both by TORUs and
  ZKRUs. (MR :gl:`!6045`)

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

- Provide correct bounds for seed availability. (MR :gl:`!4554`)

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

- Rely on the protocol for 1M and precheck all operations. (MR :gl:`!6070`)

- Split the validation from the application of blocks. (MR :gl:`!6152`)

- Expose a new ``Mempool`` mode on the protocol side that offers an
  API allowing a light validation of operations. This as well as
  maintaining a commutative set of operations that may also be
  efficiently merged with another. This enables the implementation of
  a parallelized shell's mempool. (MR :gl:`!6274`)

- Add more functions in Bitset. (MRs :gl:`!6352`, :gl:`!6376`)

- Make Micheline serialization gas accounting consistent by construction.
  (MR :gl:`!6403`)

Invoices
--------

@g.b.fefe rewarded 15000 ꜩ and @Ochem rewarded 10000 ꜩ for code contributions
(Consensus Key feature) included in this protocol. (MR :gl:`!6350`)
