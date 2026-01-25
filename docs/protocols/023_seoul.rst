Protocol Seoul
==============

This page lists the changes brought by protocol Seoul with respect
to Rio (see :ref:`naming_convention`).
For the list of changes brought by Rio with respect to Quebec, see :doc:`./022_rio`.

For a higher-level overview of the most salient new features see the
`announcement blog <https://research-development.nomadic-labs.com/seoul-announcement.html>`__.

An overview of breaking changes and deprecations introduced in
protocol Seoul can be found :ref:`here<seoul_breaking_changes>`.

The code is available in directory :src:`src/proto_023_PtSeouLo` of
the ``master`` branch of Octez and the full documentation in
:doc:`this page <../active/index>`.


Environment Version
-------------------

This protocol requires an updated protocol environment version (V15) from R version (V14).

- Environment V15 uses signature V2. This change impacts the way BLS signatures
  are handled. In previous environments that used signature V1, the BLS
  signatures were expected to be produced with the ``Augmented`` cryptographic
  scheme. Starting from V15, they are expected to be produced with the ``Proof
  of possession`` cryptographic scheme. (MR :gl:`!17036`)
- Environment V15 also exposes two new functions: ``partial_op_validation`` and
  ``add_valid_operation`` which are the result of splitting ``add_operation`` in
  two steps.

Smart Rollups
-------------

Data Availability Layer
-----------------------

- DAL rewards are no longer distributed to bakers that are not
  eligible for attestation rewards for the same cycle. (MR
  :gl:`!17934`)


Adaptive Issuance
-----------------

Michelson
---------

- A new instruction named ``IS_IMPLICIT_ACCOUNT`` has been added. It
  provides a conversion from type ``address`` to type
  ``key_hash``. (MR :gl:`!12436`)

Gas improvements
----------------

.. _seoul_changelog_breaking_changes:

Breaking Changes
----------------

- All existing tz4 addresses have been unrevealed. Indeed, starting in
  this protocol, tz4 addresses need a proof of possession provided in
  the reveal operation. This change ensures that existing tz4
  addresses will also have to provide such a proof before sending
  other operations. This does not change anything about the revelation
  of new addresses, or non-tz4 addresses. (MR :gl:`!18078`)
  However, to handle any address it is recommended to update all the tools producing or
  processing reveal operations to a Seoul-compatible version
  (see :ref:`breaking changes <operation_encodings_s>`).

- ``../context/contracts/<pkh>`` result now contains, when called on an implicit
  account, a boolean field ``revealed`` that tells if the public key of the
  manager has been revealed.


Operations
----------

- Added new operation kinds :ref:`Preattestations_aggregate and
  Attestations_aggregate<consensus_operations_aggregate>`. (MRs
  :gl:`!15244`, :gl:`!17485`)

- The ``Reveal`` operation has a new optional ``proof`` field, which
  is only present if the manager key is a :ref:`tz4 (BLS
  key) <tz4_accounts>`. This change requires updating all the tools producing or
  processing reveal operations to a Seoul-compatible version
  (see :ref:`breaking changes <operation_encodings_s>`).
  This change also results in an increase of gas cost
  per reveal of a tz4 public key. (MR :gl:`!18095`)

.. warning::

   Introduction of this new optional field might still lead to breaking changes
   for tool providers see :ref:`breaking changes <operation_encodings_s>`.

- The optional ``proof`` field of the ``Update_consensus_key``
  operation is now required if (and only if) the new consensus key is
  a :ref:`tz4 (BLS key)<tz4_accounts>`. Its encoding now
  exclusively accepts BLS signatures. (MR :gl:`!17360`)

- Added a new manager operation kind ``Update_companion_key``,
  allowing managers to register a :ref:`companion
  key<companion_key>`. (MR :gl:`!17320`)

- In order to enable denunciations of aggregated consensus operations,
  the ``Double_preattestation_evidence`` and
  ``Double_attestation_evidence`` operations have been replaced with a
  new ``Double_consensus_operation_evidence`` operation. This new
  operation contains a denounced slot and two denounced consensus
  operations. For the evidence to be valid, the denounced operations
  must both be preattestations (each one may be aggregated or not) or
  both be attestations. Moreover, both must involve the denounced
  slot, that is, be either a standalone operation for this slot or an
  aggregate whose committee includes this slot. (MR :gl:`!18032`)

- The ``Dal_entrapment_evidence`` operation has a new
  ``consensus_slot`` field, and its ``attestation`` field may now
  contain any kind of consensus operation. For the evidence to be
  valid, ``attestation`` must be either a standalone attestation for
  ``consensus_slot``, or an attestations aggregate whose committee
  includes ``consensus_slot``. (MR :gl:`!18073`)

.. _seoul_receipts_changes:

Operation receipts
------------------

- Reworked the receipts for the
  ``Double_consensus_operation_evidence`` and
  ``Double_baking_evidence`` operations (MR :gl:`!18103`):

  - The ``balance_updates`` field has been removed, because these
    operations no longer produce them since the Oxford protocol.

  - The ``forbidden_delegate`` field has been renamed to
    ``punished_delegate`` and is no longer optional, because
    forbidding the punished delegate has been systematic since the
    Paris protocol.

  - A new ``rewarded_delegate`` field has been added, because it's no
    longer easily retrievable from the balance updates since Oxford.

  - A new ``misbehaviour`` field has been added, containing the
    misbehaviour's level, round, and kind
    (double-baking/double-attesting/double-preattesting).

- Aggregate operations receipts update. Return the consensus power for each
  delegate in the committee (MR :gl:`!18435`)


RPC Changes
-----------
- Modified ``GET
  /chains/<chain_id>/blocks/<block_id>/context/contracts/<pkh>``
  result now contains, when called on an implicit account, a boolean field
  ``revealed`` that tells if the public key of the manager has been revealed.


- Added ``GET
  /chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_pkh>/companion_key``,
  which returns the active companion key of the given delegate. Returns ``none``
  if key is not yet set. (MR :gl:`!17236`)

- Updated ``GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_pkh>/``
  with the new field ``companion_key`` which returns the active companion key of
  a given delegate (``none`` if not yet set), and the list of pending updates to
  that key in the following cycles. (MR :gl:`!17236`)

- Updated ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/validators?delegate=<delegate_pkh>&level=<level_id>``
  with the new optional field ``companion_key``, only present if the
  delegate has an active companion key and this companion key is
  needed for crafting and validating attestations at the queried
  level. (MRs :gl:`!17703`, :gl:`!17957`)

- Added a new RPC ``GET
  /chains/<chain>/blocks/<block>/helpers/total_baking_power`` to retrieve the
  total baking power for the current cycle. (MR :gl:`!17553`)

Errors
------


Protocol parameters
-------------------

- Reduced the nonce revelation period from 960 levels to 300 levels, roughly
  matching the reduction of the cycle length in Rio. Reduced accordingly the VDF
  difficulty as well. (:gl:`!18003`)

- Reduced blocks per commitment to 84, bringing back the number of nonces per
  cycle to 128. (:gl:`!18147`)

Bug Fixes
---------

Minor Changes
-------------

- The :ref:`finalize_unstake
  pseudo-operation<staked_funds_management>` can now be performed
  by any account, not just the owner of the unstaked funds. This
  allows finalization of unstake requests to be done automatically by
  a third party - for example a finalization bot. (MR :gl:`!17950`)

- Consensus operations with non-minimal slots are now filtered by
  mempools (MR :gl:`!18040`).

- Consensus operations with different slots are no longer considered a
  punishable misbehaviour. (MR :gl:`!18043`)

Internal
--------

- Added ``octez_riscv_pvm`` as a dependency for the protocol
  environment. (MR :gl:`!15918`)

- Added ``companion_key`` for delegates. These keys are used to build attestations for tz4
  addresses, and are registered the same way as consensus keys. They are used at validation
  time for the consensus, so the type of the storage ``delegate_sampler_state`` had to be
  updated. (MR :gl:`!17213`)

- Moved remaining RPC implementations to the protocol plugin. (MR :gl:`!14079`)
