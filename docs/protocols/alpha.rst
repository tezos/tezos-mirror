Protocol Alpha
==============

This page lists the changes brought by protocol Alpha with respect
to Rio (see :ref:`naming_convention`).
For the list of changes brought by Rio with respect to Quebec, see :doc:`../protocols/022_rio`.

For a higher-level overview of the most salient new features see the
`announcement blog <https://research-development.nomadic-labs.com/blog.html>`__.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez and the full documentation in :doc:`this page <../alpha/index>`.

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


Client
------

- Added ``octez-client set companion key for <delegate> to <bls_key>``, setting a
  companion key for the given delegate. (MR :gl:`!17320`)

Smart Rollups
-------------

Data Availability Layer
-----------------------

- Receipt of the DAL rewards is conditioned by the receipt of the attestation
  rewards. (MR :gl:`!17934`)


Adaptive Issuance
-----------------

Michelson
---------

- A new instruction named ``IS_IMPLICIT_ACCOUNT`` has been added, it
  provides a conversion from type ``address`` to type
  ``key_hash``. (MR :gl:`!12436`)

Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

- Added ``GET
  /chains/<chain_id>/blocks/<block_id>/delegate/<delegate_pkh>/companion_key``,
  which returns the active companion key of the given delegate. Returns ``none``
  if key is not yet set. (MR :gl:`!17236`)

- Updated ``GET /chains/<chain_id>/blocks/<block_id>/delegate/<delegate_pkh>/``
  with the new field ``companion_key`` which returns the active companion key of
  a given delegate (``none`` if not yet set), and the list of pending updates to
  that key in the following cycles. (MR :gl:`!17236`)

- Updated ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/validators?delegate=<delegate_pkh>&level=<level_id>``
  with the new field ``companion_key`` which returns the active companion key for
  the given ``delegate`` and ``level``. (MR :gl:`!17703`)

Operation receipts
------------------

- Adds receipt for ``companion_key`` update. The receipt is similar to a consensus
  key update, with the ``kind`` field used to differientiate between both.
  (MR :gl:`!17320`)


Errors
------


Protocol parameters
-------------------

- Reduced the nonce revelation period from 960 levels to 300 levels, roughly
  matching the reduction of the cycle length in Rio. Reduced accordingly the VDF
  difficulty as well. (:gl:`!18003`)

Bug Fixes
---------

Minor Changes
-------------

- Allow ``finalise_unstake`` to be performed by any account (:gl:`!17950`). This allows finalisation
  of unstake requests to be done automatically by a third party - for example a finalisation bot.

- Consensus operations with different slots are no longer considered a
  punishable misbehaviour (MR :gl:`!18043`)

Internal
--------

- Added ``octez_riscv_pvm`` as a dependency for the protocol environment (:gl:`!15918`)

- Added ``companion_key`` for delegates. These keys are used to build attestations for tz4
  addresses, and are registered the same way as consensus keys. They are used at validation
  time for the consensus, so the type of the storage ``delegate_sampler_state`` had to be
  updated. (MR :gl:`!17213`)

- Moved remaining RPC implementations to the protocol plugin. (:gl:`!14079`)
