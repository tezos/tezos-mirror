Protocol Alpha
==============

This page lists the changes brought by protocol Alpha with respect
to Rio (see :ref:`naming_convention`).
For the list of changes brought by Rio with respect to Quebec, see :doc:`../protocols/022_rio`.

For a higher-level overview of the most salient new features see the 
`announcement blog <https://research-development.nomadic-labs.com/blog.html>`__.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez and the full documentation in :doc:`this page <../alpha/index>`..

Environment Version
-------------------

This protocol requires an updated protocol environment version (V15) from R version (V14).

- Environment V15 uses signature V2. This change impacts the way BLS signatures
  are handled. In previous environments that used signature V1, the BLS
  signatures were expected to be produced with the ``Augmented`` cryptographic
  scheme. Starting from V15, they are expected to be produced with the ``Proof
  of possession`` cryptographic scheme. (MR :gl:`!17036`)

Smart Rollups
-------------

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

- Added ``GET
  /chains/<chain_id>/blocks/<block_id>/delegate/<delegate_pkh>/companion_key``,
  which returns the active companion key of the given delegate. Returns ``none``
  if key is not yet set. (MR :gl:`!17236`)

- Updated ``GET /chains/<chain_id>/blocks/<block_id>/delegate/<delegate_pkh>/``
  with the new field ``companion_key`` which returns the active companion key of
  a given delegate (``none`` if not yet set), and the list of pending updates to
  that key in the following cycles. (MR :gl:`!17236`)

Operation receipts
------------------


Errors
------


Protocol parameters
-------------------



Bug Fixes
---------

Minor Changes
-------------

Internal
--------

- Added ``octez_riscv_pvm`` as a dependency for the protocol environment (:gl:`!15918`)

- Added ``companion_key`` for delegates. These keys are used to build attestations for tz4
  addresses, and are registered the same way as consensus keys. They are used at validation
  time for the consensus, so the type of the storage ``delegate_sampler_state`` had to be
  updated. (MR :gl:`!17213`)
