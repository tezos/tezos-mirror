.. TODO tezos/tezos#2170: search shifted protocol name/number & adapt

Protocol Alpha
==============

This page lists the changes brought by protocol Alpha with respect
to Tallinn (see :ref:`naming_convention`).
For the list of changes brought by Tallinn with respect to Seoul, see :doc:`./024_tallinn`.

For a higher-level overview of the most salient new features see the
`announcement blog <https://research-development.nomadic-labs.com/blog.html>`__.

An overview of breaking changes and deprecations introduced in
protocol Alpha can be found :ref:`here <alpha_breaking_changes>`. These
changes are also listed below in their respective topical section,
tagged with **Breaking change** or **Deprecation**.

The code is available in directory :src:`src/proto_alpha` of
the ``master`` branch of Octez and the full documentation in
:doc:`this page <../alpha/index>`.

Environment Version
-------------------



Smart Rollups
-------------

 - The rollup now validates imported DAL pages using the DAL parameters that
   were active at the time of publication. This aligns rollup validation with
   the protocol rules when DAL parameters change across protocol versions.
   (MR :gl:`!20402`)

Consensus
----------

 - Implemented a new algorithm for the baker selection. The current Alias
   method is used to determine the validator that should bake a block
   for a given level and round. After the feature flag ``swrr_new_baker_lottery_enable``
   is activated, the selection would use SWRR (Smooth Weighted Round Robin),
   which is a deterministic method to distribute the round 0 of all the levels
   for a given cycle. The higher rounds are then using a shifted version of this list.
   This method still remains proportional to the stake of the baker, and aims to
   reduce variability of block distribution, especially for small bakers. (MR :gl:`!20084`)

Data Availability Layer
-----------------------

- Introduced a new format for the bitset representing baker-attested DAL slots,
  used in the DAL payload of consensus attestation operation, and
  protocol-attested DAL slots, used in a block metadata's field
  ``"dal_attestation"``. The format is described in the header of the file
  ``src/proto_alpha/lib_protocol/dal_attestations_repr.mli``. (MR :gl:`!20734`)
- Increase number of slots to 160. (MR :gl:`!20457`)
- Increase slot size to 380_832 bytes. (MR :gl:`!20457`)

Adaptive Issuance
-----------------


Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------


Errors
------


Protocol parameters
-------------------

Feature flags
^^^^^^^^^^^^^

- Added ``native_contracts_enable``, that enables enshrined contracts in the
  protocol. The flag is disabled by default on the mainnet. (MR :gl:`!19709`)

- Added ``tz5_account_enable``, that enables ``tz5`` ML-DSA-44 account support
  in the protocol. The flag is disabled by default on the mainnet. (MR
  :gl:`!20680`)

Cryptography
------------

- Support for ML-DSA-44 signatures and introduction of a new account
  type whose address has the prefix ``tz5`` (whose keys are ML-DSA-44
  key pairs). In the protocol, ``tz5`` accounts are under a feature
  flag ``tz5_account_enable``, which is disabled by default on the
  mainnet. On testing networks where this feature flag is enabled,
  ``tz5`` accounts cannot be registered as delegates, and ``tz5`` keys
  cannot be used as consensus keys. (MRs :gl:`!20251`, :gl:`!20680`)


Bug Fixes
---------

Minor Changes
-------------

Internal
--------
