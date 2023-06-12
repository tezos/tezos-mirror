Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Nairobi (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------

This protocol requires a different protocol environment version than Nairobi.
It requires protocol environment V10, compared to V9 for Nairobi.

- Simplify the timelock ``opening_result`` type in the environment as we do not deal with ``Bogus_cipher`` any longer. (MR :gl:`!8404`)


Smart Rollups
-------------

- Add the support for bootstrapped smart rollups in storage initialization,
  similarly to bootstrapped accounts and smart contracts. (MR :gl:`!8552`)

- Remove the origination proof from the smart rollups’ origination operation.
  (MR :gl:`!8817`)

- The field ``commitment`` in the operation ``Sc_rollup_cement`` is now removed.
  It was no longer used and was deprecated in Nairobi. This also mean that the
  commitment does not need to be provided in the client command. (MR :gl:`!8850`)

  Before::
    ./octez-client cement commitment <commitment hash> from <src> for smart rollup <smart rollup address>

  Now::
    ./octez-client cement commitment from <src> for smart rollup <smart rollup address>

- Enable the latest version of the WASM PVM (``2.0.0-r2``). Existing smart
  rollups will see their PVM automatically upgrade, and newly originated smart
  rollups will use this version directly (MR :gl:`!9051`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Adaptive Inflation (ongoing)
----------------------------

- This protocol asks the bakers to set their votes for the adoption of
  the adaptive inflation feature. They may use the per-block votes
  file, or CLI option ``--adaptive-inflation-vote``. If they do
  not vote for the adaptive inflation feature, the vote defaults to
  "pass" (unlike for the liquidity baking vote, which is mandatory).

- Introduce feature flag for Adaptive Inflation. (MR :gl:`!8566`)

- Add parameter ``staking_over_baking_limit`` as the limit of co-staked tokens over the baked tokens for a given baker. (MR :gl:`!8744`)

- Add parameter ``max_costaking_baker_count`` to limit the number of bakers an account can co-stake to. (MR :gl:`!8766`)

When the feature flag is enabled, the following extra changes happen:

- Most rewards (baking rewards, baking bonuses, attestation rewards, revelation
  rewards) are paid on the frozen deposits balance rather than the spendable
  balance. Manager operations fees and denunciation rewards are still paid on
  the spendable balance. (MR :gl:`!8091`)


Gas improvements
----------------

Breaking Changes
----------------

- Operation ``Set_deposits_limit`` removed. (MR :gl:`!8831`)

- Protocol parameter ``ratio_of_frozen_deposits_slashed_per_double_endorsement`` is
  converted from the ratio ``1/5`` into the percentage ``50%`` and renamed to
  ``percentage_of_frozen_deposits_slashed_per_double_endorsement``. (MR :gl:`!8753`)

- Protocol parameter ``double_baking_punishment`` is converted from a fixed
  value of ``640tz`` into the percentage ``11%`` and renamed to
  ``percentage_of_frozen_deposits_slashed_per_double_baking``. (MR :gl:`!8753`)

- Since protocol Ithaca, the ratio of delegated tez over the delegate's frozen deposit
  must be at most 9. Until now, this was ensured by a protocol parameter named
  ``frozen_deposits_percentage`` (whose value is 10%) representing the minimal percentage
  of frozen deposit. We convert it from a percentage to a factor named
  ``delegation_over_baking_limit`` whose value is 9. (MR :gl:`!8884`)

- The frozen deposits are not computed automatically from the baker's total stake
  (own tokens and delegated ones). Hence there are no automatic transfers of the
  baker's spendable balance to their frozen deposits. Bakers need to use the
  ``stake`` pseudo-operation to increase their frozen deposits. (MR :gl:`!8087`)


RPC Changes
-----------

- Split duplicated argument ``pkh`` in RPC ``smart_rollups/smart_rollup/<address>/staker1/<pkh>/staker2/<pkh>/timeout``
  and ``smart_rollups/smart_rollup/<address>/staker1/<pkh>/staker2/<pkh>/timeout_reached`` into ``/staker1/<staker1_pkh>/staker2/<staker2_pkh>``.
  This changes the RPC description but not its use. (MR :gl:`!8339`)

- Update context with new reward parameters. This changes the JSON from the RPC
  ``/chains/main/blocks/head/context/constants``. (MR :gl:`!8657`)


- Remove the RPC for computing smart rollups’ origination proofs
  ``smart_rollups/all/origination_proof``. (MR :gl:`!8817`)

- Add the consensus key's public key to the reponse of the
  ``../context/delegates/<delegate_pkh>/consensus_key`` RPC. (MR :gl:`!8856`)

Operation receipts
------------------

Bug Fixes
---------

Minor Changes
-------------

- Improve the error for implicit account type check. (MR :gl:`!7714`)

- Remove infinite source ``Double_signing_evidence_rewards`` and take reward from the punishment instead. (MR :gl:`!7758`)

- Remove zero tickets from a big map of a mainnet contract during migration. (MR :gl:`!8111`)

- Add a ``Stake`` operation, implemented as an entrypoint of external operations to implicit accounts, for delegates only. (MR :gl:`!8120`)

- Add a Total supply counter in the storage. (MRs :gl:`!8732`, :gl:`!8739`)

Internal
--------

- Fail earlier when a smart rollup commitment is in conflict when cementing.
  (MR :gl:`!8128`)

- split smart rollup origination fct for readibility. (MR :gl:`!8276`)

- Remove the deprecated and unused ``tx_rollup_l2_address`` Michelson
  type. (MR :gl:`!8546`)

- Add an internal represention case for the ``UNIT`` Michelson instruction. (MR :gl:`!8579`)

- Encoding that supports ``endorsement`` kind in JSON are now suffixed with
  ``_with_legacy_attestation_name``. Non legacy encoding supports
  ``attestation`` kind. (MRs :gl:`!8563`, :gl:`!8531`)

- Michelson: remove legacy behaviour related to contract type. (MR :gl:`!5800`)

- Michelson: cleanup legacy annotation management. (MR :gl:`!8208`)

- Michelson: refactor management of metadata in ty smart constructors. (MR :gl:`!8420`)

- Michelson: remove unused deprecated tx_rollup_l2_address type. (MR :gl:`!8546`)

- Rename ``source`` into ``sender``. (MR :gl:`!7373`)

- Improve efficiency of solving the baker PoW challenge. (MR :gl:`!8403`)

- Refactor declarations of ``make_empty_context`` and ``make_empty_tree`` for easier use.
  (MR :gl:`!8550`)

- Move notions of Smart rollup address and various smart rollup hashes types to
  the shell to make them common to all protocols though the environment. (MR
  :gl:`!8562`, MR :gl:`!8625`)

- Refactoring : stake splitted between a frozen part and a delegated part. (MRs :gl:`!8051`, :gl:`!8885`)

- Refactoring : rewards computed as a relative portion of the total amount of tez
  rewarded per minute (about 85tez/min). (MR :gl:`!8657`)

- Introduce the notion of rollups “machine” which can compute the semantics of
  a given rollup, but cannot be used to generate or verify proof. (MR
  :gl:`!8815`)

- Consensus: optimized validation of attestations by maintaining a set
  of forbidden delegates instead of checking through an I/O that the
  delegate has a sufficient frozen deposit. (MR :gl:`!8722`)

- Refactor punishing transfers to be closer to each other. (MR :gl:`!7759`)

- Remove almost all transaction rollup logic from the protocol. (MR :gl:`!8466`)

- Fix encoding names for rewards. (MR :gl:`!8716`)

- Use ``pair`` type instead of ``*``` for Michelson pairs. (MR :gl:`!8720`)

- Add new function ``of_list`` to build a Merkle list. (MR :gl:`!8853`)

- Improve some aspects in the PlonK code. (MR :gl:`!8730`)

- Store a history of percentages of slashed deposits. (MR :gl:`!8828`)

- Renaming the ``endorsement_power`` and ``preendorsement_power`` fields from
  consensus operation receipt to ``consensus_power`` in the non legacy encoding.
  (MR :gl:`!8531`)

- Improve storage cleaning at the end of a refutation game. (MR :gl:`!8881`)

