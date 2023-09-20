Protocol Oxford
===============

This page documents the changes brought by protocol Oxford with respect
to Nairobi (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_018_Proxford` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------

This protocol requires a different protocol environment version than Nairobi.
It requires protocol environment V10, compared to V9 for Nairobi.

- Simplify the timelock ``opening_result`` type in the environment as we do not deal with ``Bogus_cipher`` any longer. (MR :gl:`!8404`)

- Expose encoding with legacy attestation name. (MR :gl:`!8620`)

- Expose skip list structure. (MR :gl:`!8993`)

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

- Add one new host function to the WASM PVM: ``store_exists`` (MR :gl:`!9204`).

- Remove dead refutation games at migration time. A game is dead if both players
  are no longer staking. (MR :gl:`!8975`)

- Reduce cost for internal transaction to smart rollup (MR :gl:`!9284`)

- Remove dead refutation games at migration time. A game is dead if both players
  are no longer staking. (MR :gl:`!8975`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Partial reveal preimage (MRs :gl:`!9029`, :gl:`!9177`, :gl:`!9291`, :gl:`!9453`)

Data Availability Layer (ongoing)
---------------------------------

Gossipsub (MR :gl:`!9123`)

Node consolidation (MRs :gl:`!9280`, :gl:`!9300`, :gl:`!9320`, :gl:`!9496`)

Adaptive Issuance (ongoing)
----------------------------

- This protocol asks the bakers to set their votes for the adoption of
  the adaptive issuance feature. They may use the per-block votes
  file, or CLI option ``--adaptive-issuance-vote``. If they do
  not vote for the adaptive issuance feature, the vote defaults to
  "pass" (unlike for the liquidity baking vote, which is mandatory).

- Introduce feature flag for Adaptive Issuance. (MR :gl:`!8566`)

- Add parameter ``limit_of_staking_over_baking`` as the limit of co-staked tokens over the baked tokens for a given baker. (MR :gl:`!8744`)

When the feature flag is enabled, the following extra changes happen:

- Most rewards (baking rewards, baking bonuses, attestation rewards, revelation
  rewards) are paid on the frozen deposits balance rather than the spendable
  balance. Manager operations fees and denunciation rewards are still paid on
  the spendable balance. (MR :gl:`!8091`)

- Multiplicative coefficient (with a dynamic part) applied to reward values. (MRs :gl:`!8860`, :gl:`!8861`)

- Denunciation rewards computation updated to depend on ``limit_of_staking_over_baking``. (MR :gl:`!8939`)

- EMA and launch cycle. (MRs :gl:`!8967`, :gl:`!9002`, :gl:`!9025`, :gl:`!9058`, :gl:`!9071`, :gl:`!9074`,
  :gl:`!9087`)

- Staking, deposits and supply. (MRs :gl:`!8940`, :gl:`!8957`, :gl:`!8958`, :gl:`!8965`, :gl:`!8966`, :gl:`!8973`,
  :gl:`!9000`, :gl:`!9014`, :gl:`!9015`, :gl:`!9018`, :gl:`!9022`, :gl:`!9023`, :gl:`!9031`, :gl:`!9033`, :gl:`!9039`,
  :gl:`!9040`, :gl:`!9044`, :gl:`!9052`, :gl:`!9054`, :gl:`!9055`, :gl:`!9069`, :gl:`!9089`, :gl:`!9091`,
  :gl:`!9093`, :gl:`!9241`, :gl:`!9277`, :gl:`!9298`, :gl:`!9299`, :gl:`!9301`, :gl:`!9303`, :gl:`!9304`,
  :gl:`!9312`, :gl:`!9319`, :gl:`!9330`, :gl:`!9351`, :gl:`!9365`, :gl:`!9321`, :gl:`!9367`, :gl:`!9376`,
  :gl:`!9383`, :gl:`!9386`, :gl:`!9387`, :gl:`!9403`, :gl:`!9406`, :gl:`!9407`, :gl:`!9408`, :gl:`!9420`,
  :gl:`!9456`, :gl:`!9482`, :gl:`!9498`, :gl:`!9499`, :gl:`!9511`, :gl:`!9513`, :gl:`!9523`, :gl:`!9531`,
  :gl:`!9533`, :gl:`!9537`, :gl:`!9539`, :gl:`!9543`, :gl:`!9544`, :gl:`!9547`, :gl:`!9564`, :gl:`!9577`,
  :gl:`!9611`)

- New RPCs introduced: total supply, rewards, total frozen stake, inflation, launch cycle, unstaked frozen deposits, unstaked requests, staking parameters
  (MRs :gl:`!8982`, :gl:`!8983`, :gl:`!8995`, :gl:`!8997`, :gl:`!9057`, :gl:`!9243`, :gl:`!9409`, :gl:`!9419`)

Gas improvements
----------------

- Gas model improved for smart rollups origination. (MR :gl:`!9020`)

- Gas parameters related to ``Comb`` instruction updated. (MR :gl:`!9046`)

- More accurate gas model for integer division. (MR :gl:`!8666`)

- Improved gas model for type comparison with ``TY_EQ``. (MR :gl:`!9121`)

- Improved gas model for ``Michelson_v1_gas_costs.cost_N_KMap_enter_body``. (MR :gl:`!9283`)

Breaking Changes
----------------

- Operation ``Set_deposits_limit`` removed. (MR :gl:`!8831`)

- Protocol parameter ``ratio_of_frozen_deposits_slashed_per_double_endorsement``
  is converted from the ratio ``1/5`` into the percentage ``50%`` and renamed to
  ``percentage_of_frozen_deposits_slashed_per_double_attestation``. (MRs
  :gl:`!8753`, :gl:`!9440`)

- Protocol parameter ``double_baking_punishment`` is converted from a fixed
  value of ``640tz`` into the percentage ``11%`` and renamed to
  ``percentage_of_frozen_deposits_slashed_per_double_baking``. (MR :gl:`!8753`)

- Since protocol Ithaca, the ratio of delegated tez over the delegate's frozen deposit
  must be at most 9. Until now, this was ensured by a protocol parameter named
  ``frozen_deposits_percentage`` (whose value is 10%) representing the minimal percentage
  of frozen deposit. We convert it from a percentage to a factor named
  ``limit_of_delegation_over_baking`` whose value is 9. (MR :gl:`!8884`)

- The frozen deposits are not computed automatically from the baker's total stake
  (own tokens and delegated ones). Hence there are no automatic transfers of the
  baker's spendable balance to their frozen deposits. Bakers need to use the
  ``stake`` pseudo-operation to increase their frozen deposits. (MR :gl:`!8087`)

- Receipts involving the ``Deposits`` kind of balance are updated in a
  non-backward-compatible manner. It allows non-delegates, and
  distinguishes updates to a delegate's balance from sharing of rewards
  and punishments. (MR :gl:`!9498`)

- Field ``for_double_endorsing`` from context storage has been renamed into
  ``for_double_attesting``. (MR :gl:`!9486`)

- Field ``endorsing_reward_per_slot`` from rewards storage has been renamed into
  ``attesting_reward_per_slot``. (MR :gl:`!9486`)

- Field ``missed_endorsements`` from contract storage has been renamed into
  ``missed_attestations``. (MR :gl:`!9486`)

- Fields ``preendorsements_seen``, ``endorsements_seen`` and
  ``double_endorsing_evidences_seen`` from the mempool's ``operation_state``
  encoding has been renamed ``preattestations_seen``, ``attestation_seen`` and
  ``double_attesting_evidences_seen``. (MR :gl:`!9440`)

- ``endorsement`` renamed to ``attestation`` in the protocol codebase. (MRs :gl:`!9362`, :gl:`!9364`, :gl:`!9425`)

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

- Three new variants of the ``voting_power`` RPC (which returns the
  voting power of a delegate based on the stake it had when voting
  snapshot was taken) have been added:

  - ``current_voting_power`` the voting power of a delegate based on
    its current stake (MR :gl:`!9329`)

  - ``current_baking_power`` computes the baking power of a delegate
    based on its current stake (MR :gl:`!9350`)

- Two new variants of the ``voting_power`` RPC (which returns the
  ``expected_endorsing_rewards`` field from ``/participation`` RPC has been
  renamed in ``expected_attesting_rewards``.

Operation receipts
------------------

- To handle the new staking pseudo-operations, the following changes
  to receipts have been made:

  - the ``Deposits`` kind of balance, which used to be associated to
    the public key hash of a delegate, has been generalized to handle
    non-delegate staking and sharing of rewards and punishments; it is
    now associated to either a ``Single`` delegator (represented by a
    pair of the delegator address and its delegate public key hash) or
    ``Shared`` between all the delegators of a given delegate in
    proportion to their stake (represented by the public key hash of
    the delegate). (MR :gl:`!9498`)

  - a new ``Unstaked_deposits`` kind of balance has been added to
    represent tez for which unstaking has been requested. This kind of
    balance is associated with the cycle at which the tez become
    liquid and, like in the ``Deposits`` case, it is either associated
    with a ``Single`` delegator or ``Shared`` between a delegate and
    its delegators. (MR :gl:`!9498`)

Bug Fixes
---------

- Fix the JSON field ``kind`` of the smart rollup preveal
  encoding. This constant field was wrongfully set for the
  ``metadata`` and ``request_dal_page`` case. (MR :gl:`!9307`)

Minor Changes
-------------

- Improve the error for implicit account type check. (MR :gl:`!7714`)

- Remove infinite source ``Double_signing_evidence_rewards`` and take reward from the punishment instead. (MR :gl:`!7758`)

- Remove zero tickets from a big map of a mainnet contract during migration. (MR :gl:`!8111`)

- Add a ``Stake`` operation, implemented as an entrypoint of external operations to implicit accounts, for delegates only. (MR :gl:`!8120`)

- Add a Total supply counter in the storage. (MRs :gl:`!8732`, :gl:`!8739`)

- Allow to choose the bootstrapped contracts hashes. (MR :gl:`!9176`)

- Rename ``endorsement`` into ``attestation`` in protocol errors (MR :gl:`!9192`)

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

- Michelson: type of ``cost_UNPARSE_TYPE`` changed to match with the other cost functions in ``michelson_v1_gas_costs.ml``. (MR :gl:`!7529`)

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

- ``version_value`` moved from ``raw_context.ml`` to ``constants_repr.ml``. (MR :gl:`!8867`)

- Transaction rollup: removed left parameters (:gl:`!8700`)

- ``balance_update_encoding`` now output ``attesting rewards`` and ``lost
  attesting rewards`` in JSON.
  ``balance_update_encoding_with_legacy_attestation_name`` has been added and
  output legacy ``endorsing rewards`` and ``lost endorsing rewards``. (MR
  :gl:`!9251`)
