Protocol Paris
==============

This page documents the changes brought by protocol Paris with respect
to Oxford (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_019_PtParisA` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------

This protocol requires a different protocol environment version than Oxford.
It requires protocol environment V12, compared to V11 for Oxford.

- Removed ``Z.extract`` and ``Z.extract_signed`` from all inactive protocols environments.

Smart Rollups
-------------

- The unnecessary initial PVM state hash and its associated RPC are removed. (MR :gl:`!11472`)

- Bumped Wasm PVM to V4. (MR :gl:`!11490`)

Data Availability Layer
-----------------------

- Introduced a new optional field ``dal_content`` to consensus
  attestation operations. The existing encoding of consensus
  attestations remains unchanged when this field is not present. A new
  encoding was introduced for the case when the field is present. The
  two cases are distinguished via an encoding tag. The newly
  introduced tag "23" must be used for attestations containing a DAL
  payload. (MR :gl:`!11462`).

- Introduced a new manager operation ``dal_publish_commitment``. This operation
  can be used to send a commitment over the data (to be send) over the DAL
  network. Do note that this operation itself does not contain the data
  themselves. Instead, it contains the slot index, the commitment over the data,
  and a proof regarding the size for those data. The data must be sent using the
  DAL node. For more information on how it works, please read the `DAL
  documentation <https://tezos.gitlab.io/shell/dal.html>`_.

- Optimize the DAL commitment publication operation by memoizing the
  cryptobox. (MR :gl:`!11594`)

- For smart-rollups: Introduced two reveal inputs. One for importing
  the data, called ``dal_page``. Data can be fed to the kernel by page
  of size 4096 bytes. Another reveal input named ``dal_parameters``
  enables kernel to read the DAL parameters. This should ease the
  writing of smart rollups kernels to make them generic over the
  values of those parameters. For more information on how it works,
  please read the `DAL smart rollup integration
  <https://tezos.gitlab.io/alpha/dal_support.html#smart-rollups-integration>`_.


Adaptive Issuance
-----------------

- Active-delegates update and autostaking are now done before staking-rights computation
  at cycle end. (MR :gl:`!11972`)

- The staking balance is now explicitly initialized when a delegate is registered. (MR :gl:`!11197`)

- The issuance reward coeff is now computed only once.
  It used to be computed twice, once for the bonus, assuming a zero bonus, and once afterwards taking the bonus into account. (MR :gl:`!10935`)

- The minimal frozen stake is now checked before applying limits and then re-checked after applying limits and edge. (MR :gl:`!11086`)

- Min/max issuance bounds evolve progressively over the first 6 months after the eventual activation of Adaptive Issuance. (MR :gl:`!11293`)

- The slashing from denunciations is delayed to the end of the cycle ending the denunciation period. (MR :gl:`!11684`, :gl:`!11879`)

- A delegate denounced for double baking or double attesting is now
  always forbidden from baking and attesting in the near future
  (previously, they were only forbidden if recent and incoming slashes
  summed up to at least 51% of their stake). This interdiction is
  lifted once all pending slashes have been applied and the delegate
  has enough frozen deposits to insure their baking rights for the
  next cycle. This will happen automatically
  ``consensus_right_delays`` (which is 2) cycles when rights computed
  right after the slash take effect, or possibly sooner if the
  delegate was overstaked or actively stakes more funds to match their
  previously computed rights. This change aims to protect bakers from
  incurring further penalties if a faulty configuration causes them to
  double bake/attest, by giving them some time to fix it. (MR
  :gl:`!11704`)

- Denunciations are now applied in chronological order of the denounced
  double signing events, instead of chronological order of denunciation
  inclusion. (MR :gl:`!11854`)

- Under the new slashing feature flag, the amount slashed for a double
  attestation or pre-attestation depends on the number of slots owned
  by misbehaving delegates in the exact block this double signing
  event occurred. This greatly diminishes potential losses in cases of
  legitimate accidents that do not threaten the chain, while keeping
  a strong incentive against potential consensus attacks. (MR
  :gl:`!11854`)

- Autostaking now happens only after pending denunciations are applied. (MR :gl:`!11880`)

- Activated per-block vote for Adaptive Issuance. (MR :gl:`!11935`)

- Added a feature flag which would force enabling Adaptive Issuance upon protocol activation. (MR :gl:`!11559`)

- Updated the estimation for Mainnet's total tez supply which would be used while eventually migrating from Oxford to protocol P. (MR :gl:`!11996`)

- Added a ``min_delegated_in_current_cycle`` field to the delegates' information reported via
  ``GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_id>``. (MR :gl:`!12018`)

- Activating new slashing flag. (MR :gl:`!12013`)

Breaking Changes
----------------

- A new ``dal_attestation`` field has been added to the
  ``block_metadata`` indicating the attested slots. The slots being
  attested are the slots that were published ``attestation_lag`` levels
  ago (MRs :gl:`!11903`, :gl:`!12063`) (see `DAL documentation
  <https://tezos.gitlab.io/shell/dal.html>`_ for more context).

-  The protocol no longer relies on stake snapshots to compute rights. Instead:

   * Rights originating from staked tez are computed from the value at the end of the cycle;
   * Rights originating from delegated tez are computing using the minimum value over the cycle. (MR :gl:`!10455`)

- ``Attestation`` is now the default for operations encoding. (MR :gl:`!11861`)

RPC Changes
-----------

- Make ``liquidity_baking_subsidy`` a protocol constant independent of Adaptive Issuance (MR :gl:`!11971`).
  This changes the JSON from the RPC ``/chains/<chain_id>/blocks/<block_id>/context/constants``
  and ``/chains/<chain_id>/blocks/<block_id>/context/issuance/expected_issuance``.

- Add RPC to get contract's estimated own pending slashed amount according to the currently
  available denunciations.
  ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/estimated_own_pending_slashed_amount``. (MR :gl:`!12016`)

- Add RPC to get delegate's estimated shared pending slashed amount according to the
  currently available denunciations.
  ``GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_id>/estimated_shared_pending_slashed_amount``. (MR :gl:`!12016`)

- Extend the delegate info RPC response by adding a new boolean field named ``pending_denunciations``.
  This field is set to true if there are any pending denunciations associated with the
  specified delegate, and set to false otherwise.
  ``GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_id>/``. (MR :gl:`!12042`)

- Added RPC to get ``min_delegated_in_current_cycle`` for a delegate using
  ``GET '/chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_id>/min_delegated_in_current_cycle'``. (MR :gl:`!12018`)

- New RPC to list the pending denunciations of a given delegate. ``GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_id>/denunciations``. (MR :gl:`!11885`)

- Removed RPC related to stake snapshot. ``GET /chains/<chain_id>/blocks/<block_id>/context/selected_snapshot``. (MR :gl:`!11390`)

- Updated the description of delegates' ``frozen_deposits`` queried via
  ``GET '/chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_id>/frozen_deposits'``. (MR :gl:`!12010`)

Protocol parameters
-------------------

- Replaced ``preserved_cycles`` with 3 constants ``consensus_rights_delay``,
  ``blocks_preservation_cycles`` and
  ``delegate_parameters_activation_delay``. (MR :gl:`!11188`, :gl:`!11280`,
  :gl:`!11279`, :gl:`!11220`, :gl:`!11562`, :gl:`!11629`)

- Updated ``consensus_rights_delay`` from ``5`` to ``2``. (MR :gl:`!11568`)

- Set the number of blocks preservation cycles to 1. (MR :gl:`!11325`)

- Set ``liquidity_baking_subsidy`` to 5 tez issued per minute (MR :gl:`!11971`)

- Removed ``blocks_per_stake_snapshot``. (MR :gl:`!11393`)

- Adapted Smart Rollups constants to account for 10s minimal block time. (MR :gl:`!11445`)

- Added feature flag to force AI activation at protocol activation. (MR :gl:`!11559`)

10s Blocks Time (MR :gl:`!11288`)
---------------------------------

Blocks time have been reduced from 15 seconds to 10 seconds. That is, a block
can be produced with a delay of 10 seconds with respect to the previous block,
if both blocks have round 0. This change comes with updating many related
protocol parameters in order to match the reduced blocks time. In particular,
the following quantities are kept the same:

- the minimal time period of a cycle (namely, 2 days, 20 hours, and 16 minutes),
- the length of the nonce revelation period (namely, around 2 hours and 8 minutes)
- the number of nonce commitments per cycle (namely, 128),
- the number of stake snapshots per cycle (namely, 16),
- the maximum rewards per minute (namely 80 tez), and therefore roughly the same inflation,
- the minimal "time to live" of an operation (namely, 1 hour),
- the block gas limit per minute (namely 10400000 gas),
- the ratio between the liquidity baking subsidy and the maximum rewards per block (namely, 1/16).

.. list-table:: Changes to protocol parameters
   :widths: 50 25 25
   :header-rows: 1

   * - Parameter (unit)
     - Old (oxford) value
     - New value
   * - ``minimal_block_delay`` (seconds)
     - ``15``
     - ``10``
   * - ``delay_increment_per_round`` (seconds)
     - ``8``
     - ``5``
   * - ``blocks_per_cycle`` (blocks)
     - ``16384``
     - ``24576``
   * - ``blocks_per_commitment`` (blocks)
     - ``128``
     - ``192``
   * - ``nonce_revelation_threshold`` (blocks)
     - ``512``
     - ``768``
   * - ``blocks_per_stake_snapshot`` (blocks)
     - ``1024``
     - ``1536``
   * - ``max_operations_time_to_live`` (blocks)
     - ``240``
     - ``360``
   * - ``hard_gas_limit_per_block`` (gas unit)
     - ``2600000``
     - ``1733333``


Minor Changes
-------------

- Michelson error traces for elaboration of invalid data was made more
  consistent by adding errors in some cases (BLS12-381 values, Sapling
  transactions, and timelocks). (MR :gl:`!10227`)

- At every level, a delegate may now be slashed for one double baking
  per round, one double attesting per round, and one double
  preattesting per round. Previously, it was at most one double baking
  for the whole level, and one double operation (either attestation or
  preattestation) for the whole level. (MRs :gl:`!11826`, :gl:`!11844`, :gl:`!11898`)

- Added the ``D_Ticket`` Michelson primitives. (MR :gl:`!11599`)

- ``set_deposits_limit`` operation is disabled when autostaking is off. (MR :gl:`!11866`)

- Added the ``D_Ticket`` Michelson primitives. (MR :gl:`!11599`)

Internal
--------

- On top of the 3 new parametric constants ``consensus_rights_delay``,
  ``blocks_preservation_cycles`` and ``delegate_parameters_activation_delay``
  which replace ``preserved_cycles``, we added pseudo-constants that derive from
  them : ``issuance_modification_delay``,
  ``adaptive_issuance_activation_delay``, ``tolerated_inactivity_period``,
  ``consensus_key_activation_delay``, ``slashable_deposits_period``. (MR
  :gl:`!11188`, :gl:`!11280`, :gl:`!11279`, :gl:`!11627`, :gl:`!11629`)

- The staking balance is now explicitly initialized when a delegate is registered. (MR :gl:`!11197`)

- The issuance reward coefficient is now computed only once.
  It used to be computed twice, once for the bonus, assuming a zero bonus, and once afterwards taking the bonus into account. (MR :gl:`!10935`)

- The shell uses LPBL instead of LAFL to trigger history clean-up. LPBL is
  ``blocks_preservation_cycles`` in the past.  (MR :gl:`!11201`)

- Enforced the 2 blocks finality of Tenderbake in the storage. (MR :gl:`!11262`)

- Frozen deposits are cleaned during stitching for ``P``. (MR :gl:`!11341`)

- Removed stake snapshots. (MR :gl:`!11389`, :gl:`!11390`, :gl:`!11392`)

- Moved context's subtree ``staking_balance/current`` to ``staking_balance`` at stiching for protocol P. (MR :gl:`!11391`)

- During the eventual context stitching for protocol P's activation, the ``last_snapshot`` entry will be removed from the context, as it would be no longer needed. (MR :gl:`!11394`)

- sc_rollup parametric constants update consistency is now checked. (MR :gl:`!11555`)

- Changed misbeahviour's repr. (MR :gl:`!11575`, :gl:`!12028`)

- Pending denunciations are cleaned at protocol stitching. (MR :gl:`!11833`)

- Add tooling to devtools to compute total tez supply offline. (MR :gl:`!11978`)
