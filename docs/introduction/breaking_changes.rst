Breaking Changes
================

This section presents the breaking changes that users may encounter
between successive protocols or successive Octez versions. It
complements the respective :ref:`protocol changelogs
<protocol_changelogs>` and :doc:`Octez changelog <../CHANGES>` by
gathering all breaking changes in one place and providing more context
when appropriate. Deprecated features are also presented here.

In the particular case of RPC changes, you may consult complementary
information on :ref:`RPC versioning <rpc_versioning>`, covering how
new versions are introduced, the deprecation policy, and a concrete
calendar of RPCs planned to be removed.

This page contains the breaking changes and deprecated features of
current or recent protocols, protocol proposals, and Octez versions:

- :ref:`tallinn_breaking_changes`
- :ref:`seoul_breaking_changes`
- :ref:`v24_breaking_changes`
- :ref:`v23_breaking_changes`
- :ref:`v22_breaking_changes`

as well as a preview of future breaking changes and deprecated
features in upcoming releases:

- :ref:`alpha_breaking_changes`
- :ref:`octez_dev_breaking_changes`

For features deprecated or broken in older protocols and Octez
versions, see :doc:`past_breaking_changes`.

.. toctree::
   :hidden:

   past_breaking_changes

.. _v24_breaking_changes:

Octez Version 24
----------------

:doc:`Full Octez Version 24 Changelog<../releases/version-24>`

Deprecation of protocol-specific bakers and accusers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Octez v24 deprecates the ``octez-baker-<protocol>`` and
``octez-accuser-<protocol>`` binaries. They will be removed in a
future version. Please use the protocol-independent binaries
``octez-baker`` and ``octez-accuser`` instead, which automatically
handle protocol switches.


Deprecation of Adaptive Issuance vote in the baker
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Octez v24 deprecates the optional argument ``--adaptive-issuance-vote
<vote>`` of the baker. This argument will be removed in a future
version. It was meant to decide the activation of the Adaptive
Issuance feature, and has had no effects since the Paris protocol has
been voted in, so it can already be removed without any impact on the
baker.

The ``adaptive_issuance_vote`` field of the per-block-vote
configuration file is similarly deprecated.


Node events
^^^^^^^^^^^

As of Octez v24, the following node events contain the short hash of
blocks instead of the full hash:

- from the block validator: events
  ``validation_or_application_failed``, ``application_failed``,
  ``application_failure_after_validation``, ``validation_failure``,
  ``validation_canceled``, ``commit_block_failure``,
  ``validated_block``, ``validation_and_application_success`` and
  ``updated_to_checkpoint``,

- from the prevalidator: event ``request_completed_info``,

- and from the store: ``<block_hash> (level: <level>)`` has become
  ``<short_block_hash> (level: <level>)``.

The level of event ``validator.block.validating_block`` has changed
from ``Debug`` to ``Info``, so it now appears in the daily logs by
default. Moreover, this event now shows the long block hash, level,
predecessor, fitness, and timestamp of the block.

The level of event ``validator.chain.block_info`` has changed from
``Info`` to ``Debug``, so it no longer appears in the daily logs by
default.


DAL node RPCs
^^^^^^^^^^^^^

As of Octez v24, the
``/levels/<slot_level>/slots/<slot_index>/status`` RPC answers with
``unpublished`` status for unpublished slots instead of a 404 empty
response.

Slots status are not stored in dedicated files on disk anymore, but
found in a cache and the skip list. A consequence of this is that the
``/levels/<slot_level>/slots/<slot_index>/status`` RPC now only works
with nodes that store the skip list, and therefore not with observer
nodes. Also, the RPC now answers with a 500 error if querying a level
at which the DAL was not supported, instead of a 404 error.



.. _tallinn_breaking_changes:

Protocol Tallinn
----------------

:doc:`Full Protocol Tallinn Changelog<../protocols/024_tallinn>`


6s Block Time
^^^^^^^^^^^^^

Protocol Tallinn reduces the block time from 8 seconds to 6 seconds on
mainnet. That is, a block can be produced with a delay of 6 seconds
with respect to the previous block, if the latter is at round 0.

Multiple protocol and smart rollup parameters have been updated in
consequence, to ensure that their duration in terms of
minutes/hours/weeks remains the same as in protocol Seoul. A full list
of affected parameters with their old and new values can be found
:ref:`here<6s_block_time_parameters_tallinn>`.

Unlike other parameters, the value of parameter
``smart_rollup_max_active_outbox_levels`` remains unchanged in terms
of blocks. This means that the actual duration of the maximal allowed
:ref:`period of withdrawal <withdrawal_period>` has decreased from ~14 days in protocol Seoul to
~10 days in protocol Tallinn.

Consensus changes
^^^^^^^^^^^^^^^^^

With the All Bakers Attest feature implemented in protocol Tallinn come changes to
the consensus. Most of these changes will take effect only when the feature activates,
but in some cases, the format of operations or RPCs have been updated to include
more information.

In particular, the attestation and preattestation receipts metadata have been updated.
The ``consensus_power`` field is now divided in two parts: an integer field ``slots``,
which corresponds to the number of slots attributed to the delegate, and represents
its consensus power until "All Bakers Attest" activates, and an optional string field
``baking_power`` parsed as an int64, which is the baking power of the delegate, and represents its consensus power
once "All Bakers Attest" activates. This last field is not in the receipt until
"All Bakers Attest" activates.

To track the activation status of the "All Bakers Attest" feature, the field
``all_bakers_attest_activation_level`` has been added in the block metadata.
It returns the activation level of the feature if it is set to activate.
The field remains ``null`` otherwise.

Additionally, fields related to the consensus were added in the block metadata:
``attestations`` and ``preattestations``. They can be ``null`` when the corresponding
consensus operations are not required in the block. Otherwise, they contain three fields:
the ``total_committee_power`` and ``threshold``, as described in
:ref:`the consensus documentation<tb_validator_tallinn>`, and the
``recorded_power``, summing the power of all (pre)attestations
of the block.

Breaking changes to RPCs
^^^^^^^^^^^^^^^^^^^^^^^^

Among the :ref:`RPC changes<tallinn_RPC_changes>` brought by protocol
Tallinn, the following are potentially breaking:

- Updated RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/validators`` to group
  delegates by level. The returned list contains one element for each
  queried level (by default, only the current level), and contains
  five fields: the ``level`` itself, the ``consensus_threshold``
  required for the current level, the ``consensus_committee`` of the
  current level, the ``all_bakers_attest_activated`` which indicates the
  activation status of the "All Bakers Attest" feature, and
  ``delegates`` which is the list of validators for
  that level. Each element of this last list contains the fields
  present in the previous version of this RPC: ``delegate``, "slots"
  which have been renamed to ``rounds``, ``consensus_key``, and
  ``companion_key`` (optional).  Also include new fields for
  delegates, ``attesting_power``, with their attesting power for the
  level, and ``attestation_slot``, their slot for the given level.

- Updated RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/context/issuance/expected_issuance``.
  Output field ``baking_reward_bonus_per_slot`` has been replaced with
  ``baking_reward_bonus_per_block``, and ``attesting_reward_per_slot``
  with ``attesting_reward_per_block``. Their respective values are
  consequently 7000 times as high as before (since there are 7000
  slots per block).


Removed obsolete fields from the block header and block receipts
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The obsolete field ``adaptive_issuance_vote`` has been removed from
the block header in protocol Tallinn, and fields
``adaptive_issuance_vote_ema`` and
``adaptive_issuance_activation_cycle`` from the block metadata.

Note that the adaptive issuance activation cycle (which is 748 on
mainnet) can still be queried via the RPC ``GET
/chains/<chain>/blocks/<block>/context/adaptive_issuance_launch_cycle``.


Very slight increase in gas cost when calling smart contracts
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Protocol Tallinn fixes a minor bug that caused some gas costs to be
omitted in cache functions. As a result, gas costs for smart contract
calls has increased by at most 2 units of gas each time the cache is
accessed.


.. _v23_breaking_changes:

Octez Version 23
----------------

:doc:`Full Octez Version 23 Changelog<../releases/version-23>`

Operation encoding changes
^^^^^^^^^^^^^^^^^^^^^^^^^^

Multiple client commands and RPCs in Octez v23 are affected by the
:ref:`operation encoding changes in protocol Seoul described
below<operation_encodings_s>`. It is recommended to update any tool
producing or processing reveal operations to a Seoul-compatible
version.


Unique baker executable and unique accuser executable
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Starting with Octez v23, the single executable ``octez-baker``
(previously named ``octez-experimental-agnostic-baker``) is no longer
experimental, and should be preferred over the protocol-suffixed
executables ``octez-baker-<proto-hash>``, which will be deprecated in
``v24``, and will be removed in a later version.

Similarly, Octez v23 also introduces a single executable
``octez-accuser`` meant to gradually replace the protocol-suffixed
executables ``octez-accuser-<proto-hash>``.


Stricter validation for JSON configuration files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Previously, the parser would silently ignore any content that appeared
after the first valid JSON object. Starting with Octez v23.0~rc2, any
extraneous data in a configuration file will cause the function to
return an error.

This change affects the configuration files of the node, client,
signer, baker, accuser, smart rollup node, and DAL node.



.. _seoul_breaking_changes:

Protocol Seoul
--------------

:doc:`Full Protocol Seoul Changelog<../protocols/023_seoul>`

.. _operation_encodings_s:

Operation encoding changes
^^^^^^^^^^^^^^^^^^^^^^^^^^

Protocol Seoul adds new operations and changes the encoding of some
existing operations, for instance by adding new fields.
These changes are related to the support for tz4 BLS addresses and their aggregated signatures.

Backward compatibility
~~~~~~~~~~~~~~~~~~~~~~

Most of the changes in the encodings of existing operations are either purely added operations (e.g. ``update_companion_key``) or optional fields that should not break (e.g., for operations ``reveal`` and ``update_consensus_key``).

.. warning::

  However, tool providers which do not use encodings but rather :doc:`p2p message
  format <../shell/p2p_api>` may experience some issues. For example, the ``reveal``
  operation has a new boolean field to mark the presence of the optional ``proof`` for
  tz4 revelation.
  Users of such tools should check that they are operating versions compatible with the changes introduced by the Seoul protocol, and upgrade them if needed.

Breaking changes
~~~~~~~~~~~~~~~~

Starting in protocol Seoul, the ``Double_preattestation_evidence`` and
``Double_attestation_evidence`` operations are replaced with a
new ``Double_consensus_operation_evidence`` operation,
in order to enable denunciations of aggregated consensus operations. This new
operation contains a denounced slot and two denounced consensus
operations. For the evidence to be valid, the denounced operations
must both be preattestations (each one may be aggregated or not) or
both be attestations. Moreover, both must involve the denounced
slot, that is, be either a standalone operation for this slot or an
aggregate whose committee includes this slot.
The receipts for these operations have also been reworked, see :ref:`seoul_receipts_changes`.

All existing tz4 addresses are being unrevealed when protocol S is adopted, and they must provide a proof of possession to be revealed again, see :ref:`seoul_changelog_breaking_changes`.
This proof may be generated using the client command::

	octez-client create bls proof for <alias>


.. _v22_breaking_changes:

Octez Version 22
----------------

:doc:`Full Octez Version 22 Changelog<../releases/version-22>`

Baker: Explicit choice on using DAL or not via the CLI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Octez ``v21.3`` introduces the new ``--without-dal`` option for the baker daemon.
In Octez ``v21.3``, this option is not mandatory and will only trigger a warning.

Starting from Octez ``v22``, launching a baker daemon requires an explicit mention of the DAL.
The recommended approach is to run a DAL node and start the baker using the ``--dal-node <uri>`` option.
If you do not wish to use a DAL node, you can opt-out by using the ``--without-dal`` option.



Upcoming Changes
================

.. warning::

   Changes described in this section concern code that has been merged into the master branch but **has not been released yet**!
   They are meant as a heads-up for developers wanting to known in advance how future releases and protocols may impact developers and users.
   Note that they may also be modified or discarded before being released.

.. _octez_dev_breaking_changes:

Upcoming Octez Release
----------------------

N/A


.. _alpha_breaking_changes:

Development Protocol Alpha
--------------------------

:doc:`Full Protocol Alpha Changelog<../protocols/alpha>`

N/A
