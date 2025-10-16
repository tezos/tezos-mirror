Breaking Changes
================

This section presents the breaking changes that users may encounter
between successive protocols or successive Octez versions. It
complements the respective :ref:`protocol changelogs
<protocol_changelogs>` and :doc:`Octez changelog <../CHANGES>` by
gathering all breaking changes in one place and providing more context
when appropriate.

For each change, there may be subsections ``Deprecation`` and ``Breaking
changes``. The former subsection will explain what changes can be made during a
deprecation phase to adapt smoothly to the new changes. The latter subsection
will present the changes that can not be done by the deprecation mechanism and
that may be breaking.

In the particular case of RPC changes, you may consult complementary information on :ref:`RPC versioning <rpc_versioning>`, covering how new versions are introduced, the deprecation policy, and a concrete calendar of RPCs planned to be removed.



.. _t024_breaking_changes:

Protocol T024
-------------

:doc:`Full Protocol T024 Changelog<../protocols/024_t024>`


6s Block Time
^^^^^^^^^^^^^

Block time has been reduced from 8 seconds to 6 seconds on
mainnet. That is, a block can be produced with a delay of 6 seconds
with respect to the previous block, if the latter is at round 0.

Multiple protocol and smart rollup parameters have been updated in
consequence, to ensure that their duration in terms of
minutes/hours/weeks remains the same as in protocol Seoul. A full list
of affected parameters with their old and new values can be found
:ref:`here<6s_block_time_parameters_t024>`.

Unlike other parameters, the value of parameter
``smart_rollup_max_active_outbox_levels`` remains unchanged in terms
of blocks. This means that the actual duration of the maximal allowed
period of withdrawal has decreased from ~14 days in protocol Seoul to
~10 days in protocol T024.


Breaking changes to RPCs
^^^^^^^^^^^^^^^^^^^^^^^^

- Updated RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/validators`` to group
  delegates by level. The returned list contains one element for each
  queried level (by default, only the current level), and contains
  four fields: the ``level`` itself, the ``consensus_threshold``
  required for the current level, the ``consensus_committee`` of the
  current level, and ``delegates`` which is the list of validators for
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
the block header, and fields ``adaptive_issuance_vote_ema`` and
``adaptive_issuance_activation_cycle`` from the block metadata.

Note that the adaptive issuance activation cycle (which is 748 on
mainnet) can still be queried via the RPC ``GET
/chains/<chain>/blocks/<block>/context/adaptive_issuance_launch_cycle``.


Very slight increase in gas cost when calling smart contracts
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Protocol T024 fixes a minor bug that caused some gas costs to be
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
after the first valid JSON object. Now, any extraneous data in a
configuration file will cause the function to return an error.

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

Starting in protocol S, the ``Double_preattestation_evidence`` and
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


Older Octez versions and protocols
==================================

Attestations
------------

Support for deprecated attestation legacy name ("endorsement"), that was still
usable with RPCs version ``0`` will be removed in the protocol proposal ``Q``
and Octez ``v21.0``.

Opam packages
-------------

Starting from Octez v18, the Opam packages are being reworked as a new set containing fewer packages. This allows easier installation and maintenance.

These changes are transparent for users of the different kinds of Octez distributions (static executables, Docker images, Opam-installed binaries, etc.).
They only impact software developers directly relying on Opam packages within the Octez repository (i.e. using them as dependencies).

New architecture
^^^^^^^^^^^^^^^^

Some Octez libraries which used to be distributed as their own Opam package have been aggregated into fewer and coarser Opam packages.

Each aggregate is related to a part of Octez.

Octez is now distributed as the following set of Opam packages:
  - :package-api:`octez-libs <octez-libs/index.html>`: Contains the base libraries for Octez (cryptography primitives, error management helpers, etc.).
  - :package-api:`octez-shell <octez-shell-libs/index.html>`: Contains the libraries related to the Octez shell.
  - :package-api:`octez-proto-libs <octez-proto-libs/index.html>`: Contains the libraries for the Tezos protocol.
  - :package-api:`octez-l2-libs <octez-l2-libs/index.html>`: Contains the libraries related to layer 2.
  - For each protocol ``P``:
    - :package-api:`octez-protocol-P-libs <octez-protocol-alpha-libs/index.html>`: The protocol ``P`` dependent libraries.
    - ``tezos-protocol-P``: The Tezos protocol ``P`` itself.

To have a better understanding of the packages and the complete description of them, you might want to follow the :doc:`OCaml API documentation <../api/api-inline>`.

Note on library renaming
~~~~~~~~~~~~~~~~~~~~~~~~

In aggregated packages, redundant suffixes and prefixes have been removed.
Specifically, all the sub-libraries prefixed with ``tezos-`` or ``octez-`` are now renamed without the prefix.
For instance, ``tezos-base``, which is now a sub-library of ``octez-libs``, is now ``octez-libs.base``.

The protocol name suffixes of the protocol libraries have also been removed.
For instance, ``Tezos-client-017-PtNairob`` is now the sub-library ``Client`` of the package ``Octez-017-PtNairob-libs``.


Backward compatibility
^^^^^^^^^^^^^^^^^^^^^^

One can install the Octez suite directly by using the command:

.. code-block:: ocaml

	opam install octez

This process is the same as with the previous set of packages. The only difference is the installed packages, but no compatibility issues will be encountered.

Alternatively, each Octez package can be installed separately:

.. code-block:: ocaml

	opam install package-name

Breaking changes
^^^^^^^^^^^^^^^^

Opam packages can be used as dependencies for software development.
Contrary to the previous section, the rework of the Octez Opam packages will require you to adapt how your
software declares Octez-related Opam dependencies.

For each dependency:

- Search for the new package name in the API.
- Change the Opam ``depends`` to the package name.
- Update the ``dune`` files with the new name ``package.sub-library``.
- Change the module name in the ``open`` in the code to ``Package.Sub-library``.

For instance, if your software depends on ``tezos-rpc`` which is now a sub-library of  :package-api:`octez-libs <octez-libs/index.html>` and has been renamed to ``rpc``:

- Update the opam file content to rename the ``tezos-rpc`` dependency to ``octez-libs``. If ``octez-libs`` is already present, only remove the dependency on ``tezos-rpc``.
- Update the dune file to rename occurences of ``tezos-rpc``, e.g. in ``libraries`` clauses of ``executable`` stanzas to ``octez-libs.rpc``.
- In your code, update all references to the ``Tezos_rpc`` module (e.g. ``open Tezos_rpc``) to ``Octez-libs.Rpc`` (e.g. ``open Octez-libs.Rpc``).

The same method applies to each dependency that is now a sub-library of a new package. Check the :doc:`API <../api/api-inline>` to see the new packages.

Delegates rights vs stake snapshots
-----------------------------------

The selection of the delegates' participation rights in the proof-of-stake consensus protocol is done based on their stake.
This computation is explained in generic terms in :doc:`../active/proof_of_stake`.

One detail of the rights computation has changed: which values are considered for the delegates' stake in each cycle.
Previously, the considered values corresponded to a notion of stake snapshots, recorderd regularly by the protocol.

Breaking changes
^^^^^^^^^^^^^^^^

Since :doc:`protocol Paris <../protocols/020_paris>`, there are no more stake snapshots, so the protocol no longer relies on stake snapshots to compute the rights.

Instead:

- Rights originating from staked tez are computed from the value at the end of the cycle;
- Rights originating from delegated tez are computing using the minimum value over the cycle.

Timelocks: chest keys
---------------------

Timelocks were temporarily disabled by the activation of the :doc:`Lima protocol <../protocols/015_lima>`. to address a critical vulnerability.

A new safer version of Timelocks was developed to address this issue, and the feature `was re-enabled <https://research-development.nomadic-labs.com/oxford-announcement.html#timelocks-are-re-enabled>`__ in the :doc:`Oxford protocol <../protocols/018_oxford>`. However, the new chest keys format could not be made backward compatible with the previous one.

Fortunately, **no contracts using the legacy format of chest keys are deployed on Tezos mainnet**.

Breaking changes
^^^^^^^^^^^^^^^^

However, this change may impact old contracts on the Ghostnet test network.
For example, one (trivial) `chest key demo contract <https://ghostnet.tzkt.io/KT19AtusZuLVAKEXTEERNkfL7LmzuhkXwze1/code>`__ was originated a long time ago on Ghostnet and uses the legacy format for chest keys.

As a consequence, `inspecting this Ghostnet contract <https://rpc.ghostnet.teztnets.com/chains/main/blocks/BMDLt6XUxEYc6W5SfCmYncafPd5tHxdipWVNvkm9hZz9PF6Ei2g/context/contracts/KT19AtusZuLVAKEXTEERNkfL7LmzuhkXwze1>`__ currently returns an error response with status 500::

    Body:
    [
        {
            "kind": "permanent",
            "id": "proto.019-PtParisB.michelson_v1.ill_typed_data",
            "expected_type": {
                "prim": "chest_key"
            },
            "ill_typed_expression": {
                "bytes": "e4c38197..."
            }
        },
        ...
    ]

Baker: Explicit choice on using DAL or not via the CLI
------------------------------------------------------

Octez ``v21.3`` introduces the new ``--without-dal`` option for the baker daemon.
In Octez ``v21.3``, this option is not mandatory and will only trigger a warning.

Starting from Octez ``v22``, launching a baker daemon requires an explicit mention of the DAL.
The recommended approach is to run a DAL node and start the baker using the ``--dal-node <uri>`` option.
If you do not wish to use a DAL node, you can opt-out by using the ``--without-dal`` option.
