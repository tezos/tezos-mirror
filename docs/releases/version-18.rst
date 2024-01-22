Version 18.1
============

Version 18.0 contains a new version (v10) of the protocol environment,
which is the set of functions that a protocol can call.
This new version is used by Oxford,
which is a proposal for the successor of Nairobi that has been rejected by vote on Oct 2, 2023.
This release also contains Oxford itself as well as its associated protocol-specific executable binaries (baker, accuser, etc).

Starting from Oxford, Endorsements have been renamed to Attestations.
Thus, Octez now uses Attestations:

- | RPCs now accept both endorsements and attestations as input and/or output. For now, these RPCs still output endorsement by default. For more information see the :doc:`OpenAPI specifications here <../api/openapi>`.
  | Please note that the endorsement RPCs versions are considered as deprecated and may be removed in a future major version.
- Client, baker, and accuser executables use ``attestation`` instead of ``endorsement`` in error messages and events.

DAC node and client executables are released for experimental usage only.
Users can experimentally integrate a DAC in their Smart Rollups workflow to achieve higher data throughput and lower gas fees.
Please refer to :doc:`Data Availability Committees <../shell/data_availability_committees>` for more details.

.. warning::

   Octez version 18 increments the snapshots version from ``5`` to ``6``.
   Thus, snapshots exported with Octez versions >= 18.0 cannot be used nor imported by nodes running previous versions.
   The store version upgrade is automatic and irreversible. Once a v18.0 node initializes, it will permanently upgrade a pre-existing store to the new version.

Octez version 18 improves performance, notably to the block validation process: total validation time is halved on average, resulting in a reduced block propagation time.

v18 also fixes a concurrency issue in the logging infrastructure which can cause the node to become temporarily unresponsive.

As Oxford includes a new Staking mechanism, version 18 of Octez implements new client commands for stake funds management, and to allow delegates to configure their staking policies. See :ref:`Adaptive Issuance and Staking <new_staking_alpha>` for more details.

Version 18.1 fixes an issue that would result in corrupted full and rollup snapshots being exported.
Thus, the snapshots version is bumped from ``6`` to ``7``.
Note that snapshots exported by nodes running v18.1 are not retro-compatible. As a result, these cannot be imported by a node running earlier versions of Octez. However, a node running Octez v18.1 can still import v6 (and earlier) snapshots.

.. warning::

   Octez v18.0 nodes might export corrupted tar full and rolling snapshots. Consequently, we recommend that snapshots providers upgrade to Octez v18.1 as soon as possible, in order to ensure the availability of recent full and rolling snapshots.

Oxford's feature activation vote
--------------------------------

The Oxford protocol proposal includes 3 features (all part of Adaptive Issuance and the new Staking mechanism), which would not be immediately available upon the protocol's eventual activation:

- Adaptive issuance;
- the ability for *delegators* to become *stakers*; and,
- the changes in weight for *staked* and *delegated* funds towards the computation of baking and voting rights.

Instead, these are guarded behind a *single* per-block vote mechanism, where bakers signal their position **(On, Off, Pass)**.

Specifically, the Octez v18.0 Oxford baker executable introduces a dedicated option ``--adaptive-issuance-vote``, to allow bakers to manifest their choice.
The use of this flag is *optional*, and defaults to **Pass** if not present.

See :ref:`here <feature_activation_alpha>` for further details on this additional activation vote mechanism.


Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v18.1
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v18.1`` Docker images of Octez.

You can also install Octez using Opam by running ``opam install octez``.

.. warning::

   Starting from Octez v18, the Opam packages are being reworked as a new set containing fewer packages. This allows easier installation and maintenance.

   These changes are transparent for users of the different kinds of Octez distributions (static executables, Docker images, Opam-installed binaries, etc.).
   They only impact software developers directly relying on Opam packages within the Octez repository (i.e. using them as dependencies).

   Most of the Opam packages have been aggregated into the following packages:
     - :package-api:`octez-libs <octez-libs/index.html>`: Contains the base libraries for Octez.
     - :package-api:`octez-shell <octez-shell-libs/index.html>`: Contains the Octez shell related libraries.
     - :package-api:`octez-proto-shell <octez-proto-libs/index.html>`: Contains the Tezos protocol dependent libraries.
     - :package-api:`octez-l2-libs <octez-l2-libs/index.html>`: Contains the layer 2 related libraries.
     - For each protocol ``P``
         - :package-api:`octez-protocol-P-libs <octez-protocol-alpha-libs/index.html>`: The protocol ``P`` dependent libraries.
	 - ``tezos-protocol-P``: The Tezos protocol ``P`` itself.

   The other packages have not (yet) been packed into aggregated packages: some of them may be refactored in future versions; some other are meant to remain standalone. In particular, each Octez binary is contained for now in a separate standalone package.

   Finally, be aware that the old packages, that are now sub-libraries of the packages mentioned above, have been renamed by removing the ``tezos-`` and ``octez-`` prefixes.
   For protocol dependent sub-libraries, the redundant protocol name suffixes have also been removed.
   For instance, ``Tezos-client-017-PtNairob`` is now the sub-library ``Client`` of the package ``Octez-017-PtNairob-libs``.

   For more details, see :doc:`the OCaml API <../api/api-inline>`.

You can now download experimental Debian and Redhat packages on the `release page <https://gitlab.com/tezos/tezos/-/releases/v18.1>`_  and in the `package registry <https://gitlab.com/tezos/tezos/-/packages>`_.


Changelog
---------

- `Version 18.1 <../CHANGES.html#version-18-1>`_
- `Version 18.0 <../CHANGES.html#version-18-0>`_
- `Version 18.0~rc1 <../CHANGES.html#version-18-0-rc1>`_
