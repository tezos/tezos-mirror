Version 18.0~rc1
================

Version 18.0 contains a new version (V10) of the protocol environment,
which is the set of functions that a protocol can call.
This new version is used by protocol :doc:`Oxford <../protocols/018_oxford>`,
which is a proposal for the successor of Nairobi.
This release also contains Oxford itself as well as its associated protocol-specific executable binaries (baker, accuser, etc).

Starting from Oxford, Endorsements have been renamed to Attestations.
Thus, Octez now uses Attestations:

- | RPCs now accept both endorsements and attestations as input and/or output. For now, these RPCs still output endorsement by default. For more information see the :doc:`OpenAPI specifications here <../api/openapi>`.
  | Please note that the endorsement RPCs versions are considered as deprecated and will be removed in the next Octez major version (''v19.x'').
- Client, baker, and accuser executables use ``attestation`` instead of ``endorsement`` in error messages and events.

DAC node and client executables are released for experimental usage only.
Users can experimentally integrate a DAC in their Smart Rollups workflow to achieve higher data throughput and lower gas fees.
Please refer to :doc:`Data Availability Committees <../shell/data_availability_committees>` for more details.

Octez version 18 improves performance, notably to the block validation process: total validation time is halved on average, resulting in a reduced block propagation time.

As Oxford includes a new Staking mechanism, version 18 of Octez implements new client commands for stake funds management, and to allow delegates to configure their staking policies. See `Adaptive Issuance and Staking <https://research-development.nomadic-labs.com/adaptive-issuance.html#new-staking-mechanism>`_ for more details.

Oxford's feature activation vote
--------------------------------

The Oxford protocol includes 3 features (all part of Adaptive Issuance and the new Staking mechanism), which would not be immediately available upon the protocol's eventual activation:

- Adaptive issuance;
- the ability for *delegators* to become *stakers*; and,
- the changes in weight for *staked* and *delegated* funds towards the computation of baking and voting rights.

Instead, these are guarded behind a *single* per-block vote mechanism, where bakers signal their position **(Yes, No, Pass)**.

Specifically, the Octez v18.0~rc1 Oxford baker executable introduces a dedicated option ``--adaptive-issuance-vote``, to allow bakers to manifest their choice.
The use of this flag is *optional*, and defaults to **Pass** if not present.

See `here <https://research-development.nomadic-labs.com/adaptive-issuance.html#feature-activation-vs-protocol-activation>`__ for further details on this additional activation vote mechanism.


Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v18.0-rc1
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v18.0-rc1`` Docker images of Octez.

You can also install Octez using Opam by running ``opam install octez``.

.. warning::

   Starting from Octez v18, the opam packages are being reworked.
   Octez is now composed of less packages, containing the former packages as sub-libraries.

   For more details, see :doc:`the OCaml API <../api/api-inline>`.

Changelog
---------

- `Version 18.0~rc1 <../CHANGES.html#version-18-0-rc1>`_
