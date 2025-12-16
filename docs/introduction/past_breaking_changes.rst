Past Breaking Changes and Deprecated Features
=============================================

This page list breaking changes and deprecated features in old Octez versions and protocols, that are normally no longer being run.

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
  - :package-api:`octez-l2-libs <octez-l2-libs/index.html>`: Contains the libraries related to Layer 2.
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
