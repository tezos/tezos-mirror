How to Freeze Protocols
=======================

After each protocol activation, old protocols are kept in the codebase.
However large parts of the code developed for the previous protocol can now be
removed to avoid accumulating dead code in the repository. All daemons, tests
associated to old protocols, and RW (read-write) commands available to the client can be
removed.

In this document, "protocol" refers to the protocol that we want to freeze, N,
and "current protocol" refers to its successor, N+1. For instance, if
"protocol" refers to 023, "current protocol" refers to 024.

The various pieces of code to be removed are within directory
``src/proto_<N>_<Hash>/``, unless indicated otherwise.

For a complete example, see the merge requests of previous freezes, e.g.
Seoul (:gl:`!20940`).

Update Manifest
---------------

In :src:`manifest/product_octez.ml`, look for ``let alpha = active (Name.dev "alpha")``.
In one of the lines above it is the declaration of the protocol you are freezing.
Replace ``active`` by ``frozen`` for this protocol.

Regenerate the build files with ``make -C manifest``. The manifest will
complain about ``.opam`` files that exist but are no longer generated (baker,
accuser, benchmarks, injector, protocol tests): remove them with ``git rm``
and run ``make -C manifest`` again.

Also regenerate the CI files with ``make -C ci``, which removes the frozen
protocol from the test pipelines.

The regeneration also updates ``dune-project``, ``script-inputs/``,
``tobi/config`` and the ``dune`` files of the components that depended on the
protocol's removed libraries.

Remove Accuser, Baker and Protocol-Specific Libraries
-----------------------------------------------------

These daemons and libraries are no longer needed. Thus, the code in the
following directories can be safely removed:

- ``bin_accuser/``
- ``bin_baker/``
- ``lib_agnostic_baker/``
- ``lib_delegate/``
- ``lib_benchmark/``
- ``lib_benchmarks_proto/``
- ``lib_injector/``

Remove Protocol Tests
---------------------

All tests developed for the protocol can be removed.

These are defined in

- ``lib_protocol/test``
- ``lib_client/test``
- ``lib_dal/test``
- ``lib_plugin/test``
- ``lib_sc_rollup_node/test``

Remove Per-Protocol Devtools Files
----------------------------------

The following files, generated for each protocol, can be removed:

- ``devtools/get_contracts/get_contracts_<N>_<Hash>.ml``
- ``devtools/testnet_experiment_tools/tool_<N>_<Hash>.ml``
- ``devtools/yes_wallet/get_delegates_<N>_<Hash>.ml``

Disable the Mempool Protocol Plugin
-----------------------------------

Frozen protocols do not need mempool validation. In ``lib_plugin/``:

- remove ``mempool.ml`` and ``mempool.mli``;
- remove the ``Mempool`` module export from ``plugin.ml``;
- remove the ``Validation`` module and its
  ``Protocol_plugin.register_validation_plugin`` registration from
  ``plugin_registerer.ml``.

Some client code uses the mempool plugin's default fee values
(``default_minimal_nanotez_per_byte``, ...). Inline these values where they
are used, typically in ``lib_client/injection.ml`` and in the stresstest
commands of ``lib_client_commands/``.

Other plugins should be evaluated case-by-case. At the moment of writing, the
``Mempool`` plugin is the only one that can be safely removed.

Disable Protocol Tests in Tezt
------------------------------

In :src:`tezt/lib_tezos/protocol.ml`:

- remove the protocol from ``all`` so that protocol-dependent tests no longer
  run on it;
- set ``previous_protocol`` of the current protocol to ``None`` to disable
  migration tests;
- if nothing references the protocol's constructor anymore, remove it from
  the type ``Protocol.t`` and adapt the remaining code accordingly; otherwise
  it can be kept.

Remove the Protocol's Testnet Configuration
-------------------------------------------

If a testnet was associated to the protocol (e.g. ``seoulnet`` for Seoul),
remove its configuration:

- in :src:`tezt/lib_tezos/node.ml` (and its ``.mli``): the
  ``<testnet>_network_config`` value and ``set_<testnet>_network`` function of
  ``Node.Config_file``;
- in ``tezt/tests/cloud/``: the testnet variant of the ``Network.t`` type
  (``network.ml``/``network.mli``) and all the match cases that mention it
  (``node_helpers.ml``, ``snapshot_helpers.ml``, ``monitoring_app.ml``,
  ``stake_repartition.ml``, ...);
- in :src:`tezt/lib_migration_test_registry/register.ml`: the testnet variant.

While doing so, update ``Network.default_protocol`` for the long-lived
networks (Mainnet, Ghostnet, ...) to the current protocol.

If the protocol's testnet is a built-in network of the node, also remove it
from :src:`src/lib_node_config/config_file.ml` and its registration from
:src:`src/bin_node/dune` and :src:`opam/octez-node.opam`.

Update the Profiler Patches
---------------------------

:src:`scripts/patch-profiler-proto.sh` applies a profiler patch to the latest
snapshotted protocol, in addition to Alpha. Remove the frozen protocol's
``scripts/profile_<frozen protocol>.patch`` and make the script apply the
current protocol's patch instead.

Note that the per-protocol patch is not generated when the protocol is
snapshotted: it is created manually. If the current protocol's patch does not
exist yet, it must be created (by transposing the frozen protocol's patch and
resolving the conflicts) before the frozen protocol's patch can be removed.
This is usually done in a separate merge request.

Remove RW Commands From ``lib_client_commands``
-----------------------------------------------

The client commands at
``proto_XXX/lib_client_commands/client_proto_context_commands.ml``
define both read-only and read-write commands, where reads and
writes refer to accesses to and modifications of the state of the
chain. The RW commands can now be safely removed as they are no longer
needed.

Remove Old Docker-Compose Files
-------------------------------

The docker-compose file with the corresponding protocol name can be removed from the
``scripts/docker`` directory.

Add an Entry in ``CHANGES.rst``
-------------------------------

Add an entry in :src:`CHANGES.rst` to summarize all changes for the user.

In particular it should mention:

- that the protocol's daemons were removed;
- that the corresponding ``--network`` alias was removed, if the protocol's
  testnet was a built-in network of the node.
