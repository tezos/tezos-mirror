How to Freeze Protocols
=======================

After each protocol activation, old protocols are kept in the codebase.
However large parts of the code developed for the previous protocol can now be
removed to avoid accumulating dead code in the repository. All daemons, tests
associated to old protocols, and RW (read-write) commands available to the client can be
removed.

In this document, "protocol" refers to the protocol that we want to freeze, N,
and "current protocol" refers to its successor, N+1. For instance, if
"protocol" refers to 008, "current protocol" refers to 009.

The various pieces of code to be removed are within directory
``src/proto_<N>_<Hash>/``, unless indicated otherwise.

Update Manifest
---------------

In :src:`manifest/main.ml`, look for ``let alpha = active Name.alpha``.
In one of the lines above it is the declaration of the protocol you are freezing.
Replace ``active`` by ``frozen`` for this protocol.
Run the manifest with ``make -C manifest``.

Remove Accuser, Baker
---------------------

These daemons are no longer needed. Thus, the code in
``bin_{accuser,baker}/`` can be safely removed and
``script-inputs/active_testing_protocol_versions`` should be
modified accordingly.

Remove Protocol Tests
---------------------

All tests developed for the protocol can be removed.

These are defined in

- ``lib_client/test``
- ``lib_protocol/test``

Remove ``lib_delegate``
-----------------------

This code is no longer needed and can be safely removed.

Remove Protocol Tests From Tezt
-------------------------------

The code of Tezt must be updated manually removing the protocol from the type
``Protocol.t`` and adapting the tests accordingly.

Remove Testnet From ``bin_node``
--------------------------------

Mentions of the protocol's testnets should be removed from the node executable.
In particular the file :src:`src/lib_node_config/config_file.ml` should be
amended.

The protocol plugin registration module should be removed from
:src:`src/bin_node/dune` and :src:`opam/octez-node.opam`.

Remove RW Commands From ``lib_client_commands``
-----------------------------------------------

The client commands at
``proto_XXX/lib_client_commands/client_proto_context_commands.ml``
define both read-only and read-write commands, where reads and
writes refer to accesses to and modifications of the state of the
chain. The RW commands can now be safely removed as they are no longer
needed.

Remove Mempool Protocol Plugins
-------------------------------

The Mempool protocol plugin located in ``proto_XXX/lib_plugin/plugin.ml`` can be
removed.  This implies removing the plugin code, and remove the registration of
the plugin in the file ``proto_XXX/lib_plugin/plugin_registration.ml``.

Other plugins should be evaluated case-by-case. At the moment of writing, the
``Mempool`` plugin is the only one that can be safely removed.

Remove Old Docker-Compose Files
-------------------------------

The docker-compose file with the corresponding protocol name can be removed from the
``scripts/docker`` directory.

Add an Entry in ``CHANGES.rst``
-------------------------------

Add an entry in :src:`CHANGES.rst` to summarize all changes for the user.

In particular it should mention:

- that the protocol's daemons were removed;
- that the corresponding ``--network`` alias was removed.
