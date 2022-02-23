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
`src/proto_<N>_<Hash>/`, unless indicated otherwise.

Remove Accuser, Baker, Endorser
-------------------------------

All these three daemons are no longer needed. Thus, the code in
`bin_{accuser,baker,endorser}/`,  can be safely removed and the files
`active_protocol_versions` and `active_testing_protocol_versions` should be
modified accordingly.

Remove Protocol Tests
---------------------

All tests developed for the protocol can be removed.

These are defined in

- `lib_client/test`
- `lib_protocol/test`
- `tests_python/tests_XXX`

Remove ``lib_delegate``
-----------------------

This code is no longer needed and can be safely removed.

Remove Protocol Tests From Tezt
-------------------------------

The code of Tezt must be updated manually removing the protocol from the type
`Protocol.t` and adapting the tests accordingly.

Remove Testnets From ``tezos-docker-manager.sh``
------------------------------------------------

Mentions of the protocol's testnests should be removed from the script
:src:`scripts/tezos-docker-manager.sh`.

Remove Testnet From ``bin_node``
--------------------------------

Mentions of the protocol's testnets should be removed from the node executable.
In particular the file :src:`src/bin_node/node_config_file.ml` should be
amended.

The protocol plugin registration module should be removed from
:src:`src/bin_node/dune` and :src:`src/bin_node/tezos-node.opam`.

Remove Protocol From Python Tests of Current Protocol
-----------------------------------------------------

Tests ``test_voting_full.py`` and ``test_migration.py`` from the current
protocol should be removed. Any mention of the protocol in
``tests_python/tests_XXX/protocol.py`` should also be removed.

These changes remove the migration testing code of the current protocol that is
no longer necessary to test.

Remove RW Commands From ``lib_client_commands``
-----------------------------------------------

The client commands at
`proto_XXX/lib_client_commands/client_proto_context_commands.ml`
define both read-only and read-write commands, where reads and
writes refer to accesses to and modifications of the state of the
chain. The RW commands can now be safely removed as they are no longer
needed.

Remove Mempool Protocol Plugins
-------------------------------

The Mempool protocol plugin located in `proto_XXX/lib_plugin/plugin.ml` can be
removed.  This implies removing the plugin code, and remove the registration of
the plugin in the file `proto_XXX/lib_plugin/plugin_registration.ml`.

Other plugins should be evaluated case-by-case. At the moment of writing, the
`Mempool` plugin is the only one that can be safely removed.

Add an Entry in `CHANGES.rst`
-----------------------------

Add an entry in :src:`CHANGES.rst` to summarize all changes for the user.

In particular it should mention:

- that the protocol's daemons were removed;
- that the corresponding ``--network`` alias was removed.
