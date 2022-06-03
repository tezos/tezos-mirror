Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://tezos.gitlab.io/CHANGES.html


This file lists the changes added to each version of tezos-node,
tezos-client, and the other Octez executables. The changes to the economic
protocol are documented in the ``docs/protocols/`` directory; in
particular in ``docs/protocols/alpha.rst``.

When you make a commit on master, you can add an item in one of the
following subsections (node, client, …) to document your commit or the
set of related commits. This will ensure that this change is not
forgotten in the final changelog, which can be found in ``docs/CHANGES.rst``.
By having your commits update this file you also make it easy to find the
commits which are related to your changes using ``git log -p -- CHANGES.rst``.
Relevant items are moved to ``docs/CHANGES.rst`` after each release.

Only describe changes which affect users (bug fixes and new features),
or which will affect users in the future (deprecated features),
not refactorings or tests. Changes to the documentation do not need to
be documented here either.

Node
----

- Added store metrics to expose the amount of data written while
  storing the last block and the completion time of the last merge.

- Added a block validator metric to expose the number of operation per
  pass for each new block validated.

- Added a protocol specific metrics, head_cycle, head_consumed_gas and
  head_round.

- Added a store metric to expose the number of blocks considered as invalid.

- Fixed the `tezos-node config reset` command which did not actually reset
  the configuration file to its default values.

- Added metrics to observe the bootstrapped and synchronisation
  status.

- Added metrics to track the peer validator requests.

- Added an optional query parameter ``metadata`` to the GET
  /chains/<chain>/blocks/<block>/ and GET
  /chains/<chain>/blocks/<block>/operations/ RPCs. Passing this
  parameter with value ``always`` overrides the metadata size limit
  configuration, and forces the re-computation of operation metadata
  whose size was beyond the limit, and therefore not stored. The
  re-computed metadata are not stored on disk after this call, but
  rather just returned by the RPC call. Passing this parameters with
  value ``never`` prevents the request to return metadata, to allow
  lighter requests. If the query string is not used, the configured
  metadata size limit policy is used.

- Deprecated the ``force_metadata`` query paramater for the the GET
  /chains/<chain>/blocks/<block>/ and GET
  /chains/<chain>/blocks/<block>/operations/ RPCs. To get a similar
  behaviour, use the ``metadata`` query string with the value
  ``always``.

- Deprecated the CLI argument `--enable-testchain` and the corresponding
  configuration-file option `p2p.enable_testchain`.

Client
------

- Client allows to simulate failing operations with ``--simulation
  --force``, and report errors without specifying limits.

- Added `--ignore-case` option to the `tezos-client gen vanity keys` command
  to allow case-insensitive search for the given pattern.

Accuser
-------

Signer
------

Proxy server
------------

Protocol Compiler And Environment
---------------------------------

Codec
-----

Docker Images
-------------

Miscellaneous
-------------
