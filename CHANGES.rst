Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://tezos.gitlab.io/CHANGES.html


This file lists the changes added to each version of octez-node,
octez-client, and the other Octez executables. The changes to the economic
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

General
-------

Node
----

- Changed the bounding specification of valid operations in the mempool:

  + Before, the number of valid **manager operations** in the mempool
    was at most ``max_prechecked_manager_operations`` (default 5_000),
    with no other constraints. (Operations to keep were selected
    according to a "weight" that consists in the ratio of fee over
    "resources"; the latter is the maximum between the following
    ratios: operation gas over maximal allowed gas, and operation size
    over maximal allowed size. The baker uses the same notion of
    "weight" to select operations.)

  + Now, the number of valid **operations of any kind** is at most
    ``max_operations`` (default 10_000), and also the **sum of the
    sizes in bytes** of all valid operations is at most
    ``max_total_bytes`` (default 10_000_000). See
    [src/lib_shell/prevalidator_bounding.mli] for the reasoning behind
    the default values. (Operations are selected according to the
    protocol's ``compare_operations`` function, which currently orders
    operations according to their validation pass (consensus is
    highest and manager is lowest); note that two manager operations
    are ordered using their fee over gas ratio.)

  The values of ``max_operations`` and ``max_total_bytes`` can be
  retrieved with ``GET /chains/<chain>/mempool/filter`` and configured
  with ``POST /chains/<chain>/mempool/filter`` (just as
  ``max_prechecked_manager_operations`` used to be). As a result, the
  JSON format of the outputs of these two RPCs and the input of the
  second one have slightly changed; see their updated descriptions.
  (MR :gl:`!6787`)

- Errors ``prefilter.fees_too_low_for_mempool`` and
  ``plugin.removed_fees_too_low_for_mempool`` have been replaced with
  ``node.mempool.rejected_by_full_mempool`` and
  ``node.mempool.removed_from_full_mempool`` with different
  descriptions and messages. In particular, the
  ``rejected_by_full_mempool`` error no longer indicates the minimal
  fee needed by the rejected operation to be accepted by the full
  mempool (however, we are working on providing this information
  again). (MR :gl:`!6787`)

Client
------
- Adding client commands to generate, open and verify a time-lock.

- The ``typecheck script`` command can now be used to typecheck several scripts.

Baker
-----

- Changed the baker liquidity baking vote file
  ``per_block_votes.json`` lookup so that it also considers its client
  data directory when searching an existing file. The previous
  semantics, which looks for this file in the current working
  directory, takes predecence.

Accuser
-------

Signer
------

Proxy Server
------------

Protocol Compiler And Environment
---------------------------------

Codec
-----

Docker Images
-------------

Smart Rollup node
-----------------

- Faster bootstrapping process. (MR :gl:`!8618`)

Smart Rollup client
-------------------

Smart Rollup WASM Debugger
--------------------------

Miscellaneous
-------------

- Updating and re-enabling the time-lock Michelson commands.
