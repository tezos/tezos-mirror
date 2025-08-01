Version 9.7
===========

Version 9.0 contains a new version (V2) of the protocol environment,
which is the set of functions that protocols can call. This new
version is used by Florence, which is the current protocol on
Mainnet. The release also contains Florence itself as well as its
daemons (baker, endorser and accuser).

This release also contains the necessary configuration to join the
Florencenet test network, which runs Florence. To join Florencenet,
simply configure your node with ``tezos-node config init --network
florencenet``.

Version 9.1 fixes a performance and memory usage regression issue
and reintroduces ``/normalized`` RPCs.

Version 9.2 adds Granada, a protocol proposal for Mainnet featuring,
among others, the Emmy* consensus algorithm, Liquidity Baking, and
reduced gas consumption. Version 9.2 also improves how the mempool
chooses which operations to keep. This should result in fewer
endorsements being missed.

Version 9.3 fixes some cases where ``MDB_MAP_FULL`` errors could occur
and reintroduces more ``/normalized`` RPCs which were still missing
from version 9.1.

Version 9.4 fixes an issue in the mempool that caused too many
operations referring to unknown blocks to be kept, resulting in the
node running out of memory.

Version 9.5 fixes a bug that could result in a corrupted storage
and in assert failure errors.

Version 9.6 increases the delay after which the endorser gives up on
endorsing to 1200 seconds (previously 110 seconds), to prevent an
issue where blocks that arrived too late were not endorsed at all,
causing the next block to also be produced late.

Version 9.7 improves how the node handles pending consensus operations
to remove costly computations in cases where they are not needed.

Update Instructions
-------------------

To update from sources:

.. code-block:: shell

  git fetch
  git checkout v9.7
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v9.7`` Docker images of Tezos.

Changelog
---------

- `Version 9.7 <../CHANGES.html#version-9-7>`_
- `Version 9.6 <../CHANGES.html#version-9-6>`_
- `Version 9.5 <../CHANGES.html#version-9-5>`_
- `Version 9.4 <../CHANGES.html#version-9-4>`_
- `Version 9.3 <../CHANGES.html#version-9-3>`_
- `Version 9.2 <../CHANGES.html#version-9-2>`_
- `Version 9.1 <../CHANGES.html#version-9-1>`_
- `Version 9.0 <../CHANGES.html#version-9-0>`_
- `Version 9.0~rc2 <../CHANGES.html#version-9-0-rc2>`_
- `Version 9.0~rc1 <../CHANGES.html#version-9-0-rc1>`_
