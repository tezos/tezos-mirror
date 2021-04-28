.. _version-9:

Version 9.0
===========

Version 9.0 contains a new version (V2) of the protocol environment,
which is the set of functions that protocols can call. This new
version is used by Florence, which is the current protocol proposal on
Mainnet. The release also contains Florence itself as well as its
daemons (baker, endorser and accuser).

This release also contains the necessary configuration to join the
Florencenet test network, which runs Florence. To join Florencenet,
simply configure your node with ``tezos-node config init --network
florencenet``.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v9.0
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v9.0`` Docker images of Tezos.

Known Issues
------------

Regular but infrequent (a few times a day) spikes of RAM usage have
been noticed. If you run a node on hardware with less than 8 GB of RAM
you should activate some swap to handle those spikes. Of course, swap
may have a price in terms of performance. To not risk any slowdown at
all, especially when baking on Mainnet, the safest option is to have
more RAM.

Changelog
---------

- `Version 9.0 <../CHANGES.html#version-9-0>`_
- `Version 9.0~rc2 <../CHANGES.html#version-9-0-rc2>`_
- `Version 9.0~rc1 <../CHANGES.html#version-9-0-rc1>`_
