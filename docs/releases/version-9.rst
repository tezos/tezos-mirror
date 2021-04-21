.. _version-9:

Version 9.0~rc2
===============

The first release candidate for version 9.0 contains a new version
(V2) of the protocol environment, which is the set of functions that
protocols can call.  This new version is used by Florence, which is
the current protocol proposal on Mainnet. The release candidate also
contains Florence itself as well as its daemons (baker, endorser and
accuser) so that you can test it easily.

This release candidate also contains the necessary configuration to
join the Florencenet test network, which runs Florence. To join
Florencenet, simply configure your node with ``tezos-node config
init --network florencenet``.

The second release candidate notably fixes a performance regression
and allows to bypass the cap on the connection count. It also
includes the fixes that were released in version 8.3.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v9.0-rc2
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v9-0-rc2`` Docker images of Tezos.

Known Issues
------------

If you are using ``tezos-docker-manager.sh``, the script currently tries
to download the wrong Docker images (``v9.0-rc2`` instead of ``v9-0-rc2``).
To workaround this issue, after downloading the script you can run::

  sed -i -e 's/v9.0-rc2/v9-0-rc2/' <FILENAME>

Replace ``<FILENAME>`` by ``mainnet.sh``, ``florencenet.sh`` or ``edo2net.sh``
depending on your use case. For instance, to use the script to run Mainnet::

  wget -O mainnet.sh https://gitlab.com/tezos/tezos/raw/v9.0-rc2/scripts/tezos-docker-manager.sh
  chmod +x mainnet.sh
  sed -i -e 's/v9.0-rc2/v9-0-rc2/' mainnet.sh

Changelog
---------

- `Version 9.0~rc2 <../CHANGES.html#version-9-0-rc2>`_
- `Version 9.0~rc1 <../CHANGES.html#version-9-0-rc1>`_
