Version 8.3
===========

Version 8.0 contains a new version (V1) of the protocol
environment, which is the set of functions that protocols can call. Up
to Delphi, all protocols used protocol environment V0. The new version
(V1) is used by Edo, which is a proposal for the next protocol after
Delphi. The release candidate also contains Edo itself as well as its
daemons (baker, endorser and accuser) so that you can test it easily.

We have also spawned a test network for Edo, named Edonet, that
replaces Ebetanet, which was a test network for a beta version of
Edo. The release candidate contains the necessary configuration to
join Edonet: just configure your node with
``tezos-node config init --network edonet`` (but see `Known Issues`_ below).

Version 8.1 fixes a performance regression related to operations
involving ``tz3`` addresses and several compilation problems in
some contexts.

Version 8.2 replaces ``PtEdoTez`` by ``PtEdo2Zk`` and provides RPCs to
"normalize" Michelson expressions returned by the Edo protocol along
with constraining the size of p2p messages at low level and updating
some external dependencies.

Version 8.3 fixes a couple of issues that caused the baker to not include
some operations.

Update Instructions
-------------------

Starting from version 8.0, compiling Tezos requires the Rust compiler,
version 1.44.0, and the Cargo package manager to be installed.
See :ref:`instructions to set up Rust<setup_rust>`.

To update from sources:

.. code-block:: shell

  git fetch
  git checkout v8.3
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v8.3`` Docker images of Tezos.

.. _v8_known_issues:

Known Issues
------------

The Tezos node of version 8.3 does not recognize as a builtin network ``edo2net``, the current test network for the Edo protocol (which has replaced ``edonet``). To join it, you must configure a custom network as follows:

- create a data directory for the node and copy in it the following ``config.json`` file (alternatively, you may only modify the ``network`` field in your own configuration file)::

    {
      "p2p": {},
      "network": {
        "genesis": {
          "timestamp": "2021-02-11T14:00:00Z",
          "block": "BLockGenesisGenesisGenesisGenesisGenesisdae8bZxCCxh",
          "protocol": "PtYuensgYBb3G3x1hLLbCmcav8ue8Kyd2khADcL5LsT5R1hcXex"
        },
        "genesis_parameters": {
          "values": {
            "genesis_pubkey": "edpkugeDwmwuwyyD3Q5enapgEYDxZLtEUFFSrvVwXASQMVEqsvTqWu"
          }
        },
        "chain_name": "TEZOS_EDO2NET_2021-02-11T14:00:00Z",
        "sandboxed_chain_name": "SANDBOXED_TEZOS",
        "default_bootstrap_peers": [
          "edonet.tezos.co.il",
          "188.40.128.216:29732",
          "51.79.165.131",
          "edo2net.kaml.fr",
          "edonet2.smartpy.io",
          "edonetb.boot.tezostaquito.io"
        ]
      }
    }

- optionally, import a snapshot file
- run the node, passing it the data directory via option ``--data-dir`` if needed.

Changelog
---------

- `Version 8.3 <../CHANGES.html#version-8-3>`_
- `Version 8.2 <../CHANGES.html#version-8-2>`_
- `Version 8.1 <../CHANGES.html#version-8-1>`_
- `Version 8.0 <../CHANGES.html#version-8-0>`_
- `Version 8.0~rc2 <../CHANGES.html#version-8-0-rc2>`_
- `Version 8.0~rc1 <../CHANGES.html#version-8-0-rc1>`_
