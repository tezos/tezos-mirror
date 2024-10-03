Version 21.0~rc1 and 21.0~rc2
=============================

Version 21 contains a new version (V13) of the protocol environment. This new version is used by both :doc:`Quebec A and Quebec B <../protocols/021_quebeca>` protocol proposals.
As a result, Octez version 21 is the first version compatible with these proposals.

The two candidates differ only in their version of the Quebec protocol proposal:
  - ``Octez-v21.0~rc1`` contains the code for the ``Quebec A`` protocol proposal variant and its associated executables.
  - ``Octez-v21.0~rc2`` contains the code for the ``Quebec B`` protocol proposal variant and its associated executables.

Please see the following announcement for more details on `Quebec A and Quebec B protocol proposal variants <https://research-development.nomadic-labs.com/quebec-announcement.html>`__.

Note that starting from v21, Octez releases will not distribute the ``octez-evm-node``. You can refer to the `Etherlink documentation <https://docs.etherlink.com/network/evm-nodes>`__ for information on how to get its latest version.

Should any of these protocol proposal variants be chosen during the current `Proposal governance period <https://www.tezosagora.org/period/130>`_, and the corresponding release candidate be stable enough, it will be released as ``Octez v21.0``.

Changes
-------

Version 21 introduces the following changes or new features:

Octez node storage upgrade
~~~~~~~~~~~~~~~~~~~~~~~~~~

This new version allows the storage to scale to the increasing number of blocks per cycles, paving the way to reducing minimal block time further.
When running a node with the legacy data directory, the user will encounter a message of the form::

  octez-node: Error:
              The data directory version is too old.
              Found '3.1', expected '3.2'.
              It needs to be upgraded with `octez-node upgrade storage`.

which they can solve using the following command::

  ./octez-node upgrade storage --data-dir <data-dir>

This operation should only take up to 10s in nodes operating in Rolling history mode, which is the recommended setup for bakers. However, it may require around 10 minutes for nodes operating on Full and Archive modes, depending on the performance of the machine. Users must be aware of the downtime during the upgrade that may impact their services.

Faster propagation of lower round block proposals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In previous Octez versions, the baker binary applied the block they
were creating from round 1 and beyond. This was a safety measure in
case the created block was valid but not applicable by the nodes.
The experience with the past 4 protocols proved that this safety net
was over-dimensioned, as no such issues were detected ever since.
Hence, starting with v21 and the Quebec protocol proposal, the Octez
baker binary will only apply rounds 3 and above block proposals, as
the new safety net.

Not applying round 1 -- 2 block proposals by default while creating
them will allow for faster propagation and earlier consensus
agreement. See :ref:`here <baker_manual_quebeca>`
for further details on how bakers can fine-tune this parameter.

DAL node
~~~~~~~~

Octez v21.0 release candidates increase the stability of the DAL node. In particular, the changes include:
faster loading of cryptographic material (the so-called SRS), reduced memory and
disk footprints, an improved RPC interface, resilience against Octez node's
disconnections. Also, after a restart, the DAL node cleans-up its storage and
catches up the Octez node. Finally, the DAL node can now start one of the 4 profiles:
bootstrap, attester, operator, and observer; the last two profiles match the
corresponding modes of an accompanying smart rollup node.

Smart Rollup Node
~~~~~~~~~~~~~~~~~

Octez v21.0 brings forth many quality of life improvements to the Smart Rollup
node, driven by the deployment of Etherlink Mainnet beta in May. These include:

- The rollup node's *garbage collector* (the mechanism enabling it to only keep
  most recent rollup states) is now leaner and more reliable.
- New exposed metrics which can be used to monitor the rollup state (notably
  its commitments) and the node’s health.
- Opt-in automated execution of outbox messages automatically. This allows for
  an improved UX for decentralized applications relying on the outbox of a
  Smart Rollup, like the Etherlink native bridge.

Better logging UX
~~~~~~~~~~~~~~~~~

Octez v21.0 introduces several UX improvements to the :doc:`logging infrastructure <../user/logging>` including:

- Clearer reporting of outdated nonces in baker logs
- Listing the set of delegates used by a baker at startup
- Coloured logs for most Octez binaries (including the node, baker and accuser)

Please see the `Changelog`_ for further information and other optimizations.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout octez-v21.0-rc1
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v21.0-rc1`` Docker images of Octez.

You can also install Octez using Opam by running ``opam install octez``.

Packages
~~~~~~~~

The packages, as introduced since Octez version 20, are now **available via APT repository** (See :ref:`the documentation <installing_packages>`) for Ubuntu noble and jammy and Debian bookworm.
We recommend users to install them from the APT repository corresponding to their Linux distribution, rather than from the links available in the GitLab release page.

In addition, a **new set of Debian packages is now available for testing**. These packages will replace the current available packages in the future.
Check :ref:`the documentation <new_packages>` for more details.

Changelog
---------

- `Version 21.0~rc1 and rc2 <../CHANGES.html#version-21-0-rc1-and-rc2>`_
