Version 21.0~rc1 and 21.0~rc2
=============================

Version 21 contains a new version (V13) of the protocol environment. This new version is used by both :doc:`Quebec A and Quebec B <../protocols/021_quebeca>` protocol proposals.
As a result, Octez version 21 is the first version compatible with these proposals.

The two candidates differ only in their version of the Quebec protocol proposal:
  - ``Octez-v21.0~rc1`` contains the code for the ``Quebec A`` protocol proposal variant and its associated executables.
  - ``Octez-v21.0~rc2`` contains the code for the ``Quebec B`` protocol proposal variant and its associated executables.

Please see the following announcement for more details on `Quebec A and Quebec B protocol proposal variants <https://research-development.nomadic-labs.com/quebec-announcement.html>`__.

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

This operation will be almost instant on Rolling nodes (up to 10s), which is the recommended setup for bakers. However, it may require around 10 minutes on Full and Archive nodes, depending on the performance of the machine. Users must be aware of the downtime during the upgrade that may impact their services.

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
